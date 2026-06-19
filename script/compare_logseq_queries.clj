(ns compare-logseq-queries
  (:require [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [datascript.core :as d]))

(defn query-sections [query]
  (when (vector? query)
    (loop [xs query
           current nil
           sections {}]
      (if (empty? xs)
        sections
        (let [x (first xs)]
          (if (keyword? x)
            (recur (rest xs) x (assoc sections x []))
            (recur (rest xs) current (update sections current conj x))))))))

(defn input-decls [query]
  (cond
    (vector? query) (get (query-sections query) :in)
    (map? query) (:in query)
    :else nil))

(defn find-decls [query]
  (cond
    (vector? query) (get (query-sections query) :find)
    (map? query) (:find query)
    :else nil))

(defn static-form? [form]
  (not
   (some (fn [x]
           (and (symbol? x)
                (#{"clojure.core/unquote" "clojure.core/unquote-splicing"
                   "cljs.core/unquote" "cljs.core/unquote-splicing"
                   "unquote" "unquote-splicing"}
                 (str x))))
         (tree-seq coll? seq form))))

(defn runnable-query? [query]
  (let [in (input-decls query)]
    (and (static-form? query)
         (or (nil? in)
             (= '[$] in)
             (= ['$] in)
             (= ["$"] in)))))

(defn normalize [value]
  (cond
    (map? value)
    (->> value
         (map (fn [[k v]] [(normalize k) (normalize v)]))
         (sort-by pr-str)
         vec)

    (set? value)
    (->> value (map normalize) (sort-by pr-str) vec)

    (sequential? value)
    (->> value (map normalize) vec)

    (instance? java.util.Date value)
    (.getTime ^java.util.Date value)

    :else value))

(defn result-ok [value]
  {:status :ok :value (normalize value)})

(defn result-error [t]
  {:status :error :message (.getMessage ^Throwable t)})

(defn find-token? [x token]
  (= (str x) token))

(defn tuple-find? [find]
  (and (= 1 (count find))
       (let [spec (first find)]
         (and (sequential? spec)
              (not (some #(find-token? % "...") spec))))))

(defn scalar-find? [find]
  (some #(find-token? % ".") find))

(defn unordered-query-result? [query]
  (let [find (find-decls query)]
    (and (seq find)
         (not (scalar-find? find))
         (not (tuple-find? find)))))

(defn canonicalize-result [query result]
  (if (and (= :ok (:status result))
           (unordered-query-result? query)
           (sequential? (:value result)))
    (update result :value #(->> % (sort-by pr-str) vec))
    result))

(defn log-progress [& parts]
  (binding [*out* *err*]
    (apply println parts)
    (flush)))

(defn schema-entry [[attr props]]
  [attr
   (into {}
         (keep (fn [[k v]]
                 (case k
                   :db/cardinality [:db/cardinality v]
                   :db/unique [:db/unique v]
                   :db/index [:db/index v]
                   :db/isComponent [:db/isComponent v]
                   :db/noHistory [:db/noHistory v]
                   :db/valueType [:db/valueType v]
                   nil)))
         props)])

(defn load-graph-file [path]
  (let [{:keys [schema datoms]} (edn/read-string (slurp path))
        schema (into {} (map schema-entry schema))
        datoms (map (fn [[e a v tx added]]
                      (d/datom e a v tx added))
                    datoms)]
    (d/init-db datoms schema)))

(defn dump-full-graph [runner sqlite-path]
  (let [graph-path (str "tmp/logseq_full_graph." (System/currentTimeMillis) ".edn")
        {:keys [exit err]} (shell/sh runner "dump-graph" sqlite-path graph-path)]
    (when-not (zero? exit)
      (throw (ex-info "full graph dump failed" {:exit exit :err err})))
    graph-path))

(defn env-long [name default]
  (if-let [value (System/getenv name)]
    (Long/parseLong value)
    default))

(defn timeout-result [timeout-ms]
  {:status :error :message (str "Query timed out after " timeout-ms " ms")})

(defn process-error-result [exit err-path]
  {:status :error
   :message (str "query process exited " exit ": " (subs (slurp err-path) 0 (min 1000 (count (slurp err-path)))) )})

(defn wait-for-process [^Process process timeout-ms]
  (if (.waitFor process timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
    (.exitValue process)
    (do
      (.destroyForcibly process)
      (.waitFor process)
      ::timeout)))

(defn write-edn-file [path value]
  (spit path (pr-str value)))

(defn upstream-worker-command []
  ["java"
   (str "-Xmx" (or (System/getenv "LOGSEQ_UPSTREAM_WORKER_XMX") "4g"))
   "-cp"
   (System/getProperty "java.class.path")
   "clojure.main"
   "script/upstream_query_worker.clj"])

(defn run-upstream-query-process [graph-path {:keys [id query]} timeout-ms]
  (let [base (str "tmp/logseq_upstream_query." (System/currentTimeMillis) "." id)
        query-path (str base ".edn")
        out-path (str base ".out.edn")
        err-path (str base ".err")]
    (write-edn-file query-path query)
    (let [process (-> (ProcessBuilder. ^java.util.List (vec (concat (upstream-worker-command) [graph-path query-path])))
                      (.redirectOutput (io/file out-path))
                      (.redirectError (io/file err-path))
                      (.start))
          exit (wait-for-process process timeout-ms)]
      (case exit
        ::timeout (timeout-result timeout-ms)
        0 (canonicalize-result query (edn/read-string (slurp out-path)))
        (process-error-result exit err-path)))))

(defn run-upstream [runner sqlite-path entries]
  (.mkdirs (io/file "tmp"))
  (let [runnable (vec entries)
        graph-path (do
                     (log-progress "upstream dumping full graph")
                     (dump-full-graph runner sqlite-path))
        timeout-ms (env-long "LOGSEQ_QUERY_TIMEOUT_MS" 60000)
        total (count runnable)]
    (loop [idx 0
           [entry & rest] runnable
           results {}]
      (if-not entry
        results
        (let [n (inc idx)
              id (:id entry)]
          (log-progress "upstream" n "/" total id)
          (recur n rest (assoc results id (run-upstream-query-process graph-path entry timeout-ms))))))))

(defn json-escape [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")
      (str/replace "\r" "\\r")
      (str/replace "\t" "\\t")))

(defn write-query-jsonl [path entries]
  (spit path
        (with-out-str
          (doseq [{:keys [id query]} entries]
            (println (str "{\"id\":\"" (json-escape id) "\",\"query\":\""
                          (json-escape (pr-str query)) "\"}"))))))

(defn parse-ocaml-line [line]
  (let [{:keys [id status value message]} (json/parse-string line true)]
    [id (case status
          "ok" {:status :ok :value (normalize (edn/read-string value))}
          "error" {:status :error :message message})]))

(defn write-one-query-jsonl [path {:keys [id query]}]
  (spit path
        (str "{\"id\":\"" (json-escape id) "\",\"query\":\""
             (json-escape (pr-str query)) "\"}\n")))

(defn run-ocaml-query-process [runner sqlite-path {:keys [id] :as entry} timeout-ms]
  (let [base (str "tmp/logseq_ocaml_query." (System/currentTimeMillis) "." id)
        query-path (str base ".jsonl")
        out-path (str base ".out.jsonl")
        err-path (str base ".err")]
    (write-one-query-jsonl query-path entry)
    (let [process (-> (ProcessBuilder. [runner "run" sqlite-path query-path])
                      (.redirectOutput (io/file out-path))
                      (.redirectError (io/file err-path))
                      (.start))
          exit (wait-for-process process timeout-ms)]
      (case exit
        ::timeout (timeout-result timeout-ms)
        0 (let [[_ result] (parse-ocaml-line (first (str/split-lines (slurp out-path))))]
            (canonicalize-result (:query entry) result))
        (process-error-result exit err-path)))))

(defn run-ocaml [runner sqlite-path entries]
  (let [runnable-count (count entries)
        timeout-ms (env-long "LOGSEQ_QUERY_TIMEOUT_MS" 60000)]
    (.mkdirs (io/file "tmp"))
    (log-progress "ocaml running query batch" runnable-count "queries")
    (loop [idx 0
           [entry & rest] entries
           results {}]
      (if-not entry
        results
        (let [n (inc idx)
              id (:id entry)]
          (log-progress "ocaml" n "/" runnable-count id)
          (recur n rest (assoc results id (run-ocaml-query-process runner sqlite-path entry timeout-ms))))))))

(defn mismatch? [left right]
  (not= left right))

(defn report-markdown [entries batch-entries batch-start batch-size upstream ocaml out-path]
  (let [runnable (vec (filter #(runnable-query? (:query %)) entries))
        skipped (- (count entries) (count runnable))
        mismatches (->> batch-entries
                        (keep (fn [{:keys [id file line query]}]
                                (let [u (get upstream id)
                                      o (get ocaml id)]
                                  (when (mismatch? u o)
                                    {:id id :file file :line line :query query
                                     :upstream u :ocaml o}))))
                        vec)]
    (spit out-path
          (with-out-str
            (println "# Logseq DataScript Query Parity Report")
            (println)
            (println "- Extracted queries:" (count entries))
            (println "- Runnable without extra inputs:" (count runnable))
            (println "- Skipped because they need runtime inputs or are dynamic:" skipped)
            (println "- Batch start:" batch-start)
            (println "- Batch size:" batch-size)
            (println "- Batch query ids:" (str/join ", " (map :id batch-entries)))
            (println "- Mismatches:" (count mismatches))
            (println)
            (if (empty? mismatches)
              (println "No upstream/OCaml result differences were found for runnable queries.")
              (doseq [{:keys [id file line query upstream ocaml]} mismatches]
                (println "##" id)
                (println)
                (println "- Source:" (str file ":" line))
                (println)
                (println "```clojure")
                (pprint/pprint query)
                (println "```")
                (println)
                (println "**Upstream**")
                (println)
                (println "```edn")
                (pprint/pprint upstream)
                (println "```")
                (println)
                (println "**OCaml**")
                (println)
                (println "```edn")
                (pprint/pprint ocaml)
                (println "```")
                (println)))))
    {:mismatches mismatches
     :runnable (count runnable)
     :skipped skipped
     :batch-start batch-start
     :batch-size batch-size}))

(defn parse-nonnegative-int [value default]
  (if (nil? value)
    default
    (let [parsed (Long/parseLong value)]
      (when (neg? parsed)
        (throw (ex-info "expected non-negative integer" {:value value})))
      parsed)))

(defn -main [& [queries-path sqlite-path runner report-path batch-start-arg batch-size-arg]]
  (let [queries-path (or queries-path "logseq_queries.edn")
        sqlite-path (or sqlite-path "lambda.sqlite")
        runner (or runner "_build/default/examples/logseq_query_runner.exe")
        report-path (or report-path "logseq_query_diff_report.md")
        batch-start (parse-nonnegative-int batch-start-arg 0)
        batch-size (parse-nonnegative-int batch-size-arg 20)
        entries (edn/read-string (slurp queries-path))
        runnable (vec (filter #(runnable-query? (:query %)) entries))
        batch-entries (->> runnable (drop batch-start) (take batch-size) vec)
        _ (log-progress "loaded" (count entries) "queries from" queries-path)
        _ (log-progress "selected runnable batch" batch-start batch-size "=>" (count batch-entries) "queries")
        upstream (run-upstream runner sqlite-path batch-entries)
        ocaml (run-ocaml runner sqlite-path batch-entries)
        _ (log-progress "writing report" report-path)
        summary (report-markdown entries batch-entries batch-start batch-size upstream ocaml report-path)]
    (binding [*out* *err*]
      (println "wrote report to" report-path)
      (println "runnable:" (:runnable summary) "skipped:" (:skipped summary)
               "mismatches:" (count (:mismatches summary))))
    (shutdown-agents)))

(apply -main *command-line-args*)
