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
             (= ["$"] in)
             (= '[$ %] in)
             (= ['$ '%] in)
             (= ["$" "%"] in)))))

(defn nbb-cache-file? [file]
  (and (string? file)
       (or (str/starts-with? file ".nbb/.cache/")
           (str/includes? file "/.nbb/.cache/"))))

(defn distinct-by [f coll]
  (let [seen (volatile! #{})]
    (filter
      (fn [x]
        (let [k (f x)]
          (when-not (contains? @seen k)
            (vswap! seen conj k)
            true)))
      coll)))

(defn rule-names [rules]
  (->> rules
       (keep (fn [rule]
               (when (seq? (first rule))
                 (symbol (name (first (first rule)))))))
       set))

(defn rule-name [rule]
  (when (seq? (first rule))
    (symbol (name (first (first rule))))))

(defn rules-by-name [rules]
  (->> rules
       (group-by rule-name)
       (remove (comp nil? first))
       (into {})))

(defn form-rule-calls [names form]
  (->> (tree-seq coll? seq form)
       (keep (fn [x]
               (when (and (seq? x) (symbol? (first x)))
                 (let [name (symbol (name (first x)))]
                   (when (contains? names name) name)))))
       set))

(defn query-uses-rule? [rules query]
  (let [names (rule-names rules)]
    (boolean
     (seq (form-rule-calls names query)))))

(defn rule-closure [rules query]
  (let [by-name (rules-by-name rules)
        names (set (keys by-name))]
    (loop [pending (seq (form-rule-calls names query))
           seen #{}]
      (if-not (seq pending)
        (->> seen (mapcat by-name) vec)
        (let [name (first pending)
              clauses (get by-name name)
              deps (->> clauses (mapcat #(form-rule-calls names (rest %))) set)
              next-deps (remove seen deps)]
          (recur (concat (rest pending) next-deps) (conj seen name)))))))

(defn query-with-rules-input [query rules]
  (if (and (nil? (input-decls query)) (query-uses-rule? rules query) (vector? query))
    (into query [:in '$ '%])
    query))

(defn entry-with-runtime-inputs [runtime-inputs entry]
  (let [rules (:rules runtime-inputs)
        query (query-with-rules-input (:query entry) rules)
        in (input-decls query)
        selected-rules (when (seq rules) (rule-closure rules query))]
    (cond-> (assoc entry :query query)
      (and (seq selected-rules) (sequential? in) (some #(= (str %) "%") in))
      (assoc :rules selected-rules))))

(defn query-corpus [runtime-inputs entries]
  (->> entries
       (remove #(nbb-cache-file? (:file %)))
       (map #(entry-with-runtime-inputs runtime-inputs %))
       (distinct-by :query)
       vec))

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
              (not (symbol? (first spec)))
              (not (some #(find-token? % "...") spec))))))

(defn scalar-find? [find]
  (some #(find-token? % ".") find))

(defn pull-form? [form]
  (cond
    (and (sequential? form) (= "pull" (str (first form)))) true
    (sequential? form) (some pull-form? form)
    (map? form) (some (fn [[k v]] (or (pull-form? k) (pull-form? v))) form)
    :else false))

(defn attr-pair? [value]
  (and (sequential? value)
       (= 2 (count value))
       (keyword? (first value))))

(defn pulled-entity? [value]
  (and (sequential? value)
       (seq value)
       (every? attr-pair? value)))

(defn normalize-pull-value [value]
  (cond
    (pulled-entity? value)
    (->> value
         (map (fn [[k v]] [k (normalize-pull-value v)]))
         (sort-by pr-str)
         vec)

    (sequential? value)
    (let [items (mapv normalize-pull-value value)]
      (if (every? pulled-entity? items)
        (->> items (sort-by pr-str) vec)
        items))

    :else value))

(defn unordered-query-result? [query]
  (let [find (find-decls query)]
    (and (seq find)
         (not (scalar-find? find))
         (not (tuple-find? find)))))

(defn canonicalize-result [query result]
  (cond-> result
    (and (= :ok (:status result)) (pull-form? query))
    (update :value normalize-pull-value)

    (and (= :ok (:status result))
         (unordered-query-result? query)
         (sequential? (:value result)))
    (update :value #(->> % (sort-by pr-str) vec))))

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

(defn graph-edn-path? [path]
  (str/ends-with? path ".edn"))

(defn full-graph-path [runner graph-or-sqlite-path]
  (if (graph-edn-path? graph-or-sqlite-path)
    graph-or-sqlite-path
    (do
      (log-progress "upstream dumping full graph")
      (dump-full-graph runner graph-or-sqlite-path))))

(defn env-long [name default]
  (if-let [value (System/getenv name)]
    (Long/parseLong value)
    default))

(defn timeout-result [timeout-ms]
  {:status :error :message (str "Query timed out after " timeout-ms " ms")})

(defn timeout-result-for-upstream [upstream-info timeout-ms]
  (let [upstream-result (:result upstream-info)]
    (if (and (= :error (:status upstream-result))
             (str/starts-with? (:message upstream-result) "Query timed out after "))
      upstream-result
      (timeout-result timeout-ms))))

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

(defn elapsed-ms-since [started-ns]
  (long (Math/ceil (/ (double (- (System/nanoTime) started-ns)) 1000000.0))))

(defn write-edn-file [path value]
  (spit path (pr-str value)))

(defn upstream-worker-command []
  ["java"
   (str "-Xmx" (or (System/getenv "LOGSEQ_UPSTREAM_WORKER_XMX") "4g"))
   "-cp"
   (System/getProperty "java.class.path")
   "clojure.main"
   "script/upstream_query_worker.clj"])

(defn run-upstream-query-process [graph-path {:keys [id query] :as entry} timeout-ms]
  (let [base (str "tmp/logseq_upstream_query." (System/currentTimeMillis) "." id)
        query-path (str base ".edn")
        out-path (str base ".out.edn")
        err-path (str base ".err")]
    (write-edn-file query-path (cond-> {:query query} (:rules entry) (assoc :rules (:rules entry))))
    (let [started-ns (System/nanoTime)
          process (-> (ProcessBuilder. ^java.util.List (vec (concat (upstream-worker-command) [graph-path query-path])))
                      (.redirectOutput (io/file out-path))
                      (.redirectError (io/file err-path))
                      (.start))
          exit (wait-for-process process timeout-ms)
          result (case exit
                   ::timeout (timeout-result timeout-ms)
                   0 (canonicalize-result query (edn/read-string (slurp out-path)))
                   (process-error-result exit err-path))]
      {:result result
       :elapsed-ms (elapsed-ms-since started-ns)
       :timeout-ms timeout-ms})))

(defn run-upstream [runner graph-or-sqlite-path entries]
  (.mkdirs (io/file "tmp"))
  (let [runnable (vec entries)
        graph-path (full-graph-path runner graph-or-sqlite-path)
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
          (let [result (run-upstream-query-process graph-path entry timeout-ms)]
            (log-progress "upstream" id "elapsed-ms" (:elapsed-ms result))
            (recur n rest (assoc results id result))))))))

(defn json-escape [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")
      (str/replace "\r" "\\r")
      (str/replace "\t" "\\t")))

(defn query-json-line [{:keys [id query rules]}]
  (str "{\"id\":\"" (json-escape id) "\",\"query\":\""
       (json-escape (pr-str query)) "\""
       (if rules
         (str ",\"rules\":\"" (json-escape (pr-str rules)) "\"")
         "")
       "}"))

(defn write-query-jsonl [path entries]
  (spit path
        (with-out-str
          (doseq [entry entries]
            (println (query-json-line entry))))))

(defn ocaml-json-result [{:keys [id status value message]}]
  [id (case status
        "ok" {:status :ok :value (normalize (edn/read-string value))}
        "error" {:status :error :message message})])

(defn ocaml-ready-line? [line]
  (= "ready" (:status (json/parse-string line true))))

(defn parse-ocaml-line [line]
  (ocaml-json-result (json/parse-string line true)))

(defn first-ocaml-result-line [out-path]
  (->> (str/split-lines (slurp out-path))
       (remove ocaml-ready-line?)
       first))

(defn write-one-query-jsonl [path {:keys [id query rules]}]
  (spit path (str (query-json-line {:id id :query query :rules rules}) "\n")))

(defn ocaml-run-command [runner graph-or-sqlite-path query-path]
  (if (graph-edn-path? graph-or-sqlite-path)
    [runner "run-graph" graph-or-sqlite-path query-path]
    [runner "run" graph-or-sqlite-path query-path]))

(defn run-ocaml-query-process [runner graph-or-sqlite-path {:keys [id] :as entry} timeout-ms]
  (let [base (str "tmp/logseq_ocaml_query." (System/currentTimeMillis) "." id)
        query-path (str base ".jsonl")
        out-path (str base ".out.jsonl")
        err-path (str base ".err")]
    (write-one-query-jsonl query-path entry)
    (let [started-ns (System/nanoTime)
          process (-> (ProcessBuilder. ^java.util.List (ocaml-run-command runner graph-or-sqlite-path query-path))
                      (.redirectOutput (io/file out-path))
                      (.redirectError (io/file err-path))
                      (.start))
          exit (wait-for-process process timeout-ms)
          result (case exit
                   ::timeout (timeout-result timeout-ms)
                   0 (let [[_ result] (parse-ocaml-line (first-ocaml-result-line out-path))]
                       (canonicalize-result (:query entry) result))
                   (process-error-result exit err-path))]
      {:result result
       :elapsed-ms (elapsed-ms-since started-ns)
       :timeout-ms timeout-ms})))

(defn ocaml-timeout-ms [upstream-info default-timeout-ms margin-ms]
  (if-let [elapsed-ms (:elapsed-ms upstream-info)]
    (+ elapsed-ms margin-ms)
    default-timeout-ms))

(defn current-entry-timeout [entries idx upstream default-timeout-ms margin-ms]
  (if-let [entry (nth entries idx nil)]
    (ocaml-timeout-ms (get upstream (:id entry)) default-timeout-ms margin-ms)
    default-timeout-ms))

(defn batch-process-error-results [entries start-idx exit err-path default-timeout-ms margin-ms upstream]
  (let [result (process-error-result exit err-path)]
    (into {}
          (for [{:keys [id]} (subvec entries start-idx)]
            [id {:result result
                 :elapsed-ms 0
                 :timeout-ms (ocaml-timeout-ms (get upstream id) default-timeout-ms margin-ms)}]))))

(defn run-ocaml-batch-process [runner graph-or-sqlite-path entries upstream default-timeout-ms margin-ms]
  (let [base (str "tmp/logseq_ocaml_batch." (System/currentTimeMillis))
        query-path (str base ".jsonl")
        err-path (str base ".err")
        total (count entries)]
    (write-query-jsonl query-path entries)
    (let [process-start-ns (System/nanoTime)
          process (-> (ProcessBuilder. ^java.util.List (ocaml-run-command runner graph-or-sqlite-path query-path))
                      (.redirectError (java.lang.ProcessBuilder$Redirect/to (io/file err-path)))
                      (.start))
          reader (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream process)))]
      (loop [idx 0
             query-start-ns nil
             results {}]
        (cond
          (= idx total)
          (do
            (.waitFor process)
            results)

          (.ready reader)
          (let [line (.readLine reader)
                now-ns (System/nanoTime)]
            (if (nil? line)
              (recur idx query-start-ns results)
              (let [parsed (json/parse-string line true)]
                (if (= "ready" (:status parsed))
                  (recur idx now-ns results)
                  (let [entry (nth entries idx)
                        expected-id (:id entry)
                        timeout-ms (ocaml-timeout-ms (get upstream expected-id) default-timeout-ms margin-ms)
                        [id result] (ocaml-json-result parsed)
                        elapsed-start-ns (or query-start-ns process-start-ns)
                        elapsed-ms (long (Math/ceil (/ (double (- now-ns elapsed-start-ns)) 1000000.0)))
                        result (canonicalize-result (:query entry) result)]
                    (when-not (= id expected-id)
                      (throw (ex-info "OCaml batch returned query ids out of order"
                                      {:expected expected-id :actual id})))
                    (log-progress "ocaml" id "elapsed-ms" elapsed-ms)
                    (recur (inc idx)
                           (System/nanoTime)
                           (assoc results id {:result result
                                              :elapsed-ms elapsed-ms
                                              :timeout-ms timeout-ms})))))))

          (not (.isAlive process))
          (merge results
                 (batch-process-error-results
                   entries idx (.exitValue process) err-path default-timeout-ms margin-ms upstream))

          query-start-ns
          (let [timeout-ms (current-entry-timeout entries idx upstream default-timeout-ms margin-ms)
                elapsed-ms (elapsed-ms-since query-start-ns)]
            (if (> elapsed-ms timeout-ms)
                (let [entry (nth entries idx)
                      id (:id entry)
                      current-result {id {:result (timeout-result-for-upstream (get upstream id) timeout-ms)
                                          :elapsed-ms elapsed-ms
                                          :timeout-ms timeout-ms}}
                      remaining (subvec entries (inc idx))]
                  (.destroyForcibly process)
                  (.waitFor process)
                  (merge results
                         current-result
                         (if (seq remaining)
                           (run-ocaml-batch-process runner graph-or-sqlite-path remaining upstream default-timeout-ms margin-ms)
                           {})))
              (do
                (Thread/sleep 10)
                (recur idx query-start-ns results))))

          :else
          (do
            (Thread/sleep 10)
            (recur idx query-start-ns results)))))))

(defn run-ocaml [runner graph-or-sqlite-path entries upstream]
  (let [runnable-count (count entries)
        default-timeout-ms (env-long "LOGSEQ_QUERY_TIMEOUT_MS" 60000)
        margin-ms (env-long "LOGSEQ_OCAML_TIMEOUT_MARGIN_MS" 1000)]
    (.mkdirs (io/file "tmp"))
    (log-progress "ocaml running query batch" runnable-count "queries")
    (doseq [[idx {:keys [id]}] (map-indexed vector entries)]
      (log-progress "ocaml" (inc idx) "/" runnable-count id "timeout-ms"
                    (ocaml-timeout-ms (get upstream id) default-timeout-ms margin-ms)))
    (run-ocaml-batch-process runner graph-or-sqlite-path (vec entries) upstream default-timeout-ms margin-ms)))

(defn timeout-result? [result]
  (and (= :error (:status result))
       (str/starts-with? (:message result) "Query timed out after ")))

(defn mismatch? [left right]
  (and (not (timeout-result? left))
       (not= left right)))

(defn report-markdown [entries batch-entries batch-start batch-size upstream ocaml out-path]
  (let [runnable (vec (filter #(runnable-query? (:query %)) entries))
        skipped (- (count entries) (count runnable))
        mismatches (->> batch-entries
                        (keep (fn [{:keys [id file line query]}]
                                (let [u-info (get upstream id)
                                      o-info (get ocaml id)
                                      u (:result u-info)
                                      o (:result o-info)]
                                  (when (mismatch? u o)
                                    {:id id :file file :line line :query query
                                     :upstream u :ocaml o
                                     :upstream-info u-info
                                     :ocaml-info o-info}))))
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
              (doseq [{:keys [id file line query upstream ocaml upstream-info ocaml-info]} mismatches]
                (println "##" id)
                (println)
                (println "- Source:" (str file ":" line))
                (println "- Upstream elapsed ms:" (:elapsed-ms upstream-info))
                (println "- OCaml elapsed ms:" (:elapsed-ms ocaml-info))
                (println "- OCaml timeout ms:" (:timeout-ms ocaml-info))
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

(defn -main [& [queries-path sqlite-path runner report-path batch-start-arg batch-size-arg runtime-inputs-path]]
  (let [queries-path (or queries-path "logseq_queries.edn")
        sqlite-path (or sqlite-path "lambda.sqlite")
        runner (or runner "_build/default/examples/logseq_query_runner.exe")
        report-path (or report-path "logseq_query_diff_report.md")
        batch-start (parse-nonnegative-int batch-start-arg 0)
        batch-size (parse-nonnegative-int batch-size-arg 20)
        runtime-inputs (if (and runtime-inputs-path (.exists (io/file runtime-inputs-path)))
                         (edn/read-string (slurp runtime-inputs-path))
                         (if (.exists (io/file "logseq_runtime_inputs.edn"))
                           (edn/read-string (slurp "logseq_runtime_inputs.edn"))
                           {}))
        entries (query-corpus runtime-inputs (edn/read-string (slurp queries-path)))
        runnable (vec (filter #(runnable-query? (:query %)) entries))
        batch-entries (->> runnable (drop batch-start) (take batch-size) vec)
        _ (log-progress "loaded" (count entries) "queries from" queries-path)
        _ (log-progress "selected runnable batch" batch-start batch-size "=>" (count batch-entries) "queries")
        upstream (run-upstream runner sqlite-path batch-entries)
        ocaml (run-ocaml runner sqlite-path batch-entries upstream)
        _ (log-progress "writing report" report-path)
        summary (report-markdown entries batch-entries batch-start batch-size upstream ocaml report-path)]
    (binding [*out* *err*]
      (println "wrote report to" report-path)
      (println "runnable:" (:runnable summary) "skipped:" (:skipped summary)
               "mismatches:" (count (:mismatches summary))))
    (shutdown-agents)))

(apply -main *command-line-args*)
