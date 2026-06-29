(ns extract-logseq-queries
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as rt]))

(def query-keys #{:find :where :in :with :keys :strs :syms})

(defn query-form? [form]
  (cond
    (vector? form) (contains? (set form) :find)
    (map? form) (contains? form :find)
    :else false))

(defn quoted-form? [form]
  (and (seq? form)
       (= 'quote (first form))
       (= 2 (count form))))

(def selector-input-symbol '?selector)

(defn dynamic-pull-list? [form]
  (and (seq? form)
       (= 'list (first form))
       (= 'pull (second form))
       (= 4 (count form))))

(defn contains-selector-input? [form]
  (some #(= selector-input-symbol %) (tree-seq coll? seq form)))

(defn append-selector-input [query]
  (if (and (map? query)
           (contains-selector-input? (:find query))
           (vector? (:in query))
           (not (some #{selector-input-symbol} (:in query))))
    (update query :in conj selector-input-symbol)
    query))

(defn normalize-query-form [form]
  (cond
    (quoted-form? form)
    (normalize-query-form (second form))

    (map? form)
    (-> (into (empty form)
              (map (fn [[k v]] [(normalize-query-form k) (normalize-query-form v)]))
              form)
        append-selector-input)

    (vector? form)
    (mapv normalize-query-form form)

    (list? form)
    (let [normalized (apply list (map normalize-query-form form))]
      (if (dynamic-pull-list? normalized)
        (list 'pull (nth normalized 2) selector-input-symbol)
        normalized))

    (set? form)
    (set (map normalize-query-form form))

    :else form))

(defn walk-queries [form]
  (let [children (cond
                   (map? form) (concat (keys form) (vals form))
                   (coll? form) form
                   :else nil)
        nested (mapcat walk-queries children)]
    (if (query-form? form)
      (cons (normalize-query-form form) nested)
      nested)))

(defn nbb-cache-path? [path]
  (or (str/starts-with? path ".nbb/.cache/")
      (str/includes? path "/.nbb/.cache/")))

(defn distinct-by [f coll]
  (let [seen (volatile! #{})]
    (filter
      (fn [x]
        (let [k (f x)]
          (when-not (contains? @seen k)
            (vswap! seen conj k)
            true)))
      coll)))

(defn source-files [root]
  (->> (file-seq (io/file root))
       (filter #(.isFile ^java.io.File %))
       (filter #(re-find #"\.clj[sc]?$" (.getName ^java.io.File %)))
       (remove #(nbb-cache-path? (.getPath ^java.io.File %)))
       (remove #(str/includes? (.getPath ^java.io.File %) "/node_modules/"))
       (remove #(str/includes? (.getPath ^java.io.File %) "/target/"))
       (remove #(str/includes? (.getPath ^java.io.File %) "/tmp/"))
       (sort-by #(.getPath ^java.io.File %))))

(defn read-file-forms [file]
  (with-open [r (rt/indexing-push-back-reader (slurp file))]
    (loop [forms []]
      (let [line (rt/get-line-number r)
            form (reader/read {:eof ::eof
                               :read-cond :allow
                               :features #{:clj :cljs}
                               :default (fn [tag value] (list 'tagged-literal tag value))}
                              r)]
        (if (= ::eof form)
          forms
          (recur (conj forms {:line (max 1 line) :form form})))))))

(defn relative-path [root file]
  (let [root-path (.getCanonicalPath (io/file root))
        file-path (.getCanonicalPath (io/file file))]
    (if (str/starts-with? file-path (str root-path java.io.File/separator))
      (subs file-path (inc (count root-path)))
      file-path)))

(defn extract-from-file [root file]
  (try
    (for [{:keys [line form]} (read-file-forms file)
          query (walk-queries form)]
      {:file (relative-path root file)
       :line line
       :query query})
    (catch Throwable t
      [{:file (relative-path root file)
        :line 1
        :read-error (.getMessage t)}])))

(defn -main [& [root out]]
  (when-not root
    (binding [*out* *err*]
      (println "Usage: clojure -M script/extract_logseq_queries.clj LOGSEQ_ROOT [OUT]"))
    (System/exit 2))
  (let [out (or out "test/logseq_queries.edn")
        entries (->> (source-files root)
                     (mapcat #(extract-from-file root %))
                     (remove :read-error)
                     (distinct-by :query)
                     (map-indexed (fn [idx entry] (assoc entry :id (str "q" (inc idx)))))
                     vec)]
    (spit out (with-out-str (pprint/pprint entries)))
    (binding [*out* *err*]
      (println "wrote" (count entries) "queries to" out))))

(apply -main *command-line-args*)
