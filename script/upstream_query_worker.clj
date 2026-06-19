(ns upstream-query-worker
  (:require [clojure.edn :as edn]
            [datascript.core :as d]))

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

(defn -main [graph-path query-path]
  (let [db (load-graph-file graph-path)
        payload (edn/read-string (slurp query-path))
        query (if (map? payload) (:query payload) payload)
        rules (:rules payload)]
    (prn
     (try
       (result-ok (if rules (d/q query db rules) (d/q query db)))
       (catch Throwable t
         (result-error t))))
    (shutdown-agents)))

(apply -main *command-line-args*)
