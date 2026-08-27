(require '[benchmark.datascript-bench :as bench]
         '[datahike.api :as d]
         '[datahike.query :as q])

(alter-var-root #'q/*query-result-cache?* (constantly false))

(println "runtime\tdatahike")
(println "db-mode\tshared")

(let [conn (bench/dh-db-with-people)
      db @conn]
  (d/release conn)
  (println "JIT pre-warmup...")
  (doseq [qname bench/query-order]
    (let [{:keys [query args]} (get bench/queries qname)
          qargs (or args [])]
      (dotimes [_ 500]
        (apply d/q query db qargs))))
  (doseq [qname bench/query-order]
    (let [{:keys [query args]} (get bench/queries qname)
          qargs (or args [])
          ms (bench/bench (apply d/q query db qargs))]
      (println (name qname) "\t" ms))))
