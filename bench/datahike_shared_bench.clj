(require '[benchmark.datascript-bench :as bench]
         '[datahike.api :as d]
         '[datahike.query :as q])

(alter-var-root #'q/*query-result-cache?* (constantly false))

(defn- env-int [name default]
  (some-> (System/getenv name) Integer/parseInt (or default)))

(defn- env-double [name default]
  (some-> (System/getenv name) Double/parseDouble (or default)))

(def bench-size
  (some-> (System/getenv "BENCH_SIZE") Integer/parseInt))

(def warmup-ms (env-double "BENCH_WARMUP_MS" 200.0))
(def sample-ms (env-double "BENCH_SAMPLE_MS" 200.0))
(def bench-repeats (env-int "BENCH_REPEATS" 2))
(def jit-warmup (env-int "BENCH_JIT_WARMUP" 100))

(defn people-of-size [size]
  (if (<= size (count bench/people20k))
    (subvec bench/people20k 0 size)
    (vec (take size bench/people))))

(defn db-with-people [size]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? false
             :attribute-refs? true
             :search-cache-size 0
             :index :datahike.index/persistent-set}]
    (d/delete-database cfg)
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn {:tx-data bench/dh-schema})
      (d/transact conn {:tx-data (people-of-size size)})
      (let [db @conn]
        (d/release conn)
        db))))

(println "runtime\tdatahike")
(println "db-mode\tshared")
(println "storage\tmemory-persistent-set")
(when bench-size
  (println (str "size\t" bench-size)))
(println (str "warmup-ms\t" (long warmup-ms)))
(println (str "sample-ms\t" (long sample-ms)))
(println (str "repeats\t" bench-repeats))
(println (str "jit-warmup\t" jit-warmup))

(binding [bench/*warmup-t* (long warmup-ms)
          bench/*bench-t* (long sample-ms)
          bench/*repeats* bench-repeats]
  (let [size (or bench-size 20000)
        db (db-with-people size)]
    (println (str "JIT pre-warmup (" jit-warmup "/query)..."))
    (when (pos? jit-warmup)
      (doseq [qname bench/query-order]
        (let [{:keys [query args]} (get bench/queries qname)
              qargs (or args [])]
          (dotimes [_ jit-warmup]
            (apply d/q query db qargs)))))
    (doseq [qname bench/query-order]
      (let [{:keys [query args]} (get bench/queries qname)
            qargs (or args [])
            ms (bench/bench (apply d/q query db qargs))]
        (println (name qname) "\t" ms)))))
