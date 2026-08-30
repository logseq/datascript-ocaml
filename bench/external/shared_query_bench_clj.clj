(ns shared-query-bench-clj
  "Shared people-query suite (q1/q2/q3/…) for Datahike (PSS + SQLite JDBC)
   and Datalevin (durable LMDB). Matches OCaml bench/shared_query_bench.ml
   data generation (seed=1 LCG) and emits result-edn + timing TSV.

   Datalevin timings are reported twice:
   - <query>-nocache : datalevin.query/*cache?* false (engine cost)
   - <query>          : default result+plan cache on (steady-state / Datalevin default)"
  (:require [clojure.string :as str]
            [datahike.api :as dh]
            [datahike-jdbc.core]
            [datalevin.core :as dl]
            [datalevin.query :as dq]))

(defn now-ms [] (double (/ (System/nanoTime) 1e6)))

(defn format-ms [v]
  (cond
    (> v 1) (format "%.2f" (double v))
    (> v 0.01) (format "%.3f" (double v))
    :else (format "%.4f" (double v))))

(def blackhole (atom 0))
(defn bump! [n] (swap! blackhole #(bit-and (+ % n) 0x3fffffff)))

(defn median [xs]
  (let [s (vec (sort xs))]
    (nth s (quot (count s) 2))))

(defn dotime [duration-ms step f]
  (let [start (now-ms)
        deadline (+ start duration-ms)]
    (loop [iters 0]
      (dotimes [_ step] (f))
      (let [iters (+ iters step)]
        (if (< (now-ms) deadline)
          (recur iters)
          (/ (- (now-ms) start) iters))))))

(defn bench [cfg f]
  (dotime (:warmup-ms cfg) (:step cfg) f)
  (median
   (mapv (fn [_] (dotime (:sample-ms cfg) (:step cfg) f))
         (range (:repeats cfg)))))

;; --- LCG matching OCaml Int32 (seed=1) ---
(defn next-int!
  "Mutates rng long-array[0]; matches OCaml shared_query_bench next_int."
  [^longs rng ^long bound]
  (let [s (bit-and (unchecked-add (unchecked-multiply (aget rng 0) 1664525) 1013904223)
                   0xffffffff)
        _ (aset rng 0 s)
        u (bit-and (unsigned-bit-shift-right (unchecked-int s) 1) 0x3fffffff)]
    (rem u bound)))

(def names ["Ivan" "Petr" "Sergei" "Oleg" "Yuri" "Dmitry" "Fedor" "Denis"])
(def last-names ["Ivanov" "Petrov" "Sidorov" "Kovalev" "Kuznetsov" "Voronoi"])
(def sexes [:male :female])

(defn rand-nth! [rng xs]
  (nth xs (next-int! rng (count xs))))

(defn rand-sex! [rng]
  ;; Decorrelate sex from name (same as OCaml: next_int 997 mod 2).
  (nth sexes (mod (next-int! rng 997) (count sexes))))

(def schema-dh
  (mapv
   (fn [i m] (assoc m :db/id (+ 100000 (inc i))))
   (range)
   [{:db/ident :name :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/index true}
    {:db/ident :last-name :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/index true}
    {:db/ident :sex :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one :db/index true}
    {:db/ident :age :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db/index true}
    {:db/ident :salary :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db/index true}
    {:db/ident :follows :db/valueType :db.type/ref :db/cardinality :db.cardinality/many}]))

(def schema-dl
  {:name {:db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/index true}
   :last-name {:db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/index true}
   :sex {:db/valueType :db.type/keyword :db/cardinality :db.cardinality/one :db/index true}
   :age {:db/valueType :db.type/long :db/cardinality :db.cardinality/one :db/index true}
   :salary {:db/valueType :db.type/long :db/cardinality :db.cardinality/one :db/index true}
   :follows {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}})

(defn build-tx [size]
  (let [rng (long-array [1])
        people
        (mapv
         (fn [i]
           {:db/id i
            :name (rand-nth! rng names)
            :last-name (rand-nth! rng last-names)
            :sex (rand-sex! rng)
            :age (next-int! rng 100)
            :salary (next-int! rng 100000)})
         (range 1 (inc size)))
        follows
        (into []
              (keep
               (fn [eid]
                 (when (zero? (next-int! rng 2))
                   [:db/add eid :follows (inc (next-int! rng size))])))
              (range 1 (inc size)))]
    (into people follows)))

(def follow-rules '[[[follow ?e1 ?e2] [?e1 :follows ?e2]]])

(defn parse-args [argv]
  (loop [xs argv
         cfg {:size 20000 :warmup-ms 200.0 :sample-ms 200.0
              :repeats 2 :step 10 :jit-warmup 100 :runtime "datahike"
              :sqlite-path nil :query nil}]
    (if (empty? xs)
      cfg
      (let [[a b & more] xs]
        (case a
          "--size" (recur more (assoc cfg :size (Long/parseLong b)))
          "--warmup-ms" (recur more (assoc cfg :warmup-ms (Double/parseDouble b)))
          "--sample-ms" (recur more (assoc cfg :sample-ms (Double/parseDouble b)))
          "--repeats" (recur more (assoc cfg :repeats (Long/parseLong b)))
          "--jit-warmup" (recur more (assoc cfg :jit-warmup (Long/parseLong b)))
          "--runtime" (recur more (assoc cfg :runtime b))
          "--sqlite" (recur more (assoc cfg :sqlite-path b))
          "--query" (recur more (assoc cfg :query b))
          (throw (ex-info (str "unknown arg " a) {:arg a})))))))

(defn disk-bytes [path]
  (let [f (java.io.File. path)]
    (cond
      (not (.exists f)) 0
      (.isFile f) (.length f)
      (.isDirectory f)
      (->> (file-seq f)
           (filter #(.isFile %))
           (map #(.length %))
           (reduce + 0))
      :else 0)))

(defn remove-path [path]
  (let [f (java.io.File. path)]
    (when (.exists f)
      (doseq [x (reverse (file-seq f))]
        (.delete x))))
  (doseq [sfx ["-wal" "-shm" "-lock"]]
    (let [s (java.io.File. (str path sfx))]
      (when (.exists s) (.delete s)))))

(defn q [store query & inputs]
  (case (:engine store)
    :datahike (apply dh/q query (:db store) inputs)
    :datalevin (apply dl/q query (:db store) inputs)
    (throw (ex-info "q: missing :engine" {:keys (keys store)}))))

(defn edn-q-rows [rows]
  ;; Match OCaml: sort row EDN strings, then wrap as a vector.
  (->> rows
       (map (fn [row] (vec row)))
       (sort-by pr-str)
       vec))

(defn result-edn [store name]
  (case name
    "q1" (edn-q-rows (q store '[:find ?e :where [?e :name "Ivan"]]))
    "q2" (edn-q-rows (q store '[:find ?e ?a :where [?e :name "Ivan"] [?e :age ?a]]))
    "q2-switch" (edn-q-rows (q store '[:find ?e ?a :where [?e :age ?a] [?e :name "Ivan"]]))
    "q3" (edn-q-rows (q store '[:find ?e ?a :where [?e :name "Ivan"] [?e :age ?a] [?e :sex :male]]))
    "q4" (edn-q-rows (q store '[:find ?e ?l ?a :where [?e :name "Ivan"] [?e :last-name ?l] [?e :age ?a] [?e :sex :male]]))
    "q5" (edn-q-rows (q store '[:find ?e1 ?l ?a :where [?e :name "Ivan"] [?e :age ?a] [?e1 :age ?a] [?e1 :last-name ?l]]))
    "qpred1" (edn-q-rows (q store '[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]))
    "qpred2" (edn-q-rows (q store '[:find ?e ?s :in $ ?min_s :where [?e :salary ?s] [(> ?s ?min_s)]] 50000))
    "q-or" (edn-q-rows (q store '[:find ?e :where (or [?e :name "Ivan"] [?e :name "Petr"])]))
    "q-not" (edn-q-rows (q store '[:find ?e ?a :where [?e :age ?a] (not [?e :sex :male])]))
    "q-or-join" (edn-q-rows (q store '[:find ?e ?a :where [?e :age ?a] (or-join [?e] [?e :name "Ivan"] [?e :name "Petr"])]))
    "q-not-join" (edn-q-rows (q store '[:find ?e ?a :where [?e :age ?a] (not-join [?e] [?e :sex :male])]))
    "q-pred-range" (edn-q-rows (q store '[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)] [(< ?s 80000)]]))
    "q-5-merge" (edn-q-rows (q store '[:find ?e ?n ?l ?a ?s :where [?e :name ?n] [?e :last-name ?l] [?e :age ?a] [?e :salary ?s] [?e :sex :male]]))
    "q-rule" (edn-q-rows (q store '[:find ?e1 ?e2 :in $ % :where (follow ?e1 ?e2)] follow-rules))
    (throw (ex-info (str "unknown query " name) {:name name}))))

(defn make-queries [store]
  [{:name "q1" :run (fn [] (bump! (count (q store '[:find ?e :where [?e :name "Ivan"]]))))}
   {:name "q2" :run (fn [] (bump! (count (q store '[:find ?e ?a :where [?e :name "Ivan"] [?e :age ?a]]))))}
   {:name "q2-switch" :run (fn [] (bump! (count (q store '[:find ?e ?a :where [?e :age ?a] [?e :name "Ivan"]]))))}
   {:name "q3" :run (fn [] (bump! (count (q store '[:find ?e ?a :where [?e :name "Ivan"] [?e :age ?a] [?e :sex :male]]))))}
   {:name "q4" :run (fn [] (bump! (count (q store '[:find ?e ?l ?a :where [?e :name "Ivan"] [?e :last-name ?l] [?e :age ?a] [?e :sex :male]]))))}
   {:name "q5" :run (fn [] (bump! (count (q store '[:find ?e1 ?l ?a :where [?e :name "Ivan"] [?e :age ?a] [?e1 :age ?a] [?e1 :last-name ?l]]))))}
   {:name "qpred1" :run (fn [] (bump! (count (q store '[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]))))}
   {:name "qpred2" :run (fn [] (bump! (count (q store '[:find ?e ?s :in $ ?min_s :where [?e :salary ?s] [(> ?s ?min_s)]] 50000))))}
   {:name "q-or" :run (fn [] (bump! (count (q store '[:find ?e :where (or [?e :name "Ivan"] [?e :name "Petr"])]))))}
   {:name "q-not" :run (fn [] (bump! (count (q store '[:find ?e ?a :where [?e :age ?a] (not [?e :sex :male])]))))}
   {:name "q-or-join" :run (fn [] (bump! (count (q store '[:find ?e ?a :where [?e :age ?a] (or-join [?e] [?e :name "Ivan"] [?e :name "Petr"])]))))}
   {:name "q-not-join" :run (fn [] (bump! (count (q store '[:find ?e ?a :where [?e :age ?a] (not-join [?e] [?e :sex :male])]))))}
   {:name "q-pred-range" :run (fn [] (bump! (count (q store '[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)] [(< ?s 80000)]]))))}
   {:name "q-5-merge" :run (fn [] (bump! (count (q store '[:find ?e ?n ?l ?a ?s :where [?e :name ?n] [?e :last-name ?l] [?e :age ?a] [?e :salary ?s] [?e :sex :male]]))))}
   {:name "q-rule" :run (fn [] (bump! (count (q store '[:find ?e1 ?e2 :in $ % :where (follow ?e1 ?e2)] follow-rules))))}])

(defn open-datalevin [dir]
  (remove-path dir)
  (.mkdirs (java.io.File. dir))
  (let [conn (dl/get-conn dir schema-dl)]
    {:engine :datalevin :conn conn
     :backend "lmdb-durable"
     :close! (fn []
               (try (dl/close conn) (catch Exception _))
               (remove-path dir))}))

(defn open-datahike [sqlite-path]
  (remove-path sqlite-path)
  (let [cfg {:store {:backend :jdbc :dbtype "sqlite" :dbname sqlite-path}
             :schema-flexibility :write
             :keep-history? false
             :index :datahike.index/persistent-set
             :initial-tx schema-dh}]
    (try (dh/delete-database cfg) (catch Exception _))
    (remove-path sqlite-path)
    (dh/create-database cfg)
    (let [conn (dh/connect cfg)]
      {:engine :datahike :conn conn :cfg cfg
       :backend "jdbc-sqlite-pss-durable"
       :close! (fn []
                 (try (dh/release conn) (catch Exception _))
                 (try (dh/delete-database cfg) (catch Exception _))
                 (remove-path sqlite-path))})))

(defn build-prepared [cfg]
  (let [runtime (:runtime cfg)
        sqlite-path (or (:sqlite-path cfg)
                        (str "/tmp/shared-query-bench-" runtime "-" (:size cfg) ".sqlite3"))
        tx (build-tx (:size cfg))
        started (now-ms)
        opened (case runtime
                 "datahike" (open-datahike sqlite-path)
                 "datalevin" (open-datalevin (str sqlite-path ".dl"))
                 (throw (ex-info "runtime must be datahike|datalevin" cfg)))
        conn (:conn opened)
        _ (case runtime
            "datahike" (dh/transact conn tx)
            "datalevin" (dl/transact! conn tx))
        build-ms (- (now-ms) started)
        restore-started (now-ms)
        opened (case runtime
                 "datahike"
                 (do
                   (dh/release conn)
                   (let [conn2 (dh/connect (:cfg opened))]
                     (assoc opened :conn conn2 :db @conn2
                            :close! (fn []
                                      (try (dh/release conn2) (catch Exception _))
                                      (try (dh/delete-database (:cfg opened)) (catch Exception _))
                                      (remove-path sqlite-path)))))
                 "datalevin"
                 (assoc opened :db (dl/db conn)))
        restore-ms (- (now-ms) restore-started)
        disk-path (case runtime
                    "datahike" sqlite-path
                    "datalevin" (str sqlite-path ".dl")
                    sqlite-path)]
    (merge opened
           {:build-ms build-ms
            :restore-ms restore-ms
            :sqlite-path sqlite-path
            :disk-bytes (disk-bytes disk-path)})))

(defn -main [& argv]
  (System/setProperty "taoensso.timbre.min-level.edn" ":warn")
  (try
    ((requiring-resolve 'taoensso.timbre/set-min-level!) :warn)
    (catch Exception _))
  (let [cfg (parse-args argv)
        label (or (System/getenv "BENCH_RUNTIME_LABEL")
                  (case (:runtime cfg)
                    "datahike" "datahike-pss-sqlite"
                    "datalevin" "datalevin-lmdb"
                    (:runtime cfg)))
        prepared (build-prepared cfg)
        queries (cond->> (make-queries prepared)
                  (:query cfg) (filterv #(= (:name %) (:query cfg))))]
    (try
      (println (str "runtime\t" label))
      (println "suite\tshared-people-queries")
      (println (str "backend\t" (:backend prepared)))
      (println (str "size\t" (:size cfg)))
      (println (str "warmup-ms\t" (long (:warmup-ms cfg))))
      (println (str "sample-ms\t" (long (:sample-ms cfg))))
      (println (str "repeats\t" (:repeats cfg)))
      (println (str "jit-warmup\t" (:jit-warmup cfg)))
      (println (str "sqlite-path\t" (:sqlite-path prepared)))
      (println (str "build-ms\t" (format-ms (:build-ms prepared))))
      (println (str "restore-ms\t" (format-ms (:restore-ms prepared))))
      (println (str "disk-bytes\t" (:disk-bytes prepared)))
      (println (str "query-cases\t" (count queries)))
      (doseq [q queries]
        (println (str "result-edn\t" (:name q) "\t" (pr-str (result-edn prepared (:name q))))))
      ;; Cold / no result-cache path (fair engine comparison).
      (println "cache-mode\tnocache")
      (let [run-nocache
            (fn [q]
              (case (:runtime cfg)
                "datalevin" (binding [dq/*cache?* false] ((:run q)))
                ((:run q))))]
        (when (pos? (:jit-warmup cfg))
          (doseq [q queries]
            (dotimes [_ (min 5 (:jit-warmup cfg))]
              (run-nocache q))))
        (doseq [q queries]
          (let [t0 (now-ms)
                _ (run-nocache q)
                first-ms (- (now-ms) t0)
                steady (bench cfg (fn [] (run-nocache q)))]
            (println (str (:name q) "-first\t" (format-ms first-ms)))
            (println (str (:name q) "-nocache\t" (format-ms steady))))))
      ;; Warm path with Datalevin result/plan cache (default *cache?* true).
      (println "cache-mode\twarm")
      (when (pos? (:jit-warmup cfg))
        (doseq [q queries]
          (dotimes [_ (:jit-warmup cfg)]
            ((:run q)))))
      (doseq [q queries]
        (println (str (:name q) "\t" (format-ms (bench cfg (:run q))))))
      (binding [*out* *err*]
        (println (str "blackhole=" @blackhole)))
      (finally
        ((:close! prepared))))))
