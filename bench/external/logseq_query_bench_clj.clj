(ns logseq-query-bench-clj
  "Logseq shared-query bench for Datahike (PSS + SQLite JDBC) and Datalevin.
   Emits the same TSV + result-edn lines as the OCaml/CLJS shared harness."
  (:require [clojure.string :as str]
            [datahike.api :as dh]
            [datahike-jdbc.core]
            [datalevin.core :as dl]))

(defn now-ms [] (double (/ (System/nanoTime) 1e6)))

(defn format-ms [v]
  (cond
    (> v 1) (format "%.2f" (double v))
    (> v 0.01) (format "%.3f" (double v))
    :else (format "%.4f" (double v))))

(def blackhole (atom 0))
(defn bump! [n] (swap! blackhole #(bit-and (+ % n) 0x3fffffff)))

(defn keep-take [n pred xs]
  (into [] (comp (filter pred) (take n)) xs))

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

(def schema-dh
  (mapv
   (fn [i m] (assoc m :db/id (+ 100000 (inc i))))
   (range)
   [{:db/ident :block/uuid :db/valueType :db.type/string :db/unique :db.unique/identity :db/cardinality :db.cardinality/one :db/index true}
    {:db/ident :block/title :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/index true}
    {:db/ident :block/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/index true}
    {:db/ident :block/updated-at :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db/index true}
    {:db/ident :block/created-at :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db/index true}
    {:db/ident :block/journal-day :db/valueType :db.type/long :db/cardinality :db.cardinality/one :db/index true}
    {:db/ident :block/parent :db/valueType :db.type/ref :db/cardinality :db.cardinality/one :db/index true}
    {:db/ident :block/page :db/valueType :db.type/ref :db/cardinality :db.cardinality/one :db/index true}
    {:db/ident :block/tags :db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/index true}
    {:db/ident :block/refs :db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/index true}
    {:db/ident :block/content :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/index true}]))

(def schema-dl
  {:block/uuid {:db/valueType :db.type/string :db/unique :db.unique/identity :db/cardinality :db.cardinality/one :db/index true}
   :block/title {:db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/index true}
   :block/name {:db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/index true}
   :block/updated-at {:db/valueType :db.type/long :db/cardinality :db.cardinality/one :db/index true}
   :block/created-at {:db/valueType :db.type/long :db/cardinality :db.cardinality/one :db/index true}
   :block/journal-day {:db/valueType :db.type/long :db/cardinality :db.cardinality/one :db/index true}
   :block/parent {:db/valueType :db.type/ref :db/cardinality :db.cardinality/one :db/index true}
   :block/page {:db/valueType :db.type/ref :db/cardinality :db.cardinality/one :db/index true}
   :block/tags {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/index true}
   :block/refs {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/index true}
   :block/content {:db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/index true}})

(defn- dl-index [index]
  (case index
    :eavt :eav
    :aevt :aev
    :avet :ave
    index))

(defn uuid-of [i]
  (str "00000000-0000-4000-8000-" (format "%012d" (long i))))

(defn journal-day-of [i]
  (+ 20250101 (mod i 400)))

(defn build-tx [size pages]
  (let [pages (max 1 (min pages size))
        base-ms 1700000000000
        day-ms 86400000
        tag-count (min 32 pages)
        tx (transient [])]
    (doseq [e (range 1 (inc pages))]
      (let [updated (+ base-ms (* 10 day-ms) (* e 1000))
            ent (cond-> {:db/id e
                         :block/uuid (uuid-of e)
                         :block/title (str "Page " e)
                         :block/name (str "page-" e)
                         :block/updated-at updated
                         :block/created-at (- updated day-ms)
                         :block/content (str "page body " e)}
                  (zero? (mod e 5)) (assoc :block/journal-day (journal-day-of e))
                  (zero? (mod e 7)) (assoc :block/tags (inc (mod e tag-count))))]
        (conj! tx ent)))
    (doseq [index (range (- size pages))]
      (let [e (+ pages index 1)
            page (inc (mod index pages))
            parent (if (or (zero? index) (zero? (mod index 3))) page (dec e))
            updated (+ base-ms (* e 30))
            ent (cond-> {:db/id e
                         :block/uuid (uuid-of e)
                         :block/title (str "Block " e)
                         :block/updated-at updated
                         :block/created-at (- updated 60000)
                         :block/parent parent
                         :block/page page
                         :block/content (str "block body " e)}
                  (zero? (mod e 11))
                  (assoc :block/tags (inc (mod e tag-count)) :block/refs page))]
        (conj! tx ent)))
    {:tx (persistent! tx) :pages pages :base-ms base-ms}))

(defn parse-args [argv]
  (loop [xs argv
         cfg {:size 5000 :pages 500 :warmup-ms 200.0 :sample-ms 200.0
              :repeats 3 :step 5 :jit-warmup 20 :runtime "datahike"
              :sqlite-path nil :query nil}]
    (if (empty? xs)
      cfg
      (let [[a b & more] xs]
        (case a
          "--size" (recur more (assoc cfg :size (Long/parseLong b)))
          "--pages" (recur more (assoc cfg :pages (Long/parseLong b)))
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

(defn datoms [store index & components]
  (case (:engine store)
    :datahike (apply dh/datoms (:db store) index components)
    :datalevin (apply dl/datoms (:db store) (dl-index index) components)
    (throw (ex-info "datoms: missing :engine on store" {:keys (keys store)}))))

(defn entity [store eid]
  (case (:engine store)
    :datahike (dh/entity (:db store) eid)
    :datalevin (dl/entity (:db store) eid)
    (throw (ex-info "entity: missing :engine on store" {:keys (keys store)}))))

(defn q [store query & inputs]
  (case (:engine store)
    :datahike (apply dh/q query (:db store) inputs)
    :datalevin (apply dl/q query (:db store) inputs)
    (throw (ex-info "q: missing :engine on store" {:keys (keys store)}))))

(defn avet-attr-rseq [store attr]
  ;; Logseq: (rseq (d/datoms db :avet attr)) — exact attr, then reverse.
  ;; Do not use bare rseek-datoms: it continues into earlier AVET attrs.
  (rseq (vec (datoms store :avet attr))))

(defn is-page? [store datom]
  (and (empty? (datoms store :eavt (:e datom) :block/page))
       (let [titles (datoms store :eavt (:e datom) :block/title)]
         (and (seq titles)
              (string? (:v (first titles)))
              (pos? (count (str/trim (str (:v (first titles))))))))))

(defn recent-page-datoms [store]
  (keep-take 15 #(is-page? store %) (avet-attr-rseq store :block/updated-at)))

(defn latest-journal-datoms [store pages]
  (let [today (journal-day-of pages)]
    (keep-take 10
               (fn [datom]
                 (and (number? (:v datom)) (<= (:v datom) today)))
               (avet-attr-rseq store :block/journal-day))))

(defn hydrate-forward! [ent]
  (when ent
    (doseq [attr [:block/uuid :block/title :block/name :block/updated-at :block/journal-day]]
      (when (some? (get ent attr))
        (bump! 1)))))

(defn hydrate-edn-pairs [ent]
  (->> [:block/uuid :block/title :block/name :block/updated-at :block/journal-day]
       (keep (fn [attr]
               (when-some [v (get ent attr)]
                 [attr v])))
       (sort-by (comp str first))
       vec))

(defn sorted-eids [ds]
  (vec (sort (map :e ds))))

(defn edn-q-rows [rows]
  (->> rows (map (fn [row] (mapv identity row))) (sort-by pr-str) vec))

(defn result-edn [store name]
  (let [{:keys [pages base-ms sample-uuid sample-page sample-tag]} store]
    (case name
      "recent-pages" (mapv :e (recent-page-datoms store))
      "latest-journals" (mapv :e (latest-journal-datoms store pages))
      "uuid-lookup"
      (let [e (entity store [:block/uuid sample-uuid])]
        (if e [(:db/id e) (hydrate-edn-pairs e)] nil))
      "title-lookup"
      (sorted-eids (datoms store :avet :block/title (str "Page " (quot pages 2))))
      "children-by-parent"
      (sorted-eids (datoms store :avet :block/parent sample-page))
      "blocks-by-page"
      (sorted-eids (datoms store :avet :block/page sample-page))
      "tags-scan"
      (sorted-eids (datoms store :avet :block/tags sample-tag))
      "eavt-entity"
      (->> (datoms store :eavt sample-page)
           (mapv (fn [d] [(:a d) (:v d)]))
           (sort-by (comp str first))
           vec)
      "entity-hydrate"
      (let [e (entity store sample-page)]
        (if e [(:db/id e) (hydrate-edn-pairs e)] nil))
      "q-updated-at-between"
      (let [lo (+ base-ms 3600000) hi (+ base-ms 86400000)]
        (edn-q-rows
         (q store '[:find ?e ?t :in $ ?lo ?hi
                    :where [?e :block/updated-at ?t] [(>= ?t ?lo)] [(<= ?t ?hi)]]
            lo hi)))
      "q-journal-pages"
      (edn-q-rows (q store '[:find ?e ?d :where [?e :block/journal-day ?d] [?e :block/title ?t]]))
      "q-page-by-name"
      (edn-q-rows
       (q store '[:find ?e :in $ ?n :where [?e :block/name ?n]]
          (str "page-" (quot pages 3))))
      (throw (ex-info (str "unknown query " name) {:name name})))))

(defn make-queries [store]
  (let [{:keys [pages base-ms sample-uuid sample-page sample-tag]} store]
    [{:name "recent-pages"
      :run (fn []
             (doseq [datom (recent-page-datoms store)]
               (hydrate-forward! (entity store (:e datom)))))}
     {:name "latest-journals"
      :run (fn []
             (doseq [datom (latest-journal-datoms store pages)]
               (hydrate-forward! (entity store (:e datom)))))}
     {:name "uuid-lookup"
      :run (fn [] (hydrate-forward! (entity store [:block/uuid sample-uuid])))}
     {:name "title-lookup"
      :run (fn [] (bump! (count (datoms store :avet :block/title (str "Page " (quot pages 2))))))}
     {:name "children-by-parent"
      :run (fn [] (bump! (count (datoms store :avet :block/parent sample-page))))}
     {:name "blocks-by-page"
      :run (fn [] (bump! (count (datoms store :avet :block/page sample-page))))}
     {:name "tags-scan"
      :run (fn [] (bump! (count (datoms store :avet :block/tags sample-tag))))}
     {:name "eavt-entity"
      :run (fn [] (bump! (count (datoms store :eavt sample-page))))}
     {:name "entity-hydrate"
      :run (fn [] (hydrate-forward! (entity store sample-page)))}
     {:name "q-updated-at-between"
      :run (fn []
             (let [lo (+ base-ms 3600000) hi (+ base-ms 86400000)]
               (bump! (count (q store '[:find ?e ?t :in $ ?lo ?hi
                                        :where [?e :block/updated-at ?t]
                                        [(>= ?t ?lo)] [(<= ?t ?hi)]]
                                 lo hi)))))}
     {:name "q-journal-pages"
      :run (fn []
             (bump! (count (q store '[:find ?e ?d
                                      :where [?e :block/journal-day ?d]
                                      [?e :block/title ?t]]))))}
     {:name "q-page-by-name"
      :run (fn []
             (bump! (count (q store '[:find ?e :in $ ?n :where [?e :block/name ?n]]
                               (str "page-" (quot pages 3))))))}]))

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
       :backend "jdbc-sqlite-pss"
       :close! (fn []
                 (try (dh/release conn) (catch Exception _))
                 (try (dh/delete-database cfg) (catch Exception _))
                 (remove-path sqlite-path))})))

(defn open-datalevin [dir]
  (when (.exists (java.io.File. dir))
    (dl/clear dir))
  (let [conn (dl/get-conn dir schema-dl)]
    {:engine :datalevin :conn conn
     :backend "lmdb"
     :close! (fn []
               (try (dl/close conn) (catch Exception _))
               (try (dl/clear dir) (catch Exception _)))}))

(defn build-prepared [cfg]
  (let [runtime (:runtime cfg)
        sqlite-path (or (:sqlite-path cfg)
                        (str "/tmp/logseq-query-bench-" runtime "-" (:size cfg) ".sqlite3"))
        built (build-tx (:size cfg) (:pages cfg))
        started (now-ms)
        opened (case runtime
                 "datahike" (open-datahike sqlite-path)
                 "datalevin" (open-datalevin (str sqlite-path ".dl"))
                 (throw (ex-info "runtime must be datahike|datalevin" cfg)))
        conn (:conn opened)
        _ (case runtime
            "datahike" (dh/transact conn (:tx built))
            "datalevin" (dl/transact! conn (:tx built)))
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
                                      (try (dh/delete-database (:cfg opened)) (catch Exception _))))))
                 "datalevin"
                 (assoc opened :db (dl/db conn)))
        restore-ms (- (now-ms) restore-started)
        pages (:pages built)
        disk-path (case runtime
                    "datahike" sqlite-path
                    "datalevin" (str sqlite-path ".dl")
                    sqlite-path)]
    (merge opened
           {:pages pages
            :base-ms (:base-ms built)
            :sample-uuid (uuid-of (max 1 (quot pages 2)))
            :sample-page 1
            :sample-tag 1
            :build-ms build-ms
            :restore-ms restore-ms
            :sqlite-path sqlite-path
            :disk-bytes (disk-bytes disk-path)})))

(defn -main [& argv]
  ;; Keep stdout as clean TSV for the compare scripts.
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
      (println "suite\tlogseq-queries-shared")
      (println (str "backend\t" (:backend prepared)))
      (println (str "size\t" (:size cfg)))
      (println (str "pages\t" (:pages cfg)))
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
