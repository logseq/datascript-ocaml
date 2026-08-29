#!/usr/bin/env nbb-logseq
;; Logseq-shaped query bench via @logseq/nbb-logseq#feat-db-v34.
;; Persistence matches Logseq DB graphs (see logseq.db.common.sqlite-cli):
;;   IStorage → SQLite kvs + transit, create-conn {:storage} + transact!.
;; No release-js / datascript.js interop. OCaml side stays non-PSS.
(ns logseq-query-bench-upstream
  (:require
   ["node:sqlite" :refer [DatabaseSync]]
   ["fs" :as fs]
   ["process" :as process]
   [datascript.core :as d]
   [datascript.storage :refer [IStorage]]
   [datascript.transit :as dt]))

(def default-config
  {:size 20000
   :pages 2000
   :warmup-ms 200
   :sample-ms 200
   :repeats 3
   :step 5
   :jit-warmup 50
   :query nil
   :sqlite-path nil})

(defn parse-args [argv]
  (loop [cfg default-config
         args (vec argv)]
    (if (empty? args)
      cfg
      (let [[a b & more] args]
        (case a
          "--size" (recur (assoc cfg :size (js/parseInt b 10)) more)
          "--pages" (recur (assoc cfg :pages (js/parseInt b 10)) more)
          "--warmup-ms" (recur (assoc cfg :warmup-ms (js/parseFloat b)) more)
          "--sample-ms" (recur (assoc cfg :sample-ms (js/parseFloat b)) more)
          "--repeats" (recur (assoc cfg :repeats (js/parseInt b 10)) more)
          "--jit-warmup" (recur (assoc cfg :jit-warmup (js/parseInt b 10)) more)
          "--query" (recur (assoc cfg :query b) more)
          "--sqlite" (recur (assoc cfg :sqlite-path b) more)
          "--list-queries" (recur (assoc cfg :list-queries true) more)
          (throw (js/Error. (str "unknown argument: " a))))))))

(defn now-ms []
  (let [t (.hrtime process)]
    (+ (* (aget t 0) 1000) (/ (aget t 1) 1e6))))

(defn median [xs]
  (let [s (vec (sort xs))]
    (nth s (quot (count s) 2))))

(defn format-ms [v]
  (cond
    (> v 1) (.toFixed v 2)
    (> v 0.01) (.toFixed v 3)
    :else (.toFixed v 4)))

(def blackhole (atom 0))
(defn bump! [n]
  (swap! blackhole #(bit-and (+ % n) 0x3fffffff)))

(defn keep-take [n pred xs]
  (into [] (comp (filter pred) (take n)) xs))

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

(def schema
  {:block/uuid {:db/unique :db.unique/identity :db/index true}
   :block/title {:db/index true}
   :block/name {:db/index true}
   :block/updated-at {:db/index true}
   :block/created-at {:db/index true}
   :block/journal-day {:db/index true}
   :block/parent {:db/valueType :db.type/ref :db/index true}
   :block/page {:db/valueType :db.type/ref :db/index true}
   :block/tags {:db/valueType :db.type/ref
                :db/cardinality :db.cardinality/many
                :db/index true}
   :block/refs {:db/valueType :db.type/ref
                :db/cardinality :db.cardinality/many
                :db/index true}
   :block/content {:db/index true}})

(defn uuid-of [i]
  (str "00000000-0000-4000-8000-" (.padStart (str i) 12 "0")))

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
                  (zero? (mod e 5))
                  (assoc :block/journal-day (journal-day-of e))
                  (zero? (mod e 7))
                  (assoc :block/tags (inc (mod e tag-count))))]
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
                  (assoc :block/tags (inc (mod e tag-count))
                         :block/refs page))]
        (conj! tx ent)))
    {:tx (persistent! tx)
     :pages pages
     :base-ms base-ms}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Storage — aligned with logseq.db.common.sqlite-cli / db-worker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-kvs-table!
  "Same DDL as logseq.db.common.sqlite/create-kvs-table!"
  [^js sql-db]
  (.exec sql-db
         "create table if not exists kvs (addr INTEGER primary key, content TEXT, addresses JSON)"))

(defn- upsert-addr-content!
  "Same semantics as sqlite-cli/upsert-addr-content!.
   node:sqlite has no better-sqlite3 `.transaction`; apply rows sequentially."
  [^js sql-db rows]
  (assert sql-db ::upsert-addr-content!)
  (let [insert (.prepare sql-db
                         "INSERT INTO kvs (addr, content, addresses) values (?, ?, ?) on conflict(addr) do update set content = excluded.content, addresses = excluded.addresses")]
    (doseq [item rows]
      (.run insert (.-addr item) (.-content item) (.-addresses item)))))

(defn- restore-data-from-addr
  "Same semantics as sqlite-cli/restore-data-from-addr."
  [^js sql-db addr]
  (when-let [row (.get (.prepare sql-db "select content, addresses from kvs where addr = ?") addr)]
    (let [content (.-content row)
          addresses (when (.-addresses row)
                      (js/JSON.parse (.-addresses row)))
          data (dt/read-transit-str content)]
      (if (and addresses (map? data))
        (assoc data :addresses addresses)
        data))))

(defn new-sqlite-storage
  "Creates a datascript IStorage for sqlite.
   Mirrors logseq.db.common.sqlite-cli/new-sqlite-storage (transit content + addresses JSON)."
  [^js sql-db]
  (reify IStorage
    (-store [_ addr+data-seq _delete-addrs]
      (let [data (map
                  (fn [[addr payload]]
                    (let [payload' (if (map? payload) (dissoc payload :addresses) payload)
                          addresses (when (map? payload)
                                      (when-let [as (:addresses payload)]
                                        (js/JSON.stringify (clj->js as))))]
                      #js {:addr addr
                           :content (dt/write-transit-str payload')
                           :addresses addresses}))
                  addr+data-seq)]
        (upsert-addr-content! sql-db data)))
    (-restore [_ addr]
      (restore-data-from-addr sql-db addr))))

(defn get-storage-conn
  "Same as logseq.db.common.sqlite/get-storage-conn."
  [storage schema]
  (or (d/restore-conn storage)
      (d/create-conn schema {:storage storage})))

(defn sqlite-open [sqlite-path]
  (when (and sqlite-path (fs/existsSync sqlite-path))
    (fs/unlinkSync sqlite-path))
  (let [db (DatabaseSync. (or sqlite-path ":memory:"))]
    (create-kvs-table! db)
    db))

(defn build-prepared [{:keys [size pages sqlite-path]}]
  (let [built (build-tx size pages)
        started (now-ms)
        sql-db (sqlite-open sqlite-path)
        storage (new-sqlite-storage sql-db)
        conn (get-storage-conn storage schema)
        ;; Logseq path: transact! on a storage-backed conn (auto-stores PSS nodes).
        _ (d/transact! conn (:tx built))
        restore-started (now-ms)
        ;; Re-open like a fresh process: restore-conn from the same kvs.
        conn' (or (d/restore-conn storage)
                  (throw (js/Error. "sqlite PSS restore-conn failed")))
        restore-ms (- (now-ms) restore-started)
        build-ms (- (now-ms) started)
        db @conn'
        p (:pages built)
        disk-bytes (if (and sqlite-path (fs/existsSync sqlite-path))
                     (.-size (fs/statSync sqlite-path))
                     0)]
    {:db db
     :conn conn'
     :sql-db sql-db
     :pages p
     :base-ms (:base-ms built)
     :sample-uuid (uuid-of (max 1 (quot p 2)))
     :sample-page 1
     :sample-tag 1
     :build-ms build-ms
     :restore-ms restore-ms
     :disk-bytes disk-bytes
     :sqlite-path (or sqlite-path ":memory:")}))

(defn hydrate-forward! [entity]
  (when entity
    (doseq [attr [:block/uuid :block/title :block/name :block/updated-at :block/journal-day]]
      (when (some? (get entity attr))
        (bump! 1)))))

(defn avet-attr-rseq [db attr]
  ;; Logseq: (rseq (d/datoms db :avet attr)). Prefer rseek-datoms (same order, lazy).
  (d/rseek-datoms db :avet attr))

(defn is-page? [db datom]
  (and (empty? (d/datoms db :eavt (:e datom) :block/page))
       (let [titles (d/datoms db :eavt (:e datom) :block/title)]
         (and (seq titles)
              (string? (:v (first titles)))
              (pos? (count (.trim ^js/String (:v (first titles)))))))))

(defn recent-page-datoms [db]
  (keep-take 15 #(is-page? db %) (avet-attr-rseq db :block/updated-at)))

(defn latest-journal-datoms [db pages]
  (let [today (journal-day-of pages)]
    (keep-take 10
               (fn [datom]
                 (and (number? (:v datom)) (<= (:v datom) today)))
               (avet-attr-rseq db :block/journal-day))))

(defn hydrate-edn-pairs [entity]
  (->> [:block/uuid :block/title :block/name :block/updated-at :block/journal-day]
       (keep (fn [attr]
               (when-some [v (get entity attr)]
                 [attr v])))
       (sort-by (comp str first))
       vec))

(defn sorted-eids [datoms]
  (vec (sort (map :e datoms))))

(defn edn-q-rows [rows]
  ;; Stable EDN: sorted vector of row vectors (find returns a set in CLJS).
  (->> rows
       (map (fn [row] (mapv identity row)))
       (sort-by pr-str)
       vec))

(defn result-edn [{:keys [db pages base-ms sample-uuid sample-page sample-tag]} name]
  (case name
    "recent-pages"
    (mapv :e (recent-page-datoms db))
    "latest-journals"
    (mapv :e (latest-journal-datoms db pages))
    "uuid-lookup"
    (let [e (d/entity db [:block/uuid sample-uuid])]
      (if e
        [(:db/id e) (hydrate-edn-pairs e)]
        nil))
    "title-lookup"
    (sorted-eids (d/datoms db :avet :block/title (str "Page " (quot pages 2))))
    "children-by-parent"
    (sorted-eids (d/datoms db :avet :block/parent sample-page))
    "blocks-by-page"
    (sorted-eids (d/datoms db :avet :block/page sample-page))
    "tags-scan"
    (sorted-eids (d/datoms db :avet :block/tags sample-tag))
    "eavt-entity"
    (->> (d/datoms db :eavt sample-page)
         (mapv (fn [d] [(:a d) (:v d)]))
         (sort-by (comp str first))
         vec)
    "entity-hydrate"
    (let [e (d/entity db sample-page)]
      (if e
        [(:db/id e) (hydrate-edn-pairs e)]
        nil))
    "q-updated-at-between"
    (let [lo (+ base-ms 3600000)
          hi (+ base-ms 86400000)]
      (edn-q-rows
       (d/q '[:find ?e ?t
              :in $ ?lo ?hi
              :where
              [?e :block/updated-at ?t]
              [(>= ?t ?lo)]
              [(<= ?t ?hi)]]
            db lo hi)))
    "q-journal-pages"
    (edn-q-rows
     (d/q '[:find ?e ?d
            :where
            [?e :block/journal-day ?d]
            [?e :block/title ?t]]
          db))
    "q-page-by-name"
    (edn-q-rows
     (d/q '[:find ?e
            :in $ ?n
            :where [?e :block/name ?n]]
          db
          (str "page-" (quot pages 3))))
    (throw (js/Error. (str "unknown result-edn query " name)))))

(defn make-queries [{:keys [db pages base-ms sample-uuid sample-page sample-tag]}]
  [{:name "recent-pages"
    :run (fn []
           (doseq [datom (recent-page-datoms db)]
             (hydrate-forward! (d/entity db (:e datom)))))}
   {:name "latest-journals"
    :run (fn []
           (doseq [datom (latest-journal-datoms db pages)]
             (hydrate-forward! (d/entity db (:e datom)))))}
   {:name "uuid-lookup"
    :run (fn [] (hydrate-forward! (d/entity db [:block/uuid sample-uuid])))}
   {:name "title-lookup"
    :run (fn []
           (bump! (count (d/datoms db :avet :block/title (str "Page " (quot pages 2))))))}
   {:name "children-by-parent"
    :run (fn []
           (bump! (count (d/datoms db :avet :block/parent sample-page))))}
   {:name "blocks-by-page"
    :run (fn []
           (bump! (count (d/datoms db :avet :block/page sample-page))))}
   {:name "tags-scan"
    :run (fn []
           (bump! (count (d/datoms db :avet :block/tags sample-tag))))}
   {:name "eavt-entity"
    :run (fn []
           (bump! (count (d/datoms db :eavt sample-page))))}
   {:name "entity-hydrate"
    :run (fn [] (hydrate-forward! (d/entity db sample-page)))}
   {:name "q-updated-at-between"
    :run (fn []
           (let [lo (+ base-ms 3600000)
                 hi (+ base-ms 86400000)
                 rows (d/q '[:find ?e ?t
                             :in $ ?lo ?hi
                             :where
                             [?e :block/updated-at ?t]
                             [(>= ?t ?lo)]
                             [(<= ?t ?hi)]]
                           db lo hi)]
             (bump! (count rows))))}
   {:name "q-journal-pages"
    :run (fn []
           (bump! (count
                   (d/q '[:find ?e ?d
                          :where
                          [?e :block/journal-day ?d]
                          [?e :block/title ?t]]
                        db))))}
   {:name "q-page-by-name"
    :run (fn []
           (bump! (count
                   (d/q '[:find ?e
                          :in $ ?n
                          :where [?e :block/name ?n]]
                        db
                        (str "page-" (quot pages 3))))))}])

(def query-names
  ["recent-pages" "latest-journals" "uuid-lookup" "title-lookup"
   "children-by-parent" "blocks-by-page" "tags-scan" "eavt-entity"
   "entity-hydrate" "q-updated-at-between" "q-journal-pages" "q-page-by-name"])

(defn -main [& argv]
  (let [cfg (parse-args argv)]
    (when (:list-queries cfg)
      (doseq [q query-names] (println q))
      (.exit process 0))
    (let [label (or (.-BENCH_RUNTIME_LABEL (.-env process)) "cljs-nbb-logseq-pss")
          sqlite-path (or (:sqlite-path cfg)
                          (str "/tmp/logseq-query-bench-cljs-" (:size cfg) ".sqlite3"))]
      (println (str "runtime\t" label))
      (println "suite\tlogseq-queries-shared")
      (println "backend\tsqlite-kvs-pss")
      (println (str "size\t" (:size cfg)))
      (println (str "pages\t" (:pages cfg)))
      (println (str "warmup-ms\t" (:warmup-ms cfg)))
      (println (str "sample-ms\t" (:sample-ms cfg)))
      (println (str "repeats\t" (:repeats cfg)))
      (println (str "jit-warmup\t" (:jit-warmup cfg)))
      (println (str "sqlite-path\t" sqlite-path))
      (.write (.-stderr process)
              (str "Building via nbb-logseq PSS+kvs+transact! (entities=" (:size cfg)
                   " pages=" (:pages cfg) ")...\n"))
      (let [prepared (build-prepared (assoc cfg :sqlite-path sqlite-path))
            queries (cond->> (make-queries prepared)
                      (:query cfg)
                      (filterv #(= (:name %) (:query cfg))))]
        (when (empty? queries)
          (throw (js/Error. (str "unknown query " (:query cfg)))))
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
        (.write (.-stderr process) (str "blackhole=" @blackhole "\n"))
        (when-let [sql (:sql-db prepared)]
          (try (.close sql) (catch :default _)))))))

(apply -main *command-line-args*)
