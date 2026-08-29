#!/usr/bin/env nbb
;; Logseq-shaped query bench for Logseq-forked DataScript, run via nbb.
;; Durable path: node:sqlite stores d/serializable JSON; queries run after restore.
(ns logseq-query-bench-upstream
  (:require
   ["node:sqlite" :refer [DatabaseSync]]
   ["fs" :as fs]
   ["path" :as path]
   ["process" :as process]))

(def d
  (js/require
   (or (.-UPSTREAM_DATASCRIPT_JS (.-env process))
       (throw (js/Error. "Set UPSTREAM_DATASCRIPT_JS to Logseq-forked release-js/datascript.js")))))

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

(defn as-array [xs]
  (if (array? xs) xs (js/Array.from xs)))

(defn keep-take [n pred xs]
  (let [out #js []]
    (doseq [x xs
            :while (< (.-length out) n)]
      (when (pred x)
        (.push out x)))
    out))

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
  #js {"block/uuid" #js {":db/unique" ":db.unique/identity" ":db/index" true}
       "block/title" #js {":db/index" true}
       "block/name" #js {":db/index" true}
       "block/updated-at" #js {":db/index" true}
       "block/created-at" #js {":db/index" true}
       "block/journal-day" #js {":db/index" true}
       "block/parent" #js {":db/valueType" ":db.type/ref" ":db/index" true}
       "block/page" #js {":db/valueType" ":db.type/ref" ":db/index" true}
       "block/tags" #js {":db/valueType" ":db.type/ref"
                         ":db/cardinality" ":db.cardinality/many"
                         ":db/index" true}
       "block/refs" #js {":db/valueType" ":db.type/ref"
                         ":db/cardinality" ":db.cardinality/many"
                         ":db/index" true}
       "block/content" #js {":db/index" true}})

(defn uuid-of [i]
  (str "00000000-0000-4000-8000-" (.padStart (str i) 12 "0")))

(defn journal-day-of [i]
  (+ 20250101 (mod i 400)))

(defn build-tx [size pages]
  (let [pages (max 1 (min pages size))
        base-ms 1700000000000
        day-ms 86400000
        tag-count (min 32 pages)
        tx #js []]
    (doseq [e (range 1 (inc pages))]
      (let [updated (+ base-ms (* 10 day-ms) (* e 1000))
            ent #js {":db/id" e
                     "block/uuid" (uuid-of e)
                     "block/title" (str "Page " e)
                     "block/name" (str "page-" e)
                     "block/updated-at" updated
                     "block/created-at" (- updated day-ms)
                     "block/content" (str "page body " e)}]
        (when (zero? (mod e 5))
          (aset ent "block/journal-day" (journal-day-of e)))
        (when (zero? (mod e 7))
          (aset ent "block/tags" (inc (mod e tag-count))))
        (.push tx ent)))
    (doseq [index (range (- size pages))]
      (let [e (+ pages index 1)
            page (inc (mod index pages))
            parent (if (or (zero? index) (zero? (mod index 3))) page (dec e))
            updated (+ base-ms (* e 30))
            ent #js {":db/id" e
                     "block/uuid" (uuid-of e)
                     "block/title" (str "Block " e)
                     "block/updated-at" updated
                     "block/created-at" (- updated 60000)
                     "block/parent" parent
                     "block/page" page
                     "block/content" (str "block body " e)}]
        (when (zero? (mod e 11))
          (aset ent "block/tags" (inc (mod e tag-count)))
          (aset ent "block/refs" page))
        (.push tx ent)))
    #js {:tx tx :pages pages :baseMs base-ms}))

(defn entity-get [entity attr]
  (when entity
    (if (fn? (.-get entity))
      (.get entity attr)
      (aget entity attr))))

(defn hydrate-forward! [entity]
  (when entity
    (doseq [attr ["block/uuid" "block/title" "block/name" "block/updated-at" "block/journal-day"]]
      (when (some? (entity-get entity attr))
        (bump! 1)))))

(defn avet-attr-rseq [db attr]
  (-> (.datoms d db ":avet" attr) as-array .slice .reverse))

(defn sqlite-open [sqlite-path]
  (when (and sqlite-path (fs/existsSync sqlite-path))
    (fs/unlinkSync sqlite-path))
  (let [db (DatabaseSync. (or sqlite-path ":memory:"))]
    (.exec db "CREATE TABLE IF NOT EXISTS ds_serializable (id INTEGER PRIMARY KEY CHECK (id = 1), payload TEXT NOT NULL);")
    db))

(defn sqlite-store-db! [sql-db ds-db]
  (let [payload (js/JSON.stringify (.serializable d ds-db))]
    (.run (.prepare sql-db "INSERT OR REPLACE INTO ds_serializable(id, payload) VALUES (1, ?) ")
          payload)
    (count payload)))

(defn sqlite-restore-db [sql-db]
  (let [row (.get (.prepare sql-db "SELECT payload FROM ds_serializable WHERE id = 1"))]
    (when-not row
      (throw (js/Error. "missing serializable payload in sqlite")))
    (.from_serializable d (js/JSON.parse (.-payload row)))))

(defn build-prepared [{:keys [size pages sqlite-path]}]
  (let [built (build-tx size pages)
        started (now-ms)
        mem-db (.db_with d (.empty_db d schema) (.-tx built))
        sql-db (sqlite-open sqlite-path)
        bytes (sqlite-store-db! sql-db mem-db)
        db (sqlite-restore-db sql-db)
        build-ms (- (now-ms) started)
        p (.-pages built)]
    {:db db
     :sql-db sql-db
     :pages p
     :base-ms (.-baseMs built)
     :sample-uuid (uuid-of (max 1 (quot p 2)))
     :sample-page 1
     :sample-tag 1
     :build-ms build-ms
     :sqlite-bytes bytes
     :sqlite-path (or sqlite-path ":memory:")}))

(defn make-queries [{:keys [db pages base-ms sample-uuid sample-page sample-tag]}]
  [{:name "recent-pages"
    :run (fn []
           (let [is-page (fn [datom]
                           (and (zero? (.-length (as-array (.datoms d db ":eavt" (.-e datom) "block/page"))))
                                (let [titles (as-array (.datoms d db ":eavt" (.-e datom) "block/title"))]
                                  (and (pos? (.-length titles))
                                       (string? (.-v (aget titles 0)))
                                       (pos? (count (.trim (.-v (aget titles 0)))))))))
                 pages15 (keep-take 15 is-page (avet-attr-rseq db "block/updated-at"))]
             (doseq [datom pages15]
               (hydrate-forward! (.entity d db (.-e datom))))))}
   {:name "latest-journals"
    :run (fn []
           (let [today (journal-day-of pages)
                 kept (keep-take 10
                                 (fn [datom]
                                   (and (number? (.-v datom)) (<= (.-v datom) today)))
                                 (avet-attr-rseq db "block/journal-day"))]
             (doseq [datom kept]
               (hydrate-forward! (.entity d db (.-e datom))))))}
   {:name "uuid-lookup"
    :run (fn [] (hydrate-forward! (.entity d db #js ["block/uuid" sample-uuid])))}
   {:name "title-lookup"
    :run (fn []
           (bump! (.-length (as-array (.datoms d db ":avet" "block/title"
                                               (str "Page " (quot pages 2)))))))}
   {:name "children-by-parent"
    :run (fn []
           (bump! (.-length (as-array (.datoms d db ":avet" "block/parent" sample-page)))))}
   {:name "blocks-by-page"
    :run (fn []
           (bump! (.-length (as-array (.datoms d db ":avet" "block/page" sample-page)))))}
   {:name "tags-scan"
    :run (fn []
           (bump! (.-length (as-array (.datoms d db ":avet" "block/tags" sample-tag)))))}
   {:name "eavt-entity"
    :run (fn []
           (bump! (.-length (as-array (.datoms d db ":eavt" sample-page)))))}
   {:name "entity-hydrate"
    :run (fn [] (hydrate-forward! (.entity d db sample-page)))}
   {:name "q-updated-at-between"
    :run (fn []
           (let [lo (+ base-ms 3600000)
                 hi (+ base-ms 86400000)
                 rows (.q d
                          "[:find ?e ?t :in $ ?lo ?hi :where [?e \"block/updated-at\" ?t] [(>= ?t ?lo)] [(<= ?t ?hi)]]"
                          db lo hi)]
             (bump! (.-length rows))))}
   {:name "q-journal-pages"
    :run (fn []
           (bump! (.-length
                   (.q d
                       "[:find ?e ?d :where [?e \"block/journal-day\" ?d] [?e \"block/title\" ?t]]"
                       db))))}
   {:name "q-page-by-name"
    :run (fn []
           (bump! (.-length
                   (.q d
                       "[:find ?e :in $ ?n :where [?e \"block/name\" ?n]]"
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
    (let [label (or (.-BENCH_RUNTIME_LABEL (.-env process)) "logseq-forked-cljs-nbb")
          sqlite-path (or (:sqlite-path cfg)
                          (str "/tmp/logseq-query-bench-cljs-" (:size cfg) ".sqlite3"))]
      (println (str "runtime\t" label))
      (println "suite\tlogseq-queries-shared")
      (println (str "backend\tsqlite"))
      (println (str "size\t" (:size cfg)))
      (println (str "pages\t" (:pages cfg)))
      (println (str "warmup-ms\t" (:warmup-ms cfg)))
      (println (str "sample-ms\t" (:sample-ms cfg)))
      (println (str "repeats\t" (:repeats cfg)))
      (println (str "jit-warmup\t" (:jit-warmup cfg)))
      (println (str "sqlite-path\t" sqlite-path))
      (.write (.-stderr process)
              (str "Building via nbb+sqlite (entities=" (:size cfg)
                   " pages=" (:pages cfg) ")...\n"))
      (let [prepared (build-prepared (assoc cfg :sqlite-path sqlite-path))
            queries (cond->> (make-queries prepared)
                      (:query cfg)
                      (filterv #(= (:name %) (:query cfg))))]
        (when (empty? queries)
          (throw (js/Error. (str "unknown query " (:query cfg)))))
        (println (str "build-ms\t" (format-ms (:build-ms prepared))))
        (println (str "sqlite-bytes\t" (:sqlite-bytes prepared)))
        (println (str "query-cases\t" (count queries)))
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
