(ns cross-runtime.upstream-fuzz
  (:require
   [cljs.nodejs :as nodejs]
   [datascript.core :as d]))

(nodejs/enable-util-print!)

(def tx0 0x20000000)

(defn attr-name [attr]
  (cond
    (keyword? attr) (subs (str attr) 1)
    (string? attr) attr
    :else (str attr)))

(defn normalized-value [value]
  (cond
    (keyword? value) (str value)
    (vector? value) (mapv normalized-value value)
    (set? value) (->> value (map normalized-value) sort vec)
    (map? value) (->> value
                  (map (fn [[k v]] [(normalized-value k) (normalized-value v)]))
                  (sort-by first)
                  (into {}))
    :else value))

(defn normalized-datom [datom]
  [(.-e datom)
   (attr-name (.-a datom))
   (normalized-value (.-v datom))
   (js/Math.abs (or (.-tx datom) tx0))
   true])

(defn schema-indexed? [spec]
  (boolean (or (:db/index spec) (:db/unique spec))))

(defn schema-attr [spec]
  {"cardinality" (if (= :db.cardinality/many (:db/cardinality spec)) "many" "one")
   "indexed" (schema-indexed? spec)
   "unique" (case (:db/unique spec)
              :db.unique/identity "identity"
              :db.unique/value "value"
              nil)
   "value_type" (case (:db/valueType spec)
                  :db.type/ref "ref"
                  nil)})

(defn normalized-schema [db]
  (->> (d/schema db)
       (filter (fn [[_ spec]] (map? spec)))
       (map (fn [[attr spec]] [(attr-name attr) (schema-attr spec)]))
       (sort-by first)
       vec))

(defn emit [name value]
  (println (str name "\t" (js/JSON.stringify (clj->js value)))))

(def emails
  ["person-0@example.test"
   "person-1@example.test"
   "person-2@example.test"
   "person-3@example.test"])

(def tags ["alpha" "beta" "gamma" "delta"])
(def batch-count 100)

(defn generated-batch [i]
  (let [source (nth emails (mod (+ (* i 5) 1) (count emails)))
        target (nth emails (mod (+ (* i 7) 2) (count emails)))
        tag (nth tags (mod (+ (* i 3) 1) (count tags)))
        old-tag (nth tags (mod (+ i 2) (count tags)))
        score (+ 10 (mod (* i 17) 90))]
    (cond-> [[:db/add [:email source] :tag tag]
             [:db/add [:email source] :score score]]
      (not= source target)
      (conj [:db/add [:email source] :links [:email target]])

      (zero? (mod i 3))
      (conj [:db/retract [:email source] :tag old-tag]))))

(defn run []
  (let [conn (d/create-conn)]
    (d/transact!
     conn
     [{:db/id 100
       :db/ident :email
       :db/cardinality :db.cardinality/one
       :db/unique :db.unique/identity
       :db/index true}
      {:db/id 101
       :db/ident :tag
       :db/cardinality :db.cardinality/many}
      {:db/id 102
       :db/ident :friend
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/one}
      {:db/id 103
       :db/ident :links
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/many}
      {:db/id 104
       :db/ident :kind
       :db/cardinality :db.cardinality/one}])
    (d/transact!
     conn
     [{:db/id -1 :email (nth emails 0) :tag ["alpha" "seed"] :kind :person}
      {:db/id -2 :email (nth emails 1) :friend -1 :links [-1] :tag ["beta"] :kind :person}
      {:db/id -3 :email (nth emails 2) :friend -2 :links [-1 -2] :tag ["gamma"] :kind :person}
      {:db/id -4 :email (nth emails 3) :friend -3 :links [-1] :tag ["delta"] :kind :person}])
    (d/transact!
     conn
     [{:db/id 200
       :db/ident :score
       :db/cardinality :db.cardinality/one
       :db/index true}])
    (doseq [i (range batch-count)]
      (d/transact! conn (generated-batch i)))
    (let [db @conn]
      (emit "fuzz.final.schema" (normalized-schema db))
      (emit "fuzz.final.datoms" (mapv normalized-datom (d/datoms db :eavt))))))

(set! *main-cli-fn* run)
