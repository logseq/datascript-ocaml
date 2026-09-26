(ns test.lg-api
  (:require [datascript.api :as d]
            [test.attributes :as attrs]
            [ocaml.Datascript :as ds]
            [ocaml.Datascript_lg :as api]))

(d/defattr shape-id :shape/id :string {:db/unique :db.unique/identity})
(d/defattr shape-x :shape/x :float {})
(def schema (d/schema {:shape/id {:db/unique :db.unique/identity}
                      :shape/doc {:db/index true}}))
(def conn (ds/create_conn :schema schema (Stdlib.ignore 0)))
(def ids (d/query (Datascript_lg.Projection.column 0 d/string-codec)
  '[:find ?id :in $ ?doc :where [?e :shape/doc ?doc] [?e :shape/id ?id]]))
(let [id "first" doc "one"]
  (d/transact! conn [{:shape/id (unquote :string id)
                     :shape/doc (unquote :string doc)
                     :shape/x 2.5}]))
(d/transact! conn [{:shape/id "second" :shape/doc "two" :shape/x 3.5}])
(assert (= ["first"] (vec (d/q ids (ds/db conn) (api/input d/string-codec "one")))))
(println (pr-str (vec (d/q ids (ds/db conn)
  (api/input d/string-codec "one")))))
(println (pr-str (vec (d/q ids (ds/db conn)
  (api/input d/string-codec "two")))))
(println (match (api/or_raise (Datascript_lg.pull (ds/db conn)
  (list (ds/Pull_attr "shape/x")) (ds/Lookup_ref "shape/id" (ds/String "first"))
  (Datascript_lg.Pull.field shape-x)))
  (Some (Some x)) x _ -1.0))

(d/transact-ops! conn (d/entity nil {shape-id "third" shape-x 4.5}))
(assert (= (Some (Some 4.5))
  (d/pull (ds/db conn) [:shape/x] (ds/Lookup_ref "shape/id" (ds/String "third"))
    (Datascript_lg.Pull.field shape-x))))

(assert (= ["second"] (vec (d/q ids (ds/db conn) (api/input d/string-codec "two")))))
(assert (= (Some (Some 2.5))
  (d/pull (ds/db conn) [:shape/x] (ds/Lookup_ref "shape/id" (ds/String "first"))
    (Datascript_lg.Pull.field shape-x))))
(let [calls (atom "")]
  (d/transact-ops! conn
    (d/entity nil
      {shape-id (do (swap! calls (fn [value] (str value "a"))) "ordered")
       shape-x (do (swap! calls (fn [value] (str value "b"))) 5.0)}))
  (assert (= "ab" (deref calls))))

;; Query declarations infer projections from typed attributes.
(d/defquery inferred-ids [:find ?id :in $ ?doc
  :where [?e :shape/doc ?doc] [?e shape-id ?id]])
(assert (= ["first"] (vec (d/q inferred-ids (ds/db conn)
  (api/input d/string-codec "one")))))
(assert (= ["second"] (vec (d/q inferred-ids (ds/db conn)
  (api/input d/string-codec "two")))))
(assert (= [] (vec (d/q inferred-ids (ds/db conn)
  (api/input d/string-codec "missing")))))
(d/defquery inferred-pairs '[:find ?id ?x :where [?e shape-id ?id] [?e shape-x ?x]])
(assert (= 1 (count (filter (fn [row] (= row (tuple "first" 2.5)))
  (vec (d/q inferred-pairs (ds/db conn)))))))
(d/defquery inferred-triples [:find ?id ?x ?id
  :where [?e shape-id ?id] [?e shape-x ?x]])
(assert (= 1 (count (filter (fn [row] (= row (tuple "first" 2.5 "first")))
  (vec (d/q inferred-triples (ds/db conn)))))))
(d/defquery inferred-entities [:find ?e :where [?e shape-id "first"]])
(assert (> (List.hd (d/q inferred-entities (ds/db conn))) 0))
(d/defquery explicit-type {:types {?id d/string-codec}}
  [:find ?id :where [?e :shape/id ?id]])
(assert (= 4 (count (vec (d/q explicit-type (ds/db conn))))))
(d/defquery repeated-type [:find ?id :where [?e shape-id ?id] [?e shape-id ?id]])
(assert (= 4 (count (vec (d/q repeated-type (ds/db conn))))))
;; A schema codec validates external data instead of trusting database contents.
(def bad-conn (ds/create_conn (Stdlib.ignore 0)))
(d/transact! bad-conn [{:shape/id 42}])
(assert (try (do (d/q explicit-type (ds/db bad-conn)) false)
  (catch Datascript_lg.Invalid_data _ true)))

(assert (= 4 (count (vec (d/q [:find ?id :where [?e shape-id ?id]] (ds/db conn))))))
(assert (= 4 (count (vec (d/q '[:find ?id :where [?e :shape/id ?id]] (ds/db conn)
  {:attributes {:shape/id shape-id}})))))
(d/defquery keyword-ids {:attributes {:shape/id shape-id}}
  [:find ?id :where [?e :shape/id ?id]])
(assert (= 4 (count (vec (d/q keyword-ids (ds/db conn))))))
(let [calls (atom 0)]
  (d/q [:find ?id ?id :where [?e :shape/id ?id]] (ds/db conn)
    {:types {?id (do (swap! calls inc) d/string-codec)}})
  (assert (= 1 (deref calls))))

(assert (= 4 (count (vec (d/q [:find ?id :where [?e attrs/id ?id]] (ds/db conn))))))
(d/defquery with-source [:find ?id :with ?e :where [$ ?e attrs/id ?id]])
(assert (= 4 (count (vec (d/q with-source (ds/db conn))))))
(d/defquery filtered [:find ?id :where [?e shape-id ?id] [?e shape-x ?x] [(> ?x 3.0)]])
(assert (= 3 (count (vec (d/q filtered (ds/db conn))))))
(d/defquery transaction-ids [:find ?tx :where [?e shape-id "first" ?tx]])
(assert (> (List.hd (d/q transaction-ids (ds/db conn))) 0))
(assert (try
  (do (d/q [:find ?id :where [?e :shape/doc ?id]] (ds/db conn)
        {:attributes {:shape/doc shape-id}})
      false)
  (catch Invalid_argument _ true)))
