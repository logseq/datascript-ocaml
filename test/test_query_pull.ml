open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms ?e ?a ?v ?tx db index =
  datoms_seq ?e ?a ?v ?tx db index |> List.of_seq

let assert_query_set label expected actual =
  if List.sort compare expected <> List.sort compare actual then failf "%s" label

let kw name = Keyword name
let scalar value = Pulled_scalar value
let pull id attrs = Result_pull { pulled_id = id; pulled_attrs = attrs }
let entity id attrs = Pulled_entity { pulled_id = id; pulled_attrs = attrs }
let many_values values = Pulled_many values

let db =
  empty_db ()
  |> db_with
       [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int64 44L) ] }
       ; Entity { db_id = Some (Entity_id (eid 2L)); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int64 25L) ] }
       ; Entity { db_id = Some (Entity_id (eid 3L)); attrs = [ "name", One_value (String "Oleg"); "age", One_value (Int64 11L) ] }
       ]

let test_query_pull__test_basics () =
  assert_query_set
    "pull in find relation"
    [ [ pull (eid 1L) [ kw "name", scalar (String "Petr") ] ]; [ pull (eid 2L) [ kw "name", scalar (String "Ivan") ] ] ]
    (q_string db "[:find (pull ?e [:name]) :where [?e :age ?a] [(>= ?a 18)]]");
  assert_query_set
    "pull can be mixed with scalars"
    [ [ Result_entity (eid 1L); Result_value (Int64 44L); pull (eid 1L) [ kw "name", scalar (String "Petr") ] ]
    ; [ Result_entity (eid 2L); Result_value (Int64 25L); pull (eid 2L) [ kw "name", scalar (String "Ivan") ] ]
    ]
    (q_string db "[:find ?e ?a (pull ?e [:name]) :where [?e :age ?a] [(>= ?a 18)]]")

let test_query_pull__test_var_pattern () =
  assert_query_set
    "dynamic pull pattern"
    [ [ pull (eid 1L) [ kw "name", scalar (String "Petr") ] ]; [ pull (eid 2L) [ kw "name", scalar (String "Ivan") ] ] ]
    (q_string
       ~inputs:[ Arg_scalar (Result_value (Vector [ Keyword "name" ])) ]
       db
       "[:find (pull ?e ?pattern) :in $ ?pattern :where [?e :age ?a] [(>= ?a 18)]]")

let test_query_pull__test_multiple_sources () =
  let db1 = empty_db () |> db_with [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int64 25L) ] } ] in
  let db2 = empty_db () |> db_with [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int64 25L) ] } ] in
  assert_query_set
    "pull from named source"
    [ [ Result_entity (eid 1L); pull (eid 1L) [ kw "name", scalar (String "Ivan") ] ] ]
    (q_sources_string db1 [ "1", Db_source db1; "2", Db_source db2 ] "[:find ?e (pull $1 ?e [:name]) :in $1 $2 :where [$1 ?e :age 25]]");
  assert_query_set
    "pull from default named source"
    [ [ Result_entity (eid 1L); pull (eid 1L) [ kw "name", scalar (String "Petr") ] ] ]
    (q_sources_string db1 [ "$", Db_source db2; "1", Db_source db1 ] "[:find ?e (pull ?e [:name]) :in $1 $ :where [$ ?e :age 25]]")

let test_query_pull__test_find_spec () =
  if q_return_string db "[:find (pull ?e [:name]) . :where [?e :age 25]]" <> Query_scalar (Some (pull (eid 2L) [ kw "name", scalar (String "Ivan") ])) then
    failf "scalar pull find spec";
  if q_return_string db "[:find [?e (pull ?e [:name])] :where [?e :age 25]]" <> Query_tuple (Some [ Result_entity (eid 2L); pull (eid 2L) [ kw "name", scalar (String "Ivan") ] ]) then
    failf "tuple pull find spec"

let test_query_pull__test_find_spec_input () =
  if
    q_return_string
      ~inputs:[ Arg_scalar (Result_value (Vector [ Keyword "name" ])) ]
      db
      "[:find (pull ?e ?p) . :in $ ?p :where [(ground 2) ?e]]"
    <> Query_scalar (Some (pull (eid 2L) [ kw "name", scalar (String "Ivan") ]))
  then
    failf "pull find spec accepts dynamic pattern input"

let test_query_pull__test_aggregates () =
  let value_many = Schema.spec ~cardinality:(Many) ?unique:(None) ~indexed:(false) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) () in
  let db =
    empty_db ~schema:[ "value", value_many ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "name", One_value (String "Petr"); "value", Many_values [ Int64 10L; Int64 20L; Int64 30L; Int64 40L ] ] }
         ; Entity { db_id = Some (Entity_id (eid 2L)); attrs = [ "name", One_value (String "Ivan"); "value", Many_values [ Int64 14L; Int64 16L ] ] }
         ; Entity { db_id = Some (Entity_id (eid 3L)); attrs = [ "name", One_value (String "Oleg"); "value", One_value (Int64 1L) ] }
         ]
  in
  assert_query_set
    "pull with aggregates"
    [ [ Result_entity (eid 1L); pull (eid 1L) [ kw "name", scalar (String "Petr") ]; Result_value (Int64 10L); Result_value (Int64 40L) ]
    ; [ Result_entity (eid 2L); pull (eid 2L) [ kw "name", scalar (String "Ivan") ]; Result_value (Int64 14L); Result_value (Int64 16L) ]
    ; [ Result_entity (eid 3L); pull (eid 3L) [ kw "name", scalar (String "Oleg") ]; Result_value (Int64 1L); Result_value (Int64 1L) ]
    ]
    (q_string db "[:find ?e (pull ?e [:name]) (min ?v) (max ?v) :where [?e :value ?v]]")

let test_query_pull__test_lookup_refs () =
  let db =
    empty_db ~schema:[ "name", Schema.spec ~cardinality:(One) ?unique:(Some Identity) ~indexed:(true) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) () ] ()
    |> db_with (datoms db Eavt |> List.map (fun d -> Raw_datom d))
  in
  assert_query_set
    "pull accepts lookup refs in query inputs"
    [ [ Result_value (Vector [ Keyword "name"; String "Petr" ]); Result_value (Int64 44L); pull (eid 1L) [ kw "db/id", scalar (Int64 1L); kw "name", scalar (String "Petr") ] ]
    ; [ Result_value (Vector [ Keyword "name"; String "Ivan" ]); Result_value (Int64 25L); pull (eid 2L) [ kw "db/id", scalar (Int64 2L); kw "name", scalar (String "Ivan") ] ]
    ]
    (q_string
       ~inputs:
         [ Arg_collection
             [ Result_value (Vector [ Keyword "name"; String "Ivan" ])
             ; Result_value (Vector [ Keyword "name"; String "Oleg" ])
             ; Result_value (Vector [ Keyword "name"; String "Petr" ])
             ]
         ]
       db
       "[:find ?ref ?a (pull ?ref [:db/id :name]) :in $ [?ref ...] :where [?ref :age ?a] [(>= ?a 18)]]")

let test_query_pull__test_pull_preserves_duplicate_many_ref_datoms () =
  let many =
    Schema.spec ~cardinality:(Many) ?unique:(None) ~indexed:(false) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()
  in
  let unique_identity =
    Schema.spec ~cardinality:(One) ?unique:(Some Identity) ~indexed:(true) ~is_component:((Schema.is_component many)) ~no_history:((Schema.no_history many)) ?doc:((Schema.doc many)) ?value_type:((Schema.value_type many)) ?tuple:(match ((Schema.tuple_attrs many), (Schema.tuple_types many)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()
  in
  let ref_many =
    Schema.spec ~cardinality:((Schema.cardinality many)) ?unique:((Schema.unique many)) ~indexed:((Schema.indexed many)) ~is_component:((Schema.is_component many)) ~no_history:((Schema.no_history many)) ?doc:((Schema.doc many)) ?value_type:(Some RefType) ?tuple:(match ((Schema.tuple_attrs many), (Schema.tuple_types many)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()
  in
  let db =
    init_db
      ~schema:[ "db/ident", unique_identity; "block/title", Schema.spec ~cardinality:(One) ?unique:((Schema.unique many)) ~indexed:((Schema.indexed many)) ~is_component:((Schema.is_component many)) ~no_history:((Schema.no_history many)) ?doc:((Schema.doc many)) ?value_type:((Schema.value_type many)) ?tuple:(match ((Schema.tuple_attrs many), (Schema.tuple_types many)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) (); "block/tags", ref_many ]
      [ datom ~tx:(txid 1L) (eid 2L) "db/ident" (Keyword "logseq.class/Tag")
      ; datom ~tx:(txid 1L) (eid 10L) "block/title" (String "Template")
      ; datom ~tx:(txid 1L) (eid 10L) "block/tags" (Ref (eid 2L))
      ; datom ~tx:(txid 1L) (eid 10L) "block/tags" (Ref (eid 2L))
      ]
  in
  assert_query_set
    "query pull preserves duplicate many ref datoms"
    [ [ pull (eid 10L)
          [ kw "block/tags", many_values
              [ entity (eid 2L) [ kw "db/ident", scalar (Keyword "logseq.class/Tag") ]
              ; entity (eid 2L) [ kw "db/ident", scalar (Keyword "logseq.class/Tag") ]
              ]
          ; kw "block/title", scalar (String "Template")
          ]
      ]
    ]
    (q_string
       db
       "[:find (pull ?b [:block/title {:block/tags [:db/ident]}]) :where [?b :block/tags :logseq.class/Tag]]")

let test_query_pull__test_simple_pull_uses_ref_ident_pattern () =
  let many_ref =
    Schema.spec ~cardinality:(Many) ?unique:(None) ~indexed:(false) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(Some RefType) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()
  in
  let db =
    init_db
      ~schema:[ "block/tags", many_ref ]
      [ datom ~tx:(txid 536870913L) ~added:true (eid 1L) "block/tags" (Ref (eid 2L))
      ; datom ~tx:(txid 536870913L) ~added:true (eid 2L) "db/ident" (Keyword "logseq.class/Tag")
      ; datom ~tx:(txid 536870913L) ~added:true (eid 2L) "block/title" (String "Tag")
      ; datom ~tx:(txid 536870913L) ~added:true (eid 3L) "block/tags" (Ref (eid 2L))
      ]
  in
  (match q_return_string db "[:find (pull ?b [:db/id]) :where [?b :block/tags :logseq.class/Tag]]" with
   | Query_relation rows ->
     assert_query_set
       "simple pull query resolves keyword ident in ref value pattern"
       [ [ pull (eid 1L) [ kw "db/id", scalar (Int64 1L) ] ]; [ pull (eid 3L) [ kw "db/id", scalar (Int64 3L) ] ] ]
       rows
   | _ -> failf "simple pull query should return a relation")

let test_query_pull__test_map_specs_validate_when_rows_are_pulled () =
  let db = empty_db () |> db_with [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "name", One_value (String "Petr") ] } ] in
  assert_query_set
    "query pull defers map-spec schema validation until a row is pulled"
    []
    (q_string db "[:find (pull ?e [{:friend [:name]}]) :where [?e :name \"Ivan\"]]");
  if q_return_string db "[:find (pull ?e [{:friend [:name]}]) . :where [?e :name \"Ivan\"]]" <> Query_scalar None then
    failf "scalar query pull should return nil before validating unused map specs";
  (try
     ignore (q_string db "[:find (pull ?e [{:friend [:name]}]) :where [?e :name \"Petr\"]");
     failf "query pull should validate map-spec schema when a matching row is pulled"
   with
   | Invalid_argument _ -> ())

let test_query_pull__test_unknown_rule_message_matches_upstream () =
  try
    ignore (q_return_string ~inputs:[ Arg_rules [] ] db "[:find (pull ?e [:name]) :in $ % :where (missing-rule ?e :kind \"x\")]");
    failf "unknown rule query should fail"
  with
  | Invalid_argument message ->
    if message <> "Unknown rule 'missing-rule in (missing-rule ?e :kind \"x\")" then
      failf "unknown rule message: %s" message

let () =
  test_query_pull__test_basics ();
  test_query_pull__test_var_pattern ();
  test_query_pull__test_multiple_sources ();
  test_query_pull__test_find_spec ();
  test_query_pull__test_find_spec_input ();
  test_query_pull__test_aggregates ();
  test_query_pull__test_lookup_refs ();
  test_query_pull__test_pull_preserves_duplicate_many_ref_datoms ();
  test_query_pull__test_simple_pull_uses_ref_ident_pattern ();
  test_query_pull__test_map_specs_validate_when_rows_are_pulled ();
  test_query_pull__test_unknown_rule_message_matches_upstream ()
