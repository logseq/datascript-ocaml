open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_query_set label expected actual =
  if List.sort compare expected <> List.sort compare actual then failf "%s" label

let kw name = Keyword name
let scalar value = Pulled_scalar value
let pull id attrs = Result_pull { pulled_id = id; pulled_attrs = attrs }

let db =
  empty_db ()
  |> db_with
       [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 44) ] }
       ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 25) ] }
       ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg"); "age", One_value (Int 11) ] }
       ]

let test_query_pull__test_basics () =
  assert_query_set
    "pull in find relation"
    [ [ pull 1 [ kw "name", scalar (String "Petr") ] ]; [ pull 2 [ kw "name", scalar (String "Ivan") ] ] ]
    (q_string db "[:find (pull ?e [:name]) :where [?e :age ?a] [(>= ?a 18)]]");
  assert_query_set
    "pull can be mixed with scalars"
    [ [ Result_entity 1; Result_value (Int 44); pull 1 [ kw "name", scalar (String "Petr") ] ]
    ; [ Result_entity 2; Result_value (Int 25); pull 2 [ kw "name", scalar (String "Ivan") ] ]
    ]
    (q_string db "[:find ?e ?a (pull ?e [:name]) :where [?e :age ?a] [(>= ?a 18)]]")

let test_query_pull__test_var_pattern () =
  assert_query_set
    "dynamic pull pattern"
    [ [ pull 1 [ kw "name", scalar (String "Petr") ] ]; [ pull 2 [ kw "name", scalar (String "Ivan") ] ] ]
    (q_string
       ~inputs:[ Arg_scalar (Result_value (Vector [ Keyword "name" ])) ]
       db
       "[:find (pull ?e ?pattern) :in $ ?pattern :where [?e :age ?a] [(>= ?a 18)]]")

let test_query_pull__test_multiple_sources () =
  let db1 = empty_db () |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 25) ] } ] in
  let db2 = empty_db () |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 25) ] } ] in
  assert_query_set
    "pull from named source"
    [ [ Result_entity 1; pull 1 [ kw "name", scalar (String "Ivan") ] ] ]
    (q_sources_string db1 [ "1", Db_source db1; "2", Db_source db2 ] "[:find ?e (pull $1 ?e [:name]) :in $1 $2 :where [$1 ?e :age 25]]");
  assert_query_set
    "pull from default named source"
    [ [ Result_entity 1; pull 1 [ kw "name", scalar (String "Petr") ] ] ]
    (q_sources_string db1 [ "$", Db_source db2; "1", Db_source db1 ] "[:find ?e (pull ?e [:name]) :in $1 $ :where [$ ?e :age 25]]")

let test_query_pull__test_find_spec () =
  if q_return_string db "[:find (pull ?e [:name]) . :where [?e :age 25]]" <> Query_scalar (Some (pull 2 [ kw "name", scalar (String "Ivan") ])) then
    failf "scalar pull find spec";
  if q_return_string db "[:find [?e (pull ?e [:name])] :where [?e :age 25]]" <> Query_tuple (Some [ Result_entity 2; pull 2 [ kw "name", scalar (String "Ivan") ] ]) then
    failf "tuple pull find spec"

let test_query_pull__test_find_spec_input () =
  if
    q_return_string
      ~inputs:[ Arg_scalar (Result_value (Vector [ Keyword "name" ])) ]
      db
      "[:find (pull ?e ?p) . :in $ ?p :where [(ground 2) ?e]]"
    <> Query_scalar (Some (pull 2 [ kw "name", scalar (String "Ivan") ]))
  then
    failf "pull find spec accepts dynamic pattern input"

let test_query_pull__test_aggregates () =
  let value_many = { cardinality = Many; unique = None; indexed = false; is_component = false; no_history = false; doc = None; value_type = None; tuple_attrs = None; tuple_types = None } in
  let db =
    empty_db ~schema:[ "value", value_many ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Petr"); "value", Many_values [ Int 10; Int 20; Int 30; Int 40 ] ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Ivan"); "value", Many_values [ Int 14; Int 16 ] ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg"); "value", One_value (Int 1) ] }
         ]
  in
  assert_query_set
    "pull with aggregates"
    [ [ Result_entity 1; pull 1 [ kw "name", scalar (String "Petr") ]; Result_value (Int 10); Result_value (Int 40) ]
    ; [ Result_entity 2; pull 2 [ kw "name", scalar (String "Ivan") ]; Result_value (Int 14); Result_value (Int 16) ]
    ; [ Result_entity 3; pull 3 [ kw "name", scalar (String "Oleg") ]; Result_value (Int 1); Result_value (Int 1) ]
    ]
    (q_string db "[:find ?e (pull ?e [:name]) (min ?v) (max ?v) :where [?e :value ?v]]")

let test_query_pull__test_lookup_refs () =
  let db =
    empty_db ~schema:[ "name", { cardinality = One; unique = Some Identity; indexed = true; is_component = false; no_history = false; doc = None; value_type = None; tuple_attrs = None; tuple_types = None } ] ()
    |> db_with (datoms db Eavt () |> List.map (fun d -> Raw_datom d))
  in
  assert_query_set
    "pull accepts lookup refs in query inputs"
    [ [ Result_value (Vector [ Keyword "name"; String "Petr" ]); Result_value (Int 44); pull 1 [ kw "db/id", scalar (Int 1); kw "name", scalar (String "Petr") ] ]
    ; [ Result_value (Vector [ Keyword "name"; String "Ivan" ]); Result_value (Int 25); pull 2 [ kw "db/id", scalar (Int 2); kw "name", scalar (String "Ivan") ] ]
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

let () =
  test_query_pull__test_basics ();
  test_query_pull__test_var_pattern ();
  test_query_pull__test_multiple_sources ();
  test_query_pull__test_find_spec ();
  test_query_pull__test_find_spec_input ();
  test_query_pull__test_aggregates ();
  test_query_pull__test_lookup_refs ()
