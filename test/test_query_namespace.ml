open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_query label expected actual =
  if expected <> actual then failf "%s: unexpected query result" label

let test_query_namespace__test_public_query_api () =
  let db =
    empty_db ()
    |> db_with [ Add (Entity_id 1, "name", String "Ivan"); Add (Entity_id 2, "name", String "Oleg") ]
  in
  assert_equal_query
    "Query.q_string exposes relation query API"
    [ [ Result_value (String "Ivan") ]; [ Result_value (String "Oleg") ] ]
    (Query.q_string db "[:find ?name :where [_ :name ?name]]");
  assert_equal_query
    "Query.q_sources_string exposes named sources"
    [ [ Result_value (String "Ivan") ] ]
    (Query.q_sources_string
       (empty_db ())
       [ "people", Db_source db ]
       "[:find ?name :in $people :where [$people _ :name ?name] [(= ?name \"Ivan\")]]");
  if
    Query.q_return_map_string db "[:find ?e ?name :keys id name :where [?e :name ?name]]"
    <> Query_relation_maps
         [ [ Keyword "id", Result_entity 1; Keyword "name", Result_value (String "Ivan") ]
         ; [ Keyword "id", Result_entity 2; Keyword "name", Result_value (String "Oleg") ]
         ]
  then
    failwith "Query.q_return_map_string should expose return-map query API"

let () = test_query_namespace__test_public_query_api ()
