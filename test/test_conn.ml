open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms ?e ?a ?v ?tx db index =
  datoms_seq ?e ?a ?v ?tx db index |> List.of_seq

let assert_equal_tx_flags label expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v, d.added) actual in
  if actual <> expected then failf "%s: unexpected tx-data" label

let assert_equal_triples label expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v) actual in
  if actual <> expected then failf "%s: unexpected datoms" label

let many =
  Schema.spec ~cardinality:(Many) ?unique:(None) ~indexed:(false) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let unique_identity =
  Schema.spec ~cardinality:(One) ?unique:(Some Identity) ~indexed:(true) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let conn_datoms =
  [ datom (eid 1L) "age" (Int64 17L)
  ; datom (eid 1L) "name" (String "Ivan")
  ]

let test_conn__test_ways_to_create_conn () =
  let assert_conn label expected_schema expected_datoms conn =
    if schema (conn_db conn) <> expected_schema then
      failf "%s: unexpected schema" label;
    assert_equal_triples label expected_datoms (datoms (conn_db conn) Eavt)
  in
  assert_conn "create_conn" [] [] (create_conn ());
  assert_conn "create_conn with schema" [ "aka", many ] [] (create_conn ~schema:[ "aka", many ] ());
  assert_conn
    "conn_from_datoms"
    []
    [ (eid 1L), "age", Int64 17L; (eid 1L), "name", String "Ivan" ]
    (conn_from_datoms conn_datoms);
  assert_conn
    "conn_from_datoms with schema"
    [ "aka", many ]
    [ (eid 1L), "age", Int64 17L; (eid 1L), "name", String "Ivan" ]
    (conn_from_datoms ~schema:[ "aka", many ] conn_datoms);
  assert_conn
    "conn_from_db"
    []
    [ (eid 1L), "age", Int64 17L; (eid 1L), "name", String "Ivan" ]
    (conn_from_db (init_db conn_datoms));
  assert_conn
    "conn_from_db with schema"
    [ "aka", many ]
    [ (eid 1L), "age", Int64 17L; (eid 1L), "name", String "Ivan" ]
    (conn_from_db (init_db ~schema:[ "aka", many ] conn_datoms))

let test_conn__test_reset_conn_bang () =
  let conn = conn_from_datoms ~schema:[ "aka", many ] conn_datoms in
  let report = ref None in
  ignore (listen_auto conn (fun tx_report -> report := Some tx_report));
  let replacement_datoms =
    [ datom (eid 1L) "age" (Int64 20L)
    ; datom (eid 1L) "sex" (Keyword "male")
    ]
  in
  let replacement = init_db ~schema:[ "email", unique_identity ] replacement_datoms in
  let reset_db = Compat.reset_conn_bang ~tx_meta:[ "meta", Bool true ] conn replacement in
  assert_equal_triples
    "Compat.reset_conn_bang returns the replacement db"
    [ (eid 1L), "age", Int64 20L; (eid 1L), "sex", Keyword "male" ]
    (datoms reset_db Eavt);
  assert_equal_triples
    "Compat.reset_conn_bang updates conn db"
    [ (eid 1L), "age", Int64 20L; (eid 1L), "sex", Keyword "male" ]
    (datoms (conn_db conn) Eavt);
  if schema (conn_db conn) <> [ "email", unique_identity ] then
    failwith "Compat.reset_conn_bang should update schema";
  match !report with
  | None -> failwith "Compat.reset_conn_bang should notify listeners"
  | Some report ->
    assert_equal_triples
      "reset report exposes db-before"
      [ (eid 1L), "age", Int64 17L; (eid 1L), "name", String "Ivan" ]
      (datoms report.db_before Eavt);
    assert_equal_triples
      "reset report exposes db-after"
      [ (eid 1L), "age", Int64 20L; (eid 1L), "sex", Keyword "male" ]
      (datoms report.db_after Eavt);
    assert_equal_tx_flags
      "reset report tx-data retracts old datoms and adds new datoms"
      [ (eid 1L), "age", Int64 17L, false
      ; (eid 1L), "name", String "Ivan", false
      ; (eid 1L), "age", Int64 20L, true
      ; (eid 1L), "sex", Keyword "male", true
      ]
      report.tx_data;
    if report.tx_meta <> [ "meta", Bool true ] then
      failwith "Compat.reset_conn_bang report should preserve tx meta"

let () =
  test_conn__test_ways_to_create_conn ();
  test_conn__test_reset_conn_bang ()
