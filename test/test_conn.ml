open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_tx_flags label expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v, d.added) actual in
  if actual <> expected then failf "%s: unexpected tx-data" label

let assert_equal_triples label expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v) actual in
  if actual <> expected then failf "%s: unexpected datoms" label

let many =
  { cardinality = Many
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let unique_identity =
  { cardinality = One
  ; unique = Some Identity
  ; indexed = true
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let conn_datoms =
  [ datom ~e:1 ~a:"age" ~v:(Int 17) ()
  ; datom ~e:1 ~a:"name" ~v:(String "Ivan") ()
  ]

let test_conn__test_ways_to_create_conn () =
  let assert_conn label expected_schema expected_datoms conn =
    if schema (conn_db conn) <> expected_schema then
      failf "%s: unexpected schema" label;
    assert_equal_triples label expected_datoms (datoms (conn_db conn) Eavt ())
  in
  assert_conn "create_conn" [] [] (create_conn ());
  assert_conn "create_conn with schema" [ "aka", many ] [] (create_conn ~schema:[ "aka", many ] ());
  assert_conn
    "conn_from_datoms"
    []
    [ 1, "age", Int 17; 1, "name", String "Ivan" ]
    (conn_from_datoms conn_datoms);
  assert_conn
    "conn_from_datoms with schema"
    [ "aka", many ]
    [ 1, "age", Int 17; 1, "name", String "Ivan" ]
    (conn_from_datoms ~schema:[ "aka", many ] conn_datoms);
  assert_conn
    "conn_from_db"
    []
    [ 1, "age", Int 17; 1, "name", String "Ivan" ]
    (conn_from_db (init_db conn_datoms));
  assert_conn
    "conn_from_db with schema"
    [ "aka", many ]
    [ 1, "age", Int 17; 1, "name", String "Ivan" ]
    (conn_from_db (init_db ~schema:[ "aka", many ] conn_datoms))

let test_conn__test_reset_conn_bang () =
  let conn = conn_from_datoms ~schema:[ "aka", many ] conn_datoms in
  let report = ref None in
  ignore (listen_auto conn (fun tx_report -> report := Some tx_report));
  let replacement_datoms =
    [ datom ~e:1 ~a:"age" ~v:(Int 20) ()
    ; datom ~e:1 ~a:"sex" ~v:(Keyword "male") ()
    ]
  in
  let replacement = init_db ~schema:[ "email", unique_identity ] replacement_datoms in
  let reset_db = reset_conn_bang ~tx_meta:[ "meta", Bool true ] conn replacement in
  assert_equal_triples
    "reset_conn_bang returns the replacement db"
    [ 1, "age", Int 20; 1, "sex", Keyword "male" ]
    (datoms reset_db Eavt ());
  assert_equal_triples
    "reset_conn_bang updates conn db"
    [ 1, "age", Int 20; 1, "sex", Keyword "male" ]
    (datoms (conn_db conn) Eavt ());
  if schema (conn_db conn) <> [ "email", unique_identity ] then
    failwith "reset_conn_bang should update schema";
  match !report with
  | None -> failwith "reset_conn_bang should notify listeners"
  | Some report ->
    assert_equal_triples
      "reset report exposes db-before"
      [ 1, "age", Int 17; 1, "name", String "Ivan" ]
      (datoms report.db_before Eavt ());
    assert_equal_triples
      "reset report exposes db-after"
      [ 1, "age", Int 20; 1, "sex", Keyword "male" ]
      (datoms report.db_after Eavt ());
    assert_equal_tx_flags
      "reset report tx-data retracts old datoms and adds new datoms"
      [ 1, "age", Int 17, false
      ; 1, "name", String "Ivan", false
      ; 1, "age", Int 20, true
      ; 1, "sex", Keyword "male", true
      ]
      report.tx_data;
    if report.tx_meta <> [ "meta", Bool true ] then
      failwith "reset_conn_bang report should preserve tx meta"

let () =
  test_conn__test_ways_to_create_conn ();
  test_conn__test_reset_conn_bang ()
