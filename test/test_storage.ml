open Alcotest
open Datascript

let check_bool = Test_alcotest_support.check_bool

let datoms_seq = datoms

let datoms db index ?e ?a ?v ?tx () =
  datoms_seq db index ?e ?a ?v ?tx () |> List.of_seq

let assert_equal_triples label expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v) actual in
  if expected <> actual then
    Alcotest.failf "%s: unexpected datoms" label

let indexed =
  { cardinality = One
  ; unique = None
  ; indexed = true
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let small_db ?storage () =
  empty_db ?storage ()
  |> db_with
       [ Add (Entity_id 1, "name", String "Ivan")
       ; Add (Entity_id 2, "name", String "Oleg")
       ; Add (Entity_id 3, "name", String "Petr")
       ]

let test_storage__test_basics () =
  let storage = memory_storage () in
  let db = small_db () in
  store ~storage db;
  check_bool "memory storage should use Memory backend" true
    (kind_of storage = storage_kind_memory);
  (match restore storage with
   | None -> failwith "restore should read stored db"
   | Some restored ->
     assert_equal_triples
       "restore returns stored facts"
       [ 1, "name", String "Ivan"; 2, "name", String "Oleg"; 3, "name", String "Petr" ]
       (datoms restored Eavt ());
     check_bool "settings should expose storage attachment" true
       (List.assoc_opt "storage" (settings restored) = Some (Bool true)));
  let attached_storage = memory_storage () in
  let attached = empty_db ~schema:[ "name", indexed ] ~storage:attached_storage () in
  store attached;
  (match restore attached_storage with
   | None -> failwith "store should use db-attached storage"
   | Some restored ->
     check_bool "restore should preserve schema" true (schema restored = [ "name", indexed ]))

let test_storage__test_restored_db_has_storage () =
  let storage = memory_storage () in
  let db = small_db () in
  store ~storage db;
  let restored =
    match restore storage with
    | Some db -> db
    | None -> failwith "restore should read stored db"
  in
  check_bool "restored db should remain storage-backed" true (Option.is_some restored.storage_ref)

let test_storage__test_conn () =
  let storage = memory_storage () in
  let conn = create_conn ~schema:[ "name", indexed ] ~storage () in
  ignore (transact_conn conn [ Add (Entity_id 1, "name", String "Ivan") ]);
  ignore (transact_conn conn [ Add (Entity_id 2, "name", String "Oleg") ]);
  let restored =
    match restore_conn storage with
    | Some conn -> conn
    | None -> failwith "restore_conn should restore storage-backed conn"
  in
  assert_equal_triples
    "restore_conn returns stored facts"
    [ 1, "name", String "Ivan"; 2, "name", String "Oleg" ]
    (datoms (conn_db restored) Eavt ());
  ignore (transact_conn ~tx_meta:[ "skip-store?", Bool true ] restored [ Add (Entity_id 3, "name", String "Skipped") ]);
  (match restore storage with
   | None -> failwith "storage root should remain available"
   | Some restored_db ->
     assert_equal_triples
       "skip-store transaction is not persisted"
       [ 1, "name", String "Ivan"; 2, "name", String "Oleg" ]
       (datoms restored_db Eavt ()))

let test_storage__test_multi_tx_incremental_store () =
  let storage = memory_storage () in
  let db =
    empty_db ~schema:[ "name", indexed; "age", indexed ] ()
    |> db_with [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 30) ]
  in
  let tx1 = basis_tx db in
  store ~storage db;
  let db = db_with [ Add (Entity_id 1, "age", Int 31) ] db in
  store ~storage db;
  let restored =
    match restore storage with
    | Some db -> db
    | None -> failwith "restore should read incrementally stored db"
  in
  let current_ages =
    datoms restored Eavt ~a:"age" ()
    |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  in
  Test_alcotest_support.check_int_list "restored db should see current age 31" [ 31 ] current_ages;
  let past = as_of tx1 restored in
  let past_ages =
    datoms past Eavt ~a:"age" ()
    |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  in
  Test_alcotest_support.check_int_list "restored as_of should see historical age 30" [ 30 ] past_ages;
  let hist_ages =
    datoms (history restored) Eavt ~a:"age" ()
    |> List.filter (fun d -> d.added)
    |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
    |> List.sort compare
  in
  Test_alcotest_support.check_int_list "restored history should expose both age assertions" [ 30; 31 ] hist_ages

let () =
  run "storage"
    [
      ( "memory"
      , [
          test_case "basics" `Quick test_storage__test_basics
        ; test_case "restored db has storage" `Quick test_storage__test_restored_db_has_storage
        ; test_case "conn" `Quick test_storage__test_conn
        ; test_case "multi tx incremental store" `Quick test_storage__test_multi_tx_incremental_store
        ] )
    ]
