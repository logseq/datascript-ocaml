open Alcotest
open Datascript

let check_bool = Test_alcotest_support.check_bool
let expect_invalid_arg_msg = Test_alcotest_support.expect_invalid_arg_msg

let temp_db_path name =
  let path = Filename.temp_file name ".sqlite" in
  Sys.remove path;
  path

let indexed =
  { cardinality = One
  ; unique = Some Identity
  ; indexed = true
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = Some StringType
  ; tuple_attrs = None
  ; tuple_types = None
  }

let age_indexed =
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

let datoms db index ?e ?a ?v ?tx () =
  Datascript.datoms db index ?e ?a ?v ?tx () |> List.of_seq

let test_storage_roundtrip () =
  let path = temp_db_path "datascript-sqlite-package" in
  let session = Datascript_sqlite.open_session path in
  let storage = storage_of_handle (Datascript_sqlite.storage session) in
  let db = empty_db ~schema:[ "todo/id", indexed ] ~storage () in
  check_bool "empty_db shares SQLite index handle" true
    (db_shares_storage_index storage db);
  let report =
    transact
      db
      [ Add (Temp_id "todo-1", "todo/id", String "todo-1")
      ; Add (Temp_id "todo-1", "todo/title", String "Move storage into datascript")
      ]
  in
  store ~storage report.db_after;
  let restored =
    match restore storage with
    | Some db -> db
    | None -> failwith "expected SQLite storage to restore a database"
  in
  check_bool "restore shares SQLite index handle" true
    (db_shares_storage_index storage restored);
  let entity =
    match entity restored (Lookup_ref ("todo/id", String "todo-1")) with
    | Some entity -> entity
    | None -> failwith "expected restored todo entity"
  in
  check_bool "expected restored entity title" true
    (entity_attr entity "todo/title" = Some (One_value (String "Move storage into datascript")));
  check_bool "expected SQLite storage backend" true (kind_of storage = storage_kind_sqlite);
  Datascript_sqlite.close session

let test_session_close_blocks_use () =
  let path = temp_db_path "datascript-sqlite-session-close" in
  let session = Datascript_sqlite.open_session path in
  let storage = storage_of_handle (Datascript_sqlite.storage session) in
  Datascript_sqlite.close session;
  expect_invalid_arg_msg "SQLite session is closed" (fun () -> ensure_live storage)

let test_reopen_preserves_data () =
  let path = temp_db_path "datascript-sqlite-reopen" in
  let session = Datascript_sqlite.open_session path in
  let storage = storage_of_handle (Datascript_sqlite.storage session) in
  let db = empty_db ~schema:[ "todo/id", indexed ] ~storage () in
  let report =
    transact db [ Add (Temp_id "todo-1", "todo/id", String "persisted") ]
  in
  store ~storage report.db_after;
  collect_garbage storage;
  Datascript_sqlite.close session;
  let session = Datascript_sqlite.open_session path in
  let storage = storage_of_handle (Datascript_sqlite.storage session) in
  let restored =
    match restore storage with
    | Some db -> db
    | None -> failwith "expected reopen restore"
  in
  check_bool "reopen restore shares SQLite index" true
    (db_shares_storage_index storage restored);
  (match entity restored (Lookup_ref ("todo/id", String "persisted")) with
   | Some _ -> ()
   | None -> failwith "expected persisted entity after reopen");
  Datascript_sqlite.close session

let test_temporal_views () =
  let path = temp_db_path "datascript-sqlite-temporal" in
  let session = Datascript_sqlite.open_session path in
  let storage = storage_of_handle (Datascript_sqlite.storage session) in
  let db =
    empty_db ~schema:[ "name", indexed; "age", age_indexed ] ~storage ()
    |> db_with [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 30) ]
  in
  let tx1 = basis_tx db in
  store ~storage db;
  let db = db_with [ Add (Entity_id 1, "age", Int 31) ] db in
  store ~storage db;
  let restored =
    match restore storage with
    | Some db -> db
    | None -> failwith "restore should read sqlite temporal db"
  in
  let current_ages =
    datoms restored Eavt ~a:"age" ()
    |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  in
  Test_alcotest_support.check_int_list "sqlite current age 31" [ 31 ] current_ages;
  let past_ages =
    datoms (as_of tx1 restored) Eavt ~a:"age" ()
    |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  in
  Test_alcotest_support.check_int_list "sqlite as_of age 30" [ 30 ] past_ages;
  let hist_ages =
    datoms (history restored) Eavt ~a:"age" ()
    |> List.filter (fun d -> d.added)
    |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
    |> List.sort compare
  in
  Test_alcotest_support.check_int_list "sqlite history ages" [ 30; 31 ] hist_ages;
  Datascript_sqlite.close session

let test_tave_since_attr_scan () =
  let path = temp_db_path "datascript-sqlite-tave" in
  let session = Datascript_sqlite.open_session path in
  let storage = storage_of_handle (Datascript_sqlite.storage session) in
  let db = empty_db ~schema:[ "name", indexed; "age", age_indexed ] ~storage () in
  let r1 =
    transact ~tx_meta:[ "db/txInstant", Instant 1_000 ]
      db
      [ Add (Temp_id "a", "name", String "Alice"); Add (Temp_id "a", "age", Int 20) ]
  in
  let tx1 = r1.db_after.max_tx in
  let r2 =
    transact ~tx_meta:[ "db/txInstant", Instant 2_000 ]
      r1.db_after
      [ Add (Temp_id "b", "name", String "Bob"); Add (Temp_id "b", "age", Int 30) ]
  in
  store ~storage r2.db_after;
  (* since tx1: only Bob's age should appear via since-bounded AEVT (TAVE path). *)
  let ages =
    datoms (since tx1 r2.db_after) Aevt ~a:"age" ()
    |> List.filter (fun d -> d.added)
    |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  in
  Test_alcotest_support.check_int_list "since+AEVT ages via TAVE" [ 30 ] ages;
  set_tave_retention_days 30;
  check (Alcotest.int) "default retention days" 30 (tave_retention_days ());
  set_tave_retention_days 7;
  check (Alcotest.int) "adjusted retention days" 7 (tave_retention_days ());
  set_tave_retention_days 30;
  prune_tave_to_retention r2.db_after;
  Datascript_sqlite.close session

let test_memory_session () =
  let session = Datascript_sqlite.open_memory () in
  let storage = storage_of_handle (Datascript_sqlite.storage session) in
  let db = empty_db ~schema:[ "todo/id", indexed ] ~storage () in
  let report =
    transact db [ Add (Temp_id "todo-1", "todo/id", String "memory") ]
  in
  store ~storage report.db_after;
  let restored =
    match restore storage with
    | Some db -> db
    | None -> failwith "expected memory sqlite restore"
  in
  check_bool "memory sqlite shares index" true (db_shares_storage_index storage restored);
  (match entity restored (Lookup_ref ("todo/id", String "memory")) with
   | Some _ -> ()
   | None -> failwith "expected memory sqlite entity");
  Datascript_sqlite.close session

let () =
  run "sqlite package"
    [
      ( "session"
      , [
          test_case "storage roundtrip" `Quick test_storage_roundtrip
        ; test_case "session close blocks use" `Quick test_session_close_blocks_use
        ; test_case "reopen preserves data" `Quick test_reopen_preserves_data
        ; test_case "temporal views" `Quick test_temporal_views
        ; test_case "tave since attr scan" `Quick test_tave_since_attr_scan
        ; test_case "memory session" `Quick test_memory_session
        ] )
    ]
