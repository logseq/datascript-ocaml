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

let test_storage_roundtrip () =
  let path = temp_db_path "datascript-sqlite-package" in
  let session = Datascript_sqlite.open_session path in
  let storage = storage_of_handle (Datascript_sqlite.storage session) in
  let db = empty_db ~schema:[ "todo/id", indexed ] ~storage () in
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

let () =
  run "sqlite package"
    [
      ( "session"
      , [
          test_case "storage roundtrip" `Quick test_storage_roundtrip
        ; test_case "session close blocks use" `Quick test_session_close_blocks_use
        ] )
    ]
