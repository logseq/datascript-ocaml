open Datascript
open Internal.Datascript_types

module DT = Internal.Datascript_types

let require condition message =
  if not condition then failwith message

let temp_db_path name =
  let path = Filename.temp_file name ".sqlite" in
  Sys.remove path;
  path

(* The SQLite backend persists the cljs-interop payload layout, so this test
   stays on the [Internal] dialect end to end. *)
let indexed : DT.schema_attr =
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
  let storage = Datascript_sqlite.storage session in
  let db = Internal.empty_db ~schema:[ "todo/id", indexed ] ~storage () in
  let report =
    Internal.transact
      db
      [ Add (Temp_id "todo-1", "todo/id", String "todo-1")
      ; Add (Temp_id "todo-1", "todo/title", String "Move storage into datascript")
      ]
  in
  ignore (Internal.store ~storage report.db_after);
  let restored =
    match Internal.restore storage with
    | Some db -> db
    | None -> failwith "expected SQLite storage to restore a database"
  in
  let entity =
    match Internal.entity restored (Lookup_ref ("todo/id", String "todo-1")) with
    | Some entity -> entity
    | None -> failwith "expected restored todo entity"
  in
  require
    (entity.lookup_attr "todo/title" = Some (One_value (String "Move storage into datascript")))
    "expected restored entity title";
  require
    (List.mem Internal.Storage.root_address (Internal.storage_addresses storage))
    "expected SQLite storage to contain the root address";
  let _packaged_logseq_reader = Logseq_sqlite_storage.inspect in
  Datascript_sqlite.close session

let test_session_close_blocks_use () =
  let path = temp_db_path "datascript-sqlite-session-close" in
  let session = Datascript_sqlite.open_session path in
  let storage = Datascript_sqlite.storage session in
  Datascript_sqlite.close session;
  match storage.storage_list_addresses () with
  | _ -> failwith "expected closed SQLite session to reject storage operations"
  | exception Invalid_argument message ->
      require
        (String.equal message "SQLite session is closed")
        "expected closed session error message"

let () =
  test_storage_roundtrip ();
  test_session_close_blocks_use ()
