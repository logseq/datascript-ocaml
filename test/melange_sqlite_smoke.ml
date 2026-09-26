open Datascript

let fail message = raise (Failure message)

let assert_true label value =
  if not value then fail (label ^ ": expected true")

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

let () =
  (* Default indexes are still in-memory LMDB (hashtable on Melange). *)
  let lmdb_db =
    empty_db () |> db_with [ Add (Entity_id 1, "name", String "Ivan") ]
  in
  (match q_string lmdb_db "[:find ?name :where [1 :name ?name]]" with
   | [ [ Result_value (String "Ivan") ] ] -> ()
   | _ -> fail "Melange LMDB memory query returned an unexpected result");
  let lmdb_storage = memory_storage () in
  assert_true "memory_storage is LMDB memory backend" (kind_of lmdb_storage = storage_kind_memory);
  store ~storage:lmdb_storage lmdb_db;
  (match restore lmdb_storage with
   | Some restored -> (
     match q_string restored "[:find ?name :where [1 :name ?name]]" with
     | [ [ Result_value (String "Ivan") ] ] -> ()
     | _ -> fail "Melange LMDB restore query returned an unexpected result")
   | None -> fail "expected LMDB memory storage to restore")

let () =
  (* SQLite in-memory via create_temp — no host driver required. *)
  let session = Datascript_sqlite.open_memory () in
  let sqlite_storage = storage_of_handle (Datascript_sqlite.storage session) in
  assert_true "memory sqlite session is sqlite" (kind_of sqlite_storage = storage_kind_sqlite);
  let db = empty_db ~schema:[ "todo/id", indexed ] ~storage:sqlite_storage () in
  assert_true "empty_db shares sqlite index" (db_shares_storage_index sqlite_storage db);
  let report =
    transact db
      [ Add (Temp_id "todo-1", "todo/id", String "todo-1")
      ; Add (Temp_id "todo-1", "todo/title", String "SQLite on Melange")
      ]
  in
  store ~storage:sqlite_storage report.db_after;
  let restored =
    match restore sqlite_storage with
    | Some db -> db
    | None -> fail "expected SQLite memory storage to restore"
  in
  (match entity restored (Lookup_ref ("todo/id", String "todo-1")) with
   | Some e -> (
     match entity_attr e "todo/title" with
     | Some (One_value (String "SQLite on Melange")) -> ()
     | _ -> fail "unexpected restored title")
   | None -> fail "expected restored sqlite entity");
  Datascript_sqlite.close session
