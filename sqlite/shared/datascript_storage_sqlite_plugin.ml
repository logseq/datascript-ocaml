open Datascript_types

let backend_of_sqlite sqlite =
  let restore_meta () = Datascript_storage_sqlite.restore_meta sqlite in
  let store_meta db = Datascript_storage_sqlite.store_meta sqlite db in
  (* Share path: live indexes use this SQLite db, so mirror sync is unnecessary. *)
  let sync_indexes_to_storage ~since_tx = ignore since_tx in
  let sync_removals_to_storage removed_datoms =
    (* Removals already applied to the shared SQLite indexes during purge. *)
    ignore removed_datoms
  in
  let load_indexes_from_storage _target = () in
  {
    Datascript_storage_protocol.kind = storage_kind_sqlite
  ; restore_meta
  ; store_meta
  ; sync_indexes_to_storage
  ; sync_removals_to_storage
  ; load_indexes_from_storage
  ; index_db = Share_index_db (Sqlite sqlite)
  }

let wrap_sqlite ?check_live db =
  Datascript_storage_protocol.register_backend (backend_of_sqlite db) ?check_live ()
