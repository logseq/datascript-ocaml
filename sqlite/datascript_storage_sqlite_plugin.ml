open Datascript_types

let backend_of_sqlite sqlite =
  let restore_meta () = Datascript_storage_sqlite.restore_meta sqlite in
  let store_meta db = Datascript_storage_sqlite.store_meta sqlite db in
  let sync_indexes_to_storage ~since_tx eavt aevt avet =
    Datascript_storage_sqlite.sync_append_since_tx ~since_tx Eavt (Datascript_lmdb_index.db_of eavt) sqlite;
    Datascript_storage_sqlite.sync_append_since_tx ~since_tx Aevt (Datascript_lmdb_index.db_of aevt) sqlite;
    Datascript_storage_sqlite.sync_append_since_tx ~since_tx Avet (Datascript_lmdb_index.db_of avet) sqlite
  in
  let sync_removals_to_storage removed_datoms =
    Datascript_storage_sqlite.remove_datoms removed_datoms sqlite
  in
  let load_indexes_from_storage target_lmdb =
    Datascript_storage_sqlite.copy_indexes_to_lmdb sqlite target_lmdb
  in
  {
    Datascript_storage_protocol.kind = storage_kind_sqlite
  ; restore_meta
  ; store_meta
  ; sync_indexes_to_storage
  ; sync_removals_to_storage
  ; load_indexes_from_storage
  ; index_db = Separate_index_db
  }

let wrap_sqlite ?check_live db =
  Datascript_storage_protocol.register_backend (backend_of_sqlite db) ?check_live ()
