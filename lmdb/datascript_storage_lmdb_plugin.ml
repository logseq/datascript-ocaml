open Datascript_types

let backend_of_lmdb lmdb =
  let restore_meta () = Datascript_storage_lmdb.restore_meta lmdb in
  let store_meta db = Datascript_storage_lmdb.store_meta lmdb db in
  let sync_indexes_to_storage ~since_tx eavt aevt avet =
    Datascript_lmdb_index.sync_append_since_tx ~since_tx eavt lmdb;
    Datascript_lmdb_index.sync_append_since_tx ~since_tx aevt lmdb;
    Datascript_lmdb_index.sync_append_since_tx ~since_tx avet lmdb
  in
  let sync_removals_to_storage removed_datoms =
    let remove index =
      let t = Datascript_lmdb_index.empty index lmdb in
      ignore (Datascript_lmdb_index.remove_datoms removed_datoms t)
    in
    remove Eavt;
    remove Aevt;
    remove Avet
  in
  let load_indexes_from_storage target_lmdb =
    if lmdb != target_lmdb then Datascript_storage_lmdb.sync_indexes lmdb target_lmdb
  in
  {
    Datascript_storage_protocol.kind = storage_kind_lmdb
  ; restore_meta
  ; store_meta
  ; sync_indexes_to_storage
  ; sync_removals_to_storage
  ; load_indexes_from_storage
  ; index_db = Share_index_db lmdb
  }

let wrap_lmdb ?check_live db =
  Datascript_storage_protocol.register_backend (backend_of_lmdb db) ?check_live ()
