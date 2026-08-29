open Datascript_types

type t = Datascript_lmdb_db.t

let create_temp () = Datascript_lmdb_db.create_temp ()
let open_path path = Datascript_lmdb_db.open_path path
let close = Datascript_lmdb_db.close
let sync = Datascript_lmdb_db.sync

let store_meta lmdb db =
  Datascript_storage_meta.store_meta (Datascript_lmdb_db.meta_set lmdb) db;
  sync lmdb

let restore_meta lmdb = Datascript_storage_meta.restore_meta (Datascript_lmdb_db.meta_get lmdb)

let sync_indexes from_lmdb to_lmdb =
  if from_lmdb != to_lmdb then
    Datascript_lmdb_db.with_write_txn to_lmdb (fun txn ->
      List.iter
        (fun index ->
          Datascript_lmdb_db.copy_index_txn index txn from_lmdb to_lmdb)
        [ Eavt; Aevt; Avet; Tave ])
