open Datascript_types

type t = Datascript_sqlite_db.t

let create_temp () = Datascript_sqlite_db.create_temp ()
let open_path path = Datascript_sqlite_db.open_path path
let close = Datascript_sqlite_db.close
let sync = Datascript_sqlite_db.sync

let store_meta sqlite_db db =
  Datascript_storage_meta.store_meta (Datascript_sqlite_db.meta_set sqlite_db) db;
  sync sqlite_db

let restore_meta sqlite_db =
  Datascript_storage_meta.restore_meta (Datascript_sqlite_db.meta_get sqlite_db)

let copy_indexes_to_lmdb from_db to_lmdb =
  Datascript_lmdb_db.with_write_txn to_lmdb (fun txn ->
    List.iter
      (fun index ->
        Datascript_sqlite_db.fold_index index from_db (fun key value ->
          Datascript_lmdb_db.put_index_txn index txn to_lmdb key value))
      [ Eavt; Aevt; Avet ])

let decode_entry index key value =
  let datom = Datascript_lmdb_codec.decode_datom_key index key in
  let payload = Datascript_lmdb_codec.decode_datom_value value in
  { datom with v = payload.v }

let remove_datom index sqlite_db datom =
  let key = Datascript_lmdb_codec.encode_datom_key index datom in
  Datascript_sqlite_db.remove_index index sqlite_db key

let sync_append_since_tx ~since_tx index source_lmdb target_db =
  Datascript_sqlite_db.with_write_txn target_db (fun () ->
    Datascript_lmdb_db.fold_index index source_lmdb (fun key value ->
      let datom = decode_entry index key value in
      if datom.tx > since_tx then (
        let key = Datascript_lmdb_codec.encode_datom_key index datom in
        let value = Datascript_lmdb_codec.encode_datom_value datom in
        Datascript_sqlite_db.put_index_txn index target_db key value)))

let remove_datoms datoms target_db =
  if datoms = [] then ()
  else
    Datascript_sqlite_db.with_write_txn target_db (fun () ->
      List.iter
        (fun datom ->
          remove_datom Eavt target_db datom;
          remove_datom Aevt target_db datom;
          remove_datom Avet target_db datom)
        datoms)
