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
