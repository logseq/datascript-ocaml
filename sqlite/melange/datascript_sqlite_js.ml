external js_set_driver : 'a -> unit = "setDriver"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_has_driver : unit -> bool = "hasDriver"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_open : string -> 'a = "sqlOpen"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_close : 'a -> unit = "sqlClose"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_exec : 'a -> string -> unit = "sqlExec"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_meta_get : 'a -> string -> string Js.undefined = "sqlMetaGet"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_meta_set : 'a -> string -> string -> unit = "sqlMetaSet"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_put : 'a -> string -> string -> string -> unit = "sqlPut"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_put_many : 'a -> string -> (string * string) array -> unit = "sqlPutMany"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_get : 'a -> string -> string -> string Js.undefined = "sqlGet"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_remove : 'a -> string -> string -> unit = "sqlRemove"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_fold :
  'a -> string -> string Js.undefined -> bool -> (string -> string -> bool) -> unit
  = "sqlFold"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_begin : 'a -> unit = "sqlBegin"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_commit : 'a -> unit = "sqlCommit"
  [@@mel.module "./datascript_sqlite_driver.js"]

external js_rollback : 'a -> unit = "sqlRollback"
  [@@mel.module "./datascript_sqlite_driver.js"]

let wrap_js_conn conn : Datascript_sqlite_db.remote_conn =
  {
    close = (fun () -> js_close conn)
  ; exec = (fun sql -> js_exec conn sql)
  ; meta_get = (fun key -> Js.Undefined.toOption (js_meta_get conn key))
  ; meta_set = (fun key value -> js_meta_set conn key value)
  ; put = (fun table key value -> js_put conn table key value)
  ; put_many = (fun table entries -> js_put_many conn table (Array.of_list entries))
  ; get = (fun table key -> Js.Undefined.toOption (js_get conn table key))
  ; remove = (fun table key -> js_remove conn table key)
  ; fold =
      (fun table from_key desc fn ->
        let from =
          match from_key with
          | None -> Js.Undefined.empty
          | Some key -> Js.Undefined.return key
        in
        js_fold conn table from desc fn)
  ; begin_txn = (fun () -> js_begin conn)
  ; commit = (fun () -> js_commit conn)
  ; rollback = (fun () -> js_rollback conn)
  }

let set_driver driver =
  js_set_driver driver;
  Datascript_sqlite_db.set_driver_open (fun path -> wrap_js_conn (js_open path))

let has_driver () = js_has_driver () || Datascript_sqlite_db.has_driver ()
