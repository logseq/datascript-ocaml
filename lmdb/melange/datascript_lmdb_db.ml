open Datascript_types

type js = Js.t

external open_root : string -> js = "open"
  [@@mel.module "./datascript_lmdb_node.js"]

external open_subdb : js -> string -> js = "openDB"
  [@@mel.module "./datascript_lmdb_node.js"]

external js_get : js -> string -> string Js.nullable = "get"
  [@@mel.module "./datascript_lmdb_node.js"]

external js_put : js -> string -> string -> unit = "put"
  [@@mel.module "./datascript_lmdb_node.js"]

external js_remove : js -> string -> unit = "remove"
  [@@mel.module "./datascript_lmdb_node.js"]

external js_sync : js -> unit = "sync"
  [@@mel.module "./datascript_lmdb_node.js"]

external js_close : js -> unit = "close"
  [@@mel.module "./datascript_lmdb_node.js"]

external js_range : js -> (string * string) array = "range"
  [@@mel.module "./datascript_lmdb_node.js"]

external temp_path : unit -> string = "tempPath"
  [@@mel.module "./datascript_lmdb_node.js"]

type t =
  { path : string
  ; env : js
  ; eavt : js
  ; aevt : js
  ; avet : js
  ; meta : js
  ; mutable closed : bool
  }

let remove_path _path = ()

let open_db path =
  let root = open_root path in
  { path; env = root; eavt = open_subdb root "ds/eavt"; aevt = open_subdb root "ds/aevt"
  ; avet = open_subdb root "ds/avet"; meta = open_subdb root "ds/meta"; closed = false
  }

let create_temp () = open_db (temp_path ())

let open_path path = open_db path

let ensure_open db =
  if db.closed then invalid_arg ("LMDB database is closed: " ^ db.path)

let close db =
  if not db.closed then (
    js_close db.env;
    db.closed <- true)

let sync db =
  ensure_open db;
  js_sync db.env

let meta_get db key =
  ensure_open db;
  match Js.Nullable.toOption (js_get db.meta key) with
  | None -> None
  | Some value -> Some value

let meta_set db key value =
  ensure_open db;
  js_put db.meta key value

let with_write db f =
  ensure_open db;
  f ()

let fold_index index db f =
  ensure_open db;
  let map =
    match index with
    | Eavt -> db.eavt
    | Aevt -> db.aevt
    | Avet -> db.avet
  in
  Array.iter (fun (key, value) -> f key value) (js_range map)

let put_index index db key value =
  ensure_open db;
  let map =
    match index with
    | Eavt -> db.eavt
    | Aevt -> db.aevt
    | Avet -> db.avet
  in
  js_put map key value

let remove_index index db key =
  ensure_open db;
  let map =
    match index with
    | Eavt -> db.eavt
    | Aevt -> db.aevt
    | Avet -> db.avet
  in
  js_remove map key
