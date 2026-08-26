open Datascript_types
open Lmdb

type t =
  { path : string
  ; env : Env.t
  ; eavt : (string, string, [ `Uni ]) Map.t
  ; aevt : (string, string, [ `Uni ]) Map.t
  ; avet : (string, string, [ `Uni ]) Map.t
  ; meta : (string, string, [ `Uni ]) Map.t
  ; mutable closed : bool
  }

let default_map_size = 1024 * 1024 * 1024
let lock_path path = path ^ "-lock"

let remove_path path =
  if Sys.file_exists path then Sys.remove path;
  let lock = lock_path path in
  if Sys.file_exists lock then Sys.remove lock

let open_env db_path =
  Env.(create Rw ~flags:Flags.no_subdir ~map_size:default_map_size ~max_maps:8 db_path)

let open_named_map env name =
  try Map.open_existing Nodup ~key:Conv.string ~value:Conv.string ~name env
  with Not_found -> Map.create Nodup ~key:Conv.string ~value:Conv.string ~name env

let open_db path =
  remove_path path;
  let env = open_env path in
  { path; env; eavt = open_named_map env "ds/eavt"; aevt = open_named_map env "ds/aevt"
  ; avet = open_named_map env "ds/avet"; meta = open_named_map env "ds/meta"; closed = false
  }

let open_path path = open_db path

let ensure_open db =
  if db.closed then invalid_arg ("LMDB database is closed: " ^ db.path)

let close db =
  if not db.closed then (
    Map.close db.eavt;
    Map.close db.aevt;
    Map.close db.avet;
    Map.close db.meta;
    Env.sync db.env;
    Env.close db.env;
    db.closed <- true)

let temps_created = ref 0

let create_temp () =
  let db =
    open_db
      (Filename.temp_file
         ~temp_dir:(Filename.get_temp_dir_name ())
         "datascript_lmdb"
         ".mdb")
  in
  Gc.finalise
    (fun lmdb ->
      if not lmdb.closed then close lmdb)
    db;
  incr temps_created;
  if !temps_created mod 64 = 0 then Gc.full_major ();
  db

let sync db =
  ensure_open db;
  Env.sync db.env

let map_for_index index db =
  match index with
  | Eavt -> db.eavt
  | Aevt -> db.aevt
  | Avet -> db.avet

let meta_get db key =
  ensure_open db;
  try Some (Map.get db.meta key) with Not_found -> None

let meta_set db key value =
  ensure_open db;
  ignore
    (Txn.go Rw db.env (fun txn ->
       Map.set ~txn db.meta key value;
       ()))

let fold_index index db f =
  ensure_open db;
  let map = map_for_index index db in
  let next = Map.to_dispenser map in
  let rec loop () =
    match next () with
    | None -> ()
    | Some (key, value) ->
        f key value;
        loop ()
  in
  loop ()

let put_index index db key value =
  ensure_open db;
  ignore
    (Txn.go Rw db.env (fun txn ->
       Map.set ~txn (map_for_index index db) key value;
       ()))

let remove_index index db key =
  ensure_open db;
  ignore
    (Txn.go Rw db.env (fun txn ->
       (try Map.remove ~txn (map_for_index index db) key with Not_found -> ());
       ()))
