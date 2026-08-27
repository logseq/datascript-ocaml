open Datascript_types
open Lmdb

type read_session =
  { txn : Mdb.txn
  }

type lmdb_env_profile = Default | Benchmark

type t =
  { path : string
  ; env : Env.t
  ; eavt : (string, string, [ `Uni ]) Map.t
  ; aevt : (string, string, [ `Uni ]) Map.t
  ; avet : (string, string, [ `Uni ]) Map.t
  ; meta : (string, string, [ `Uni ]) Map.t
  ; profile : lmdb_env_profile
  ; mutable closed : bool
  ; mutable read : read_session option
  }

let default_map_size = 1024 * 1024 * 1024
let lock_path path = path ^ "-lock"

let remove_path path =
  if Sys.file_exists path then Sys.remove path;
  let lock = lock_path path in
  if Sys.file_exists lock then Sys.remove lock

let env_flags = function
  | Default -> Env.Flags.no_subdir
  | Benchmark ->
      (* Match in-memory benchmark backends: skip fsync on commit/close. *)
      Env.Flags.(no_subdir + no_sync + no_meta_sync + write_map)

let open_env db_path profile =
  Env.(create Rw ~flags:(env_flags profile) ~map_size:default_map_size ~max_maps:8 db_path)

let open_named_map env name =
  try Map.open_existing Nodup ~key:Conv.string ~value:Conv.string ~name env
  with Not_found -> Map.create Nodup ~key:Conv.string ~value:Conv.string ~name env

(* Open (or create) without deleting an existing env. Callers that want a fresh
   file must call [remove_path] first — same contract as SQLite [open_path]. *)
let open_db path profile =
  let env = open_env path profile in
  { path; env; eavt = open_named_map env "ds/eavt"; aevt = open_named_map env "ds/aevt"
  ; avet = open_named_map env "ds/avet"; meta = open_named_map env "ds/meta"; profile
  ; closed = false; read = None
  }

let open_path path = open_db path Default

let ensure_open db =
  if db.closed then invalid_arg ("LMDB database is closed: " ^ db.path)

let close db =
  if not db.closed then (
    (match db.read with
     | None -> ()
     | Some { txn } ->
       (try Mdb.txn_abort txn with _ -> ()));
    db.read <- None;
    Map.close db.eavt;
    Map.close db.aevt;
    Map.close db.avet;
    Map.close db.meta;
    (match db.profile with
     | Default -> Env.sync db.env
     | Benchmark -> ());
    Env.close db.env;
    db.closed <- true)

let temps_created = ref 0

let create_temp ?(profile = Default) () =
  let path =
    Filename.temp_file ~temp_dir:(Filename.get_temp_dir_name ()) "datascript_lmdb" ".mdb"
  in
  remove_path path;
  let db = open_db path profile in
  Gc.finalise
    (fun lmdb ->
      if not lmdb.closed then close lmdb)
    db;
  incr temps_created;
  if !temps_created mod 64 = 0 then Gc.full_major ();
  db

let create_benchmark_temp () = create_temp ~profile:Benchmark ()

let sync db =
  ensure_open db;
  match db.profile with
  | Default -> Env.sync db.env
  | Benchmark -> ()

let map_for_index index db =
  match index with
  | Eavt -> db.eavt
  | Aevt -> db.aevt
  | Avet -> db.avet

let invalidate_read db =
  match db.read with
  | None -> ()
  | Some { txn } ->
    (try Mdb.txn_abort txn with _ -> ());
    db.read <- None

let mdb_env env =
  (* Lmdb.Env.t is Mdb.env; the public interface hides the alias. *)
  (Obj.magic env : Mdb.env)

let read_session db =
  match db.read with
  | Some session -> session
  | None ->
    let txn = Mdb.txn_begin (mdb_env db.env) None Env.Flags.read_only in
    let session = { txn } in
    db.read <- Some session;
    session

let ro_txn mdb_txn =
  (* Ro Txn.t wraps Mdb.txn; reuse a long-lived read transaction for index scans. *)
  (Obj.magic mdb_txn : [ `Read ] Txn.t)

let with_read_cursor index db f =
  let session = read_session db in
  let map = map_for_index index db in
  Cursor.go Ro ~txn:(ro_txn session.txn) map f

let meta_get db key =
  ensure_open db;
  let session = read_session db in
  try Some (Map.get ~txn:(ro_txn session.txn) db.meta key) with Not_found -> None

let meta_set db key value =
  ensure_open db;
  ignore
    (Txn.go Rw db.env (fun txn ->
       Map.set ~txn db.meta key value;
       ()))

let with_write_txn db f =
  ensure_open db;
  invalidate_read db;
  ignore
    (Txn.go Rw db.env (fun txn ->
       f txn;
       ()))

let put_index_txn index txn db key value =
  Map.set ~txn (map_for_index index db) key value

let remove_index_txn index txn db key =
  try Map.remove ~txn (map_for_index index db) key with Not_found -> ()

let put_index index db key value =
  with_write_txn db (fun txn -> put_index_txn index txn db key value)

let remove_index index db key =
  with_write_txn db (fun txn -> remove_index_txn index txn db key)

let get_index index db key =
  ensure_open db;
  let session = read_session db in
  try Some (Map.get ~txn:(ro_txn session.txn) (map_for_index index db) key) with Not_found -> None

let fold_index index db f =
  ensure_open db;
  (try
     with_read_cursor index db (fun cursor ->
       (try ignore (Cursor.first cursor) with Not_found -> raise Exit);
       let rec loop () =
         let key, value =
           try Cursor.current cursor
           with Not_found -> raise Exit
         in
         f key value;
         try
           ignore (Cursor.next cursor);
           loop ()
         with Not_found -> raise Exit
       in
       loop ())
   with Exit -> ())

let fold_index_prefix index db prefix f =
  ensure_open db;
  let prefix_len = String.length prefix in
  (try
     with_read_cursor index db (fun cursor ->
       (try ignore (Cursor.seek_range cursor prefix) with Not_found -> raise Exit);
       let rec loop () =
         let key, value =
           try Cursor.current cursor
           with Not_found -> raise Exit
         in
         if String.length key < prefix_len || String.sub key 0 prefix_len <> prefix then raise Exit;
         f key value;
         try
           ignore (Cursor.next cursor);
           loop ()
         with Not_found -> raise Exit
       in
       loop ())
   with Exit -> ())

let fold_index_range index db ?from_key ?to_key f =
  ensure_open db;
  (try
     with_read_cursor index db (fun cursor ->
       (match from_key with
        | None -> (
          try ignore (Cursor.first cursor) with Not_found -> raise Exit)
        | Some key -> (
          try ignore (Cursor.seek_range cursor key) with Not_found -> raise Exit));
       let rec loop () =
         let key, value =
           try Cursor.current cursor
           with Not_found -> raise Exit
         in
         (match to_key with
          | Some bound when String.compare key bound > 0 -> raise Exit
          | _ -> ());
         f key value;
         try
           ignore (Cursor.next cursor);
           loop ()
         with Not_found -> raise Exit
       in
       loop ())
   with Exit -> ())

let fold_index_range_until index db ?from_key ?stop f =
  ensure_open db;
  (try
     with_read_cursor index db (fun cursor ->
       (match from_key with
        | None -> (
          try ignore (Cursor.first cursor) with Not_found -> raise Exit)
        | Some key -> (
          try ignore (Cursor.seek_range cursor key) with Not_found -> raise Exit));
       let rec loop () =
         let key, value =
           try Cursor.current cursor
           with Not_found -> raise Exit
         in
         (match stop with
          | Some stop when stop key value -> raise Exit
          | _ -> ());
         f key value;
         try
           ignore (Cursor.next cursor);
           loop ()
         with Not_found -> raise Exit
       in
       loop ())
   with Exit -> ())

let copy_index_txn index txn from_db to_db =
  fold_index index from_db (fun key value ->
    put_index_txn index txn to_db key value)
