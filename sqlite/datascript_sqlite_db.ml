open Datascript_types

type t =
  { path : string
  ; db : Sqlite3.db
  ; mutable closed : bool
  }

let table_name = function
  | Eavt -> "ds_eavt"
  | Aevt -> "ds_aevt"
  | Avet -> "ds_avet"

let check t sql rc =
  if not (Sqlite3.Rc.is_success rc) then
    invalid_arg
      (Printf.sprintf "SQLite failed (%s) while running %s: %s" (Sqlite3.Rc.to_string rc) sql
         (Sqlite3.errmsg t.db))

let ensure_open t =
  if t.closed then invalid_arg ("SQLite database is closed: " ^ t.path)

let exec_sql t sql =
  ensure_open t;
  check t sql (Sqlite3.exec t.db sql)

let apply_open_pragmas t =
  exec_sql t "PRAGMA journal_mode=WAL;";
  exec_sql t "PRAGMA synchronous=NORMAL;";
  exec_sql t "PRAGMA busy_timeout=5000;";
  exec_sql t "PRAGMA foreign_keys=ON;"

let ensure_schema db =
  List.iter
    (fun index ->
      exec_sql db
        (Printf.sprintf
           "CREATE TABLE IF NOT EXISTS %s (\n\
           \  key BLOB PRIMARY KEY NOT NULL,\n\
           \  value BLOB NOT NULL\n\
            ) WITHOUT ROWID;"
           (table_name index)))
    [ Eavt; Aevt; Avet ];
  exec_sql db
    "CREATE TABLE IF NOT EXISTS ds_meta (\n\
    \  key TEXT PRIMARY KEY NOT NULL,\n\
    \  value BLOB NOT NULL\n\
     ) WITHOUT ROWID;"

let open_path path =
  let db = Sqlite3.db_open path in
  let t = { path; db; closed = false } in
  apply_open_pragmas t;
  ensure_schema t;
  t

let temps_created = ref 0

let close t =
  if not t.closed then (
    if not (Sqlite3.db_close t.db) then invalid_arg ("failed to close SQLite database: " ^ t.path);
    t.closed <- true)

let create_temp () =
  let t =
    open_path
      (Filename.temp_file ~temp_dir:(Filename.get_temp_dir_name ()) "datascript_sqlite" ".sqlite")
  in
  Gc.finalise
    (fun t ->
      if not t.closed then close t)
    t;
  incr temps_created;
  if !temps_created mod 64 = 0 then Gc.full_major ();
  t

let sync t =
  ensure_open t;
  exec_sql t "PRAGMA synchronous=FULL;";
  exec_sql t "PRAGMA wal_checkpoint(FULL);";
  exec_sql t "PRAGMA synchronous=NORMAL;"

let meta_get db key =
  ensure_open db;
  let sql = "SELECT value FROM ds_meta WHERE key = ?;" in
  let stmt = Sqlite3.prepare db.db sql in
  Fun.protect
    ~finally:(fun () -> check db sql (Sqlite3.finalize stmt))
    (fun () ->
      check db sql (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (Sqlite3.column_blob stmt 0)
      | Sqlite3.Rc.DONE -> None
      | rc ->
        check db sql rc;
        None)

let meta_set db key value =
  ensure_open db;
  let sql = "REPLACE INTO ds_meta (key, value) VALUES (?, ?);" in
  let stmt = Sqlite3.prepare db.db sql in
  Fun.protect
    ~finally:(fun () -> check db sql (Sqlite3.finalize stmt))
    (fun () ->
      check db sql (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key));
      check db sql (Sqlite3.bind_blob stmt 2 value);
      check db sql (Sqlite3.step stmt))

let with_write_txn db f =
  ensure_open db;
  exec_sql db "BEGIN IMMEDIATE TRANSACTION;";
  (try
     f ();
     exec_sql db "COMMIT;"
   with exn ->
     (try exec_sql db "ROLLBACK;" with _ -> ());
     raise exn)

let put_index_txn index db key value =
  let sql =
    Printf.sprintf "REPLACE INTO %s (key, value) VALUES (?, ?);" (table_name index)
  in
  let stmt = Sqlite3.prepare db.db sql in
  Fun.protect
    ~finally:(fun () -> check db sql (Sqlite3.finalize stmt))
    (fun () ->
      check db sql (Sqlite3.bind_blob stmt 1 key);
      check db sql (Sqlite3.bind_blob stmt 2 value);
      check db sql (Sqlite3.step stmt))

let remove_index_txn index db key =
  let sql = Printf.sprintf "DELETE FROM %s WHERE key = ?;" (table_name index) in
  let stmt = Sqlite3.prepare db.db sql in
  Fun.protect
    ~finally:(fun () -> check db sql (Sqlite3.finalize stmt))
    (fun () ->
      check db sql (Sqlite3.bind_blob stmt 1 key);
      check db sql (Sqlite3.step stmt))

let put_index index db key value =
  with_write_txn db (fun () -> put_index_txn index db key value)

let remove_index index db key = with_write_txn db (fun () -> remove_index_txn index db key)

let get_index index db key =
  ensure_open db;
  let sql = Printf.sprintf "SELECT value FROM %s WHERE key = ?;" (table_name index) in
  let stmt = Sqlite3.prepare db.db sql in
  Fun.protect
    ~finally:(fun () -> check db sql (Sqlite3.finalize stmt))
    (fun () ->
      check db sql (Sqlite3.bind_blob stmt 1 key);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (Sqlite3.column_blob stmt 0)
      | Sqlite3.Rc.DONE -> None
      | rc ->
        check db sql rc;
        None)

let fold_index index db f =
  ensure_open db;
  let sql = Printf.sprintf "SELECT key, value FROM %s ORDER BY key;" (table_name index) in
  let stmt = Sqlite3.prepare db.db sql in
  Fun.protect
    ~finally:(fun () -> check db sql (Sqlite3.finalize stmt))
    (fun () ->
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            f (Sqlite3.column_blob stmt 0) (Sqlite3.column_blob stmt 1);
            loop ()
        | Sqlite3.Rc.DONE -> ()
        | rc -> check db sql rc
      in
      loop ())

let fold_index_prefix index db prefix f =
  ensure_open db;
  let sql =
    Printf.sprintf "SELECT key, value FROM %s WHERE key >= ? ORDER BY key;" (table_name index)
  in
  let stmt = Sqlite3.prepare db.db sql in
  let prefix_len = String.length prefix in
  Fun.protect
    ~finally:(fun () -> check db sql (Sqlite3.finalize stmt))
    (fun () ->
      check db sql (Sqlite3.bind_blob stmt 1 prefix);
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let key = Sqlite3.column_blob stmt 0 in
            if String.length key < prefix_len || String.sub key 0 prefix_len <> prefix then ()
            else (
              f key (Sqlite3.column_blob stmt 1);
              loop ())
        | Sqlite3.Rc.DONE -> ()
        | rc -> check db sql rc
      in
      loop ())

let fold_index_range_until index db ?from_key ?stop f =
  ensure_open db;
  let sql =
    match from_key with
    | None -> Printf.sprintf "SELECT key, value FROM %s ORDER BY key;" (table_name index)
    | Some _ ->
      Printf.sprintf "SELECT key, value FROM %s WHERE key >= ? ORDER BY key;" (table_name index)
  in
  let stmt = Sqlite3.prepare db.db sql in
  Fun.protect
    ~finally:(fun () -> check db sql (Sqlite3.finalize stmt))
    (fun () ->
      (match from_key with
       | None -> ()
       | Some key -> check db sql (Sqlite3.bind_blob stmt 1 key));
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let key = Sqlite3.column_blob stmt 0 in
            let value = Sqlite3.column_blob stmt 1 in
            (match stop with
             | Some stop when stop key value -> ()
             | _ ->
                 f key value;
                 loop ())
        | Sqlite3.Rc.DONE -> ()
        | rc -> check db sql rc
      in
      loop ())

let fold_index_range_desc_until index db ?hi_key ?stop f =
  ensure_open db;
  let sql =
    match hi_key with
    | None -> Printf.sprintf "SELECT key, value FROM %s ORDER BY key DESC;" (table_name index)
    | Some _ ->
      Printf.sprintf "SELECT key, value FROM %s WHERE key <= ? ORDER BY key DESC;"
        (table_name index)
  in
  let stmt = Sqlite3.prepare db.db sql in
  Fun.protect
    ~finally:(fun () -> check db sql (Sqlite3.finalize stmt))
    (fun () ->
      (match hi_key with
       | None -> ()
       | Some key -> check db sql (Sqlite3.bind_blob stmt 1 key));
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let key = Sqlite3.column_blob stmt 0 in
            let value = Sqlite3.column_blob stmt 1 in
            (match stop with
             | Some stop when stop key value -> ()
             | _ ->
                 f key value;
                 loop ())
        | Sqlite3.Rc.DONE -> ()
        | rc -> check db sql rc
      in
      loop ())

let copy_index index from_db to_db =
  fold_index index from_db (fun key value -> put_index index to_db key value)
