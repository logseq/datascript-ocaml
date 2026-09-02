open Datascript_types

type t =
  { path : string
  ; db : Sqlite3.db
  ; mutable closed : bool
  ; stmts : (string, Sqlite3.stmt) Hashtbl.t
  ; mutable sync_count : int
  }

let table_name = function
  | Eavt -> "ds_eavt"
  | Aevt -> "ds_aevt"
  | Avet -> "ds_avet"
  | Tave -> "ds_tave"

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

(* Reuse prepared statements: build and point lookups previously prepared+finalized
   once per datom / scan, which dominated Share SQLite cost vs PSS+memory. *)
let cached_stmt t sql =
  ensure_open t;
  match Hashtbl.find_opt t.stmts sql with
  | Some stmt ->
      check t sql (Sqlite3.reset stmt);
      check t sql (Sqlite3.clear_bindings stmt);
      stmt
  | None ->
      let stmt = Sqlite3.prepare t.db sql in
      Hashtbl.add t.stmts sql stmt;
      stmt

let with_cached_stmt t sql f =
  let stmt = cached_stmt t sql in
  f stmt

let apply_open_pragmas t =
  exec_sql t "PRAGMA journal_mode=WAL;";
  exec_sql t "PRAGMA synchronous=NORMAL;";
  exec_sql t "PRAGMA busy_timeout=5000;";
  exec_sql t "PRAGMA foreign_keys=ON;";
  (* Larger page cache + mmap cuts repeated B-tree seeks for Share index scans. *)
  exec_sql t "PRAGMA cache_size=-65536;";
  exec_sql t "PRAGMA temp_store=MEMORY;";
  exec_sql t "PRAGMA mmap_size=268435456;"

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
    [ Eavt; Aevt; Avet; Tave ];
  exec_sql db
    "CREATE TABLE IF NOT EXISTS ds_meta (\n\
    \  key TEXT PRIMARY KEY NOT NULL,\n\
    \  value BLOB NOT NULL\n\
     ) WITHOUT ROWID;"

let open_path path =
  let db = Sqlite3.db_open path in
  let t = { path; db; closed = false; stmts = Hashtbl.create 32; sync_count = 0 } in
  apply_open_pragmas t;
  ensure_schema t;
  t

let temps_created = ref 0

let finalize_cached_stmts t =
  Hashtbl.iter
    (fun _sql stmt -> ignore (Sqlite3.finalize stmt))
    t.stmts;
  Hashtbl.clear t.stmts

let close t =
  if not t.closed then (
    finalize_cached_stmts t;
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
  t.sync_count <- t.sync_count + 1;
  (* Cached prepared statements keep the WAL busy; drop them before checkpoint. *)
  finalize_cached_stmts t;
  exec_sql t "PRAGMA synchronous=FULL;";
  exec_sql t "PRAGMA wal_checkpoint(FULL);";
  exec_sql t "PRAGMA synchronous=NORMAL;"

let sync_count t = t.sync_count

let meta_get db key =
  let sql = "SELECT value FROM ds_meta WHERE key = ?;" in
  with_cached_stmt db sql (fun stmt ->
      check db sql (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT key));
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (Sqlite3.column_blob stmt 0)
      | Sqlite3.Rc.DONE -> None
      | rc ->
        check db sql rc;
        None)

let meta_set db key value =
  let sql = "REPLACE INTO ds_meta (key, value) VALUES (?, ?);" in
  with_cached_stmt db sql (fun stmt ->
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

(* Bulk loads issue hundreds of thousands of REPLACE steps; NORMAL sync per page
   dominates. Turn sync off for the txn and restore NORMAL afterward. *)
let with_bulk_write_txn db f =
  ensure_open db;
  exec_sql db "PRAGMA synchronous=OFF;";
  (try
     with_write_txn db f;
     exec_sql db "PRAGMA synchronous=NORMAL;"
   with exn ->
     (try exec_sql db "PRAGMA synchronous=NORMAL;" with _ -> ());
     raise exn)

let put_index_txn index db key value =
  let sql =
    Printf.sprintf "REPLACE INTO %s (key, value) VALUES (?, ?);" (table_name index)
  in
  with_cached_stmt db sql (fun stmt ->
      check db sql (Sqlite3.bind_blob stmt 1 key);
      check db sql (Sqlite3.bind_blob stmt 2 value);
      check db sql (Sqlite3.step stmt))

(* Multi-row REPLACE cuts prepare/step overhead vs one statement per key. *)
let put_index_chunk_size = 64

let put_index_entries_txn index db entries =
  match entries with
  | [] -> ()
  | _ ->
    let table = table_name index in
    let rec loop = function
      | [] -> ()
      | rest ->
        let chunk, rest =
          let rec take n acc xs =
            if n = 0 then List.rev acc, xs
            else
              match xs with
              | [] -> List.rev acc, []
              | x :: xs -> take (n - 1) (x :: acc) xs
          in
          take put_index_chunk_size [] rest
        in
        let n = List.length chunk in
        let placeholders =
          List.init n (fun _ -> "(?, ?)") |> String.concat ", "
        in
        let sql =
          Printf.sprintf "REPLACE INTO %s (key, value) VALUES %s;" table placeholders
        in
        with_cached_stmt db sql (fun stmt ->
          List.iteri
            (fun i (key, value) ->
              let base = (i * 2) + 1 in
              check db sql (Sqlite3.bind_blob stmt base key);
              check db sql (Sqlite3.bind_blob stmt (base + 1) value))
            chunk;
          check db sql (Sqlite3.step stmt));
        loop rest
    in
    loop entries

let remove_index_txn index db key =
  let sql = Printf.sprintf "DELETE FROM %s WHERE key = ?;" (table_name index) in
  with_cached_stmt db sql (fun stmt ->
      check db sql (Sqlite3.bind_blob stmt 1 key);
      check db sql (Sqlite3.step stmt))

let put_index index db key value =
  with_write_txn db (fun () -> put_index_txn index db key value)

let remove_index index db key = with_write_txn db (fun () -> remove_index_txn index db key)

let get_index index db key =
  let sql = Printf.sprintf "SELECT value FROM %s WHERE key = ?;" (table_name index) in
  with_cached_stmt db sql (fun stmt ->
      check db sql (Sqlite3.bind_blob stmt 1 key);
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW -> Some (Sqlite3.column_blob stmt 0)
      | Sqlite3.Rc.DONE -> None
      | rc ->
        check db sql rc;
        None)

let fold_index index db f =
  let sql = Printf.sprintf "SELECT key, value FROM %s ORDER BY key;" (table_name index) in
  with_cached_stmt db sql (fun stmt ->
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
  let sql =
    Printf.sprintf "SELECT key, value FROM %s WHERE key >= ? ORDER BY key;" (table_name index)
  in
  let prefix_len = String.length prefix in
  with_cached_stmt db sql (fun stmt ->
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
  let sql =
    match from_key with
    | None -> Printf.sprintf "SELECT key, value FROM %s ORDER BY key;" (table_name index)
    | Some _ ->
      Printf.sprintf "SELECT key, value FROM %s WHERE key >= ? ORDER BY key;" (table_name index)
  in
  with_cached_stmt db sql (fun stmt ->
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
  let sql =
    match hi_key with
    | None -> Printf.sprintf "SELECT key, value FROM %s ORDER BY key DESC;" (table_name index)
    | Some _ ->
      Printf.sprintf "SELECT key, value FROM %s WHERE key <= ? ORDER BY key DESC;"
        (table_name index)
  in
  with_cached_stmt db sql (fun stmt ->
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

(* Datalevin-style lazy ranges: pull batches through cached fold statements, then
   re-seek from a continuation key. Avoids holding a live stmt across Seq yields
   (which blocked WAL checkpoint and made point lookups pay prepare-per-scan). *)
let stream_batch_size = 64

type asc_cont =
  | Asc_start
  | Asc_from of { key : string; include_key : bool }
  | Asc_done

let seq_index_range_until index db ?from_key ?stop () =
  ensure_open db;
  let cont =
    ref
      (match from_key with
       | None -> Asc_start
       | Some key -> Asc_from { key; include_key = true })
  in
  let fetch () =
    match !cont with
    | Asc_done -> []
    | _ ->
        let batch = ref [] in
        let count = ref 0 in
        let from_key, include_key =
          match !cont with
          | Asc_start -> None, true
          | Asc_from { key; include_key } -> Some key, include_key
          | Asc_done -> None, true
        in
        let skipping = ref (match from_key with Some _ when not include_key -> true | _ -> false) in
        let hit_end = ref true in
        fold_index_range_until index db ?from_key
          ~stop:(fun key value ->
            if !count >= stream_batch_size then (
              hit_end := false;
              true)
            else if !skipping then false
            else
              match stop with
              | Some stop when stop key value ->
                  cont := Asc_done;
                  true
              | _ -> false)
          (fun key value ->
            if !skipping then (
              match from_key with
              | Some cont_key when key = cont_key -> ()
              | _ ->
                  skipping := false;
                  batch := (key, value) :: !batch;
                  incr count;
                  cont := Asc_from { key; include_key = false })
            else (
              batch := (key, value) :: !batch;
              incr count;
              cont := Asc_from { key; include_key = false }));
        if !hit_end && !count < stream_batch_size then cont := Asc_done;
        List.rev !batch
  in
  let rec stream () =
    match fetch () with
    | [] -> Seq.Nil
    | items ->
        let rec of_list = function
          | [] -> stream
          | x :: xs -> fun () -> Seq.Cons (x, of_list xs)
        in
        of_list items ()
  in
  stream

let seq_index_prefix index db prefix () =
  let prefix_len = String.length prefix in
  seq_index_range_until index db ~from_key:prefix
    ~stop:(fun key _value ->
      String.length key < prefix_len || String.sub key 0 prefix_len <> prefix)
    ()

type desc_cont =
  | Desc_start of string option
  | Desc_after of string
  | Desc_done

let seq_index_range_desc_until index db ?hi_key ?stop () =
  ensure_open db;
  let cont = ref (Desc_start hi_key) in
  let fetch () =
    match !cont with
    | Desc_done -> []
    | _ ->
        let batch = ref [] in
        let count = ref 0 in
        let hi_key, skip_hi =
          match !cont with
          | Desc_start hi -> hi, false
          | Desc_after key -> Some key, true
          | Desc_done -> None, false
        in
        let skipping = ref skip_hi in
        let hit_end = ref true in
        fold_index_range_desc_until index db ?hi_key
          ~stop:(fun key value ->
            if !count >= stream_batch_size then (
              hit_end := false;
              true)
            else if !skipping then false
            else
              match stop with
              | Some stop when stop key value ->
                  cont := Desc_done;
                  true
              | _ -> false)
          (fun key value ->
            if !skipping then (
              match hi_key with
              | Some cont_key when key = cont_key -> ()
              | _ ->
                  skipping := false;
                  batch := (key, value) :: !batch;
                  incr count;
                  cont := Desc_after key)
            else (
              batch := (key, value) :: !batch;
              incr count;
              cont := Desc_after key));
        if !hit_end && !count < stream_batch_size then cont := Desc_done;
        List.rev !batch
  in
  let rec stream () =
    match fetch () with
    | [] -> Seq.Nil
    | items ->
        let rec of_list = function
          | [] -> stream
          | x :: xs -> fun () -> Seq.Cons (x, of_list xs)
        in
        of_list items ()
  in
  stream

let copy_index index from_db to_db =
  fold_index index from_db (fun key value -> put_index index to_db key value)
