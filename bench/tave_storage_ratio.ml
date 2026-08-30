open Datascript

let indexed =
  { cardinality = One
  ; unique = None
  ; indexed = true
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let schema =
  [ "name", indexed; "age", indexed; "salary", indexed; "sex", indexed ]

let names = [| "Ivan"; "Petr"; "Sergey"; "Oleg"; "Yuri"; "Dmitry"; "Fedor"; "Denis" |]

let now_ms () = int_of_float (Unix.gettimeofday () *. 1000.)

let build size ~sqlite ~prune =
  if prune then set_tave_retention_days 30 else set_tave_retention_days 0;
  let path =
    Filename.temp_file "tave-ratio" (if sqlite then ".sqlite3" else ".mdb")
  in
  Sys.remove path;
  let session_close, storage =
    if sqlite then
      let s = Datascript_sqlite.open_session path in
      (fun () -> Datascript_sqlite.close s), storage_of_handle (Datascript_sqlite.storage s)
    else
      let s = Datascript_lmdb.open_session path in
      (fun () -> Datascript_lmdb.close s), storage_of_handle (Datascript_lmdb.storage s)
  in
  let db = empty_db ~schema ~storage () in
  let base_ms = now_ms () - 3_600_000 in
  let batch = 500 in
  let rec loop db i =
    if i >= size then db
    else
      let hi = min size (i + batch) in
      let tx =
        List.init (hi - i) (fun j ->
            let e = i + j + 1 in
            let name = names.(e mod Array.length names) in
            [ Add (Entity_id e, "name", String name)
            ; Add (Entity_id e, "age", Int (e mod 100))
            ; Add (Entity_id e, "salary", Int (e * 10))
            ; Add (Entity_id e, "sex", String (if e mod 2 = 0 then "m" else "f"))
            ])
        |> List.concat
      in
      let r =
        transact ~tx_meta:[ "db/txInstant", Instant (base_ms + i) ] db tx
      in
      loop r.db_after hi
  in
  let db = loop db 0 in
  store ~storage db;
  path, session_close, db

let sqlite_table_bytes path table =
  let db = Sqlite3.db_open ~mode:`READONLY path in
  let sql = Printf.sprintf "SELECT SUM(LENGTH(key)+LENGTH(value)) FROM %s;" table in
  let stmt = Sqlite3.prepare db sql in
  let n =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT i -> Int64.to_int i
      | Sqlite3.Data.FLOAT f -> int_of_float f
      | _ -> 0)
    | _ -> 0
  in
  ignore (Sqlite3.finalize stmt);
  ignore (Sqlite3.db_close db);
  n

let sqlite_table_count path table =
  let db = Sqlite3.db_open ~mode:`READONLY path in
  let sql = Printf.sprintf "SELECT COUNT(*) FROM %s;" table in
  let stmt = Sqlite3.prepare db sql in
  let n =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> (
      match Sqlite3.column stmt 0 with
      | Sqlite3.Data.INT i -> Int64.to_int i
      | _ -> 0)
    | _ -> 0
  in
  ignore (Sqlite3.finalize stmt);
  ignore (Sqlite3.db_close db);
  n

let file_size path =
  try (Unix.stat path).Unix.st_size with _ -> 0

let report_sqlite label path =
  let tables = [ "ds_eavt"; "ds_aevt"; "ds_avet"; "ds_tave"; "ds_meta" ] in
  List.iter
    (fun t ->
      Printf.printf "%s-table\t%s\tbytes=%d\trows=%d\n%!" label t (sqlite_table_bytes path t)
        (sqlite_table_count path t))
    tables;
  let eavt = sqlite_table_bytes path "ds_eavt" in
  let aevt = sqlite_table_bytes path "ds_aevt" in
  let avet = sqlite_table_bytes path "ds_avet" in
  let tave = sqlite_table_bytes path "ds_tave" in
  let three = eavt + aevt + avet in
  let four = three + tave in
  Printf.printf "%s-three-indexes\t%d\n%!" label three;
  Printf.printf "%s-with-tave\t%d\n%!" label four;
  Printf.printf "%s-tave-ratio-vs-three\t%.3f\n%!" label (float tave /. float (max 1 three));
  Printf.printf "%s-total-growth\t%.3f\n%!" label (float four /. float (max 1 three));
  Printf.printf "%s-file\t%d\n%!" label (file_size path)

let () =
  let size = try int_of_string Sys.argv.(1) with _ -> 10000 in
  Printf.printf "size\t%d\n%!" size;
  let path, close, _ = build size ~sqlite:true ~prune:false in
  report_sqlite "sqlite-noprune" path;
  close ();
  let path, close, _ = build size ~sqlite:true ~prune:true in
  report_sqlite "sqlite-retain30d" path;
  close ();
  let path, close, _ = build size ~sqlite:false ~prune:false in
  Printf.printf "lmdb-noprune-file\t%d\n%!" (file_size path);
  close ()
