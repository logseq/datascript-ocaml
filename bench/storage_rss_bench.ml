(* Storage RSS microbench: 50k people-like entities, common ops, GC/close release.
   Backends: memory (temp LMDB), lmdb-file, sqlite-file. *)

open Datascript

type backend =
  | Memory
  | Lmdb_file
  | Sqlite_file

let backend_label = function
  | Memory -> "memory"
  | Lmdb_file -> "lmdb"
  | Sqlite_file -> "sqlite"

let now_ms () = Unix.gettimeofday () *. 1000.

let rss_bytes () =
  let channel = Unix.open_process_in (Printf.sprintf "ps -o rss= -p %d" (Unix.getpid ())) in
  let line = try input_line channel with End_of_file -> "0" in
  ignore (Unix.close_process_in channel);
  line |> String.trim |> int_of_string |> fun kb -> kb * 1024

let heap_bytes () =
  let stat = Gc.stat () in
  stat.live_words * (Sys.word_size / 8)

let settle () =
  Gc.full_major ();
  Unix.sleepf 0.05

let report backend phase =
  settle ();
  Printf.printf "backend\t%s\n%!" (backend_label backend);
  Printf.printf "phase\t%s\n%!" phase;
  Printf.printf "rss-bytes\t%d\n%!" (rss_bytes ());
  Printf.printf "heap-bytes\t%d\n%!" (heap_bytes ())

let ensure_dir path =
  let rec loop dir =
    if dir = "" || dir = Filename.current_dir_name || Sys.file_exists dir then ()
    else (
      loop (Filename.dirname dir);
      try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  loop path

let remove_path path =
  List.iter
    (fun p -> if Sys.file_exists p then Sys.remove p)
    [ path; path ^ "-wal"; path ^ "-shm"; path ^ "-lock" ]

type session =
  | No_session
  | Lmdb of Datascript_lmdb.session
  | Sqlite of Datascript_sqlite.session

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
  [ "name", indexed
  ; "last-name", indexed
  ; "sex", indexed
  ; "age", indexed
  ; "salary", indexed
  ]

let names = [| "Ivan"; "Petr"; "Sergei"; "Oleg"; "Yuri"; "Dmitry"; "Fedor"; "Denis" |]
let last_names = [| "Ivanov"; "Petrov"; "Sidorov"; "Kovalev"; "Kuznetsov"; "Voronoi" |]
let sexes = [| "male"; "female" |]

type rng = { mutable state : int32 }

let rng seed = { state = Int32.of_int seed }

let next_int rng bound =
  rng.state <- Int32.add (Int32.mul rng.state 1_664_525l) 1_013_904_223l;
  Int32.(to_int (rem (logand (shift_right_logical rng.state 1) 0x3fffffffl) (of_int bound)))

let rand_nth rng values = values.(next_int rng (Array.length values))
let rand_sex rng = sexes.(next_int rng 997 mod Array.length sexes)

let person rng i =
  Entity
    { db_id = Some (Temp_id (string_of_int i))
    ; attrs =
        [ "name", One_value (String (rand_nth rng names))
        ; "last-name", One_value (String (rand_nth rng last_names))
        ; "sex", One_value (Keyword (rand_sex rng))
        ; "age", One_value (Int (next_int rng 100))
        ; "salary", One_value (Int (next_int rng 100_000))
        ]
    }

let chunk = 10_000

let build_db ~storage size =
  let r = rng 1 in
  let rec loop i db =
    if i > size then db
    else
      let hi = min size (i + chunk - 1) in
      let tx = List.init (hi - i + 1) (fun k -> person r (i + k)) in
      let db = db_with tx db in
      (match storage with
       | Some storage ->
         store db;
         collect_garbage storage
       | None -> ());
      Printf.eprintf "built\t%d/%d\trss=%d\n%!" hi size (rss_bytes ());
      loop (hi + 1) db
  in
  match storage with
  | Some storage -> loop 1 (empty_db ~schema ~storage ())
  | None -> loop 1 (empty_db ~schema ())

let update_person rng i =
  Entity
    { db_id = Some (Entity_id (i + 1))
    ; attrs =
        [ "age", One_value (Int (next_int rng 100))
        ; "salary", One_value (Int (next_int rng 100_000))
        ]
    }

let blackhole = ref 0
let consume n = blackhole := (!blackhole + n) land 0x3fffffff

let query_name_age = lazy (parse_query_string "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]")
let query_salary = lazy (parse_query_string "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]")
let query_sex =
  lazy (parse_query_string "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e :sex :male]]")

let run_queries db =
  consume (Seq.fold_left (fun n _ -> n + 1) 0 (datoms db Aevt ~a:"name" ()));
  consume (List.length (q db (Lazy.force query_name_age)));
  consume (List.length (q db (Lazy.force query_salary)));
  consume (List.length (q db (Lazy.force query_sex)));
  for entity_id = 1 to 100 do
    match pull db [ Pull_attr "name"; Pull_attr "age"; Pull_attr "salary" ] (Entity_id entity_id) with
    | None -> consume 0
    | Some entity -> consume (List.length entity.pulled_attrs)
  done

type config =
  { size : int
  ; tx_size : int
  ; data_dir : string
  ; backends : backend list
  }

let default_config =
  { size = 50_000
  ; tx_size = 200
  ; data_dir = Filename.concat (Filename.get_temp_dir_name ()) "datascript-storage-rss"
  ; backends = [ Memory; Lmdb_file; Sqlite_file ]
  }

let parse_backends value =
  value
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (( <> ) "")
  |> List.map (function
    | "memory" -> Memory
    | "lmdb" -> Lmdb_file
    | "sqlite" -> Sqlite_file
    | other -> invalid_arg ("unknown backend: " ^ other))

let parse_args () =
  let config = ref default_config in
  let rec loop = function
    | [] -> !config
    | "--size" :: v :: rest ->
        config := { !config with size = int_of_string v };
        loop rest
    | "--tx-size" :: v :: rest ->
        config := { !config with tx_size = int_of_string v };
        loop rest
    | "--data-dir" :: v :: rest ->
        config := { !config with data_dir = v };
        loop rest
    | "--backends" :: v :: rest ->
        config := { !config with backends = parse_backends v };
        loop rest
    | arg :: _ -> invalid_arg ("unknown argument: " ^ arg)
  in
  Sys.argv |> Array.to_list |> List.tl |> loop

let open_backend ~data_dir backend size =
  match backend with
  | Memory -> No_session, None, None
  | Lmdb_file ->
      let path = Filename.concat data_dir (Printf.sprintf "rss-lmdb-%d.mdb" size) in
      remove_path path;
      let session = Datascript_lmdb.open_session path in
      let storage = storage_of_handle (Datascript_lmdb.storage session) in
      Lmdb session, Some storage, Some path
  | Sqlite_file ->
      let path = Filename.concat data_dir (Printf.sprintf "rss-sqlite-%d.sqlite3" size) in
      remove_path path;
      let session = Datascript_sqlite.open_session path in
      let storage = storage_of_handle (Datascript_sqlite.storage session) in
      Sqlite session, Some storage, Some path

let close_session = function
  | No_session -> ()
  | Lmdb session -> Datascript_lmdb.close session
  | Sqlite session -> Datascript_sqlite.close session

let disk_bytes path =
  List.fold_left
    (fun total suffix ->
      let p = if suffix = "" then path else path ^ suffix in
      if Sys.file_exists p then total + (Unix.stat p).st_size else total)
    0
    [ ""; "-wal"; "-shm"; "-lock" ]

let run_backend config backend =
  Printf.printf "storage\t%s\n%!" (backend_label backend);
  report backend "baseline";
  let session, storage, path = open_backend ~data_dir:config.data_dir backend config.size in
  let build_start = now_ms () in
  let db = ref (build_db ~storage config.size) in
  (match storage with
   | Some storage -> store !db; collect_garbage storage
   | None -> ());
  Printf.printf "build-ms\t%.1f\n%!" (now_ms () -. build_start);
  (match path with
   | Some path -> Printf.printf "disk-bytes\t%d\n%!" (disk_bytes path)
   | None -> Printf.printf "disk-bytes\t0\n%!");
  report backend "after-build";
  run_queries !db;
  report backend "after-queries";
  let r = rng 99 in
  let tx = List.init config.tx_size (fun i -> update_person r i) in
  db := db_with tx !db;
  (match storage with
   | Some storage -> store !db; collect_garbage storage
   | None -> ());
  report backend "after-tx";
  run_queries !db;
  report backend "after-queries-2";
  settle ();
  report backend "after-gc-full-major";
  Gc.compact ();
  Unix.sleepf 0.05;
  report backend "after-gc-compact";
  db := empty_db ();
  settle ();
  Gc.compact ();
  Unix.sleepf 0.1;
  report backend "after-drop-db";
  close_session session;
  settle ();
  Gc.compact ();
  Unix.sleepf 0.1;
  report backend "after-close";
  (match path with
   | Some path -> remove_path path
   | None -> ());
  Printf.printf "blackhole\t%d\n%!" !blackhole

let () =
  let config = parse_args () in
  ensure_dir config.data_dir;
  Printf.printf "runtime\tOCaml\n%!";
  Printf.printf "size\t%d\n%!" config.size;
  Printf.printf "tx-size\t%d\n%!" config.tx_size;
  Printf.printf "data-dir\t%s\n%!" config.data_dir;
  Printf.printf "bench\tstorage-rss\n%!";
  List.iter (run_backend config) config.backends
