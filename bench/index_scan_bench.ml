(* Index-scan microbench: measure LMDB vs SQLite storage engines directly.
   Avoids the shared query evaluator so differences are mostly Index I/O + decode.

   Phases per backend × size:
   1. build large db on disk (batched tx), durable sync, close
   2. cold open + restore (no prior warmup in this process for that file)
   3. cold single-shot index scans
   4. hot scans after warmup (OS page cache + stmt/cursor warm)

   Optional --drop-caches attempts to flush page cache between close and reopen
   (requires write access to /proc/sys/vm/drop_caches). *)

open Datascript

type backend = Lmdb | Sqlite

let backend_label = function
  | Lmdb -> "lmdb"
  | Sqlite -> "sqlite"

let now_ms () = Unix.gettimeofday () *. 1000.

let ensure_dir path =
  let rec loop dir =
    if dir = "" || dir = Filename.current_dir_name || Sys.file_exists dir then ()
    else (
      loop (Filename.dirname dir);
      try Unix.mkdir dir 0o755 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  loop path

let remove_file path = if Sys.file_exists path then Sys.remove path

let remove_path path =
  remove_file path;
  List.iter remove_file [ path ^ "-wal"; path ^ "-shm"; path ^ "-lock" ]

let file_size path =
  if Sys.file_exists path then (Unix.stat path).st_size else 0

let disk_footprint path =
  List.fold_left
    (fun total suffix -> total + file_size (if suffix = "" then path else path ^ suffix))
    0
    [ ""; "-wal"; "-shm"; "-lock" ]

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

(* Build in chunks so a 1M entity tx list does not dominate RSS. *)
let build_chunk_size = 25_000

let consume_seq seq = Seq.fold_left (fun n _ -> n + 1) 0 seq

let blackhole = ref 0

let consume_count n = blackhole := !blackhole + n

type config =
  { sizes : int list
  ; data_dir : string
  ; warmup : int
  ; repeats : int
  ; drop_caches : bool
  ; backends : backend list
  }

let default_config =
  { sizes = [ 50_000 ]
  ; data_dir = Filename.concat (Filename.get_temp_dir_name ()) "datascript-index-scan"
  ; warmup = 20
  ; repeats = 5
  ; drop_caches = false
  ; backends = [ Lmdb; Sqlite ]
  }

let parse_int_list value =
  value
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (( <> ) "")
  |> List.map int_of_string

let parse_backends value =
  value
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (( <> ) "")
  |> List.map (function
    | "lmdb" -> Lmdb
    | "sqlite" -> Sqlite
    | other -> invalid_arg ("unknown backend: " ^ other))

let parse_args () =
  let config = ref default_config in
  let rec loop = function
    | [] -> !config
    | "--size" :: v :: rest ->
        config := { !config with sizes = [ int_of_string v ] };
        loop rest
    | "--sizes" :: v :: rest ->
        config := { !config with sizes = parse_int_list v };
        loop rest
    | "--data-dir" :: v :: rest ->
        config := { !config with data_dir = v };
        loop rest
    | "--warmup" :: v :: rest ->
        config := { !config with warmup = int_of_string v };
        loop rest
    | "--repeats" :: v :: rest ->
        config := { !config with repeats = int_of_string v };
        loop rest
    | "--drop-caches" :: rest ->
        config := { !config with drop_caches = true };
        loop rest
    | "--backends" :: v :: rest ->
        config := { !config with backends = parse_backends v };
        loop rest
    | arg :: _ -> invalid_arg ("unknown argument: " ^ arg)
  in
  Sys.argv |> Array.to_list |> List.tl |> loop

let try_drop_caches enabled =
  if not enabled then Printf.printf "drop-caches\tskipped\n%!"
  else (
    ignore (Unix.system "sync");
    let oc_opt =
      try Some (open_out "/proc/sys/vm/drop_caches") with
      | Sys_error _ -> None
    in
    match oc_opt with
    | None -> Printf.printf "drop-caches\tunavailable\n%!"
    | Some oc ->
        (try
           output_string oc "3\n";
           close_out oc;
           Printf.printf "drop-caches\tok\n%!"
         with Sys_error msg ->
           (try close_out_noerr oc with _ -> ());
           Printf.printf "drop-caches\tfailed:%s\n%!" msg))

let time_once f =
  let start = now_ms () in
  let result = f () in
  now_ms () -. start, result

let time_median ~warmup ~repeats f =
  for _ = 1 to warmup do
    ignore (f ())
  done;
  let samples =
    List.init repeats (fun _ ->
        let start = now_ms () in
        ignore (f ());
        now_ms () -. start)
  in
  let sorted = List.sort compare samples in
  List.nth sorted (List.length sorted / 2)

let format_ms ms = Printf.sprintf "%.3f" ms

type session_handle =
  | Lmdb_session of Datascript_lmdb.session
  | Sqlite_session of Datascript_sqlite.session

let open_backend backend path =
  match backend with
  | Lmdb ->
      let session = Datascript_lmdb.open_session path in
      Lmdb_session session, storage_of_handle (Datascript_lmdb.storage session)
  | Sqlite ->
      let session = Datascript_sqlite.open_session path in
      Sqlite_session session, storage_of_handle (Datascript_sqlite.storage session)

let close_backend = function
  | Lmdb_session session -> Datascript_lmdb.close session
  | Sqlite_session session -> Datascript_sqlite.close session

let db_path ~data_dir backend size =
  let ext = match backend with Lmdb -> "mdb" | Sqlite -> "sqlite3" in
  Filename.concat data_dir
    (Printf.sprintf "index-scan-%s-%d.%s" (backend_label backend) size ext)

let build_db ~storage size =
  let r = rng 1 in
  let rec loop i db =
    if i > size then db
    else
      let chunk_end = min size (i + build_chunk_size - 1) in
      let tx = List.init (chunk_end - i + 1) (fun k -> person r (i + k)) in
      let db = db_with tx db in
      (* Persist chunk boundaries so Share backends flush index pages and the
         OCaml db does not retain a giant pending tx history. *)
      store db;
      loop (chunk_end + 1) db
  in
  loop 1 (empty_db ~schema ~storage ())

let build_on_disk ~data_dir backend size =
  let path = db_path ~data_dir backend size in
  remove_path path;
  let handle, storage = open_backend backend path in
  let build_ms, () =
    time_once (fun () ->
        let db = build_db ~storage size in
        store db;
        collect_garbage storage;
        ignore db)
  in
  close_backend handle;
  path, build_ms, disk_footprint path

type scan =
  { name : string
  ; run : db -> int
  }

let scans ~size =
  let mid = max 1 (size / 2) in
  [ { name = "point-eavt-entity"
    ; run =
        (fun db ->
          consume_seq (datoms db Eavt ~e:mid ()))
    }
  ; { name = "prefix-aevt-name"
    ; run =
        (fun db ->
          fold_datoms (fun n _ -> n + 1) 0 db Aevt ~a:"name" ())
    }
  ; { name = "exact-avet-name-ivan"
    ; run =
        (fun db ->
          consume_seq (datoms db Avet ~a:"name" ~v:(String "Ivan") ()))
    }
  ; { name = "range-avet-salary-50k-60k"
    ; run =
        (fun db ->
          consume_seq (index_range db "salary" ~start:(Int 50_000) ~stop:(Int 60_000) ()))
    }
  ; { name = "seek-eavt-mid-take-100"
    ; run =
        (fun db ->
          seek_datoms db Eavt ~e:mid ()
          |> Seq.take 100
          |> consume_seq)
    }
  ; { name = "scan-eavt-all"
    ; run =
        (fun db ->
          fold_datoms (fun n _ -> n + 1) 0 db Eavt ())
    }
  ]

let run_backend ~warmup ~repeats ~drop_caches ~data_dir size backend =
  let label = backend_label backend in
  Printf.printf "storage\t%s\n%!" label;
  let path, build_ms, bytes = build_on_disk ~data_dir backend size in
  Printf.printf "path\t%s\n%!" path;
  Printf.printf "disk-bytes\t%d\n%!" bytes;
  Printf.printf "build-ms\t%s\n%!" (format_ms build_ms);
  try_drop_caches drop_caches;
  let open_ms, (handle, db) =
    time_once (fun () ->
        let handle, storage = open_backend backend path in
        match restore storage with
        | Some db -> handle, db
        | None -> failwith (label ^ " restore failed"))
  in
  Printf.printf "cold-open-restore-ms\t%s\n%!" (format_ms open_ms);
  Fun.protect
    ~finally:(fun () ->
      close_backend handle;
      remove_path path)
    (fun () ->
      List.iter
        (fun scan ->
          let cold_ms, cold_count = time_once (fun () -> scan.run db) in
          consume_count cold_count;
          Printf.printf "cold-%s-ms\t%s\n%!" scan.name (format_ms cold_ms);
          Printf.printf "cold-%s-count\t%d\n%!" scan.name cold_count;
          let hot_ms =
            time_median ~warmup ~repeats (fun () -> consume_count (scan.run db))
          in
          Printf.printf "hot-%s-ms\t%s\n%!" scan.name (format_ms hot_ms))
        (scans ~size))

let run_size config size =
  Printf.printf "size\t%d\n%!" size;
  List.iter
    (run_backend ~warmup:config.warmup ~repeats:config.repeats
       ~drop_caches:config.drop_caches ~data_dir:config.data_dir size)
    config.backends

let main () =
  let config = parse_args () in
  if config.sizes = [] then invalid_arg "at least one --size / --sizes entry required";
  ensure_dir config.data_dir;
  Printf.printf "runtime\tOCaml\n%!";
  Printf.printf "data-dir\t%s\n%!" config.data_dir;
  Printf.printf "warmup\t%d\n%!" config.warmup;
  Printf.printf "repeats\t%d\n%!" config.repeats;
  Printf.printf "bench\tindex-scan\n%!";
  List.iter (run_size config) config.sizes;
  Printf.eprintf "blackhole=%d\n%!" !blackhole

let () = main ()
