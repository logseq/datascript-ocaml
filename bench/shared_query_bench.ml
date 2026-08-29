open Datascript

(* Align with the shared 20k people query suite and timing protocol. *)

type storage_backend =
  | Memory_lmdb_nosync
  | Lmdb_file
  | Sqlite_file

type config =
  { size : int
  ; warmup_ms : float
  ; sample_ms : float
  ; repeats : int
  ; step : int
  ; jit_warmup : int
  ; query : string option
  ; storages : storage_backend list
  ; data_dir : string
  }

let default_config =
  { size = 20_000
  ; warmup_ms = 200.
  ; sample_ms = 200.
  ; repeats = 2
  ; step = 10
  ; jit_warmup = 100
  ; query = None
  ; storages = [ Memory_lmdb_nosync ]
  ; data_dir = Filename.get_temp_dir_name ()
  }

let storage_label = function
  | Memory_lmdb_nosync -> "memory-lmdb-nosync"
  | Lmdb_file -> "lmdb"
  | Sqlite_file -> "sqlite"

let parse_storage_list value =
  value
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.map (function
    | "memory-lmdb-nosync" | "memory" -> Memory_lmdb_nosync
    | "lmdb" -> Lmdb_file
    | "sqlite" -> Sqlite_file
    | other ->
      invalid_arg
        ("unknown storage "
        ^ other
        ^ " (expected: memory-lmdb-nosync|lmdb|sqlite, comma-separated)"))

let int_from_env name default =
  match Sys.getenv_opt name with
  | Some value -> int_of_string value
  | None -> default

let float_from_env name default =
  match Sys.getenv_opt name with
  | Some value -> float_of_string value
  | None -> default

let query_from_env () =
  match Sys.getenv_opt "BENCH_QUERY" with
  | Some "" -> None
  | Some value -> Some value
  | None -> None

let config_from_env base =
  { base with
    warmup_ms = float_from_env "BENCH_WARMUP_MS" base.warmup_ms
  ; sample_ms = float_from_env "BENCH_SAMPLE_MS" base.sample_ms
  ; repeats = int_from_env "BENCH_REPEATS" base.repeats
  ; jit_warmup = int_from_env "BENCH_JIT_WARMUP" base.jit_warmup
  ; query = (match query_from_env () with Some query -> Some query | None -> base.query)
  }

let parse_args () =
  let config = ref (config_from_env default_config) in
  let set_size value = config := { !config with size = int_of_string value } in
  let set_warmup value = config := { !config with warmup_ms = float_of_string value } in
  let set_sample_ms value = config := { !config with sample_ms = float_of_string value } in
  let set_repeats value = config := { !config with repeats = int_of_string value } in
  let set_jit_warmup value = config := { !config with jit_warmup = int_of_string value } in
  let set_query value = config := { !config with query = Some value } in
  let set_storage value =
    config :=
      { !config with
        storages =
          (match value with
           | "all" -> [ Memory_lmdb_nosync; Lmdb_file; Sqlite_file ]
           | "compare" -> [ Lmdb_file; Sqlite_file ]
           | other -> parse_storage_list other)
      }
  in
  let set_data_dir value = config := { !config with data_dir = value } in
  let rec loop = function
    | [] -> !config
    | "--size" :: value :: rest ->
      set_size value;
      loop rest
    | "--warmup-ms" :: value :: rest ->
      set_warmup value;
      loop rest
    | "--sample-ms" :: value :: rest ->
      set_sample_ms value;
      loop rest
    | "--repeats" :: value :: rest ->
      set_repeats value;
      loop rest
    | "--jit-warmup" :: value :: rest ->
      set_jit_warmup value;
      loop rest
    | "--query" :: value :: rest ->
      set_query value;
      loop rest
    | "--storage" :: value :: rest ->
      set_storage value;
      loop rest
    | "--data-dir" :: value :: rest ->
      set_data_dir value;
      loop rest
    | arg :: _ -> invalid_arg ("unknown benchmark argument: " ^ arg)
  in
  Sys.argv |> Array.to_list |> List.tl |> loop

let now_ms () = Unix.gettimeofday () *. 1000.

let median values =
  let sorted = List.sort Float.compare values in
  List.nth sorted (List.length sorted / 2)

let format_ms value =
  if value > 1. then Printf.sprintf "%.2f" value
  else if value > 0.01 then Printf.sprintf "%.3f" value
  else Printf.sprintf "%.4f" value

let blackhole = ref 0

let consume_rows rows =
  (* Keep the result live without a second full walk; the query already
     materializes the list. Matching reference benches that discard results. *)
  match rows with
  | [] -> ()
  | first :: rest ->
    blackhole :=
      (!blackhole + List.length first + if rest == [] then 0 else 1) land 0x3fffffff

let dotime duration_ms step f =
  let start = now_ms () in
  let deadline = start +. duration_ms in
  let rec loop iterations =
    for _ = 1 to step do
      f ()
    done;
    let iterations = iterations + step in
    if now_ms () < deadline then loop iterations else (now_ms () -. start) /. float iterations
  in
  loop step

let bench config f =
  ignore (dotime config.warmup_ms config.step f);
  let samples = List.init config.repeats (fun _ -> dotime config.sample_ms config.step f) in
  median samples

let indexed =
  {
    cardinality = One
  ; unique = None
  ; indexed = true
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let ref_many =
  {
    cardinality = Many
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = Some RefType
  ; tuple_attrs = None
  ; tuple_types = None
  }

let schema =
  [ "name", indexed
  ; "last-name", indexed
  ; "sex", indexed
  ; "age", indexed
  ; "salary", indexed
  ; "follows", ref_many
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

(* See test_shared_queries.ml: decorrelate sex from name under this LCG. *)
let rand_sex rng = sexes.(next_int rng 997 mod Array.length sexes)

let random_man rng i =
  let name = rand_nth rng names in
  let last_name = rand_nth rng last_names in
  let sex = rand_sex rng in
  let age = next_int rng 100 in
  let salary = next_int rng 100_000 in
  Entity
    {
      db_id = Some (Entity_id i)
    ; attrs =
        [ "name", One_value (String name)
        ; "last-name", One_value (String last_name)
        ; "sex", One_value (Keyword sex)
        ; "age", One_value (Int age)
        ; "salary", One_value (Int salary)
        ]
    }

let follow_rules =
  Parser.parse_rules
    (QueryFormVector
       [ QueryFormVector
           [ QueryFormVector [ QueryFormSymbol "follow"; QueryFormSymbol "?e1"; QueryFormSymbol "?e2" ]
           ; QueryFormVector
               [ QueryFormSymbol "?e1"; QueryFormKeyword "follows"; QueryFormSymbol "?e2" ]
           ] ])

(* Canonical EDN strings for cross-runtime result equality (match Clojure pr-str). *)
let edn_of_value = function
  | Nil -> "nil"
  | Bool true -> "true"
  | Bool false -> "false"
  | Int i -> string_of_int i
  | Float f when float_of_int (int_of_float f) = f -> string_of_int (int_of_float f)
  | Float f ->
    let s = Printf.sprintf "%.15g" f in
    if String.contains s '.' || String.contains s 'e' || String.contains s 'E' then s
    else s ^ ".0"
  | String s -> "\"" ^ String.escaped s ^ "\""
  | Keyword k -> ":" ^ k
  | Symbol s -> s
  | Uuid u -> "#uuid \"" ^ u ^ "\""
  | Instant i -> string_of_int i
  | Ref e -> string_of_int e
  | Regex r -> "#\"" ^ String.escaped r ^ "\""
  | _ -> "nil"

let edn_vector items = "[" ^ String.concat " " items ^ "]"

let edn_of_result_cell = function
  | Result_value v -> edn_of_value v
  | Result_entity e -> string_of_int e
  | Result_attr a -> ":" ^ a
  | Result_db _ -> "$"
  | Result_pull _ -> "nil"

let edn_of_q_rows rows =
  rows
  |> List.map (fun row -> edn_vector (List.map edn_of_result_cell row))
  |> List.sort String.compare
  |> edn_vector

type query_case =
  { name : string
  ; run : db -> unit
  ; result_edn : db -> string
  }

let mk_q name query_string =
  let parsed = parse_query_string query_string in
  {
    name
  ; run = (fun db -> consume_rows (Datascript.q db parsed))
  ; result_edn = (fun db -> edn_of_q_rows (Datascript.q db parsed))
  }

let mk_q_inputs name query_string inputs =
  let parsed = parse_query_string query_string in
  {
    name
  ; run = (fun db -> consume_rows (Datascript.q ~inputs db parsed))
  ; result_edn = (fun db -> edn_of_q_rows (Datascript.q ~inputs db parsed))
  }

let mk_q_rules name query_string =
  let parsed = parse_query_string query_string in
  let inputs = [ Arg_rules follow_rules ] in
  {
    name
  ; run = (fun db -> consume_rows (Datascript.q ~inputs db parsed))
  ; result_edn = (fun db -> edn_of_q_rows (Datascript.q ~inputs db parsed))
  }

let queries =
  [
    mk_q "q1" "[:find ?e :where [?e :name \"Ivan\"]]"
  ; mk_q "q2" "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]"
  ; mk_q "q2-switch" "[:find ?e ?a :where [?e :age ?a] [?e :name \"Ivan\"]]"
  ; mk_q "q3" "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e :sex :male]]"
  ; mk_q
      "q4"
      "[:find ?e ?l ?a :where [?e :name \"Ivan\"] [?e :last-name ?l] [?e :age ?a] [?e :sex :male]]"
  ; mk_q
      "q5"
      "[:find ?e1 ?l ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e1 :age ?a] [?e1 :last-name ?l]]"
  ; mk_q "qpred1" "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]"
  ; mk_q_inputs "qpred2" "[:find ?e ?s :in $ ?min_s :where [?e :salary ?s] [(> ?s ?min_s)]]"
      [ Arg_scalar (Result_value (Int 50_000)) ]
  ; mk_q "q-or" "[:find ?e :where (or [?e :name \"Ivan\"] [?e :name \"Petr\"])]"
  ; mk_q "q-not" "[:find ?e ?a :where [?e :age ?a] (not [?e :sex :male])]"
  ; mk_q
      "q-or-join"
      "[:find ?e ?a :where [?e :age ?a] (or-join [?e] [?e :name \"Ivan\"] [?e :name \"Petr\"])]"
  ; mk_q "q-not-join" "[:find ?e ?a :where [?e :age ?a] (not-join [?e] [?e :sex :male])]"
  ; mk_q
      "q-pred-range"
      "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)] [(< ?s 80000)]]"
  ; mk_q
      "q-5-merge"
      "[:find ?e ?n ?l ?a ?s :where [?e :name ?n] [?e :last-name ?l] [?e :age ?a] [?e :salary ?s] [?e :sex :male]]"
  ; mk_q_rules "q-rule" "[:find ?e1 ?e2 :in $ % :where (follow ?e1 ?e2)]"
  ]

let query_names =
  List.map (fun query -> query.name) queries

let select_queries = function
  | None -> queries
  | Some name ->
    (match List.find_opt (fun query -> query.name = name) queries with
     | Some query -> [ query ]
     | None ->
       invalid_arg
         (Printf.sprintf "unknown query %S (available: %s)" name (String.concat ", " query_names)))

let remove_path path =
  if Sys.file_exists path then Sys.remove path;
  List.iter
    (fun suffix ->
      let sibling = path ^ suffix in
      if Sys.file_exists sibling then Sys.remove sibling)
    [ "-wal"; "-shm"; "-lock" ]

let file_size path =
  if Sys.file_exists path then (Unix.stat path).st_size else 0

let disk_footprint path =
  List.fold_left
    (fun total suffix -> total + file_size (if suffix = "" then path else path ^ suffix))
    0
    [ ""; "-wal"; "-shm"; "-lock" ]

let people_and_follows size =
  let rng = rng 1 in
  let entities = List.init size (fun index -> random_man rng (index + 1)) in
  let follow_ops =
    List.concat_map
      (fun entity_id ->
        if next_int rng 2 = 0 then
          let target = 1 + next_int rng size in
          [ Add (Entity_id entity_id, "follows", Ref target) ]
        else
          [])
      (List.init size (fun index -> index + 1))
  in
  entities, follow_ops

let build_db_with_storage ~storage ~persist size =
  let entities, follow_ops = people_and_follows size in
  let started = now_ms () in
  let db = db_with entities (empty_db ~schema ~storage ()) in
  let db = if follow_ops = [] then db else db_with follow_ops db in
  let db = refresh_db_indexes db in
  let build_ms = now_ms () -. started in
  if not persist then db, build_ms, 0.
  else
    let store_started = now_ms () in
    store db;
    collect_garbage storage;
    let restored =
      match restore storage with
      | Some db -> db
      | None -> failwith "storage-backed benchmark db should restore"
    in
    let restore_ms = now_ms () -. store_started in
    restored, build_ms, restore_ms

type prepared_db =
  { label : string
  ; db : db
  ; build_ms : float
  ; restore_ms : float
  ; path : string option
  ; cleanup : unit -> unit
  }

let prepare_backend ~data_dir backend size =
  match backend with
  | Memory_lmdb_nosync ->
    let storage = benchmark_memory_storage () in
    let db, build_ms, restore_ms =
      build_db_with_storage ~storage ~persist:false size
    in
    { label = storage_label backend; db; build_ms; restore_ms; path = None; cleanup = Fun.id }
  | Lmdb_file ->
    let path =
      Filename.concat data_dir
        (Printf.sprintf "datascript-query-bench-lmdb-%d.mdb" size)
    in
    remove_path path;
    let session = Datascript_lmdb.open_session path in
    let (storage : storage) = storage_of_handle (Datascript_lmdb.storage session) in
    let db, build_ms, restore_ms =
      build_db_with_storage ~storage ~persist:true size
    in
    { label = storage_label backend
    ; db
    ; build_ms
    ; restore_ms
    ; path = Some path
    ; cleanup =
        (fun () ->
          Datascript_lmdb.close session;
          remove_path path)
    }
  | Sqlite_file ->
    let path =
      Filename.concat data_dir
        (Printf.sprintf "datascript-query-bench-sqlite-%d.sqlite3" size)
    in
    remove_path path;
    let session = Datascript_sqlite.open_session path in
    let (storage : storage) = storage_of_handle (Datascript_sqlite.storage session) in
    let db, build_ms, restore_ms =
      build_db_with_storage ~storage ~persist:true size
    in
    { label = storage_label backend
    ; db
    ; build_ms
    ; restore_ms
    ; path = Some path
    ; cleanup =
        (fun () ->
          Datascript_sqlite.close session;
          remove_path path)
    }

let warmup_queries jit_warmup selected db =
  if jit_warmup <= 0 then ()
  else
    List.iter
      (fun query ->
        for _ = 1 to jit_warmup do
          query.run db
        done)
      selected

let run_backend config selected prepared =
  Printf.printf "storage\t%s\n%!" prepared.label;
  (match prepared.path with
   | Some path ->
       Printf.printf "path\t%s\n%!" path;
       Printf.printf "disk-bytes\t%d\n%!" (disk_footprint path)
   | None -> Printf.printf "path\tmemory\n%!");
  Printf.printf "build-ms\t%s\n%!" (format_ms prepared.build_ms);
  if prepared.restore_ms > 0. then
    Printf.printf "store-restore-ms\t%s\n%!" (format_ms prepared.restore_ms);
  Printf.eprintf
    "[%s] JIT pre-warmup (%d/query)...\n%!"
    prepared.label
    config.jit_warmup;
  warmup_queries config.jit_warmup selected prepared.db;
  List.iter
    (fun query ->
      Printf.printf "result-edn\t%s\t%s\n%!" query.name (query.result_edn prepared.db))
    selected;
  Printf.eprintf "[%s] Running %d query benchmarks...\n%!" prepared.label (List.length selected);
  List.iter
    (fun query ->
      let ms = bench config (fun () -> query.run prepared.db) in
      Printf.printf "%s\t%s\n%!" query.name (format_ms ms))
    selected

let ensure_dir path =
  let rec loop dir =
    if dir = "" || dir = Filename.current_dir_name || Sys.file_exists dir then ()
    else (
      loop (Filename.dirname dir);
      try Unix.mkdir dir 0o755 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  loop path

let main () =
  let config = parse_args () in
  let selected = select_queries config.query in
  ensure_dir config.data_dir;
  let runtime_label =
    match Sys.getenv_opt "BENCH_RUNTIME_LABEL" with
    | Some label -> label
    | None -> "ocaml"
  in
  Printf.printf "runtime\t%s\n%!" runtime_label;
  Printf.printf "size\t%d\n%!" config.size;
  Printf.printf "warmup-ms\t%.0f\n%!" config.warmup_ms;
  Printf.printf "sample-ms\t%.0f\n%!" config.sample_ms;
  Printf.printf "repeats\t%d\n%!" config.repeats;
  Printf.printf "jit-warmup\t%d\n%!" config.jit_warmup;
  Printf.printf "data-dir\t%s\n%!" config.data_dir;
  Printf.printf "db-mode\tshared\n%!";
  Printf.printf "query-cases\t%d\n%!" (List.length selected);
  (match config.query with
  | Some name -> Printf.printf "query\t%s\n%!" name
  | None -> ());
  List.iter
    (fun backend ->
      Printf.eprintf
        "Building database (%d entities, storage=%s, data-dir=%s)...\n%!"
        config.size
        (storage_label backend)
        config.data_dir;
      let prepared = prepare_backend ~data_dir:config.data_dir backend config.size in
      Fun.protect ~finally:prepared.cleanup (fun () -> run_backend config selected prepared))
    config.storages;
  Printf.eprintf "blackhole=%d\n%!" !blackhole

let () =
  if Array.mem "--list-queries" Sys.argv then (
    List.iter (fun query -> Printf.printf "%s\n%!" query.name) queries;
    exit 0);
  main ()