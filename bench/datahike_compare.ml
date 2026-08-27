open Datascript

(* Align with Datahike benchmark.datascript-bench: 20k people, query suite, timing protocol. *)

type config =
  { size : int
  ; warmup_ms : float
  ; sample_ms : float
  ; repeats : int
  ; step : int
  ; jit_warmup : int
  ; query : string option
  }

let default_config =
  { size = 20_000
  ; warmup_ms = 200.
  ; sample_ms = 200.
  ; repeats = 2
  ; step = 10
  ; jit_warmup = 100
  ; query = None
  }

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

let consume_rows rows = blackhole := (!blackhole + List.length rows) land 0x3fffffff

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

let random_man rng i =
  Entity
    {
      db_id = Some (Temp_id (string_of_int i)
      )
    ; attrs =
        [ "name", One_value (String (rand_nth rng names))
        ; "last-name", One_value (String (rand_nth rng last_names))
        ; "sex", One_value (Keyword (rand_nth rng sexes))
        ; "age", One_value (Int (next_int rng 100))
        ; "salary", One_value (Int (next_int rng 100_000))
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

type query_case =
  { name : string
  ; run : db -> unit
  }

let q name query =
  { name; run = (fun db -> consume_rows (q_string db query)) }

let q_inputs name query inputs =
  {
    name
  ; run =
      (fun db -> consume_rows (q_string ~inputs db query))
  }

let q_rules name query =
  { name; run = (fun db -> consume_rows (q_string ~inputs:[ Arg_rules follow_rules ] db query)) }

let queries =
  [
    q "q1" "[:find ?e :where [?e :name \"Ivan\"]]"
  ; q "q2" "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]"
  ; q "q2-switch" "[:find ?e ?a :where [?e :age ?a] [?e :name \"Ivan\"]]"
  ; q "q3" "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e :sex :male]]"
  ; q
      "q4"
      "[:find ?e ?l ?a :where [?e :name \"Ivan\"] [?e :last-name ?l] [?e :age ?a] [?e :sex :male]]"
  ; q
      "q5"
      "[:find ?e1 ?l ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e1 :age ?a] [?e1 :last-name ?l]]"
  ; q "qpred1" "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]"
  ; q_inputs "qpred2" "[:find ?e ?s :in $ ?min_s :where [?e :salary ?s] [(> ?s ?min_s)]]"
      [ Arg_scalar (Result_value (Int 50_000)) ]
  ; q "q-or" "[:find ?e :where (or [?e :name \"Ivan\"] [?e :name \"Petr\"])]"
  ; q "q-not" "[:find ?e ?a :where [?e :age ?a] (not [?e :sex :male])]"
  ; q
      "q-or-join"
      "[:find ?e ?a :where [?e :age ?a] (or-join [?e] [?e :name \"Ivan\"] [?e :name \"Petr\"])]"
  ; q "q-not-join" "[:find ?e ?a :where [?e :age ?a] (not-join [?e] [?e :sex :male])]"
  ; q
      "q-pred-range"
      "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)] [(< ?s 80000)]]"
  ; q
      "q-5-merge"
      "[:find ?e ?n ?l ?a ?s :where [?e :name ?n] [?e :last-name ?l] [?e :age ?a] [?e :salary ?s] [?e :sex :male]]"
  ; q_rules "q-rule" "[:find ?e1 ?e2 :in $ % :where (follow ?e1 ?e2)]"
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

let build_db size =
  let storage = benchmark_memory_storage () in
  let rng = rng 1 in
  let entities = List.init size (fun index -> random_man rng (index + 1)) in
  let db = db_with entities (empty_db ~schema ~storage ()) in
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
  let db = if follow_ops = [] then db else db_with follow_ops db in
  refresh_db_indexes db

let warmup_queries jit_warmup selected db =
  if jit_warmup <= 0 then ()
  else
    List.iter
      (fun query ->
        for _ = 1 to jit_warmup do
          query.run db
        done)
      selected

let main () =
  let config = parse_args () in
  let selected = select_queries config.query in
  let runtime_label =
    match Sys.getenv_opt "BENCH_RUNTIME_LABEL" with
    | Some label -> label
    | None -> "ocaml"
  in
  Printf.printf "runtime\t%s\n%!" runtime_label;
  Printf.printf "size\t%d\n%!" config.size;
  Printf.printf "storage\tmemory-lmdb-nosync-index\n%!";
  Printf.printf "warmup-ms\t%.0f\n%!" config.warmup_ms;
  Printf.printf "sample-ms\t%.0f\n%!" config.sample_ms;
  Printf.printf "repeats\t%d\n%!" config.repeats;
  Printf.printf "jit-warmup\t%d\n%!" config.jit_warmup;
  Printf.printf "db-mode\tshared\n%!";
  (match config.query with
  | Some name -> Printf.printf "query\t%s\n%!" name
  | None -> ());
  Printf.eprintf "Building shared database (%d entities)...\n%!" config.size;
  let db = build_db config.size in
  Printf.eprintf "JIT pre-warmup (%d/query)...\n%!" config.jit_warmup;
  warmup_queries config.jit_warmup selected db;
  Printf.eprintf "Running benchmarks...\n%!";
  List.iter
    (fun query ->
      let ms = bench config (fun () -> query.run db) in
      Printf.printf "%s\t%s\n%!" query.name (format_ms ms))
    selected;
  Printf.eprintf "blackhole=%d\n%!" !blackhole

let () =
  if Array.mem "--list-queries" Sys.argv then (
    List.iter (fun query -> Printf.printf "%s\n%!" query.name) queries;
    exit 0);
  main ()
