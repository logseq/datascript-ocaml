open Datascript

type config =
  { size : int
  ; warmup_ms : float
  ; sample_ms : float
  ; samples : int
  }

let default_config = { size = 200; warmup_ms = 200.; sample_ms = 500.; samples = 5 }

let parse_args () =
  let config = ref default_config in
  let set_size value = config := { !config with size = int_of_string value } in
  let set_warmup value = config := { !config with warmup_ms = float_of_string value } in
  let set_sample_ms value = config := { !config with sample_ms = float_of_string value } in
  let set_samples value = config := { !config with samples = int_of_string value } in
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
    | "--samples" :: value :: rest ->
      set_samples value;
      loop rest
    | arg :: _ -> invalid_arg ("unknown benchmark argument: " ^ arg)
  in
  Sys.argv |> Array.to_list |> List.tl |> loop

let now_ms () =
  Unix.gettimeofday () *. 1000.

let median values =
  let sorted = List.sort Float.compare values in
  List.nth sorted (List.length sorted / 2)

let format_ms value =
  if value > 1. then Printf.sprintf "%.2f" value else Printf.sprintf "%.5f" value

let blackhole = ref 0

let consume_int value =
  blackhole := (!blackhole + value) land 0x3fffffff

let seq_length seq =
  Seq.fold_left (fun count _ -> count + 1) 0 seq

let consume_db db =
  consume_int (seq_length (datoms db Eavt ()))

let consume_rows rows =
  consume_int (List.length rows)

let consume_pull = function
  | Some entity -> consume_int (List.length entity.pulled_attrs)
  | None -> consume_int 0

let run_for duration_ms f =
  let start = now_ms () in
  let deadline = start +. duration_ms in
  let rec loop iterations =
    f ();
    let iterations = iterations + 1 in
    if now_ms () < deadline then loop iterations else iterations, now_ms () -. start
  in
  loop 0

let bench config name f =
  ignore (run_for config.warmup_ms f);
  let samples =
    List.init config.samples (fun _ ->
      let iterations, elapsed = run_for config.sample_ms f in
      elapsed /. float_of_int iterations)
  in
  Printf.printf "%s\t%s\n%!" name (format_ms (median samples))

let one =
  { cardinality = One
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

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

let unique_identity =
  { cardinality = One
  ; unique = Some Identity
  ; indexed = true
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let ref_attr =
  { cardinality = One
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = Some RefType
  ; tuple_attrs = None
  ; tuple_types = None
  }

let many =
  { cardinality = Many
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let schema =
  [ "id", unique_identity
  ; "name", indexed
  ; "age", indexed
  ; "salary", indexed
  ; "friend", ref_attr
  ; "alias", many
  ]

let names = [| "Ivan"; "Petr"; "Sergey"; "Oleg"; "Yuri"; "Dmitry"; "Fedor"; "Denis" |]
let last_names = [| "Ivanov"; "Petrov"; "Sidorov"; "Kovalev"; "Kuznetsov"; "Voronoi" |]

let person size i =
  let friend = if i = size then 1 else i + 1 in
  Entity
    { db_id = Some (Entity_id i)
    ; attrs =
        [ "id", One_value (Int i)
        ; "name", One_value (String names.((i - 1) mod Array.length names))
        ; "last-name", One_value (String last_names.((i - 1) mod Array.length last_names))
        ; "age", One_value (Int ((i * 37) mod 100))
        ; "salary", One_value (Int ((i * 7919) mod 100_000))
        ; "sex", One_value (String (if i mod 2 = 0 then "male" else "female"))
        ; "friend", One_value (Ref friend)
        ; "alias", Many_values [ String ("alias-" ^ string_of_int (i mod 10)); String ("tag-" ^ string_of_int (i mod 17)) ]
        ]
    }

let people size =
  List.init size (fun index -> person size (index + 1))

let build_db size =
  db_with (people size) (empty_db ~schema ())

let add_one_by_one size =
  List.fold_left
    (fun db entity -> db_with [ entity ] db)
    (empty_db ~schema ())
    (people size)

let main () =
  let config = parse_args () in
  let runtime_label =
    match Sys.getenv_opt "BENCH_RUNTIME_LABEL" with
    | Some label -> label
    | None -> "ocaml"
  in
  Printf.printf "runtime\t%s\n" runtime_label;
  Printf.printf "size\t%d\n" config.size;
  let db = lazy (build_db config.size) in
  bench config "add-all" (fun () -> consume_db (build_db config.size));
  bench config "add-one-by-one" (fun () -> consume_db (add_one_by_one config.size));
  bench config "datoms-name" (fun () -> consume_int (seq_length (datoms (Lazy.force db) Aevt ~a:"name" ())));
  bench config "query-name-age" (fun () ->
    consume_rows (q_string (Lazy.force db) "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]"));
  bench config "query-salary-pred" (fun () ->
    consume_rows (q_string (Lazy.force db) "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]"));
  bench config "pull-one" (fun () ->
    consume_pull (pull (Lazy.force db) [ Pull_attr "name"; Pull_attr "age"; Pull_ref ("friend", [ Pull_attr "name"; Pull_attr "age" ]) ] (Entity_id 1)));
  Printf.eprintf "blackhole=%d\n%!" !blackhole

let () = main ()
