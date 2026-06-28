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
  let rec loop count seq =
    match seq () with
    | Seq.Nil -> count
    | Seq.Cons (_, rest) -> loop (count + 1) rest
  in
  loop 0 seq

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
let aliases =
  [| "A. C. Q. W."
   ; "A. J. Finn"
   ; "A.A. Fair"
   ; "Aapeli"
   ; "Aaron Wolfe"
   ; "Abigail Van Buren"
   ; "Jeanne Phillips"
   ; "Abram Tertz"
   ; "Abu Nuwas"
   ; "Acton Bell"
   ; "Adunis"
  |]

type rng = { mutable state : int32 }

let rng seed = { state = Int32.of_int seed }

let next_int rng bound =
  rng.state <- Int32.add (Int32.mul rng.state 1_664_525l) 1_013_904_223l;
  Int32.(to_int (rem (logand (shift_right_logical rng.state 1) 0x3fffffffl) (of_int bound)))

let rand_nth rng values =
  values.(next_int rng (Array.length values))

let random_man rng i =
  let name = rand_nth rng names in
  let last_name = rand_nth rng last_names in
  let alias_count = next_int rng 10 in
  let alias_values = List.init alias_count (fun _ -> String (rand_nth rng aliases)) in
  Entity
    { db_id = Some (Temp_id (string_of_int i))
    ; attrs =
        [ "name", One_value (String name)
        ; "last-name", One_value (String last_name)
        ; "full-name", One_value (String (name ^ " " ^ last_name))
        ; "alias", Many_values alias_values
        ; "sex", One_value (Keyword (if next_int rng 2 = 0 then "male" else "female"))
        ; "age", One_value (Int (next_int rng 100))
        ; "salary", One_value (Int (next_int rng 100_000))
        ]
    }

let people size =
  let rng = rng 1 in
  List.init size (fun index -> random_man rng (index + 1))

let build_db size =
  db_with (people size) (empty_db ~schema ())

let build_storage_db size =
  let storage = memory_storage () in
  let db = db_with (people size) (empty_db ~schema ~storage ()) in
  store db;
  match restore storage with
  | Some db -> db
  | None -> failwith "storage-backed benchmark db should restore"

let add_one_by_one size =
  List.fold_left
    (fun db entity -> db_with [ entity ] db)
    (empty_db ~schema ())
    (people size)

let add_one_datom_per_tx size =
  let single_datom_attrs = [ "name"; "last-name"; "sex"; "age"; "salary" ] in
  let add_entity db entity =
    match entity with
    | Entity { db_id = Some entity_ref; attrs; _ } ->
      List.fold_left
        (fun db (attr, value) ->
          if List.mem attr single_datom_attrs then
            match value with
            | One_value value -> db_with [ Add (entity_ref, attr, value) ] db
            | Many_values _ | One_entity _ | Many_entities _ -> db
          else
            db)
        db
        attrs
    | _ -> db_with [ entity ] db
  in
  List.fold_left add_entity (empty_db ~schema ()) (people size)

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
  bench config "add-1" (fun () -> consume_db (add_one_datom_per_tx config.size));
  bench config "add-5" (fun () -> consume_db (add_one_by_one config.size));
  bench config "add-all" (fun () -> consume_db (build_db config.size));
  bench config "storage-roundtrip" (fun () ->
    consume_db (build_storage_db config.size));
  bench config "datoms-name" (fun () ->
    consume_int (fold_datoms (fun count _ -> count + 1) 0 (Lazy.force db) Aevt ~a:"name" ()));
  bench config "q1" (fun () ->
    consume_rows (q_string (Lazy.force db) "[:find ?e :where [?e :name \"Ivan\"]]"));
  bench config "q2" (fun () ->
    consume_rows (q_string (Lazy.force db) "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]"));
  bench config "q3" (fun () ->
    consume_rows (q_string (Lazy.force db) "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e :sex :male]]"));
  bench config "q4" (fun () ->
    consume_rows (q_string (Lazy.force db) "[:find ?e ?l ?a :where [?e :name \"Ivan\"] [?e :last-name ?l] [?e :age ?a] [?e :sex :male]]"));
  bench config "q5-shortcircuit" (fun () ->
    consume_rows
      (q_string
         ~inputs:[ Arg_scalar (Result_value (String "Anastasia")); Arg_scalar (Result_value (Int 35)) ]
         (Lazy.force db)
         "[:find ?e ?n ?l ?a ?s ?al :in $ ?n ?a :where [?e :name ?n] [?e :age ?a] [?e :last-name ?l] [?e :sex ?s] [?e :alias ?al]]"));
  bench config "qpred1" (fun () ->
    consume_rows (q_string (Lazy.force db) "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]"));
  bench config "qpred2" (fun () ->
    consume_rows
      (q_string
         ~inputs:[ Arg_scalar (Result_value (Int 50000)) ]
         (Lazy.force db)
         "[:find ?e ?s :in $ ?min-s :where [?e :salary ?s] [(> ?s ?min-s)]]"));
  bench config "q2pred" (fun () ->
    consume_rows
      (q_string
         (Lazy.force db)
         "[:find ?e ?s :where [?e :name \"Ivan\"] [?e :salary ?s] [(> ?s 50000)]]"));
  bench config "pull-one" (fun () ->
    consume_pull (pull (Lazy.force db) [ Pull_attr "name"; Pull_attr "age"; Pull_ref ("friend", [ Pull_attr "name"; Pull_attr "age" ]) ] (Entity_id 1)));
  Printf.eprintf "blackhole=%d\n%!" !blackhole

let () = main ()
