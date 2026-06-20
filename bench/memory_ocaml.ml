open Datascript

type config =
  { size : int
  ; tx_size : int
  }

let default_config = { size = 5_000; tx_size = 500 }

let parse_args () =
  let config = ref default_config in
  let rec loop = function
    | [] -> !config
    | "--size" :: value :: rest ->
      config := { !config with size = int_of_string value };
      loop rest
    | "--tx-size" :: value :: rest ->
      config := { !config with tx_size = int_of_string value };
      loop rest
    | arg :: _ -> invalid_arg ("unknown memory benchmark argument: " ^ arg)
  in
  Sys.argv |> Array.to_list |> List.tl |> loop

let blackhole = ref 0

let consume_int value =
  blackhole := (!blackhole + value) land 0x3fffffff

let seq_length seq =
  Seq.fold_left (fun count _ -> count + 1) 0 seq

let rss_bytes () =
  let command = Printf.sprintf "ps -o rss= -p %d" (Unix.getpid ()) in
  let channel = Unix.open_process_in command in
  let line =
    try input_line channel with
    | End_of_file -> "0"
  in
  ignore (Unix.close_process_in channel);
  line |> String.trim |> int_of_string |> fun kb -> kb * 1024

let heap_bytes () =
  let stat = Gc.stat () in
  stat.live_words * (Sys.word_size / 8)

let report runtime scenario =
  Printf.printf "%s\t%s\t%d\t%d\n%!" runtime scenario (rss_bytes ()) (heap_bytes ())

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
  { indexed with unique = Some Identity }

let ref_attr =
  { indexed with indexed = false; value_type = Some RefType }

let ref_many =
  { ref_attr with cardinality = Many }

let many =
  { indexed with cardinality = Many; indexed = false }

let schema =
  [ "id", unique_identity
  ; "name", indexed
  ; "age", indexed
  ; "salary", indexed
  ; "status", indexed
  ; "score", indexed
  ; "friend", ref_attr
  ; "mentor", ref_attr
  ; "team", ref_many
  ; "alias", many
  ]

let names = [| "Ivan"; "Petr"; "Sergey"; "Oleg"; "Yuri"; "Dmitry"; "Fedor"; "Denis" |]
let statuses = [| "todo"; "doing"; "done"; "blocked" |]

let person size i =
  let friend = if i = size then 1 else i + 1 in
  let mentor = if i <= 10 then 1 else i - 10 in
  Entity
    { db_id = Some (Entity_id i)
    ; attrs =
        [ "id", One_value (Int i)
        ; "name", One_value (String names.((i - 1) mod Array.length names))
        ; "age", One_value (Int ((i * 37) mod 100))
        ; "salary", One_value (Int ((i * 7919) mod 100_000))
        ; "status", One_value (String statuses.(i mod Array.length statuses))
        ; "score", One_value (Int ((i * 13) mod 10_000))
        ; "friend", One_value (Ref friend)
        ; "mentor", One_value (Ref mentor)
        ; ( "team"
          , Many_values
              [ Ref (((i + 7) mod size) + 1)
              ; Ref (((i + 19) mod size) + 1)
              ] )
        ; "alias", Many_values [ String ("alias-" ^ string_of_int (i mod 64)); String ("tag-" ^ string_of_int (i mod 251)) ]
        ]
    }

let people size =
  List.init size (fun index -> person size (index + 1))

let build_db size =
  db_with (people size) (empty_db ~schema ())

let update_entity size i =
  let entity_id = ((i * 17) mod size) + 1 in
  Entity
    { db_id = Some (Entity_id entity_id)
    ; attrs =
        [ "status", One_value (String statuses.((i + 1) mod Array.length statuses))
        ; "score", One_value (Int ((i * 97) mod 10_000))
        ; "alias", Many_values [ String ("updated-" ^ string_of_int (i mod 128)) ]
        ]
    }

let run_queries db =
  consume_int (seq_length (datoms db Aevt ~a:"name" ()));
  consume_int (List.length (q_string db "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]"));
  consume_int (List.length (q_string db "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]"));
  consume_int (List.length (q_string db "[:find ?e ?score :where [?e :status \"doing\"] [?e :score ?score] [(> ?score 500)]]"));
  for entity_id = 1 to 100 do
    match pull db [ Pull_attr "name"; Pull_attr "status"; Pull_ref ("friend", [ Pull_attr "name"; Pull_attr "age" ]) ] (Entity_id entity_id) with
    | None -> consume_int 0
    | Some entity -> consume_int (List.length entity.pulled_attrs)
  done

let run_scenario config db =
  run_queries db;
  let tx_data = List.init config.tx_size (fun index -> update_entity config.size index) in
  let db = db_with tx_data db in
  run_queries db;
  db

let main () =
  let config = parse_args () in
  let runtime =
    match Sys.getenv_opt "MEMORY_RUNTIME_LABEL" with
    | Some label -> label
    | None -> "ocaml-native"
  in
  let db = build_db config.size in
  report runtime "initial-open";
  let db = run_scenario config db in
  report runtime "after-transact-query";
  Gc.full_major ();
  Gc.compact ();
  report runtime "after-gc";
  consume_int db.max_eid;
  Printf.eprintf "blackhole=%d\n%!" !blackhole

let () = main ()
