open Datascript

type config =
  { size : int
  ; tx_size : int
  }

let default_config = { size = 1_000; tx_size = 20 }

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

let query_name_age = lazy (parse_query_string "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]")

let query_salary_pred = lazy (parse_query_string "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]")

let query_status_score =
  lazy (parse_query_string "[:find ?e ?score :where [?e :status \"doing\"] [?e :score ?score] [(> ?score 500)]]")

let pull_friend =
  [ Pull_attr "name"; Pull_attr "status"; Pull_ref ("friend", [ Pull_attr "name"; Pull_attr "age" ]) ]

let run_queries ?(probe = fun _ -> ()) db =
  consume_int (seq_length (datoms db Aevt ~a:"name" ()));
  probe "datoms-name";
  consume_int (List.length (q db (Lazy.force query_name_age)));
  probe "query-name-age";
  consume_int (List.length (q db (Lazy.force query_salary_pred)));
  probe "query-salary-pred";
  consume_int (List.length (q db (Lazy.force query_status_score)));
  probe "query-status-score";
  for entity_id = 1 to 100 do
    match pull db pull_friend (Entity_id entity_id) with
    | None -> consume_int 0
    | Some entity -> consume_int (List.length entity.pulled_attrs)
  done;
  probe "pull-friends"

let run_scenario ?probe config db =
  run_queries ?probe db;
  let tx_data = List.init config.tx_size (fun index -> update_entity config.size index) in
  let db = db_with tx_data db in
  (match probe with
   | None -> ()
   | Some probe -> probe "transact");
  run_queries ?probe db;
  db

let report runtime scenario rss_bytes heap_bytes =
  Printf.printf "%s\t%s\t%d\t%d\n%!" runtime scenario rss_bytes heap_bytes

let ref_attrs =
  [ "friend"; "mentor"; "team" ]

let canonical_value attr = function
  | Int value when List.mem attr ref_attrs -> "ref:" ^ string_of_int value
  | Int value -> "int:" ^ string_of_int value
  | Ref entity_id -> "ref:" ^ string_of_int entity_id
  | String value -> "string:" ^ value
  | Bool value -> "bool:" ^ string_of_bool value
  | Keyword value -> "keyword:" ^ value
  | Symbol value -> "symbol:" ^ value
  | Float value -> "float:" ^ string_of_float value
  | Nil -> "nil"
  | Uuid value -> "uuid:" ^ value
  | Instant value -> "instant:" ^ string_of_int value
  | Regex value -> "regex:" ^ value
  | TxRef -> "tx-ref"
  | Ref_to _ -> "ref-to"
  | List _ | Vector _ | Set _ | Map _ | Tuple _ -> "compound"

let canonical_datom_line datom =
  Printf.sprintf
    "datom\t%d\t%s\t%s"
    datom.e
    datom.a
    (canonical_value datom.a datom.v)

let write_final_data path db =
  let lines =
    datoms db Eavt ()
    |> List.of_seq
    |> List.map canonical_datom_line
    |> List.sort String.compare
  in
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () ->
      List.iter (fun line -> output_string channel line; output_char channel '\n') lines)

let maybe_write_final_data db =
  match Sys.getenv_opt "MEMORY_VERIFY_FILE" with
  | None -> ()
  | Some path -> write_final_data path db

let finish db =
  maybe_write_final_data db;
  consume_int db.max_eid;
  Printf.eprintf "blackhole=%d\n%!" !blackhole
