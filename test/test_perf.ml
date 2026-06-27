open Datascript

let failf fmt = Printf.ksprintf failwith fmt

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

let unique_identity = { indexed with unique = Some Identity }
let many = { indexed with cardinality = Many; indexed = false }

let schema =
  [ "id", unique_identity
  ; "name", indexed
  ; "age", indexed
  ; "salary", indexed
  ; "alias", many
  ]

let outliner_schema =
  [ "block/id", unique_identity
  ; "block/journal-day", indexed
  ; "block/content", indexed
  ; "block/order", indexed
  ; "block/collapsed", indexed
  ; "block/parent", { indexed with indexed = false; value_type = Some RefType }
  ]

let names = [| "Ivan"; "Petr"; "Sergey"; "Oleg"; "Yuri"; "Dmitry"; "Fedor"; "Denis" |]

let person i =
  Entity
    { db_id = Some (Entity_id i)
    ; attrs =
        [ "id", One_value (Int i)
        ; "name", One_value (String names.((i - 1) mod Array.length names))
        ; "age", One_value (Int ((i * 37) mod 100))
        ; "salary", One_value (Int ((i * 7919) mod 100_000))
        ; "sex", One_value (Keyword (if i mod 2 = 0 then "male" else "female"))
        ; "alias", Many_values [ String ("alias-" ^ string_of_int (i mod 10)); String ("tag-" ^ string_of_int (i mod 17)) ]
        ]
    }

let people size =
  List.init size (fun index -> person (index + 1))

let outliner_block_datoms index =
  let e = index + 1 in
  [ datom ~e ~a:"block/id" ~v:(String (Printf.sprintf "block-%05d" e)) ()
  ; datom ~e ~a:"block/journal-day" ~v:(String "2026-06-27") ()
  ; datom ~e ~a:"block/content" ~v:(String (Printf.sprintf "Block %05d" e)) ()
  ; datom ~e ~a:"block/order" ~v:(Float (float_of_int e)) ()
  ; datom ~e ~a:"block/collapsed" ~v:(Bool false) ()
  ]

let build_outliner_db size =
  init_db ~schema:outliner_schema (List.concat (List.init size outliner_block_datoms))

let add_outliner_block ?parent_id id order =
  let entity = Temp_id id in
  let parent_ops =
    match parent_id with
    | None -> []
    | Some parent_id -> [ Add (entity, "block/parent", Ref_to (Lookup_ref ("block/id", String parent_id))) ]
  in
  [ Add (entity, "block/id", String id)
  ; Add (entity, "block/journal-day", String "2026-06-27")
  ; Add (entity, "block/content", String id)
  ; Add (entity, "block/order", Float order)
  ; Add (entity, "block/collapsed", Bool false)
  ]
  @ parent_ops

let seq_length seq =
  Seq.fold_left (fun count _ -> count + 1) 0 seq

let blackhole = ref 0

let consume_int value =
  blackhole := (!blackhole + value) land 0x3fffffff

let time f =
  Gc.compact ();
  let start = Unix.gettimeofday () in
  let result = f () in
  result, Unix.gettimeofday () -. start

let time_repeated iterations f =
  let _, elapsed =
    time (fun () ->
      for _ = 1 to iterations do
        consume_int (f ())
      done)
  in
  elapsed

let build_db size =
  db_with (people size) (empty_db ~schema ())

let add_one_by_one size =
  List.fold_left
    (fun db entity -> db_with [ entity ] db)
    (empty_db ~schema ())
    (people size)

let consume_db db =
  consume_int (seq_length (datoms db Eavt ()))

let consume_block_id db id =
  consume_int (seq_length (datoms db Avet ~a:"block/id" ~v:(String id) ()))

let first_name_entity db =
  match Seq.uncons (datoms db Aevt ~a:"name" ()) with
  | Some (datom, _) -> datom.e
  | None -> 0

let count_name_datoms db =
  seq_length (datoms db Aevt ~a:"name" ())

let query_name_age =
  lazy (parse_query_string "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]")

let first_seek_name_entity db =
  match Seq.uncons (seek_datoms db Avet ~a:"name" ~v:(String "Ivan") ()) with
  | Some (datom, _) -> datom.e
  | None -> 0

let test_incremental_explicit_entity_adds_stay_near_bulk_cost () =
  let size = 3000 in
  let _, bulk_elapsed = time (fun () -> consume_db (build_db size)) in
  let _, incremental_elapsed = time (fun () -> consume_db (add_one_by_one size)) in
  if incremental_elapsed > (bulk_elapsed *. 5.0) then
    failf
      "sequential explicit-id entity adds should not repeatedly rescan the full DB: bulk=%.4fs sequential=%.4fs"
      bulk_elapsed
      incremental_elapsed

let test_aevt_prefix_lookup_is_lazy_to_first_match () =
  let db = build_db 10_000 in
  let iterations = 200 in
  let first_elapsed = time_repeated iterations (fun () -> first_name_entity db) in
  let count_elapsed = time_repeated iterations (fun () -> count_name_datoms db) in
  if first_elapsed > (count_elapsed *. 0.25) then
    failf
      "taking the first AEVT prefix datom should not materialize the whole prefix: first=%.4fs count=%.4fs"
      first_elapsed
      count_elapsed

let test_seek_datoms_is_lazy_to_first_match () =
  let db = build_db 10_000 in
  let iterations = 200 in
  let first_elapsed = time_repeated iterations (fun () -> first_seek_name_entity db) in
  let count_elapsed = time_repeated iterations (fun () -> seq_length (seek_datoms db Avet ~a:"name" ~v:(String "Ivan") ())) in
  if first_elapsed > (count_elapsed *. 0.25) then
    failf
      "taking the first seek_datoms result should not materialize the whole seek: first=%.4fs count=%.4fs"
      first_elapsed
      count_elapsed

let test_aevt_prefix_count_has_low_per_item_overhead () =
  let db = build_db 10_000 in
  let iterations = 200 in
  let elapsed = time_repeated iterations (fun () -> count_name_datoms db) in
  if elapsed > 0.180 then
    failf
      "counting an AEVT attr prefix should avoid comparator allocation overhead: elapsed=%.4fs"
      elapsed

let test_name_age_join_uses_indexed_plan () =
  let db = build_db 1_000 in
  let iterations = 200 in
  let elapsed = time_repeated iterations (fun () -> q db (Lazy.force query_name_age) |> List.length) in
  if elapsed > 0.120 then
    failf
      "name/age join should use bounded indexed lookups without full DB materialization: elapsed=%.4fs"
      elapsed

let test_tempid_unique_entity_add_uses_indexes () =
  let db = build_outliner_db 5000 in
  let id = "block-new" in
  let _, elapsed =
    time (fun () ->
      db_with (add_outliner_block ~parent_id:"block-00001" id 5001.0) db
      |> fun db -> consume_block_id db id)
  in
  if elapsed > 0.010 then
    failf
      "tempid entity map upsert should stay below 10ms and use indexes instead of scanning EAVT: elapsed=%.4fs"
      elapsed

let test_lookup_ref_cardinality_one_update_uses_indexes () =
  let db = build_outliner_db 5000 in
  let _, elapsed =
    time (fun () ->
      db_with [ Add (Lookup_ref ("block/id", String "block-00001"), "block/content", String "Edited") ] db
      |> fun db -> consume_block_id db "block-00001")
  in
  if elapsed > 0.010 then
    failf
      "lookup-ref cardinality-one update should stay below 10ms and use indexes instead of scanning EAVT: elapsed=%.4fs"
      elapsed

let () =
  let failures =
    [ ( "incremental explicit-id entity adds"
      , fun () -> test_incremental_explicit_entity_adds_stay_near_bulk_cost () )
    ; "AEVT prefix first match laziness", (fun () -> test_aevt_prefix_lookup_is_lazy_to_first_match ())
    ; "seek_datoms first match laziness", (fun () -> test_seek_datoms_is_lazy_to_first_match ())
    ; "AEVT prefix count overhead", (fun () -> test_aevt_prefix_count_has_low_per_item_overhead ())
    ; "name/age indexed join", (fun () -> test_name_age_join_uses_indexed_plan ())
    ; "tempid unique entity add uses indexes", (fun () -> test_tempid_unique_entity_add_uses_indexes ())
    ; ( "lookup-ref cardinality-one update uses indexes"
      , fun () -> test_lookup_ref_cardinality_one_update_uses_indexes () )
    ]
    |> List.filter_map (fun (name, test) ->
      try
        test ();
        None
      with Failure message -> Some (name ^ ": " ^ message))
  in
  (match failures with
   | [] -> ()
   | _ -> failwith (String.concat "\n" failures));
  if !blackhole = -1 then failwith "unreachable"
