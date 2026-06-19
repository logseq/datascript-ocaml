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

let names = [| "Ivan"; "Petr"; "Sergey"; "Oleg"; "Yuri"; "Dmitry"; "Fedor"; "Denis" |]

let person i =
  Entity
    { db_id = Some (Entity_id i)
    ; attrs =
        [ "id", One_value (Int i)
        ; "name", One_value (String names.((i - 1) mod Array.length names))
        ; "age", One_value (Int ((i * 37) mod 100))
        ; "salary", One_value (Int ((i * 7919) mod 100_000))
        ; "alias", Many_values [ String ("alias-" ^ string_of_int (i mod 10)); String ("tag-" ^ string_of_int (i mod 17)) ]
        ]
    }

let people size =
  List.init size (fun index -> person (index + 1))

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

let first_name_entity db =
  match Seq.uncons (datoms db Aevt ~a:"name" ()) with
  | Some (datom, _) -> datom.e
  | None -> 0

let count_name_datoms db =
  seq_length (datoms db Aevt ~a:"name" ())

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

let () =
  let failures =
    [ ( "incremental explicit-id entity adds"
      , fun () -> test_incremental_explicit_entity_adds_stay_near_bulk_cost () )
    ; "AEVT prefix first match laziness", (fun () -> test_aevt_prefix_lookup_is_lazy_to_first_match ())
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
