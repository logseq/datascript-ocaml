open Alcotest
open Datascript
open Test_alcotest_support

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

let build_db size =
  let rng = rng 1 in
  let entities =
    List.init size (fun index ->
        let i = index + 1 in
        Entity
          {
            db_id = Some (Temp_id (string_of_int i))
          ; attrs =
              [ "name", One_value (String (rand_nth rng names))
              ; "last-name", One_value (String (rand_nth rng last_names))
              ; "sex", One_value (Keyword (rand_nth rng sexes))
              ; "age", One_value (Int (next_int rng 100))
              ; "salary", One_value (Int (next_int rng 100_000))
              ]
          })
  in
  let db = db_with entities (empty_db ~schema ()) in
  let follow_ops =
    List.concat_map
      (fun entity_id ->
        if next_int rng 2 = 0 then
          [ Add (Entity_id entity_id, "follows", Ref (1 + next_int rng size)) ]
        else
          [])
      (List.init size (fun index -> index + 1))
  in
  if follow_ops = [] then db else db_with follow_ops db

let follow_rules =
  Parser.parse_rules
    (QueryFormVector
       [ QueryFormVector
           [ QueryFormVector [ QueryFormSymbol "follow"; QueryFormSymbol "?e1"; QueryFormSymbol "?e2" ]
           ; QueryFormVector
               [ QueryFormSymbol "?e1"; QueryFormKeyword "follows"; QueryFormSymbol "?e2" ]
           ] ])

let count_rows db query =
  List.length (q_string db query)

let count_rows_inputs db query inputs =
  List.length (q_string ~inputs db query)

(* Golden counts for size=2000, rng seed=1 — aligned with bench/datahike_compare.ml *)
let db = lazy (build_db 2000)

let test_q1 () =
  check_int "q1 Ivan count" 250 (count_rows (Lazy.force db) "[:find ?e :where [?e :name \"Ivan\"]]")

let test_qpred1 () =
  let rows =
    count_rows (Lazy.force db) "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]"
  in
  check_int "qpred1 result count" 997 rows

let test_qpred2 () =
  let rows =
    count_rows_inputs
      (Lazy.force db)
      "[:find ?e ?s :in $ ?min_s :where [?e :salary ?s] [(> ?s ?min_s)]]"
      [ Arg_scalar (Result_value (Int 50_000)) ]
  in
  check_int "qpred2 result count" 997 rows

let test_q_pred_range () =
  let rows =
    count_rows
      (Lazy.force db)
      "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)] [(< ?s 80000)]]"
  in
  check_int "q-pred-range result count" 616 rows

let test_q_rule () =
  let rows =
    count_rows_inputs
      (Lazy.force db)
      "[:find ?e1 ?e2 :in $ % :where (follow ?e1 ?e2)]"
      [ Arg_rules follow_rules ]
  in
  if rows <= 0 then
    failwith "q-rule should return follow edges";
  ()

let () =
  Alcotest.run "datahike query parity"
    [
      ( "queries"
      , [
          test_case "q1 name lookup" `Quick test_q1
        ; test_case "qpred1 salary predicate" `Quick test_qpred1
        ; test_case "qpred2 salary predicate with input" `Quick test_qpred2
        ; test_case "q-pred-range salary range" `Quick test_q_pred_range
        ; test_case "q-rule non-recursive" `Quick test_q_rule
        ] )
    ]
