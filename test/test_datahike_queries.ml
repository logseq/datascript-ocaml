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

let count_rows db query = List.length (q_string db query)

let count_rows_inputs db query inputs = List.length (q_string ~inputs db query)

(* Golden counts for size=2000, rng seed=1 — aligned with bench/datahike_compare.ml *)
let db = lazy (build_db 2000)

let check_count name expected query =
  check_int name expected (count_rows (Lazy.force db) query)

let check_count_inputs name expected query inputs =
  check_int name expected (count_rows_inputs (Lazy.force db) query inputs)

let () =
  Alcotest.run "datahike query parity"
    [
      ( "queries"
      , [
          test_case "q1 name lookup" `Quick
            (fun () -> check_count "q1" 250 "[:find ?e :where [?e :name \"Ivan\"]]")
        ; test_case "q2 name and age" `Quick
            (fun () ->
              check_count "q2" 250 "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]")
        ; test_case "q2-switch clause order" `Quick
            (fun () ->
              check_count "q2-switch" 250
                "[:find ?e ?a :where [?e :age ?a] [?e :name \"Ivan\"]]")
        ; test_case "q3 name age sex" `Quick
            (fun () ->
              check_count "q3" 0
                "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e :sex :male]]")
        ; test_case "q4 name last-name age sex" `Quick
            (fun () ->
              check_count "q4" 0
                "[:find ?e ?l ?a :where [?e :name \"Ivan\"] [?e :last-name ?l] [?e :age ?a] [?e :sex :male]]")
        ; test_case "last-name AEVT attr slice" `Quick
            (fun () ->
              let db = Lazy.force db in
              check_int "last-name datoms"
                2000
                (datoms db Aevt ~a:"last-name" () |> List.of_seq |> List.length))
        ; test_case "q5 cross-entity age join" `Quick
            (fun () ->
              check_count "q5" 1000
                "[:find ?e1 ?l ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e1 :age ?a] [?e1 :last-name ?l]]")
        ; test_case "qpred1 salary predicate" `Quick
            (fun () ->
              check_count "qpred1" 997 "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]")
        ; test_case "qpred2 salary predicate with input" `Quick
            (fun () ->
              check_count_inputs "qpred2" 997
                "[:find ?e ?s :in $ ?min_s :where [?e :salary ?s] [(> ?s ?min_s)]]"
                [ Arg_scalar (Result_value (Int 50_000)) ])
        ; test_case "q-or names" `Quick
            (fun () ->
              check_count "q-or" 500
                "[:find ?e :where (or [?e :name \"Ivan\"] [?e :name \"Petr\"])]")
        ; test_case "q-not not male" `Quick
            (fun () ->
              check_count "q-not" 1000 "[:find ?e ?a :where [?e :age ?a] (not [?e :sex :male])]")
        ; test_case "q-or-join names" `Quick
            (fun () ->
              check_count "q-or-join" 500
                "[:find ?e ?a :where [?e :age ?a] (or-join [?e] [?e :name \"Ivan\"] [?e :name \"Petr\"])]")
        ; test_case "q-not-join not male" `Quick
            (fun () ->
              check_count "q-not-join" 1000
                "[:find ?e ?a :where [?e :age ?a] (not-join [?e] [?e :sex :male])]")
        ; test_case "q-pred-range salary range" `Quick
            (fun () ->
              check_count "q-pred-range" 616
                "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)] [(< ?s 80000)]]")
        ; test_case "q-5-merge male attrs" `Quick
            (fun () ->
              check_count "q-5-merge" 1000
                "[:find ?e ?n ?l ?a ?s :where [?e :name ?n] [?e :last-name ?l] [?e :age ?a] [?e :salary ?s] [?e :sex :male]]")
        ; test_case "q-rule non-recursive" `Quick
            (fun () ->
              check_count_inputs "q-rule" 667
                "[:find ?e1 ?e2 :in $ % :where (follow ?e1 ?e2)]"
                [ Arg_rules follow_rules ])
        ] )
    ]
