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

(* next_int 8 then next_int 6 then next_int 2 is LCG-periodic: name index
   parity always determines sex. Draw sex from a larger modulus so q3/q4 are
   non-vacuous under seed=1. *)
let rand_sex rng = sexes.(next_int rng 997 mod Array.length sexes)

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
              ; "sex", One_value (Keyword (rand_sex rng))
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

let sort_rows rows =
  List.sort
    (fun left right ->
      compare
        (List.map
           (function
             | Result_value v -> v
             | Result_entity e -> Int e
             | Result_attr a -> Keyword a
             | Result_db _ -> Nil
             | Result_pull _ -> Nil)
           left)
        (List.map
           (function
             | Result_value v -> v
             | Result_entity e -> Int e
             | Result_attr a -> Keyword a
             | Result_db _ -> Nil
             | Result_pull _ -> Nil)
           right))
    rows

let cell_digest = function
  | Result_entity e -> "e:" ^ string_of_int e
  | Result_attr a -> "a:" ^ a
  | Result_value (Int i) -> "i:" ^ string_of_int i
  | Result_value (Float f) -> "f:" ^ string_of_float f
  | Result_value (String s) -> "s:" ^ s
  | Result_value (Keyword k) -> "k:" ^ k
  | Result_value (Bool b) -> "b:" ^ string_of_bool b
  | Result_value (Ref e) -> "r:" ^ string_of_int e
  | Result_value _ -> "v:?"
  | Result_db _ -> "db"
  | Result_pull _ -> "pull"

let rows_digest rows =
  sort_rows rows
  |> List.map (fun row -> String.concat "," (List.map cell_digest row))
  |> String.concat "|"
  |> Digest.string
  |> Digest.to_hex

(* Golden counts + digests for size=2000, rng seed=1 with decorrelated sex.
   Digests cover full sorted result identity (not just counts). *)
let db = lazy (build_db 2000)

let check_query name expected_count expected_digest query =
  let rows = q_string (Lazy.force db) query in
  check_int (name ^ "-count") expected_count (List.length rows);
  check string (name ^ "-digest") expected_digest (rows_digest rows)

let check_query_inputs name expected_count expected_digest query inputs =
  let rows = q_string ~inputs (Lazy.force db) query in
  check_int (name ^ "-count") expected_count (List.length rows);
  check string (name ^ "-digest") expected_digest (rows_digest rows)

let () =
  if Array.exists (( = ) "--dump-goldens") Sys.argv then (
    let db = Lazy.force db in
    let dump name query =
      let rows = q_string db query in
      Printf.printf "%s\t%d\t%s\n%!" name (List.length rows) (rows_digest rows)
    in
    let dump_in name query inputs =
      let rows = q_string ~inputs db query in
      Printf.printf "%s\t%d\t%s\n%!" name (List.length rows) (rows_digest rows)
    in
    dump "q1" "[:find ?e :where [?e :name \"Ivan\"]]";
    dump "q2" "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]";
    dump "q2-switch" "[:find ?e ?a :where [?e :age ?a] [?e :name \"Ivan\"]]";
    dump "q3" "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e :sex :male]]";
    dump "q4" "[:find ?e ?l ?a :where [?e :name \"Ivan\"] [?e :last-name ?l] [?e :age ?a] [?e :sex :male]]";
    dump "q5" "[:find ?e1 ?l ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e1 :age ?a] [?e1 :last-name ?l]]";
    dump "qpred1" "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]";
    dump_in "qpred2" "[:find ?e ?s :in $ ?min_s :where [?e :salary ?s] [(> ?s ?min_s)]]"
      [ Arg_scalar (Result_value (Int 50_000)) ];
    dump "q-or" "[:find ?e :where (or [?e :name \"Ivan\"] [?e :name \"Petr\"])]";
    dump "q-not" "[:find ?e ?a :where [?e :age ?a] (not [?e :sex :male])]";
    dump "q-or-join" "[:find ?e ?a :where [?e :age ?a] (or-join [?e] [?e :name \"Ivan\"] [?e :name \"Petr\"])]";
    dump "q-not-join" "[:find ?e ?a :where [?e :age ?a] (not-join [?e] [?e :sex :male])]";
    dump "q-pred-range" "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)] [(< ?s 80000)]]";
    dump "q-5-merge" "[:find ?e ?n ?l ?a ?s :where [?e :name ?n] [?e :last-name ?l] [?e :age ?a] [?e :salary ?s] [?e :sex :male]]";
    dump_in "q-rule" "[:find ?e1 ?e2 :in $ % :where (follow ?e1 ?e2)]" [ Arg_rules follow_rules ];
    exit 0);
  Alcotest.run "datahike query parity"
    [
      ( "queries"
      , [
          test_case "q1 name lookup" `Quick
            (fun () ->
              check_query "q1" 250 "780fcaea87b17bebd114540b5eaf652c"
                "[:find ?e :where [?e :name \"Ivan\"]]")
        ; test_case "q2 name and age" `Quick
            (fun () ->
              check_query "q2" 250 "1aec6a903ad75d94ee5ded861793211a"
                "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]")
        ; test_case "q2-switch clause order" `Quick
            (fun () ->
              check_query "q2-switch" 250 "1aec6a903ad75d94ee5ded861793211a"
                "[:find ?e ?a :where [?e :age ?a] [?e :name \"Ivan\"]]")
        ; test_case "q3 name age sex" `Quick
            (fun () ->
              check_query "q3" 126 "8a4d70ec7d9fb33625b7e1f2d0326093"
                "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e :sex :male]]")
        ; test_case "q4 name last-name age sex" `Quick
            (fun () ->
              check_query "q4" 126 "3c5b56081b70ece6e365b8692e1a1377"
                "[:find ?e ?l ?a :where [?e :name \"Ivan\"] [?e :last-name ?l] [?e :age ?a] [?e :sex :male]]")
        ; test_case "last-name AEVT attr slice" `Quick
            (fun () ->
              let db = Lazy.force db in
              check_int "last-name datoms"
                2000
                (datoms db Aevt ~a:"last-name" () |> List.of_seq |> List.length))
        ; test_case "q5 cross-entity age join" `Quick
            (fun () ->
              check_query "q5" 1000 "a7a229a8898b5406488910ed4a7486dc"
                "[:find ?e1 ?l ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e1 :age ?a] [?e1 :last-name ?l]]")
        ; test_case "qpred1 salary predicate" `Quick
            (fun () ->
              check_query "qpred1" 997 "e4d5c52c111db71906000b3929ad50e3"
                "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]")
        ; test_case "qpred2 salary predicate with input" `Quick
            (fun () ->
              check_query_inputs "qpred2" 997 "e4d5c52c111db71906000b3929ad50e3"
                "[:find ?e ?s :in $ ?min_s :where [?e :salary ?s] [(> ?s ?min_s)]]"
                [ Arg_scalar (Result_value (Int 50_000)) ])
        ; test_case "q-or names" `Quick
            (fun () ->
              check_query "q-or" 500 "c6a640c51b7729e6c19ad62b389139e4"
                "[:find ?e :where (or [?e :name \"Ivan\"] [?e :name \"Petr\"])]")
        ; test_case "q-not not male" `Quick
            (fun () ->
              check_query "q-not" 1012 "9ef16dcc5f56ba6bf085c326db4e258a"
                "[:find ?e ?a :where [?e :age ?a] (not [?e :sex :male])]")
        ; test_case "q-or-join names" `Quick
            (fun () ->
              check_query "q-or-join" 500 "e7953f1cd05ffbbdbe192c8bb7599efe"
                "[:find ?e ?a :where [?e :age ?a] (or-join [?e] [?e :name \"Ivan\"] [?e :name \"Petr\"])]")
        ; test_case "q-not-join not male" `Quick
            (fun () ->
              check_query "q-not-join" 1012 "9ef16dcc5f56ba6bf085c326db4e258a"
                "[:find ?e ?a :where [?e :age ?a] (not-join [?e] [?e :sex :male])]")
        ; test_case "q-pred-range salary range" `Quick
            (fun () ->
              check_query "q-pred-range" 616 "f0414689e934bd25a597c2102c5e4475"
                "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)] [(< ?s 80000)]]")
        ; test_case "q-5-merge male attrs" `Quick
            (fun () ->
              check_query "q-5-merge" 988 "d7a75b59b1f97c417173a63821d3bd31"
                "[:find ?e ?n ?l ?a ?s :where [?e :name ?n] [?e :last-name ?l] [?e :age ?a] [?e :salary ?s] [?e :sex :male]]")
        ; test_case "q-rule non-recursive" `Quick
            (fun () ->
              check_query_inputs "q-rule" 667 "d1c7c5173bb8c5ff34ecbeeed24acc17"
                "[:find ?e1 ?e2 :in $ % :where (follow ?e1 ?e2)]"
                [ Arg_rules follow_rules ])
        ] )
    ]
