(** Datahike shared-API category parity tests.
    Covers queries / writes / rules / aggregates / temporal / joins with
    deterministic fixtures and identical result-set assertions (not just counts). *)

open Alcotest
open Datascript
open Test_alcotest_support

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

let ref_one =
  { indexed with indexed = false; value_type = Some RefType }

let ref_many = { ref_one with cardinality = Many }

let sort_rows rows =
  List.sort
    (fun left right ->
      compare
        (List.map (fun r -> match r with Result_value v -> v | Result_entity e -> Int e | _ -> Nil) left)
        (List.map (fun r -> match r with Result_value v -> v | Result_entity e -> Int e | _ -> Nil) right))
    rows

let check_rows label expected actual =
  check
    (list (list (testable (fun fmt r -> Format.pp_print_string fmt (match r with
       | Result_value (Int i) -> string_of_int i
       | Result_value (Float f) -> string_of_float f
       | Result_value (String s) -> Printf.sprintf "%S" s
       | Result_value (Keyword k) -> ":" ^ k
       | Result_entity e -> "e:" ^ string_of_int e
       | _ -> "?")) ( = ))))
    label
    (sort_rows expected)
    (sort_rows actual)

let rv v = Result_value v
let re e = Result_entity e

let float_close label expected actual =
  match actual with
  | Result_value (Float value) ->
    if abs_float (value -. expected) > 1e-9 then
      failf "%s: expected %g, got %g" label expected value
  | Result_value (Int value) when float_of_int value = expected -> ()
  | _ -> failf "%s: expected float %g" label expected

(* ---------- people fixture (queries + aggregates) ---------- *)

let people_schema =
  [ "name", indexed
  ; "last-name", indexed
  ; "sex", indexed
  ; "age", indexed
  ; "salary", indexed
  ; "follows", ref_many
  ]

let people_db () =
  empty_db ~schema:people_schema ()
  |> db_with
       [ Entity
           { db_id = Some (Entity_id 1)
           ; attrs =
               [ "name", One_value (String "Ivan")
               ; "last-name", One_value (String "Ivanov")
               ; "sex", One_value (Keyword "male")
               ; "age", One_value (Int 30)
               ; "salary", One_value (Int 60_000)
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 2)
           ; attrs =
               [ "name", One_value (String "Petr")
               ; "last-name", One_value (String "Petrov")
               ; "sex", One_value (Keyword "male")
               ; "age", One_value (Int 25)
               ; "salary", One_value (Int 40_000)
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 3)
           ; attrs =
               [ "name", One_value (String "Ivan")
               ; "last-name", One_value (String "Sidorov")
               ; "sex", One_value (Keyword "female")
               ; "age", One_value (Int 30)
               ; "salary", One_value (Int 80_000)
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 4)
           ; attrs =
               [ "name", One_value (String "Oleg")
               ; "last-name", One_value (String "Kovalev")
               ; "sex", One_value (Keyword "female")
               ; "age", One_value (Int 40)
               ; "salary", One_value (Int 55_000)
               ]
           }
       ]
  |> db_with
       [ Add (Entity_id 1, "follows", Ref 2)
       ; Add (Entity_id 2, "follows", Ref 3)
       ]

let follow_rules_nonrec =
  Parser.parse_rules
    (QueryFormVector
       [ QueryFormVector
           [ QueryFormVector [ QueryFormSymbol "follow"; QueryFormSymbol "?e1"; QueryFormSymbol "?e2" ]
           ; QueryFormVector
               [ QueryFormSymbol "?e1"; QueryFormKeyword "follows"; QueryFormSymbol "?e2" ]
           ] ])

let follow_rules_rec =
  Parser.parse_rules
    (QueryFormVector
       [ QueryFormVector
           [ QueryFormVector [ QueryFormSymbol "follows"; QueryFormSymbol "?x"; QueryFormSymbol "?y" ]
           ; QueryFormVector
               [ QueryFormSymbol "?x"; QueryFormKeyword "follows"; QueryFormSymbol "?y" ]
           ]
       ; QueryFormVector
           [ QueryFormVector [ QueryFormSymbol "follows"; QueryFormSymbol "?x"; QueryFormSymbol "?y" ]
           ; QueryFormVector
               [ QueryFormSymbol "?x"; QueryFormKeyword "follows"; QueryFormSymbol "?t" ]
           ; QueryFormVector
               [ QueryFormSymbol "follows"; QueryFormSymbol "?t"; QueryFormSymbol "?y" ]
           ]
       ])

(* ---------- queries category ---------- *)

let test_queries () =
  let db = people_db () in
  check_rows "q1"
    [ [ re 1 ]; [ re 3 ] ]
    (q_string db "[:find ?e :where [?e :name \"Ivan\"]]");
  check_rows "q2"
    [ [ re 1; rv (Int 30) ]; [ re 3; rv (Int 30) ] ]
    (q_string db "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]");
  check_rows "q2-switch"
    [ [ re 1; rv (Int 30) ]; [ re 3; rv (Int 30) ] ]
    (q_string db "[:find ?e ?a :where [?e :age ?a] [?e :name \"Ivan\"]]");
  check_rows "q3"
    [ [ re 1; rv (Int 30) ] ]
    (q_string db "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e :sex :male]]");
  check_rows "q4"
    [ [ re 1; rv (String "Ivanov"); rv (Int 30) ] ]
    (q_string db
       "[:find ?e ?l ?a :where [?e :name \"Ivan\"] [?e :last-name ?l] [?e :age ?a] [?e :sex :male]]");
  (* ?l is bound from ?e1 (same-age peers), not crossed with Ivan last-names. *)
  check_rows "q5"
    [ [ re 1; rv (String "Ivanov"); rv (Int 30) ]
    ; [ re 3; rv (String "Sidorov"); rv (Int 30) ]
    ]
    (q_string db
       "[:find ?e1 ?l ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e1 :age ?a] [?e1 :last-name ?l]]");
  check_rows "qpred1"
    [ [ re 1; rv (Int 60_000) ]; [ re 3; rv (Int 80_000) ]; [ re 4; rv (Int 55_000) ] ]
    (q_string db "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]");
  check_rows "qpred2"
    [ [ re 1; rv (Int 60_000) ]; [ re 3; rv (Int 80_000) ]; [ re 4; rv (Int 55_000) ] ]
    (q_string ~inputs:[ Arg_scalar (Result_value (Int 50_000)) ] db
       "[:find ?e ?s :in $ ?min_s :where [?e :salary ?s] [(> ?s ?min_s)]]");
  check_rows "q-or"
    [ [ re 1 ]; [ re 2 ]; [ re 3 ] ]
    (q_string db "[:find ?e :where (or [?e :name \"Ivan\"] [?e :name \"Petr\"])]");
  check_rows "q-not"
    [ [ re 3; rv (Int 30) ]; [ re 4; rv (Int 40) ] ]
    (q_string db "[:find ?e ?a :where [?e :age ?a] (not [?e :sex :male])]");
  check_rows "q-or-join"
    [ [ re 1; rv (Int 30) ]; [ re 2; rv (Int 25) ]; [ re 3; rv (Int 30) ] ]
    (q_string db
       "[:find ?e ?a :where [?e :age ?a] (or-join [?e] [?e :name \"Ivan\"] [?e :name \"Petr\"])]");
  check_rows "q-not-join"
    [ [ re 3; rv (Int 30) ]; [ re 4; rv (Int 40) ] ]
    (q_string db "[:find ?e ?a :where [?e :age ?a] (not-join [?e] [?e :sex :male])]");
  check_rows "q-pred-range"
    [ [ re 1; rv (Int 60_000) ]; [ re 4; rv (Int 55_000) ] ]
    (q_string db "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)] [(< ?s 80000)]]");
  check_rows "q-5-merge"
    [ [ re 1
      ; rv (String "Ivan")
      ; rv (String "Ivanov")
      ; rv (Int 30)
      ; rv (Int 60_000)
      ]
    ; [ re 2
      ; rv (String "Petr")
      ; rv (String "Petrov")
      ; rv (Int 25)
      ; rv (Int 40_000)
      ]
    ]
    (q_string db
       "[:find ?e ?n ?l ?a ?s :where [?e :name ?n] [?e :last-name ?l] [?e :age ?a] [?e :salary ?s] [?e :sex :male]]");
  check_rows "q-rule"
    [ [ re 1; re 2 ]; [ re 2; re 3 ] ]
    (q_string ~inputs:[ Arg_rules follow_rules_nonrec ] db
       "[:find ?e1 ?e2 :in $ % :where (follow ?e1 ?e2)]")

(* ---------- writes category ---------- *)

let test_writes_add_all () =
  let people =
    List.init 5 (fun i ->
      let id = i + 1 in
      Entity
        { db_id = Some (Entity_id id)
        ; attrs =
            [ "name", One_value (String (Printf.sprintf "p-%d" id))
            ; "age", One_value (Int (20 + id))
            ]
        })
  in
  let db = db_with people (empty_db ~schema:[ "name", indexed; "age", indexed ] ()) in
  check_rows "add-all names"
    [ [ re 1; rv (String "p-1") ]
    ; [ re 2; rv (String "p-2") ]
    ; [ re 3; rv (String "p-3") ]
    ; [ re 4; rv (String "p-4") ]
    ; [ re 5; rv (String "p-5") ]
    ]
    (q_string db "[:find ?e ?n :where [?e :name ?n]]");
  check_int "add-all age datoms" 5 (datoms db Eavt ~a:"age" () |> Seq.length)

let test_writes_add_5 () =
  let schema = [ "name", indexed; "age", indexed ] in
  let db =
    List.fold_left
      (fun db id ->
        db_with
          [ Entity
              { db_id = Some (Entity_id id)
              ; attrs =
                  [ "name", One_value (String (Printf.sprintf "p-%d" id))
                  ; "age", One_value (Int (20 + id))
                  ]
              }
          ]
          db)
      (empty_db ~schema ())
      [ 1; 2; 3; 4; 5 ]
  in
  check_rows "add-5 ages"
    [ [ re 1; rv (Int 21) ]
    ; [ re 2; rv (Int 22) ]
    ; [ re 3; rv (Int 23) ]
    ; [ re 4; rv (Int 24) ]
    ; [ re 5; rv (Int 25) ]
    ]
    (q_string db "[:find ?e ?a :where [?e :age ?a]]")

(* ---------- rules category (recursive) ---------- *)

let wide_db depth width =
  (* Port of Datahike wide-db-data: each node has [width] children, [depth] levels. *)
  let rec build id depth =
    if depth <= 0 then [ Entity { db_id = Some (Temp_id (string_of_int id)); attrs = [ "name", One_value (String "Ivan") ] } ]
    else
      let children = List.init width (fun i -> (id * width) + i) in
      let edges =
        List.map
          (fun child ->
            Entity
              { db_id = Some (Temp_id (string_of_int id))
              ; attrs =
                  [ "name", One_value (String "Ivan")
                  ; "follows", One_value (Ref_to (Temp_id (string_of_int child)))
                  ]
              })
          children
      in
      edges @ List.concat_map (fun child -> build child (depth - 1)) children
  in
  db_with (build 1 depth) (empty_db ~schema:[ "name", indexed; "follows", ref_many ] ())

let long_db depth width =
  let ops =
    List.concat
      (List.init width (fun x ->
         List.init depth (fun y ->
           let from_id = (x * (depth + 1)) + y in
           let to_id = from_id + 1 in
           [ Entity
               { db_id = Some (Temp_id (string_of_int from_id))
               ; attrs =
                   [ "name", One_value (String "Ivan")
                   ; "follows", One_value (Ref_to (Temp_id (string_of_int to_id)))
                   ]
               }
           ; Entity
               { db_id = Some (Temp_id (string_of_int to_id))
               ; attrs = [ "name", One_value (String "Ivan") ]
               }
           ])))
  in
  db_with (List.concat ops) (empty_db ~schema:[ "name", indexed; "follows", ref_many ] ())

let test_rules_wide_3x3 () =
  let db = wide_db 3 3 in
  let rows =
    q_string ~inputs:[ Arg_rules follow_rules_rec ] db
      "[:find ?e ?e2 :in $ % :where (follows ?e ?e2)]"
  in
  (* 39 direct edges; recursive follows yields 102 distinct reachable pairs. *)
  check_int "rules-wide-3x3 count" 102 (List.length rows)

let test_rules_wide_5x3 () =
  let db = wide_db 5 3 in
  let rows =
    q_string ~inputs:[ Arg_rules follow_rules_rec ] db
      "[:find ?e ?e2 :in $ % :where (follows ?e ?e2)]"
  in
  check_int "rules-wide-5x3 count" 1641 (List.length rows)

let test_rules_long_10x3 () =
  let db = long_db 10 3 in
  let rows =
    q_string ~inputs:[ Arg_rules follow_rules_rec ] db
      "[:find ?e ?e2 :in $ % :where (follows ?e ?e2)]"
  in
  (* 3 chains × (10+9+...+1) = 3 × 55 = 165 transitive pairs *)
  check_int "rules-long-10x3 count" 165 (List.length rows)

let test_rules_long_30x3 () =
  let db = long_db 30 3 in
  let rows =
    q_string ~inputs:[ Arg_rules follow_rules_rec ] db
      "[:find ?e ?e2 :in $ % :where (follows ?e ?e2)]"
  in
  (* 3 × (30+29+...+1) = 3 × 465 = 1395 *)
  check_int "rules-long-30x3 count" 1395 (List.length rows)

let test_rules_small_exact () =
  let db = people_db () in
  check_rows "recursive follows exact"
    [ [ re 1; re 2 ]; [ re 1; re 3 ]; [ re 2; re 3 ] ]
    (q_string ~inputs:[ Arg_rules follow_rules_rec ] db
       "[:find ?e ?e2 :in $ % :where (follows ?e ?e2)]")

(* ---------- aggregates category ---------- *)

let test_aggregates () =
  let db = people_db () in
  (match q_string db "[:find (avg ?s) :where [?e :salary ?s]]" with
   | [ [ avg ] ] -> float_close "q-agg-avg" 58750.0 avg
   | rows -> failf "q-agg-avg unexpected rows: %d" (List.length rows));
  check_rows "q-agg-group"
    [ [ rv (Keyword "female"); rv (Float 67500.0); rv (Int 2) ]
    ; [ rv (Keyword "male"); rv (Float 50000.0); rv (Int 2) ]
    ]
    (q_string db "[:find ?sex (avg ?s) (count ?e) :where [?e :sex ?sex] [?e :salary ?s]]");
  (match q_string db "[:find (avg ?s) (min ?s) (max ?s) :where [?e :salary ?s] [?e :sex :male]]" with
   | [ [ avg; min_v; max_v ] ] ->
     float_close "q-agg-filter avg" 50000.0 avg;
     check_rows "q-agg-filter min/max" [ [ min_v; max_v ] ] [ [ rv (Int 40_000); rv (Int 60_000) ] ]
   | _ -> failf "q-agg-filter shape");
  check_rows "q-agg-pred"
    [ [ rv (Keyword "female"); rv (Float 67500.0) ]
    ; [ rv (Keyword "male"); rv (Float 60000.0) ]
    ]
    (q_string db
       "[:find ?sex (avg ?s) :where [?e :salary ?s] [?e :sex ?sex] [(> ?s 50000)]]");
  check_rows "q-agg-multi"
    [ [ rv (Keyword "female"); rv (String "Ivan"); rv (Float 80000.0) ]
    ; [ rv (Keyword "female"); rv (String "Oleg"); rv (Float 55000.0) ]
    ; [ rv (Keyword "male"); rv (String "Ivan"); rv (Float 60000.0) ]
    ; [ rv (Keyword "male"); rv (String "Petr"); rv (Float 40000.0) ]
    ]
    (q_string db
       "[:find ?sex ?n (avg ?s) :where [?e :sex ?sex] [?e :name ?n] [?e :salary ?s]]");
  (match
     q_string db "[:find (avg ?s) (variance ?s) (stddev ?s) (median ?s) :where [?e :salary ?s]]"
   with
   | [ [ avg; variance; stddev; median ] ] ->
     float_close "q-agg-stats avg" 58750.0 avg;
     float_close "q-agg-stats median" 57500.0 median;
     (match variance, stddev with
      | Result_value (Float v), Result_value (Float s) ->
        check_bool "q-agg-stats variance positive" true (v > 0.0);
        check_bool "q-agg-stats stddev=sqrt(variance)" true (abs_float (s -. sqrt v) < 1e-9)
      | _ -> failf "q-agg-stats variance/stddev types")
   | _ -> failf "q-agg-stats shape")

(* ---------- temporal category ---------- *)

let temporal_fixture () =
  (* Match test_tx_history: db_with → basis_tx → db_with → as_of tx0. *)
  let schema = [ "name", indexed; "age", indexed; "sex", indexed ] in
  let db =
    db_with
      [ Entity
          { db_id = Some (Entity_id 1)
          ; attrs =
              [ "name", One_value (String "Ivan")
              ; "age", One_value (Int 20)
              ; "sex", One_value (Keyword "male")
              ]
          }
      ; Entity
          { db_id = Some (Entity_id 2)
          ; attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 30) ]
          }
      ]
      (empty_db ~schema ())
  in
  let tx0 = basis_tx db in
  let current =
    db_with
      [ Add (Entity_id 1, "age", Int 21)
      ; Entity
          { db_id = Some (Entity_id 3)
          ; attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 40) ]
          }
      ]
      db
  in
  current, as_of tx0 current, history current

let test_temporal () =
  let current, as_of_db, hist = temporal_fixture () in
  check_rows "t-current-q1"
    [ [ re 1 ]; [ re 3 ] ]
    (q_string current "[:find ?e :where [?e :name \"Ivan\"]]");
  check_rows "t-current-q2"
    [ [ re 1; rv (Int 21) ]; [ re 3; rv (Int 40) ] ]
    (q_string current "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]");
  check_rows "t-asof-q1"
    [ [ re 1 ] ]
    (q_string as_of_db "[:find ?e :where [?e :name \"Ivan\"]]");
  check_rows "t-asof-q2"
    [ [ re 1; rv (Int 20) ] ]
    (q_string as_of_db "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]");
  check_rows "t-asof-q3"
    [ [ re 1; rv (Int 20) ] ]
    (q_string as_of_db
       "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e :sex :male]]");
  check_int "t-hist-q1 names" 3
    (List.length (q_string hist "[:find ?e :where [?e :name]]"));
  check_bool "t-hist-q2 age+tx non-empty" true
    (q_string hist "[:find ?e ?a ?tx :where [?e :age ?a ?tx]]" <> []);
  check_rows "t-hist-q3 name+age includes retracted age"
    [ [ re 1; rv (String "Ivan"); rv (Int 20) ]
    ; [ re 1; rv (String "Ivan"); rv (Int 21) ]
    ; [ re 2; rv (String "Petr"); rv (Int 30) ]
    ; [ re 3; rv (String "Ivan"); rv (Int 40) ]
    ]
    (q_string hist "[:find ?e ?n ?a :where [?e :name ?n] [?e :age ?a]]");
  check_rows "t-hist-retract"
    [ [ re 1; rv (Int 20) ] ]
    (q_string hist "[:find ?e ?a :where [?e :age ?a _ false]]")

(* ---------- joins category ---------- *)

let join_db () =
  let schema =
    [ "div/name", indexed
    ; "d/name", indexed
    ; "d/budget", indexed
    ; "d/div", ref_one
    ; "p/name", indexed
    ; "p/dept", ref_one
    ; "p/salary", indexed
    ]
  in
  empty_db ~schema ()
  |> db_with
       [ Entity { db_id = Some (Entity_id 1); attrs = [ "div/name", One_value (String "div-A") ] }
       ; Entity { db_id = Some (Entity_id 2); attrs = [ "div/name", One_value (String "div-B") ] }
       ; Entity
           { db_id = Some (Entity_id 10)
           ; attrs =
               [ "d/name", One_value (String "dept-99")
               ; "d/budget", One_value (Int 500_000)
               ; "d/div", One_value (Ref 1)
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 11)
           ; attrs =
               [ "d/name", One_value (String "dept-01")
               ; "d/budget", One_value (Int 420_000)
               ; "d/div", One_value (Ref 1)
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 12)
           ; attrs =
               [ "d/name", One_value (String "dept-02")
               ; "d/budget", One_value (Int 300_000)
               ; "d/div", One_value (Ref 2)
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 100)
           ; attrs =
               [ "p/name", One_value (String "p-100")
               ; "p/dept", One_value (Ref 10)
               ; "p/salary", One_value (Int 95_000)
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 101)
           ; attrs =
               [ "p/name", One_value (String "p-101")
               ; "p/dept", One_value (Ref 10)
               ; "p/salary", One_value (Int 50_000)
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 102)
           ; attrs =
               [ "p/name", One_value (String "p-102")
               ; "p/dept", One_value (Ref 11)
               ; "p/salary", One_value (Int 91_000)
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 103)
           ; attrs =
               [ "p/name", One_value (String "p-103")
               ; "p/dept", One_value (Ref 12)
               ; "p/salary", One_value (Int 70_000)
               ]
           }
       ]

let test_joins () =
  let db = join_db () in
  check_rows "q-join-ref-1"
    [ [ rv (String "p-100"); rv (String "dept-99") ]
    ; [ rv (String "p-101"); rv (String "dept-99") ]
    ]
    (q_string db
       "[:find ?pn ?dn :where [?d :d/name \"dept-99\"] [?d :d/budget ?b] [?e :p/dept ?d] [?e :p/name ?pn] [?d :d/name ?dn]]");
  check_rows "q-join-ref-10"
    [ [ rv (String "p-100"); rv (String "dept-99") ]
    ; [ rv (String "p-101"); rv (String "dept-99") ]
    ]
    (q_string db
       "[:find ?pn ?dn :where [?d :d/budget ?b] [(> ?b 450000)] [?d :d/name ?dn] [?e :p/dept ?d] [?e :p/name ?pn]]");
  check_rows "q-join-pred"
    [ [ rv (String "p-100"); rv (String "dept-99") ]
    ; [ rv (String "p-101"); rv (String "dept-99") ]
    ; [ rv (String "p-102"); rv (String "dept-01") ]
    ]
    (q_string db
       "[:find ?pn ?dn :where [?d :d/budget ?b] [(> ?b 400000)] [?d :d/name ?dn] [?e :p/dept ?d] [?e :p/name ?pn]]");
  check_rows "q-join-chain"
    [ [ rv (String "p-100"); rv (String "dept-99"); rv (String "div-A") ]
    ; [ rv (String "p-101"); rv (String "dept-99"); rv (String "div-A") ]
    ; [ rv (String "p-102"); rv (String "dept-01"); rv (String "div-A") ]
    ; [ rv (String "p-103"); rv (String "dept-02"); rv (String "div-B") ]
    ]
    (q_string db
       "[:find ?pn ?dn ?divn :where [?e :p/name ?pn] [?e :p/dept ?d] [?d :d/name ?dn] [?d :d/div ?div] [?div :div/name ?divn]]");
  check_rows "q-join-selective"
    [ [ rv (String "p-100"); rv (String "dept-99") ]
    ; [ rv (String "p-102"); rv (String "dept-01") ]
    ]
    (q_string db
       "[:find ?pn ?dn :where [?e :p/salary ?s] [(> ?s 90000)] [?e :p/name ?pn] [?e :p/dept ?d] [?d :d/name ?dn]]")

let () =
  run "datahike category parity"
    [ ( "queries"
      , [ test_case "all query shapes exact rows" `Quick test_queries ] )
    ; ( "writes"
      , [ test_case "add-all bulk insert result set" `Quick test_writes_add_all
        ; test_case "add-5 sequential insert result set" `Quick test_writes_add_5
        ] )
    ; ( "rules"
      , [ test_case "recursive follows exact people fixture" `Quick test_rules_small_exact
        ; test_case "rules-wide-3x3 count" `Quick test_rules_wide_3x3
        ; test_case "rules-wide-5x3 count" `Quick test_rules_wide_5x3
        ; test_case "rules-long-10x3 count" `Quick test_rules_long_10x3
        ; test_case "rules-long-30x3 count" `Quick test_rules_long_30x3
        ] )
    ; ( "aggregates"
      , [ test_case "all aggregate shapes" `Quick test_aggregates ] )
    ; ( "temporal"
      , [ test_case "all temporal query shapes" `Quick test_temporal ] )
    ; ( "joins"
      , [ test_case "all join shapes exact rows" `Quick test_joins ] )
    ]
