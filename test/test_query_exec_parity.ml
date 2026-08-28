(** Query_exec path probes + fused vs relational fallback parity.

    Complements [test_shared_queries] (result goldens) by asserting:
    1. hot shared shapes actually take [Fused_execute]
    2. forcing relational fallback yields identical sorted digests
    3. known non-fused shapes stay on [Relation_fallback] *)

open Alcotest
open Datascript
open Test_alcotest_support

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

let ref_many =
  { cardinality = Many
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
let rand_sex rng = sexes.(next_int rng 997 mod Array.length sexes)

let build_db size =
  let rng = rng 1 in
  let entities =
    List.init size (fun index ->
        let i = index + 1 in
        Entity
          { db_id = Some (Temp_id (string_of_int i))
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

let path_name = function
  | Fused_execute -> "fused"
  | Relation_fallback -> "fallback"
  | Binding_interpreter -> "binding"

let check_path name expected =
  check string (name ^ "-path") (path_name expected) (path_name (last_query_exec_path ()))

let db = lazy (build_db 500)

let plan_of query_string =
  let query = parse_query_string query_string in
  match Query_plan.compile ~max_datom_e:(Lazy.force db).max_datom_e query.where with
  | Some plan -> plan
  | None -> failwith ("expected plan for " ^ query_string)

type case =
  { name : string
  ; query : string
  ; inputs : query_arg list
  ; expect_path : query_exec_path
  ; expect_fused_plan : bool
  }

let fused_cases =
  [ { name = "q1"
    ; query = "[:find ?e :where [?e :name \"Ivan\"]]"
    ; inputs = []
    ; expect_path = Fused_execute
    ; expect_fused_plan = true
    }
  ; { name = "q2"
    ; query = "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]"
    ; inputs = []
    ; expect_path = Fused_execute
    ; expect_fused_plan = true
    }
  ; { name = "q2-switch"
    ; query = "[:find ?e ?a :where [?e :age ?a] [?e :name \"Ivan\"]]"
    ; inputs = []
    ; expect_path = Fused_execute
    ; expect_fused_plan = true
    }
  ; { name = "q3"
    ; query = "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e :sex :male]]"
    ; inputs = []
    ; expect_path = Fused_execute
    ; expect_fused_plan = true
    }
  ; { name = "q-5-merge"
    ; query =
        "[:find ?e ?n ?l ?a ?s :where [?e :name ?n] [?e :last-name ?l] [?e :age ?a] [?e :salary ?s] [?e :sex :male]]"
    ; inputs = []
    ; expect_path = Fused_execute
    ; expect_fused_plan = true
    }
  ; { name = "q-not"
    ; query = "[:find ?e ?a :where [?e :age ?a] (not [?e :sex :male])]"
    ; inputs = []
    ; expect_path = Fused_execute
    ; expect_fused_plan = true
    }
  ; { name = "q-not-join"
    ; query = "[:find ?e ?a :where [?e :age ?a] (not-join [?e] [?e :sex :male])]"
    ; inputs = []
    ; expect_path = Fused_execute
    ; expect_fused_plan = true
    }
  ]

let fallback_cases =
  [ { name = "q-or"
    ; query = "[:find ?e :where (or [?e :name \"Ivan\"] [?e :name \"Petr\"])]"
    ; inputs = []
    ; expect_path = Relation_fallback
    ; expect_fused_plan = false
    }
  ; { name = "q-or-join"
    ; query =
        "[:find ?e ?a :where [?e :age ?a] (or-join [?e] [?e :name \"Ivan\"] [?e :name \"Petr\"])]"
    ; inputs = []
    ; expect_path = Relation_fallback
    ; expect_fused_plan = false
    }
  ; { name = "q-rule"
    ; query = "[:find ?e1 ?e2 :in $ % :where (follow ?e1 ?e2)]"
    ; inputs = [ Arg_rules follow_rules ]
    ; expect_path = Relation_fallback
    ; expect_fused_plan = false
    }
  ; { name = "qpred2-input"
    ; query = "[:find ?e ?s :in $ ?min_s :where [?e :salary ?s] [(> ?s ?min_s)]]"
    ; inputs = [ Arg_scalar (Result_value (Int 50_000)) ]
    ; expect_path = Relation_fallback
    ; expect_fused_plan = false
    }
  ]

let run_case db case =
  match case.inputs with
  | [] -> q_string db case.query
  | inputs -> q_string ~inputs db case.query

let test_fused_path_and_plan_shape () =
  let db = Lazy.force db in
  List.iter
    (fun case ->
      let plan = plan_of case.query in
      check_bool (case.name ^ "-fused-plan") case.expect_fused_plan
        (Query_plan.plan_is_fused_execute plan);
      ignore (run_case db case);
      check_path case.name case.expect_path)
    fused_cases

let test_fallback_path_shapes () =
  let db = Lazy.force db in
  List.iter
    (fun case ->
      (match case.inputs with
       | [] ->
         let plan = plan_of case.query in
         check_bool (case.name ^ "-not-fused-plan") (not case.expect_fused_plan)
           (not (Query_plan.plan_is_fused_execute plan))
       | _ -> ());
      ignore (run_case db case);
      check_path case.name case.expect_path)
    fallback_cases

let test_force_fallback_parity () =
  let db = Lazy.force db in
  List.iter
    (fun case ->
      let fused_rows = run_case db case in
      check_path (case.name ^ "-before-force") Fused_execute;
      let fused_digest = rows_digest fused_rows in
      let fallback_rows =
        with_force_relation_fallback (fun () ->
            let rows = run_case db case in
            check_path (case.name ^ "-forced") Relation_fallback;
            rows)
      in
      check string (case.name ^ "-digest-parity") fused_digest (rows_digest fallback_rows);
      check_int (case.name ^ "-count-parity") (List.length fused_rows) (List.length fallback_rows))
    fused_cases

let test_force_fallback_restores () =
  let db = Lazy.force db in
  let case = List.hd fused_cases in
  ignore (with_force_relation_fallback (fun () -> run_case db case));
  ignore (run_case db case);
  check_path "restored-after-force" Fused_execute

let () =
  run "query exec parity"
    [ ( "path"
      , [ test_case "fused shapes use Query_exec" `Quick test_fused_path_and_plan_shape
        ; test_case "fallback shapes stay relational" `Quick test_fallback_path_shapes
        ; test_case "force fallback matches fused digests" `Quick test_force_fallback_parity
        ; test_case "force fallback restores fused path" `Quick test_force_fallback_restores
        ] )
    ]
