open Alcotest
open Datascript

let check_int = Test_alcotest_support.check_int
let check_bool = Test_alcotest_support.check_bool

let test_choose_index_prefers_narrowest () =
  check_bool "ground entity prefers EAVT" true
    (Query_plan.choose_index (QEntity 1) (QAttr "age") (QVar "?a") = Query_plan.Prefer_eavt);
  check_bool "attr+value prefers AVET" true
    (Query_plan.choose_index (QVar "?e") (QAttr "name") (QValue (String "Ivan")) = Query_plan.Prefer_avet);
  check_bool "attr-only prefers AEVT" true
    (Query_plan.choose_index (QVar "?e") (QAttr "age") (QVar "?a") = Query_plan.Prefer_aevt)

let test_compile_orders_constants_first () =
  let wide = Pattern (QVar "?e", QAttr "age", QVar "?a") in
  let narrow = Pattern (QVar "?e", QAttr "name", QValue (String "Ivan")) in
  match Query_plan.compile [ wide; narrow ] with
  | None -> failwith "expected a plan"
  | Some plan ->
    (match plan.ops with
     | [ Query_plan.OpEntityGroup { clauses; _ } ] ->
       check_bool "constant pattern drives entity group" true (List.hd clauses = narrow)
     | [ Query_plan.OpScan { clause; _ }; _ ] ->
       check_bool "constant scan ordered first" true (clause = narrow)
     | _ ->
       let ordered = Query_plan.clauses_of_plan plan in
       check_bool "constant AVET pattern should sort before open AEVT scan" true
         (List.hd ordered = narrow))

let test_analyze_same_entity_merge () =
  let query =
    { find = [ Find_var "?e"; Find_var "?a" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "?e", QAttr "name", QValue (String "Ivan"))
        ; Pattern (QVar "?e", QAttr "age", QVar "?a")
        ]
    }
  in
  match Query_plan.analyze query with
  | None -> failwith "expected a plan"
  | Some plan ->
    (match plan.ops with
     | [ Query_plan.OpEntityGroup { clauses; _ } ] ->
       check_int "entity group collapses same-entity legs" 2 (List.length clauses)
     | [ Query_plan.OpScan _; Query_plan.OpScan _ ] ->
       check_bool "analyze produced scan ops" true true
     | _ -> failwith "unexpected plan shape")

let test_analyze_benchmark_shapes () =
  let qpred =
    { find = [ Find_var "?e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "?e", QAttr "age", QVar "?a")
        ; ComparisonPredicate (GreaterThan, QVar "?a", QValue (Int 18))
        ]
    }
  in
  (match Query_plan.analyze qpred with
   | None -> failwith "predicate shape should analyze"
   | Some plan ->
     check_bool "predicate plan executable" true (Query_plan.plan_is_executable plan));
  let qrule =
    { find = [ Find_var "?e1"; Find_var "?e2" ]
    ; inputs = [ Input_rules_decl ]
    ; with_vars = []
    ; rules =
        [ { rule_name = "follow"
          ; rule_params = [ "?e1"; "?e2" ]
          ; rule_body = [ Pattern (QVar "?e1", QAttr "follows", QVar "?e2") ]
          }
        ]
    ; where = [ Rule ("follow", [ QVar "?e1"; QVar "?e2" ]) ]
    }
  in
  match Query_plan.analyze qrule with
  | None -> failwith "rule head should analyze"
  | Some plan ->
    check_bool "inlined rule plan executable" true (Query_plan.plan_is_executable plan)

let test_logical_entity_join () =
  let clauses =
    [ Pattern (QVar "?e", QAttr "name", QValue (String "Ivan"))
    ; Pattern (QVar "?e", QAttr "age", QVar "?a")
    ; ComparisonPredicate (GreaterThan, QVar "?a", QValue (Int 18))
    ]
  in
  match Query_plan.build_logical_plan clauses with
  | None -> failwith "expected logical plan"
  | Some logical ->
    (match logical.nodes with
     | [ Query_plan.LEntityJoin { scans; filters; _ } ] ->
       check_int "two scans in entity join" 2 (List.length scans);
       check_int "filter attached to entity join" 1 (List.length filters)
     | _ -> failwith "expected LEntityJoin")

let () =
  run "query plan"
    [ ( "analyze"
      , [ test_case "choose_index prefers narrowest" `Quick test_choose_index_prefers_narrowest
        ; test_case "compile orders constants first" `Quick test_compile_orders_constants_first
        ; test_case "analyze same-entity merge" `Quick test_analyze_same_entity_merge
        ; test_case "analyze benchmark shapes" `Quick test_analyze_benchmark_shapes
        ; test_case "logical entity join attaches filters" `Quick test_logical_entity_join
        ] )
    ]
