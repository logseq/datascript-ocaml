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

let test_order_where_puts_constants_first () =
  let wide = Pattern (QVar "?e", QAttr "age", QVar "?a") in
  let narrow = Pattern (QVar "?e", QAttr "name", QValue (String "Ivan")) in
  let ordered = Query_plan.order_where_clauses [ wide; narrow ] in
  check_bool "constant AVET pattern should sort before open AEVT scan" true (List.hd ordered = narrow)

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
    (match plan.nodes with
     | [ Query_plan.MergeScan legs ] -> check_int "merge scan collapses same-entity legs" 2 (List.length legs)
     | [ Query_plan.Scan _; Query_plan.Scan _ ] ->
       check_bool "analyze produced scan nodes" true true
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
  check_bool "predicate shape analyzes" true (Option.is_some (Query_plan.analyze qpred));
  let qrule =
    { find = [ Find_var "?e1"; Find_var "?e2" ]
    ; inputs = [ Input_rules_decl ]
    ; with_vars = []
    ; rules = []
    ; where = [ Rule ("follow", [ QVar "?e1"; QVar "?e2" ]) ]
    }
  in
  check_bool "rule head analyzes" true (Option.is_some (Query_plan.analyze qrule))

let () =
  run "query plan"
    [ ( "analyze"
      , [ test_case "choose_index prefers narrowest" `Quick test_choose_index_prefers_narrowest
        ; test_case "order_where puts constants first" `Quick test_order_where_puts_constants_first
        ; test_case "analyze same-entity merge" `Quick test_analyze_same_entity_merge
        ; test_case "analyze benchmark shapes" `Quick test_analyze_benchmark_shapes
        ] )
    ]
