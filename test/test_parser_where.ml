open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal label expected actual =
  if expected <> actual then failf "%s" label

let assert_invalid label f =
  match f () with
  | exception Invalid_argument _ -> ()
  | exception exn -> failf "%s: unexpected exception %s" label (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

let sym name = QueryFormSymbol name
let kw name = QueryFormKeyword name
let int value = QueryFormInt value
let str value = QueryFormString value
let vec forms = QueryFormVector forms
let list forms = QueryFormList forms

let test_parser_where__pattern () =
  assert_equal "three term pattern" (Pattern (QVar "e", QVar "a", QVar "v")) (Parser.parse_clause (vec [ sym "?e"; sym "?a"; sym "?v" ]));
  assert_equal "four term wildcard pattern" (PatternTx (QWildcard, QVar "a", QWildcard, QWildcard)) (Parser.parse_clause (vec [ sym "_"; sym "?a"; sym "_"; sym "_" ]));
  assert_equal
    "source pattern"
    (SourcePatternTx ("x", QWildcard, QVar "a", QWildcard, QWildcard))
    (Parser.parse_clause (vec [ sym "$x"; sym "_"; sym "?a"; sym "_"; sym "_" ]));
  assert_equal
    "source keyword pattern"
    (SourcePattern ("x", QWildcard, QAttr "name", QVar "v"))
    (Parser.parse_clause (vec [ sym "$x"; sym "_"; kw "name"; sym "?v" ]));
  assert_equal
    "source symbol pattern"
    (SourcePattern ("x", QWildcard, QValue (Symbol "sym"), QVar "v"))
    (Parser.parse_clause (vec [ sym "$x"; sym "_"; sym "sym"; sym "?v" ]));
  assert_equal
    "source-like symbol is constant outside source position"
    (SourcePattern ("x", QWildcard, QValue (Symbol "$src-sym"), QVar "v"))
    (Parser.parse_clause (vec [ sym "$x"; sym "_"; sym "$src-sym"; sym "?v" ]));
  assert_invalid "empty pattern rejected" (fun () -> ignore (Parser.parse_clause (vec [])))

let test_parser_where__test_pred () =
  assert_equal
    "plain predicate"
    (DynamicPredicate ("pred", [ QVar "a"; QValue (Int 1) ]))
    (Parser.parse_clause (vec [ list [ sym "pred"; sym "?a"; int 1 ] ]));
  assert_equal "plain predicate no args" (DynamicPredicate ("pred", [])) (Parser.parse_clause (vec [ list [ sym "pred" ] ]));
  assert_equal
    "custom predicate"
    (DynamicPredicate ("custom-pred", [ QVar "a" ]))
    (Parser.parse_clause (vec [ list [ sym "?custom-pred"; sym "?a" ] ]))

let test_parser_where__test_fn () =
  assert_equal
    "plain function"
    (DynamicFunction ("fn", [ QVar "a"; QValue (Int 1) ], [ "x" ]))
    (Parser.parse_clause (vec [ list [ sym "fn"; sym "?a"; int 1 ]; sym "?x" ]));
  assert_equal "plain function no args" (DynamicFunction ("fn", [], [ "x" ])) (Parser.parse_clause (vec [ list [ sym "fn" ]; sym "?x" ]));
  assert_equal "custom function" (DynamicFunction ("custom-fn", [], [ "x" ])) (Parser.parse_clause (vec [ list [ sym "?custom-fn" ]; sym "?x" ]));
  assert_equal
    "custom function with arg"
    (DynamicFunction ("custom-fn", [ QVar "arg" ], [ "x" ]))
    (Parser.parse_clause (vec [ list [ sym "?custom-fn"; sym "?arg" ]; sym "?x" ]))

let test_parser_where__rule_expr () =
  assert_equal "rule expr" (Rule ("friends", [ QVar "x"; QVar "y" ])) (Parser.parse_clause (list [ sym "friends"; sym "?x"; sym "?y" ]));
  assert_equal "rule expr constant" (Rule ("friends", [ QValue (String "Ivan"); QWildcard ])) (Parser.parse_clause (list [ sym "friends"; str "Ivan"; sym "_" ]));
  assert_equal "source rule expr" (SourceRule ("1", "friends", [ QVar "x"; QVar "y" ])) (Parser.parse_clause (list [ sym "$1"; sym "friends"; sym "?x"; sym "?y" ]));
  assert_equal "rule expr symbol constant" (Rule ("friends", [ QValue (Symbol "something") ])) (Parser.parse_clause (list [ sym "friends"; sym "something" ]));
  assert_invalid "rule requires arguments" (fun () -> ignore (Parser.parse_clause (list [ sym "friends" ])))

let test_parser_where__not_clause () =
  assert_equal
    "not clause"
    (Not [ Pattern (QVar "e", QAttr "follows", QVar "x") ])
    (Parser.parse_clause (list [ sym "not"; vec [ sym "?e"; kw "follows"; sym "?x" ] ]));
  assert_equal
    "source not clause"
    (SourceNot ("1", [ SourceRelationPattern ("$", [ QVar "x" ]) ]))
    (Parser.parse_clause (list [ sym "$1"; sym "not"; vec [ sym "?x" ] ]));
  assert_equal
    "not-join clause"
    (NotJoin ([ "e"; "y" ], [ Pattern (QVar "e", QAttr "follows", QVar "x"); Pattern (QVar "x", QWildcard, QVar "y") ]))
    (Parser.parse_clause (list [ sym "not-join"; vec [ sym "?e"; sym "?y" ]; vec [ sym "?e"; kw "follows"; sym "?x" ]; vec [ sym "?x"; sym "_"; sym "?y" ] ]));
  assert_invalid "empty not-join vars" (fun () -> ignore (Parser.parse_clause (list [ sym "not-join"; vec []; vec [ sym "?y" ] ])));
  assert_invalid "empty inferred not vars" (fun () -> ignore (Parser.parse_clause (list [ sym "not"; vec [ sym "_" ] ])));
  assert_invalid "not-join missing clauses" (fun () -> ignore (Parser.parse_clause (list [ sym "not-join"; vec [ sym "?x" ] ])));
  assert_invalid "not missing clauses" (fun () -> ignore (Parser.parse_clause (list [ sym "not" ])))

let test_parser_where__or_clause () =
  assert_equal
    "or clause"
    (Or [ [ Pattern (QVar "e", QAttr "follows", QVar "x") ] ])
    (Parser.parse_clause (list [ sym "or"; vec [ sym "?e"; kw "follows"; sym "?x" ] ]));
  assert_equal
    "or multiple branches"
    (Or [ [ Pattern (QVar "e", QAttr "follows", QVar "x") ]; [ Pattern (QVar "e", QAttr "friend", QVar "x") ] ])
    (Parser.parse_clause (list [ sym "or"; vec [ sym "?e"; kw "follows"; sym "?x" ]; vec [ sym "?e"; kw "friend"; sym "?x" ] ]));
  assert_equal
    "or-join clause"
    (OrJoin ([ "e" ], [ [ Pattern (QVar "e", QAttr "follows", QVar "x") ]; [ Pattern (QVar "e", QAttr "friend", QVar "y") ] ]))
    (Parser.parse_clause (list [ sym "or-join"; vec [ sym "?e" ]; vec [ sym "?e"; kw "follows"; sym "?x" ]; vec [ sym "?e"; kw "friend"; sym "?y" ] ]));
  assert_equal
    "source or-join clause"
    (SourceOrJoin ("1", [ "x" ], [ [ Pattern (QVar "e", QAttr "follows", QVar "x") ] ]))
    (Parser.parse_clause (list [ sym "$1"; sym "or-join"; vec [ sym "?x" ]; vec [ sym "?e"; kw "follows"; sym "?x" ] ]));
  assert_invalid "empty or-join vars" (fun () -> ignore (Parser.parse_clause (list [ sym "or-join"; vec []; vec [ sym "?y" ] ])));
  assert_invalid "empty inferred or vars" (fun () -> ignore (Parser.parse_clause (list [ sym "or"; vec [ sym "_" ] ])));
  assert_invalid "or-join missing branches" (fun () -> ignore (Parser.parse_clause (list [ sym "or-join"; vec [ sym "?x" ] ])));
  assert_invalid "or missing branches" (fun () -> ignore (Parser.parse_clause (list [ sym "or" ])))

let () =
  test_parser_where__pattern ();
  test_parser_where__test_pred ();
  test_parser_where__test_fn ();
  test_parser_where__rule_expr ();
  test_parser_where__not_clause ();
  test_parser_where__or_clause ()
