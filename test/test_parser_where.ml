open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal label expected actual =
  if expected <> actual then failf "%s" label

let assert_invalid label f =
  match f () with
  | exception Invalid_argument _ -> ()
  | exception exn -> failf "%s: unexpected exception %s" label (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

let assert_none label = function
  | None -> ()
  | Some _ -> failf "%s: expected None" label

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

let test_parser_where__clause_helper_batch () =
  assert_equal
    "parse_data_pattern_clause parses tx-op patterns"
    (PatternTxOp (QVar "e", QAttr "name", QVar "v", QVar "tx", QVar "op"))
    (Parser.parse_data_pattern_clause [ sym "?e"; kw "name"; sym "?v"; sym "?tx"; sym "?op" ]);
  assert_equal
    "parse_data_pattern_clause parses relation patterns"
    (SourceRelationPattern ("$", [ QVar "e"; QVar "v" ]))
    (Parser.parse_data_pattern_clause [ sym "?e"; sym "?v" ]);
  assert_equal
    "parse_rule_expr parses rule arguments"
    ("friend", [ QVar "e"; QValue (String "Ivan") ])
    (Parser.parse_rule_expr "friend" [ sym "?e"; str "Ivan" ]);
  assert_invalid "parse_rule_expr rejects missing args" (fun () ->
    ignore (Parser.parse_rule_expr "friend" []));
  assert_equal
    "parse_source_pattern_clause parses sourced rules"
    (SourceRule ("other", "friend", [ QVar "e" ]))
    (Parser.parse_source_pattern_clause "other" [ sym "friend"; sym "?e" ]);
  assert_equal
    "parse_source_pattern_clause parses sourced tx patterns"
    (SourcePatternTx ("other", QVar "e", QAttr "name", QVar "v", QVar "tx"))
    (Parser.parse_source_pattern_clause "other" [ sym "?e"; kw "name"; sym "?v"; sym "?tx" ]);
  assert_equal
    "parse_missing_clause parses default missing"
    (Missing (QVar "e", "name"))
    (Parser.parse_missing_clause [ sym "?e"; kw "name" ]);
  assert_equal
    "parse_missing_clause parses sourced missing"
    (SourceMissing ("other", QVar "e", "name"))
    (Parser.parse_missing_clause [ sym "$other"; sym "?e"; kw "name" ]);
  assert_equal
    "parse_get_else_clause parses default get-else"
    (GetElse (QVar "e", "name", String "unknown", "out"))
    (Parser.parse_get_else_clause [ sym "?e"; kw "name"; str "unknown" ] "?out");
  assert_equal
    "parse_get_else_clause parses sourced get-else"
    (SourceGetElse ("other", QVar "e", "name", String "unknown", "out"))
    (Parser.parse_get_else_clause [ sym "$other"; sym "?e"; kw "name"; str "unknown" ] "?out");
  assert_equal
    "parse_two_output_vars parses output tuples"
    ("attr", "value")
    (Parser.parse_two_output_vars (vec [ sym "?attr"; sym "?value" ]));
  assert_equal
    "parse_get_some_clause parses default get-some"
    (GetSome (QVar "e", [ "name"; "age" ], "attr", "value"))
    (Parser.parse_get_some_clause [ sym "?e"; kw "name"; kw "age" ] (vec [ sym "?attr"; sym "?value" ]));
  assert_equal
    "parse_get_some_clause parses sourced get-some"
    (SourceGetSome ("other", QVar "e", [ "name" ], "attr", "value"))
    (Parser.parse_get_some_clause [ sym "$other"; sym "?e"; kw "name" ] (vec [ sym "?attr"; sym "?value" ]));
  assert_equal
    "parse_get_clause parses get with default"
    (GetDefaultValue (QVar "m", QValue (Keyword "name"), QValue (String "unknown"), "out"))
    (Parser.parse_get_clause [ sym "?m"; kw "name"; str "unknown" ] "?out");
  assert_invalid "parse_get_clause rejects too few args" (fun () ->
    ignore (Parser.parse_get_clause [ sym "?m" ] "?out"))

let test_parser_where__value_function_helper_batch () =
  assert_equal
    "parse_core_value_function parses identity"
    (IdentityValue (QVar "x", "out"))
    (Parser.parse_core_value_function "identity" [ sym "?x" ] "?out");
  assert_equal
    "parse_core_value_function parses random values"
    (RandomValue "out")
    (Parser.parse_core_value_function "rand" [] "?out");
  assert_invalid "parse_core_value_function validates compare arity" (fun () ->
    ignore (Parser.parse_core_value_function "compare" [ sym "?x" ] "?out"));
  assert_equal
    "parse_collection_function parses vectors"
    (VectorValue ([ QVar "x"; QValue (Int 1) ], "out"))
    (Parser.parse_collection_function "vector" [ sym "?x"; int 1 ] "?out");
  assert_equal
    "parse_collection_function parses ranges"
    (RangeStepValue (QValue (Int 1), QValue (Int 5), QValue (Int 2), "out"))
    (Parser.parse_collection_function "range" [ int 1; int 5; int 2 ] "?out");
  assert_invalid "parse_collection_function validates hash-map arity" (fun () ->
    ignore (Parser.parse_collection_function "hash-map" [ kw "a" ] "?out"));
  assert_equal
    "parse_flat_value_function parses identity tuple"
    (GroundTermTuple (QVar "x", [ "out"; "_" ]))
    (Parser.parse_flat_value_function "identity" [ sym "?x" ] [ "out"; "_" ]);
  assert_equal
    "ground_values_of_form parses tuple ground values"
    [ String "a"; Int 1 ]
    (Parser.ground_values_of_form (vec [ str "a"; int 1 ]));
  assert_equal
    "ground_relation_rows_of_form parses relation ground values"
    [ [ String "a"; Int 1 ]; [ String "b"; Int 2 ] ]
    (Parser.ground_relation_rows_of_form (vec [ vec [ str "a"; int 1 ]; vec [ str "b"; int 2 ] ]));
  assert_equal
    "dynamic_ground_term accepts query vars"
    (Some (QVar "x"))
    (Parser.dynamic_ground_term (sym "?x"));
  assert_equal
    "parse_ground_function parses dynamic scalar ground"
    (GroundTerm (QVar "x", "out"))
    (Parser.parse_ground_function [ sym "?x" ] (sym "?out"));
  assert_equal
    "parse_ground_function parses static relation ground"
    (GroundRelation ([ [ String "a"; Int 1 ] ], [ "name"; "age" ]))
    (Parser.parse_ground_function
       [ vec [ vec [ str "a"; int 1 ] ] ]
       (vec [ vec [ sym "?name"; sym "?age" ]; sym "..." ]));
  assert_invalid "parse_ground_function validates arity" (fun () ->
    ignore (Parser.parse_ground_function [] (sym "?out")));
  assert_equal
    "parse_value_metadata_function parses keyword with namespace"
    (KeywordFromNamespaceName (QVar "ns", QVar "name", "out"))
    (Parser.parse_value_metadata_function "keyword" [ sym "?ns"; sym "?name" ] "?out");
  assert_equal
    "parse_value_metadata_function falls through to collection functions"
    (TupleFunction ([ QVar "x"; QVar "y" ], "out"))
    (Parser.parse_value_metadata_function "tuple" [ sym "?x"; sym "?y" ] "?out");
  assert_invalid "parse_value_metadata_function validates name arity" (fun () ->
    ignore (Parser.parse_value_metadata_function "name" [ sym "?x"; sym "?y" ] "?out"))

let test_parser_where__string_transform_helper_batch () =
  assert_equal
    "parse_string_transform_function parses lower-case"
    (StringLowerCaseValue (QVar "s", "out"))
    (Parser.parse_string_transform_function "clojure.string/lower-case" [ sym "?s" ] "?out");
  assert_equal
    "parse_string_transform_function parses join with separator"
    (StringJoinValue (QValue (String ","), QVar "xs", "out"))
    (Parser.parse_string_transform_function "clojure.string/join" [ str ","; sym "?xs" ] "?out");
  assert_equal
    "parse_string_transform_function parses replace"
    (StringReplaceValue (QVar "s", QValue (String "a"), QValue (String "b"), "out"))
    (Parser.parse_string_transform_function "clojure.string/replace" [ sym "?s"; str "a"; str "b" ] "?out");
  assert_equal
    "parse_string_transform_function parses split with limit"
    (StringSplitLimitValue (QVar "s", QValue (String ","), QValue (Int 2), "out"))
    (Parser.parse_string_transform_function "clojure.string/split" [ sym "?s"; str ","; int 2 ] "?out");
  assert_equal
    "parse_string_transform_function parses subs without end"
    (StringSubstringValue (QVar "s", QValue (Int 1), None, "out"))
    (Parser.parse_string_transform_function "subs" [ sym "?s"; int 1 ] "?out");
  assert_equal
    "parse_string_transform_function parses subs with end"
    (StringSubstringValue (QVar "s", QValue (Int 1), Some (QValue (Int 3)), "out"))
    (Parser.parse_string_transform_function "subs" [ sym "?s"; int 1; int 3 ] "?out");
  assert_equal
    "parse_string_transform_function falls through to metadata/value functions"
    (TypeValue (QVar "x", "out"))
    (Parser.parse_string_transform_function "type" [ sym "?x" ] "?out");
  assert_invalid "parse_string_transform_function validates replace arity" (fun () ->
    ignore (Parser.parse_string_transform_function "clojure.string/replace" [ sym "?s"; str "a" ] "?out"));
  assert_invalid "parse_string_transform_function validates split arity" (fun () ->
    ignore (Parser.parse_string_transform_function "clojure.string/split" [ sym "?s" ] "?out"))

let test_parser_where__string_predicate_symbol_helper_batch () =
  let unary symbol term =
    match Parser.unary_string_predicate_clause_of_symbol symbol with
    | Some make_clause -> make_clause term
    | None -> failf "expected unary predicate helper for %s" symbol
  in
  let binary symbol left right =
    match Parser.binary_string_predicate_clause_of_symbol symbol with
    | Some make_clause -> make_clause left right
    | None -> failf "expected binary predicate helper for %s" symbol
  in
  assert_equal
    "unary_string_predicate_clause_of_symbol maps blank?"
    (StringBlankValue (QVar "text"))
    (unary "clojure.string/blank?" (QVar "text"));
  assert_none
    "unary_string_predicate_clause_of_symbol ignores unknown symbols"
    (Parser.unary_string_predicate_clause_of_symbol "clojure.string/includes?");
  assert_equal
    "binary_string_predicate_clause_of_symbol maps includes?"
    (StringIncludesValue (QVar "text", QValue (String "needle")))
    (binary "clojure.string/includes?" (QVar "text") (QValue (String "needle")));
  assert_equal
    "binary_string_predicate_clause_of_symbol maps starts-with?"
    (StringStartsWithValue (QVar "text", QValue (String "pre")))
    (binary "clojure.string/starts-with?" (QVar "text") (QValue (String "pre")));
  assert_equal
    "binary_string_predicate_clause_of_symbol maps ends-with?"
    (StringEndsWithValue (QVar "text", QValue (String "post")))
    (binary "clojure.string/ends-with?" (QVar "text") (QValue (String "post")));
  assert_none
    "binary_string_predicate_clause_of_symbol ignores unknown symbols"
    (Parser.binary_string_predicate_clause_of_symbol "clojure.string/blank?")

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
  test_parser_where__clause_helper_batch ();
  test_parser_where__value_function_helper_batch ();
  test_parser_where__string_transform_helper_batch ();
  test_parser_where__string_predicate_symbol_helper_batch ();
  test_parser_where__not_clause ();
  test_parser_where__or_clause ()
