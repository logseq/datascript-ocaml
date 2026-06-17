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
let vec forms = QueryFormVector forms

let rule name params body = { rule_name = name; rule_params = params; rule_body = body }
let wildcard_relation = SourceRelationPattern ("$", [ QWildcard ])
let keyword_relation name = SourceRelationPattern ("$", [ QValue (Keyword name) ])

let test_parser_rules__clauses () =
  assert_equal
    "parse simple rule"
    [ rule "rule" [ "x" ] [ Pattern (QVar "x", QAttr "name", QWildcard) ] ]
    (Parser.parse_rules (vec [ vec [ vec [ sym "rule"; sym "?x" ]; vec [ sym "?x"; kw "name"; sym "_" ] ] ]))

let test_parser_rules__rule_vars () =
  assert_equal
    "parse required and free rule vars"
    [ rule "rule" [ "x"; "y" ] [ wildcard_relation ] ]
    (Parser.parse_rules (vec [ vec [ vec [ sym "rule"; vec [ sym "?x" ]; sym "?y" ]; vec [ sym "_" ] ] ]));
  assert_equal
    "parse multiple required vars"
    [ rule "rule" [ "x"; "y"; "a"; "b" ] [ wildcard_relation ] ]
    (Parser.parse_rules (vec [ vec [ vec [ sym "rule"; vec [ sym "?x"; sym "?y" ]; sym "?a"; sym "?b" ]; vec [ sym "_" ] ] ]));
  assert_equal
    "parse required-only vars"
    [ rule "rule" [ "x" ] [ wildcard_relation ] ]
    (Parser.parse_rules (vec [ vec [ vec [ sym "rule"; vec [ sym "?x" ] ]; vec [ sym "_" ] ] ]));
  assert_invalid "reject missing vars" (fun () -> ignore (Parser.parse_rules (vec [ vec [ vec [ sym "rule" ]; vec [ sym "_" ] ] ])));
  assert_invalid "reject empty required vars" (fun () -> ignore (Parser.parse_rules (vec [ vec [ vec [ sym "rule"; vec [] ]; vec [ sym "_" ] ] ])));
  assert_invalid "reject duplicate free vars" (fun () -> ignore (Parser.parse_rules (vec [ vec [ vec [ sym "rule"; sym "?x"; sym "?y"; sym "?x" ]; vec [ sym "_" ] ] ])));
  assert_invalid "reject duplicate required/free vars" (fun () -> ignore (Parser.parse_rules (vec [ vec [ vec [ sym "rule"; vec [ sym "?x"; sym "?y" ]; sym "?z"; sym "?x" ]; vec [ sym "_" ] ] ])))

let test_parser_rules__branches () =
  assert_equal
    "parse multiple branches with same rule name"
    [ rule "rule" [ "x" ] [ keyword_relation "a"; keyword_relation "b" ]
    ; rule "rule" [ "x" ] [ keyword_relation "c" ]
    ]
    (Parser.parse_rules
       (vec
          [ vec [ vec [ sym "rule"; sym "?x" ]; vec [ kw "a" ]; vec [ kw "b" ] ]
          ; vec [ vec [ sym "rule"; sym "?x" ]; vec [ kw "c" ] ]
          ]));
  assert_equal
    "parse different rule names"
    [ rule "rule" [ "x" ] [ keyword_relation "a"; keyword_relation "b" ]
    ; rule "other" [ "x" ] [ keyword_relation "c" ]
    ]
    (Parser.parse_rules
       (vec
          [ vec [ vec [ sym "rule"; sym "?x" ]; vec [ kw "a" ]; vec [ kw "b" ] ]
          ; vec [ vec [ sym "other"; sym "?x" ]; vec [ kw "c" ] ]
          ]));
  assert_invalid "reject branch without clauses" (fun () -> ignore (Parser.parse_rules (vec [ vec [ vec [ sym "rule"; sym "?x" ] ] ])));
  assert_invalid
    "reject arity mismatch"
    (fun () -> ignore (Parser.parse_rules (vec [ vec [ vec [ sym "rule"; sym "?x" ]; vec [ sym "_" ] ]; vec [ vec [ sym "rule"; sym "?x"; sym "?y" ]; vec [ sym "_" ] ] ])));
  assert_invalid
    "reject required/free arity mismatch"
    (fun () -> ignore (Parser.parse_rules (vec [ vec [ vec [ sym "rule"; sym "?x" ]; vec [ sym "_" ] ]; vec [ vec [ sym "rule"; vec [ sym "?x" ]; vec [ sym "_" ] ] ] ])))

let () =
  test_parser_rules__clauses ();
  test_parser_rules__rule_vars ();
  test_parser_rules__branches ()
