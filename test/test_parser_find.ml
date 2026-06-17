open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal label expected actual =
  if expected <> actual then failf "%s" label

let sym name = QueryFormSymbol name
let int value = QueryFormInt value
let vec forms = QueryFormVector forms
let list forms = QueryFormList forms

let test_parser_find__test_parse_find () =
  assert_equal "find relation" (Return_relation, [ Find_var "a"; Find_var "b" ]) (Parser.parse_find (vec [ sym "?a"; sym "?b" ]));
  assert_equal "find collection" (Return_collection, [ Find_var "a" ]) (Parser.parse_find (vec [ vec [ sym "?a"; sym "..." ] ]));
  assert_equal "find scalar" (Return_scalar, [ Find_var "a" ]) (Parser.parse_find (vec [ sym "?a"; sym "." ]));
  assert_equal "find tuple" (Return_tuple, [ Find_var "a"; Find_var "b" ]) (Parser.parse_find (vec [ vec [ sym "?a"; sym "?b" ] ]))

let test_parser_find__test_parse_aggregate () =
  assert_equal
    "aggregate relation"
    (Return_relation, [ Find_var "a"; Find_aggregate (Count, [ QVar "b" ]) ])
    (Parser.parse_find (vec [ sym "?a"; list [ sym "count"; sym "?b" ] ]));
  assert_equal
    "aggregate collection"
    (Return_collection, [ Find_aggregate (Count, [ QVar "a" ]) ])
    (Parser.parse_find (vec [ vec [ list [ sym "count"; sym "?a" ]; sym "..." ] ]));
  assert_equal
    "aggregate scalar"
    (Return_scalar, [ Find_aggregate (Count, [ QVar "a" ]) ])
    (Parser.parse_find (vec [ list [ sym "count"; sym "?a" ]; sym "." ]));
  assert_equal
    "aggregate tuple"
    (Return_tuple, [ Find_aggregate (Count, [ QVar "a" ]); Find_var "b" ])
    (Parser.parse_find (vec [ vec [ list [ sym "count"; sym "?a" ]; sym "?b" ] ]))

let test_parser_find__test_parse_custom_aggregates () =
  assert_equal
    "custom aggregate relation"
    (Return_relation, [ Find_aggregate (CustomVar "f", [ QVar "a" ]) ])
    (Parser.parse_find (vec [ list [ sym "aggregate"; sym "?f"; sym "?a" ] ]));
  assert_equal
    "custom aggregate mixed relation"
    (Return_relation, [ Find_var "a"; Find_aggregate (CustomVar "f", [ QVar "b" ]) ])
    (Parser.parse_find (vec [ sym "?a"; list [ sym "aggregate"; sym "?f"; sym "?b" ] ]));
  assert_equal
    "custom aggregate collection"
    (Return_collection, [ Find_aggregate (CustomVar "f", [ QVar "a" ]) ])
    (Parser.parse_find (vec [ vec [ list [ sym "aggregate"; sym "?f"; sym "?a" ]; sym "..." ] ]));
  assert_equal
    "custom aggregate scalar"
    (Return_scalar, [ Find_aggregate (CustomVar "f", [ QVar "a" ]) ])
    (Parser.parse_find (vec [ list [ sym "aggregate"; sym "?f"; sym "?a" ]; sym "." ]));
  assert_equal
    "custom aggregate tuple"
    (Return_tuple, [ Find_aggregate (CustomVar "f", [ QVar "a" ]); Find_var "b" ])
    (Parser.parse_find (vec [ vec [ list [ sym "aggregate"; sym "?f"; sym "?a" ]; sym "?b" ] ]))

let test_parser_find__test_parse_find_elements () =
  assert_equal
    "aggregate supports constants and source vars"
    (Return_scalar, [ Find_aggregate (Count, [ QVar "b"; QValue (Int 1); QSource "x" ]) ])
    (Parser.parse_find (vec [ list [ sym "count"; sym "?b"; int 1; sym "$x" ]; sym "." ]))

let () =
  test_parser_find__test_parse_find ();
  test_parser_find__test_parse_aggregate ();
  test_parser_find__test_parse_custom_aggregates ();
  test_parser_find__test_parse_find_elements ()
