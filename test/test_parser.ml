open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal label expected actual =
  if expected <> actual then failf "%s" label

let assert_equal_query_form_option label expected actual =
  if expected <> actual then failf "%s" label

let assert_equal_string label expected actual =
  if expected <> actual then failf "%s: expected %S but got %S" label expected actual

let assert_invalid label f =
  match f () with
  | exception Invalid_argument _ -> ()
  | exception exn -> failf "%s: unexpected exception %s" label (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

let sym name = QueryFormSymbol name

let vec forms = QueryFormVector forms

let test_parser__bindings () =
  assert_equal "parse scalar binding" (Bind_scalar "x") (Parser.parse_binding (sym "?x"));
  assert_equal "parse ignore binding" Bind_ignore (Parser.parse_binding (sym "_"));
  assert_equal
    "parse collection binding"
    (Bind_collection (Bind_scalar "x"))
    (Parser.parse_binding (vec [ sym "?x"; sym "..." ]));
  assert_equal "parse tuple binding" (Bind_tuple [ Bind_scalar "x" ]) (Parser.parse_binding (vec [ sym "?x" ]));
  assert_equal
    "parse multi tuple binding"
    (Bind_tuple [ Bind_scalar "x"; Bind_scalar "y" ])
    (Parser.parse_binding (vec [ sym "?x"; sym "?y" ]));
  assert_equal
    "parse tuple ignore binding"
    (Bind_tuple [ Bind_ignore; Bind_scalar "y" ])
    (Parser.parse_binding (vec [ sym "_"; sym "?y" ]));
  assert_equal
    "parse nested collection binding"
    (Bind_collection
       (Bind_tuple
          [ Bind_ignore
          ; Bind_collection (Bind_scalar "x")
          ]))
    (Parser.parse_binding (vec [ vec [ sym "_"; vec [ sym "?x"; sym "..." ] ]; sym "..." ]));
  assert_equal
    "parse relation binding"
    (Bind_collection (Bind_tuple [ Bind_scalar "a"; Bind_scalar "b"; Bind_scalar "c" ]))
    (Parser.parse_binding (vec [ vec [ sym "?a"; sym "?b"; sym "?c" ] ]));
  assert_invalid "invalid binding" (fun () -> ignore (Parser.parse_binding (QueryFormKeyword "key")))

let test_parser__in () =
  assert_equal
    "parse scalar :in"
    [ Input_scalar_decl "x" ]
    (Parser.parse_in (vec [ sym "?x" ]));
  assert_equal
    "parse mixed :in"
    [ Input_source_decl "$"
    ; Input_source_decl "1"
    ; Input_rules_decl
    ; Input_ignore_decl
    ; Input_scalar_decl "x"
    ]
    (Parser.parse_in (vec [ sym "$"; sym "$1"; sym "%"; sym "_"; sym "?x" ]));
  assert_equal
    "parse nested :in"
    [ Input_source_decl "$"
    ; Input_nested_relation_decl
        [ Bind_ignore
        ; Bind_collection (Bind_scalar "x")
        ]
    ]
    (Parser.parse_in (vec [ sym "$"; vec [ vec [ sym "_"; vec [ sym "?x"; sym "..." ] ]; sym "..." ] ]));
  assert_invalid "invalid :in binding" (fun () -> ignore (Parser.parse_in (vec [ sym "?x"; QueryFormKeyword "key" ])))

let test_parser__with () =
  assert_equal "parse :with" [ "x"; "y" ] (Parser.parse_with (vec [ sym "?x"; sym "?y" ]));
  assert_invalid "reject :with placeholder" (fun () -> ignore (Parser.parse_with (vec [ sym "?x"; sym "_" ])))

let test_parser__query_form_helpers () =
  assert_equal
    "section_forms returns vector forms"
    [ sym "?e"; sym "?name" ]
    (Parser.section_forms (vec [ sym "?e"; sym "?name" ]));
  assert_equal "section_forms wraps scalar forms" [ sym "?e" ] (Parser.section_forms (sym "?e"));
  let entries =
    Parser.query_form_sections
      [ QueryFormKeyword "find"
      ; sym "?e"
      ; QueryFormKeyword "where"
      ; vec [ sym "?e"; QueryFormKeyword "name"; sym "?name" ]
      ; QueryFormKeyword "where"
      ; vec [ sym "?e"; QueryFormKeyword "age"; sym "?age" ]
      ]
  in
  assert_equal_query_form_option
    "query_form_section concatenates repeated vector sections"
    (Some
       (vec
          [ vec [ sym "?e"; QueryFormKeyword "name"; sym "?name" ]
          ; vec [ sym "?e"; QueryFormKeyword "age"; sym "?age" ]
          ]))
    (Parser.query_form_section "where" entries);
  assert_equal
    "query_form_map converts query vectors to section maps"
    entries
    (Parser.query_form_map
       (vec
          [ QueryFormKeyword "find"
          ; sym "?e"
          ; QueryFormKeyword "where"
          ; vec [ sym "?e"; QueryFormKeyword "name"; sym "?name" ]
          ; QueryFormKeyword "where"
          ; vec [ sym "?e"; QueryFormKeyword "age"; sym "?age" ]
          ]));
  assert_equal_query_form_option
    "query_form_sequence returns list forms"
    (Some [ sym "?e" ])
    (Parser.query_form_sequence (QueryFormList [ sym "?e" ]));
  assert_invalid "query_form_sections rejects forms before first keyword" (fun () ->
    ignore (Parser.query_form_sections [ sym "?e"; QueryFormKeyword "find" ]))

let test_parser__query_symbol_helpers () =
  assert_equal_string "query_symbol_name strips ?" "name" (Parser.query_symbol_name "?name");
  assert_invalid "query_symbol_name rejects non-vars" (fun () -> ignore (Parser.query_symbol_name "name"));
  assert_equal_string "query_callable_name keeps plain callable names" "missing?" (Parser.query_callable_name "missing?");
  assert_equal_string "query_callable_name strips var callables" "pred" (Parser.query_callable_name "?pred");
  if not (Parser.is_plain_input_symbol "name") then failwith "plain input symbol should be accepted";
  if Parser.is_plain_input_symbol "?name" then failwith "query vars are not plain input symbols";
  if not (Parser.is_query_input_symbol "?name") then failwith "query input vars should be accepted";
  assert_equal_string "query_input_name strips ?" "name" (Parser.query_input_name "?name");
  assert_equal_string "query_input_name accepts plain names" "name" (Parser.query_input_name "name");
  assert_equal_string "query_source_name accepts default source" "$" (Parser.query_source_name "$");
  assert_equal_string "query_source_name strips $" "other" (Parser.query_source_name "$other");
  if not (Parser.is_query_source_symbol "$other") then failwith "source symbols should be detected";
  if Parser.is_query_source_symbol "?other" then failwith "query vars are not source symbols";
  if not (Parser.is_plain_rule_symbol "ancestor") then failwith "plain rule symbols should be accepted";
  if Parser.is_plain_rule_symbol "$source" then failwith "source vars are not plain rule symbols"

let test_parser__aggregate_and_find_arg_helpers () =
  assert_equal "aggregate_of_symbol parses sum" (Some Sum) (Parser.aggregate_of_symbol "sum");
  assert_equal "aggregate_of_symbol rejects unknown symbols" None (Parser.aggregate_of_symbol "unknown");
  assert_equal
    "amount_aggregate_of_symbol parses min amount"
    (Some (MinN 2))
    (Parser.amount_aggregate_of_symbol "min" 2);
  assert_invalid "amount_aggregate_of_symbol rejects negative amounts" (fun () ->
    ignore (Parser.amount_aggregate_of_symbol "sample" (-1)));
  assert_equal
    "dynamic_amount_aggregate_of_symbol parses amount vars"
    (Some (SampleVar "n"))
    (Parser.dynamic_amount_aggregate_of_symbol "sample" "n");
  assert_equal
    "parse_find_arg parses source args"
    (QSource "other")
    (Parser.parse_find_arg (sym "$other"));
  assert_equal
    "parse_find_arg parses query vars"
    (QVar "name")
    (Parser.parse_find_arg (sym "?name"));
  assert_equal
    "parse_find_arg parses constants"
    (QValue (Keyword "status"))
    (Parser.parse_find_arg (QueryFormKeyword "status"));
  assert_equal
    "parse_find_args preserves arg order"
    [ QVar "name"; QSource "other"; QValue (Int 1) ]
    (Parser.parse_find_args [ sym "?name"; sym "$other"; QueryFormInt 1 ])

let () =
  test_parser__bindings ();
  test_parser__in ();
  test_parser__with ();
  test_parser__query_form_helpers ();
  test_parser__query_symbol_helpers ();
  test_parser__aggregate_and_find_arg_helpers ()
