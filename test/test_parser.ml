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

let () =
  test_parser__bindings ();
  test_parser__in ();
  test_parser__with ()
