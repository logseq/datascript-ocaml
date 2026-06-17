open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let many =
  { cardinality = Many
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let ref_attr = { many with cardinality = One; value_type = Some RefType }
let ref_many = { many with value_type = Some RefType }
let component = { ref_attr with is_component = true }
let multicomponent = { ref_many with is_component = true }

let db =
  empty_db
    ~schema:
      [ "ref", ref_attr
      ; "ref2", ref_attr
      ; "ref3", ref_attr
      ; "ns/ref", ref_attr
      ; "multival", many
      ; "multiref", ref_many
      ; "component", component
      ; "multicomponent", multicomponent
      ]
    ()

let sym name = QueryFormSymbol name
let kw name = QueryFormKeyword name
let str value = QueryFormString value
let int value = QueryFormInt value
let vec forms = QueryFormVector forms
let list forms = QueryFormList forms
let map entries = QueryFormMap entries

let assert_equal label expected actual =
  if expected <> actual then failf "%s" label

let assert_invalid label form =
  match Pull_parser.parse_pattern db form with
  | exception Invalid_argument _ -> ()
  | exception exn -> failf "%s: unexpected %s" label (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

let test_pull_parser__test_parse_pattern () =
  assert_equal "normal attr" [ Pull_attr "normal" ] (Pull_parser.parse_pattern db (vec [ kw "normal" ]));
  assert_equal "list attr" [ Pull_attr "normal" ] (Pull_parser.parse_pattern db (vec [ list [ kw "normal" ] ]));
  assert_equal "vector attr" [ Pull_attr "normal" ] (Pull_parser.parse_pattern db (vec [ vec [ kw "normal" ] ]));
  assert_equal "db id" [ Pull_attr "db/id" ] (Pull_parser.parse_pattern db (vec [ kw "db/id" ]));
  assert_equal "wildcard" [ Pull_wildcard ] (Pull_parser.parse_pattern db (vec [ sym "*" ]));
  assert_equal "string wildcard" [ Pull_wildcard ] (Pull_parser.parse_pattern db (vec [ str "*" ]));
  assert_equal "ref attr" [ Pull_attr "ref" ] (Pull_parser.parse_pattern db (vec [ kw "ref" ]));
  assert_equal "reverse ref" [ Pull_attr "_ref" ] (Pull_parser.parse_pattern db (vec [ kw "_ref" ]));
  assert_equal "namespaced reverse ref" [ Pull_attr "ns/_ref" ] (Pull_parser.parse_pattern db (vec [ kw "ns/_ref" ]));
  assert_equal
    "alias attr"
    [ Pull_as (Pull_attr "normal", Keyword "normal2") ]
    (Pull_parser.parse_pattern db (vec [ list [ kw "normal"; kw "as"; kw "normal2" ] ]));
  assert_equal
    "limit attr"
    [ Pull_attr_limit ("multival", 100) ]
    (Pull_parser.parse_pattern db (vec [ list [ kw "multival"; kw "limit"; int 100 ] ]));
  assert_equal
    "legacy limit attr"
    [ Pull_attr_limit ("multival", 100) ]
    (Pull_parser.parse_pattern db (vec [ list [ sym "limit"; kw "multival"; int 100 ] ]));
  assert_equal
    "unlimited limit attr"
    [ Pull_attr_unlimited "multival" ]
    (Pull_parser.parse_pattern db (vec [ list [ sym "limit"; kw "multival"; QueryFormNil ] ]));
  assert_equal
    "default attr"
    [ Pull_attr_default ("multival", Keyword "xyz") ]
    (Pull_parser.parse_pattern db (vec [ list [ sym "default"; kw "multival"; kw "xyz" ] ]));
  assert_equal
    "map spec"
    [ Pull_ref ("ref", [ Pull_attr "normal" ]) ]
    (Pull_parser.parse_pattern db (vec [ map [ kw "ref", vec [ kw "normal" ] ] ]));
  assert_equal
    "multi map specs"
    [ Pull_ref ("ref", [ Pull_attr "normal" ]); Pull_ref ("ref2", [ Pull_attr "normal2" ]) ]
    (Pull_parser.parse_pattern db (vec [ map [ kw "ref", vec [ kw "normal" ]; kw "ref2", vec [ kw "normal2" ] ] ]));
  assert_equal
    "recursive map spec"
    [ Pull_recursive_ref ("ref", [], None) ]
    (Pull_parser.parse_pattern db (vec [ map [ kw "ref", sym "..." ] ]));
  assert_equal
    "component depth map spec"
    [ Pull_recursive_ref ("component", [], Some 1) ]
    (Pull_parser.parse_pattern db (vec [ map [ kw "component", int 1 ] ]));
  assert_invalid "reverse non-ref" (vec [ kw "_normal" ]);
  assert_invalid "odd attr opts" (vec [ list [ kw "multival"; kw "limit" ] ]);
  assert_invalid "bad limit arity" (vec [ list [ sym "limit"; kw "multival" ] ]);
  assert_invalid "limit on cardinality one" (vec [ list [ sym "limit"; kw "normal"; int 100 ] ]);
  assert_invalid "bad limit value" (vec [ list [ kw "multival"; kw "limit"; kw "abc" ] ]);
  assert_invalid "bad default arity" (vec [ list [ sym "default"; kw "normal" ] ]);
  assert_invalid "map spec non-ref attr" (vec [ map [ kw "normal", vec [ kw "normal2" ] ] ]);
  assert_invalid "map spec non-sequential pattern" (vec [ map [ kw "ref", kw "normal" ] ])

let () = test_pull_parser__test_parse_pattern ()
