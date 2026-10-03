open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let kw name = Keyword name

let many =
  Schema.spec ~cardinality:(Many) ?unique:(None) ~indexed:(false) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let unique_identity =
  Schema.spec ~cardinality:(One) ?unique:(Some Identity) ~indexed:(true) ~is_component:((Schema.is_component many)) ~no_history:((Schema.no_history many)) ?doc:((Schema.doc many)) ?value_type:((Schema.value_type many)) ?tuple:(match ((Schema.tuple_attrs many), (Schema.tuple_types many)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let ref_attr =
  Schema.spec ~cardinality:(One) ?unique:((Schema.unique many)) ~indexed:((Schema.indexed many)) ~is_component:((Schema.is_component many)) ~no_history:((Schema.no_history many)) ?doc:((Schema.doc many)) ?value_type:(Some RefType) ?tuple:(match ((Schema.tuple_attrs many), (Schema.tuple_types many)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let ref_many =
  Schema.spec ~cardinality:((Schema.cardinality many)) ?unique:((Schema.unique many)) ~indexed:((Schema.indexed many)) ~is_component:((Schema.is_component many)) ~no_history:((Schema.no_history many)) ?doc:((Schema.doc many)) ?value_type:(Some RefType) ?tuple:(match ((Schema.tuple_attrs many), (Schema.tuple_types many)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let component_many =
  Schema.spec ~cardinality:((Schema.cardinality ref_many)) ?unique:((Schema.unique ref_many)) ~indexed:((Schema.indexed ref_many)) ~is_component:(true) ~no_history:((Schema.no_history ref_many)) ?doc:((Schema.doc ref_many)) ?value_type:((Schema.value_type ref_many)) ?tuple:(match ((Schema.tuple_attrs ref_many), (Schema.tuple_types ref_many)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let component_one =
  Schema.spec ~cardinality:((Schema.cardinality ref_attr)) ?unique:((Schema.unique ref_attr)) ~indexed:((Schema.indexed ref_attr)) ~is_component:(true) ~no_history:((Schema.no_history ref_attr)) ?doc:((Schema.doc ref_attr)) ?value_type:((Schema.value_type ref_attr)) ?tuple:(match ((Schema.tuple_attrs ref_attr), (Schema.tuple_types ref_attr)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let schema =
  [ "name", unique_identity
  ; "aka", many
  ; "child", ref_many
  ; "friend", ref_many
  ; "enemy", ref_many
  ; "father", ref_attr
  ; "part", component_many
  ; "spec", component_one
  ]

let test_db () =
  empty_db ~schema ()
  |> db_with
       [ Entity
           { db_id = Some (Entity_id (eid 1L))
           ; attrs =
               [ "name", One_value (String "Petr")
               ; "aka", Many_values [ String "Devil"; String "Tupen" ]
               ; "child", Many_values [ Ref (eid 2L); Ref (eid 3L) ]
               ]
           }
       ; Entity { db_id = Some (Entity_id (eid 2L)); attrs = [ "name", One_value (String "David"); "father", One_value (Ref (eid 1L)) ] }
       ; Entity { db_id = Some (Entity_id (eid 3L)); attrs = [ "name", One_value (String "Thomas"); "father", One_value (Ref (eid 1L)) ] }
       ; Entity { db_id = Some (Entity_id (eid 4L)); attrs = [ "name", One_value (String "Lucy") ] }
       ; Entity { db_id = Some (Entity_id (eid 5L)); attrs = [ "name", One_value (String "Elizabeth") ] }
       ; Entity { db_id = Some (Entity_id (eid 6L)); attrs = [ "name", One_value (String "Matthew"); "father", One_value (Ref (eid 3L)) ] }
       ; Entity { db_id = Some (Entity_id (eid 7L)); attrs = [ "name", One_value (String "Eunan") ] }
       ; Entity { db_id = Some (Entity_id (eid 8L)); attrs = [ "name", One_value (String "Kerri") ] }
       ; Entity { db_id = Some (Entity_id (eid 9L)); attrs = [ "name", One_value (String "Rebecca") ] }
       ; Entity { db_id = Some (Entity_id (eid 10L)); attrs = [ "name", One_value (String "Part A"); "part", Many_values [ Ref (eid 11L); Ref (eid 15L) ] ] }
       ; Entity { db_id = Some (Entity_id (eid 11L)); attrs = [ "name", One_value (String "Part A.A"); "part", Many_values [ Ref (eid 12L) ] ] }
       ; Entity { db_id = Some (Entity_id (eid 12L)); attrs = [ "name", One_value (String "Part A.A.A"); "part", Many_values [ Ref (eid 13L); Ref (eid 14L) ] ] }
       ; Entity { db_id = Some (Entity_id (eid 13L)); attrs = [ "name", One_value (String "Part A.A.A.A") ] }
       ; Entity { db_id = Some (Entity_id (eid 14L)); attrs = [ "name", One_value (String "Part A.A.A.B") ] }
       ; Entity { db_id = Some (Entity_id (eid 15L)); attrs = [ "name", One_value (String "Part A.B"); "part", Many_values [ Ref (eid 16L) ] ] }
       ; Entity { db_id = Some (Entity_id (eid 16L)); attrs = [ "name", One_value (String "Part A.B.A"); "part", Many_values [ Ref (eid 17L); Ref (eid 18L) ] ] }
       ; Entity { db_id = Some (Entity_id (eid 17L)); attrs = [ "name", One_value (String "Part A.B.A.A") ] }
       ; Entity { db_id = Some (Entity_id (eid 18L)); attrs = [ "name", One_value (String "Part A.B.A.B") ] }
       ]

let rec string_of_value = function
  | Nil -> "nil"
  | Int64 value -> Int64.to_string value
  | Float value -> string_of_float value
  | String value -> Printf.sprintf "%S" value
  | Symbol value -> value
  | Bool value -> string_of_bool value
  | Keyword value -> ":" ^ value
  | Uuid value -> "#uuid " ^ value
  | Instant value -> "#inst " ^ Int64.to_string value
  | Regex value -> "#\"" ^ value ^ "\""
  | Ref value -> "Ref " ^ Int64.to_string (Entity_id.to_int64 value)
  | List values -> "[" ^ String.concat " " (List.map string_of_value values) ^ "]"
  | Vector values -> "#vector[" ^ String.concat " " (List.map string_of_value values) ^ "]"
  | Map entries ->
    "{"
    ^ (entries |> List.map (fun (key, value) -> string_of_value key ^ " " ^ string_of_value value) |> String.concat " ")
    ^ "}"
  | Set values -> "#{" ^ String.concat " " (List.map string_of_value values) ^ "}"
  | Tuple values ->
    "[" ^ String.concat " " (List.map (function None -> "nil" | Some value -> string_of_value value) values) ^ "]"
  | TxRef -> ":db/current-tx"
  | Ref_to _ -> "#ref"

let rec string_of_pulled_value = function
  | Pulled_scalar value -> string_of_value value
  | Pulled_many values -> "[" ^ String.concat " " (List.map string_of_pulled_value values) ^ "]"
  | Pulled_entity entity -> string_of_pulled_entity entity

and string_of_pulled_entity entity =
  "{"
  ^ (entity.pulled_attrs
     |> List.map (fun (key, value) -> string_of_value key ^ " " ^ string_of_pulled_value value)
     |> String.concat ", ")
  ^ "}"

let string_of_attrs attrs =
  attrs
  |> List.map (fun (key, value) -> string_of_value key ^ " " ^ string_of_pulled_value value)
  |> String.concat "; "

let rec normalize_pulled_value = function
  | Pulled_scalar value -> Pulled_scalar value
  | Pulled_many values -> Pulled_many (List.map normalize_pulled_value values)
  | Pulled_entity entity ->
    Pulled_entity
      { entity with
        pulled_attrs =
          entity.pulled_attrs
          |> List.map (fun (key, value) -> key, normalize_pulled_value value)
          |> List.sort (fun (left, _) (right, _) -> compare left right)
      }

let normalize_attrs attrs =
  attrs
  |> List.map (fun (key, value) -> key, normalize_pulled_value value)
  |> List.sort (fun (left, _) (right, _) -> compare left right)

let expect_pull label db pattern entity_ref expected_attrs =
  match pull db pattern entity_ref with
  | None -> failf "%s: expected entity" label
  | Some entity ->
    let expected_attrs = normalize_attrs expected_attrs in
    let pulled_attrs = normalize_attrs entity.pulled_attrs in
    if pulled_attrs <> expected_attrs then
      failf
        "%s: expected [%s], got [%s]"
        label
        (string_of_attrs expected_attrs)
        (string_of_attrs pulled_attrs)

let scalar value = Pulled_scalar value
let entity id attrs = Pulled_entity { pulled_id = id; pulled_attrs = attrs }
let many_values values = Pulled_many values

let test_pull_api__test_pull_attr_spec () =
  let db = test_db () in
  expect_pull
    "attr spec"
    db
    [ Pull_attr "name"; Pull_attr "aka" ]
    (Entity_id (eid 1L))
    [ kw "aka", many_values [ scalar (String "Devil"); scalar (String "Tupen") ]
    ; kw "name", scalar (String "Petr")
    ];
  let pulled = pull_many db [ Pull_attr "name" ] [ Entity_id (eid 1L); Entity_id (eid 5L); Entity_id (eid 7L); Entity_id (eid 9L) ] in
  if List.length pulled <> 4 || List.exists Option.is_none pulled then failf "pull-many should preserve requested entities"

let test_pull_api__test_pull_reverse_attr_spec () =
  let db = test_db () in
  expect_pull
    "reverse attr spec"
    db
    [ Pull_attr "name"; Pull_reverse_ref ("child", [ Pull_id ]) ]
    (Entity_id (eid 2L))
    [ kw "child", many_values [ entity (eid 1L) [ kw "db/id", scalar (Int64 1L) ] ]
    ; kw "name", scalar (String "David")
    ];
  expect_pull
    "reverse ref map spec"
    db
    [ Pull_attr "name"; Pull_reverse_ref ("father", [ Pull_attr "name" ]) ]
    (Entity_id (eid 3L))
    [ kw "father", many_values [ entity (eid 6L) [ kw "name", scalar (String "Matthew") ] ]
    ; kw "name", scalar (String "Thomas")
    ]

let test_pull_api__test_pull_component_attr () =
  let db = test_db () in
  expect_pull
    "component attr recursively expands"
    db
    [ Pull_attr "name"; Pull_ref ("part", [ Pull_attr "name" ]) ]
    (Entity_id (eid 10L))
    [ kw "name", scalar (String "Part A")
    ; kw "part", many_values
        [ entity (eid 11L) [ kw "name", scalar (String "Part A.A") ]
        ; entity (eid 15L) [ kw "name", scalar (String "Part A.B") ]
        ]
    ];
  expect_pull
    "reverse component returns single entity"
    db
    [ Pull_attr "name"; Pull_reverse_ref ("part", [ Pull_attr "name" ]) ]
    (Entity_id (eid 11L))
    [ kw "part", entity (eid 10L) [ kw "name", scalar (String "Part A") ]
    ; kw "name", scalar (String "Part A.A")
    ]

let test_pull_api__test_pull_wildcard () =
  let db = test_db () in
  match pull db [ Pull_wildcard ] (Entity_id (eid 1L)) with
  | Some entity when List.assoc_opt (kw "db/id") entity.pulled_attrs = Some (scalar (Int64 1L))
                 && List.assoc_opt (kw "name") entity.pulled_attrs = Some (scalar (String "Petr")) -> ()
  | _ -> failf "wildcard pull should include db/id and attrs"

let test_pull_api__test_pull_limit () =
  let db =
    test_db ()
    |> db_with
         (List.init 2000 (fun index -> Add (Entity_id (eid 8L), "aka", String ("aka-" ^ string_of_int index))))
  in
  expect_pull
    "explicit limit"
    db
    [ Pull_attr_limit ("aka", 2) ]
    (Entity_id (eid 8L))
    [ kw "aka", many_values [ scalar (String "aka-0"); scalar (String "aka-1") ] ];
  match pull db [ Pull_attr_unlimited "aka" ] (Entity_id (eid 8L)) with
  | Some entity ->
    (match List.assoc_opt (kw "aka") entity.pulled_attrs with
     | Some (Pulled_many values) when List.length values = 2000 -> ()
     | _ -> failf "unlimited limit should return all values")
  | None -> failf "expected unlimited pull"

let test_pull_api__test_pull_default () =
  let db = test_db () in
  if pull db [ Pull_attr "missing" ] (Entity_id (eid 1L)) <> None then failf "missing attr should drop empty pull";
  expect_pull
    "default attr"
    db
    [ Pull_attr_default ("missing", String "fallback") ]
    (Entity_id (eid 1L))
    [ kw "missing", scalar (String "fallback") ];
  expect_pull
    "default does not override result"
    db
    [ Pull_attr_default ("name", String "fallback") ]
    (Entity_id (eid 1L))
    [ kw "name", scalar (String "Petr") ]

let test_pull_api__test_pull_as () =
  expect_pull
    "pull as"
    (test_db ())
    [ Pull_as (Pull_attr "name", String "Name"); Pull_as (Pull_attr "aka", kw "alias") ]
    (Entity_id (eid 1L))
    [ kw "alias", many_values [ scalar (String "Devil"); scalar (String "Tupen") ]
    ; String "Name", scalar (String "Petr")
    ]

let test_pull_api__test_pull_attr_with_opts () =
  expect_pull
    "attr with as and default"
    (test_db ())
    [ Pull_as (Pull_attr_default ("x", String "Nothing"), String "Name") ]
    (Entity_id (eid 1L))
    [ String "Name", scalar (String "Nothing") ]

let test_pull_api__test_pull_map () =
  let db = test_db () in
  expect_pull
    "single ref map"
    db
    [ Pull_attr "name"; Pull_ref ("father", [ Pull_attr "name" ]) ]
    (Entity_id (eid 6L))
    [ kw "father", entity (eid 3L) [ kw "name", scalar (String "Thomas") ]
    ; kw "name", scalar (String "Matthew")
    ];
  expect_pull
    "multi ref map"
    db
    [ Pull_attr "name"; Pull_ref ("child", [ Pull_attr "name" ]) ]
    (Entity_id (eid 1L))
    [ kw "child", many_values [ entity (eid 2L) [ kw "name", scalar (String "David") ]; entity (eid 3L) [ kw "name", scalar (String "Thomas") ] ]
    ; kw "name", scalar (String "Petr")
    ]

let test_pull_api__test_pull_ref_preserves_duplicate_many_datoms () =
  let db =
    init_db
      ~schema:[ "db/ident", unique_identity; "block/title", many; "block/tags", ref_many ]
      [ datom ~tx:(txid 1L) (eid 2L) "db/ident" (Keyword "logseq.class/Tag")
      ; datom ~tx:(txid 1L) (eid 10L) "block/title" (String "Template")
      ; datom ~tx:(txid 1L) (eid 10L) "block/tags" (Ref (eid 2L))
      ; datom ~tx:(txid 1L) (eid 10L) "block/tags" (Ref (eid 2L))
      ]
  in
  expect_pull
    "pull ref preserves duplicate many datoms"
    db
    [ Pull_attr "block/title"; Pull_ref ("block/tags", [ Pull_attr "db/ident" ]) ]
    (Entity_id (eid 10L))
    [ kw "block/tags", many_values
        [ entity (eid 2L) [ kw "db/ident", scalar (Keyword "logseq.class/Tag") ]
        ; entity (eid 2L) [ kw "db/ident", scalar (Keyword "logseq.class/Tag") ]
        ]
    ; kw "block/title", many_values [ scalar (String "Template") ]
    ]

let test_pull_api__test_pull_recursion () =
  let db =
    test_db ()
    |> db_with
         [ Add (Entity_id (eid 4L), "friend", Ref (eid 5L))
         ; Add (Entity_id (eid 5L), "friend", Ref (eid 6L))
         ; Add (Entity_id (eid 6L), "friend", Ref (eid 7L))
         ; Add (Entity_id (eid 7L), "friend", Ref (eid 8L))
         ]
  in
  match pull db [ Pull_id; Pull_attr "name"; Pull_recursive_ref ("friend", [ Pull_id; Pull_attr "name" ], None) ] (Entity_id (eid 4L)) with
  | Some entity when List.assoc_opt (kw "friend") entity.pulled_attrs <> None -> ()
  | _ -> failf "recursive pull should expand friends"

let test_pull_api__test_dual_recursion () =
  let db =
    empty_db ~schema:[ "friend", ref_attr; "enemy", ref_attr ] ()
    |> db_with
         [ Add (Entity_id (eid 1L), "friend", Ref (eid 2L))
         ; Add (Entity_id (eid 2L), "enemy", Ref (eid 3L))
         ; Add (Entity_id (eid 3L), "friend", Ref (eid 4L))
         ; Add (Entity_id (eid 4L), "enemy", Ref (eid 5L))
         ]
  in
  match pull db [ Pull_id; Pull_recursive_ref ("friend", [ Pull_id ], Some 2); Pull_recursive_ref ("enemy", [ Pull_id ], Some 1) ] (Entity_id (eid 1L)) with
  | Some entity when List.assoc_opt (kw "friend") entity.pulled_attrs <> None -> ()
  | _ -> failf "dual recursion should preserve sibling recursive attrs"

let test_pull_api__test_deep_recursion () =
  let depth = 150 in
  let ops =
    List.init (depth - 1) (fun index -> Add (Entity_id (eid (Int64.of_int (index + 1))), "friend", Ref (eid (Int64.of_int (index + 2)))))
    @ List.init depth (fun index -> Add (Entity_id (eid (Int64.of_int (index + 1))), "name", String ("Person-" ^ string_of_int (index + 1))))
  in
  let db = empty_db ~schema:[ "friend", ref_attr ] () |> db_with ops in
  match pull db [ Pull_attr "name"; Pull_recursive_ref ("friend", [ Pull_attr "name" ], None) ] (Entity_id (eid 1L)) with
  | Some _ -> ()
  | None -> failf "deep recursive pull should complete"

let test_pull_api__test_component_reverse () =
  let db =
    empty_db ~schema:[ "ref", component_one ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id (eid 1L))
             ; attrs = [ "name", One_value (String "1"); "ref", One_entity { db_id = Some (Entity_id (eid 2L)); attrs = [ "name", One_value (String "2") ] } ]
             }
         ]
  in
  expect_pull
    "reverse component nested pull"
    db
    [ Pull_attr "name"; Pull_ref ("ref", [ Pull_attr "name"; Pull_reverse_ref ("ref", [ Pull_attr "name" ]) ]) ]
    (Entity_id (eid 1L))
    [ kw "name", scalar (String "1")
    ; kw "ref", entity (eid 2L) [ kw "name", scalar (String "2"); kw "ref", entity (eid 1L) [ kw "name", scalar (String "1") ] ]
    ]

let test_pull_api__test_lookup_ref_pull () =
  let db = test_db () in
  expect_pull
    "lookup ref pull"
    db
    [ Pull_attr "name"; Pull_attr "aka" ]
    (Lookup_ref ("name", String "Petr"))
    [ kw "aka", many_values [ scalar (String "Devil"); scalar (String "Tupen") ]
    ; kw "name", scalar (String "Petr")
    ];
  if pull db [ Pull_wildcard ] (Lookup_ref ("name", String "Unknown")) <> None then
    failf "missing lookup ref pull should return none"

let test_pull_api__test_xform () =
  let wrap = function value -> Pulled_many [ value ] in
  expect_pull
    "xform attr"
    (test_db ())
    [ Pull_attr_xform ("name", wrap); Pull_attr_xform ("aka", wrap) ]
    (Entity_id (eid 1L))
    [ kw "aka", many_values [ many_values [ scalar (String "Devil"); scalar (String "Tupen") ] ]
    ; kw "name", many_values [ scalar (String "Petr") ]
    ]

let test_pull_api__test_visitor () =
  let visits = ref [] in
  let visitor visit = visits := visit :: !visits in
  ignore (pull ~visitor (test_db ()) [ Pull_wildcard; Pull_attr "name"; Pull_reverse_ref ("child", [ Pull_id ]) ] (Entity_id (eid 2L)));
  if not (List.exists (( = ) (PullVisitAttr (eid 2L, "name"))) !visits) then failf "visitor should see attrs";
  if not (List.exists (( = ) (PullVisitWildcard (eid 2L))) !visits) then failf "visitor should see wildcard";
  if not (List.exists (( = ) (PullVisitReverse ("child", eid 2L))) !visits) then failf "visitor should see reverse attrs"

let test_pull_api__test_pull_other_dbs () =
  let db = test_db () in
  let filtered = filter db (fun _ datom -> datom.v <> String "Tupen") in
  expect_pull
    "pull reads filtered db"
    filtered
    [ Pull_attr "name"; Pull_attr "aka" ]
    (Entity_id (eid 1L))
    [ kw "aka", many_values [ scalar (String "Devil") ]; kw "name", scalar (String "Petr") ];
  let restored = db |> serializable |> from_serializable in
  expect_pull
    "pull reads restored db"
    restored
    [ Pull_attr "name"; Pull_attr "aka" ]
    (Entity_id (eid 1L))
    [ kw "aka", many_values [ scalar (String "Devil"); scalar (String "Tupen") ]; kw "name", scalar (String "Petr") ]

let () =
  test_pull_api__test_pull_attr_spec ();
  test_pull_api__test_pull_reverse_attr_spec ();
  test_pull_api__test_pull_component_attr ();
  test_pull_api__test_pull_wildcard ();
  test_pull_api__test_pull_limit ();
  test_pull_api__test_pull_default ();
  test_pull_api__test_pull_as ();
  test_pull_api__test_pull_attr_with_opts ();
  test_pull_api__test_pull_map ();
  test_pull_api__test_pull_ref_preserves_duplicate_many_datoms ();
  test_pull_api__test_pull_recursion ();
  test_pull_api__test_dual_recursion ();
  test_pull_api__test_deep_recursion ();
  test_pull_api__test_component_reverse ();
  test_pull_api__test_lookup_ref_pull ();
  test_pull_api__test_xform ();
  test_pull_api__test_visitor ();
  test_pull_api__test_pull_other_dbs ()
