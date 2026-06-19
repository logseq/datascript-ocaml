open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let kw name = Keyword name

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

let unique_identity =
  { many with cardinality = One; unique = Some Identity; indexed = true }

let ref_attr =
  { many with cardinality = One; value_type = Some RefType }

let ref_many =
  { many with value_type = Some RefType }

let component_many =
  { ref_many with is_component = true }

let component_one =
  { ref_attr with is_component = true }

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
           { db_id = Some (Entity_id 1)
           ; attrs =
               [ "name", One_value (String "Petr")
               ; "aka", Many_values [ String "Devil"; String "Tupen" ]
               ; "child", Many_values [ Ref 2; Ref 3 ]
               ]
           }
       ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "David"); "father", One_value (Ref 1) ] }
       ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Thomas"); "father", One_value (Ref 1) ] }
       ; Entity { db_id = Some (Entity_id 4); attrs = [ "name", One_value (String "Lucy") ] }
       ; Entity { db_id = Some (Entity_id 5); attrs = [ "name", One_value (String "Elizabeth") ] }
       ; Entity { db_id = Some (Entity_id 6); attrs = [ "name", One_value (String "Matthew"); "father", One_value (Ref 3) ] }
       ; Entity { db_id = Some (Entity_id 7); attrs = [ "name", One_value (String "Eunan") ] }
       ; Entity { db_id = Some (Entity_id 8); attrs = [ "name", One_value (String "Kerri") ] }
       ; Entity { db_id = Some (Entity_id 9); attrs = [ "name", One_value (String "Rebecca") ] }
       ; Entity { db_id = Some (Entity_id 10); attrs = [ "name", One_value (String "Part A"); "part", Many_values [ Ref 11; Ref 15 ] ] }
       ; Entity { db_id = Some (Entity_id 11); attrs = [ "name", One_value (String "Part A.A"); "part", Many_values [ Ref 12 ] ] }
       ; Entity { db_id = Some (Entity_id 12); attrs = [ "name", One_value (String "Part A.A.A"); "part", Many_values [ Ref 13; Ref 14 ] ] }
       ; Entity { db_id = Some (Entity_id 13); attrs = [ "name", One_value (String "Part A.A.A.A") ] }
       ; Entity { db_id = Some (Entity_id 14); attrs = [ "name", One_value (String "Part A.A.A.B") ] }
       ; Entity { db_id = Some (Entity_id 15); attrs = [ "name", One_value (String "Part A.B"); "part", Many_values [ Ref 16 ] ] }
       ; Entity { db_id = Some (Entity_id 16); attrs = [ "name", One_value (String "Part A.B.A"); "part", Many_values [ Ref 17; Ref 18 ] ] }
       ; Entity { db_id = Some (Entity_id 17); attrs = [ "name", One_value (String "Part A.B.A.A") ] }
       ; Entity { db_id = Some (Entity_id 18); attrs = [ "name", One_value (String "Part A.B.A.B") ] }
       ]

let rec string_of_value = function
  | Nil -> "nil"
  | Int value -> string_of_int value
  | Float value -> string_of_float value
  | String value -> Printf.sprintf "%S" value
  | Symbol value -> value
  | Bool value -> string_of_bool value
  | Keyword value -> ":" ^ value
  | Uuid value -> "#uuid " ^ value
  | Instant value -> "#inst " ^ string_of_int value
  | Regex value -> "#\"" ^ value ^ "\""
  | Ref value -> "Ref " ^ string_of_int value
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
          |> List.sort (fun (left, _) (right, _) -> Util.compare_value left right)
      }

let normalize_attrs attrs =
  attrs
  |> List.map (fun (key, value) -> key, normalize_pulled_value value)
  |> List.sort (fun (left, _) (right, _) -> Util.compare_value left right)

let expect_pull label db pattern entity_ref expected_attrs =
  match Pull_api.pull db pattern entity_ref with
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
    (Entity_id 1)
    [ kw "aka", many_values [ scalar (String "Devil"); scalar (String "Tupen") ]
    ; kw "name", scalar (String "Petr")
    ];
  let pulled = Pull_api.pull_many db [ Pull_attr "name" ] [ Entity_id 1; Entity_id 5; Entity_id 7; Entity_id 9 ] in
  if List.length pulled <> 4 || List.exists Option.is_none pulled then failf "pull-many should preserve requested entities"

let test_pull_api__test_pull_reverse_attr_spec () =
  let db = test_db () in
  expect_pull
    "reverse attr spec"
    db
    [ Pull_attr "name"; Pull_reverse_ref ("child", [ Pull_id ]) ]
    (Entity_id 2)
    [ kw "child", many_values [ entity 1 [ kw "db/id", scalar (Int 1) ] ]
    ; kw "name", scalar (String "David")
    ];
  expect_pull
    "reverse ref map spec"
    db
    [ Pull_attr "name"; Pull_reverse_ref ("father", [ Pull_attr "name" ]) ]
    (Entity_id 3)
    [ kw "father", many_values [ entity 6 [ kw "name", scalar (String "Matthew") ] ]
    ; kw "name", scalar (String "Thomas")
    ]

let test_pull_api__test_pull_component_attr () =
  let db = test_db () in
  expect_pull
    "component attr recursively expands"
    db
    [ Pull_attr "name"; Pull_ref ("part", [ Pull_attr "name" ]) ]
    (Entity_id 10)
    [ kw "name", scalar (String "Part A")
    ; kw "part", many_values
        [ entity 11 [ kw "name", scalar (String "Part A.A") ]
        ; entity 15 [ kw "name", scalar (String "Part A.B") ]
        ]
    ];
  expect_pull
    "reverse component returns single entity"
    db
    [ Pull_attr "name"; Pull_reverse_ref ("part", [ Pull_attr "name" ]) ]
    (Entity_id 11)
    [ kw "part", entity 10 [ kw "name", scalar (String "Part A") ]
    ; kw "name", scalar (String "Part A.A")
    ]

let test_pull_api__test_pull_wildcard () =
  let db = test_db () in
  match Pull_api.pull db [ Pull_wildcard ] (Entity_id 1) with
  | Some entity when List.assoc_opt (kw "db/id") entity.pulled_attrs = Some (scalar (Int 1))
                 && List.assoc_opt (kw "name") entity.pulled_attrs = Some (scalar (String "Petr")) -> ()
  | _ -> failf "wildcard pull should include db/id and attrs"

let test_pull_api__test_pull_limit () =
  let db =
    test_db ()
    |> db_with
         (List.init 2000 (fun index -> Add (Entity_id 8, "aka", String ("aka-" ^ string_of_int index))))
  in
  expect_pull
    "explicit limit"
    db
    [ Pull_attr_limit ("aka", 2) ]
    (Entity_id 8)
    [ kw "aka", many_values [ scalar (String "aka-0"); scalar (String "aka-1") ] ];
  match Pull_api.pull db [ Pull_attr_unlimited "aka" ] (Entity_id 8) with
  | Some entity ->
    (match List.assoc_opt (kw "aka") entity.pulled_attrs with
     | Some (Pulled_many values) when List.length values = 2000 -> ()
     | _ -> failf "unlimited limit should return all values")
  | None -> failf "expected unlimited pull"

let test_pull_api__test_pull_default () =
  let db = test_db () in
  if Pull_api.pull db [ Pull_attr "missing" ] (Entity_id 1) <> None then failf "missing attr should drop empty pull";
  expect_pull
    "default attr"
    db
    [ Pull_attr_default ("missing", String "fallback") ]
    (Entity_id 1)
    [ kw "missing", scalar (String "fallback") ];
  expect_pull
    "default does not override result"
    db
    [ Pull_attr_default ("name", String "fallback") ]
    (Entity_id 1)
    [ kw "name", scalar (String "Petr") ]

let test_pull_api__test_pull_as () =
  expect_pull
    "pull as"
    (test_db ())
    [ Pull_as (Pull_attr "name", String "Name"); Pull_as (Pull_attr "aka", kw "alias") ]
    (Entity_id 1)
    [ kw "alias", many_values [ scalar (String "Devil"); scalar (String "Tupen") ]
    ; String "Name", scalar (String "Petr")
    ]

let test_pull_api__test_pull_attr_with_opts () =
  expect_pull
    "attr with as and default"
    (test_db ())
    [ Pull_as (Pull_attr_default ("x", String "Nothing"), String "Name") ]
    (Entity_id 1)
    [ String "Name", scalar (String "Nothing") ]

let test_pull_api__test_pull_map () =
  let db = test_db () in
  expect_pull
    "single ref map"
    db
    [ Pull_attr "name"; Pull_ref ("father", [ Pull_attr "name" ]) ]
    (Entity_id 6)
    [ kw "father", entity 3 [ kw "name", scalar (String "Thomas") ]
    ; kw "name", scalar (String "Matthew")
    ];
  expect_pull
    "multi ref map"
    db
    [ Pull_attr "name"; Pull_ref ("child", [ Pull_attr "name" ]) ]
    (Entity_id 1)
    [ kw "child", many_values [ entity 2 [ kw "name", scalar (String "David") ]; entity 3 [ kw "name", scalar (String "Thomas") ] ]
    ; kw "name", scalar (String "Petr")
    ]

let test_pull_api__test_pull_ref_preserves_duplicate_many_datoms () =
  let db =
    init_db
      ~schema:[ "db/ident", unique_identity; "block/title", many; "block/tags", ref_many ]
      [ datom ~tx:1 ~e:2 ~a:"db/ident" ~v:(Keyword "logseq.class/Tag") ()
      ; datom ~tx:1 ~e:10 ~a:"block/title" ~v:(String "Template") ()
      ; datom ~tx:1 ~e:10 ~a:"block/tags" ~v:(Ref 2) ()
      ; datom ~tx:1 ~e:10 ~a:"block/tags" ~v:(Ref 2) ()
      ]
  in
  expect_pull
    "pull ref preserves duplicate many datoms"
    db
    [ Pull_attr "block/title"; Pull_ref ("block/tags", [ Pull_attr "db/ident" ]) ]
    (Entity_id 10)
    [ kw "block/tags", many_values
        [ entity 2 [ kw "db/ident", scalar (Keyword "logseq.class/Tag") ]
        ; entity 2 [ kw "db/ident", scalar (Keyword "logseq.class/Tag") ]
        ]
    ; kw "block/title", many_values [ scalar (String "Template") ]
    ]

let test_pull_api__test_pull_recursion () =
  let db =
    test_db ()
    |> db_with
         [ Add (Entity_id 4, "friend", Ref 5)
         ; Add (Entity_id 5, "friend", Ref 6)
         ; Add (Entity_id 6, "friend", Ref 7)
         ; Add (Entity_id 7, "friend", Ref 8)
         ]
  in
  match Pull_api.pull db [ Pull_id; Pull_attr "name"; Pull_recursive_ref ("friend", [ Pull_id; Pull_attr "name" ], None) ] (Entity_id 4) with
  | Some entity when List.assoc_opt (kw "friend") entity.pulled_attrs <> None -> ()
  | _ -> failf "recursive pull should expand friends"

let test_pull_api__test_dual_recursion () =
  let db =
    empty_db ~schema:[ "friend", ref_attr; "enemy", ref_attr ] ()
    |> db_with
         [ Add (Entity_id 1, "friend", Ref 2)
         ; Add (Entity_id 2, "enemy", Ref 3)
         ; Add (Entity_id 3, "friend", Ref 4)
         ; Add (Entity_id 4, "enemy", Ref 5)
         ]
  in
  match Pull_api.pull db [ Pull_id; Pull_recursive_ref ("friend", [ Pull_id ], Some 2); Pull_recursive_ref ("enemy", [ Pull_id ], Some 1) ] (Entity_id 1) with
  | Some entity when List.assoc_opt (kw "friend") entity.pulled_attrs <> None -> ()
  | _ -> failf "dual recursion should preserve sibling recursive attrs"

let test_pull_api__test_deep_recursion () =
  let depth = 150 in
  let ops =
    List.init (depth - 1) (fun index -> Add (Entity_id (index + 1), "friend", Ref (index + 2)))
    @ List.init depth (fun index -> Add (Entity_id (index + 1), "name", String ("Person-" ^ string_of_int (index + 1))))
  in
  let db = empty_db ~schema:[ "friend", ref_attr ] () |> db_with ops in
  match Pull_api.pull db [ Pull_attr "name"; Pull_recursive_ref ("friend", [ Pull_attr "name" ], None) ] (Entity_id 1) with
  | Some _ -> ()
  | None -> failf "deep recursive pull should complete"

let test_pull_api__test_component_reverse () =
  let db =
    empty_db ~schema:[ "ref", component_one ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "1"); "ref", One_entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "2") ] } ]
             }
         ]
  in
  expect_pull
    "reverse component nested pull"
    db
    [ Pull_attr "name"; Pull_ref ("ref", [ Pull_attr "name"; Pull_reverse_ref ("ref", [ Pull_attr "name" ]) ]) ]
    (Entity_id 1)
    [ kw "name", scalar (String "1")
    ; kw "ref", entity 2 [ kw "name", scalar (String "2"); kw "ref", entity 1 [ kw "name", scalar (String "1") ] ]
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
  if Pull_api.pull db [ Pull_wildcard ] (Lookup_ref ("name", String "Unknown")) <> None then
    failf "missing lookup ref pull should return none"

let test_pull_api__test_xform () =
  let wrap = function value -> Pulled_many [ value ] in
  expect_pull
    "xform attr"
    (test_db ())
    [ Pull_attr_xform ("name", wrap); Pull_attr_xform ("aka", wrap) ]
    (Entity_id 1)
    [ kw "aka", many_values [ many_values [ scalar (String "Devil"); scalar (String "Tupen") ] ]
    ; kw "name", many_values [ scalar (String "Petr") ]
    ]

let test_pull_api__test_visitor () =
  let visits = ref [] in
  let visitor visit = visits := visit :: !visits in
  ignore (Pull_api.pull ~visitor (test_db ()) [ Pull_wildcard; Pull_attr "name"; Pull_reverse_ref ("child", [ Pull_id ]) ] (Entity_id 2));
  if not (List.exists (( = ) (PullVisitAttr (2, "name"))) !visits) then failf "visitor should see attrs";
  if not (List.exists (( = ) (PullVisitWildcard 2)) !visits) then failf "visitor should see wildcard";
  if not (List.exists (( = ) (PullVisitReverse ("child", 2))) !visits) then failf "visitor should see reverse attrs"

let test_pull_api__test_pull_other_dbs () =
  let db = test_db () in
  let filtered = filter db (fun _ datom -> datom.v <> String "Tupen") in
  expect_pull
    "pull reads filtered db"
    filtered
    [ Pull_attr "name"; Pull_attr "aka" ]
    (Entity_id 1)
    [ kw "aka", many_values [ scalar (String "Devil") ]; kw "name", scalar (String "Petr") ];
  let restored = db |> serializable |> from_serializable in
  expect_pull
    "pull reads restored db"
    restored
    [ Pull_attr "name"; Pull_attr "aka" ]
    (Entity_id 1)
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
