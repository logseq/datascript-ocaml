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

let indexed =
  { many with cardinality = One; indexed = true }

let unique_identity =
  { indexed with unique = Some Identity }

let ref_attr =
  { many with cardinality = One; value_type = Some RefType }

let tuple attrs =
  { cardinality = One
  ; unique = None
  ; indexed = true
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = Some TupleType
  ; tuple_attrs = Some attrs
  ; tuple_types = None
  }

let tuple_unique_identity attrs =
  { (tuple attrs) with unique = Some Identity }

let tuple_value values =
  Tuple (List.map (fun value -> Some value) values)

let tuple_opt values =
  Tuple values

let scalar value = Pulled_scalar value

let datom_triples db =
  datoms db Eavt () |> List.map (fun datom -> datom.e, datom.a, datom.v)

let sort_triples triples =
  List.sort compare triples

let assert_triples label expected actual =
  if sort_triples expected <> sort_triples actual then failf "%s" label

let assert_query_set label expected actual =
  if List.sort compare expected <> List.sort compare actual then failf "%s" label

let assert_invalid label f =
  match f () with
  | exception Invalid_argument _ -> ()
  | exception exn -> failf "%s: unexpected %s" label (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

let expect_pull_attrs ?(pattern = [ Pull_wildcard ]) label db entity_ref expected =
  match pull db pattern entity_ref with
  | Some entity when List.sort compare entity.pulled_attrs = List.sort compare expected -> ()
  | Some _ -> failf "%s: unexpected pulled attrs" label
  | None -> failf "%s: expected entity" label

let test_tuples__test_schema () =
  let db =
    empty_db
      ~schema:
        [ "year+session", tuple [ "year"; "session" ]
        ; "semester+course+student", tuple [ "semester"; "course"; "student" ]
        ; "session+student", tuple [ "session"; "student" ]
        ]
      ()
  in
  List.iter
    (fun attr ->
      match List.assoc_opt attr (schema db) with
      | Some { value_type = Some TupleType; tuple_attrs = Some _; indexed = true; cardinality = One; _ } -> ()
      | _ -> failf "expected tuple schema for %s" attr)
    [ "year+session"; "semester+course+student"; "session+student" ];
  assert_invalid
    "tuple attrs cannot depend on another tuple attr"
    (fun () -> ignore (empty_db ~schema:[ "t1", tuple [ "a"; "b" ]; "t2", tuple [ "c"; "d"; "t1" ] ] ()));
  assert_invalid "tuple attrs cannot be empty" (fun () -> ignore (empty_db ~schema:[ "t1", tuple [] ] ()));
  assert_invalid
    "tuple attrs must be cardinality one"
    (fun () -> ignore (empty_db ~schema:[ "t1", { (tuple [ "a"; "b"; "c" ]) with cardinality = Many } ] ()));
  assert_invalid
    "tuple attrs cannot depend on cardinality many attr"
    (fun () -> ignore (empty_db ~schema:[ "a", many; "t1", tuple [ "a"; "b"; "c" ] ] ()));
  assert_invalid
    "tuple value type requires tuple attrs"
    (fun () ->
      ignore
        (empty_db
           ~schema:
             [ ( "foo+bar"
               , { indexed with value_type = Some TupleType; tuple_attrs = None; tuple_types = None } )
             ]
           ()))

let test_tuples__test_tx () =
  let conn = create_conn ~schema:[ "a+b", tuple [ "a"; "b" ]; "a+c+d", tuple [ "a"; "c"; "d" ] ] () in
  let step tx expected =
    ignore (transact_bang conn tx);
    assert_triples "tuple tx datoms" expected (datom_triples (conn_db conn))
  in
  step
    [ Add (Entity_id 1, "a", String "a") ]
    [ 1, "a", String "a"; 1, "a+b", tuple_opt [ Some (String "a"); None ]; 1, "a+c+d", tuple_opt [ Some (String "a"); None; None ] ];
  step
    [ Add (Entity_id 1, "b", String "b") ]
    [ 1, "a", String "a"; 1, "b", String "b"; 1, "a+b", tuple_value [ String "a"; String "b" ]; 1, "a+c+d", tuple_opt [ Some (String "a"); None; None ] ];
  step
    [ Add (Entity_id 1, "a", String "A") ]
    [ 1, "a", String "A"; 1, "b", String "b"; 1, "a+b", tuple_value [ String "A"; String "b" ]; 1, "a+c+d", tuple_opt [ Some (String "A"); None; None ] ];
  step
    [ Add (Entity_id 1, "c", String "c"); Add (Entity_id 1, "d", String "d") ]
    [ 1, "a", String "A"; 1, "b", String "b"; 1, "a+b", tuple_value [ String "A"; String "b" ]; 1, "c", String "c"; 1, "d", String "d"; 1, "a+c+d", tuple_value [ String "A"; String "c"; String "d" ] ];
  step
    [ Retract (Entity_id 1, "a", Some (String "A")) ]
    [ 1, "b", String "b"; 1, "a+b", tuple_opt [ None; Some (String "b") ]; 1, "c", String "c"; 1, "d", String "d"; 1, "a+c+d", tuple_opt [ None; Some (String "c"); Some (String "d") ] ];
  assert_invalid
    "cannot modify tuple attrs directly"
    (fun () -> ignore (transact_bang conn [ Entity { db_id = Some (Entity_id 1); attrs = [ "a+b", One_value (tuple_value [ String "A"; String "B" ]) ] } ]))

let test_tuples__test_ignore_correct () =
  let conn = create_conn ~schema:[ "a+b", tuple [ "a"; "b" ] ] () in
  ignore
    (transact_bang
       conn
       [ Entity
           { db_id = Some (Entity_id 1)
           ; attrs = [ "a", One_value (String "a"); "b", One_value (String "b"); "a+b", One_value (tuple_value [ String "a"; String "b" ]) ]
           }
       ]);
  assert_invalid
    "mismatched tuple insert is rejected"
    (fun () ->
      ignore
        (transact_bang
           conn
           [ Entity
               { db_id = Some (Entity_id 2)
               ; attrs = [ "a", One_value (String "x"); "b", One_value (String "y"); "a+b", One_value (tuple_value [ String "a"; String "b" ]) ]
               }
           ]));
  ignore
    (transact_bang
       conn
       [ Entity
           { db_id = Some (Entity_id 1)
           ; attrs = [ "b", One_value (String "B"); "a+b", One_value (tuple_value [ String "a"; String "B" ]) ]
           }
       ]);
  expect_pull_attrs
    "matching direct tuple write is ignored"
    (conn_db conn)
    (Entity_id 1)
    [ Keyword "a", scalar (String "a"); Keyword "a+b", scalar (tuple_value [ String "a"; String "B" ]); Keyword "b", scalar (String "B"); Keyword "db/id", scalar (Int 1) ]

let test_tuples__test_unique () =
  let conn = create_conn ~schema:[ "a+b", tuple_unique_identity [ "a"; "b" ] ] () in
  ignore (transact_bang conn [ Add (Entity_id 1, "a", String "a") ]);
  ignore (transact_bang conn [ Add (Entity_id 2, "a", String "A") ]);
  assert_invalid "unique tuple rejects duplicate partial update" (fun () -> ignore (transact_bang conn [ Add (Entity_id 1, "a", String "A") ]));
  ignore
    (transact_bang
       conn
       [ Add (Entity_id 1, "b", String "b")
       ; Add (Entity_id 2, "b", String "b")
       ; Entity { db_id = Some (Entity_id 3); attrs = [ "a", One_value (String "a"); "b", One_value (String "B") ] }
       ]);
  assert_invalid "unique tuple rejects duplicate a" (fun () -> ignore (transact_bang conn [ Add (Entity_id 1, "a", String "A") ]));
  assert_invalid "unique tuple rejects duplicate b" (fun () -> ignore (transact_bang conn [ Add (Entity_id 1, "b", String "B") ]));
  ignore (transact_bang conn [ Entity { db_id = Some (Entity_id 1); attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ] } ]);
  expect_pull_attrs
    "multiple tuple updates are atomic"
    (conn_db conn)
    (Entity_id 1)
    [ Keyword "a", scalar (String "A"); Keyword "a+b", scalar (tuple_value [ String "A"; String "B" ]); Keyword "b", scalar (String "B"); Keyword "db/id", scalar (Int 1) ];
  ignore (transact_bang conn [ Entity { db_id = Some (Entity_id 4); attrs = [ "a", One_value (String "a"); "b", One_value (String "b") ] } ]);
  expect_pull_attrs
    "insert with two tuple components is atomic"
    (conn_db conn)
    (Entity_id 4)
    [ Keyword "a", scalar (String "a"); Keyword "a+b", scalar (tuple_value [ String "a"; String "b" ]); Keyword "b", scalar (String "b"); Keyword "db/id", scalar (Int 4) ]

let test_tuples__test_upsert () =
  let conn = create_conn ~schema:[ "a+b", tuple_unique_identity [ "a"; "b" ]; "c", unique_identity ] () in
  ignore
    (transact_bang
       conn
       [ Entity { db_id = Some (Entity_id 1); attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ] }
       ; Entity { db_id = Some (Entity_id 2); attrs = [ "a", One_value (String "a"); "b", One_value (String "b") ] }
       ]);
  ignore
    (transact_bang
       conn
       [ Entity { db_id = None; attrs = [ "a+b", One_value (tuple_value [ String "A"; String "B" ]); "c", One_value (String "C") ] }
       ; Entity { db_id = None; attrs = [ "a+b", One_value (tuple_value [ String "a"; String "b" ]); "c", One_value (String "c") ] }
       ]);
  assert_triples
    "upsert by unique tuple"
    [ 1, "a", String "A"; 1, "b", String "B"; 1, "a+b", tuple_value [ String "A"; String "B" ]; 1, "c", String "C"
    ; 2, "a", String "a"; 2, "b", String "b"; 2, "a+b", tuple_value [ String "a"; String "b" ]; 2, "c", String "c"
    ]
    (datom_triples (conn_db conn));
  assert_invalid
    "conflicting tuple upserts are rejected"
    (fun () -> ignore (transact_bang conn [ Entity { db_id = None; attrs = [ "a+b", One_value (tuple_value [ String "A"; String "B" ]); "c", One_value (String "c") ] } ]));
  ignore (transact_bang conn [ Entity { db_id = None; attrs = [ "a+b", One_value (tuple_value [ String "A"; String "B" ]); "b", One_value (String "b"); "d", One_value (String "D") ] } ]);
  expect_pull_attrs
    "change tuple source during upsert"
    (conn_db conn)
    (Entity_id 1)
    [ Keyword "a", scalar (String "A"); Keyword "a+b", scalar (tuple_value [ String "A"; String "b" ]); Keyword "b", scalar (String "b"); Keyword "c", scalar (String "C"); Keyword "d", scalar (String "D"); Keyword "db/id", scalar (Int 1) ]

let test_tuples__test_upsert_by_tuple_components () =
  let db =
    empty_db ~schema:[ "a+b", tuple_unique_identity [ "a"; "b" ] ] ()
    |> db_with [ Entity { db_id = None; attrs = [ "a", One_value (String "A"); "b", One_value (String "B"); "name", One_value (String "Ivan") ] } ]
  in
  let expected = [ 1, "a", String "A"; 1, "b", String "B"; 1, "a+b", tuple_value [ String "A"; String "B" ]; 1, "name", String "Oleg" ] in
  assert_triples
    "entity map with temp id upserts by tuple components"
    expected
    (datom_triples (db_with [ Entity { db_id = Some (Temp_id "x"); attrs = [ "a", One_value (String "A"); "b", One_value (String "B"); "name", One_value (String "Oleg") ] } ] db));
  assert_triples
    "entity map without id upserts by tuple components"
    expected
    (datom_triples (db_with [ Entity { db_id = None; attrs = [ "a", One_value (String "A"); "b", One_value (String "B"); "name", One_value (String "Oleg") ] } ] db));
  assert_triples
    "add ops upsert by tuple components"
    expected
    (datom_triples (db_with [ Add (Temp_id "x", "a", String "A"); Add (Temp_id "x", "b", String "B"); Add (Temp_id "x", "name", String "Oleg") ] db))

let test_tuples__test_lookup_refs () =
  let conn = create_conn ~schema:[ "a+b", tuple_unique_identity [ "a"; "b" ]; "c", unique_identity ] () in
  ignore
    (transact_bang
       conn
       [ Entity { db_id = Some (Entity_id 1); attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ] }
       ; Entity { db_id = Some (Entity_id 2); attrs = [ "a", One_value (String "a"); "b", One_value (String "b") ] }
       ]);
  ignore (transact_bang conn [ Add (Lookup_ref ("a+b", tuple_value [ String "A"; String "B" ]), "c", String "C"); Entity { db_id = Some (Lookup_ref ("a+b", tuple_value [ String "a"; String "b" ])); attrs = [ "c", One_value (String "c") ] } ]);
  assert_invalid
    "lookup ref tuple unique violation"
    (fun () -> ignore (transact_bang conn [ Add (Lookup_ref ("a+b", tuple_value [ String "A"; String "B" ]), "c", String "c") ]));
  assert_invalid
    "explicit lookup ref conflicts with c upsert"
    (fun () -> ignore (transact_bang conn [ Entity { db_id = Some (Lookup_ref ("a+b", tuple_value [ String "A"; String "B" ])); attrs = [ "c", One_value (String "c") ] } ]));
  ignore
    (transact_bang
       conn
       [ Entity
           { db_id = Some (Lookup_ref ("a+b", tuple_value [ String "A"; String "B" ]))
           ; attrs = [ "b", One_value (String "b"); "d", One_value (String "D") ]
           }
       ]);
  expect_pull_attrs
    "pull by tuple lookup ref"
    (conn_db conn)
    (Lookup_ref ("a+b", tuple_value [ String "a"; String "b" ]))
    [ Keyword "a", scalar (String "a"); Keyword "a+b", scalar (tuple_value [ String "a"; String "b" ]); Keyword "b", scalar (String "b"); Keyword "c", scalar (String "c"); Keyword "db/id", scalar (Int 2) ]

let test_tuples__lookup_refs_in_tuple () =
  let db =
    empty_db ~schema:[ "ref", ref_attr; "name", unique_identity; "ref+name", tuple_unique_identity [ "ref"; "name" ] ] ()
    |> db_with
         [ Entity { db_id = Some (Temp_id "ivan"); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Temp_id "oleg"); attrs = [ "name", One_value (String "Oleg") ] }
         ; Entity { db_id = Some (Temp_id "petr"); attrs = [ "name", One_value (String "Petr"); "ref", One_value (Ref_to (Temp_id "ivan")) ] }
         ; Entity { db_id = Some (Temp_id "yuri"); attrs = [ "name", One_value (String "Yuri"); "ref", One_value (Ref_to (Temp_id "oleg")) ] }
         ]
  in
  let by_id = db_with [ Entity { db_id = None; attrs = [ "ref+name", One_value (tuple_value [ Ref 1; String "Petr" ]); "age", One_value (Int 32) ] } ] db in
  expect_pull_attrs ~pattern:[ Pull_attr "age" ] "tuple lookup with id ref" by_id (Entity_id 3) [ Keyword "age", scalar (Int 32) ];
  let by_lookup =
    db_with
      [ Entity
          { db_id = None
          ; attrs = [ "ref+name", One_value (Tuple [ Some (Vector [ Keyword "name"; String "Ivan" ]); Some (String "Petr") ]); "age", One_value (Int 32) ]
          }
      ]
      db
  in
  expect_pull_attrs ~pattern:[ Pull_attr "age" ] "tuple lookup with nested lookup ref" by_lookup (Entity_id 3) [ Keyword "age", scalar (Int 32) ];
  if entid db "ref+name" (tuple_value [ Ref 1; String "Petr" ]) <> Some 3 then failf "tuple entid by id ref";
  if entid db "ref+name" (Vector [ Vector [ Keyword "name"; String "Ivan" ]; String "Petr" ]) <> Some 3 then failf "tuple entid by nested lookup ref"

let test_tuples__test_validation () =
  let db = empty_db ~schema:[ "a+b", tuple [ "a"; "b" ] ] () in
  let db1 = db_with [ Add (Entity_id 1, "a", String "a") ] db in
  assert_invalid "cannot add nil tuple directly" (fun () -> ignore (db_with [ Add (Entity_id 1, "a+b", tuple_opt [ None; None ]) ] db));
  assert_invalid "cannot add partial tuple directly" (fun () -> ignore (db_with [ Add (Entity_id 1, "a+b", tuple_opt [ Some (String "a"); None ]) ] db1));
  assert_invalid
    "cannot mix source and partial direct tuple"
    (fun () -> ignore (db_with [ Add (Entity_id 1, "a", String "a"); Add (Entity_id 1, "a+b", tuple_opt [ Some (String "a"); None ]) ] db));
  assert_invalid "cannot retract tuple directly" (fun () -> ignore (db_with [ Retract (Entity_id 1, "a+b", Some (tuple_opt [ Some (String "a"); None ])) ] db1))

let test_tuples__test_indexes () =
  let db =
    empty_db ~schema:[ "a+b+c", tuple [ "a"; "b"; "c" ] ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "a", One_value (String "a"); "b", One_value (String "b"); "c", One_value (String "c") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "a", One_value (String "A"); "b", One_value (String "b"); "c", One_value (String "c") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "a", One_value (String "a"); "b", One_value (String "B"); "c", One_value (String "c") ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "a", One_value (String "A"); "b", One_value (String "B"); "c", One_value (String "c") ] }
         ; Entity { db_id = Some (Entity_id 5); attrs = [ "a", One_value (String "a"); "b", One_value (String "b"); "c", One_value (String "C") ] }
         ; Entity { db_id = Some (Entity_id 6); attrs = [ "a", One_value (String "A"); "b", One_value (String "b"); "c", One_value (String "C") ] }
         ; Entity { db_id = Some (Entity_id 7); attrs = [ "a", One_value (String "a"); "b", One_value (String "B"); "c", One_value (String "C") ] }
         ; Entity { db_id = Some (Entity_id 8); attrs = [ "a", One_value (String "A"); "b", One_value (String "B"); "c", One_value (String "C") ] }
         ]
  in
  if (datoms db Avet ~a:"a+b+c" ~v:(tuple_value [ String "A"; String "b"; String "C" ]) () |> List.map (fun d -> d.e)) <> [ 6 ] then failf "tuple avet exact lookup";
  if datoms db Avet ~a:"a+b+c" ~v:(tuple_opt [ Some (String "A"); Some (String "b"); None ]) () <> [] then failf "tuple avet exact lookup with nil";
  if (index_range db "a+b+c" ~start:(tuple_value [ String "A"; String "B"; String "C" ]) ~stop:(tuple_value [ String "A"; String "b"; String "c" ]) () |> List.map (fun d -> d.e)) <> [ 8; 4; 6; 2 ] then failf "tuple index range";
  if (index_range db "a+b+c" ~start:(tuple_opt [ Some (String "A"); Some (String "B"); None ]) ~stop:(tuple_opt [ Some (String "A"); Some (String "b"); None ]) () |> List.map (fun d -> d.e)) <> [ 8; 4 ] then failf "tuple index range with nil bounds"

let test_tuples__test_queries () =
  let db =
    empty_db ~schema:[ "a+b", tuple_unique_identity [ "a"; "b" ] ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "a", One_value (String "A"); "b", One_value (String "b") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "a", One_value (String "a"); "b", One_value (String "B") ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "a", One_value (String "a"); "b", One_value (String "b") ] }
         ]
  in
  assert_query_set
    "query tuple attr value"
    [ [ Result_entity 3 ] ]
    (q_string db "[:find ?e :where [?e :a+b [\"a\" \"B\"]]]");
  assert_query_set
    "query tuple lookup ref"
    [ [ Result_value (tuple_value [ String "a"; String "B" ]) ] ]
    (q_string db "[:find ?a+b :where [[:a+b [\"a\" \"B\"]] :a+b ?a+b]]");
  assert_query_set
    "query tuple function"
    [ [ Result_value (tuple_value [ String "A"; String "B" ]) ]
    ; [ Result_value (tuple_value [ String "A"; String "b" ]) ]
    ; [ Result_value (tuple_value [ String "a"; String "B" ]) ]
    ; [ Result_value (tuple_value [ String "a"; String "b" ]) ]
    ]
    (q_string db "[:find ?a+b :where [?e :a ?a] [?e :b ?b] [(tuple ?a ?b) ?a+b]]");
  assert_query_set
    "query untuple function"
    [ [ Result_value (String "A"); Result_value (String "B") ]
    ; [ Result_value (String "A"); Result_value (String "b") ]
    ; [ Result_value (String "a"); Result_value (String "B") ]
    ; [ Result_value (String "a"); Result_value (String "b") ]
    ]
    (q_string db "[:find ?a ?b :where [?e :a+b ?a+b] [(untuple ?a+b) [?a ?b]]]")

let run label f =
  match f () with
  | () -> ()
  | exception exn -> failf "%s: %s" label (Printexc.to_string exn)

let () =
  run "test_tuples__test_schema" test_tuples__test_schema;
  run "test_tuples__test_tx" test_tuples__test_tx;
  run "test_tuples__test_ignore_correct" test_tuples__test_ignore_correct;
  run "test_tuples__test_unique" test_tuples__test_unique;
  run "test_tuples__test_upsert" test_tuples__test_upsert;
  run "test_tuples__test_upsert_by_tuple_components" test_tuples__test_upsert_by_tuple_components;
  run "test_tuples__test_lookup_refs" test_tuples__test_lookup_refs;
  run "test_tuples__lookup_refs_in_tuple" test_tuples__lookup_refs_in_tuple;
  run "test_tuples__test_validation" test_tuples__test_validation;
  run "test_tuples__test_indexes" test_tuples__test_indexes;
  run "test_tuples__test_queries" test_tuples__test_queries
