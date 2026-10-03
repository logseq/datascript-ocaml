open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms ?e ?a ?v ?tx db index =
  datoms_seq ?e ?a ?v ?tx db index |> List.of_seq

let index_range_seq = index_range
let index_range ?start ?stop db attr =
  index_range_seq ?start ?stop db attr |> List.of_seq

let many =
  Schema.spec ~cardinality:(Many) ?unique:(None) ~indexed:(false) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let indexed =
  Schema.spec ~cardinality:(One) ?unique:((Schema.unique many)) ~indexed:(true) ~is_component:((Schema.is_component many)) ~no_history:((Schema.no_history many)) ?doc:((Schema.doc many)) ?value_type:((Schema.value_type many)) ?tuple:(match ((Schema.tuple_attrs many), (Schema.tuple_types many)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let unique_identity =
  Schema.spec ~cardinality:((Schema.cardinality indexed)) ?unique:(Some Identity) ~indexed:((Schema.indexed indexed)) ~is_component:((Schema.is_component indexed)) ~no_history:((Schema.no_history indexed)) ?doc:((Schema.doc indexed)) ?value_type:((Schema.value_type indexed)) ?tuple:(match ((Schema.tuple_attrs indexed), (Schema.tuple_types indexed)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let ref_attr =
  Schema.spec ~cardinality:(One) ?unique:((Schema.unique many)) ~indexed:((Schema.indexed many)) ~is_component:((Schema.is_component many)) ~no_history:((Schema.no_history many)) ?doc:((Schema.doc many)) ?value_type:(Some RefType) ?tuple:(match ((Schema.tuple_attrs many), (Schema.tuple_types many)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let tuple attrs =
  Schema.spec ~cardinality:(One) ?unique:(None) ~indexed:(true) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(Some TupleType) ?tuple:(match (Some attrs, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let tuple_unique_identity attrs =
  Schema.spec ~cardinality:(Schema.cardinality (tuple attrs)) ~unique:Identity ~indexed:(Schema.indexed (tuple attrs)) ~is_component:(Schema.is_component (tuple attrs)) ~no_history:(Schema.no_history (tuple attrs)) ?doc:(Schema.doc (tuple attrs)) ?value_type:(Schema.value_type (tuple attrs)) ?tuple:(match (Schema.tuple_attrs (tuple attrs), Schema.tuple_types (tuple attrs)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let tuple_value values =
  Tuple (List.map (fun value -> Some value) values)

let tuple_opt values =
  Tuple values

let scalar value = Pulled_scalar value

let datom_triples db =
  datoms db Eavt |> List.map (fun datom -> datom.e, datom.a, datom.v)

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
      | Some spec when Schema.value_type spec = Some TupleType && Schema.tuple_attrs spec <> None && Schema.indexed spec && Schema.cardinality spec = One -> ()
      | _ -> failf "expected tuple schema for %s" attr)
    [ "year+session"; "semester+course+student"; "session+student" ];
  assert_invalid
    "tuple attrs cannot depend on another tuple attr"
    (fun () -> ignore (empty_db ~schema:[ "t1", tuple [ "a"; "b" ]; "t2", tuple [ "c"; "d"; "t1" ] ] ()));
  assert_invalid "tuple attrs cannot be empty" (fun () -> ignore (empty_db ~schema:[ "t1", tuple [] ] ()));
  assert_invalid
    "tuple attrs must be cardinality one"
    (fun () -> ignore (empty_db ~schema:[ "t1", Schema.spec ~cardinality:Many ?unique:(Schema.unique (tuple [ "a"; "b"; "c" ])) ~indexed:(Schema.indexed (tuple [ "a"; "b"; "c" ])) ~is_component:(Schema.is_component (tuple [ "a"; "b"; "c" ])) ~no_history:(Schema.no_history (tuple [ "a"; "b"; "c" ])) ?doc:(Schema.doc (tuple [ "a"; "b"; "c" ])) ?value_type:(Schema.value_type (tuple [ "a"; "b"; "c" ])) ?tuple:(match (Schema.tuple_attrs (tuple [ "a"; "b"; "c" ]), Schema.tuple_types (tuple [ "a"; "b"; "c" ])) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) () ] ()));
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
               , Schema.spec ~cardinality:((Schema.cardinality indexed)) ?unique:((Schema.unique indexed)) ~indexed:((Schema.indexed indexed)) ~is_component:((Schema.is_component indexed)) ~no_history:((Schema.no_history indexed)) ?doc:((Schema.doc indexed)) ?value_type:(Some TupleType) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) () )
             ]
           ()))

let test_tuples__test_tx () =
  let conn = create_conn ~schema:[ "a+b", tuple [ "a"; "b" ]; "a+c+d", tuple [ "a"; "c"; "d" ] ] () in
  let step tx expected =
    ignore (Compat.transact_bang conn tx);
    assert_triples "tuple tx datoms" expected (datom_triples (conn_db conn))
  in
  step
    [ Add (Entity_id (eid 1L), "a", String "a") ]
    [ (eid 1L), "a", String "a"; (eid 1L), "a+b", tuple_opt [ Some (String "a"); None ]; (eid 1L), "a+c+d", tuple_opt [ Some (String "a"); None; None ] ];
  step
    [ Add (Entity_id (eid 1L), "b", String "b") ]
    [ (eid 1L), "a", String "a"; (eid 1L), "b", String "b"; (eid 1L), "a+b", tuple_value [ String "a"; String "b" ]; (eid 1L), "a+c+d", tuple_opt [ Some (String "a"); None; None ] ];
  step
    [ Add (Entity_id (eid 1L), "a", String "A") ]
    [ (eid 1L), "a", String "A"; (eid 1L), "b", String "b"; (eid 1L), "a+b", tuple_value [ String "A"; String "b" ]; (eid 1L), "a+c+d", tuple_opt [ Some (String "A"); None; None ] ];
  step
    [ Add (Entity_id (eid 1L), "c", String "c"); Add (Entity_id (eid 1L), "d", String "d") ]
    [ (eid 1L), "a", String "A"; (eid 1L), "b", String "b"; (eid 1L), "a+b", tuple_value [ String "A"; String "b" ]; (eid 1L), "c", String "c"; (eid 1L), "d", String "d"; (eid 1L), "a+c+d", tuple_value [ String "A"; String "c"; String "d" ] ];
  step
    [ Retract (Entity_id (eid 1L), "a", Some (String "A")) ]
    [ (eid 1L), "b", String "b"; (eid 1L), "a+b", tuple_opt [ None; Some (String "b") ]; (eid 1L), "c", String "c"; (eid 1L), "d", String "d"; (eid 1L), "a+c+d", tuple_opt [ None; Some (String "c"); Some (String "d") ] ];
  assert_invalid
    "cannot modify tuple attrs directly"
    (fun () -> ignore (Compat.transact_bang conn [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "a+b", One_value (tuple_value [ String "A"; String "B" ]) ] } ]))

let test_tuples__test_ignore_correct () =
  let conn = create_conn ~schema:[ "a+b", tuple [ "a"; "b" ] ] () in
  ignore
    (Compat.transact_bang
       conn
       [ Entity
           { db_id = Some (Entity_id (eid 1L))
           ; attrs = [ "a", One_value (String "a"); "b", One_value (String "b"); "a+b", One_value (tuple_value [ String "a"; String "b" ]) ]
           }
       ]);
  assert_invalid
    "mismatched tuple insert is rejected"
    (fun () ->
      ignore
        (Compat.transact_bang
           conn
           [ Entity
               { db_id = Some (Entity_id (eid 2L))
               ; attrs = [ "a", One_value (String "x"); "b", One_value (String "y"); "a+b", One_value (tuple_value [ String "a"; String "b" ]) ]
               }
           ]));
  ignore
    (Compat.transact_bang
       conn
       [ Entity
           { db_id = Some (Entity_id (eid 1L))
           ; attrs = [ "b", One_value (String "B"); "a+b", One_value (tuple_value [ String "a"; String "B" ]) ]
           }
       ]);
  expect_pull_attrs
    "matching direct tuple write is ignored"
    (conn_db conn)
    (Entity_id (eid 1L))
    [ Keyword "a", scalar (String "a"); Keyword "a+b", scalar (tuple_value [ String "a"; String "B" ]); Keyword "b", scalar (String "B"); Keyword "db/id", scalar (Int64 1L) ]

let test_tuples__test_unique () =
  let conn = create_conn ~schema:[ "a+b", tuple_unique_identity [ "a"; "b" ] ] () in
  ignore (Compat.transact_bang conn [ Add (Entity_id (eid 1L), "a", String "a") ]);
  ignore (Compat.transact_bang conn [ Add (Entity_id (eid 2L), "a", String "A") ]);
  assert_invalid "unique tuple rejects duplicate partial update" (fun () -> ignore (Compat.transact_bang conn [ Add (Entity_id (eid 1L), "a", String "A") ]));
  ignore
    (Compat.transact_bang
       conn
       [ Add (Entity_id (eid 1L), "b", String "b")
       ; Add (Entity_id (eid 2L), "b", String "b")
       ; Entity { db_id = Some (Entity_id (eid 3L)); attrs = [ "a", One_value (String "a"); "b", One_value (String "B") ] }
       ]);
  assert_invalid "unique tuple rejects duplicate a" (fun () -> ignore (Compat.transact_bang conn [ Add (Entity_id (eid 1L), "a", String "A") ]));
  assert_invalid "unique tuple rejects duplicate b" (fun () -> ignore (Compat.transact_bang conn [ Add (Entity_id (eid 1L), "b", String "B") ]));
  ignore (Compat.transact_bang conn [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ] } ]);
  expect_pull_attrs
    "multiple tuple updates are atomic"
    (conn_db conn)
    (Entity_id (eid 1L))
    [ Keyword "a", scalar (String "A"); Keyword "a+b", scalar (tuple_value [ String "A"; String "B" ]); Keyword "b", scalar (String "B"); Keyword "db/id", scalar (Int64 1L) ];
  ignore (Compat.transact_bang conn [ Entity { db_id = Some (Entity_id (eid 4L)); attrs = [ "a", One_value (String "a"); "b", One_value (String "b") ] } ]);
  expect_pull_attrs
    "insert with two tuple components is atomic"
    (conn_db conn)
    (Entity_id (eid 4L))
    [ Keyword "a", scalar (String "a"); Keyword "a+b", scalar (tuple_value [ String "a"; String "b" ]); Keyword "b", scalar (String "b"); Keyword "db/id", scalar (Int64 4L) ]

let test_tuples__test_upsert () =
  let conn = create_conn ~schema:[ "a+b", tuple_unique_identity [ "a"; "b" ]; "c", unique_identity ] () in
  ignore
    (Compat.transact_bang
       conn
       [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ] }
       ; Entity { db_id = Some (Entity_id (eid 2L)); attrs = [ "a", One_value (String "a"); "b", One_value (String "b") ] }
       ]);
  ignore
    (Compat.transact_bang
       conn
       [ Entity { db_id = None; attrs = [ "a+b", One_value (tuple_value [ String "A"; String "B" ]); "c", One_value (String "C") ] }
       ; Entity { db_id = None; attrs = [ "a+b", One_value (tuple_value [ String "a"; String "b" ]); "c", One_value (String "c") ] }
       ]);
  assert_triples
    "upsert by unique tuple"
    [ (eid 1L), "a", String "A"; (eid 1L), "b", String "B"; (eid 1L), "a+b", tuple_value [ String "A"; String "B" ]; (eid 1L), "c", String "C"
    ; (eid 2L), "a", String "a"; (eid 2L), "b", String "b"; (eid 2L), "a+b", tuple_value [ String "a"; String "b" ]; (eid 2L), "c", String "c"
    ]
    (datom_triples (conn_db conn));
  assert_invalid
    "conflicting tuple upserts are rejected"
    (fun () -> ignore (Compat.transact_bang conn [ Entity { db_id = None; attrs = [ "a+b", One_value (tuple_value [ String "A"; String "B" ]); "c", One_value (String "c") ] } ]));
  ignore (Compat.transact_bang conn [ Entity { db_id = None; attrs = [ "a+b", One_value (tuple_value [ String "A"; String "B" ]); "b", One_value (String "b"); "d", One_value (String "D") ] } ]);
  expect_pull_attrs
    "change tuple source during upsert"
    (conn_db conn)
    (Entity_id (eid 1L))
    [ Keyword "a", scalar (String "A"); Keyword "a+b", scalar (tuple_value [ String "A"; String "b" ]); Keyword "b", scalar (String "b"); Keyword "c", scalar (String "C"); Keyword "d", scalar (String "D"); Keyword "db/id", scalar (Int64 1L) ]

let test_tuples__test_upsert_by_tuple_components () =
  let db =
    empty_db ~schema:[ "a+b", tuple_unique_identity [ "a"; "b" ] ] ()
    |> db_with [ Entity { db_id = None; attrs = [ "a", One_value (String "A"); "b", One_value (String "B"); "name", One_value (String "Ivan") ] } ]
  in
  let expected = [ (eid 1L), "a", String "A"; (eid 1L), "b", String "B"; (eid 1L), "a+b", tuple_value [ String "A"; String "B" ]; (eid 1L), "name", String "Oleg" ] in
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
    (Compat.transact_bang
       conn
       [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ] }
       ; Entity { db_id = Some (Entity_id (eid 2L)); attrs = [ "a", One_value (String "a"); "b", One_value (String "b") ] }
       ]);
  ignore (Compat.transact_bang conn [ Add (Lookup_ref ("a+b", tuple_value [ String "A"; String "B" ]), "c", String "C"); Entity { db_id = Some (Lookup_ref ("a+b", tuple_value [ String "a"; String "b" ])); attrs = [ "c", One_value (String "c") ] } ]);
  assert_invalid
    "lookup ref tuple unique violation"
    (fun () -> ignore (Compat.transact_bang conn [ Add (Lookup_ref ("a+b", tuple_value [ String "A"; String "B" ]), "c", String "c") ]));
  assert_invalid
    "explicit lookup ref conflicts with c upsert"
    (fun () -> ignore (Compat.transact_bang conn [ Entity { db_id = Some (Lookup_ref ("a+b", tuple_value [ String "A"; String "B" ])); attrs = [ "c", One_value (String "c") ] } ]));
  ignore
    (Compat.transact_bang
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
    [ Keyword "a", scalar (String "a"); Keyword "a+b", scalar (tuple_value [ String "a"; String "b" ]); Keyword "b", scalar (String "b"); Keyword "c", scalar (String "c"); Keyword "db/id", scalar (Int64 2L) ]

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
  let by_id = db_with [ Entity { db_id = None; attrs = [ "ref+name", One_value (tuple_value [ Ref (eid 1L); String "Petr" ]); "age", One_value (Int64 32L) ] } ] db in
  expect_pull_attrs ~pattern:[ Pull_attr "age" ] "tuple lookup with id ref" by_id (Entity_id (eid 3L)) [ Keyword "age", scalar (Int64 32L) ];
  let by_lookup =
    db_with
      [ Entity
          { db_id = None
          ; attrs = [ "ref+name", One_value (Tuple [ Some (Vector [ Keyword "name"; String "Ivan" ]); Some (String "Petr") ]); "age", One_value (Int64 32L) ]
          }
      ]
      db
  in
  expect_pull_attrs ~pattern:[ Pull_attr "age" ] "tuple lookup with nested lookup ref" by_lookup (Entity_id (eid 3L)) [ Keyword "age", scalar (Int64 32L) ];
  if entid db "ref+name" (tuple_value [ Ref (eid 1L); String "Petr" ]) <> Some (eid 3L) then failf "tuple entid by id ref";
  if entid db "ref+name" (Vector [ Vector [ Keyword "name"; String "Ivan" ]; String "Petr" ]) <> Some (eid 3L) then failf "tuple entid by nested lookup ref"

let test_tuples__test_validation () =
  let db = empty_db ~schema:[ "a+b", tuple [ "a"; "b" ] ] () in
  let db1 = db_with [ Add (Entity_id (eid 1L), "a", String "a") ] db in
  assert_invalid "cannot add nil tuple directly" (fun () -> ignore (db_with [ Add (Entity_id (eid 1L), "a+b", tuple_opt [ None; None ]) ] db));
  assert_invalid "cannot add partial tuple directly" (fun () -> ignore (db_with [ Add (Entity_id (eid 1L), "a+b", tuple_opt [ Some (String "a"); None ]) ] db1));
  assert_invalid
    "cannot mix source and partial direct tuple"
    (fun () -> ignore (db_with [ Add (Entity_id (eid 1L), "a", String "a"); Add (Entity_id (eid 1L), "a+b", tuple_opt [ Some (String "a"); None ]) ] db));
  assert_invalid "cannot retract tuple directly" (fun () -> ignore (db_with [ Retract (Entity_id (eid 1L), "a+b", Some (tuple_opt [ Some (String "a"); None ])) ] db1))

let test_tuples__test_indexes () =
  let db =
    empty_db ~schema:[ "a+b+c", tuple [ "a"; "b"; "c" ] ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "a", One_value (String "a"); "b", One_value (String "b"); "c", One_value (String "c") ] }
         ; Entity { db_id = Some (Entity_id (eid 2L)); attrs = [ "a", One_value (String "A"); "b", One_value (String "b"); "c", One_value (String "c") ] }
         ; Entity { db_id = Some (Entity_id (eid 3L)); attrs = [ "a", One_value (String "a"); "b", One_value (String "B"); "c", One_value (String "c") ] }
         ; Entity { db_id = Some (Entity_id (eid 4L)); attrs = [ "a", One_value (String "A"); "b", One_value (String "B"); "c", One_value (String "c") ] }
         ; Entity { db_id = Some (Entity_id (eid 5L)); attrs = [ "a", One_value (String "a"); "b", One_value (String "b"); "c", One_value (String "C") ] }
         ; Entity { db_id = Some (Entity_id (eid 6L)); attrs = [ "a", One_value (String "A"); "b", One_value (String "b"); "c", One_value (String "C") ] }
         ; Entity { db_id = Some (Entity_id (eid 7L)); attrs = [ "a", One_value (String "a"); "b", One_value (String "B"); "c", One_value (String "C") ] }
         ; Entity { db_id = Some (Entity_id (eid 8L)); attrs = [ "a", One_value (String "A"); "b", One_value (String "B"); "c", One_value (String "C") ] }
         ]
  in
  if (datoms ~a:"a+b+c" ~v:(tuple_value [ String "A"; String "b"; String "C" ]) db Avet |> List.map (fun d -> d.e)) <> [ eid 6L ] then failf "tuple avet exact lookup";
  if datoms ~a:"a+b+c" ~v:(tuple_opt [ Some (String "A"); Some (String "b"); None ]) db Avet <> [] then failf "tuple avet exact lookup with nil";
  if (index_range ~start:(tuple_value [ String "A"; String "B"; String "C" ]) ~stop:(tuple_value [ String "A"; String "b"; String "c" ]) db "a+b+c" |> List.map (fun d -> d.e)) <> [ eid 8L; eid 4L; eid 6L; eid 2L ] then failf "tuple index range";
  if (index_range ~start:(tuple_opt [ Some (String "A"); Some (String "B"); None ]) ~stop:(tuple_opt [ Some (String "A"); Some (String "b"); None ]) db "a+b+c" |> List.map (fun d -> d.e)) <> [ eid 8L; eid 4L ] then failf "tuple index range with nil bounds"

let test_tuples__test_queries () =
  let db =
    empty_db ~schema:[ "a+b", tuple_unique_identity [ "a"; "b" ] ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ] }
         ; Entity { db_id = Some (Entity_id (eid 2L)); attrs = [ "a", One_value (String "A"); "b", One_value (String "b") ] }
         ; Entity { db_id = Some (Entity_id (eid 3L)); attrs = [ "a", One_value (String "a"); "b", One_value (String "B") ] }
         ; Entity { db_id = Some (Entity_id (eid 4L)); attrs = [ "a", One_value (String "a"); "b", One_value (String "b") ] }
         ]
  in
  assert_query_set
    "query tuple attr value"
    [ [ Result_entity (eid 3L) ] ]
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
