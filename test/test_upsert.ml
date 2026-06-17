open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_triples label expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v) actual in
  if expected <> actual then failf "%s: unexpected datoms" label

let assert_equal_tempids label expected actual =
  let normalize tempids =
    tempids
    |> List.filter (fun (tempid, _) -> tempid <> "db/current-tx")
    |> List.sort compare
  in
  if normalize expected <> normalize actual then failf "%s: unexpected tempids" label

let assert_raises_invalid_arg label f =
  match f () with
  | exception Invalid_argument _ -> ()
  | exception exn -> failf "%s: expected Invalid_argument, got %s" label (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

let assert_raises_invalid_arg_message label expected f =
  match f () with
  | exception Invalid_argument message when message = expected -> ()
  | exception Invalid_argument message ->
    failf "%s: expected Invalid_argument(%S), got Invalid_argument(%S)" label expected message
  | exception exn -> failf "%s: expected Invalid_argument, got %s" label (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

let indexed =
  { cardinality = One
  ; unique = None
  ; indexed = true
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let unique_identity = { indexed with unique = Some Identity }
let ref_attr = { indexed with indexed = false; value_type = Some RefType }
let ref_unique_identity = { unique_identity with value_type = Some RefType }

let unique_many_identity =
  { unique_identity with cardinality = Many }

let entity ?db_id attrs = Entity { db_id; attrs }
let one attr value = attr, One_value value
let many attr values = attr, Many_values values

let base_schema =
  [ "name", unique_identity
  ; "email", unique_identity
  ; "slugs", unique_many_identity
  ; "ref", ref_unique_identity
  ]

let base_db () =
  empty_db ~schema:base_schema ()
  |> db_with
       [ entity
           ~db_id:(Entity_id 1)
           [ one "name" (String "Ivan")
           ; one "email" (String "@1")
           ]
       ; entity
           ~db_id:(Entity_id 2)
           [ one "name" (String "Petr")
           ; one "email" (String "@2")
           ; one "ref" (Ref 3)
           ]
       ; entity
           ~db_id:(Entity_id 3)
           [ one "name" (String "Dima")
           ; one "email" (String "@3")
           ; one "ref" (Ref 4)
           ]
       ; entity
           ~db_id:(Entity_id 4)
           [ one "name" (String "Olga")
           ; one "email" (String "@4")
           ; one "ref" (Ref 1)
           ]
       ]

let assert_entity label expected db entity_id =
  assert_equal_triples label expected (datoms db Eavt ~e:entity_id ())

let test_upsert__test_upsert () =
  let db = base_db () in
  let tx = transact db [ entity [ one "name" (String "Ivan"); one "age" (Int 35) ] ] in
  assert_entity
    "upsert, no tempid"
    [ 1, "age", Int 35; 1, "email", String "@1"; 1, "name", String "Ivan" ]
    tx.db_after
    1;
  assert_equal_tempids "upsert, no tempid has no user tempids" [] tx.tempids;
  let tx =
    transact db [ entity [ one "name" (String "Ivan"); one "email" (String "@1"); one "age" (Int 35) ] ]
  in
  assert_entity
    "upsert by 2 attrs, no tempid"
    [ 1, "age", Int 35; 1, "email", String "@1"; 1, "name", String "Ivan" ]
    tx.db_after
    1;
  assert_equal_tempids "upsert by 2 attrs, no tempid has no user tempids" [] tx.tempids;
  let tx =
    transact db [ entity ~db_id:(Temp_id "-1") [ one "name" (String "Ivan"); one "age" (Int 35) ] ]
  in
  assert_entity
    "upsert with tempid"
    [ 1, "age", Int 35; 1, "email", String "@1"; 1, "name", String "Ivan" ]
    tx.db_after
    1;
  assert_equal_tempids "upsert with tempid resolves existing entity" [ "-1", 1 ] tx.tempids;
  let tx =
    transact
      db
      [ entity ~db_id:(Temp_id "1") [ one "name" (String "Ivan"); one "age" (Int 35) ]
      ; Add (Temp_id "2", "name", String "Oleg")
      ; Add (Temp_id "2", "email", String "@2")
      ]
  in
  assert_entity
    "upsert with string tempid updates first entity"
    [ 1, "age", Int 35; 1, "email", String "@1"; 1, "name", String "Ivan" ]
    tx.db_after
    1;
  assert_entity
    "upsert with string tempid merges later add ops"
    [ 2, "email", String "@2"; 2, "name", String "Oleg"; 2, "ref", Ref 3 ]
    tx.db_after
    2;
  assert_equal_tempids "upsert with string tempid resolves both ids" [ "1", 1; "2", 2 ] tx.tempids;
  let tx =
    transact
      db
      [ entity ~db_id:(Temp_id "-1") [ one "name" (String "Ivan"); one "email" (String "@1"); one "age" (Int 35) ] ]
  in
  assert_entity
    "upsert by 2 attrs with tempid"
    [ 1, "age", Int 35; 1, "email", String "@1"; 1, "name", String "Ivan" ]
    tx.db_after
    1;
  assert_equal_tempids "upsert by 2 attrs with tempid resolves existing entity" [ "-1", 1 ] tx.tempids;
  let tx =
    transact
      db
      [ entity ~db_id:(Temp_id "-1") [ one "name" (String "Ivan"); one "age" (Int 35) ]
      ; entity ~db_id:(Temp_id "-1") [ one "name" (String "Ivan"); one "age" (Int 36) ]
      ]
  in
  assert_entity
    "upsert to two entities, resolve to same tempid"
    [ 1, "age", Int 36; 1, "email", String "@1"; 1, "name", String "Ivan" ]
    tx.db_after
    1;
  assert_equal_tempids "same tempid remains resolved to one entity" [ "-1", 1 ] tx.tempids;
  let tx =
    transact
      db
      [ entity ~db_id:(Temp_id "-1") [ one "name" (String "Ivan"); one "age" (Int 35) ]
      ; entity ~db_id:(Temp_id "-2") [ one "name" (String "Ivan"); one "age" (Int 36) ]
      ]
  in
  assert_entity
    "upsert to two entities, two tempids"
    [ 1, "age", Int 36; 1, "email", String "@1"; 1, "name", String "Ivan" ]
    tx.db_after
    1;
  assert_equal_tempids "two tempids resolve to the same existing entity" [ "-1", 1; "-2", 1 ] tx.tempids;
  ignore (transact db [ entity ~db_id:(Entity_id 1) [ one "name" (String "Ivan"); one "age" (Int 35) ] ]);
  ignore
    (transact
       db
       [ entity
           ~db_id:(Lookup_ref ("name", String "Ivan"))
           [ one "name" (String "Ivan")
           ; one "email" (String "@1")
           ; one "age" (Int 35)
           ]
       ]);
  assert_raises_invalid_arg_message
    "upsert conflicts with existing id"
    "Conflicting upsert: [:name \"Ivan\"] resolves to 1, but entity already has :db/id 2"
    (fun () ->
      ignore (transact db [ entity ~db_id:(Entity_id 2) [ one "name" (String "Ivan"); one "age" (Int 36) ] ]));
  assert_raises_invalid_arg_message
    "upsert conflicts with non-existing id"
    "Conflicting upsert: [:name \"Ivan\"] resolves to 1, but entity already has :db/id 5"
    (fun () ->
      ignore (transact db [ entity ~db_id:(Entity_id 5) [ one "name" (String "Ivan"); one "age" (Int 36) ] ]));
  let tx =
    transact db [ entity [ one "name" (String "Ivan"); one "email" (String "@5"); one "age" (Int 35) ] ]
  in
  assert_entity
    "upsert by non-existing value resolves as update"
    [ 1, "age", Int 35; 1, "email", String "@5"; 1, "name", String "Ivan" ]
    tx.db_after
    1;
  assert_equal_tempids "upsert by non-existing value has no user tempids" [] tx.tempids;
  assert_raises_invalid_arg_message
    "upsert by 2 conflicting fields"
    "Conflicting upserts: [:name \"Ivan\"] resolves to 1, but [:email \"@2\"] resolves to 2"
    (fun () ->
      ignore (transact db [ entity [ one "name" (String "Ivan"); one "email" (String "@2"); one "age" (Int 35) ] ]));
  let tx =
    transact db [ entity [ one "name" (String "Igor"); one "age" (Int 35) ]; entity [ one "name" (String "Igor"); one "age" (Int 36) ] ]
  in
  assert_entity "upsert over intermediate db" [ 5, "age", Int 36; 5, "name", String "Igor" ] tx.db_after 5;
  let tx =
    transact
      db
      [ entity ~db_id:(Temp_id "-1") [ one "name" (String "Igor"); one "age" (Int 35) ]
      ; entity ~db_id:(Temp_id "-2") [ one "name" (String "Igor"); one "age" (Int 36) ]
      ]
  in
  assert_entity "upsert over intermediate db, different tempids" [ 5, "age", Int 36; 5, "name", String "Igor" ] tx.db_after 5;
  assert_equal_tempids "intermediate upsert tempids resolve together" [ "-1", 5; "-2", 5 ] tx.tempids;
  assert_raises_invalid_arg
    "upsert and current-tx conflict"
    (fun () ->
      ignore (transact db [ entity ~db_id:CurrentTx [ one "name" (String "Ivan"); one "age" (Int 35) ] ]));
  let tx =
    transact
      db
      [ entity [ one "name" (String "Ivan"); one "slugs" (String "ivan1") ]
      ; entity [ one "name" (String "Petr"); one "slugs" (String "petr1") ]
      ]
  in
  let tx2 =
    transact tx.db_after [ entity [ one "name" (String "Ivan"); many "slugs" [ String "ivan1"; String "ivan2" ] ] ]
  in
  assert_entity
    "upsert of unique, cardinality-many values"
    [ 1, "email", String "@1"; 1, "name", String "Ivan"; 1, "slugs", String "ivan1" ]
    tx.db_after
    1;
  assert_entity
    "upsert extends unique cardinality-many values"
    [ 1, "email", String "@1"; 1, "name", String "Ivan"; 1, "slugs", String "ivan1"; 1, "slugs", String "ivan2" ]
    tx2.db_after
    1;
  assert_raises_invalid_arg
    "conflicting unique cardinality-many upserts are rejected"
    (fun () ->
      ignore (transact tx.db_after [ entity [ many "slugs" [ String "ivan1"; String "petr1" ] ] ]));
  [ 3, 2, 36; 4, 3, 37; 1, 4, 38 ]
  |> List.iter (fun (ref_e, target_e, age) ->
    let tx = transact db [ entity [ one "ref" (Ref ref_e); one "age" (Int age) ] ] in
    assert_entity
      "upsert by ref"
      [ target_e, "age", Int age
      ; target_e, "email", String (if target_e = 2 then "@2" else if target_e = 3 then "@3" else "@4")
      ; target_e, "name", String (if target_e = 2 then "Petr" else if target_e = 3 then "Dima" else "Olga")
      ; target_e, "ref", Ref ref_e
      ]
      tx.db_after
      target_e);
  [ "Dima", 2, 3, 36; "Olga", 3, 4, 37; "Ivan", 4, 1, 38 ]
  |> List.iter (fun (lookup_name, target_e, ref_e, age) ->
    let tx =
      transact db [ entity [ one "ref" (Ref_to (Lookup_ref ("name", String lookup_name))); one "age" (Int age) ] ]
    in
    assert_entity
      "upsert by lookup ref"
      [ target_e, "age", Int age
      ; target_e, "email", String (if target_e = 2 then "@2" else if target_e = 3 then "@3" else "@4")
      ; target_e, "name", String (if target_e = 2 then "Petr" else if target_e = 3 then "Dima" else "Olga")
      ; target_e, "ref", Ref ref_e
      ]
      tx.db_after
      target_e);
  let tx =
    transact
      db
      [ entity ~db_id:(Temp_id "-1") [ one "name" (String "Igor") ]
      ; entity ~db_id:(Temp_id "-2") [ one "name" (String "Anna"); one "ref" (Ref_to (Temp_id "-1")) ]
      ]
  in
  assert_entity "not upsert by ref target" [ 5, "name", String "Igor" ] tx.db_after 5;
  assert_entity "not upsert by ref source" [ 6, "name", String "Anna"; 6, "ref", Ref 5 ] tx.db_after 6

let test_upsert__test_redefining_ids () =
  let db =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with [ entity ~db_id:(Temp_id "-1") [ one "name" (String "Ivan") ] ]
  in
  let tx =
    transact
      db
      [ entity ~db_id:(Temp_id "-1") [ one "age" (Int 35) ]
      ; entity ~db_id:(Temp_id "-1") [ one "name" (String "Ivan"); one "age" (Int 36) ]
      ]
  in
  assert_equal_triples
    "redefining ids keeps the upsert target"
    [ 1, "age", Int 36; 1, "name", String "Ivan" ]
    (datoms tx.db_after Eavt ());
  assert_equal_tempids "redefined tempid resolves to existing entity" [ "-1", 1 ] tx.tempids;
  let db =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with
         [ entity ~db_id:(Temp_id "-1") [ one "name" (String "Ivan") ]
         ; entity ~db_id:(Temp_id "-2") [ one "name" (String "Oleg") ]
         ]
  in
  assert_raises_invalid_arg_message
    "one tempid cannot resolve to two upsert targets"
    "Conflicting upsert: -1 resolves both to 1 and 2"
    (fun () ->
      ignore
        (transact
           db
           [ entity ~db_id:(Temp_id "-1") [ one "name" (String "Ivan"); one "age" (Int 35) ]
           ; entity ~db_id:(Temp_id "-1") [ one "name" (String "Oleg"); one "age" (Int 36) ]
           ]))

let test_upsert__test_retries_order () =
  let first =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with
         [ Add (Temp_id "-1", "age", Int 42)
         ; Add (Temp_id "-2", "likes", String "Pizza")
         ; Add (Temp_id "-1", "name", String "Bob")
         ; Add (Temp_id "-2", "name", String "Bob")
         ]
  in
  assert_equal_triples
    "retry order merges later tempid into the first upsert target"
    [ 1, "age", Int 42; 1, "likes", String "Pizza"; 1, "name", String "Bob" ]
    (datoms first Eavt ());
  let second =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with
         [ Add (Temp_id "-1", "age", Int 42)
         ; Add (Temp_id "-2", "likes", String "Pizza")
         ; Add (Temp_id "-2", "name", String "Bob")
         ; Add (Temp_id "-1", "name", String "Bob")
         ]
  in
  assert_equal_triples
    "retry order preserves the first unique identity owner"
    [ 2, "age", Int 42; 2, "likes", String "Pizza"; 2, "name", String "Bob" ]
    (datoms second Eavt ())

let test_upsert__test_upsert_string_tempid_ref () =
  let db =
    empty_db ~schema:[ "name", unique_identity; "ref", ref_attr ] ()
    |> db_with [ entity [ one "name" (String "Alice") ] ]
  in
  let expected = [ 1, "name", String "Alice"; 2, "age", Int 36; 2, "ref", Ref 1 ] in
  [ [ entity ~db_id:(Temp_id "user") [ one "name" (String "Alice") ]
    ; entity [ one "age" (Int 36); one "ref" (Ref_to (Temp_id "user")) ]
    ]
  ; [ Add (Temp_id "user", "name", String "Alice")
    ; entity [ one "age" (Int 36); one "ref" (Ref_to (Temp_id "user")) ]
    ]
  ; [ entity ~db_id:(Temp_id "-1") [ one "name" (String "Alice") ]
    ; entity [ one "age" (Int 36); one "ref" (Ref_to (Temp_id "-1")) ]
    ]
  ; [ Add (Temp_id "-1", "name", String "Alice")
    ; entity [ one "age" (Int 36); one "ref" (Ref_to (Temp_id "-1")) ]
    ]
  ]
  |> List.iter (fun tx ->
    assert_equal_triples
      "upserted string tempid refs remap later refs"
      expected
      (datoms (db_with tx db) Eavt ()))

let test_upsert__test_two_tempids_two_retries () =
  let db =
    empty_db ~schema:[ "name", unique_identity; "ref", ref_attr ] ()
    |> db_with [ entity [ one "name" (String "Alice") ]; entity [ one "name" (String "Bob") ] ]
  in
  let expected = [ 1, "name", String "Alice"; 2, "name", String "Bob"; 3, "ref", Ref 1; 4, "ref", Ref 2 ] in
  let actual =
    db_with
      [ entity ~db_id:(Entity_id 3) [ one "ref" (Ref_to (Temp_id "A")) ]
      ; entity ~db_id:(Entity_id 4) [ one "ref" (Ref_to (Temp_id "B")) ]
      ; entity ~db_id:(Temp_id "A") [ one "name" (String "Alice") ]
      ; entity ~db_id:(Temp_id "B") [ one "name" (String "Bob") ]
      ]
      db
  in
  assert_equal_triples "two tempids retry without clobbering explicit ids" expected (datoms actual Eavt ())

let test_upsert__test_vector_upsert () =
  let db =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with [ entity ~db_id:(Temp_id "-1") [ one "name" (String "Ivan") ] ]
  in
  [ [ Add (Temp_id "-1", "name", String "Ivan"); Add (Temp_id "-1", "age", Int 12) ]
  ; [ Add (Temp_id "-1", "age", Int 12); Add (Temp_id "-1", "name", String "Ivan") ]
  ]
  |> List.iter (fun tx ->
    assert_equal_triples
      "vector add upsert resolves tempid into existing unique identity entity"
      [ 1, "age", Int 12; 1, "name", String "Ivan" ]
      (datoms (db_with tx db) Eavt ()));
  let db =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with
         [ Add (Temp_id "-1", "name", String "Ivan")
         ; Add (Temp_id "-2", "name", String "Oleg")
         ]
  in
  assert_raises_invalid_arg_message
    "vector add upsert rejects one tempid resolving to two targets"
    "Conflicting upsert: -1 resolves both to 1 and 2"
    (fun () ->
      ignore
        (transact
           db
           [ Add (Temp_id "-1", "name", String "Ivan")
           ; Add (Temp_id "-1", "age", Int 35)
           ; Add (Temp_id "-1", "name", String "Oleg")
           ; Add (Temp_id "-1", "age", Int 36)
           ]))

let () =
  test_upsert__test_upsert ();
  test_upsert__test_redefining_ids ();
  test_upsert__test_retries_order ();
  test_upsert__test_upsert_string_tempid_ref ();
  test_upsert__test_two_tempids_two_retries ();
  test_upsert__test_vector_upsert ()
