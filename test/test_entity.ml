open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms db index ?e ?a ?v ?tx () =
  datoms_seq db index ?e ?a ?v ?tx () |> List.of_seq

let assert_bool message value =
  if not value then failwith message

let assert_equal_int label expected actual =
  if expected <> actual then
    failf "%s: expected %d, got %d" label expected actual

let assert_equal_datoms label expected actual =
  if expected <> actual then
    failf "%s: unexpected datoms" label

let assert_equal_tx_value label expected actual =
  if expected <> actual then
    failf "%s: unexpected tx value" label

let assert_raises_invalid_arg label f =
  match f () with
  | exception Invalid_argument _ -> ()
  | exception exn -> failf "%s: expected Invalid_argument, got %s" label (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

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
  { cardinality = One
  ; unique = Some Identity
  ; indexed = true
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let ref_attr =
  { cardinality = One
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = Some RefType
  ; tuple_attrs = None
  ; tuple_types = None
  }

let ref_many = { ref_attr with cardinality = Many }

let component = { ref_attr with is_component = true }

let component_many = { component with cardinality = Many }

let test_entity__test_entity () =
  let db =
    empty_db ~schema:[ "aka", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "age", One_value (Int 19)
                 ; "aka", Many_values [ String "X"; String "Y" ]
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "sex", One_value (String "male")
                 ; "aka", Many_values [ String "Z" ]
                 ]
             }
         ; Add (Entity_id 3, "huh?", Bool false)
         ; Add (Entity_id 1, "name", String "Petr")
         ; Retract (Entity_id 1, "aka", Some (String "X"))
         ]
  in
  (match entity db (Entity_id 1) with
   | None -> failwith "expected entity 1"
   | Some entity ->
     assert_equal_int "entity id" 1 entity.id;
     assert_equal_tx_value
       "entity exposes db/id as a virtual attribute"
       (Some (One_value (Int 1)))
       (entity_attr entity "db/id");
     assert_equal_tx_value
       "entity reads current cardinality-one value"
       (Some (One_value (String "Petr")))
       (entity_attr entity "name");
     assert_equal_tx_value
       "entity reads ordinary scalar attrs"
       (Some (One_value (Int 19)))
       (entity_attr entity "age");
     assert_equal_tx_value
       "entity reads current cardinality-many values"
       (Some (Many_values [ String "Y" ]))
       (entity_attr entity "aka");
     assert_equal_tx_value "missing attributes return none" None (entity_attr entity "missing");
     let touched = touch entity in
     assert_equal_int "touch preserves entity id" 1 touched.id;
     assert_equal_datoms
       "entity_db returns the db that produced the entity"
       (datoms db Eavt ())
       (datoms (entity_db touched) Eavt ()));
  (match entity db (Entity_id 2) with
   | None -> failwith "expected entity 2"
   | Some entity ->
     assert_equal_tx_value
       "second entity reads its attr map"
       (Some (One_value (String "male")))
       (entity_attr entity "sex");
     assert_equal_tx_value
       "second entity reads many attrs"
       (Some (Many_values [ String "Z" ]))
       (entity_attr entity "aka"));
  match entity db (Entity_id 3) with
  | None -> failwith "expected entity 3"
  | Some entity ->
    assert_equal_tx_value
      "false entity attrs are preserved"
      (Some (One_value (Bool false)))
      (entity_attr entity "huh?")

let test_entity__test_entity_refs () =
  let db =
    empty_db ~schema:[ "father", ref_attr; "children", ref_many; "profile", component ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "children", Many_values [ Ref 10 ] ] }
         ; Entity { db_id = Some (Entity_id 10); attrs = [ "father", One_value (Ref 1); "children", Many_values [ Ref 100; Ref 101 ] ] }
         ; Entity { db_id = Some (Entity_id 100); attrs = [ "father", One_value (Ref 10) ] }
         ; Entity { db_id = Some (Entity_id 101); attrs = [ "father", One_value (Ref 10) ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "profile", One_value (Ref 10) ] }
         ]
  in
  let entity_or_fail entity_id =
    match entity db (Entity_id entity_id) with
    | Some entity -> entity
    | None -> failf "expected entity %d" entity_id
  in
  assert_equal_tx_value
    "cardinality-many refs navigate to target entities"
    (Some
       (Many_entities
          [ { db_id = Some (Entity_id 10)
            ; attrs = [ "children", Many_values [ Ref 100; Ref 101 ]; "father", One_value (Ref 1) ]
            }
          ]))
    (entity_attr (entity_or_fail 1) "children");
  assert_equal_tx_value
    "nested navigation reads child refs"
    (Some
       (Many_entities
          [ { db_id = Some (Entity_id 100); attrs = [ "father", One_value (Ref 10) ] }
          ; { db_id = Some (Entity_id 101); attrs = [ "father", One_value (Ref 10) ] }
          ]))
    (entity_attr (entity_or_fail 10) "children");
  assert_equal_tx_value
    "backward navigation uses reverse attrs"
    (Some
       (Many_entities
          [ { db_id = Some (Entity_id 10)
            ; attrs = [ "children", Many_values [ Ref 100; Ref 101 ]; "father", One_value (Ref 1) ]
            }
          ]))
    (entity_attr (entity_or_fail 1) "_father");
  assert_equal_tx_value
    "reverse component attrs navigate to the single owner"
    (Some (One_entity { db_id = Some (Entity_id 4); attrs = [ "profile", One_value (Ref 10) ] }))
    (entity_attr (entity_or_fail 10) "_profile");
  assert_equal_tx_value
    "namespaced reverse attrs preserve namespace"
    (Some (Many_entities [ { db_id = Some (Entity_id 1); attrs = [ "children", Many_values [ Ref 10 ] ] } ]))
    (entity_attr (entity_or_fail 10) "_children")

let test_entity__test_missing_refs () =
  let db =
    empty_db
      ~schema:
        [ "ref", ref_attr
        ; "comp", component
        ; "multiref", ref_many
        ; "multicomp", component_many
        ]
      ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 1, "ref", Ref 2)
         ; Add (Entity_id 1, "comp", Ref 3)
         ; Add (Entity_id 1, "multiref", Ref 4)
         ; Add (Entity_id 1, "multiref", Ref 7)
         ; Add (Entity_id 1, "multicomp", Ref 5)
         ; Add (Entity_id 1, "multicomp", Ref 6)
         ; Add (Entity_id 7, "name", String "Existing")
         ]
  in
  match entity db (Entity_id 1) with
  | None -> failwith "expected entity 1"
  | Some entity ->
    let _ = touch entity in
    assert_equal_tx_value "cardinality-one missing ref target is omitted" None (entity_attr entity "ref");
    assert_equal_tx_value "missing component target is omitted" None (entity_attr entity "comp");
    assert_equal_tx_value
      "cardinality-many refs keep only existing targets"
      (Some
         (Many_entities
            [ { db_id = Some (Entity_id 7); attrs = [ "name", One_value (String "Existing") ] }
            ]))
      (entity_attr entity "multiref");
    assert_equal_tx_value "cardinality-many missing component targets are omitted" None (entity_attr entity "multicomp")

let test_entity__test_entity_misses () =
  let db =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] } ]
  in
  if entity db (Entity_id 777) <> None then failwith "missing entity should return None";
  if entity db (Lookup_ref ("name", String "Petr")) <> None then failwith "missing lookup ref should return None";
  let reverse_only =
    empty_db ()
    |> db_with [ Add (Entity_id 1, "friend", Ref 2) ]
  in
  if entity reverse_only (Entity_id 2) <> None then
    failwith "incoming refs alone should not make an entity exist";
  assert_raises_invalid_arg
    "entity lookup refs require unique attrs like upstream"
    (fun () -> ignore (entity db (Lookup_ref ("not-an-attr", Int 777))))

let test_entity__test_entity_equality () =
  let db1 =
    empty_db ()
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] } ]
  in
  let entity_or_fail db =
    match entity db (Entity_id 1) with
    | Some entity -> entity
    | None -> failwith "expected entity"
  in
  let e1 = entity_or_fail db1 in
  let db2 = db_with [] db1 in
  let db3 = db_with [ Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Oleg") ] } ] db2 in
  assert_bool "entity_equal should be reflexive" (entity_equal e1 e1);
  assert_bool "entities from the same db and id should be equal" (entity_equal e1 (entity_or_fail db1));
  assert_bool "entities from different db values should not be equal" (not (entity_equal e1 (entity_or_fail db2)));
  assert_bool "entities from later db values should not be equal" (not (entity_equal e1 (entity_or_fail db3)))

let test_entity__test_entity_hash () =
  let db1 =
    empty_db ()
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] } ]
  in
  let entity_or_fail db =
    match entity db (Entity_id 1) with
    | Some entity -> entity
    | None -> failwith "expected entity"
  in
  let e1 = entity_or_fail db1 in
  let db2 = db_with [] db1 in
  let db3 = db_with [ Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Oleg") ] } ] db1 in
  assert_equal_int "same db/id entities should have the same entity_hash" (entity_hash e1) (entity_hash (entity_or_fail db1));
  assert_bool "different db values should produce different entity_hash values" (entity_hash e1 <> entity_hash (entity_or_fail db2));
  assert_bool "later db values should produce different entity_hash values" (entity_hash e1 <> entity_hash (entity_or_fail db3))

let () =
  test_entity__test_entity ();
  test_entity__test_entity_refs ();
  test_entity__test_missing_refs ();
  test_entity__test_entity_misses ();
  test_entity__test_entity_equality ();
  test_entity__test_entity_hash ()
