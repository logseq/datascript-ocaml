open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms ?e ?a ?v ?tx db index =
  datoms_seq ?e ?a ?v ?tx db index |> List.of_seq

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
  Schema.spec ~cardinality:(Many) ?unique:(None) ~indexed:(false) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let unique_identity =
  Schema.spec ~cardinality:(One) ?unique:(Some Identity) ~indexed:(true) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let ref_attr =
  Schema.spec ~cardinality:(One) ?unique:(None) ~indexed:(false) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(Some RefType) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let ref_many = Schema.spec ~cardinality:(Many) ?unique:((Schema.unique ref_attr)) ~indexed:((Schema.indexed ref_attr)) ~is_component:((Schema.is_component ref_attr)) ~no_history:((Schema.no_history ref_attr)) ?doc:((Schema.doc ref_attr)) ?value_type:((Schema.value_type ref_attr)) ?tuple:(match ((Schema.tuple_attrs ref_attr), (Schema.tuple_types ref_attr)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let component = Schema.spec ~cardinality:((Schema.cardinality ref_attr)) ?unique:((Schema.unique ref_attr)) ~indexed:((Schema.indexed ref_attr)) ~is_component:(true) ~no_history:((Schema.no_history ref_attr)) ?doc:((Schema.doc ref_attr)) ?value_type:((Schema.value_type ref_attr)) ?tuple:(match ((Schema.tuple_attrs ref_attr), (Schema.tuple_types ref_attr)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let component_many = Schema.spec ~cardinality:(Many) ?unique:((Schema.unique component)) ~indexed:((Schema.indexed component)) ~is_component:((Schema.is_component component)) ~no_history:((Schema.no_history component)) ?doc:((Schema.doc component)) ?value_type:((Schema.value_type component)) ?tuple:(match ((Schema.tuple_attrs component), (Schema.tuple_types component)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let test_entity__test_entity () =
  let db =
    empty_db ~schema:[ "aka", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id (eid 1L))
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "age", One_value (Int64 19L)
                 ; "aka", Many_values [ String "X"; String "Y" ]
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id (eid 2L))
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "sex", One_value (String "male")
                 ; "aka", Many_values [ String "Z" ]
                 ]
             }
         ; Add (Entity_id (eid 3L), "huh?", Bool false)
         ; Add (Entity_id (eid 1L), "name", String "Petr")
         ; Retract (Entity_id (eid 1L), "aka", Some (String "X"))
         ]
  in
  (match entity db (Entity_id (eid 1L)) with
   | None -> failwith "expected entity 1"
   | Some entity ->
     assert_equal_int "entity id" 1 (Entity_id.to_int (Entity.id entity));
     assert_equal_tx_value
       "entity exposes db/id as a virtual attribute"
       (Some (One_value (Int64 1L)))
       (entity_attr entity "db/id");
     assert_equal_tx_value
       "entity reads current cardinality-one value"
       (Some (One_value (String "Petr")))
       (entity_attr entity "name");
     assert_equal_tx_value
       "entity reads ordinary scalar attrs"
       (Some (One_value (Int64 19L)))
       (entity_attr entity "age");
     assert_equal_tx_value
       "entity reads current cardinality-many values"
       (Some (Many_values [ String "Y" ]))
       (entity_attr entity "aka");
     assert_equal_tx_value "missing attributes return none" None (entity_attr entity "missing");
     let touched = touch entity in
     assert_equal_int "touch preserves entity id" 1 (Entity_id.to_int (Entity.id touched));
     assert_equal_datoms
       "entity_db returns the db that produced the entity"
       (datoms db Eavt)
       (datoms (entity_db touched) Eavt));
  (match entity db (Entity_id (eid 2L)) with
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
  match entity db (Entity_id (eid 3L)) with
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
         [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "children", Many_values [ Ref (eid 10L) ] ] }
         ; Entity { db_id = Some (Entity_id (eid 10L)); attrs = [ "father", One_value (Ref (eid 1L)); "children", Many_values [ Ref (eid 100L); Ref (eid 101L) ] ] }
         ; Entity { db_id = Some (Entity_id (eid 100L)); attrs = [ "father", One_value (Ref (eid 10L)) ] }
         ; Entity { db_id = Some (Entity_id (eid 101L)); attrs = [ "father", One_value (Ref (eid 10L)) ] }
         ; Entity { db_id = Some (Entity_id (eid 4L)); attrs = [ "profile", One_value (Ref (eid 10L)) ] }
         ]
  in
  let entity_or_fail entity_id =
    match entity db (Entity_id entity_id) with
    | Some entity -> entity
    | None -> failf "expected entity %d" (Entity_id.to_int entity_id)
  in
  assert_equal_tx_value
    "cardinality-many refs navigate to target entities"
    (Some
       (Many_entities
          [ { db_id = Some (Entity_id (eid 10L))
            ; attrs = [ "children", Many_values [ Ref (eid 100L); Ref (eid 101L) ]; "father", One_value (Ref (eid 1L)) ]
            }
          ]))
    (entity_attr (entity_or_fail (eid 1L)) "children");
  assert_equal_tx_value
    "nested navigation reads child refs"
    (Some
       (Many_entities
          [ { db_id = Some (Entity_id (eid 100L)); attrs = [ "father", One_value (Ref (eid 10L)) ] }
          ; { db_id = Some (Entity_id (eid 101L)); attrs = [ "father", One_value (Ref (eid 10L)) ] }
          ]))
    (entity_attr (entity_or_fail (eid 10L)) "children");
  assert_equal_tx_value
    "backward navigation uses reverse attrs"
    (Some
       (Many_entities
          [ { db_id = Some (Entity_id (eid 10L))
            ; attrs = [ "children", Many_values [ Ref (eid 100L); Ref (eid 101L) ]; "father", One_value (Ref (eid 1L)) ]
            }
          ]))
    (entity_attr (entity_or_fail (eid 1L)) "_father");
  assert_equal_tx_value
    "reverse component attrs navigate to the single owner"
    (Some (One_entity { db_id = Some (Entity_id (eid 4L)); attrs = [ "profile", One_value (Ref (eid 10L)) ] }))
    (entity_attr (entity_or_fail (eid 10L)) "_profile");
  assert_equal_tx_value
    "namespaced reverse attrs preserve namespace"
    (Some (Many_entities [ { db_id = Some (Entity_id (eid 1L)); attrs = [ "children", Many_values [ Ref (eid 10L) ] ] } ]))
    (entity_attr (entity_or_fail (eid 10L)) "_children")

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
         [ Add (Entity_id (eid 1L), "name", String "Ivan")
         ; Add (Entity_id (eid 1L), "ref", Ref (eid 2L))
         ; Add (Entity_id (eid 1L), "comp", Ref (eid 3L))
         ; Add (Entity_id (eid 1L), "multiref", Ref (eid 4L))
         ; Add (Entity_id (eid 1L), "multiref", Ref (eid 7L))
         ; Add (Entity_id (eid 1L), "multicomp", Ref (eid 5L))
         ; Add (Entity_id (eid 1L), "multicomp", Ref (eid 6L))
         ; Add (Entity_id (eid 7L), "name", String "Existing")
         ]
  in
  match entity db (Entity_id (eid 1L)) with
  | None -> failwith "expected entity 1"
  | Some entity ->
    let _ = touch entity in
    assert_equal_tx_value "cardinality-one missing ref target is omitted" None (entity_attr entity "ref");
    assert_equal_tx_value "missing component target is omitted" None (entity_attr entity "comp");
    assert_equal_tx_value
      "cardinality-many refs keep only existing targets"
      (Some
         (Many_entities
            [ { db_id = Some (Entity_id (eid 7L)); attrs = [ "name", One_value (String "Existing") ] }
            ]))
      (entity_attr entity "multiref");
    assert_equal_tx_value "cardinality-many missing component targets are omitted" None (entity_attr entity "multicomp")

let test_entity__test_entity_misses () =
  let db =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "name", One_value (String "Ivan") ] } ]
  in
  if entity db (Entity_id (eid 777L)) <> None then failwith "missing entity should return None";
  if entity db (Lookup_ref ("name", String "Petr")) <> None then failwith "missing lookup ref should return None";
  let reverse_only =
    empty_db ()
    |> db_with [ Add (Entity_id (eid 1L), "friend", Ref (eid 2L)) ]
  in
  if entity reverse_only (Entity_id (eid 2L)) <> None then
    failwith "incoming refs alone should not make an entity exist";
  assert_raises_invalid_arg
    "entity lookup refs require unique attrs like upstream"
    (fun () -> ignore (entity db (Lookup_ref ("not-an-attr", Int64 777L))))

let test_entity__test_entity_equality () =
  let db1 =
    empty_db ()
    |> db_with [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "name", One_value (String "Ivan") ] } ]
  in
  let entity_or_fail db =
    match entity db (Entity_id (eid 1L)) with
    | Some entity -> entity
    | None -> failwith "expected entity"
  in
  let e1 = entity_or_fail db1 in
  let db2 = db_with [] db1 in
  let db3 = db_with [ Entity { db_id = Some (Entity_id (eid 2L)); attrs = [ "name", One_value (String "Oleg") ] } ] db2 in
  assert_bool "entity_equal should be reflexive" (entity_equal e1 e1);
  assert_bool "entities from the same db and id should be equal" (entity_equal e1 (entity_or_fail db1));
  assert_bool "entities from different db values should not be equal" (not (entity_equal e1 (entity_or_fail db2)));
  assert_bool "entities from later db values should not be equal" (not (entity_equal e1 (entity_or_fail db3)))

let test_entity__test_entity_hash () =
  let db1 =
    empty_db ()
    |> db_with [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "name", One_value (String "Ivan") ] } ]
  in
  let entity_or_fail db =
    match entity db (Entity_id (eid 1L)) with
    | Some entity -> entity
    | None -> failwith "expected entity"
  in
  let e1 = entity_or_fail db1 in
  let db2 = db_with [] db1 in
  let db3 = db_with [ Entity { db_id = Some (Entity_id (eid 2L)); attrs = [ "name", One_value (String "Oleg") ] } ] db1 in
  assert_equal_int "same db/id entities should have the same entity_hash" (entity_hash e1) (entity_hash (entity_or_fail db1));
  assert_bool "different db values should produce different entity_hash values" (entity_hash e1 <> entity_hash (entity_or_fail db2));
  assert_bool "later db values should produce different entity_hash values" (entity_hash e1 <> entity_hash (entity_or_fail db3))

let test_entity__test_entity_attr_lookup_is_lazy () =
  let module DT = Internal.Datascript_types in
  let dt_ref_attr =
    { DT.cardinality = DT.One; DT.unique = None; DT.indexed = false
    ; DT.is_component = false; DT.no_history = false; DT.doc = None
    ; DT.value_type = Some DT.RefType; DT.tuple_attrs = None; DT.tuple_types = None
    }
  in
  let db =
    Internal.empty_db ~schema:[ "friend", dt_ref_attr ] ()
    |> Internal.db_with
         (DT.Entity { DT.db_id = Some (DT.Entity_id 1L); DT.attrs = [ "name", DT.One_value (DT.String "Ivan") ] }
          :: List.init 5000 (fun index ->
            DT.Add (DT.Entity_id (Int64.of_int (index + 2)), "friend", DT.Ref 1L)))
  in
  let all_datoms_calls = ref 0 in
  let schema_attr db attr = List.assoc_opt attr (Internal.schema db) in
  let cardinality db attr =
    match schema_attr db attr with
    | Some (spec : DT.schema_attr) -> spec.cardinality
    | None -> DT.One
  in
  let is_ref_attr db attr =
    match schema_attr db attr with
    | Some (spec : DT.schema_attr) -> spec.value_type = Some DT.RefType
    | _ -> false
  in
  let is_component db attr =
    match schema_attr db attr with
    | Some (spec : DT.schema_attr) -> spec.is_component
    | _ -> false
  in
  let entity_id_of_ref db = function
    | DT.Entity_id entity_id ->
      if Seq.is_empty (Internal.datoms db Eavt ~e:entity_id ()) then None else Some entity_id
    | _ -> None
  in
  let context : Internal.Entity.context =
    { datoms_by_entity = (fun db entity_id -> Internal.datoms db Eavt ~e:entity_id ())
    ; datoms_by_avet_ref = (fun db attr entity_id -> Internal.datoms db Avet ~a:attr ~v:(DT.Ref entity_id) ())
    ; all_datoms =
        (fun db ->
          incr all_datoms_calls;
          Internal.datoms db Eavt ())
    ; compare_value = Internal.Util.compare_value
    ; cardinality
    ; is_ref_attr
    ; is_component
    ; reverse_ref = Internal.reverse_ref
    ; is_reverse_ref = Internal.is_reverse_ref
    ; entity_id_of_ref
    }
  in
  let entity =
    match Internal.Entity.entity context db (DT.Entity_id 1L) with
    | Some entity -> entity
    | None -> failwith "expected entity"
  in
  assert_equal_int "constructing an entity should not scan all datoms" 0 !all_datoms_calls;
  assert_equal_tx_value
    "forward attr lookup should not materialize reverse attrs"
    (Some (DT.One_value (DT.String "Ivan")))
    (Internal.Entity.entity_attr context entity "name");
  assert_equal_int "forward attr lookup should still avoid all datoms" 0 !all_datoms_calls;
  ignore (Internal.Entity.entity_attr context entity "_friend");
  assert_equal_int "reverse attr lookup should use AVET instead of all datoms" 0 !all_datoms_calls;
  ignore (Internal.Entity.entity_attrs entity);
  assert_equal_int "full entity materialization only reads the entity's own datoms" 0 !all_datoms_calls

let () =
  test_entity__test_entity ();
  test_entity__test_entity_refs ();
  test_entity__test_missing_refs ();
  test_entity__test_entity_misses ();
  test_entity__test_entity_equality ();
  test_entity__test_entity_hash ();
  test_entity__test_entity_attr_lookup_is_lazy ()
