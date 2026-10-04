open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms ?e ?a ?v ?tx db index =
  datoms_seq ?e ?a ?v ?tx db index |> List.of_seq

let datoms_ref_seq = datoms_ref

let datoms_ref ?e ?a ?v ?tx db index =
  datoms_ref_seq ?e ?a ?v ?tx db index |> List.of_seq

let seek_datoms_ref_seq = seek_datoms_ref
let seek_datoms_ref ?e ?a ?v ?tx db index =
  seek_datoms_ref_seq ?e ?a ?v ?tx db index |> List.of_seq

let index_range_seq = index_range
let index_range ?start ?stop db attr =
  index_range_seq ?start ?stop db attr |> List.of_seq

let assert_equal_triples label expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v) actual in
  if expected <> actual then
    let value = function
      | Int64 value -> Int64.to_string value
      | String value -> Printf.sprintf "%S" value
      | Ref value -> "Ref " ^ Int64.to_string (Entity_id.to_int64 value)
      | other -> Printf.sprintf "%d" (Hashtbl.hash other)
    in
    let triples triples =
      triples
      |> List.map (fun (e, a, v) -> Printf.sprintf "(%d, %s, %s)" (Entity_id.to_int e) a (value v))
      |> String.concat "; "
    in
    failf "%s: expected [%s], got [%s]" label (triples expected) (triples actual)

let assert_equal_query_set label expected actual =
  let normalize rows = List.sort_uniq compare rows in
  if normalize expected <> normalize actual then failf "%s: unexpected query result" label

let assert_raises_invalid_arg_message label expected f =
  match f () with
  | exception Invalid_argument message when message = expected -> ()
  | exception Invalid_argument message ->
    failf "%s: expected Invalid_argument(%S), got Invalid_argument(%S)" label expected message
  | exception exn -> failf "%s: expected Invalid_argument, got %s" label (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

let indexed =
  Schema.spec ~cardinality:(One) ?unique:(None) ~indexed:(true) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let unique_identity = Schema.spec ~cardinality:((Schema.cardinality indexed)) ?unique:(Some Identity) ~indexed:((Schema.indexed indexed)) ~is_component:((Schema.is_component indexed)) ~no_history:((Schema.no_history indexed)) ?doc:((Schema.doc indexed)) ?value_type:((Schema.value_type indexed)) ?tuple:(match ((Schema.tuple_attrs indexed), (Schema.tuple_types indexed)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()
let unique_value = Schema.spec ~cardinality:((Schema.cardinality indexed)) ?unique:(Some Value) ~indexed:((Schema.indexed indexed)) ~is_component:((Schema.is_component indexed)) ~no_history:((Schema.no_history indexed)) ?doc:((Schema.doc indexed)) ?value_type:((Schema.value_type indexed)) ?tuple:(match ((Schema.tuple_attrs indexed), (Schema.tuple_types indexed)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()
let ref_attr = Schema.spec ~cardinality:((Schema.cardinality indexed)) ?unique:((Schema.unique indexed)) ~indexed:(false) ~is_component:((Schema.is_component indexed)) ~no_history:((Schema.no_history indexed)) ?doc:((Schema.doc indexed)) ?value_type:(Some RefType) ?tuple:(match ((Schema.tuple_attrs indexed), (Schema.tuple_types indexed)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()
let ref_many = Schema.spec ~cardinality:(Many) ?unique:((Schema.unique ref_attr)) ~indexed:((Schema.indexed ref_attr)) ~is_component:((Schema.is_component ref_attr)) ~no_history:((Schema.no_history ref_attr)) ?doc:((Schema.doc ref_attr)) ?value_type:((Schema.value_type ref_attr)) ?tuple:(match ((Schema.tuple_attrs ref_attr), (Schema.tuple_types ref_attr)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let tx_entity ?db_id attrs = Entity { db_id; attrs }
let one attr value = attr, One_value value
let many attr values = attr, Many_values values

let lookup_base () =
  empty_db ~schema:[ "name", unique_identity; "email", unique_value; "age", indexed ] ()
  |> db_with
       [ tx_entity
           ~db_id:(Entity_id (eid 1L))
           [ one "name" (String "Ivan")
           ; one "email" (String "@1")
           ; one "age" (Int64 35L)
           ]
       ; tx_entity
           ~db_id:(Entity_id (eid 2L))
           [ one "name" (String "Petr")
           ; one "email" (String "@2")
           ; one "age" (Int64 22L)
           ]
       ]

let assert_entity label expected db entity_ref =
  match entity db entity_ref with
  | None when expected = [] -> ()
  | None -> failf "%s: expected entity" label
  | Some entity ->
    assert_equal_triples label expected (datoms ~e:(Entity.id entity) db Eavt)

let test_lookup_refs__test_lookup_refs () =
  let db = lookup_base () in
  assert_entity
    "lookup ref resolves unique identity attr"
    [ (eid 1L), "age", Int64 35L; (eid 1L), "email", String "@1"; (eid 1L), "name", String "Ivan" ]
    db
    (Lookup_ref ("name", String "Ivan"));
  assert_entity
    "lookup ref resolves unique value attr"
    [ (eid 1L), "age", Int64 35L; (eid 1L), "email", String "@1"; (eid 1L), "name", String "Ivan" ]
    db
    (Lookup_ref ("email", String "@1"));
  assert_entity "missing lookup ref returns no entity" [] db (Lookup_ref ("name", String "Sergey"));
  assert_entity "nil lookup ref value returns no entity" [] db (Lookup_ref ("name", Nil));
  assert_raises_invalid_arg_message
    "lookup ref rejects non-unique attrs"
    "Lookup ref attribute should be marked as :db/unique: [:age 10]"
    (fun () -> ignore (entity db (Lookup_ref ("age", Int64 10L))))

let transact_base () =
  empty_db ~schema:[ "name", unique_identity; "friend", ref_attr; "friends", ref_many; "age", indexed ] ()
  |> db_with
       [ tx_entity ~db_id:(Entity_id (eid 1L)) [ one "name" (String "Ivan") ]
       ; tx_entity ~db_id:(Entity_id (eid 2L)) [ one "name" (String "Petr") ]
       ; tx_entity ~db_id:(Entity_id (eid 3L)) [ one "name" (String "Oleg") ]
       ; tx_entity ~db_id:(Entity_id (eid 4L)) [ one "name" (String "Sergey") ]
       ]

let test_lookup_refs__test_lookup_refs_transact () =
  let db = transact_base () in
  let ivan = Lookup_ref ("name", String "Ivan") in
  let petr = Lookup_ref ("name", String "Petr") in
  let oleg = Lookup_ref ("name", String "Oleg") in
  let db =
    db
    |> db_with [ Add (ivan, "age", Int64 35L) ]
    |> db_with [ tx_entity ~db_id:ivan [ one "age" (Int64 36L) ] ]
    |> db_with [ Add (Entity_id (eid 1L), "friend", Ref_to petr) ]
    |> db_with [ tx_entity ~db_id:(Entity_id (eid 1L)) [ one "friend" (Ref_to oleg) ] ]
    |> db_with [ tx_entity ~db_id:(Entity_id (eid 2L)) [ one "_friend" (Ref_to ivan) ] ]
  in
  assert_equal_triples
    "lookup refs transact through add and entity maps"
    [ (eid 1L), "age", Int64 36L
    ; (eid 1L), "friend", Ref (eid 2L)
    ; (eid 1L), "name", String "Ivan"
    ; (eid 2L), "name", String "Petr"
    ; (eid 3L), "name", String "Oleg"
    ; (eid 4L), "name", String "Sergey"
    ]
    (datoms db Eavt);
  let db =
    db
    |> db_with [ Add (Entity_id (eid 3L), "name", String "Oleg") ]
    |> db_with [ Add (Entity_id (eid 1L), "friend", Ref_to oleg) ]
    |> db_with [ CompareAndSet (ivan, "name", Some (String "Ivan"), String "Vanya") ]
    |> db_with [ CompareAndSet (Entity_id (eid 1L), "friend", Some (Ref_to oleg), Ref_to (Lookup_ref ("name", String "Sergey"))) ]
    |> db_with [ Retract (Lookup_ref ("name", String "Vanya"), "age", Some (Int64 36L)) ]
    |> db_with [ RetractAttr (Lookup_ref ("name", String "Vanya"), "friend") ]
  in
  assert_equal_triples
    "lookup refs transact through CAS and retractions"
    [ (eid 1L), "name", String "Vanya"
    ; (eid 2L), "name", String "Petr"
    ; (eid 3L), "name", String "Oleg"
    ; (eid 4L), "name", String "Sergey"
    ]
    (datoms db Eavt);
  let retracted = db_with [ RetractEntity (Lookup_ref ("name", String "Vanya")) ] db in
  assert_entity "lookup refs can retract an entity" [] retracted (Lookup_ref ("name", String "Vanya"));
  assert_raises_invalid_arg_message
    "lookup refs in add entity position must resolve"
    "Nothing found for entity id [:name \"Missing\"]"
    (fun () -> ignore (db_with [ Add (Lookup_ref ("name", String "Missing"), "age", Int64 10L) ] (transact_base ())));
  assert_raises_invalid_arg_message
    "lookup refs in entity-map db/id must resolve"
    "Nothing found for entity id [:name \"Missing\"]"
    (fun () ->
      ignore (db_with [ tx_entity ~db_id:(Lookup_ref ("name", String "Missing")) [ one "age" (Int64 10L) ] ] (transact_base ())))

let test_lookup_refs__test_lookup_refs_transact_multi () =
  let db = transact_base () in
  let petr = Lookup_ref ("name", String "Petr") in
  let oleg = Lookup_ref ("name", String "Oleg") in
  let db =
    db
    |> db_with [ Add (Entity_id (eid 1L), "friends", Ref_to petr) ]
    |> db_with [ Add (Entity_id (eid 1L), "friends", Ref_to oleg) ]
    |> db_with [ tx_entity ~db_id:(Entity_id (eid 2L)) [ many "_friends" [ Ref_to (Lookup_ref ("name", String "Ivan")); Ref_to oleg ] ] ]
  in
  assert_equal_triples
    "lookup refs transact through cardinality-many ref attrs and reverse attrs"
    [ (eid 1L), "friends", Ref (eid 2L)
    ; (eid 1L), "friends", Ref (eid 3L)
    ; (eid 1L), "name", String "Ivan"
    ; (eid 2L), "name", String "Petr"
    ; (eid 3L), "friends", Ref (eid 2L)
    ; (eid 3L), "name", String "Oleg"
    ; (eid 4L), "name", String "Sergey"
    ]
    (datoms db Eavt);
  let mapped =
    transact_base ()
    |> db_with [ tx_entity ~db_id:(Entity_id (eid 1L)) [ many "friends" [ Ref_to petr; Ref_to oleg ] ] ]
  in
  assert_equal_triples
    "entity maps accept lookup refs in many ref values"
    [ (eid 1L), "friends", Ref (eid 2L)
    ; (eid 1L), "friends", Ref (eid 3L)
    ; (eid 1L), "name", String "Ivan"
    ; (eid 2L), "name", String "Petr"
    ; (eid 3L), "name", String "Oleg"
    ; (eid 4L), "name", String "Sergey"
    ]
    (datoms mapped Eavt)

let index_base () =
  empty_db ~schema:[ "name", unique_identity; "friends", ref_many ] ()
  |> db_with
       [ tx_entity ~db_id:(Entity_id (eid 1L)) [ one "name" (String "Ivan"); many "friends" [ Ref (eid 2L); Ref (eid 3L) ] ]
       ; tx_entity ~db_id:(Entity_id (eid 2L)) [ one "name" (String "Petr"); one "friends" (Ref (eid 3L)) ]
       ; tx_entity ~db_id:(Entity_id (eid 3L)) [ one "name" (String "Oleg") ]
       ]

let test_lookup_refs__lookup_refs_index_access () =
  let db = index_base () in
  assert_equal_triples
    "datoms resolves lookup refs in EAVT entity position"
    [ (eid 1L), "friends", Ref (eid 2L); (eid 1L), "friends", Ref (eid 3L); (eid 1L), "name", String "Ivan" ]
    (datoms_ref ~e:(Lookup_ref ("name", String "Ivan")) db Eavt);
  assert_equal_triples
    "datoms resolves lookup refs in EAVT entity, attr, and value position"
    [ (eid 1L), "friends", Ref (eid 2L) ]
    (datoms_ref ~e:(Lookup_ref ("name", String "Ivan")) ~a:"friends" ~v:(Ref_to (Lookup_ref ("name", String "Petr"))) db Eavt);
  assert_equal_triples
    "datoms resolves lookup refs in AVET value position"
    [ (eid 1L), "friends", Ref (eid 3L); (eid 2L), "friends", Ref (eid 3L) ]
    (datoms ~a:"friends" ~v:(Ref_to (Lookup_ref ("name", String "Oleg"))) db Avet);
  assert_equal_triples
    "seek_datoms resolves lookup refs in entity position"
    [ (eid 2L), "friends", Ref (eid 3L); (eid 2L), "name", String "Petr"; (eid 3L), "name", String "Oleg" ]
    (seek_datoms_ref ~e:(Lookup_ref ("name", String "Petr")) db Eavt);
  assert_equal_triples
    "index_range resolves lookup refs in bounds"
    [ (eid 1L), "friends", Ref (eid 2L)
    ; (eid 1L), "friends", Ref (eid 3L)
    ; (eid 2L), "friends", Ref (eid 3L)
    ]
    (index_range ~start:(Ref_to (Lookup_ref ("name", String "Petr"))) ~stop:(Ref_to (Lookup_ref ("name", String "Oleg"))) db "friends")

let query_base () =
  empty_db ~schema:[ "name", unique_identity; "friend", ref_attr ] ()
  |> db_with
       [ tx_entity ~db_id:(Entity_id (eid 1L)) [ one "id" (Int64 1L); one "name" (String "Ivan"); one "age" (Int64 11L); one "friend" (Ref (eid 2L)) ]
       ; tx_entity ~db_id:(Entity_id (eid 2L)) [ one "id" (Int64 2L); one "name" (String "Petr"); one "age" (Int64 22L); one "friend" (Ref (eid 3L)) ]
       ; tx_entity ~db_id:(Entity_id (eid 3L)) [ one "id" (Int64 3L); one "name" (String "Oleg"); one "age" (Int64 33L) ]
       ]

let test_lookup_refs__test_lookup_refs_query () =
  let db = query_base () in
  let entity_input_query =
    Query.v ~in_:[ Spec_scalar "e" ] ~with_:[] ~rules:[]
    [ Find_var "e"; Find_var "v" ]
    [ Clause.pattern (QVar "e") (QAttr "age") (QVar "v") ]
  in
  assert_equal_query_set
    "q accepts lookup refs as scalar entity inputs and preserves the returned input value"
    [ [ Result_value (Ref_to (Lookup_ref ("name", String "Ivan"))); Result_value (Int64 11L) ] ]
    (q ~inputs:[ Arg_scalar (Result_value (Ref_to (Lookup_ref ("name", String "Ivan")))) ] db entity_input_query);
  let collection_query =
    Query.v ~in_:[ Spec_collection "e" ] ~with_:(Query.with_ entity_input_query) ~rules:(Query.rules entity_input_query)
    [ Find_var "v" ]
    (Query.where entity_input_query)
  in
  assert_equal_query_set
    "q resolves lookup refs inside collection inputs"
    [ [ Result_value (Int64 11L) ]; [ Result_value (Int64 22L) ] ]
    (q
       ~inputs:
         [ Arg_collection
             [ Result_value (Ref_to (Lookup_ref ("name", String "Ivan")))
             ; Result_value (Ref_to (Lookup_ref ("name", String "Petr")))
             ]
         ]
       db
       collection_query);
  let ref_value_query =
    Query.v ~in_:[ Spec_scalar "v" ] ~with_:[] ~rules:[]
    [ Find_var "e" ]
    [ Clause.pattern (QVar "e") (QAttr "friend") (QVar "v") ]
  in
  assert_equal_query_set
    "q resolves lookup refs in ref value inputs"
    [ [ Result_entity (eid 1L) ] ]
    (q ~inputs:[ Arg_scalar (Result_value (Ref_to (Lookup_ref ("name", String "Petr")))) ] db ref_value_query);
  let inline_entity =
    Query.v ~in_:[] ~with_:[] ~rules:[]
    [ Find_var "v" ]
    [ Clause.pattern (QLookupRef ("name", String "Ivan"))  (QAttr "friend") (QVar "v") ]
  in
  assert_equal_query_set
    "q resolves inline lookup refs in entity position"
    [ [ Result_entity (eid 2L) ] ]
    (q db inline_entity);
  let inline_value =
    Query.v ~in_:[] ~with_:[] ~rules:[]
    [ Find_var "e" ]
    [ Clause.pattern (QVar "e") (QAttr "friend") (QValue (Ref_to (Lookup_ref ("name", String "Petr")))) ]
  in
  assert_equal_query_set
    "q resolves inline lookup refs in value position"
    [ [ Result_entity (eid 1L) ] ]
    (q db inline_value);
  assert_raises_invalid_arg_message
    "q rejects unresolved inline lookup refs"
    "Nothing found for entity id [:name \"Valery\"]"
    (fun () ->
      ignore
        (q
           db
           (Query.v ~in_:[] ~with_:[] ~rules:[]
              [ Find_var "e" ]
              [ Clause.pattern (QLookupRef ("name", String "Valery"))  (QAttr "friend") (QVar "e") ])))

let () =
  test_lookup_refs__test_lookup_refs ();
  test_lookup_refs__test_lookup_refs_transact ();
  test_lookup_refs__test_lookup_refs_transact_multi ();
  test_lookup_refs__lookup_refs_index_access ();
  test_lookup_refs__test_lookup_refs_query ()
