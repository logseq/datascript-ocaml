open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms db index ?e ?a ?v ?tx () =
  datoms_seq db index ?e ?a ?v ?tx () |> List.of_seq

let datoms_ref_seq = datoms_ref

let datoms_ref db index ?e ?a ?v ?tx () =
  datoms_ref_seq db index ?e ?a ?v ?tx () |> List.of_seq

let seek_datoms_ref_seq = seek_datoms_ref
let seek_datoms_ref db index ?e ?a ?v ?tx () =
  seek_datoms_ref_seq db index ?e ?a ?v ?tx () |> List.of_seq

let index_range_seq = index_range
let index_range db attr ?start ?stop () =
  index_range_seq db attr ?start ?stop () |> List.of_seq

let assert_equal_triples label expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v) actual in
  if expected <> actual then
    let value = function
      | Int value -> string_of_int value
      | String value -> Printf.sprintf "%S" value
      | Ref value -> "Ref " ^ string_of_int value
      | other -> Printf.sprintf "%d" (Hashtbl.hash other)
    in
    let triples triples =
      triples
      |> List.map (fun (e, a, v) -> Printf.sprintf "(%d,%s,%s)" e a (value v))
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
let unique_value = { indexed with unique = Some Value }
let ref_attr = { indexed with indexed = false; value_type = Some RefType }
let ref_many = { ref_attr with cardinality = Many }

let tx_entity ?db_id attrs = Entity { db_id; attrs }
let one attr value = attr, One_value value
let many attr values = attr, Many_values values

let lookup_base () =
  empty_db ~schema:[ "name", unique_identity; "email", unique_value; "age", indexed ] ()
  |> db_with
       [ tx_entity
           ~db_id:(Entity_id 1)
           [ one "name" (String "Ivan")
           ; one "email" (String "@1")
           ; one "age" (Int 35)
           ]
       ; tx_entity
           ~db_id:(Entity_id 2)
           [ one "name" (String "Petr")
           ; one "email" (String "@2")
           ; one "age" (Int 22)
           ]
       ]

let assert_entity label expected db entity_ref =
  match entity db entity_ref with
  | None when expected = [] -> ()
  | None -> failf "%s: expected entity" label
  | Some entity ->
    assert_equal_triples label expected (datoms db Eavt ~e:entity.id ())

let test_lookup_refs__test_lookup_refs () =
  let db = lookup_base () in
  assert_entity
    "lookup ref resolves unique identity attr"
    [ 1, "age", Int 35; 1, "email", String "@1"; 1, "name", String "Ivan" ]
    db
    (Lookup_ref ("name", String "Ivan"));
  assert_entity
    "lookup ref resolves unique value attr"
    [ 1, "age", Int 35; 1, "email", String "@1"; 1, "name", String "Ivan" ]
    db
    (Lookup_ref ("email", String "@1"));
  assert_entity "missing lookup ref returns no entity" [] db (Lookup_ref ("name", String "Sergey"));
  assert_entity "nil lookup ref value returns no entity" [] db (Lookup_ref ("name", Nil));
  assert_raises_invalid_arg_message
    "lookup ref rejects non-unique attrs"
    "Lookup ref attribute should be marked as :db/unique: [:age 10]"
    (fun () -> ignore (entity db (Lookup_ref ("age", Int 10))))

let transact_base () =
  empty_db ~schema:[ "name", unique_identity; "friend", ref_attr; "friends", ref_many; "age", indexed ] ()
  |> db_with
       [ tx_entity ~db_id:(Entity_id 1) [ one "name" (String "Ivan") ]
       ; tx_entity ~db_id:(Entity_id 2) [ one "name" (String "Petr") ]
       ; tx_entity ~db_id:(Entity_id 3) [ one "name" (String "Oleg") ]
       ; tx_entity ~db_id:(Entity_id 4) [ one "name" (String "Sergey") ]
       ]

let test_lookup_refs__test_lookup_refs_transact () =
  let db = transact_base () in
  let ivan = Lookup_ref ("name", String "Ivan") in
  let petr = Lookup_ref ("name", String "Petr") in
  let oleg = Lookup_ref ("name", String "Oleg") in
  let db =
    db
    |> db_with [ Add (ivan, "age", Int 35) ]
    |> db_with [ tx_entity ~db_id:ivan [ one "age" (Int 36) ] ]
    |> db_with [ Add (Entity_id 1, "friend", Ref_to petr) ]
    |> db_with [ tx_entity ~db_id:(Entity_id 1) [ one "friend" (Ref_to oleg) ] ]
    |> db_with [ tx_entity ~db_id:(Entity_id 2) [ one "_friend" (Ref_to ivan) ] ]
  in
  assert_equal_triples
    "lookup refs transact through add and entity maps"
    [ 1, "age", Int 36
    ; 1, "friend", Ref 2
    ; 1, "name", String "Ivan"
    ; 2, "name", String "Petr"
    ; 3, "name", String "Oleg"
    ; 4, "name", String "Sergey"
    ]
    (datoms db Eavt ());
  let db =
    db
    |> db_with [ Add (Entity_id 3, "name", String "Oleg") ]
    |> db_with [ Add (Entity_id 1, "friend", Ref_to oleg) ]
    |> db_with [ CompareAndSet (ivan, "name", Some (String "Ivan"), String "Vanya") ]
    |> db_with [ CompareAndSet (Entity_id 1, "friend", Some (Ref_to oleg), Ref_to (Lookup_ref ("name", String "Sergey"))) ]
    |> db_with [ Retract (Lookup_ref ("name", String "Vanya"), "age", Some (Int 36)) ]
    |> db_with [ RetractAttr (Lookup_ref ("name", String "Vanya"), "friend") ]
  in
  assert_equal_triples
    "lookup refs transact through CAS and retractions"
    [ 1, "name", String "Vanya"
    ; 2, "name", String "Petr"
    ; 3, "name", String "Oleg"
    ; 4, "name", String "Sergey"
    ]
    (datoms db Eavt ());
  let retracted = db_with [ RetractEntity (Lookup_ref ("name", String "Vanya")) ] db in
  assert_entity "lookup refs can retract an entity" [] retracted (Lookup_ref ("name", String "Vanya"));
  assert_raises_invalid_arg_message
    "lookup refs in add entity position must resolve"
    "Nothing found for entity id [:name \"Missing\"]"
    (fun () -> ignore (db_with [ Add (Lookup_ref ("name", String "Missing"), "age", Int 10) ] (transact_base ())));
  assert_raises_invalid_arg_message
    "lookup refs in entity-map db/id must resolve"
    "Nothing found for entity id [:name \"Missing\"]"
    (fun () ->
      ignore (db_with [ tx_entity ~db_id:(Lookup_ref ("name", String "Missing")) [ one "age" (Int 10) ] ] (transact_base ())))

let test_lookup_refs__test_lookup_refs_transact_multi () =
  let db = transact_base () in
  let petr = Lookup_ref ("name", String "Petr") in
  let oleg = Lookup_ref ("name", String "Oleg") in
  let db =
    db
    |> db_with [ Add (Entity_id 1, "friends", Ref_to petr) ]
    |> db_with [ Add (Entity_id 1, "friends", Ref_to oleg) ]
    |> db_with [ tx_entity ~db_id:(Entity_id 2) [ many "_friends" [ Ref_to (Lookup_ref ("name", String "Ivan")); Ref_to oleg ] ] ]
  in
  assert_equal_triples
    "lookup refs transact through cardinality-many ref attrs and reverse attrs"
    [ 1, "friends", Ref 2
    ; 1, "friends", Ref 3
    ; 1, "name", String "Ivan"
    ; 2, "name", String "Petr"
    ; 3, "friends", Ref 2
    ; 3, "name", String "Oleg"
    ; 4, "name", String "Sergey"
    ]
    (datoms db Eavt ());
  let mapped =
    transact_base ()
    |> db_with [ tx_entity ~db_id:(Entity_id 1) [ many "friends" [ Ref_to petr; Ref_to oleg ] ] ]
  in
  assert_equal_triples
    "entity maps accept lookup refs in many ref values"
    [ 1, "friends", Ref 2
    ; 1, "friends", Ref 3
    ; 1, "name", String "Ivan"
    ; 2, "name", String "Petr"
    ; 3, "name", String "Oleg"
    ; 4, "name", String "Sergey"
    ]
    (datoms mapped Eavt ())

let index_base () =
  empty_db ~schema:[ "name", unique_identity; "friends", ref_many ] ()
  |> db_with
       [ tx_entity ~db_id:(Entity_id 1) [ one "name" (String "Ivan"); many "friends" [ Ref 2; Ref 3 ] ]
       ; tx_entity ~db_id:(Entity_id 2) [ one "name" (String "Petr"); one "friends" (Ref 3) ]
       ; tx_entity ~db_id:(Entity_id 3) [ one "name" (String "Oleg") ]
       ]

let test_lookup_refs__lookup_refs_index_access () =
  let db = index_base () in
  assert_equal_triples
    "datoms resolves lookup refs in EAVT entity position"
    [ 1, "friends", Ref 2; 1, "friends", Ref 3; 1, "name", String "Ivan" ]
    (datoms_ref db Eavt ~e:(Lookup_ref ("name", String "Ivan")) ());
  assert_equal_triples
    "datoms resolves lookup refs in EAVT entity, attr, and value position"
    [ 1, "friends", Ref 2 ]
    (datoms_ref db Eavt ~e:(Lookup_ref ("name", String "Ivan")) ~a:"friends" ~v:(Ref_to (Lookup_ref ("name", String "Petr"))) ());
  assert_equal_triples
    "datoms resolves lookup refs in AVET value position"
    [ 1, "friends", Ref 3; 2, "friends", Ref 3 ]
    (datoms db Avet ~a:"friends" ~v:(Ref_to (Lookup_ref ("name", String "Oleg"))) ());
  assert_equal_triples
    "seek_datoms resolves lookup refs in entity position"
    [ 2, "friends", Ref 3; 2, "name", String "Petr"; 3, "name", String "Oleg" ]
    (seek_datoms_ref db Eavt ~e:(Lookup_ref ("name", String "Petr")) ());
  assert_equal_triples
    "index_range resolves lookup refs in bounds"
    [ 1, "friends", Ref 2
    ; 1, "friends", Ref 3
    ; 2, "friends", Ref 3
    ]
    (index_range
       db
       "friends"
       ~start:(Ref_to (Lookup_ref ("name", String "Petr")))
       ~stop:(Ref_to (Lookup_ref ("name", String "Oleg")))
       ())

let query_base () =
  empty_db ~schema:[ "name", unique_identity; "friend", ref_attr ] ()
  |> db_with
       [ tx_entity ~db_id:(Entity_id 1) [ one "id" (Int 1); one "name" (String "Ivan"); one "age" (Int 11); one "friend" (Ref 2) ]
       ; tx_entity ~db_id:(Entity_id 2) [ one "id" (Int 2); one "name" (String "Petr"); one "age" (Int 22); one "friend" (Ref 3) ]
       ; tx_entity ~db_id:(Entity_id 3) [ one "id" (Int 3); one "name" (String "Oleg"); one "age" (Int 33) ]
       ]

let test_lookup_refs__test_lookup_refs_query () =
  let db = query_base () in
  let entity_input_query =
    { find = [ Find_var "e"; Find_var "v" ]
    ; inputs = [ Input_scalar_decl "e" ]
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "age", QVar "v") ]
    }
  in
  assert_equal_query_set
    "q accepts lookup refs as scalar entity inputs and preserves the returned input value"
    [ [ Result_value (Ref_to (Lookup_ref ("name", String "Ivan"))); Result_value (Int 11) ] ]
    (q ~inputs:[ Arg_scalar (Result_value (Ref_to (Lookup_ref ("name", String "Ivan")))) ] db entity_input_query);
  let collection_query =
    { entity_input_query with inputs = [ Input_collection_decl "e" ]; find = [ Find_var "v" ] }
  in
  assert_equal_query_set
    "q resolves lookup refs inside collection inputs"
    [ [ Result_value (Int 11) ]; [ Result_value (Int 22) ] ]
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
    { find = [ Find_var "e" ]
    ; inputs = [ Input_scalar_decl "v" ]
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "friend", QVar "v") ]
    }
  in
  assert_equal_query_set
    "q resolves lookup refs in ref value inputs"
    [ [ Result_entity 1 ] ]
    (q ~inputs:[ Arg_scalar (Result_value (Ref_to (Lookup_ref ("name", String "Petr")))) ] db ref_value_query);
  let inline_entity =
    { find = [ Find_var "v" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QLookupRef ("name", String "Ivan"), QAttr "friend", QVar "v") ]
    }
  in
  assert_equal_query_set
    "q resolves inline lookup refs in entity position"
    [ [ Result_entity 2 ] ]
    (q db inline_entity);
  let inline_value =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "friend", QValue (Ref_to (Lookup_ref ("name", String "Petr")))) ]
    }
  in
  assert_equal_query_set
    "q resolves inline lookup refs in value position"
    [ [ Result_entity 1 ] ]
    (q db inline_value);
  assert_raises_invalid_arg_message
    "q rejects unresolved inline lookup refs"
    "Nothing found for entity id [:name \"Valery\"]"
    (fun () ->
      ignore
        (q
           db
           { find = [ Find_var "e" ]
           ; inputs = []
           ; with_vars = []
           ; rules = []
           ; where = [ Pattern (QLookupRef ("name", String "Valery"), QAttr "friend", QVar "e") ]
           }))

let () =
  test_lookup_refs__test_lookup_refs ();
  test_lookup_refs__test_lookup_refs_transact ();
  test_lookup_refs__test_lookup_refs_transact_multi ();
  test_lookup_refs__lookup_refs_index_access ();
  test_lookup_refs__test_lookup_refs_query ()
