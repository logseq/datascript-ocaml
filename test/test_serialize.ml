open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms ?e ?a ?v ?tx db index =
  datoms_seq ?e ?a ?v ?tx db index |> List.of_seq

let assert_equal_int label expected actual =
  if expected <> actual then failf "%s: expected %d, got %d" label expected actual

let assert_equal_datoms label expected actual =
  if expected <> actual then failf "%s: unexpected datoms" label

let assert_equal_triples label expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v) actual in
  if expected <> actual then failf "%s: unexpected datoms" label

let assert_float_nan label = function
  | Float value when classify_float value = FP_nan -> ()
  | _ -> failf "%s: expected NaN" label

let many =
  Schema.spec ~cardinality:(Many) ?unique:(None) ~indexed:(false) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let indexed =
  Schema.spec ~cardinality:(One) ?unique:(None) ~indexed:(true) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let unique_identity =
  Schema.spec ~cardinality:((Schema.cardinality indexed)) ?unique:(Some Identity) ~indexed:((Schema.indexed indexed)) ~is_component:((Schema.is_component indexed)) ~no_history:((Schema.no_history indexed)) ?doc:((Schema.doc indexed)) ?value_type:((Schema.value_type indexed)) ?tuple:(match ((Schema.tuple_attrs indexed), (Schema.tuple_types indexed)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let ref_attr =
  Schema.spec ~cardinality:(One) ?unique:(None) ~indexed:(false) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(Some RefType) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let test_serialize__test_pr_read () =
  let db =
    db_from_reader_string
      "#datascript/DB {:schema {:name {:db/unique :db.unique/identity}
                                :friend {:db/valueType :db.type/ref}}
                       :datoms [[1 :age 44 536870913]
                                [1 :name \"Petr\" 536870913]
                                [2 :friend 1 536870914]
                                [3 :name \"DefaultTx\"]]}"
  in
  assert_equal_datoms
    "db_from_reader_string restores active datoms from #datascript/DB"
    [ datom ~tx:(txid (Int64.add (Tx_id.to_int64 tx0) 1L)) (eid 1L) "age" (Int64 44L)
    ; datom ~tx:(txid (Int64.add (Tx_id.to_int64 tx0) 1L)) (eid 1L) "name" (String "Petr")
    ; datom ~tx:(txid (Int64.add (Tx_id.to_int64 tx0) 2L)) (eid 2L) "friend" (Ref (eid 1L))
    ; datom (eid 3L) "name" (String "DefaultTx")
    ]
    (datoms db Eavt);
  if entid db "name" (String "Petr") <> Some (eid 1L) then
    failwith "db_from_reader_string should restore schema"

let test_serialize__test_init_db () =
  let source_datoms =
    [ datom (eid 1L) "name" (String "Petr")
    ; datom (eid 1L) "aka" (String "Devil")
    ; datom (eid 1L) "aka" (String "Tupen")
    ; datom (eid 1L) "age" (Int64 15L)
    ; datom (eid 1L) "follows" (Ref (eid 2L))
    ; datom (eid 2L) "name" (String "Oleg")
    ; datom (eid 2L) "age" (Int64 30L)
    ; datom (eid 30L) "url" (String "https://")
    ]
  in
  let schema = [ "aka", many; "age", indexed; "follows", ref_attr; "name", unique_identity ] in
  let db_init = init_db ~schema source_datoms in
  let db_transact =
    empty_db ~schema ()
    |> db_with
         [ Add (Entity_id (eid 1L), "name", String "Petr")
         ; Add (Entity_id (eid 1L), "aka", String "Devil")
         ; Add (Entity_id (eid 1L), "aka", String "Tupen")
         ; Add (Entity_id (eid 1L), "age", Int64 15L)
         ; Add (Entity_id (eid 1L), "follows", Ref (eid 2L))
         ; Add (Entity_id (eid 2L), "name", String "Oleg")
         ; Add (Entity_id (eid 2L), "age", Int64 30L)
         ; Add (Entity_id (eid 30L), "url", String "https://")
         ]
  in
  assert_equal_triples
    "init_db produces same active facts as regular transactions"
    (List.map (fun d -> d.e, d.a, d.v) (datoms db_transact Eavt))
    (datoms db_init Eavt);
  assert_equal_int "init_db tracks max entity ids from datoms" (Entity_id.to_int (serializable db_transact).serializable_max_eid) (Entity_id.to_int (serializable db_init).serializable_max_eid);
  let add_next db = db_with [ Entity { db_id = Some (Temp_id "next"); attrs = [ "name", One_value (String "Lex") ] } ] db in
  assert_equal_triples
    "init_db produces same next tempid allocation as regular transactions"
    (List.map (fun d -> d.e, d.a, d.v) (datoms (add_next db_transact) Eavt))
    (datoms (add_next db_init) Eavt)

let test_serialize__test_max_eid_from_refs () =
  let db =
    empty_db ~schema:[ "ref", ref_attr ] ()
    |> db_with [ Add (Entity_id (eid 1L), "name", String "Ivan") ]
    |> db_with [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "ref", One_entity { db_id = None; attrs = [ "name", One_value (String "Oleg") ] } ] } ]
  in
  assert_equal_int "nested ref entities should advance max-eid" 2 (Entity_id.to_int (serializable db).serializable_max_eid);
  let restored = db |> serializable |> from_serializable in
  assert_equal_int "from_serializable preserves max-eid" 2 (Entity_id.to_int (serializable restored).serializable_max_eid)

let test_serialize__serialize () =
  let db =
    empty_db ~schema:[ "aka", many; "created-at", indexed; "name", unique_identity; "uuid", indexed ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id (eid 1L))
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "aka", Many_values [ String "IV"; String "Terrible" ]
                 ; "created-at", One_value (Instant 1_710_000_123_456L)
                 ; "uuid", One_value (Uuid "65ec87fb-0000-0000-0000-000000000001")
                 ]
             }
         ]
    |> db_with [ Add (Entity_id (eid 1L), "name", String "Petr") ]
  in
  let restored = db |> serializable |> from_serializable in
  assert_equal_datoms "from_serializable restores active datoms" (datoms db Eavt) (datoms restored Eavt);
  if entid restored "name" (String "Petr") <> Some (eid 1L) then
    failwith "from_serializable should preserve schema"

let test_serialize__test_nan () =
  let nan = Float Float.nan in
  let db =
    empty_db ~schema:[ "nan", indexed ] ()
    |> db_with [ Add (Entity_id (eid 1L), "nan", nan) ]
  in
  let restored = db |> serializable |> from_serializable in
  (match Option.bind (entity restored (Entity_id (eid 1L))) (fun entity -> entity_attr entity "nan") with
   | Some (One_value value) -> assert_float_nan "from_serializable should preserve NaN" value
   | _ -> failwith "from_serializable should preserve NaN");
  match datoms ~a:"nan" ~v:nan restored Avet with
  | [ { v; _ } ] -> assert_float_nan "AVET value lookup should find NaN after restore" v
  | _ -> failwith "AVET value lookup should find NaN after restore"

let () =
  test_serialize__test_pr_read ();
  test_serialize__test_init_db ();
  test_serialize__test_max_eid_from_refs ();
  test_serialize__serialize ();
  test_serialize__test_nan ()
