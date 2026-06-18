open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms db index ?e ?a ?v ?tx () =
  datoms_seq db index ?e ?a ?v ?tx () |> List.of_seq

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

let unique_identity =
  { indexed with unique = Some Identity }

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
    [ datom ~tx:(tx0 + 1) ~e:1 ~a:"age" ~v:(Int 44) ()
    ; datom ~tx:(tx0 + 1) ~e:1 ~a:"name" ~v:(String "Petr") ()
    ; datom ~tx:(tx0 + 2) ~e:2 ~a:"friend" ~v:(Ref 1) ()
    ; datom ~e:3 ~a:"name" ~v:(String "DefaultTx") ()
    ]
    (datoms db Eavt ());
  if entid db "name" (String "Petr") <> Some 1 then
    failwith "db_from_reader_string should restore schema";
  if datoms (history db) Eavt () <> datoms db Eavt () then
    failwith "db_from_reader_string should initialize history datoms"

let test_serialize__test_init_db () =
  let source_datoms =
    [ datom ~e:1 ~a:"name" ~v:(String "Petr") ()
    ; datom ~e:1 ~a:"aka" ~v:(String "Devil") ()
    ; datom ~e:1 ~a:"aka" ~v:(String "Tupen") ()
    ; datom ~e:1 ~a:"age" ~v:(Int 15) ()
    ; datom ~e:1 ~a:"follows" ~v:(Ref 2) ()
    ; datom ~e:2 ~a:"name" ~v:(String "Oleg") ()
    ; datom ~e:2 ~a:"age" ~v:(Int 30) ()
    ; datom ~e:30 ~a:"url" ~v:(String "https://") ()
    ]
  in
  let schema = [ "aka", many; "age", indexed; "follows", ref_attr; "name", unique_identity ] in
  let db_init = init_db ~schema source_datoms in
  let db_transact =
    empty_db ~schema ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Petr")
         ; Add (Entity_id 1, "aka", String "Devil")
         ; Add (Entity_id 1, "aka", String "Tupen")
         ; Add (Entity_id 1, "age", Int 15)
         ; Add (Entity_id 1, "follows", Ref 2)
         ; Add (Entity_id 2, "name", String "Oleg")
         ; Add (Entity_id 2, "age", Int 30)
         ; Add (Entity_id 30, "url", String "https://")
         ]
  in
  assert_equal_triples
    "init_db produces same active facts as regular transactions"
    (List.map (fun d -> d.e, d.a, d.v) (datoms db_transact Eavt ()))
    (datoms db_init Eavt ());
  assert_equal_int "init_db tracks max entity ids from datoms" db_transact.max_eid db_init.max_eid;
  let add_next db = db_with [ Entity { db_id = Some (Temp_id "next"); attrs = [ "name", One_value (String "Lex") ] } ] db in
  assert_equal_triples
    "init_db produces same next tempid allocation as regular transactions"
    (List.map (fun d -> d.e, d.a, d.v) (datoms (add_next db_transact) Eavt ()))
    (datoms (add_next db_init) Eavt ())

let test_serialize__test_max_eid_from_refs () =
  let db =
    empty_db ~schema:[ "ref", ref_attr ] ()
    |> db_with [ Add (Entity_id 1, "name", String "Ivan") ]
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "ref", One_entity { db_id = None; attrs = [ "name", One_value (String "Oleg") ] } ] } ]
  in
  assert_equal_int "nested ref entities should advance max-eid" 2 db.max_eid;
  let restored = db |> serializable |> from_serializable in
  assert_equal_int "from_serializable preserves max-eid" 2 restored.max_eid

let test_serialize__serialize () =
  let db =
    empty_db ~schema:[ "aka", many; "created-at", indexed; "name", unique_identity; "uuid", indexed ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "aka", Many_values [ String "IV"; String "Terrible" ]
                 ; "created-at", One_value (Instant 1_710_000_123_456)
                 ; "uuid", One_value (Uuid "65ec87fb-0000-0000-0000-000000000001")
                 ]
             }
         ]
    |> db_with [ Add (Entity_id 1, "name", String "Petr") ]
  in
  let restored = db |> serializable |> from_serializable in
  assert_equal_datoms "from_serializable restores active datoms" (datoms db Eavt ()) (datoms restored Eavt ());
  assert_equal_datoms "from_serializable restores history datoms" (datoms (history db) Eavt ()) (datoms (history restored) Eavt ());
  if entid restored "name" (String "Petr") <> Some 1 then
    failwith "from_serializable should preserve schema"

let test_serialize__test_nan () =
  let nan = Float Float.nan in
  let db =
    empty_db ~schema:[ "nan", indexed ] ()
    |> db_with [ Add (Entity_id 1, "nan", nan) ]
  in
  let restored = db |> serializable |> from_serializable in
  (match Option.bind (entity restored (Entity_id 1)) (fun entity -> entity_attr entity "nan") with
   | Some (One_value value) -> assert_float_nan "from_serializable should preserve NaN" value
   | _ -> failwith "from_serializable should preserve NaN");
  match datoms restored Avet ~a:"nan" ~v:nan () with
  | [ { v; _ } ] -> assert_float_nan "AVET value lookup should find NaN after restore" v
  | _ -> failwith "AVET value lookup should find NaN after restore"

let () =
  test_serialize__test_pr_read ();
  test_serialize__test_init_db ();
  test_serialize__test_max_eid_from_refs ();
  test_serialize__serialize ();
  test_serialize__test_nan ()
