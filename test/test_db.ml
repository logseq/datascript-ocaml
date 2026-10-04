open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_int label expected actual =
  if expected <> actual then
    failf "%s: expected %d, got %d" label expected actual

let assert_equal_string label expected actual =
  if expected <> actual then
    failf "%s: expected %s, got %s" label expected actual

type hash_beef =
  { x : value
  ; tag : string
  }

let hash_hash_beef (_ : hash_beef) = 0xBEEF

let rec debug_value = function
  | Nil -> "nil"
  | Int64 value -> Int64.to_string value
  | Float value -> string_of_float value
  | String value -> Printf.sprintf "%S" value
  | Symbol value -> value
  | Bool value -> string_of_bool value
  | Keyword value -> ":" ^ value
  | Uuid value -> "#uuid " ^ value
  | Instant value -> "#inst " ^ Int64.to_string value
  | Regex value -> "#\"" ^ value ^ "\""
  | Ref value -> "Ref " ^ Int64.to_string (Entity_id.to_int64 value)
  | List values -> "[" ^ (values |> List.map debug_value |> String.concat " ") ^ "]"
  | Vector values -> "#vector[" ^ (values |> List.map debug_value |> String.concat " ") ^ "]"
  | Map entries ->
    "{"
    ^ (entries
       |> List.map (fun (key, value) -> debug_value key ^ " " ^ debug_value value)
       |> String.concat ", ")
    ^ "}"
  | Set values -> "#{" ^ (values |> List.map debug_value |> String.concat " ") ^ "}"
  | Tuple values ->
    "("
    ^ (values
       |> List.map (function Some value -> debug_value value | None -> "_")
       |> String.concat ", ")
    ^ ")"
  | TxRef -> "#datascript/tx"
  | Ref_to _ -> "Ref_to"

let assert_equal_triples label expected actual =
  let triples = List.map (fun d -> d.e, d.a, d.v) actual in
  if expected <> triples then
    let format triples =
      triples
      |> List.map (fun (e, a, v) -> Printf.sprintf "(%d, %s, %s)" (Entity_id.to_int e) a (debug_value v))
      |> String.concat "; "
    in
    failf "%s: expected [%s], got [%s]" label (format expected) (format triples)

let indexed =
  Schema.spec ~cardinality:(One) ?unique:(None) ~indexed:(true) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let unique_identity = Schema.spec ~cardinality:((Schema.cardinality indexed)) ?unique:(Some Identity) ~indexed:((Schema.indexed indexed)) ~is_component:((Schema.is_component indexed)) ~no_history:((Schema.no_history indexed)) ?doc:((Schema.doc indexed)) ?value_type:((Schema.value_type indexed)) ?tuple:(match ((Schema.tuple_attrs indexed), (Schema.tuple_types indexed)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

module DT = Internal.Datascript_types

let assert_uses_persistent_sorted_set (_index : DT.datom Persistent_sorted_set.t) = ()

let test_db__test_defrecord_updatable () =
  let value = { x = Keyword "ignored"; tag = "kept" } in
  let updated = { value with x = String "updated" } in
  assert_equal_int "custom hash analogue returns 0xBEEF" 0xBEEF (hash_hash_beef value);
  if updated.x <> String "updated" || updated.tag <> "kept" then
    failwith "record update should preserve generated field accessors"

let test_db__test_db_hash_cache () =
  let db = empty_db () in
  let before = db_hash_cache_size () in
  let first_hash = db_hash db in
  assert_equal_int "first db_hash call stores one cache entry" (before + 1) (db_hash_cache_size ());
  assert_equal_int "second db_hash call returns same value" first_hash (db_hash db);
  assert_equal_int "second db_hash call reuses cache entry" (before + 1) (db_hash_cache_size ());
  let changed = db_with [ Add (Entity_id (eid 1L), "name", String "Ivan") ] db in
  ignore (db_hash changed);
  assert_equal_int "different db identity gets a separate hash cache entry" (before + 2) (db_hash_cache_size ())

let test_db__test_uuid () =
  let first = squuid ~msec:1_710_000_123_456L () in
  let second = squuid ~msec:1_710_000_123_456L () in
  if first = second then failwith "squuid should include random bits";
  let first_uuid =
    match first with
    | Uuid uuid -> uuid
    | _ -> failwith "squuid should return a Uuid value"
  in
  if not (Int64.equal (squuid_time_millis first) 1_710_000_123_000L) then
    failwith "squuid_time_millis should return the embedded second";
  assert_equal_string
    "squuid uses the timestamp as its first UUID segment"
    "65ec87fb"
    (String.sub first_uuid 0 8);
  assert_equal_int "squuid has UUID string length" 36 (String.length first_uuid);
  if first_uuid.[8] <> '-' || first_uuid.[13] <> '-' || first_uuid.[18] <> '-' || first_uuid.[23] <> '-' then
    failwith "squuid should use canonical UUID separators"

let test_db__test_squuid_uses_wall_clock_time () =
  let before = int_of_float (Unix.gettimeofday ()) in
  let uuid =
    match squuid () with
    | Uuid uuid -> uuid
    | _ -> failwith "squuid should return a Uuid value"
  in
  let after = int_of_float (Unix.gettimeofday ()) in
  let seconds = int_of_string ("0x" ^ String.sub uuid 0 8) in
  if seconds < before || seconds > after then
    failf "squuid should embed wall-clock seconds, got %d outside [%d, %d]"
      seconds before after

let test_db__test_diff () =
  let left =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "a", One_value (Int64 1L); "b", One_value (Int64 2L); "c", One_value (Int64 4L) ] }
         ; Entity { db_id = Some (Entity_id (eid 2L)); attrs = [ "a", One_value (Int64 1L) ] }
         ]
  in
  let right =
    empty_db ()
    |> db_with [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "b", One_value (Int64 3L); "d", One_value (Int64 5L) ] } ]
    |> db_with [ Entity { db_id = Some (Entity_id (eid 1L)); attrs = [ "a", One_value (Int64 1L) ] } ]
  in
  let only_left, only_right, both = diff left right in
  assert_equal_triples
    "db diff returns datoms only on the left"
    [ (eid 1L), "b", Int64 2L; (eid 1L), "c", Int64 4L; (eid 2L), "a", Int64 1L ]
    only_left;
  assert_equal_triples
    "db diff returns datoms only on the right"
    [ (eid 1L), "b", Int64 3L; (eid 1L), "d", Int64 5L ]
    only_right;
  assert_equal_triples
    "db diff returns datoms present in both dbs"
    [ (eid 1L), "a", Int64 1L ]
    both

let test_db__test_index_api () =
  let db =
    empty_db ~schema:[ "name", indexed; "email", unique_identity ] ()
    |> db_with
         [ Add (Entity_id (eid 1L), "name", String "Ivan")
         ; Add (Entity_id (eid 1L), "email", String "ivan@example.com")
         ; Add (Entity_id (eid 2L), "name", String "Oleg")
         ; Add (Entity_id (eid 2L), "email", String "oleg@example.com")
         ]
  in
  assert_equal_triples
    "Db.datoms exposes index lookup through the db namespace"
    [ (eid 1L), "name", String "Ivan" ]
    (Db.datoms ~a:"name" ~v:(String "Ivan") db Avet |> List.of_seq);
  assert_equal_triples
    "Db.datoms_ref resolves lookup-ref entity bounds through the db namespace"
    [ (eid 1L), "email", String "ivan@example.com"; (eid 1L), "name", String "Ivan" ]
    (Db.datoms_ref ~e:(Lookup_ref ("email", String "ivan@example.com")) db Eavt |> List.of_seq);
  assert_equal_triples
    "Db.seek_datoms exposes ordered index seeks through the db namespace"
    [ (eid 1L), "name", String "Ivan"; (eid 2L), "name", String "Oleg" ]
    (Db.seek_datoms ~a:"name" ~v:(String "I") db Avet |> List.of_seq);
  assert_equal_triples
    "Db.index_range exposes AVET ranges through the db namespace"
    [ (eid 1L), "name", String "Ivan"; (eid 2L), "name", String "Oleg" ]
    (Db.index_range ~start:(String "I") ~stop:(String "P") db "name" |> List.of_seq);
  assert_equal_triples
    "XXXfolds the same ordered datoms as Db.datoms"
    [ (eid 1L), "name", String "Ivan"; (eid 2L), "name", String "Oleg" ]
    (Db.fold_datoms ~a:"name" (fun acc datom -> datom :: acc) [] db Aevt
     |> List.rev)

let test_db__test_indexes_use_persistent_sorted_set () =
  let _ =
    empty_db ~schema:[ "name", indexed; "friend", Schema.spec ~cardinality:((Schema.cardinality indexed)) ?unique:((Schema.unique indexed)) ~indexed:((Schema.indexed indexed)) ~is_component:((Schema.is_component indexed)) ~no_history:((Schema.no_history indexed)) ?doc:((Schema.doc indexed)) ?value_type:(Some RefType) ?tuple:(match ((Schema.tuple_attrs indexed), (Schema.tuple_types indexed)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) () ] ()
    |> db_with
         [ Add (Entity_id (eid 1L), "name", String "Ivan")
         ; Add (Entity_id (eid 1L), "friend", Ref (eid 2L))
         ; Add (Entity_id (eid 2L), "name", String "Oleg")
         ]
  in
  let impl_db =
    Internal.empty_db
      ~schema:[ "name", { DT.cardinality = DT.One; DT.unique = None; DT.indexed = true
                        ; DT.is_component = false; DT.no_history = false; DT.doc = None
                        ; DT.value_type = None; DT.tuple_attrs = None; DT.tuple_types = None }
              ; "friend", { DT.cardinality = DT.One; DT.unique = None; DT.indexed = false
                          ; DT.is_component = false; DT.no_history = false; DT.doc = None
                          ; DT.value_type = Some DT.RefType; DT.tuple_attrs = None; DT.tuple_types = None }
              ]
      ()
    |> Internal.db_with
         [ DT.Add (DT.Entity_id 1L, "name", DT.String "Ivan")
         ; DT.Add (DT.Entity_id 1L, "friend", DT.Ref 2L)
         ; DT.Add (DT.Entity_id 2L, "name", DT.String "Oleg")
         ]
  in
  assert_uses_persistent_sorted_set impl_db.DT.eavt_index;
  assert_uses_persistent_sorted_set impl_db.DT.aevt_index;
  assert_uses_persistent_sorted_set impl_db.DT.avet_index

let test_db__test_index_lookup_matches_upstream_numeric_comparator_bounds () =
  let db =
    empty_db ~schema:[ "x", Schema.spec ~cardinality:(Many) ?unique:((Schema.unique indexed)) ~indexed:((Schema.indexed indexed)) ~is_component:((Schema.is_component indexed)) ~no_history:((Schema.no_history indexed)) ?doc:((Schema.doc indexed)) ?value_type:((Schema.value_type indexed)) ?tuple:(match ((Schema.tuple_attrs indexed), (Schema.tuple_types indexed)) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) () ] ()
    |> db_with
         [ Add (Entity_id (eid 1L), "x", Int64 1L)
         ; Add (Entity_id (eid 2L), "x", Float 1.0)
         ; Add (Entity_id (eid 3L), "x", Int64 2L)
         ]
  in
  assert_equal_triples
    "AVET exact int lookup includes comparator-equal float values like upstream DataScript"
    [ (eid 1L), "x", Int64 1L; (eid 2L), "x", Float 1.0 ]
    (Db.datoms ~a:"x" ~v:(Int64 1L) db Avet |> List.of_seq);
  assert_equal_triples
    "AVET exact float lookup includes comparator-equal int values like upstream DataScript"
    [ (eid 1L), "x", Int64 1L; (eid 2L), "x", Float 1.0 ]
    (Db.datoms ~a:"x" ~v:(Float 1.0) db Avet |> List.of_seq);
  assert_equal_triples
    "AVET range preserves comparator-bound numeric behavior"
    [ (eid 1L), "x", Int64 1L; (eid 2L), "x", Float 1.0 ]
    (Db.index_range ~start:(Float 1.0) ~stop:(Float 1.0) db "x" |> List.of_seq)

let () =
  test_db__test_defrecord_updatable ();
  test_db__test_db_hash_cache ();
  test_db__test_uuid ();
  test_db__test_squuid_uses_wall_clock_time ();
  test_db__test_diff ();
  test_db__test_index_api ();
  test_db__test_indexes_use_persistent_sorted_set ();
  test_db__test_index_lookup_matches_upstream_numeric_comparator_bounds ()
