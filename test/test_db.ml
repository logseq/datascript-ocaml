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
  | Int value -> string_of_int value
  | Float value -> string_of_float value
  | String value -> Printf.sprintf "%S" value
  | Symbol value -> value
  | Bool value -> string_of_bool value
  | Keyword value -> ":" ^ value
  | Uuid value -> "#uuid " ^ value
  | Instant value -> "#inst " ^ string_of_int value
  | Regex value -> "#\"" ^ value ^ "\""
  | Ref value -> "Ref " ^ string_of_int value
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
      |> List.map (fun (e, a, v) -> Printf.sprintf "(%d, %s, %s)" e a (debug_value v))
      |> String.concat "; "
    in
    failf "%s: expected [%s], got [%s]" label (format expected) (format triples)

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

let assert_uses_persistent_sorted_set (_index : datom Persistent_sorted_set.t) = ()

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
  let changed = db_with [ Add (Entity_id 1, "name", String "Ivan") ] db in
  ignore (db_hash changed);
  assert_equal_int "different db identity gets a separate hash cache entry" (before + 2) (db_hash_cache_size ())

let test_db__test_uuid () =
  let first = squuid ~msec:1_710_000_123_456 () in
  let second = squuid ~msec:1_710_000_123_456 () in
  if first = second then failwith "squuid should include random bits";
  let first_uuid =
    match first with
    | Uuid uuid -> uuid
    | _ -> failwith "squuid should return a Uuid value"
  in
  assert_equal_int
    "squuid_time_millis returns the embedded second"
    1_710_000_123_000
    (squuid_time_millis first);
  assert_equal_string
    "squuid uses the timestamp as its first UUID segment"
    "65ec87fb"
    (String.sub first_uuid 0 8);
  assert_equal_int "squuid has UUID string length" 36 (String.length first_uuid);
  if first_uuid.[8] <> '-' || first_uuid.[13] <> '-' || first_uuid.[18] <> '-' || first_uuid.[23] <> '-' then
    failwith "squuid should use canonical UUID separators"

let test_db__test_diff () =
  let left =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "a", One_value (Int 1); "b", One_value (Int 2); "c", One_value (Int 4) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "a", One_value (Int 1) ] }
         ]
  in
  let right =
    empty_db ()
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "b", One_value (Int 3); "d", One_value (Int 5) ] } ]
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "a", One_value (Int 1) ] } ]
  in
  let only_left, only_right, both = diff left right in
  assert_equal_triples
    "db diff returns datoms only on the left"
    [ 1, "b", Int 2; 1, "c", Int 4; 2, "a", Int 1 ]
    only_left;
  assert_equal_triples
    "db diff returns datoms only on the right"
    [ 1, "b", Int 3; 1, "d", Int 5 ]
    only_right;
  assert_equal_triples
    "db diff returns datoms present in both dbs"
    [ 1, "a", Int 1 ]
    both

let test_db__test_index_api () =
  let db =
    empty_db ~schema:[ "name", indexed; "email", unique_identity ] ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 1, "email", String "ivan@example.com")
         ; Add (Entity_id 2, "name", String "Oleg")
         ; Add (Entity_id 2, "email", String "oleg@example.com")
         ]
  in
  assert_equal_triples
    "Db.datoms exposes index lookup through the db namespace"
    [ 1, "name", String "Ivan" ]
    (Db.datoms db Avet ~a:"name" ~v:(String "Ivan") () |> List.of_seq);
  assert_equal_triples
    "Db.datoms_ref resolves lookup-ref entity bounds through the db namespace"
    [ 1, "email", String "ivan@example.com"; 1, "name", String "Ivan" ]
    (Db.datoms_ref db Eavt ~e:(Lookup_ref ("email", String "ivan@example.com")) () |> List.of_seq);
  assert_equal_triples
    "Db.seek_datoms exposes ordered index seeks through the db namespace"
    [ 1, "name", String "Ivan"; 2, "name", String "Oleg" ]
    (Db.seek_datoms db Avet ~a:"name" ~v:(String "I") ());
  assert_equal_triples
    "Db.index_range exposes AVET ranges through the db namespace"
    [ 1, "name", String "Ivan"; 2, "name", String "Oleg" ]
    (Db.index_range db "name" ~start:(String "I") ~stop:(String "P") ())

let test_db__test_indexes_use_persistent_sorted_set () =
  let db =
    empty_db ~schema:[ "name", indexed; "friend", { indexed with value_type = Some RefType } ] ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 1, "friend", Ref 2)
         ; Add (Entity_id 2, "name", String "Oleg")
         ]
  in
  assert_uses_persistent_sorted_set db.eavt_index;
  assert_uses_persistent_sorted_set db.aevt_index;
  assert_uses_persistent_sorted_set db.avet_index

let test_db__test_index_lookup_matches_upstream_numeric_comparator_bounds () =
  let db =
    empty_db ~schema:[ "x", { indexed with cardinality = Many } ] ()
    |> db_with
         [ Add (Entity_id 1, "x", Int 1)
         ; Add (Entity_id 2, "x", Float 1.0)
         ; Add (Entity_id 3, "x", Int 2)
         ]
  in
  assert_equal_triples
    "AVET exact int lookup includes comparator-equal float values like upstream DataScript"
    [ 1, "x", Int 1; 2, "x", Float 1.0 ]
    (Db.datoms db Avet ~a:"x" ~v:(Int 1) () |> List.of_seq);
  assert_equal_triples
    "AVET exact float lookup includes comparator-equal int values like upstream DataScript"
    [ 1, "x", Int 1; 2, "x", Float 1.0 ]
    (Db.datoms db Avet ~a:"x" ~v:(Float 1.0) () |> List.of_seq);
  assert_equal_triples
    "AVET range preserves comparator-bound numeric behavior"
    [ 1, "x", Int 1; 2, "x", Float 1.0 ]
    (Db.index_range db "x" ~start:(Float 1.0) ~stop:(Float 1.0) ())

let () =
  test_db__test_defrecord_updatable ();
  test_db__test_db_hash_cache ();
  test_db__test_uuid ();
  test_db__test_diff ();
  test_db__test_index_api ();
  test_db__test_indexes_use_persistent_sorted_set ();
  test_db__test_index_lookup_matches_upstream_numeric_comparator_bounds ()
