open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms db index ?e ?a ?v ?tx () =
  datoms_seq db index ?e ?a ?v ?tx () |> List.of_seq

let assert_equal_int label expected actual =
  if expected <> actual then failf "%s: expected %d, got %d" label expected actual

let assert_equal_triples label expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v) actual in
  if expected <> actual then failf "%s: unexpected datoms" label

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

let test_core__test_protocols () =
  let schema = [ "aka", many ] in
  let db =
    empty_db ~schema ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "aka", Many_values [ String "IV"; String "Terrible" ]
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "age", One_value (Int 37)
                 ; "huh?", One_value (Bool false)
                 ]
             }
         ]
  in
  if not (is_db db) then failwith "db should satisfy the db predicate";
  if schema_of_edn_string "{:aka {:db/cardinality :db.cardinality/many}}" <> schema then
    failwith "schema parser should preserve the upstream cardinality-many schema";
  assert_equal_int "db count analogue is visible datom count" 6 (List.length (datoms db Eavt ()));
  assert_equal_triples
    "db indexes are seqable and expose the expected facts"
    [ 1, "aka", String "IV"
    ; 1, "aka", String "Terrible"
    ; 1, "name", String "Ivan"
    ; 2, "age", Int 37
    ; 2, "huh?", Bool false
    ; 2, "name", String "Petr"
    ]
    (datoms db Eavt ());
  if not (List.for_all is_datom (datoms db Eavt ())) then
    failwith "all indexed values should satisfy is_datom";
  if datoms (empty_db ~schema ()) Eavt () <> [] then
    failwith "empty db with the same schema should contain no datoms"

let () =
  test_core__test_protocols ()
