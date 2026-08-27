open Alcotest
open Datascript

let check_int_list = Test_alcotest_support.check_int_list
let check_string_list = Test_alcotest_support.check_string_list
let check_bool = Test_alcotest_support.check_bool
let expect_invalid_arg = Test_alcotest_support.expect_invalid_arg

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

let datoms_list db ?e ?a () =
  datoms db Eavt ?e ?a () |> List.of_seq

let int_values db ?a ?e () =
  datoms_list db ?a ?e ()
  |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  |> List.sort compare

let history_int_values db ?a ?e () =
  datoms_list (history db) ?a ?e ()
  |> List.filter (fun d -> d.added)
  |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  |> List.sort compare

let history_all_int_values db ?a ?e () =
  datoms_list (history db) ?a ?e ()
  |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  |> List.sort compare

let string_values db ?a ?e () =
  datoms_list db ?a ?e ()
  |> List.map (fun d -> match d.v with String s -> s | _ -> "")
  |> List.sort compare

let setup_db () =
  db_with
    [ Add (Entity_id 1, "name", String "Alice")
    ; Add (Entity_id 1, "age", Int 25)
    ; Add (Entity_id 2, "name", String "Bob")
    ; Add (Entity_id 2, "age", Int 35)
    ]
    (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())

let test_purge_datom_from_current_and_history () =
  let db = setup_db () in
  let db = db_with [ Retract (Lookup_ref ("name", String "Alice"), "age", Some (Int 25)) ] db in
  check_int_list "Alice age should be absent after retract" [] (int_values db ~a:"age" ~e:1 ());
  check_int_list "Alice age should remain in history after retract" [ 25 ]
    (history_int_values db ~a:"age" ~e:1 ());
  let db = db_with [ Purge (Lookup_ref ("name", String "Bob"), "age", Int 35) ] db in
  check_int_list "Bob age should be absent after purge" [] (int_values db ~a:"age" ~e:2 ());
  check_int_list "Bob age should be absent from history after purge" []
    (history_all_int_values db ~a:"age" ~e:2 ());
  let db = db_with [ Purge (Lookup_ref ("name", String "Alice"), "age", Int 25) ] db in
  check_int_list "purged retracted datom should leave history" []
    (history_all_int_values db ~a:"age" ~e:1 ())

let test_purge_attribute () =
  let db = setup_db () in
  let db = db_with [ PurgeAttr (Lookup_ref ("name", String "Alice"), "age") ] db in
  check_int_list "Alice age should be absent after attribute purge" [] (int_values db ~a:"age" ~e:1 ());
  check_int_list "Alice age should be absent from history" []
    (history_all_int_values db ~a:"age" ~e:1 ());
  check_string_list "Alice name should remain" [ "Alice"; "Bob" ] (string_values db ~a:"name" ());
  let db = setup_db () in
  let db = db_with [ RetractAttr (Lookup_ref ("name", String "Bob"), "age") ] db in
  check_int_list "Bob age should be absent after retract attribute" [] (int_values db ~a:"age" ~e:2 ());
  check_int_list "Bob age should remain in history" [ 35 ] (history_int_values db ~a:"age" ~e:2 ());
  let db = db_with [ PurgeAttr (Lookup_ref ("name", String "Bob"), "age") ] db in
  check_int_list "Bob age should be purged from history" []
    (history_all_int_values db ~a:"age" ~e:2 ())

let test_purge_entity () =
  let db = setup_db () in
  let db = db_with [ PurgeEntity (Lookup_ref ("name", String "Alice")) ] db in
  check_string_list "Alice should be removed from current db" [ "Bob" ] (string_values db ~a:"name" ());
  check_string_list "Alice should be removed from history" [ "Bob" ]
    (string_values (history db) ~a:"name" ());
  let db = setup_db () in
  let db = db_with [ RetractEntity (Lookup_ref ("name", String "Bob")) ] db in
  check_string_list "Bob should be retracted from current db" [ "Alice" ] (string_values db ~a:"name" ());
  check_bool "Bob should remain in history" true
    (List.mem "Bob" (string_values (history db) ~a:"name" ()));
  let db = db_with [ PurgeEntity (Lookup_ref ("name", String "Bob")) ] db in
  check_bool "Bob should be purged from history" false
    (List.mem "Bob" (string_values (history db) ~a:"name" ()))

let test_purge_missing_entity_fails () =
  let db = setup_db () in
  let db = db_with [ PurgeEntity (Lookup_ref ("name", String "Alice")) ] db in
  expect_invalid_arg (fun () -> ignore (db_with [ PurgeEntity (Lookup_ref ("name", String "Alice")) ] db))

let () =
  run "purge"
    [
      ( "operations"
      , [
          test_case "purge datom from current and history" `Quick test_purge_datom_from_current_and_history
        ; test_case "purge attribute" `Quick test_purge_attribute
        ; test_case "purge entity" `Quick test_purge_entity
        ; test_case "purge missing entity fails" `Quick test_purge_missing_entity_fails
        ] )
    ]
