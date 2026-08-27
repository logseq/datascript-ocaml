open Datascript

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
  if int_values db ~a:"age" ~e:1 () <> [] then failwith "Alice age should be absent after retract";
  if history_int_values db ~a:"age" ~e:1 () <> [ 25 ] then
    failwith "Alice age should remain in history after retract";
  let db = db_with [ Purge (Lookup_ref ("name", String "Bob"), "age", Int 35) ] db in
  if int_values db ~a:"age" ~e:2 () <> [] then failwith "Bob age should be absent after purge";
  if history_all_int_values db ~a:"age" ~e:2 () <> [] then
    failwith "Bob age should be absent from history after purge";
  let db = db_with [ Purge (Lookup_ref ("name", String "Alice"), "age", Int 25) ] db in
  if history_all_int_values db ~a:"age" ~e:1 () <> [] then
    failwith "purged retracted datom should leave history"

let test_purge_attribute () =
  let db = setup_db () in
  let db = db_with [ PurgeAttr (Lookup_ref ("name", String "Alice"), "age") ] db in
  if int_values db ~a:"age" ~e:1 () <> [] then failwith "Alice age should be absent after attribute purge";
  if history_all_int_values db ~a:"age" ~e:1 () <> [] then
    failwith "Alice age should be absent from history";
  if string_values db ~a:"name" () <> [ "Alice"; "Bob" ] then failwith "Alice name should remain";
  let db = setup_db () in
  let db = db_with [ RetractAttr (Lookup_ref ("name", String "Bob"), "age") ] db in
  if int_values db ~a:"age" ~e:2 () <> [] then failwith "Bob age should be absent after retract attribute";
  if history_int_values db ~a:"age" ~e:2 () <> [ 35 ] then failwith "Bob age should remain in history";
  let db = db_with [ PurgeAttr (Lookup_ref ("name", String "Bob"), "age") ] db in
  if history_all_int_values db ~a:"age" ~e:2 () <> [] then failwith "Bob age should be purged from history"

let test_purge_entity () =
  let db = setup_db () in
  let db = db_with [ PurgeEntity (Lookup_ref ("name", String "Alice")) ] db in
  if string_values db ~a:"name" () <> [ "Bob" ] then failwith "Alice should be removed from current db";
  if string_values (history db) ~a:"name" () <> [ "Bob" ] then failwith "Alice should be removed from history";
  let db = setup_db () in
  let db = db_with [ RetractEntity (Lookup_ref ("name", String "Bob")) ] db in
  if string_values db ~a:"name" () <> [ "Alice" ] then failwith "Bob should be retracted from current db";
  if not (List.mem "Bob" (string_values (history db) ~a:"name" ())) then
    failwith "Bob should remain in history";
  let db = db_with [ PurgeEntity (Lookup_ref ("name", String "Bob")) ] db in
  if List.mem "Bob" (string_values (history db) ~a:"name" ()) then
    failwith "Bob should be purged from history"

let test_purge_missing_entity_fails () =
  let db = setup_db () in
  let db = db_with [ PurgeEntity (Lookup_ref ("name", String "Alice")) ] db in
  (match db_with [ PurgeEntity (Lookup_ref ("name", String "Alice")) ] db with
   | exception Invalid_argument message when
     (try
        let len = String.length "to be purged" in
        String.length message >= len
        && String.sub message (String.length message - len) len = "to be purged"
      with _ -> false) ->
     ()
   | _ -> failwith "expected purge of missing entity to fail")

let () =
  test_purge_datom_from_current_and_history ();
  test_purge_attribute ();
  test_purge_entity ();
  test_purge_missing_entity_fails ()
