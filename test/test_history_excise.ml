(* Comprehensive history + Datahike-compatible excise (purge) coverage.
   Covers temporal views, retract vs purge, EDN purge ops, components/refs,
   noHistory, purge_history_before, and query visibility after excision. *)

open Alcotest
open Datascript

let check_int = Test_alcotest_support.check_int
let check_bool = Test_alcotest_support.check_bool
let check_string_list = Test_alcotest_support.check_string_list
let check_int_list = Test_alcotest_support.check_int_list
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

let many = { indexed with cardinality = Many; indexed = false }
let unique_identity = { indexed with unique = Some Identity }
let no_history_attr = { indexed with no_history = true }
let ref_one = { indexed with value_type = Some RefType }
let component = { ref_one with is_component = true }

let datoms_list db index ?e ?a ?v ?tx () =
  datoms db index ?e ?a ?v ?tx () |> List.of_seq

let int_values db ?a ?e () =
  datoms_list db Eavt ?a ?e ()
  |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  |> List.sort compare

let string_values db ?a ?e () =
  datoms_list db Eavt ?a ?e ()
  |> List.map (fun d -> match d.v with String s -> s | _ -> "")
  |> List.sort compare

let history_asserted_ints db ?a ?e () =
  datoms_list (history db) Eavt ?a ?e ()
  |> List.filter (fun d -> d.added)
  |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  |> List.sort compare

let history_all_ints db ?a ?e () =
  datoms_list (history db) Eavt ?a ?e ()
  |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  |> List.sort compare

let history_asserted_strings db ?a ?e () =
  datoms_list (history db) Eavt ?a ?e ()
  |> List.filter (fun d -> d.added)
  |> List.map (fun d -> match d.v with String s -> s | _ -> "")
  |> List.sort compare

let setup_people () =
  db_with
    [ Add (Entity_id 1, "name", String "Alice")
    ; Add (Entity_id 1, "age", Int 25)
    ; Add (Entity_id 2, "name", String "Bob")
    ; Add (Entity_id 2, "age", Int 35)
    ]
    (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())

(* ---------- history temporal API ---------- *)

let test_history_timeline_and_views () =
  let db = setup_people () in
  let tx0 = basis_tx db in
  let db = db_with [ Add (Entity_id 1, "age", Int 30) ] db in
  let tx1 = basis_tx db in
  let db = db_with [ Retract (Entity_id 1, "name", Some (String "Alice")) ] db in
  check_int "basis advanced after updates" 1 (if tx1 > tx0 then 1 else 0);
  check_int_list "current keeps latest age" [ 30 ] (int_values db ~e:1 ~a:"age" ());
  check_string_list "current drops retracted name" [] (string_values db ~e:1 ~a:"name" ());
  let hist = history db in
  check_bool "history flag" true (is_history hist);
  check_bool "history is temporal" true (temporal_view hist);
  check_int_list "history keeps age trail" [ 25; 30 ] (history_asserted_ints hist ~e:1 ~a:"age" ());
  let past = as_of tx0 hist in
  check_int_list "history+as_of sees bootstrap age" [ 25 ] (int_values past ~e:1 ~a:"age" ());
  let delta = since tx0 hist in
  check_int_list "history+since sees post-bootstrap age" [ 30 ]
    (history_asserted_ints delta ~e:1 ~a:"age" ());
  let retracted =
    datoms_list hist Eavt ~e:1 ~a:"name" ()
    |> List.filter (fun d -> not d.added)
    |> List.map (fun d -> match d.v with String s -> s | _ -> "")
  in
  check_string_list "history exposes name retraction" [ "Alice" ] retracted

let test_no_history_attr_skips_prior_facts () =
  let db =
    empty_db ~schema:[ "name", unique_identity; "secret", no_history_attr ] ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "secret", String "one") ]
    |> db_with
         [ Add (Entity_id 1, "name", String "Alicia"); Add (Entity_id 1, "secret", String "two") ]
  in
  check_string_list "current secret is latest" [ "two" ] (string_values db ~a:"secret" ());
  check_string_list "history does not retain prior noHistory values" [ "two" ]
    (history_asserted_strings db ~a:"secret" ());
  check_string_list "history retains normal attr trail" [ "Alice"; "Alicia" ]
    (history_asserted_strings db ~a:"name" ())

(* ---------- retract vs excise (purge) ---------- *)

let test_retract_keeps_history_purge_excises () =
  let db = setup_people () in
  let db = db_with [ RetractAttr (Lookup_ref ("name", String "Alice"), "age") ] db in
  check_int_list "retract removes from current" [] (int_values db ~e:1 ~a:"age" ());
  check_int_list "retract keeps asserted age in history" [ 25 ]
    (history_asserted_ints db ~e:1 ~a:"age" ());
  let report =
    with_tx db [ Purge (Lookup_ref ("name", String "Alice"), "age", Int 25) ]
  in
  check_int "purge reports removed datoms" 1 (if report.purged_datoms <> [] then 1 else 0);
  check_int "tx_data empty for purge-only" 0 (List.length report.tx_data);
  check_int_list "purged fact gone from current" []
    (int_values report.db_after ~e:1 ~a:"age" ());
  check_int_list "purged fact excised from history" []
    (history_all_ints report.db_after ~e:1 ~a:"age" ());
  check_int_list "unrelated Bob age remains" [ 35 ]
    (history_asserted_ints report.db_after ~e:2 ~a:"age" ())

let test_purge_attribute_and_entity () =
  let db = setup_people () in
  let db = db_with [ PurgeAttr (Lookup_ref ("name", String "Alice"), "age") ] db in
  check_int_list "purge attr clears current" [] (int_values db ~e:1 ~a:"age" ());
  check_int_list "purge attr clears history" [] (history_all_ints db ~e:1 ~a:"age" ());
  check_string_list "purge attr keeps name" [ "Alice"; "Bob" ] (string_values db ~a:"name" ());
  let db = db_with [ PurgeEntity (Lookup_ref ("name", String "Alice")) ] db in
  check_string_list "purge entity removes from current" [ "Bob" ] (string_values db ~a:"name" ());
  let hist_names =
    string_values (history db) ~a:"name" ()
    |> List.filter (fun s -> s = "Alice" || s = "Bob")
    |> List.sort compare
  in
  check_string_list "purge entity removes from history" [ "Bob" ] hist_names

let test_edn_purge_ops () =
  let db = setup_people () in
  let db = db_with_string "[[:db/purge [:name \"Bob\"] :age 35]]" db in
  check_int_list "EDN db/purge clears current" [] (int_values db ~e:2 ~a:"age" ());
  check_int_list "EDN db/purge clears history" [] (history_all_ints db ~e:2 ~a:"age" ());
  let db = db_with_string "[[:db.purge/attribute [:name \"Alice\"] :age]]" db in
  check_int_list "EDN purge/attribute clears Alice age history" []
    (history_all_ints db ~e:1 ~a:"age" ());
  let db = setup_people () in
  let db = db_with_string "[[:db.purge/entity [:name \"Alice\"]]]" db in
  check_bool "EDN purge/entity removes Alice from history" false
    (List.mem "Alice" (string_values (history db) ~a:"name" ()))

let test_purge_entity_removes_refs_and_components () =
  let db =
    empty_db
      ~schema:
        [ "name", unique_identity
        ; "profile", component
        ; "friend", ref_one
        ; "title", indexed
        ]
      ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Alice")
         ; Add (Entity_id 10, "title", String "bio")
         ; Add (Entity_id 1, "profile", Ref 10)
         ; Add (Entity_id 2, "name", String "Bob")
         ; Add (Entity_id 2, "friend", Ref 1)
         ]
  in
  check_string_list "component present before purge" [ "bio" ]
    (string_values db ~e:10 ~a:"title" ());
  let db = db_with [ PurgeEntity (Lookup_ref ("name", String "Alice")) ] db in
  check_string_list "Alice gone from current" [ "Bob" ] (string_values db ~a:"name" ());
  check_bool "Alice gone from history" false
    (List.mem "Alice" (string_values (history db) ~a:"name" ()));
  check_string_list "component excised with entity" []
    (string_values (history db) ~e:10 ~a:"title" ());
  let friend_refs =
    datoms_list (history db) Eavt ~e:2 ~a:"friend" ()
    |> List.map (fun d -> match d.v with Ref n -> n | _ -> -1)
    |> List.sort compare
  in
  check_int_list "incoming friend ref excised" [] friend_refs;
  check_string_list "Bob survives" [ "Bob" ] (string_values db ~a:"name" ())

let test_purge_missing_fails () =
  let db = setup_people () in
  let db = db_with [ PurgeEntity (Lookup_ref ("name", String "Alice")) ] db in
  expect_invalid_arg (fun () ->
    ignore (db_with [ PurgeEntity (Lookup_ref ("name", String "Alice")) ] db));
  expect_invalid_arg (fun () ->
    ignore (db_with [ Purge (Entity_id 2, "age", Int 99) ] db))

let test_temporal_views_reject_purge () =
  let db = setup_people () in
  let tx0 = basis_tx db in
  expect_invalid_arg (fun () ->
    ignore (transact (history db) [ Purge (Entity_id 1, "age", Int 25) ]));
  expect_invalid_arg (fun () ->
    ignore (transact (as_of tx0 db) [ PurgeAttr (Entity_id 1, "age") ]));
  expect_invalid_arg (fun () ->
    ignore (transact (since tx0 db) [ PurgeEntity (Entity_id 1) ]))

(* ---------- purge_history_before + query ---------- *)

let test_purge_history_before_and_query () =
  let db =
    empty_db ~schema:[ "name", unique_identity; "age", indexed ] ()
    |> fun db ->
    (transact
       ~tx_meta:[ "db/txInstant", Instant 1_000 ]
       db
       [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 25) ]).db_after
  in
  let tx0 = basis_tx db in
  let db =
    (transact
       ~tx_meta:[ "db/txInstant", Instant 2_000 ]
       db
       [ Add (Entity_id 1, "age", Int 30) ]).db_after
  in
  let past = as_of_instant (Instant 1_500) db in
  check_int "as_of_instant resolves first tx" tx0 (basis_tx past);
  check_int_list "as_of_instant age" [ 25 ] (int_values past ~e:1 ~a:"age" ());
  check_int_list "pre-compaction history ages" [ 25; 30 ]
    (history_asserted_ints db ~e:1 ~a:"age" ());
  let db2, removed = purge_history_before (basis_tx db) db in
  check_int "purge_history_before removed superseded" 1 (if removed <> [] then 1 else 0);
  check_int_list "current age survives compaction" [ 30 ] (int_values db2 ~e:1 ~a:"age" ());
  check_int_list "old age excised by compaction" []
    (history_asserted_ints db2 ~e:1 ~a:"age" () |> List.filter (( = ) 25));
  let ages =
    q_string (history db2) "[:find ?a :where [1 :age ?a]]"
    |> List.map (function
      | [ Result_value (Int n) ] -> string_of_int n
      | _ -> "?")
    |> List.sort compare
  in
  check_string_list "history query after compaction sees current age only" [ "30" ] ages

let test_history_query_after_selective_excise () =
  let db =
    empty_db ~schema:[ "name", unique_identity; "age", indexed; "tag", many ] ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Alice")
         ; Add (Entity_id 1, "age", Int 25)
         ; Add (Entity_id 1, "tag", String "a")
         ; Add (Entity_id 1, "tag", String "b")
         ; Add (Entity_id 2, "name", String "Bob")
         ; Add (Entity_id 2, "age", Int 35)
         ]
    |> db_with [ Add (Entity_id 1, "age", Int 30) ]
  in
  let db = db_with [ Purge (Entity_id 1, "age", Int 25) ] db in
  let ages =
    q_string (history db) "[:find ?a :where [1 :age ?a]]"
    |> List.map (function
      | [ Result_value (Int n) ] -> n
      | _ -> -1)
    |> List.sort compare
  in
  check_int_list "history query drops excised age 25" [ 30 ] ages;
  let tags =
    q_string (history db) "[:find ?t :where [1 :tag ?t]]"
    |> List.map (function
      | [ Result_value (String s) ] -> s
      | _ -> "")
    |> List.sort compare
  in
  check_string_list "unrelated many-attr history intact" [ "a"; "b" ] tags;
  let db = db_with [ PurgeAttr (Entity_id 1, "tag") ] db in
  check_string_list "purge attr clears many values from history" []
    (history_asserted_strings db ~e:1 ~a:"tag" ())

let test_cardinality_many_history_then_purge_one () =
  let db =
    empty_db ~schema:[ "name", unique_identity; "tag", many ] ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Alice")
         ; Add (Entity_id 1, "tag", String "a")
         ; Add (Entity_id 1, "tag", String "b")
         ]
    |> db_with [ Retract (Entity_id 1, "tag", Some (String "a")) ]
  in
  check_string_list "current keeps surviving tag" [ "b" ] (string_values db ~a:"tag" ());
  check_string_list "history keeps both asserts" [ "a"; "b" ]
    (history_asserted_strings db ~a:"tag" ());
  let db = db_with [ Purge (Entity_id 1, "tag", String "a") ] db in
  check_string_list "excising retracted many-value leaves other" [ "b" ]
    (history_asserted_strings db ~a:"tag" ());
  check_string_list "current still has b" [ "b" ] (string_values db ~a:"tag" ())

let () =
  run "history+excise"
    [ ( "history"
      , [ test_case "timeline and as_of/since composition" `Quick test_history_timeline_and_views
        ; test_case "noHistory skips prior facts" `Quick test_no_history_attr_skips_prior_facts
        ; test_case "cardinality many history then purge one" `Quick
            test_cardinality_many_history_then_purge_one
        ] )
    ; ( "excise"
      , [ test_case "retract keeps history; purge excises" `Quick
            test_retract_keeps_history_purge_excises
        ; test_case "purge attribute and entity" `Quick test_purge_attribute_and_entity
        ; test_case "EDN purge ops" `Quick test_edn_purge_ops
        ; test_case "purge entity removes refs and components" `Quick
            test_purge_entity_removes_refs_and_components
        ; test_case "purge missing fails" `Quick test_purge_missing_fails
        ; test_case "temporal views reject purge" `Quick test_temporal_views_reject_purge
        ; test_case "purge_history_before and query" `Quick test_purge_history_before_and_query
        ; test_case "history query after selective excise" `Quick
            test_history_query_after_selective_excise
        ] )
    ]
