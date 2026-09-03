open Alcotest
open Datascript

let check_int = Test_alcotest_support.check_int
let check_bool = Test_alcotest_support.check_bool
let check_string_list = Test_alcotest_support.check_string_list
let expect_invalid_arg = Test_alcotest_support.expect_invalid_arg

let datoms_list db index ?e ?a ?v ?tx () =
  datoms db index ?e ?a ?v ?tx () |> List.of_seq

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

let unique_identity = { indexed with unique = Some Identity }

let int_values db ?a ?e () =
  datoms_list db Eavt ?a ?e ()
  |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  |> List.sort compare

let string_values db ?a ?e () =
  datoms_list db Eavt ?a ?e ()
  |> List.map (fun d -> match d.v with String s -> s | _ -> "")
  |> List.sort compare

let history_asserted_values db ?a ?e () =
  datoms_list (history db) Eavt ?a ?e ()
  |> List.filter (fun d -> d.added)
  |> List.map (fun d -> match d.v with Int n -> string_of_int n | String s -> s | _ -> "?")
  |> List.sort compare

let test_basis_tx_tracks_latest_transaction () =
  let db =
    empty_db ~schema:[ "age", indexed ] ()
    |> db_with [ Add (Entity_id 1, "age", Int 25) ]
  in
  let tx0 = basis_tx db in
  let db = db_with [ Add (Entity_id 1, "age", Int 30) ] db in
  let tx1 = basis_tx db in
  check_int "basis advances across transactions" 1 (if tx1 > tx0 then 1 else 0);
  check_int "current view uses latest basis" 30 (List.hd (int_values db ~a:"age" ()))

let test_as_of_point_in_time () =
  let db =
    db_with
      [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 25)
      ; Add (Entity_id 2, "name", String "Bob"); Add (Entity_id 2, "age", Int 35)
      ]
      (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())
  in
  let tx0 = basis_tx db in
  let db = db_with [ Add (Entity_id 1, "age", Int 30) ] db in
  let past = as_of tx0 db in
  (match as_of_t past, as_of_tx past with
   | Some tx, Some tx' when tx = tx0 && tx' = tx0 -> ()
   | _ -> failwith "as_of should expose as_of_t and as_of_tx");
  check_int "as_of lowers basis_tx" tx0 (basis_tx past);
  check_int "as_of is a temporal view" 1 (if temporal_view past then 1 else 0);
  check_bool "as_of is not history" false (is_history past);
  check_string_list "as_of tx0 sees Alice age 25" [ "25" ]
    (List.map string_of_int (int_values past ~e:1 ~a:"age" ()));
  check_string_list "as_of tx0 sees Bob age 35" [ "35" ]
    (List.map string_of_int (int_values past ~e:2 ~a:"age" ()))

let test_since_delta_is_exclusive () =
  let db =
    db_with
      [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 2, "name", String "Bob") ]
      (empty_db ~schema:[ "name", unique_identity ] ())
  in
  let tx0 = basis_tx db in
  let db = db_with [ Add (Entity_id 3, "name", String "Carol") ] db in
  let delta = since tx0 db in
  (match since_t delta, since_tx delta with
   | Some tx, Some tx' when tx = tx0 && tx' = tx0 -> ()
   | _ -> failwith "since should expose since_t and since_tx");
  check_int "since keeps latest basis_tx" (basis_tx db) (basis_tx delta);
  check_int "since is a temporal view" 1 (if temporal_view delta then 1 else 0);
  check_bool "since is not history" false (is_history delta);
  check_string_list "since after tx0 only sees Carol" [ "Carol" ] (string_values delta ~a:"name" ())

let test_history_exposes_assertions_and_retractions () =
  let db =
    db_with
      [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 25) ]
      (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())
  in
  let db = db_with [ Add (Entity_id 1, "age", Int 30) ] db in
  let db = db_with [ Retract (Entity_id 1, "name", Some (String "Alice")) ] db in
  check_string_list "current db keeps latest age only" [ "30" ]
    (List.map string_of_int (int_values db ~a:"age" ()));
  check_string_list "current db drops retracted name" [] (string_values db ~a:"name" ());
  let hist = history db in
  check_bool "history enables history flag" true (is_history hist);
  check_int "history is temporal" 1 (if temporal_view hist then 1 else 0);
  check_string_list "history keeps asserted ages" [ "25"; "30" ] (history_asserted_values hist ~a:"age" ());
  let retracted_names =
    datoms_list hist Eavt ~a:"name" ()
    |> List.filter (fun d -> not d.added)
    |> List.map (fun d -> match d.v with String s -> s | _ -> "")
  in
  check_string_list "history exposes retraction datoms" [ "Alice" ] retracted_names

let test_history_survives_entity_retraction () =
  let db =
    db_with
      [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 25) ]
      (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())
  in
  let tx0 = basis_tx db in
  let db = db_with [ Add (Entity_id 1, "age", Int 30) ] db in
  ignore (basis_tx db);
  let db = db_with [ RetractEntity (Entity_id 1) ] db in
  check_string_list "retracted entity absent from current db" []
    (List.map string_of_int (int_values db ~e:1 ~a:"age" ()));
  let hist = history db in
  check_string_list "history after retraction keeps age trail" [ "25"; "30" ]
    (history_asserted_values hist ~e:1 ~a:"age" ());
  let past = as_of tx0 hist in
  check_string_list "history + as_of tx0 sees bootstrap age" [ "25" ]
    (List.map string_of_int (int_values past ~e:1 ~a:"age" ()));
  let delta = since tx0 hist in
  check_string_list "history + since tx0 sees post-update age only" [ "30" ]
    (history_asserted_values delta ~e:1 ~a:"age" ())

let test_temporal_views_reject_transact () =
  let db =
    db_with [ Add (Entity_id 1, "name", String "Alice") ] (empty_db ~schema:[ "name", indexed ] ())
  in
  let tx0 = basis_tx db in
  expect_invalid_arg (fun () ->
    ignore (transact (as_of tx0 db) [ Add (Entity_id 2, "name", String "Bob") ]));
  expect_invalid_arg (fun () ->
    ignore (transact (since tx0 db) [ Add (Entity_id 2, "name", String "Bob") ]));
  expect_invalid_arg (fun () ->
    ignore (transact (history db) [ Add (Entity_id 2, "name", String "Bob") ]))

let test_as_of_beyond_store_basis_fails () =
  let db =
    db_with [ Add (Entity_id 1, "name", String "Alice") ] (empty_db ~schema:[ "name", indexed ] ())
  in
  expect_invalid_arg (fun () -> ignore (as_of (basis_tx db + 1) db))

let test_view_constructors_do_not_mutate_input_db () =
  let db =
    db_with
      [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 25) ]
      (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())
  in
  let tx0 = basis_tx db in
  let before_datoms = datoms_list db Eavt () in
  ignore (as_of tx0 db);
  ignore (since tx0 db);
  ignore (history db);
  check_int "input basis unchanged" tx0 (basis_tx db);
  check_bool "input is not temporal" false (temporal_view db);
  check_bool "input is not history" false (is_history db);
  check_bool "view constructors must not mutate input db"
    true
    (datoms_list db Eavt () = before_datoms)

let test_with_tx_preserves_db_before_basis () =
  let db =
    db_with [ Add (Entity_id 1, "name", String "Alice") ] (empty_db ~schema:[ "name", indexed ] ())
  in
  let before_basis = basis_tx db in
  let report = with_tx db [ Add (Entity_id 2, "name", String "Bob") ] in
  check_int "original db basis unchanged" before_basis (basis_tx db);
  check_int "db_before pins old basis" before_basis (basis_tx report.db_before);
  check_int "db_after advances basis" 1 (if basis_tx report.db_after > before_basis then 1 else 0)

let test_history_as_of_composition () =
  let db =
    db_with
      [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 25)
      ; Add (Entity_id 2, "name", String "Bob"); Add (Entity_id 2, "age", Int 35)
      ]
      (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())
  in
  let tx0 = basis_tx db in
  let db = db_with [ Add (Entity_id 1, "age", Int 30) ] db in
  let bootstrap = as_of tx0 (history db) in
  check_string_list "history then as_of tx0 sees bootstrap ages" [ "25"; "35" ]
    (List.map string_of_int (int_values bootstrap ~a:"age" ()))

let test_temporal_views_preserve_index_parity () =
  let db =
    db_with
      [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 30) ]
      (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())
  in
  let tx0 = basis_tx db in
  let db = db_with [ Add (Entity_id 1, "age", Int 31) ] db in
  let past = as_of tx0 db in
  let eavt = datoms_list past Eavt ~e:1 ~a:"age" () |> List.map (fun d -> d.v) in
  let aevt = datoms_list past Aevt ~a:"age" () |> List.filter (fun d -> d.e = 1) |> List.map (fun d -> d.v) in
  check_bool "as_of view should return consistent EAVT and AEVT slices" true (eavt = aevt)

let test_history_cardinality_many () =
  let db =
    db_with
      [ Add (Entity_id 1, "name", String "Alice")
      ; Add (Entity_id 1, "tag", String "a")
      ; Add (Entity_id 1, "tag", String "b")
      ]
      (empty_db ~schema:[ "name", unique_identity; "tag", many ] ())
  in
  let db = db_with [ Retract (Entity_id 1, "tag", Some (String "a")) ] db in
  check_string_list "current many attr keeps surviving value" [ "b" ] (string_values db ~a:"tag" ());
  check_string_list "history many attr keeps both assertions" [ "a"; "b" ]
    (history_asserted_values db ~a:"tag" ())

let test_seek_respects_as_of_view () =
  let db =
    db_with
      [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 25)
      ; Add (Entity_id 2, "name", String "Bob"); Add (Entity_id 2, "age", Int 35)
      ]
      (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())
  in
  let tx0 = basis_tx db in
  let db = db_with [ Add (Entity_id 1, "age", Int 30) ] db in
  let past = as_of tx0 db in
  let seek_ages =
    seek_datoms past Aevt ~a:"age" ()
    |> List.of_seq
    |> List.filter (fun d -> d.e = 1)
    |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  in
  let rseek_ages =
    rseek_datoms past Aevt ~a:"age" ()
    |> List.of_seq
    |> List.filter (fun d -> d.e = 1)
    |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  in
  check_string_list "seek_datoms on as_of sees past age" [ "25" ] (List.map string_of_int seek_ages);
  check_string_list "rseek_datoms on as_of sees past age" [ "25" ] (List.map string_of_int rseek_ages)

let test_attr_caches_detach_and_preserve_untouched () =
  let db =
    db_with
      [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 25)
      ; Add (Entity_id 2, "name", String "Bob"); Add (Entity_id 2, "age", Int 35)
      ]
      (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())
  in
  (* Warm current-fact caches. *)
  ignore (datoms_list db Aevt ~a:"name" ());
  ignore (datoms_list db Aevt ~a:"age" ());
  check_int "name cache warmed" 1 (if Hashtbl.mem db.aevt_by_attr "name" then 1 else 0);
  check_int "age cache warmed" 1 (if Hashtbl.mem db.aevt_by_attr "age" then 1 else 0);
  let tx0 = basis_tx db in
  let past = as_of tx0 db in
  check_int "as_of detaches attr caches" 0 (Hashtbl.length past.aevt_by_attr);
  check_int "live name cache survives as_of" 1 (if Hashtbl.mem db.aevt_by_attr "name" then 1 else 0);
  let db = db_with [ Add (Entity_id 1, "age", Int 30) ] db in
  check_int "untouched name cache survives age write" 1 (if Hashtbl.mem db.aevt_by_attr "name" then 1 else 0);
  check_int "touched age cache invalidated" 0 (if Hashtbl.mem db.aevt_by_attr "age" then 1 else 0);
  check_string_list "name still readable after selective invalidate" [ "Alice"; "Bob" ]
    (string_values db ~a:"name" ())

let test_as_of_instant_and_purge_history_before () =
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
  check_int "as_of_instant resolves to first tx" tx0 (basis_tx past);
  check_string_list "as_of_instant sees age 25" [ "25" ]
    (List.map string_of_int (int_values past ~e:1 ~a:"age" ()));
  let db2, removed = purge_history_before (basis_tx db) db in
  check_int "purge_history_before removes superseded history" 1 (if removed <> [] then 1 else 0);
  check_string_list "current age remains after purge-before" [ "30" ]
    (List.map string_of_int (int_values db2 ~e:1 ~a:"age" ()));
  check_string_list "purged history no longer exposes old age" []
    (history_asserted_values db2 ~a:"age" ~e:1 ()
     |> List.filter (fun s -> s = "25"))

let test_public_api_aliases () =
  let db =
    db_with [ Add (Entity_id 1, "name", String "Alice") ] (empty_db ~schema:[ "name", indexed ] ())
  in
  let tx0 = basis_tx db in
  check_bool "plain db is not history" false (is_history db);
  (match (as_of_t db, as_of_tx db, since_t db, since_tx db) with
   | None, None, None, None -> ()
   | _ -> failwith "plain db should not expose temporal markers");
  let past = as_of tx0 db in
  check_bool "is_history mirrors history flag" true (is_history (history db));
  check_bool "is_history false on as_of" false (is_history past)

let test_history_datoms_since_returns_only_new_assertions_and_retractions () =
  let db =
    db_with
      [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 25) ]
      (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())
  in
  let checkpoint = basis_tx db in
  let db = db_with [ Add (Entity_id 1, "age", Int 30) ] db in
  let db = db_with [ Retract (Entity_id 1, "name", Some (String "Alice")) ] db in
  let delta = history_datoms_since checkpoint db in
  check_int "replacement and deletion include three history datoms" 3 (List.length delta);
  check_int "all returned datoms are newer than the checkpoint" 3
    (List.length (List.filter (fun datom -> datom.tx > checkpoint) delta));
  check_int "the deletion is retained" 1
    (List.length
       (List.filter
          (fun datom -> datom.a = "name" && not datom.added)
          delta))

let () =
  run "tx history"
    [
      ( "views"
      , [
          test_case "basis_tx tracks latest transaction" `Quick test_basis_tx_tracks_latest_transaction
        ; test_case "as_of point in time" `Quick test_as_of_point_in_time
        ; test_case "since delta is exclusive" `Quick test_since_delta_is_exclusive
        ; test_case "history exposes assertions and retractions" `Quick
            test_history_exposes_assertions_and_retractions
        ; test_case "history survives entity retraction" `Quick test_history_survives_entity_retraction
        ; test_case "temporal views reject transact" `Quick test_temporal_views_reject_transact
        ; test_case "as_of beyond store basis fails" `Quick test_as_of_beyond_store_basis_fails
        ; test_case "view constructors do not mutate input db" `Quick
            test_view_constructors_do_not_mutate_input_db
        ; test_case "with_tx preserves db_before basis" `Quick test_with_tx_preserves_db_before_basis
        ; test_case "history as_of composition" `Quick test_history_as_of_composition
        ; test_case "temporal views preserve index parity" `Quick test_temporal_views_preserve_index_parity
        ; test_case "history cardinality many" `Quick test_history_cardinality_many
        ; test_case "seek and rseek respect as_of" `Quick test_seek_respects_as_of_view
        ; test_case "attr caches detach and preserve untouched" `Quick
            test_attr_caches_detach_and_preserve_untouched
        ; test_case "as_of_instant and purge_history_before" `Quick
            test_as_of_instant_and_purge_history_before
        ; test_case "public api aliases" `Quick test_public_api_aliases
        ; test_case "history datoms since checkpoint" `Quick
            test_history_datoms_since_returns_only_new_assertions_and_retractions
        ] )
    ]
