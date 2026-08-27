open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_int label expected actual =
  if expected <> actual then failf "%s: expected %d, got %d" label expected actual

let assert_equal_bool label expected actual =
  if expected <> actual then failf "%s: expected %b, got %b" label expected actual

let assert_equal_string_list label expected actual =
  if expected <> actual then
    failf "%s: expected [%s], got [%s]" label (String.concat "; " expected) (String.concat "; " actual)

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
  datoms_list db ?a ?e ()
  |> List.map (fun d -> match d.v with String s -> s | _ -> "")
  |> List.sort compare

let history_asserted_values db ?a ?e () =
  datoms_list (history db) Eavt ?a ?e ()
  |> List.filter (fun d -> d.added)
  |> List.map (fun d -> match d.v with Int n -> string_of_int n | String s -> s | _ -> "?")
  |> List.sort compare

let expect_invalid_arg f =
  match f () with
  | exception Invalid_argument _ -> ()
  | _ -> failwith "expected Invalid_argument"

let test_basis_tx_tracks_latest_transaction () =
  let db =
    empty_db ~schema:[ "age", indexed ] ()
    |> db_with [ Add (Entity_id 1, "age", Int 25) ]
  in
  let tx0 = basis_tx db in
  let db = db_with [ Add (Entity_id 1, "age", Int 30) ] db in
  let tx1 = basis_tx db in
  assert_equal_int "basis advances across transactions" 1 (if tx1 > tx0 then 1 else 0);
  assert_equal_int "current view uses latest basis" 30 (List.hd (int_values db ~a:"age" ()));

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
  assert_equal_int "as_of lowers basis_tx" tx0 (basis_tx past);
  assert_equal_int "as_of is a temporal view" 1 (if temporal_view past then 1 else 0);
  assert_equal_bool "as_of is not history" false (is_history past);
  assert_equal_string_list "as_of tx0 sees Alice age 25" [ "25" ]
    (List.map string_of_int (int_values past ~e:1 ~a:"age" ()));
  assert_equal_string_list "as_of tx0 sees Bob age 35" [ "35" ]
    (List.map string_of_int (int_values past ~e:2 ~a:"age" ()));

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
  assert_equal_int "since keeps latest basis_tx" (basis_tx db) (basis_tx delta);
  assert_equal_int "since is a temporal view" 1 (if temporal_view delta then 1 else 0);
  assert_equal_bool "since is not history" false (is_history delta);
  assert_equal_string_list "since after tx0 only sees Carol" [ "Carol" ] (string_values delta ~a:"name" ());

let test_history_exposes_assertions_and_retractions () =
  let db =
    db_with
      [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 25) ]
      (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())
  in
  let db = db_with [ Add (Entity_id 1, "age", Int 30) ] db in
  let db = db_with [ Retract (Entity_id 1, "name", Some (String "Alice")) ] db in
  assert_equal_string_list "current db keeps latest age only" [ "30" ]
    (List.map string_of_int (int_values db ~a:"age" ()));
  assert_equal_string_list "current db drops retracted name" [] (string_values db ~a:"name" ());
  let hist = history db in
  assert_equal_bool "history enables history flag" true (is_history hist);
  assert_equal_int "history is temporal" 1 (if temporal_view hist then 1 else 0);
  assert_equal_string_list "history keeps asserted ages" [ "25"; "30" ] (history_asserted_values hist ~a:"age" ());
  let retracted_names =
    datoms_list hist Eavt ~a:"name" ()
    |> List.filter (fun d -> not d.added)
    |> List.map (fun d -> match d.v with String s -> s | _ -> "")
  in
  if retracted_names <> [ "Alice" ] then
    failf "history should expose retraction datoms, got [%s]" (String.concat "; " retracted_names);
  ()

let test_history_survives_entity_retraction () =
  let db =
    db_with
      [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 25) ]
      (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())
  in
  let tx0 = basis_tx db in
  let db = db_with [ Add (Entity_id 1, "age", Int 30) ] db in
  let tx1 = basis_tx db in
  let db = db_with [ RetractEntity (Entity_id 1) ] db in
  assert_equal_string_list "retracted entity absent from current db" [] (int_values db ~e:1 ~a:"age" ());
  let hist = history db in
  assert_equal_string_list "history after retraction keeps age trail" [ "25"; "30" ]
    (history_asserted_values hist ~e:1 ~a:"age" ());
  let past = as_of tx0 hist in
  assert_equal_string_list "history + as_of tx0 sees bootstrap age" [ "25" ]
    (List.map string_of_int (int_values past ~e:1 ~a:"age" ()));
  let delta = since tx1 hist in
  assert_equal_string_list "history + since tx1 sees post-update age only" [ "30" ]
    (history_asserted_values delta ~e:1 ~a:"age" ());

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
    ignore (transact (history db) [ Add (Entity_id 2, "name", String "Bob") ]));

let test_as_of_beyond_store_basis_fails () =
  let db =
    db_with [ Add (Entity_id 1, "name", String "Alice") ] (empty_db ~schema:[ "name", indexed ] ())
  in
  expect_invalid_arg (fun () -> ignore (as_of (basis_tx db + 1) db));

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
  assert_equal_int "input basis unchanged" tx0 (basis_tx db);
  assert_equal_bool "input is not temporal" false (temporal_view db);
  assert_equal_bool "input is not history" false (is_history db);
  if datoms_list db Eavt () <> before_datoms then failwith "view constructors must not mutate input db";

let test_with_tx_preserves_db_before_basis () =
  let db =
    db_with [ Add (Entity_id 1, "name", String "Alice") ] (empty_db ~schema:[ "name", indexed ] ())
  in
  let before_basis = basis_tx db in
  let report = with_tx db [ Add (Entity_id 2, "name", String "Bob") ] in
  assert_equal_int "original db basis unchanged" before_basis (basis_tx db);
  assert_equal_int "db_before pins old basis" before_basis (basis_tx report.db_before);
  assert_equal_int "db_after advances basis" 1 (if basis_tx report.db_after > before_basis then 1 else 0);

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
  assert_equal_string_list "history then as_of tx0 sees bootstrap ages" [ "25"; "35" ]
    (List.map string_of_int (int_values bootstrap ~a:"age" ()));

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
  if eavt <> aevt then failwith "as_of view should return consistent EAVT and AEVT slices";

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
  assert_equal_string_list "current many attr keeps surviving value" [ "b" ] (string_values db ~a:"tag" ());
  assert_equal_string_list "history many attr keeps both assertions" [ "a"; "b" ]
    (history_asserted_values db ~a:"tag" ());

let test_public_api_aliases () =
  let db =
    db_with [ Add (Entity_id 1, "name", String "Alice") ] (empty_db ~schema:[ "name", indexed ] ())
  in
  let tx0 = basis_tx db in
  assert_equal_bool "plain db is not history" false (is_history db);
  (match (as_of_t db, as_of_tx db, since_t db, since_tx db) with
   | None, None, None, None -> ()
   | _ -> failwith "plain db should not expose temporal markers");
  let past = as_of tx0 db in
  assert_equal_bool "is_history mirrors history flag" true (is_history (history db));
  assert_equal_bool "is_history false on as_of" false (is_history past);

let () =
  test_basis_tx_tracks_latest_transaction ();
  test_as_of_point_in_time ();
  test_since_delta_is_exclusive ();
  test_history_exposes_assertions_and_retractions ();
  test_history_survives_entity_retraction ();
  test_temporal_views_reject_transact ();
  test_as_of_beyond_store_basis_fails ();
  test_view_constructors_do_not_mutate_input_db ();
  test_with_tx_preserves_db_before_basis ();
  test_history_as_of_composition ();
  test_temporal_views_preserve_index_parity ();
  test_history_cardinality_many ();
  test_public_api_aliases ();
  Printf.printf "test_tx_history: ok\n"
