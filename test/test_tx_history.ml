open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_int label expected actual =
  if expected <> actual then failf "%s: expected %d, got %d" label expected actual

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

let ages db =
  datoms db Eavt ~a:":age" ()
  |> Seq.map (fun d -> d.v)
  |> List.of_seq

let test_history_exposes_retractions () =
  let db =
    db_with
      [ Add (Entity_id 1, ":name", String "Alice")
      ; Add (Entity_id 1, ":age", Int 30)
      ]
      (empty_db ~schema:[ "name", unique_identity; "age", indexed ] ())
  in
  let tx1 = basis_tx db in
  let db = db_with [ Add (Entity_id 1, ":age", Int 31) ] db in
  let current =
    ages db
    |> List.map (function Int n -> n | _ -> -1)
  in
  assert_equal_int "current db keeps latest age" 1 (List.length current);
  if current <> [ 31 ] then failf "current ages should be [31], got %S" (string_of_int (List.hd current));
  let past = as_of tx1 db in
  let past_ages =
    ages past
    |> List.map (function Int n -> n | _ -> -1)
  in
  if past_ages <> [ 30 ] then failf "as_of should see age 30, got %d entries" (List.length past_ages);
  let hist = history db in
  let hist_ages =
    datoms hist Eavt ~a:":age" ()
    |> Seq.filter (fun d -> d.added)
    |> Seq.map (fun d -> match d.v with Int n -> n | _ -> -1)
    |> List.of_seq
    |> List.sort compare
  in
  if hist_ages <> [ 30; 31 ] then
    failf "history should expose both asserted ages, got [%s]"
      (String.concat "; " (List.map string_of_int hist_ages))

let test_since_sees_post_tx_datoms () =
  let db =
    db_with
      [ Add (Entity_id 1, ":name", String "Alice")
      ; Add (Entity_id 2, ":name", String "Bob")
      ]
      (empty_db ~schema:[ "name", unique_identity ] ())
  in
  let tx1 = basis_tx db in
  let db = db_with [ Add (Entity_id 3, ":name", String "Carol") ] db in
  let names delta =
    datoms delta Aevt ~a:":name" ()
    |> Seq.map (fun d -> match d.v with String s -> s | _ -> "")
    |> List.of_seq
    |> List.sort compare
  in
  if names db <> [ "Alice"; "Bob"; "Carol" ] then failf "current db missing Carol";
  let delta = since tx1 db in
  if names delta <> [ "Carol" ] then failf "since tx1 should only see Carol"

let test_with_tx_preserves_db_before_basis () =
  let db =
    db_with [ Add (Entity_id 1, ":name", String "Alice") ] (empty_db ~schema:[ "name", indexed ] ())
  in
  let before_basis = basis_tx db in
  let report =
    with_tx db [ Add (Entity_id 2, ":name", String "Bob") ]
  in
  assert_equal_int "input db unchanged" before_basis (basis_tx db);
  assert_equal_int "db_before pins old basis" before_basis (basis_tx report.db_before);
  assert_equal_int "db_after advances basis" 1 (if basis_tx report.db_after > before_basis then 1 else 0)

let () =
  test_history_exposes_retractions ();
  test_since_sees_post_tx_datoms ();
  test_with_tx_preserves_db_before_basis ();
  Printf.printf "test_tx_history: ok\n"
