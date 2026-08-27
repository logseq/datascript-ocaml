open Datascript_types
open Datascript.Tx_visibility

let datom ~e ~a ~v ~tx ~added =
  { e; a; v; tx; added }

let assert_equal_int label expected actual =
  if expected <> actual then
    Printf.ksprintf failwith "%s: expected %d, got %d" label expected actual

let assert_equal_bool label expected actual =
  if expected <> actual then
    Printf.ksprintf failwith "%s: expected %b, got %b" label expected actual

let test_datoms_filter_cancels_later_retract () =
  let d1 = datom ~e:1 ~a:":name" ~v:(String "Ivan") ~tx:100 ~added:true in
  let d2 = datom ~e:1 ~a:":name" ~v:(String "Ivan") ~tx:200 ~added:false in
  let result = datoms_filter [ d1; d2 ] in
  assert_equal_int "later retract cancels add" 0 (List.length result)

let test_datoms_filter_keeps_active_add () =
  let d1 = datom ~e:1 ~a:":name" ~v:(String "Ivan") ~tx:100 ~added:true in
  let result = datoms_filter [ d1 ] in
  assert_equal_int "single add is kept" 1 (List.length result)

let test_datoms_filter_same_tx_cancel () =
  let retract = datom ~e:1 ~a:":name" ~v:(String "Ivan") ~tx:100 ~added:false in
  let add = datom ~e:1 ~a:":name" ~v:(String "Ivan") ~tx:100 ~added:true in
  let result = datoms_filter [ retract; add ] in
  assert_equal_int "same-tx retract then add cancel" 0 (List.length result)

let test_visible_at_tx_respects_bounds () =
  let bounds = { view_tx = 200; since_tx = Some 100; history = false } in
  let before = datom ~e:1 ~a:":a" ~v:(String "x") ~tx:100 ~added:true in
  let inside = datom ~e:1 ~a:":a" ~v:(String "y") ~tx:150 ~added:true in
  let after = datom ~e:1 ~a:":a" ~v:(String "z") ~tx:250 ~added:true in
  assert_equal_bool "since excludes boundary tx" false (visible_at_tx bounds before);
  assert_equal_bool "inside range is visible" true (visible_at_tx bounds inside);
  assert_equal_bool "view_tx excludes future tx" false (visible_at_tx bounds after)

let () =
  test_datoms_filter_cancels_later_retract ();
  test_datoms_filter_keeps_active_add ();
  test_datoms_filter_same_tx_cancel ();
  test_visible_at_tx_respects_bounds ();
  Printf.printf "test_tx_visibility: ok\n"
