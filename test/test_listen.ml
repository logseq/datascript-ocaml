open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_tx_flags label expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v, d.added) actual in
  if actual <> expected then failf "%s: unexpected tx-data" label

let test_listen__test_listen_bang () =
  let conn = create_conn () in
  let reports = ref [] in
  ignore
    (transact_conn_string
       conn
       "[[:db/add -1 :name \"Alex\"]
         [:db/add -2 :name \"Boris\"]]");
  ignore (Compat.listen_bang conn "test" (fun report -> reports := !reports @ [ report ]));
  ignore
    (Compat.transact_bang_string
       ~tx_meta:[ "some-metadata", Int64 1L ]
       conn
       "[[:db/add -1 :name \"Dima\"]
         [:db/add -1 :age 19]
         [:db/add -2 :name \"Evgeny\"]]");
  ignore
    (Compat.transact_bang_string
       conn
       "[[:db/add -1 :name \"Fedor\"]
         [:db/add 1 :name \"Alex2\"]
         [:db/retract 2 :name \"Not Boris\"]
         [:db/retract 4 :name \"Evgeny\"]]");
  Compat.unlisten_bang conn "test";
  ignore (Compat.transact_bang_string conn "[[:db/add -1 :name \"George\"]]");
  match !reports with
  | [ first; second ] ->
    assert_equal_tx_flags
      "listen reports first observed tx-data like upstream"
      [ (eid 3L), "name", String "Dima", true
      ; (eid 3L), "age", Int64 19L, true
      ; (eid 4L), "name", String "Evgeny", true
      ]
      first.tx_data;
    if first.tx_meta <> [ "some-metadata", Int64 1L ] then
      failwith "listen should preserve tx metadata for the first observed report";
    assert_equal_tx_flags
      "listen reports replacements and skips no-op retracts like upstream"
      [ (eid 5L), "name", String "Fedor", true
      ; (eid 1L), "name", String "Alex", false
      ; (eid 1L), "name", String "Alex2", true
      ; (eid 4L), "name", String "Evgeny", false
      ]
      second.tx_data;
    if second.tx_meta <> [] then failwith "listen should use empty metadata when none is supplied"
  | reports -> failf "expected two listener reports, got %d" (List.length reports)

let () = test_listen__test_listen_bang ()
