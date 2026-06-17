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
  ignore (listen_bang conn "test" (fun report -> reports := !reports @ [ report ]));
  ignore
    (transact_bang_string
       ~tx_meta:[ "some-metadata", Int 1 ]
       conn
       "[[:db/add -1 :name \"Dima\"]
         [:db/add -1 :age 19]
         [:db/add -2 :name \"Evgeny\"]]");
  ignore
    (transact_bang_string
       conn
       "[[:db/add -1 :name \"Fedor\"]
         [:db/add 1 :name \"Alex2\"]
         [:db/retract 2 :name \"Not Boris\"]
         [:db/retract 4 :name \"Evgeny\"]]");
  unlisten_bang conn "test";
  ignore (transact_bang_string conn "[[:db/add -1 :name \"George\"]]");
  match !reports with
  | [ first; second ] ->
    assert_equal_tx_flags
      "listen reports first observed tx-data like upstream"
      [ 3, "name", String "Dima", true
      ; 3, "age", Int 19, true
      ; 4, "name", String "Evgeny", true
      ]
      first.tx_data;
    if first.tx_meta <> [ "some-metadata", Int 1 ] then
      failwith "listen should preserve tx metadata for the first observed report";
    assert_equal_tx_flags
      "listen reports replacements and skips no-op retracts like upstream"
      [ 5, "name", String "Fedor", true
      ; 1, "name", String "Alex", false
      ; 1, "name", String "Alex2", true
      ; 4, "name", String "Evgeny", false
      ]
      second.tx_data;
    if second.tx_meta <> [] then failwith "listen should use empty metadata when none is supplied"
  | reports -> failf "expected two listener reports, got %d" (List.length reports)

let () = test_listen__test_listen_bang ()
