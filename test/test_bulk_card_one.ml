(** Bulk card-one tx_data: same-tx re-assert after update must keep the final value. *)

open Alcotest
open Datascript
open Test_alcotest_support

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

let int_ages db e =
  datoms db Eavt ~e ~a:"age" ()
  |> List.of_seq
  |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  |> List.sort compare

let test_bulk_same_tx_reassert_original_after_update () =
  (* Regression for O(n) by_ea path: from_db must ignore in-tx retracts, and
     from_acc must only include live added facts. Otherwise age 20→30→20 drops
     the final assert because from_db still looks like age=20 is live. *)
  let db =
    empty_db ~schema:[ "age", indexed; "name", indexed ] ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Alice")
         ; Add (Entity_id 1, "age", Int 20)
         ]
  in
  let report =
    transact db
      [ Add (Entity_id 1, "age", Int 30); Add (Entity_id 1, "age", Int 20) ]
  in
  check_int_list "live age is final re-assert 20" [ 20 ] (int_ages report.db_after 1);
  let asserted_ages =
    report.tx_data
    |> List.filter (fun d -> d.a = "age" && d.added)
    |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  in
  check_int_list "tx_data asserts both intermediate and final ages" [ 30; 20 ] asserted_ages;
  let final_assert_present =
    List.exists
      (fun d -> d.a = "age" && d.added && d.v = Int 20)
      report.tx_data
  in
  check_bool "final age=20 assert present in tx_data" true final_assert_present

let test_bulk_same_tx_multiple_updates () =
  let db =
    empty_db ~schema:[ "age", indexed ] ()
    |> db_with [ Add (Entity_id 1, "age", Int 1) ]
  in
  let report =
    transact db
      [ Add (Entity_id 1, "age", Int 2)
      ; Add (Entity_id 1, "age", Int 3)
      ; Add (Entity_id 1, "age", Int 4)
      ]
  in
  check_int_list "live age is last write" [ 4 ] (int_ages report.db_after 1)

let test_bulk_new_entity_card_one_updates () =
  (* Bulk path with tempids: same-tx card-one updates on a newly allocated entity. *)
  let report =
    transact
      (empty_db ~schema:[ "age", indexed; "name", indexed ] ())
      [ Entity
          { db_id = Some (Temp_id "a")
          ; attrs =
              [ "name", One_value (String "Ada")
              ; "age", One_value (Int 10)
              ]
          }
      ; Add (Temp_id "a", "age", Int 11)
      ; Add (Temp_id "a", "age", Int 10)
      ]
  in
  match List.assoc_opt "a" report.tempids with
  | None -> fail "tempid a should resolve"
  | Some e -> check_int_list "new entity ends at age 10" [ 10 ] (int_ages report.db_after e)

let () =
  run "bulk_card_one"
    [ ( "same_tx"
      , [ test_case "reassert original after update" `Quick
            test_bulk_same_tx_reassert_original_after_update
        ; test_case "multiple updates keep last" `Quick test_bulk_same_tx_multiple_updates
        ; test_case "new entity tempid updates" `Quick test_bulk_new_entity_card_one_updates
        ] )
    ]
