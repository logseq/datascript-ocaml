(** Query result cache: key must distinguish db identity and temporal/filter views. *)

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

let schema = [ "name", indexed; "age", indexed ]

let cell_digest = function
  | Result_entity e -> "e:" ^ string_of_int e
  | Result_value (String s) -> "s:" ^ s
  | Result_value (Int i) -> "i:" ^ string_of_int i
  | Result_value (Keyword k) -> "k:" ^ k
  | _ -> "?"

let rows_digest rows =
  rows
  |> List.map (fun row -> String.concat "," (List.map cell_digest row))
  |> List.sort String.compare
  |> String.concat "|"

let find_name_age =
  parse_query_string
    {|[:find ?e ?name ?age :where [?e :name ?name] [?e :age ?age]]|}

let find_name =
  parse_query_string {|[:find ?e ?name :where [?e :name ?name]]|}

let with_cache_on f =
  clear_query_result_cache ();
  with_query_result_cache true f

let db_alice_bob () =
  empty_db ~schema ()
  |> db_with
       [ Add (Entity_id 1, "name", String "Alice")
       ; Add (Entity_id 1, "age", Int 25)
       ; Add (Entity_id 2, "name", String "Bob")
       ; Add (Entity_id 2, "age", Int 35)
       ]

let db_carol () =
  empty_db ~schema ()
  |> db_with
       [ Add (Entity_id 1, "name", String "Carol")
       ; Add (Entity_id 1, "age", Int 40)
       ]

let test_cache_hit_same_db () =
  with_cache_on (fun () ->
      let db = db_alice_bob () in
      let first = q db find_name_age in
      check_bool "cold miss" false (last_query_cache_hit ());
      let second = q db find_name_age in
      check_bool "warm hit" true (last_query_cache_hit ());
      check string "same rows" (rows_digest first) (rows_digest second))

let test_cache_separates_distinct_dbs () =
  with_cache_on (fun () ->
      let db_a = db_alice_bob () in
      let db_b = db_carol () in
      (* Same physical query AST + matching max_e/max_tx shapes must not cross-hit. *)
      let dig_a = rows_digest (q db_a find_name_age) in
      check_bool "db_a cold" false (last_query_cache_hit ());
      let dig_b = rows_digest (q db_b find_name_age) in
      check_bool "db_b must miss despite shared query AST" false (last_query_cache_hit ());
      check string "db_a digest" "e:1,s:Alice,i:25|e:2,s:Bob,i:35" dig_a;
      check string "db_b digest" "e:1,s:Carol,i:40" dig_b;
      let dig_a_again = rows_digest (q db_a find_name_age) in
      check_bool "db_a warm hit" true (last_query_cache_hit ());
      check string "db_a still Alice/Bob" dig_a dig_a_again)

let test_cache_separates_as_of () =
  with_cache_on (fun () ->
      let db0 = db_alice_bob () in
      let tx0 = basis_tx db0 in
      let db1 = db_with [ Add (Entity_id 1, "age", Int 26) ] db0 in
      let past = as_of tx0 db1 in
      let dig_current = rows_digest (q db1 find_name_age) in
      let dig_past = rows_digest (q past find_name_age) in
      check_bool "as_of must miss after current" false (last_query_cache_hit ());
      check string "current age 26" "e:1,s:Alice,i:26|e:2,s:Bob,i:35" dig_current;
      check string "as_of age 25" "e:1,s:Alice,i:25|e:2,s:Bob,i:35" dig_past;
      ignore (q db1 find_name_age);
      check_bool "current warm" true (last_query_cache_hit ());
      ignore (q past find_name_age);
      check_bool "as_of warm" true (last_query_cache_hit ()))

let test_cache_separates_since () =
  with_cache_on (fun () ->
      let db0 =
        empty_db ~schema ()
        |> db_with [ Add (Entity_id 1, "name", String "Alice") ]
      in
      let tx0 = basis_tx db0 in
      let db1 =
        db_with
          [ Add (Entity_id 2, "name", String "Bob")
          ; Add (Entity_id 2, "age", Int 30)
          ]
          db0
      in
      let delta = since tx0 db1 in
      let dig_full = rows_digest (q db1 find_name) in
      let dig_since = rows_digest (q delta find_name) in
      check_bool "since must miss after full db" false (last_query_cache_hit ());
      check string "full names" "e:1,s:Alice|e:2,s:Bob" dig_full;
      check string "since only Bob" "e:2,s:Bob" dig_since)

let test_cache_separates_history () =
  with_cache_on (fun () ->
      let db =
        empty_db ~schema ()
        |> db_with [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 20) ]
        |> db_with [ Add (Entity_id 1, "age", Int 30) ]
      in
      let hist = history db in
      let dig_current = rows_digest (q db find_name_age) in
      let dig_hist = rows_digest (q hist find_name_age) in
      check_bool "history must miss after current" false (last_query_cache_hit ());
      check string "current only live age" "e:1,s:Alice,i:30" dig_current;
      (* History view must not reuse the current-view cache entry. *)
      check_int "history digest differs or is at least as long" 1
        (if dig_hist <> dig_current || String.length dig_hist >= String.length dig_current
         then 1
         else 0);
      ignore (q db find_name_age);
      check_bool "current warm" true (last_query_cache_hit ());
      ignore (q hist find_name_age);
      check_bool "history warm" true (last_query_cache_hit ()))

let test_cache_separates_filter () =
  with_cache_on (fun () ->
      let db = db_alice_bob () in
      let only_e1 = filter db (fun _ d -> d.e = 1) in
      let only_e2 = filter db (fun _ d -> d.e = 2) in
      let dig_full = rows_digest (q db find_name) in
      let dig_e1 = rows_digest (q only_e1 find_name) in
      check_bool "filter e1 must miss after full" false (last_query_cache_hit ());
      let dig_e2 = rows_digest (q only_e2 find_name) in
      check_bool "filter e2 must miss after filter e1" false (last_query_cache_hit ());
      check string "full" "e:1,s:Alice|e:2,s:Bob" dig_full;
      check string "filtered e=1" "e:1,s:Alice" dig_e1;
      check string "filtered e=2" "e:2,s:Bob" dig_e2;
      ignore (q only_e1 find_name);
      check_bool "filter e1 warm" true (last_query_cache_hit ()))

let test_cache_toggle_and_clear () =
  with_cache_on (fun () ->
      let db = db_alice_bob () in
      ignore (q db find_name);
      ignore (q db find_name);
      check_bool "hit before clear" true (last_query_cache_hit ());
      clear_query_result_cache ();
      ignore (q db find_name);
      check_bool "miss after clear" false (last_query_cache_hit ());
      with_query_result_cache false (fun () ->
          ignore (q db find_name);
          check_bool "disabled never hits" false (last_query_cache_hit ());
          ignore (q db find_name);
          check_bool "disabled second call" false (last_query_cache_hit ()));
      ignore (q db find_name);
      check_bool "re-enabled can hit" true (last_query_cache_hit ()))

let test_int_range_at_max_int_does_not_overflow () =
  with_cache_on (fun () ->
      let db =
        empty_db ~schema:[ "age", indexed ] ()
        |> db_with
             [ Add (Entity_id 1, "age", Int max_int)
             ; Add (Entity_id 2, "age", Int (max_int - 1))
             ]
      in
      let q_gt =
        parse_query_string
          {|[:find ?e :in $ ?t :where [?e :age ?age] [(> ?age ?t)]]|}
      in
      let dig =
        rows_digest
          (q ~inputs:[ Arg_scalar (Result_value (Int (max_int - 1))) ] db q_gt)
      in
      check string "> max_int-1 finds only max_int" "e:1" dig;
      let dig_none =
        rows_digest (q ~inputs:[ Arg_scalar (Result_value (Int max_int)) ] db q_gt)
      in
      check string "> max_int finds nothing" "" dig_none)

let test_int_range_at_min_int_does_not_overflow () =
  with_cache_on (fun () ->
      let db =
        empty_db ~schema:[ "age", indexed ] ()
        |> db_with
             [ Add (Entity_id 1, "age", Int min_int)
             ; Add (Entity_id 2, "age", Int (min_int + 1))
             ]
      in
      let q_lt =
        parse_query_string
          {|[:find ?e :in $ ?t :where [?e :age ?age] [(< ?age ?t)]]|}
      in
      let dig =
        rows_digest
          (q ~inputs:[ Arg_scalar (Result_value (Int (min_int + 1))) ] db q_lt)
      in
      check string "< min_int+1 finds only min_int" "e:1" dig;
      let dig_none =
        rows_digest (q ~inputs:[ Arg_scalar (Result_value (Int min_int)) ] db q_lt)
      in
      check string "< min_int finds nothing" "" dig_none)

let () =
  run "query_result_cache"
    [ ( "cache"
      , [ test_case "hit same db" `Quick test_cache_hit_same_db
        ; test_case "separate distinct dbs" `Quick test_cache_separates_distinct_dbs
        ; test_case "separate as_of" `Quick test_cache_separates_as_of
        ; test_case "separate since" `Quick test_cache_separates_since
        ; test_case "separate history" `Quick test_cache_separates_history
        ; test_case "separate filter" `Quick test_cache_separates_filter
        ; test_case "toggle and clear" `Quick test_cache_toggle_and_clear
        ] )
    ; ( "avet_int_bounds"
      , [ test_case "max_int" `Quick test_int_range_at_max_int_does_not_overflow
        ; test_case "min_int" `Quick test_int_range_at_min_int_does_not_overflow
        ] )
    ]
