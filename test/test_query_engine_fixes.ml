open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_rows label expected actual =
  let norm rows = List.sort compare rows in
  if norm expected <> norm actual then failf "%s" label

let rules_of_string s = Parser.parse_rules (Parser.read_edn s)

(* Bug 1: recursive rules should walk the graph in the direction the body says.
   Upstream: datascript/test/query_rules.cljc test-rules *)
let test_recursive_rules_direction () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "follow", Ref 2)
         ; Add (Entity_id 2, "follow", Ref 3)
         ; Add (Entity_id 3, "follow", Ref 4)
         ]
  in
  let rules =
    rules_of_string
      "[[(follow ?x ?y) [?x :follow ?y]]
        [(follow ?x ?y) [?x :follow ?t] (follow ?t ?y)]]"
  in
  assert_rows
    "recursive rule returns all follow pairs"
    [ [ Result_entity 1; Result_entity 2 ]
    ; [ Result_entity 1; Result_entity 3 ]
    ; [ Result_entity 1; Result_entity 4 ]
    ; [ Result_entity 2; Result_entity 3 ]
    ; [ Result_entity 2; Result_entity 4 ]
    ; [ Result_entity 3; Result_entity 4 ]
    ]
    (q_string ~inputs:[ Arg_rules rules ] db
       "[:find ?e1 ?e2 :in $ % :where (follow ?e1 ?e2)]")

let test_rule_branches_positional_binding () =
  (* Upstream "Rule with branches": head var names (?e2 ?e1) intentionally differ
     from the invocation args; binding must be positional. init_db keeps all raw
     datoms like upstream's datom-vector inputs. *)
  let db =
    init_db
      [ datom ~e:5 ~a:"follow" ~v:(Ref 3) ()
      ; datom ~e:1 ~a:"follow" ~v:(Ref 2) ()
      ; datom ~e:2 ~a:"follow" ~v:(Ref 3) ()
      ; datom ~e:3 ~a:"follow" ~v:(Ref 4) ()
      ; datom ~e:4 ~a:"follow" ~v:(Ref 6) ()
      ; datom ~e:2 ~a:"follow" ~v:(Ref 4) ()
      ]
  in
  let rules =
    rules_of_string
      "[[(follow ?e2 ?e1) [?e2 :follow ?e1]]
        [(follow ?e2 ?e1) [?e2 :follow ?t] [?t :follow ?e1]]]"
  in
  assert_rows
    "rule head vars bind positionally, not by name"
    [ [ Result_entity 2 ]; [ Result_entity 3 ]; [ Result_entity 4 ] ]
    (q_string ~inputs:[ Arg_scalar (Result_entity 1); Arg_rules rules ] db
       "[:find ?e2 :in $ ?e1 % :where (follow ?e1 ?e2)]")

let test_recursive_rule_swapped_args () =
  (* Upstream "Recursive rules": self-call with swapped args produces the
     symmetric closure *)
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "follow", Ref 2)
         ; Add (Entity_id 2, "follow", Ref 3)
         ]
  in
  let rules =
    rules_of_string
      "[[(follow ?e1 ?e2) [?e1 :follow ?e2]]
        [(follow ?e1 ?e2) (follow ?e2 ?e1)]]"
  in
  assert_rows
    "recursive self-call with swapped args yields symmetric pairs"
    [ [ Result_entity 1; Result_entity 2 ]
    ; [ Result_entity 2; Result_entity 3 ]
    ; [ Result_entity 2; Result_entity 1 ]
    ; [ Result_entity 3; Result_entity 2 ]
    ]
    (q_string ~inputs:[ Arg_rules rules ] db
       "[:find ?e1 ?e2 :in $ % :where (follow ?e1 ?e2)]")

(* Bug 2: predicates over :in-bound scalars must see the bindings *)
let test_predicate_over_in_scalar () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "attr", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "attr", One_value (Int 2) ] }
         ]
  in
  assert_rows
    "[(= ?v ?target)] filters by :in binding"
    [ [ Result_entity 2 ] ]
    (q_string ~inputs:[ Arg_scalar (Result_value (Int 2)) ] db
       "[:find ?e :in $ ?target :where [?e :attr ?v] [(= ?v ?target)]]")

(* Bug 3: collection-form :in binds each element *)
let test_collection_in_binding () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "attr", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "attr", One_value (Int 2) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "attr", One_value (Int 3) ] }
         ]
  in
  assert_rows
    "[?x ...] collection :in iterates elements"
    [ [ Result_entity 1 ]; [ Result_entity 3 ] ]
    (q_string ~inputs:[ Arg_collection [ Result_value (Int 1); Result_value (Int 3) ] ] db
       "[:find ?e :in $ [?x ...] :where [?e :attr ?x]]")

(* Bug 2 extended: comparison predicates over :in-bound vars *)
let test_comparison_predicates_over_in () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "d", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "d", One_value (Int 3) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "d", One_value (Int 5) ] }
         ]
  in
  assert_rows
    "[(<= ?d ?cutoff)] filters by :in binding"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q_string ~inputs:[ Arg_scalar (Result_value (Int 3)) ] db
       "[:find ?e :in $ ?cutoff :where [?e :d ?d] [(<= ?d ?cutoff)]]");
  assert_rows
    "[(not= ?d ?x)] filters by :in binding"
    [ [ Result_entity 1 ]; [ Result_entity 3 ] ]
    (q_string ~inputs:[ Arg_scalar (Result_value (Int 3)) ] db
       "[:find ?e :in $ ?x :where [?e :d ?d] [(not= ?d ?x)]]")

(* Bug 2 extended: predicate over collection-bound :in var sees each element *)
let test_predicate_over_collection_in () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "attr", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "attr", One_value (Int 3) ] }
         ]
  in
  assert_rows
    "[(= ?v ?x)] sees collection :in elements"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q_string ~inputs:[ Arg_collection [ Result_value (Int 1); Result_value (Int 3) ] ] db
       "[:find ?e :in $ [?x ...] :where [?e :attr ?v] [(= ?v ?x)]]")

(* Bug 5: lookup refs inside a transaction resolve against the db with the
   pending tx datoms, matching upstream's sequential transact-add semantics *)
let block_schema () =
  let base_attr =
    { cardinality = One; unique = None; indexed = false; is_component = false
    ; no_history = false; doc = None; value_type = None; tuple_attrs = None
    ; tuple_types = None }
  in
  [ "block/uuid", { base_attr with unique = Some Identity; indexed = true }
  ; "block/parent", { base_attr with value_type = Some RefType }
  ]

let lookup_ref attr value = List [ Keyword attr; value ]

let test_entity_map_lookup_ref_earlier_tx_entity () =
  let u1 = "11111111-1111-1111-1111-111111111111" in
  let u2 = "22222222-2222-2222-2222-222222222222" in
  let db =
    empty_db ~schema:(block_schema ()) ()
    |> db_with
         [ Entity { db_id = None; attrs = [ "block/uuid", One_value (Uuid u1) ] }
         ; Entity
             { db_id = None
             ; attrs =
                 [ "block/uuid", One_value (Uuid u2)
                 ; "block/parent", One_value (lookup_ref "block/uuid" (Uuid u1))
                 ]
             }
         ]
  in
  let parent_datoms =
    datoms db Aevt ~a:"block/parent" ()
    |> List.of_seq
    |> List.map (fun d -> d.e, d.v)
  in
  if parent_datoms <> [ 2, Ref 1 ] then
    failf "lookup ref to earlier-tx entity should resolve to e=1, got %d datoms" (List.length parent_datoms);
  (* upstream test-lookup-refs-transact: "lookup refs are resolved at
     intermediate DB value" — Add ops resolve against the pending tx too *)
  let u3 = "33333333-3333-3333-3333-333333333333" in
  let db =
    empty_db ~schema:(block_schema ()) ()
    |> db_with
         [ Entity { db_id = None; attrs = [ "block/uuid", One_value (Uuid u1) ] }
         ; Add (Entity_id 3, "block/uuid", Uuid u3)
         ; Add (Entity_id 1, "block/parent", lookup_ref "block/uuid" (Uuid u3))
         ]
  in
  let parent_datoms =
    datoms db Aevt ~a:"block/parent" ()
    |> List.of_seq
    |> List.map (fun d -> d.e, d.v)
  in
  if parent_datoms <> [ 1, Ref 3 ] then failf "Add lookup ref to earlier-tx entity should resolve to e=3"

let test_entity_map_lookup_ref_same_entity () =
  (* An entity map's earlier attrs must be visible when resolving a later
     attr's lookup ref, matching upstream's sequential add order *)
  let u1 = "11111111-1111-1111-1111-111111111111" in
  let db =
    empty_db ~schema:(block_schema ()) ()
    |> db_with
         [ Entity
             { db_id = None
             ; attrs =
                 [ "block/uuid", One_value (Uuid u1)
                 ; "block/parent", One_value (lookup_ref "block/uuid" (Uuid u1))
                 ]
             }
         ]
  in
  let parent_datoms =
    datoms db Aevt ~a:"block/parent" ()
    |> List.of_seq
    |> List.map (fun d -> d.e, d.v)
  in
  if parent_datoms <> [ 1, Ref 1 ] then failf "self lookup ref should resolve to the same entity e=1"

let test_entity_map_lookup_ref_unresolved_raises () =
  let u1 = "11111111-1111-1111-1111-111111111111" in
  (try
     ignore
       (empty_db ~schema:(block_schema ()) ()
        |> db_with
             [ Entity
                 { db_id = None
                 ; attrs = [ "block/parent", One_value (lookup_ref "block/uuid" (Uuid u1)) ]
                 }
             ]);
     failf "unresolvable lookup ref should raise"
   with
   | Invalid_argument msg ->
     if not
          (String.starts_with ~prefix:"Nothing found for entity id" msg)
     then
       failf "unexpected error message: %s" msg)

(* Bug 6: EDN reader accepts ' and friends inside symbol/keyword bodies *)
let test_edn_symbol_special_chars () =
  (match Parser.read_edn "{:user.property/foo*+!_'?<>=- nil}" with
   | QueryFormMap [ QueryFormKeyword "user.property/foo*+!_'?<>=-", QueryFormNil ] -> ()
   | _ -> failf "read_edn should parse keywords containing *+!_'?<>=-");
  (match Parser.read_edn "[sym' foo*+!_'?<>=- 'quoted ?var]" with
   | QueryFormVector
       [ QueryFormSymbol "sym'"
       ; QueryFormSymbol "foo*+!_'?<>=-"
       ; QueryFormSymbol "quoted"
       ; QueryFormSymbol "?var" ] -> ()
   | _ -> failf "read_edn should parse symbols containing *+!_'?<>=- and leading 'quote")

let () =
  List.iter
    (fun (name, f) ->
      f ();
      Printf.printf "%s ok\n" name)
    [ "recursive_rules_direction", test_recursive_rules_direction
    ; "rule_branches_positional_binding", test_rule_branches_positional_binding
    ; "recursive_rule_swapped_args", test_recursive_rule_swapped_args
    ; "predicate_over_in_scalar", test_predicate_over_in_scalar
    ; "collection_in_binding", test_collection_in_binding
    ; "comparison_predicates_over_in", test_comparison_predicates_over_in
    ; "predicate_over_collection_in", test_predicate_over_collection_in
    ; "entity_map_lookup_ref_earlier_tx_entity", test_entity_map_lookup_ref_earlier_tx_entity
    ; "entity_map_lookup_ref_same_entity", test_entity_map_lookup_ref_same_entity
    ; "entity_map_lookup_ref_unresolved_raises", test_entity_map_lookup_ref_unresolved_raises
    ; "edn_symbol_special_chars", test_edn_symbol_special_chars
    ]
