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

(* Logseq's own rules (deps/db/src/logseq/db/frontend/rules.cljc): the
   recursive call binds its first arg to an intermediate via a ref-pattern
   clause and keeps the head var in the second arg. *)
let test_logseq_parent_rule () =
  let schema =
    let base_attr =
      { cardinality = One; unique = None; indexed = false; is_component = false
      ; no_history = false; doc = None; value_type = None; tuple_attrs = None
      ; tuple_types = None }
    in
    [ "block/parent", { base_attr with value_type = Some RefType } ]
  in
  let db =
    empty_db ~schema ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "block/parent", One_value (Ref 1) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "block/parent", One_value (Ref 2) ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "block/parent", One_value (Ref 3) ] }
         ]
  in
  let rules =
    rules_of_string
      "[[(parent ?p ?c) [?c :block/parent ?p]]
        [(parent ?p ?c) [?t :block/parent ?p] (parent ?t ?c)]]"
  in
  assert_rows
    "(parent 1 ?c) returns descendants of block 1"
    [ [ Result_entity 2 ]; [ Result_entity 3 ]; [ Result_entity 4 ] ]
    (q_string ~inputs:[ Arg_rules rules ] db
       "[:find ?c :in $ % :where (parent 1 ?c)]");
  assert_rows
    "(parent ?p 4) returns ancestors of block 4"
    [ [ Result_entity 1 ]; [ Result_entity 2 ]; [ Result_entity 3 ] ]
    (q_string ~inputs:[ Arg_rules rules ] db
       "[:find ?p :in $ % :where (parent ?p 4)]")

(* Bug A: a bound head arg — literal or :in-bound — must flow into recursive
   rule calls. With two disjoint trees, a dropped binding leaks the other
   tree's children. *)
let test_recursive_rule_bound_head_arg () =
  let schema =
    let base_attr =
      { cardinality = One; unique = None; indexed = false; is_component = false
      ; no_history = false; doc = None; value_type = None; tuple_attrs = None
      ; tuple_types = None }
    in
    [ "block/parent", { base_attr with value_type = Some RefType } ]
  in
  let db =
    empty_db ~schema ()
    |> db_with
         (* tree 1: 1 <- 2 <- 3 ; tree 2: 5 <- 6 <- 7 *)
         [ Entity { db_id = Some (Entity_id 1); attrs = [] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "block/parent", One_value (Ref 1) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "block/parent", One_value (Ref 2) ] }
         ; Entity { db_id = Some (Entity_id 5); attrs = [] }
         ; Entity { db_id = Some (Entity_id 6); attrs = [ "block/parent", One_value (Ref 5) ] }
         ; Entity { db_id = Some (Entity_id 7); attrs = [ "block/parent", One_value (Ref 6) ] }
         ]
  in
  let rules =
    rules_of_string
      "[[(parent ?p ?c) [?c :block/parent ?p]]
        [(parent ?p ?c) [?t :block/parent ?p] (parent ?t ?c)]]"
  in
  assert_rows
    "(parent 1 ?c) with two trees returns only tree-1 descendants"
    [ [ Result_entity 2 ]; [ Result_entity 3 ] ]
    (q_string ~inputs:[ Arg_rules rules ] db
       "[:find ?c :in $ % :where (parent 1 ?c)]");
  assert_rows
    "(parent ?p ?c) with :in-bound ?p returns only its descendants"
    [ [ Result_entity 2 ]; [ Result_entity 3 ] ]
    (q_string ~inputs:[ Arg_scalar (Result_entity 1); Arg_rules rules ] db
       "[:find ?c :in $ ?p % :where (parent ?p ?c)]")

(* Bug B: Arg_scalar (Result_entity e) binds the entity as the :in input,
   same as Arg_scalar (Result_value (Ref e)) — a second tree exposes leaks *)
let test_in_scalar_result_entity () =
  let schema =
    let base_attr =
      { cardinality = One; unique = None; indexed = false; is_component = false
      ; no_history = false; doc = None; value_type = None; tuple_attrs = None
      ; tuple_types = None }
    in
    [ "block/parent", { base_attr with value_type = Some RefType } ]
  in
  let db =
    empty_db ~schema ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "block/parent", One_value (Ref 1) ] }
         ; Entity { db_id = Some (Entity_id 5); attrs = [] }
         ; Entity { db_id = Some (Entity_id 6); attrs = [ "block/parent", One_value (Ref 5) ] }
         ]
  in
  assert_rows
    ":in ?x bound to Arg_scalar (Result_entity 1) filters ref datoms"
    [ [ Result_entity 2 ] ]
    (q_string ~inputs:[ Arg_scalar (Result_entity 1) ] db
       "[:find ?c :in $ ?x :where [?c :block/parent ?x]]");
  assert_rows
    ":in ?x bound to Arg_scalar (Result_value (Ref 1)) filters ref datoms"
    [ [ Result_entity 2 ] ]
    (q_string ~inputs:[ Arg_scalar (Result_value (Ref 1)) ] db
       "[:find ?c :in $ ?x :where [?c :block/parent ?x]]")

(* A var bound to a non-entity value in entity position is unsatisfiable:
   upstream `-search` finds no datoms for it. Before the fix, the unresolved
   bound narrowed like an unbound term, so the clause (and any rule body
   using it, e.g. datascript's ref->val) matched every entity's datoms. *)
let test_bound_non_entity_in_entity_position () =
  let db =
    init_db
      [ datom ~e:1 ~a:"title" ~v:(String "Page1") ()
      ; datom ~e:2 ~a:"title" ~v:(String "Page A") ()
      ; datom ~e:3 ~a:"title" ~v:(String "Page B") ()
      ]
  in
  assert_rows
    ":in ?pv bound to a string in entity position matches nothing"
    []
    (q_string ~inputs:[ Arg_scalar (Result_value (String "Page1")) ] db
       "[:find ?v :in $ ?pv :where [?pv :title ?v]]");
  assert_rows
    "literal non-entity value in entity position matches nothing"
    []
    (q_string db "[:find ?v :where [\"Page1\" :title ?v]]");
  let rules =
    rules_of_string
      "[[(titled ?pv ?v) [?pv :title ?v]]
        [(prop ?b ?v) [?b :title ?pv] (titled ?pv ?v)]]"
  in
  assert_rows
    "rule body with non-entity-bound arg in entity position matches nothing"
    []
    (q_string ~inputs:[ Arg_rules rules ] db
       "[:find ?v :in $ % :where (prop ?b ?v) [?b :title \"Page1\"]]")

(* Upstream "Mutually recursive rules" (test-rules): two rules recursing into
   each other *)
let test_mutually_recursive_rules () =
  let db =
    init_db
      [ datom ~e:0 ~a:"f1" ~v:(Ref 1) ()
      ; datom ~e:1 ~a:"f2" ~v:(Ref 2) ()
      ; datom ~e:2 ~a:"f1" ~v:(Ref 3) ()
      ; datom ~e:3 ~a:"f2" ~v:(Ref 4) ()
      ; datom ~e:4 ~a:"f1" ~v:(Ref 5) ()
      ; datom ~e:5 ~a:"f2" ~v:(Ref 6) ()
      ]
  in
  let rules =
    rules_of_string
      "[[(f1 ?e1 ?e2) [?e1 :f1 ?e2]]
        [(f1 ?e1 ?e2) [?t :f1 ?e2] (f2 ?e1 ?t)]
        [(f2 ?e1 ?e2) [?e1 :f2 ?e2]]
        [(f2 ?e1 ?e2) [?t :f2 ?e2] (f1 ?e1 ?t)]]"
  in
  assert_rows
    "mutually recursive rules walk alternating edge kinds"
    [ [ Result_entity 0; Result_entity 1 ]
    ; [ Result_entity 0; Result_entity 3 ]
    ; [ Result_entity 0; Result_entity 5 ]
    ; [ Result_entity 1; Result_entity 3 ]
    ; [ Result_entity 1; Result_entity 5 ]
    ; [ Result_entity 2; Result_entity 3 ]
    ; [ Result_entity 2; Result_entity 5 ]
    ; [ Result_entity 3; Result_entity 5 ]
    ; [ Result_entity 4; Result_entity 5 ]
    ]
    (q_string ~inputs:[ Arg_rules rules ] db
       "[:find ?e1 ?e2 :in $ % :where (f1 ?e1 ?e2)]")

(* Upstream "Joining regular clauses with rule" (test-rules): a bound var
   flows into a rule invocation and a predicate filters it *)
let test_rule_joined_with_clauses () =
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
    rules_of_string "[[(rule ?a ?b) [?a :follow ?b]]]"
  in
  assert_rows
    "rule invocation unifies with already-bound vars"
    [ [ Result_entity 3; Result_entity 2 ]
    ; [ Result_entity 6; Result_entity 4 ]
    ; [ Result_entity 4; Result_entity 2 ]
    ]
    (q_string ~inputs:[ Arg_rules rules ] db
       "[:find ?y ?x :in $ % :where [_ _ ?x] (rule ?x ?y) [(even? ?x)]]")

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
  ; "block/refs", { base_attr with cardinality = Many; value_type = Some RefType }
  ; "block/title", base_attr
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

(* Forward lookup refs: an entity map may reference an entity defined by a
   LATER op in the same transaction. Ops whose refs do not resolve yet are
   deferred and retried as the tx datoms accumulate; a ref that never resolves
   still raises the upstream "Nothing found for entity id" error. *)
let uuid_of_eid db e =
  datoms db Eavt ~e ~a:"block/uuid" ()
  |> List.of_seq
  |> (function [ d ] -> Some d.v | _ -> None)

let test_entity_map_lookup_ref_later_tx_entity () =
  let u1 = "11111111-1111-1111-1111-111111111111" in
  let u2 = "22222222-2222-2222-2222-222222222222" in
  let db =
    empty_db ~schema:(block_schema ()) ()
    |> db_with
         [ Entity
             { db_id = None
             ; attrs =
                 [ "block/uuid", One_value (Uuid u1)
                 ; "block/parent", One_value (lookup_ref "block/uuid" (Uuid u2))
                 ]
             }
         ; Entity { db_id = None; attrs = [ "block/uuid", One_value (Uuid u2) ] }
         ]
  in
  let parent_datoms =
    datoms db Aevt ~a:"block/parent" ()
    |> List.of_seq
    |> List.map (fun d -> d.e, d.v)
  in
  (match parent_datoms with
   | [ (e_a, Ref e_b) ] ->
     if uuid_of_eid db e_a <> Some (Uuid u1) || uuid_of_eid db e_b <> Some (Uuid u2) then
       failf "forward lookup ref should link u1's entity to u2's entity"
   | _ ->
     failf "forward lookup ref should produce one parent datom, got %d" (List.length parent_datoms))

let test_entity_map_lookup_ref_later_tx_entity_many_values () =
  (* logseq db-worker repro: :block/refs is a cardinality-many ref attr whose
     list contains a lookup ref to an entity map appearing LATER in the tx *)
  let u1 = "11111111-1111-1111-1111-111111111111" in
  let u2 = "22222222-2222-2222-2222-222222222222" in
  let db =
    empty_db ~schema:(block_schema ()) ()
    |> db_with
         [ Entity
             { db_id = None
             ; attrs =
                 [ "block/uuid", One_value (Uuid u1)
                 ; "block/title", One_value (String "Root Page")
                 ; "block/refs", Many_values [ lookup_ref "block/uuid" (Uuid u2) ]
                 ]
             }
         ; Entity
             { db_id = None
             ; attrs =
                 [ "block/uuid", One_value (Uuid u2)
                 ; "block/title", One_value (String "Leaf Page")
                 ]
             }
         ]
  in
  let refs_datoms =
    datoms db Aevt ~a:"block/refs" ()
    |> List.of_seq
    |> List.map (fun d -> d.e, d.v)
  in
  (match refs_datoms with
   | [ (e_a, Ref e_b) ] ->
     if uuid_of_eid db e_a <> Some (Uuid u1) || uuid_of_eid db e_b <> Some (Uuid u2) then
       failf "forward lookup ref in multi-value attr should link u1's entity to u2's entity"
   | _ ->
     failf "forward lookup ref should produce one refs datom, got %d" (List.length refs_datoms))

let test_add_op_lookup_ref_later_tx_entity () =
  let u1 = "11111111-1111-1111-1111-111111111111" in
  let u2 = "22222222-2222-2222-2222-222222222222" in
  let db =
    empty_db ~schema:(block_schema ()) ()
    |> db_with
         [ Add (Entity_id 1, "block/uuid", Uuid u1)
         ; Add (Entity_id 1, "block/parent", lookup_ref "block/uuid" (Uuid u2))
         ; Entity { db_id = None; attrs = [ "block/uuid", One_value (Uuid u2) ] }
         ]
  in
  let parent_datoms =
    datoms db Aevt ~a:"block/parent" ()
    |> List.of_seq
    |> List.map (fun d -> d.e, d.v)
  in
  (match parent_datoms with
   | [ (e_a, Ref e_b) ] ->
     if uuid_of_eid db e_a <> Some (Uuid u1) || uuid_of_eid db e_b <> Some (Uuid u2) then
       failf "Add op forward lookup ref should link u1's entity to u2's entity"
   | _ ->
     failf "Add op forward lookup ref should produce one parent datom, got %d" (List.length parent_datoms))

let test_entity_map_lookup_ref_never_resolves_raises () =
  (* Deferral is not permanent: a ref whose target is absent from the whole tx
     raises the same error as before, after all ops ran *)
  let u1 = "11111111-1111-1111-1111-111111111111" in
  let u2 = "22222222-2222-2222-2222-222222222222" in
  (try
     ignore
       (empty_db ~schema:(block_schema ()) ()
        |> db_with
             [ Entity
                 { db_id = None
                 ; attrs =
                     [ "block/uuid", One_value (Uuid u1)
                     ; "block/parent", One_value (lookup_ref "block/uuid" (Uuid u2))
                     ]
                 }
             ; Entity { db_id = None; attrs = [ "block/uuid", One_value (Uuid u1) ] }
             ]);
     failf "lookup ref that never resolves should raise"
   with
   | Invalid_argument msg ->
     if not
          (String.starts_with ~prefix:"Nothing found for entity id" msg)
     then
       failf "unexpected error message: %s" msg)

let test_retract_cleans_duplicate_avet_tables () =
  (* Bug 5: datoms that compare equal under eavt (same fact stored twice, as
     produced by a full index rebuild over restore) are kept out of the PSet
     indexes in the duplicate_* tables — and find_avet_exact consults
     duplicate_avet_by_attr. Retraction removed the fact from the three PSets
     but left the duplicate copy behind, so [:block/uuid u] still resolved
     after the entity was retracted. Upstream retracts the fact entirely and
     the lookup ref raises "Nothing found for entity id". *)
  let u1 = "11111111-1111-1111-1111-111111111111" in
  let uuid_datom =
    { e = 1; a = "block/uuid"; v = Uuid u1; tx = 1; added = true }
  in
  let db =
    init_db ~schema:(block_schema ())
      [ uuid_datom
      ; { e = 1; a = "block/title"; v = String "Page"; tx = 1; added = true }
      ; uuid_datom ]
  in
  (if List.length db.duplicate_datoms = 0 then
     failf "precondition: init_db should route the repeated fact to duplicate_datoms");
  let db = db |> db_with [ RetractEntity (Entity_id 1) ] in
  (match datoms db Eavt ~a:"block/uuid" () |> List.of_seq with
   | [] -> ()
   | datoms -> failf "retracted uuid datom still visible in eavt: %d" (List.length datoms));
  (match datoms db Avet ~a:"block/uuid" () |> List.of_seq with
   | [] -> ()
   | datoms -> failf "retracted uuid datom still visible in avet: %d" (List.length datoms));
  (try
     ignore
       (db
        |> db_with
             [ Entity
                 { db_id = None
                 ; attrs =
                     [ "block/title", One_value (String "Child")
                     ; "block/parent", One_value (lookup_ref "block/uuid" (Uuid u1))
                     ]
                 }
             ]);
     failf "lookup ref to a retracted entity should raise"
   with
   | Invalid_argument msg ->
     if not
          (String.starts_with ~prefix:"Nothing found for entity id" msg)
     then
       failf "unexpected error message: %s" msg)

(* Bug D: upstream maybe-wrap-multival — inside a multival attr a 2-element
   collection is a lookup ref only when its head names a unique-identity attr;
   otherwise it expands into individual values *)
let refs_datoms db =
  datoms db Aevt ~a:"block/refs" ()
  |> List.of_seq
  |> List.map (fun d -> d.e, d.v)

let test_many_ref_vector_of_idents_expands () =
  (* logseq repro: {:block/tags [:logseq.class/Page :logseq.class/Task]} — the
     head keyword is not an attr at all, so the vector is a collection *)
  let db =
    empty_db ~schema:(block_schema ()) ()
    |> db_with
         [ Add (Entity_id 10, "db/ident", Keyword "logseq.class/Page")
         ; Add (Entity_id 11, "db/ident", Keyword "logseq.class/Task")
         ]
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "block/refs"
                 , One_value
                     (Vector [ Keyword "logseq.class/Page"; Keyword "logseq.class/Task" ])
                 ]
             }
         ]
  in
  let refs = refs_datoms db in
  if refs <> [ 1, Ref 10; 1, Ref 11 ] then
    failf "vector of ident keywords should expand into one datom per ident, got %d"
      (List.length refs)

let test_many_ref_vector_of_same_ident_idempotent () =
  let db =
    empty_db ~schema:(block_schema ()) ()
    |> db_with [ Add (Entity_id 10, "db/ident", Keyword "logseq.class/Page") ]
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "block/refs"
                 , One_value
                     (Vector [ Keyword "logseq.class/Page"; Keyword "logseq.class/Page" ])
                 ]
             }
         ]
  in
  let refs = refs_datoms db in
  if refs <> [ 1, Ref 10 ] then
    failf "duplicate ident refs should yield a single datom, got %d" (List.length refs)

let test_many_ref_vector_with_unique_head_is_lookup_ref () =
  (* Same 2-vector whose head names a unique-identity attr stays a lookup ref *)
  let u1 = "11111111-1111-1111-1111-111111111111" in
  let db =
    empty_db ~schema:(block_schema ()) ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "block/uuid", One_value (Uuid u1) ] }
         ; Entity
             { db_id = Some (Entity_id 9)
             ; attrs =
                 [ "block/refs"
                 , One_value (Vector [ Keyword "block/uuid"; Uuid u1 ])
                 ]
             }
         ]
  in
  let refs = refs_datoms db in
  if refs <> [ 9, Ref 1 ] then
    failf "2-vector headed by a unique attr should resolve as a lookup ref to e=1"

let test_entity_map_lookup_ref_vector_form () =
  (* {:block/parent [:block/uuid u]} — single-attr lookup ref in vector form *)
  let u1 = "11111111-1111-1111-1111-111111111111" in
  let db =
    empty_db ~schema:(block_schema ()) ()
    |> db_with
         [ Entity { db_id = None; attrs = [ "block/uuid", One_value (Uuid u1) ] }
         ; Entity
             { db_id = None
             ; attrs =
                 [ "block/parent", One_value (Vector [ Keyword "block/uuid"; Uuid u1 ]) ]
             }
         ]
  in
  let parent_datoms =
    datoms db Aevt ~a:"block/parent" ()
    |> List.of_seq
    |> List.map (fun d -> d.e, d.v)
  in
  if parent_datoms <> [ 2, Ref 1 ] then
    failf "vector lookup ref should resolve to e=1"

(* Batch 4 regression (fixed by 0645055): a tx asserting a bare :db/ident for an
   attr that already has a schema entry must keep that entry mid-transaction —
   upstream update-schema merges per-datom. At e72915c the mid-tx schema
   refresh stripped block/tags' cardinality-many/ref entry, so every later
   block/tags value in the same tx was stored as a raw keyword instead of a
   ref, and logseq's db_query_dsl rule queries over the attr (has-property /
   property / tags) returned no rows. Tests below mirror those three queries
   over a fixture whose tx includes such a bare :db/ident assert. *)
let logseq_rule_schema () =
  let base_attr =
    { cardinality = One; unique = None; indexed = false; is_component = false
    ; no_history = false; doc = None; value_type = None; tuple_attrs = None
    ; tuple_types = None }
  in
  [ "block/tags", { base_attr with cardinality = Many; value_type = Some RefType }
  ; "block/title", base_attr
  ; "user.property/foo", base_attr
  ; "user.property/number-many", { base_attr with cardinality = Many }
  ; "user.property/page-many", { base_attr with cardinality = Many; value_type = Some RefType }
  ; "logseq.property/public?", base_attr
  ; "logseq.property.class/extends", { base_attr with cardinality = Many; value_type = Some RefType }
  ]

(* Entity 10 asserts only :db/ident for block/tags — the bare-ident form whose
   schema-stripping corrupts every subsequent block/tags value of the same tx
   at e72915c. Attr entities tagged logseq.class/Property mirror logseq's
   built-in/user properties; block/title's ident entity is intentionally
   untagged so it is excluded from property rule results. *)
let logseq_rule_db () =
  let property_tag = Many_values [ Keyword "logseq.class/Property" ] in
  empty_db ~schema:(logseq_rule_schema ()) ()
  |> db_with
       [ Entity
           { db_id = Some (Entity_id 11)
           ; attrs = [ "db/ident", One_value (Keyword "logseq.class/Property") ]
           }
       ; Entity
           { db_id = Some (Entity_id 16)
           ; attrs =
               [ "db/ident", One_value (Keyword "logseq.class/Page")
               ; "block/title", One_value (String "Page")
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 17)
           ; attrs =
               [ "db/ident", One_value (Keyword "user.class/Person")
               ; "block/title", One_value (String "Person")
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 18)
           ; attrs =
               [ "db/ident", One_value (Keyword "user.class/Employee")
               ; "logseq.property.class/extends"
               , Many_values [ Keyword "user.class/Person" ]
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 10)
           ; attrs =
               [ "db/ident", One_value (Keyword "block/tags")
               ; "block/tags", property_tag
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 13)
           ; attrs = [ "db/ident", One_value (Keyword "block/title") ]
           }
       ; Entity
           { db_id = Some (Entity_id 12)
           ; attrs =
               [ "db/ident", One_value (Keyword "user.property/foo")
               ; "block/tags", property_tag
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 14)
           ; attrs =
               [ "db/ident", One_value (Keyword "user.property/number-many")
               ; "block/tags", property_tag
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 15)
           ; attrs =
               [ "db/ident", One_value (Keyword "user.property/page-many")
               ; "block/tags", property_tag
               ; "db/valueType", One_value (Keyword "db.type/ref")
               ; "db/cardinality", One_value (Keyword "db.cardinality/many")
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 2)
           ; attrs = [ "block/title", One_value (String "Page A") ]
           }
       ; Entity
           { db_id = Some (Entity_id 1)
           ; attrs =
               [ "block/title", One_value (String "Page1")
               ; "block/tags", Many_values [ Keyword "user.class/Person" ]
               ; "user.property/foo", One_value (String "bar")
               ; "user.property/number-many", Many_values [ Int 5; Int 10 ]
               ; "user.property/page-many", Many_values [ Keyword "logseq.class/Page" ]
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 3)
           ; attrs =
               [ "block/title", One_value (String "Page2")
               ; "block/tags", Many_values [ Keyword "user.class/Person" ]
               ]
           }
       ; Entity
           { db_id = Some (Entity_id 4)
           ; attrs =
               [ "block/title", One_value (String "Page3")
               ; "block/tags", Many_values [ Keyword "user.class/Employee" ]
               ]
           }
       ]

let logseq_dsl_rules =
  "[[(has-property ?b ?prop)
     [?b ?prop _]
     [?prop-e :db/ident ?prop]
     [?prop-e :block/tags :logseq.class/Property]
     (or [(missing? $ ?prop-e :logseq.property/public?)]
         [?prop-e :logseq.property/public? true])]
    [(property ?b ?prop ?val)
     [?prop-e :db/ident ?prop]
     [?prop-e :block/tags :logseq.class/Property]
     (or [(missing? $ ?prop-e :logseq.property/public?)]
         [?prop-e :logseq.property/public? true])
     [?b ?prop ?pv]
     (or (and [(missing? $ ?prop-e :db/valueType)]
              [?b ?prop ?val])
         (and [?prop-e :db/valueType :db.type/ref]
              (or [?pv :block/title ?val]
                  [?pv :logseq.property/value ?val])))]
    [(tags ?b ?tags)
     [(identity ?tags) [?spec ...]]
     (tag-spec->tag ?tag ?spec)
     [?b :block/tags ?tc]
     (or [(= ?tag ?tc)] (class-extends ?tag ?tc))
     [(missing? $ ?b :block/link)]]
    [(tag-spec->tag ?tag ?spec) [(number? ?spec)] [(identity ?spec) ?tag]]
    [(tag-spec->tag ?tag ?spec) [?tag :block/title ?spec]]
    [(tag-spec->tag ?tag ?spec) [?tag :db/ident ?spec]]
    [(class-extends ?p ?c)
     [?c :logseq.property.class/extends ?p]]
    [(class-extends ?p ?c)
     [?t :logseq.property.class/extends ?p]
     (class-extends ?t ?c)]]"

let attr_results label expected rows =
  let got =
    List.sort_uniq compare
      (List.filter_map
         (function
           | [ Result_attr a ] -> Some a
           | [ Result_value (Keyword a) ] -> Some a
           | _ -> None)
         rows)
  in
  if got <> List.sort compare expected then
    failf "%s: got %d attrs" label (List.length got)

let test_schema_keeps_entry_after_bare_ident_assert () =
  (* direct check for the e72915c corruption: every block/tags datom must be a
     Ref, not a raw keyword *)
  let db = logseq_rule_db () in
  List.iter
    (fun d ->
      match d.v with
      | Ref _ -> ()
      | _ -> failf "block/tags datom for e=%d stored as raw value, not a ref" d.e)
    (List.of_seq (datoms db Aevt ~a:"block/tags" ()))

let test_rule_attr_var_has_property () =
  let db = logseq_rule_db () in
  attr_results "has-property with unbound attr var"
    [ "block/tags"; "user.property/foo"; "user.property/number-many"
    ; "user.property/page-many" ]
    (q_string ~inputs:[ Arg_rules (rules_of_string logseq_dsl_rules) ] db
       "[:find ?p :in $ % :where (has-property ?b ?p) [?b :block/title \"Page1\"]]")

let test_rule_attr_var_property () =
  let db = logseq_rule_db () in
  attr_results "property with unbound attr var"
    [ "block/tags"; "user.property/foo"; "user.property/number-many"
    ; "user.property/page-many" ]
    (q_string ~inputs:[ Arg_rules (rules_of_string logseq_dsl_rules) ] db
       "[:find ?p :in $ % :where (property ?b ?p _) [?b :block/title \"Page1\"]]")

let test_rule_tags_query_with_eid_input () =
  let db = logseq_rule_db () in
  (* cljs binds #{person-eid} to scalar ?tag-ids; (number? ?spec) +
     (identity ?spec) ?tag picks the eid up inside tag-spec->tag *)
  assert_rows
    "tags rule over an eid input matches subclass tags too"
    [ [ Result_entity 1 ]; [ Result_entity 3 ]; [ Result_entity 4 ] ]
    (q_string db
       ~inputs:
         [ Arg_rules (rules_of_string logseq_dsl_rules)
         ; Arg_scalar (Result_value (Set [ Int 17 ]))
         ]
       "[:find ?b :in $ % ?tag-ids :where (tags ?b ?tag-ids)]")

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

(* Perf: the mid-tx schema refresh must fold only the datoms appended since
   the previous refresh — rescanning all of tx_data on every schema-field
   datom was O(n^2) and dominated seed transactions. Schema.folded_datoms
   counts datoms folded by schema_from_transaction_datoms; with the
   incremental refresh it grows linearly with the number of schema datoms. *)
let test_mid_tx_schema_refresh_is_incremental () =
  let entity i =
    Entity
      { db_id = Some (Entity_id (100 + i))
      ; attrs =
          [ "db/ident", One_value (Keyword (Printf.sprintf "user.property/p%d" i))
          ; "db/valueType", One_value (Keyword "db.type/string")
          ; "db/cardinality", One_value (Keyword "db.cardinality/one")
          ; "db/index", One_value (Bool true)
          ]
      }
  in
  let ops = List.init 80 entity in
  let before = !Schema.folded_datoms in
  let db = db_with ops (empty_db ()) in
  let folded = !Schema.folded_datoms - before in
  (* each refresh refolds only the touched entity's accumulated schema
     datoms (~4-6 each): a linear total is ~n * fields, the old rescan
     folded the whole tx_data prefix each time (~n^2/2 datoms) *)
  if folded > 6000 then
    Printf.ksprintf failwith
      "schema refresh folded %d datoms; expected O(n) (a full-tx_data rescan would fold ~n^2/2)"
      folded;
  (match Schema.schema_attr_by_name db.schema "user.property/p79" with
   | Some { indexed = true; _ } -> ()
   | _ -> failf "schema for user.property/p79 missing or wrong after seed tx")

(* Regression: with incremental mid-tx refresh, an attr removal registered by
   an earlier refresh must not re-strip an attr re-installed by a later batch.
   prop/owner is installed on entity 500, retracted (marking it removed),
   re-installed on entity 501, then entity 502's refresh must not drop it —
   a stale removal would collapse its cardinality-many spec to one for later
   datoms (the export/import roundtrip regression). *)
let test_mid_tx_refresh_reapplies_removals_once () =
  let schema_entity e ident card =
    Entity
      { db_id = Some (Entity_id e)
      ; attrs =
          [ "db/ident", One_value (Keyword ident)
          ; "db/valueType", One_value (Keyword "db.type/string")
          ; "db/cardinality", One_value (Keyword card)
          ]
      }
  in
  let db =
    empty_db ()
    |> db_with
         [ schema_entity 500 "prop/owner" "db.cardinality/many"
         ; Retract (Entity_id 500, "db/ident", Some (Keyword "prop/owner"))
         ; schema_entity 501 "prop/owner" "db.cardinality/many"
         ; schema_entity 502 "prop/other" "db.cardinality/one"
         ; Add (Entity_id 1, "prop/owner", String "a")
         ; Add (Entity_id 1, "prop/owner", String "b")
         ]
  in
  (match Schema.schema_attr_by_name db.schema "prop/owner" with
   | Some _ -> ()
   | None -> failf "prop/owner schema entry lost mid-tx by stale removal");
  let vals =
    datoms db Eavt ~e:1 ~a:"prop/owner" ()
    |> List.of_seq
    |> List.map (fun d -> d.v)
    |> List.sort compare
  in
  (match vals with
   | [ String "a"; String "b" ] -> ()
   | _ -> failf "prop/owner should keep cardinality-many mid-tx, got %d datoms" (List.length vals))

let () =
  List.iter
    (fun (name, f) ->
      f ();
      Printf.printf "%s ok\n" name)
    [ "recursive_rules_direction", test_recursive_rules_direction
    ; "rule_branches_positional_binding", test_rule_branches_positional_binding
    ; "recursive_rule_swapped_args", test_recursive_rule_swapped_args
    ; "logseq_parent_rule", test_logseq_parent_rule
    ; "recursive_rule_bound_head_arg", test_recursive_rule_bound_head_arg
    ; "in_scalar_result_entity", test_in_scalar_result_entity
    ; "mutually_recursive_rules", test_mutually_recursive_rules
    ; "rule_joined_with_clauses", test_rule_joined_with_clauses
    ; "predicate_over_in_scalar", test_predicate_over_in_scalar
    ; "collection_in_binding", test_collection_in_binding
    ; "comparison_predicates_over_in", test_comparison_predicates_over_in
    ; "predicate_over_collection_in", test_predicate_over_collection_in
    ; "entity_map_lookup_ref_earlier_tx_entity", test_entity_map_lookup_ref_earlier_tx_entity
    ; "entity_map_lookup_ref_same_entity", test_entity_map_lookup_ref_same_entity
    ; "entity_map_lookup_ref_unresolved_raises", test_entity_map_lookup_ref_unresolved_raises
    ; "entity_map_lookup_ref_later_tx_entity", test_entity_map_lookup_ref_later_tx_entity
    ; "entity_map_lookup_ref_later_tx_entity_many_values", test_entity_map_lookup_ref_later_tx_entity_many_values
    ; "add_op_lookup_ref_later_tx_entity", test_add_op_lookup_ref_later_tx_entity
    ; "entity_map_lookup_ref_never_resolves_raises", test_entity_map_lookup_ref_never_resolves_raises
    ; "many_ref_vector_of_idents_expands", test_many_ref_vector_of_idents_expands
    ; "many_ref_vector_of_same_ident_idempotent", test_many_ref_vector_of_same_ident_idempotent
    ; "many_ref_vector_with_unique_head_is_lookup_ref", test_many_ref_vector_with_unique_head_is_lookup_ref
    ; "entity_map_lookup_ref_vector_form", test_entity_map_lookup_ref_vector_form
    ; "schema_keeps_entry_after_bare_ident_assert", test_schema_keeps_entry_after_bare_ident_assert
    ; "rule_attr_var_has_property", test_rule_attr_var_has_property
    ; "rule_attr_var_property", test_rule_attr_var_property
    ; "rule_tags_query_with_eid_input", test_rule_tags_query_with_eid_input
    ; "edn_symbol_special_chars", test_edn_symbol_special_chars
    ; "retract_cleans_duplicate_avet_tables", test_retract_cleans_duplicate_avet_tables
    ; "mid_tx_schema_refresh_is_incremental", test_mid_tx_schema_refresh_is_incremental
    ; "mid_tx_refresh_reapplies_removals_once", test_mid_tx_refresh_reapplies_removals_once
    ; "bound_non_entity_in_entity_position", test_bound_non_entity_in_entity_position
    ]
