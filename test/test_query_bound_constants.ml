open Datascript

let failf fmt = Printf.ksprintf failwith fmt
let indexed =
  { cardinality = One; unique = None; indexed = true; is_component = false;
    no_history = false; doc = None; value_type = None; tuple_attrs = None; tuple_types = None }

let rules body = Parser.parse_rules (read_edn body)
let scalar n = Result_value (Int64 (Int64.of_int n))
let text s = Result_value (String s)

let fixture () =
  let db = init_db ~schema:["name", indexed; "plain", {indexed with indexed = false}]
    (datom ~e:1 ~a:"name" ~v:(String "Alice") () ::
     datom ~e:2 ~a:"name" ~v:(String "Bob") () ::
     datom ~e:3 ~a:"name" ~v:(String "Alice") () ::
     datom ~e:1 ~a:"plain" ~v:(String "Alice") () ::
     List.init 10_000 (fun i -> datom ~e:(100 + i) ~a:"name" ~v:(String ("Unrelated " ^ string_of_int i)) ())) in
  let storage = memory_storage () in
  ignore (store ~storage db);
  let reads = ref 0 in
  let storage = {storage with storage_restore = (fun addr -> incr reads; storage.storage_restore addr)} in
  let db = Option.get (restore storage) in
  reads := 0;
  db, reads

let named = rules "[[(named ?scope ?b ?title) [?b :name ?title] [(identity ?scope) ?scope]]]"
let query = "[:find ?scope ?b :in $ % [?scope ...] :where (named ?scope ?b \"Alice\")]"

let assert_rows expected actual =
  if List.sort compare expected <> List.sort compare actual then
    let show rows = rows |> List.map (fun row -> row |> List.map (function
      | Result_entity e -> "entity:" ^ string_of_int e
      | Result_value (Int64 v) -> Int64.to_string v
      | Result_value (Keyword v) -> ":" ^ v
      | _ -> "other") |> String.concat ",") |> String.concat ";" in
    failf "expected [%s], got [%s]" (show expected) (show actual)

let run ?(inputs = []) ?(rule_set = named) db query =
  q_string ~inputs:(Arg_rules rule_set :: inputs) db query

let expect_bounded reads =
  if !reads > 80 then failf "bound indexed values should not scan 10k unrelated names: %d storage reads" !reads

let matching_rows () =
  let db, reads = fixture () in
  run ~inputs:[Arg_collection [scalar 10; scalar 11]] db query
  |> assert_rows [[scalar 10; Result_entity 1]; [scalar 10; Result_entity 3];
                  [scalar 11; Result_entity 1]; [scalar 11; Result_entity 3]];
  expect_bounded reads

let missing_rows () =
  let db, reads = fixture () in
  run ~inputs:[Arg_collection [scalar 10; scalar 11]] db
    "[:find ?scope ?b :in $ % [?scope ...] :where (named ?scope ?b \"Nobody\")]"
  |> assert_rows [];
  expect_bounded reads

let varying_values () =
  let db, reads = fixture () in
  let rule_set = rules "[[(named ?scope ?b ?title) [?b :name ?title]]]" in
  run ~rule_set ~inputs:[Arg_relation [[scalar 10; text "Alice"]; [scalar 11; text "Bob"]]] db
    "[:find ?scope ?b :in $ % [[?scope ?title]] :where (named ?scope ?b ?title)]"
  |> assert_rows [[scalar 10; Result_entity 1]; [scalar 10; Result_entity 3]; [scalar 11; Result_entity 2]];
  expect_bounded reads

let source_database () =
  let db, reads = fixture () in
  let rule_set = named in
  q_sources_string ~inputs:[Arg_rules rule_set; Arg_collection [scalar 10; scalar 11]]
    (empty_db ()) ["other", Db_source db]
    "[:find ?scope ?b :in $ $other % [?scope ...] :where ($other named ?scope ?b \"Alice\")]"
  |> assert_rows [[scalar 10; Result_entity 1]; [scalar 10; Result_entity 3];
                  [scalar 11; Result_entity 1]; [scalar 11; Result_entity 3]];
  expect_bounded reads

let transaction_pattern () =
  let db, reads = fixture () in
  let rule_set = rules "[[(named-tx ?scope ?b ?title ?tx ?op) [?b :name ?title ?tx ?op] [(identity ?scope) ?scope]]]" in
  let rows = run ~rule_set ~inputs:[Arg_collection [scalar 10; scalar 11]] db
    "[:find ?scope ?b ?op :in $ % [?scope ...] :where (named-tx ?scope ?b \"Alice\" ?tx ?op)]" in
  assert_rows [[scalar 10; Result_entity 1; Result_value (Keyword "db/add")];
               [scalar 10; Result_entity 3; Result_value (Keyword "db/add")];
               [scalar 11; Result_entity 1; Result_value (Keyword "db/add")];
               [scalar 11; Result_entity 3; Result_value (Keyword "db/add")]] rows;
  expect_bounded reads

let nonindexed_pattern () =
  let db, _ = fixture () in
  let rule_set = rules "[[(plain ?scope ?b ?title) [?b :plain ?title] [(identity ?scope) ?scope]]]" in
  run ~rule_set ~inputs:[Arg_collection [scalar 10; scalar 11]] db
    "[:find ?scope ?b :in $ % [?scope ...] :where (plain ?scope ?b \"Alice\")]"
  |> assert_rows [[scalar 10; Result_entity 1]; [scalar 11; Result_entity 1]]

let dynamic_attribute () =
  let db, reads = fixture () in
  let rule_set = rules "[[(dynamic ?scope ?b ?attr ?title) [?b ?attr ?title] [(identity ?scope) ?scope]]]" in
  run ~rule_set ~inputs:[Arg_collection [scalar 10; scalar 11]] db
    "[:find ?scope ?b :in $ % [?scope ...] :where (dynamic ?scope ?b :name \"Alice\")]"
  |> assert_rows [[scalar 10; Result_entity 1]; [scalar 10; Result_entity 3];
                  [scalar 11; Result_entity 1]; [scalar 11; Result_entity 3]];
  expect_bounded reads

let alias_fixture ?(indexed_alias = true) () =
  let db = init_db ~schema:[
    "db/ident", {indexed with unique = Some Identity};
    "alias", {indexed with indexed = indexed_alias};
    "member", {indexed with cardinality = Many; value_type = Some RefType}]
    ([datom ~e:1 ~a:"db/ident" ~v:(Keyword "target") ();
      datom ~e:2 ~a:"alias" ~v:(Keyword "target") ();
      datom ~e:3 ~a:"alias" ~v:(Ref 1) ()]
     @ List.init 10_000 (fun i -> datom ~e:(100 + i) ~a:"alias" ~v:(Keyword ("other" ^ string_of_int i)) ())
     @ List.init 10_000 (fun i -> datom ~e:(100 + i) ~a:"member" ~v:(Ref 1) ())) in
  let storage = memory_storage () in
  ignore (store ~storage db);
  let reads = ref 0 in
  let counted = {storage with storage_restore = (fun addr -> incr reads; storage.storage_restore addr)} in
  let db = Option.get (restore counted) in
  reads := 0;
  db, reads

let alias_rules = rules "[[(aliased ?scope ?b ?value) [?b :alias ?value] [(identity ?scope) ?scope]]]"

let alias_value_ranges () =
  let db, reads = alias_fixture () in
  run ~rule_set:alias_rules ~inputs:[Arg_collection [scalar 10; scalar 11]] db
    "[:find ?scope ?b :in $ % [?scope ...] :where (aliased ?scope ?b :target)]"
  |> assert_rows [[scalar 10; Result_entity 2]; [scalar 11; Result_entity 2]];
  expect_bounded reads

let bound_entity_alias () =
  List.iter (fun indexed_alias ->
    let db, reads = alias_fixture ~indexed_alias () in
    run ~rule_set:alias_rules ~inputs:[Arg_scalar (Result_entity 2)] db
      "[:find ?b :in $ % ?b :where (aliased 10 ?b :target)]"
    |> assert_rows [[Result_entity 2]];
    expect_bounded reads) [true; false]

let bound_ref_entity () =
  let db, reads = alias_fixture () in
  let rule_set = rules "[[(member ?b ?person) [?b :member ?person]]]" in
  run ~rule_set ~inputs:[Arg_scalar (Result_entity 100)] db
    "[:find ?b :in $ % ?b :where (member ?b :target)]"
  |> assert_rows [[Result_entity 100]];
  expect_bounded reads

let missing_bound_ref_entity () =
  let db, reads = alias_fixture () in
  let rule_set = rules "[[(member ?b ?person) [?b :member ?person]]]" in
  run ~rule_set ~inputs:[Arg_scalar (Result_entity 2)] db
    "[:find ?b :in $ % ?b :where (member ?b :target)]"
  |> assert_rows [];
  expect_bounded reads

let filtered_alias_range () =
  let db, reads = alias_fixture () in
  let db = filter db (fun _ datom -> datom.e <> 2) in
  run ~rule_set:alias_rules ~inputs:[Arg_collection [scalar 10; scalar 11]] db
    "[:find ?scope ?b :in $ % [?scope ...] :where (aliased ?scope ?b :target)]"
  |> assert_rows [];
  expect_bounded reads

let nonindexed_alias_values () =
  let db, _ = alias_fixture ~indexed_alias:false () in
  run ~rule_set:alias_rules ~inputs:[Arg_collection [scalar 10; scalar 11]] db
    "[:find ?scope ?b :in $ % [?scope ...] :where (aliased ?scope ?b :target)]"
  |> assert_rows [[scalar 10; Result_entity 2]; [scalar 11; Result_entity 2]]

let collection_rule_continuations_keep_bulk_context () =
  let count = 1_000 in
  let db = init_db ~schema:["member", {indexed with cardinality = Many; value_type = Some RefType};
                           "extends", {indexed with cardinality = Many; value_type = Some RefType}]
    (datom ~e:11 ~a:"extends" ~v:(Ref 10) () ::
     datom ~e:12 ~a:"extends" ~v:(Ref 11) () ::
     List.init count (fun index -> datom ~e:(100 + index) ~a:"member" ~v:(Ref (10 + index mod 3)) ())) in
  let rule_set = rules "[[(tagged ?b ?tags) [(identity ?tags) [?tag ...]] [?b :member ?tc]
      (or [(= ?tag ?tc)] (inherits ?tag ?tc))]
    [(inherits ?p ?c) [?c :extends ?p]]
    [(inherits ?p ?c) [?t :extends ?p] (inherits ?t ?c)]]" in
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  let rows = run ~rule_set db "[:find ?b :in $ % :where (tagged ?b #{10 11 12})]" in
  let allocated = Gc.allocated_bytes () -. before in
  if List.length rows <> count then failf "expected %d members, got %d" count (List.length rows);
  Printf.printf "collection allocations: %.0f bytes\n%!" allocated;
  (* The per-row continuation allocated 37 MB for this fixture. Allow
     headroom above the batched path while rejecting the repeated expansion. *)
  if allocated > 16_000_000. then
    failf "collection rule continuations must avoid expanding every subsequent clause per row: %.0f bytes" allocated

let bound_entity_rule_continuations_keep_bulk_context () =
  let count = 1_000 in
  let db = init_db ~schema:["member", {indexed with value_type = Some RefType}; "name", indexed]
    (datom ~e:10 ~a:"name" ~v:(String "Alice") () ::
     List.init count (fun index -> datom ~e:(100 + index) ~a:"member" ~v:(Ref 10) ())) in
  let rule_set = rules "[[(member-name ?b ?label) [?b :member ?p] (named ?p ?label)]
    [(named ?p ?label) [?p :name ?label]]]" in
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  let rows = run ~rule_set db
    "[:find ?b :in $ % :where [?b :member _] (member-name ?b \"Alice\")]" in
  let allocated = Gc.allocated_bytes () -. before in
  if List.length rows <> count then failf "expected %d members, got %d" count (List.length rows);
  Printf.printf "bound entity allocations: %.0f bytes\n%!" allocated;
  (* The previous bound-entity fallback allocated over 20 MB. *)
  if allocated > 12_000_000. then
    failf "bound entity rule continuations must preserve bulk evaluation: %.0f bytes" allocated

let derived_attribute_uses_current_bindings scopes () =
  let db = init_db ~schema:["kind", indexed; "db/ident", indexed; "member", indexed]
    (datom ~e:1 ~a:"kind" ~v:(Keyword "property") () ::
     datom ~e:1 ~a:"db/ident" ~v:(Keyword "member") () ::
     datom ~e:2 ~a:"member" ~v:(String "Alice") () ::
     List.init 10_000 (fun index -> datom ~e:(100 + index) ~a:"unrelated" ~v:(String "other") ())) in
  let storage = memory_storage () in
  ignore (store ~storage db);
  let reads = ref 0 in
  let counted = {storage with storage_restore = (fun addr -> incr reads; storage.storage_restore addr)} in
  let db = Option.get (restore counted) in
  reads := 0;
  let rule_set = rules "[[(member-value ?scope ?b ?v)
    [?property :kind :property] [?property :db/ident ?attr] [?b ?attr ?v]
    [(identity ?scope) ?scope]]]" in
  run ~rule_set ~inputs:[Arg_collection (List.map scalar scopes)] db
    "[:find ?scope ?b :in $ % [?scope ...] :where (member-value ?scope ?b \"Alice\")]"
  |> assert_rows (List.map (fun scope -> [scalar scope; Result_entity 2]) scopes);
  expect_bounded reads

let bulk_closure_preserves_scope_and_sources () =
  let db = init_db ~schema:["extends", {indexed with value_type = Some RefType}]
    [datom ~e:2 ~a:"extends" ~v:(Ref 1) ();
     datom ~e:3 ~a:"extends" ~v:(Ref 2) ();
     datom ~e:1 ~a:"extends" ~v:(Ref 3) ();
     datom ~e:5 ~a:"extends" ~v:(Ref 4) ()] in
  let rule_set = rules "[[(inherits ?p ?c) [?c :extends ?p]]
    [(inherits ?p ?c) [?t :extends ?p] (inherits ?t ?c)]]" in
  let input = Arg_relation [[scalar 10; Result_entity 1]; [scalar 11; Result_entity 1];
                            [scalar 12; Result_entity 4]] in
  let expected = List.concat_map (fun (scope, children) ->
    List.map (fun child -> [scalar scope; Result_entity child]) children)
    [10, [1; 2; 3]; 11, [1; 2; 3]; 12, [5]] in
  run ~rule_set ~inputs:[input] db
    "[:find ?scope ?c :in $ % [[?scope ?p]] :where (inherits ?p ?c)]"
  |> assert_rows expected;
  q_sources_string ~inputs:[Arg_rules rule_set; input]
    (empty_db ()) ["other", Db_source db]
    "[:find ?scope ?c :in $ $other % [[?scope ?p]] :where ($other inherits ?p ?c)]"
  |> assert_rows expected

let native_concat_map_avoids_repeated_list_copies () =
  let input = List.init 10_000 Fun.id in
  let calls = ref 0 in
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  let output = List.concat_map (fun value -> incr calls; [value]) input in
  let allocated = Gc.allocated_bytes () -. before in
  if output <> input || !calls <> List.length input then
    failf "concat_map must preserve mapping order and calls";
  if Sys.backend_type = Native && allocated > 550_000. then
    failf "native concat_map must not copy every mapped singleton twice: %.0f bytes" allocated;
  let large = List.init 100_000 Fun.id in
  if List.concat_map (fun value -> if value mod 10_000 = 0 then [value; value + 1] else []) large
     <> List.concat_map (fun value -> [value; value + 1]) (List.init 10 (fun n -> n * 10_000)) then
    failf "concat_map must preserve large sparse collections"

let overlapping_bound_aliases_preserve_rows () =
  let db = init_db ~schema:["db/ident", {indexed with unique = Some Identity};
                           "member", {indexed with value_type = Some RefType}]
    [datom ~e:1 ~a:"db/ident" ~v:(Keyword "target") ();
     datom ~e:2 ~a:"member" ~v:(Ref 1) ()] in
  let rule_set = rules "[[(member-value ?scope ?b ?value) [?b :member ?value]]]" in
  let input = Arg_relation [[scalar 10; Result_value (Ref 1)];
                            [scalar 11; Result_entity 1];
                            [scalar 12; Result_value (Keyword "target")]] in
  run ~rule_set ~inputs:[input] db
    "[:find ?scope ?b :in $ % [[?scope ?value]] :where (member-value ?scope ?b ?value)]"
  |> assert_rows [[scalar 10; Result_entity 2]; [scalar 11; Result_entity 2]];
  run ~rule_set ~inputs:[input] db
    "[:find (count ?b) :with ?scope :in $ % [[?scope ?value]] :where (member-value ?scope ?b ?value)]"
  |> assert_rows [[scalar 2]]

let varying_nonindexed_values_scan_once () =
  let db = init_db ~schema:["plain", {indexed with indexed = false}]
    (List.init 10_000 (fun index -> datom ~e:(100 + index) ~a:"plain" ~v:(String (string_of_int index)) ())) in
  let rule_set = rules "[[(named ?scope ?b ?title) [?b :plain ?title]]]" in
  let inputs = Arg_relation (List.init 20 (fun index -> [scalar index; text (string_of_int index)])) in
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  let rows = run ~rule_set ~inputs:[inputs] db
    "[:find ?scope ?b :in $ % [[?scope ?title]] :where (named ?scope ?b ?title)]" in
  let allocated = Gc.allocated_bytes () -. before in
  assert_rows (List.init 20 (fun index -> [scalar index; Result_entity (100 + index)])) rows;
  if allocated > 16_000_000. then
    failf "varying nonindexed values must share one scan: %.0f bytes" allocated

let () =
  let failed = ref false in
  List.iter (fun (name, test) ->
    try test (); Printf.printf "PASS %s\n%!" name
    with exn -> failed := true; Printf.eprintf "FAIL %s: %s\n%!" name (Printexc.to_string exn))
    ["matching rows", matching_rows; "missing rows", missing_rows;
     "varying bound values", varying_values; "source database", source_database;
     "transaction and operation columns", transaction_pattern;
     "nonindexed values", nonindexed_pattern; "dynamic attributes", dynamic_attribute;
     "indexed keyword aliases use bounded ranges", alias_value_ranges;
     "bound literal entity checks remain indexed", bound_entity_alias;
     "bound ref entities avoid shared value scans", bound_ref_entity;
     "missing bound ref entities avoid shared value scans", missing_bound_ref_entity;
     "filtered aliases retain source predicates", filtered_alias_range;
     "nonindexed aliases retain value semantics", nonindexed_alias_values;
     "collection rules preserve bulk continuation", collection_rule_continuations_keep_bulk_context;
     "bound entity rules preserve bulk continuation", bound_entity_rule_continuations_keep_bulk_context;
     "derived attributes use current bindings", derived_attribute_uses_current_bindings [10; 11];
     "single-row derived attributes use current bindings", derived_attribute_uses_current_bindings [10];
     "bulk closures preserve scope and sources", bulk_closure_preserves_scope_and_sources;
     "native concat_map avoids repeated copies", native_concat_map_avoids_repeated_list_copies;
     "overlapping aliases preserve rows", overlapping_bound_aliases_preserve_rows;
     "varying nonindexed values scan once", varying_nonindexed_values_scan_once];
  if !failed then exit 1
