open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let ref_many =
  { cardinality = Many
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = Some RefType
  ; tuple_attrs = None
  ; tuple_types = None
  }

let ref_one =
  { ref_many with cardinality = One }

let one =
  { ref_many with cardinality = One; value_type = None }

let unique_identity =
  { one with unique = Some Identity; indexed = true }

let timed label max_seconds f =
  let started = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. started in
  if elapsed > max_seconds then
    failf "%s took %.3fs, expected <= %.3fs" label elapsed max_seconds;
  result

let entity_ids_of_collection = function
  | Query_collection values ->
    values
    |> List.map (function
      | Result_entity entity_id -> entity_id
      | _ -> failwith "expected entity id")
    |> List.sort compare
  | _ -> failwith "expected collection query result"

let entity_pairs_of_relation rows =
  rows
  |> List.map (function
    | [ Result_entity left; Result_entity right ] -> left, right
    | _ -> failwith "expected entity pair")
  |> List.sort compare

let assert_equal_ints label expected actual =
  let actual = List.sort compare actual in
  if expected <> actual then
    failf
      "%s: expected [%s], got [%s]"
      label
      (expected |> List.map string_of_int |> String.concat "; ")
      (actual |> List.map string_of_int |> String.concat "; ")

let assert_equal_pairs label expected actual =
  let actual = List.sort compare actual in
  if expected <> actual then
    let format pairs =
      pairs
      |> List.map (fun (left, right) -> Printf.sprintf "(%d,%d)" left right)
      |> String.concat "; "
    in
    failf "%s: expected [%s], got [%s]" label (format expected) (format actual)

let value_label = function
  | Keyword value -> ":" ^ value
  | String value -> Printf.sprintf "%S" value
  | value -> Printf.sprintf "%d" (Hashtbl.hash value)

let pulled_attr attr attrs =
  match List.assoc_opt attr attrs with
  | Some value -> value
  | None ->
    failwith
      ("missing pulled attr; keys: "
       ^ (attrs |> List.map (fun (key, _) -> value_label key) |> String.concat ", "))

let test_wildcard_pull_uses_upstream_value_order_for_scalar_ties () =
  let normal_icon = Map [ Keyword "id", String "robot_face"; Keyword "type", Keyword "emoji" ] in
  let inverted_icon = Map [ Keyword "id", String "robot_face"; Keyword "emoji", Keyword "type" ] in
  let db =
    init_db
      ~schema:[ "block/name", one; "logseq.property/icon", one ]
      [ datom ~e:1 ~a:"block/name" ~v:(String "robot") ~tx:10 ()
      ; datom ~e:1 ~a:"logseq.property/icon" ~v:normal_icon ~tx:20 ()
      ; datom ~e:1 ~a:"logseq.property/icon" ~v:inverted_icon ~tx:20 ()
      ]
  in
  match q_return_string db "[:find (pull ?b [*]) :where [?b :block/name]]" with
  | Query_relation [ [ Result_pull entity ] ] ->
    (match pulled_attr (Keyword "logseq.property/icon") entity.pulled_attrs with
     | Pulled_scalar value ->
       if not (Db.value_equal (Util.normalize_value inverted_icon) value) then
         failf
           "wildcard pull should keep the same scalar value as upstream DataScript, got %s"
           (value_label value)
     | _ -> failwith "expected scalar icon pull value")
  | _ -> failwith "expected one pulled entity"

let test_comment_area_parent_join_uses_indexed_shape () =
  let comment_tag = 10 in
  let count = 1_200 in
  let datoms =
    datom ~e:comment_tag ~a:"db/ident" ~v:(Keyword "logseq.class/Comments") ()
    :: List.concat
         (List.init count (fun index ->
            let area = 1_000 + index in
            let parent = 10_000 + index in
            [ datom ~e:area ~a:"block/tags" ~v:(Ref comment_tag) ()
            ; datom ~e:area ~a:"block/parent" ~v:(Ref parent) ()
            ; datom ~e:(20_000 + index) ~a:"block/title" ~v:(String "unrelated") ()
            ]))
  in
  let db = init_db ~schema:[ "db/ident", unique_identity; "block/tags", ref_many; "block/parent", ref_one ] datoms in
  let expected =
    List.init count (fun index -> 1_000 + index, 10_000 + index)
  in
  let rows =
    timed "comment-area parent join" 0.500 (fun () ->
      q_string
        db
        "[:find ?comments-area-id ?parent-id :where [?comments-area-id :block/tags :logseq.class/Comments] [?comments-area-id :block/parent ?parent-id]]")
  in
  assert_equal_pairs "comment-area parent join results" expected (entity_pairs_of_relation rows)

let test_comment_parent_reverse_join_uses_ref_value_lookup () =
  let comment_tag = 10 in
  let count = 1_000 in
  let datoms =
    datom ~e:comment_tag ~a:"db/ident" ~v:(Keyword "logseq.class/Comments") ()
    :: List.concat
         (List.init count (fun index ->
            let area = 1_000 + index in
            let comment = 10_000 + index in
            [ datom ~e:area ~a:"block/tags" ~v:(Ref comment_tag) ()
            ; datom ~e:comment ~a:"block/parent" ~v:(Ref area) ()
            ; datom ~e:(20_000 + index) ~a:"block/parent" ~v:(Ref (30_000 + index)) ()
            ]))
  in
  let db = init_db ~schema:[ "db/ident", unique_identity; "block/tags", ref_many; "block/parent", ref_one ] datoms in
  let expected = List.init count (fun index -> 10_000 + index) in
  let result =
    timed "comment reverse parent join" 0.500 (fun () ->
      q_return_string
        db
        "[:find [?comment ...] :where [?comments-area :block/tags :logseq.class/Comments] [?comment :block/parent ?comments-area]]")
  in
  assert_equal_ints "comment reverse parent join results" expected (entity_ids_of_collection result)

let test_wildcard_pull_single_attr_pattern_uses_bounded_entity_scan () =
  let count = 1_200 in
  let datoms =
    List.concat
      (List.init count (fun index ->
         let entity = 1_000 + index in
         [ datom ~e:entity ~a:"block/uuid" ~v:(Uuid (Printf.sprintf "00000000-0000-0000-0000-%012d" entity)) ()
         ; datom ~e:entity ~a:"block/title" ~v:(String (Printf.sprintf "Block %d" entity)) ()
         ; datom ~e:entity ~a:"block/order" ~v:(String "a") ()
         ]))
  in
  let db = init_db ~schema:[ "block/uuid", unique_identity; "block/title", one; "block/order", one ] datoms in
  match
    timed "wildcard pull attr-present query" 0.750 (fun () ->
      q_return_string db "[:find [(pull ?b [*]) ...] :where [?b :block/uuid]]")
  with
  | Query_collection values ->
    if List.length values <> count then
      failf "wildcard pull should return %d entities, got %d" count (List.length values)
  | _ -> failwith "expected collection pull result"

let test_wildcard_pull_page_missing_query_uses_bounded_entity_scan () =
  let page_count = 20 in
  let noise_count = 500_000 in
  let page_datoms =
    List.concat
      (List.init page_count (fun index ->
         let page = 1_000 + index in
         let block = 10_000 + index in
         [ datom ~e:page ~a:"block/name" ~v:(String (Printf.sprintf "page-%d" index)) ()
         ; datom ~e:page ~a:"block/title" ~v:(String (Printf.sprintf "Page %d" index)) ()
         ; datom ~e:block ~a:"block/title" ~v:(String (Printf.sprintf "Block %d" index)) ()
         ; datom ~e:block ~a:"block/page" ~v:(Ref page) ()
         ]))
  in
  let noise_datoms =
    List.init noise_count (fun index ->
      datom ~e:(100_000 + index) ~a:"noise/value" ~v:(String (Printf.sprintf "noise-%d" index)) ())
  in
  let db =
    init_db
      ~schema:
        [ "block/name", one
        ; "block/title", one
        ; "block/page", ref_one
        ; "logseq.property/built-in?", one
        ; "noise/value", one
        ]
      (page_datoms @ noise_datoms)
  in
  match
    timed "wildcard pull page missing query" 0.250 (fun () ->
      q_return_string
        db
        "[:find (pull ?p [*]) :where [?b :block/title] [?b :block/page ?p] [(missing? $ ?p :logseq.property/built-in?)]]")
  with
  | Query_relation rows ->
    if List.length rows <> page_count then
      failf "wildcard pull page query should return %d pages, got %d" page_count (List.length rows)
  | _ -> failwith "expected relation pull result"

let test_tag_value_with_present_attr_uses_indexed_intersection () =
  let tag = 10 in
  let tagged_count = 60 in
  let noise_count = 120_000 in
  let tagged_datoms =
    datom ~e:tag ~a:"db/ident" ~v:(Keyword "logseq.class/Tag") ()
    :: List.concat
         (List.init tagged_count (fun index ->
            let entity = 1_000 + index in
            [ datom ~e:entity ~a:"block/tags" ~v:(Ref tag) ()
            ; datom ~e:entity ~a:"block/uuid" ~v:(Uuid (Printf.sprintf "00000000-0000-0000-0000-%012d" entity)) ()
            ]))
  in
  let noise_datoms =
    List.init noise_count (fun index ->
      datom ~e:(100_000 + index) ~a:"noise/value" ~v:(String (Printf.sprintf "noise-%d" index)) ())
  in
  let db =
    init_db
      ~schema:
        [ "db/ident", unique_identity
        ; "block/tags", ref_many
        ; "block/uuid", unique_identity
        ; "noise/value", one
        ]
      (tagged_datoms @ noise_datoms)
  in
  match
    timed "tag value with present attr query" 0.250 (fun () ->
      q_return_string
        db
        "[:find [?e ...] :where [?e :block/tags :logseq.class/Tag] [?e :block/uuid]]")
  with
  | Query_collection values ->
    if List.length values <> tagged_count then
      failf "tag value query should return %d entities, got %d" tagged_count (List.length values)
  | _ -> failwith "expected collection result"

let test_tag_value_without_attr_uses_indexed_difference () =
  let tag = 10 in
  let count = 80 in
  let noise_count = 120_000 in
  let tagged_datoms =
    datom ~e:tag ~a:"db/ident" ~v:(Keyword "logseq.class/Tag") ()
    :: List.concat
         (List.init count (fun index ->
            let entity = 1_000 + index in
            [ datom ~e:entity ~a:"block/tags" ~v:(Ref tag) ()
            ; datom ~e:entity ~a:"block/uuid" ~v:(Uuid (Printf.sprintf "00000000-0000-0000-0000-%012d" entity)) ()
            ]
            @
            if index mod 2 = 0 then
              [ datom ~e:entity ~a:"logseq.property/built-in?" ~v:(Bool true) () ]
            else
              []))
  in
  let noise_datoms =
    List.init noise_count (fun index ->
      datom ~e:(100_000 + index) ~a:"noise/value" ~v:(String (Printf.sprintf "noise-%d" index)) ())
  in
  let db =
    init_db
      ~schema:
        [ "db/ident", unique_identity
        ; "block/tags", ref_many
        ; "block/uuid", unique_identity
        ; "logseq.property/built-in?", one
        ; "noise/value", one
        ]
      (tagged_datoms @ noise_datoms)
  in
  match
    timed "tag value without attr query" 0.250 (fun () ->
      q_return_string
        db
        "[:find [?e ...] :where [?e :block/tags :logseq.class/Tag] (not [?e :logseq.property/built-in?])]")
  with
  | Query_collection values ->
    if List.length values <> count / 2 then
      failf "tag value without attr query should return %d entities, got %d" (count / 2) (List.length values)
  | _ -> failwith "expected collection result"

let test_tag_value_ident_without_attr_allows_reversed_clause_order () =
  let property_tag = 10 in
  let count = 80 in
  let noise_count = 120_000 in
  let property_datoms =
    datom ~e:property_tag ~a:"db/ident" ~v:(Keyword "logseq.class/Property") ()
    :: List.concat
         (List.init count (fun index ->
            let entity = 1_000 + index in
            [ datom ~e:entity ~a:"db/ident" ~v:(Keyword (Printf.sprintf "user.property/p%d" index)) ()
            ; datom ~e:entity ~a:"block/tags" ~v:(Ref property_tag) ()
            ]
            @
            if index mod 2 = 0 then
              [ datom ~e:entity ~a:"logseq.property/built-in?" ~v:(Bool true) () ]
            else
              []))
  in
  let noise_datoms =
    List.init noise_count (fun index ->
      datom ~e:(100_000 + index) ~a:"noise/value" ~v:(String (Printf.sprintf "noise-%d" index)) ())
  in
  let db =
    init_db
      ~schema:
        [ "db/ident", unique_identity
        ; "block/tags", ref_many
        ; "logseq.property/built-in?", one
        ; "noise/value", one
        ]
      (property_datoms @ noise_datoms)
  in
  match
    timed "tag value ident without attr query" 0.250 (fun () ->
      q_return_string
        db
        "[:find [?ident ...] :where [?p :db/ident ?ident] [?p :block/tags :logseq.class/Property] (not [?p :logseq.property/built-in?])]")
  with
  | Query_collection values ->
    if List.length values <> count / 2 then
      failf "tag value ident query should return %d idents, got %d" (count / 2) (List.length values)
  | _ -> failwith "expected collection result"

let test_page_ref_pairs_with_tagged_non_journal_pages_use_indexed_join () =
  let tag = 10 in
  let journal = 11 in
  let page_count = 80 in
  let noise_count = 120_000 in
  let page_datoms =
    [ datom ~e:tag ~a:"db/ident" ~v:(Keyword "logseq.class/Tag") ()
    ; datom ~e:journal ~a:"db/ident" ~v:(Keyword "logseq.class/Journal") ()
    ]
    @ List.concat
        (List.init page_count (fun index ->
           let page = 1_000 + index in
           let block = 10_000 + index in
           [ datom ~e:page ~a:"block/tags" ~v:(Ref tag) ()
           ; datom ~e:block ~a:"block/page" ~v:(Ref page) ()
           ; datom ~e:block ~a:"block/refs" ~v:(Ref (20_000 + index)) ()
           ]
           @
           if index mod 2 = 0 then
             [ datom ~e:page ~a:"block/tags" ~v:(Ref journal) () ]
           else
             []))
  in
  let noise_datoms =
    List.init noise_count (fun index ->
      datom ~e:(100_000 + index) ~a:"noise/value" ~v:(String (Printf.sprintf "noise-%d" index)) ())
  in
  let db =
    init_db
      ~schema:
        [ "db/ident", unique_identity
        ; "block/tags", ref_many
        ; "block/page", ref_one
        ; "block/refs", ref_many
        ; "noise/value", one
        ]
      (page_datoms @ noise_datoms)
  in
  match
    timed "tagged non-journal page ref pairs" 0.350 (fun () ->
      q_return_string
        db
        "[:find ?p ?ref-page :where [?block :block/page ?p] [?p :block/tags] (not [?p :block/tags :logseq.class/Journal]) [?block :block/refs ?ref-page]]")
  with
  | Query_relation rows ->
    if List.length rows <> page_count / 2 then
      failf "page ref query should return %d rows, got %d" (page_count / 2) (List.length rows)
  | _ -> failwith "expected relation result"

let test_missing_property_ident_rule_call_returns_empty_without_rule_scan () =
  let rules =
    Parser.parse_rules
      (read_edn
         "[[[has-property ?b ?prop]
           [?x :noise/value ?v]
           [?b ?prop _]
           [?prop-e :db/ident ?prop]
           [?prop-e :block/tags :logseq.class/Property]]]")
  in
  let db =
    init_db
      ~schema:[ "block/title", one; "noise/value", one ]
      (List.init 120_000 (fun index ->
         datom ~e:(100_000 + index) ~a:"noise/value" ~v:(String (Printf.sprintf "noise-%d" index)) ()))
  in
  match
    timed "missing property ident rule call" 0.250 (fun () ->
      q_return_string
        ~inputs:[ Arg_rules rules ]
        db
        "[:find (pull ?b [:block/title]) :where (has-property ?b :user.property/foo) :in $ %]")
  with
  | Query_relation [] -> ()
  | Query_relation rows -> failf "missing property ident should return no rows, got %d" (List.length rows)
  | _ -> failwith "expected relation result"

let test_has_property_with_missing_title_returns_empty_without_rule_scan () =
  let rules =
    Parser.parse_rules
      (read_edn
         "[[[has-property ?b ?prop]
           [?x :noise/value ?v]
           [?b ?prop _]
           [?prop-e :db/ident ?prop]]]")
  in
  let db =
    init_db
      ~schema:[ "block/title", one; "noise/value", one ]
      (List.init 120_000 (fun index ->
         datom ~e:(100_000 + index) ~a:"noise/value" ~v:(String (Printf.sprintf "noise-%d" index)) ()))
  in
  match
    timed "has-property missing title" 0.250 (fun () ->
      q_return_string
        ~inputs:[ Arg_rules rules ]
        db
        "[:find [?p ...] :where (has-property ?b ?p) [?b :block/title \"Page1\"] :in $ %]")
  with
  | Query_collection [] -> ()
  | Query_collection values -> failf "missing title should return no values, got %d" (List.length values)
  | _ -> failwith "expected collection result"

let () =
  test_wildcard_pull_uses_upstream_value_order_for_scalar_ties ();
  test_comment_area_parent_join_uses_indexed_shape ();
  test_comment_parent_reverse_join_uses_ref_value_lookup ();
  test_wildcard_pull_single_attr_pattern_uses_bounded_entity_scan ();
  test_wildcard_pull_page_missing_query_uses_bounded_entity_scan ();
  test_tag_value_with_present_attr_uses_indexed_intersection ();
  test_tag_value_without_attr_uses_indexed_difference ();
  test_tag_value_ident_without_attr_allows_reversed_clause_order ();
  test_page_ref_pairs_with_tagged_non_journal_pages_use_indexed_join ();
  test_missing_property_ident_rule_call_returns_empty_without_rule_scan ();
  test_has_property_with_missing_title_returns_empty_without_rule_scan ()
