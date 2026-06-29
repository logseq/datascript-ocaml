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

let perf_threshold_multiplier () =
  (* Normal test runs use coarse wall-clock gates; set this to 1 for strict local checks. *)
  match Sys.getenv_opt "DATASCRIPT_TEST_PERF_MULTIPLIER" with
  | None -> 4.0
  | Some value ->
    (try max 1.0 (float_of_string value) with
     | Failure _ -> 4.0)

let timed label max_seconds f =
  Gc.full_major ();
  let started = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. started in
  let effective_max_seconds = max_seconds *. perf_threshold_multiplier () in
  if elapsed > effective_max_seconds then
    failf
      "%s took %.3fs, expected <= %.3fs"
      label
      elapsed
      effective_max_seconds;
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

let test_source_comment_parent_join_uses_relation_ref_lookup () =
  let comment_tag = 10 in
  let count = 1_000 in
  let noise_count = 120_000 in
  let area_datoms =
    datom ~e:comment_tag ~a:"db/ident" ~v:(Keyword "logseq.class/Comments") ()
    :: List.concat
         (List.init count (fun index ->
            let area = 1_000 + index in
            [ datom ~e:area ~a:"block/tags" ~v:(Ref comment_tag) () ]))
  in
  let comment_datoms =
    List.concat
      (List.init count (fun index ->
         let area = 1_000 + index in
         let comment = 10_000 + index in
         [ datom ~e:comment ~a:"block/parent" ~v:(Ref area) ()
         ; datom ~e:(100_000 + index) ~a:"block/parent" ~v:(Ref (200_000 + index)) ()
         ]))
    @ List.init noise_count (fun index ->
        datom ~e:(300_000 + index) ~a:"noise/value" ~v:(String (Printf.sprintf "noise-%d" index)) ())
  in
  let areas = init_db ~schema:[ "db/ident", unique_identity; "block/tags", ref_many ] area_datoms in
  let comments = init_db ~schema:[ "block/parent", ref_one; "noise/value", one ] comment_datoms in
  let query =
    { find = [ Find_var "comment" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ SourcePattern ("areas", QVar "comments-area", QAttr "block/tags", QValue (Keyword "logseq.class/Comments"))
        ; SourcePattern ("comments", QVar "comment", QAttr "block/parent", QVar "comments-area")
        ]
    }
  in
  let rows =
    timed "source comment reverse parent join" 0.120 (fun () ->
      q_sources (empty_db ()) [ "areas", Db_source areas; "comments", Db_source comments ] query)
  in
  if List.length rows <> count then
    failf "source comment reverse parent join should return %d rows, got %d" count (List.length rows)

let test_source_namespace_value_join_uses_relation_functions () =
  let count = 80_000 in
  let datoms =
    List.concat
      (List.init count (fun index ->
         let entity = 1_000 + index in
         let ident =
           if index mod 10 = 0 then
             Printf.sprintf "user.property/p%d" index
           else
             Printf.sprintf "system.property/p%d" index
         in
         [ datom ~e:entity ~a:"db/ident" ~v:(Keyword ident) ()
         ; datom ~e:entity ~a:"property/value" ~v:(String (Printf.sprintf "value-%d" index)) ()
         ]))
  in
  let db = init_db ~schema:[ "db/ident", unique_identity; "property/value", one ] datoms in
  let query =
    { find = [ Find_var "ident"; Find_var "value" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ SourcePattern ("props", QVar "p", QAttr "db/ident", QVar "ident")
        ; NamespaceValue (QVar "ident", "namespace")
        ; EqualityPredicate (EqualValues, [ QVar "namespace"; QValue (String "user.property") ])
        ; SourcePattern ("props", QVar "p", QAttr "property/value", QVar "value")
        ]
    }
  in
  let rows =
    timed "source namespace value relation join" 0.120 (fun () ->
      q_sources (empty_db ()) [ "props", Db_source db ] query)
  in
  if List.length rows <> count / 10 then
    failf "source namespace value join should return %d rows, got %d" (count / 10) (List.length rows)

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

let test_attr_prefix_query_ignores_unrelated_duplicate_datoms () =
  let duplicate_count = 120_000 in
  let target_count = 500 in
  let duplicate_noise =
    List.concat
      (List.init duplicate_count (fun index ->
         let d = datom ~e:(100_000 + index) ~a:"aaa/noise" ~v:(String "duplicate") () in
         [ d; d ]))
  in
  let targets =
    List.init target_count (fun index ->
      let entity = 1_000 + index in
      datom ~e:entity ~a:"block/uuid" ~v:(Uuid (Printf.sprintf "00000000-0000-0000-0000-%012d" entity)) ())
  in
  let db =
    init_db
      ~schema:[ "aaa/noise", one; "block/uuid", unique_identity ]
      (duplicate_noise @ targets)
  in
  match
    timed "attr prefix query ignores unrelated duplicate datoms" 0.120 (fun () ->
      q_return_string db "[:find [?e ...] :where [?e :block/uuid]]")
  with
  | Query_collection values ->
    if List.length values <> target_count then
      failf "attr prefix query should return %d entities, got %d" target_count (List.length values)
  | _ -> failwith "expected collection query result"

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

let test_simple_not_uses_relation_antijoin () =
  let count = 120_000 in
  let datoms =
    List.concat
      (List.init count (fun index ->
         let entity = 1_000 + index in
         [ datom ~e:entity ~a:"item/value" ~v:(Int index) () ]
         @
         if index mod 2 = 0 then
           [ datom ~e:entity ~a:"item/excluded?" ~v:(Bool true) () ]
         else
           []))
  in
  let db = init_db ~schema:[ "item/value", one; "item/excluded?", one ] datoms in
  match
    timed "simple not relation antijoin" 0.300 (fun () ->
      q_return_string
        db
        "[:find [?e ...] :where [?e :item/value ?v] (not [?e :item/excluded?])]")
  with
  | Query_collection values ->
    if List.length values <> count / 2 then
      failf "simple not should return %d entities, got %d" (count / 2) (List.length values)
  | _ -> failwith "expected collection result"

let test_source_not_uses_relation_antijoin () =
  let count = 120_000 in
  let item_datoms =
    List.init count (fun index ->
      let entity = 1_000 + index in
      datom ~e:entity ~a:"item/value" ~v:(Int index) ())
  in
  let excluded_datoms =
    List.filter_map
      (fun index ->
        if index mod 2 = 0 then
          let entity = 1_000 + index in
          Some (datom ~e:entity ~a:"item/excluded?" ~v:(Bool true) ())
        else
          None)
      (List.init count Fun.id)
  in
  let items = init_db ~schema:[ "item/value", one ] item_datoms in
  let excluded = init_db ~schema:[ "item/excluded?", one ] excluded_datoms in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ SourcePattern ("items", QVar "e", QAttr "item/value", QVar "v")
        ; SourceNot ("excluded", [ Pattern (QVar "e", QAttr "item/excluded?", QWildcard) ])
        ]
    }
  in
  let rows =
    timed "source not relation antijoin" 0.300 (fun () ->
      q_sources (empty_db ()) [ "items", Db_source items; "excluded", Db_source excluded ] query)
  in
  if List.length rows <> count / 2 then
    failf "source not should return %d entities, got %d" (count / 2) (List.length rows)

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

let test_source_page_ref_pairs_with_tagged_non_journal_pages_use_relation_evaluator () =
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
  let rows =
    timed "source tagged non-journal page ref pairs" 0.350 (fun () ->
      q_sources_string
        (empty_db ())
        [ "graph", Db_source db ]
        "[:find ?p ?ref-page
          :in $ $graph
          :where [$graph ?block :block/page ?p]
                 [$graph ?p :block/tags]
                 (not [$graph ?p :block/tags :logseq.class/Journal])
                 [$graph ?block :block/refs ?ref-page]]")
  in
  if List.length rows <> page_count / 2 then
    failf "source page ref query should return %d rows, got %d" (page_count / 2) (List.length rows)

let test_source_incoming_ref_without_attr_uses_relation_antijoin () =
  let count = 1_000 in
  let noise_count = 120_000 in
  let datoms =
    List.concat
      (List.init count (fun index ->
         let page = 1_000 + index in
         let block = 10_000 + index in
         [ datom ~e:page ~a:"block/title" ~v:(String (Printf.sprintf "Page %d" index)) ()
         ; datom ~e:block ~a:"block/refs" ~v:(Ref page) ()
         ]
         @
         if index mod 2 = 0 then
           [ datom ~e:page ~a:"logseq.property/built-in?" ~v:(Bool true) () ]
         else
           []))
    @ List.init noise_count (fun index ->
        datom ~e:(100_000 + index) ~a:"noise/value" ~v:(String (Printf.sprintf "noise-%d" index)) ())
  in
  let db =
    init_db
      ~schema:
        [ "block/title", one
        ; "block/refs", ref_many
        ; "logseq.property/built-in?", one
        ; "noise/value", one
        ]
      datoms
  in
  let query =
    { find = [ Find_var "page" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ SourcePattern ("graph", QVar "page", QAttr "block/title", QWildcard)
        ; SourcePattern ("graph", QWildcard, QAttr "block/refs", QVar "page")
        ; SourceNot ("graph", [ Pattern (QVar "page", QAttr "logseq.property/built-in?", QWildcard) ])
        ]
    }
  in
  let rows =
    timed "source incoming ref without attr" 0.120 (fun () ->
      q_sources (empty_db ()) [ "graph", Db_source db ] query)
  in
  if List.length rows <> count / 2 then
    failf "source incoming ref without attr should return %d rows, got %d" (count / 2) (List.length rows)

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

let test_source_missing_property_ident_rule_call_uses_rule_prefix_context () =
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
  let rows =
    timed "source missing property ident rule call" 0.250 (fun () ->
      q_sources_string
        ~inputs:[ Arg_rules rules ]
        db
        []
        "[:find ?b :where (has-property ?b :user.property/foo) :in $ %]")
  in
  if rows <> [] then
    failf "source missing property ident should return no rows, got %d" (List.length rows)

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

let test_source_has_property_with_missing_title_uses_rule_prefix_context () =
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
  let rows =
    timed "source has-property missing title" 0.250 (fun () ->
      q_sources_string
        ~inputs:[ Arg_rules rules ]
        db
        []
        "[:find ?p :where (has-property ?b ?p) [?b :block/title \"Page1\"] :in $ %]")
  in
  if rows <> [] then
    failf "source missing title should return no rows, got %d" (List.length rows)

let test_has_property_with_bound_title_uses_rule_suffix_context () =
  let rules =
    Parser.parse_rules
      (read_edn
         "[[[has-property ?b ?prop]
           [?b ?prop _]
           [?prop-e :db/ident ?prop]]]")
  in
  let count = 180_000 in
  let db =
    init_db
      ~schema:[ "block/title", one; "db/ident", unique_identity; "noise/value", one ]
      ([ datom ~e:1 ~a:"block/title" ~v:(String "Page1") ()
       ; datom ~e:2 ~a:"db/ident" ~v:(Keyword "noise/value") ()
       ]
       @ List.init count (fun index ->
         datom ~e:(100_000 + index) ~a:"noise/value" ~v:(Int index) ()))
  in
  match
    timed "has-property bound title suffix context" 0.250 (fun () ->
      q_return_string
        ~inputs:[ Arg_rules rules ]
        db
        "[:find [?p ...] :where (has-property ?b ?p) [?b :block/title \"Page1\"] :in $ %]")
  with
  | Query_collection [] -> ()
  | Query_collection values -> failf "bound title should return no properties, got %d" (List.length values)
  | _ -> failwith "expected collection result"

let test_ref_property_with_bound_title_uses_rule_suffix_context () =
  let rules =
    Parser.parse_rules
      (read_edn
         "[[[ref-property ?b ?prop ?val]
           [?b ?prop ?pv]
           [?prop-e :db/ident ?prop]
           (ref->val ?pv ?val)]
          [[ref->val ?pv ?val]
           [?pv :block/title ?val]]]")
  in
  let count = 180_000 in
  let target = 99 in
  let db =
    init_db
      ~schema:[ "block/title", one; "db/ident", unique_identity; "noise/ref", ref_one ]
      ([ datom ~e:1 ~a:"block/title" ~v:(String "Page1") ()
       ; datom ~e:2 ~a:"db/ident" ~v:(Keyword "noise/ref") ()
       ; datom ~e:target ~a:"block/title" ~v:(String "bar") ()
       ]
       @ List.init count (fun index ->
         datom ~e:(100_000 + index) ~a:"noise/ref" ~v:(Ref target) ()))
  in
  match
    timed "ref-property bound title suffix context" 0.250 (fun () ->
      q_return_string
        ~inputs:[ Arg_rules rules ]
        db
        "[:find [?p ...] :where (ref-property ?b ?p \"bar\") [?b :block/title \"Page1\"] :in $ %]")
  with
  | Query_collection [] -> ()
  | Query_collection values -> failf "bound title should return no ref properties, got %d" (List.length values)
  | _ -> failwith "expected collection result"

let test_source_task_page_ref_literal_string_uses_rule_prefix_context () =
  let rules =
    Parser.parse_rules
      (read_edn
         "[[[task ?b]
           [?x :noise/value ?v]
           [?b :block/marker ?m]]
          [[page-ref ?b ?ref]
           [?b :block/refs ?ref]]]")
  in
  let db =
    init_db
      ~schema:[ "block/marker", one; "block/refs", ref_many; "noise/value", one ]
      (List.concat
         [ List.init 2_000 (fun index ->
             datom ~e:(1_000 + index) ~a:"block/marker" ~v:(String "TODO") ())
         ; List.init 120_000 (fun index ->
             datom ~e:(100_000 + index) ~a:"noise/value" ~v:(String (Printf.sprintf "noise-%d" index)) ())
         ])
  in
  let rows =
    timed "source task page-ref literal string" 0.250 (fun () ->
      q_sources_string
        ~inputs:[ Arg_rules rules ]
        db
        []
        "[:find ?b :where (task ?b) (page-ref ?b \"missing-page\") :in $ %]")
  in
  if rows <> [] then
    failf "source task page-ref literal string should return no rows, got %d" (List.length rows)

let test_scalar_title_query_rejects_non_string_input_without_title_scan () =
  let tag = 10 in
  let count = 180_000 in
  let datoms =
    datom ~e:tag ~a:"db/ident" ~v:(Keyword "logseq.class/Tag") ()
    :: List.concat
         (List.init count (fun index ->
            let entity = 1_000 + index in
            [ datom ~e:entity ~a:"block/title" ~v:(String (Printf.sprintf "Title %d" index)) ()
            ; datom ~e:entity ~a:"block/tags" ~v:(Ref tag) ()
            ]))
  in
  let db = init_db ~schema:[ "db/ident", unique_identity; "block/title", one; "block/tags", ref_many ] datoms in
  match
    timed "scalar title non-string input" 0.006 (fun () ->
      q_return_string
        ~inputs:
          [ Arg_scalar (Result_value (Keyword "logseq.class/Tag"))
          ; Arg_scalar (Result_value (Keyword "logseq.class/Tag"))
          ]
        db
        "[:find ?other .
          :in $ ?class-title ?class-id
          :where [?other :block/title ?class-title]
                 [?other :block/tags :logseq.class/Tag]
                 [(not= ?other ?class-id)]
                 (not [?other :logseq.property/deleted-at])]")
  with
  | Query_scalar None -> ()
  | _ -> failwith "non-string title input should not match block/title"

let test_exact_title_simple_pull_uses_entity_lookup_for_small_results () =
  let count = 180_000 in
  let target = 42_000 in
  let datoms =
    List.concat
      (List.init count (fun index ->
         let entity = 1_000 + index in
         let title =
           if entity = target then
             "Plain Page"
           else
             Printf.sprintf "Page %d" index
         in
         [ datom ~e:entity ~a:"block/title" ~v:(String title) ()
         ; datom ~e:entity ~a:"block/uuid" ~v:(Uuid (Printf.sprintf "00000000-0000-0000-0000-%012d" entity)) ()
         ]))
  in
  let db = init_db ~schema:[ "block/title", one; "block/uuid", unique_identity ] datoms in
  match
    timed "exact title small pull" 0.006 (fun () ->
      q_return_string db "[:find (pull ?p [:block/uuid]) :where [?p :block/title \"Plain Page\"]]")
  with
  | Query_relation [ [ Result_pull entity ] ] ->
    if entity.pulled_id <> target then
      failf "expected target entity %d, got %d" target entity.pulled_id
  | Query_relation rows -> failf "expected one pulled row, got %d" (List.length rows)
  | _ -> failwith "expected relation result"

let test_block_content_rule_uses_title_scan_once () =
  let count = 180_000 in
  let db =
    init_db
      ~schema:[ "block/title", one ]
      (List.init count (fun index ->
         datom ~e:(1_000 + index) ~a:"block/title" ~v:(String (Printf.sprintf "Title %d" index)) ()))
  in
  let rules =
    Parser.parse_rules
      (read_edn
         "[[(block-content ?b ?query)
            [?b :block/title ?content]
            [(clojure.string/includes? ?content ?query)]]]")
  in
  let result =
    timed "block-content rule title includes" 0.025 (fun () ->
      q_return_string
        ~inputs:[ Arg_scalar (Result_value (String "__logseq_input__")); Arg_rules rules ]
        db
        "[:find ?b :where (block-content ?b ?query) :in $ ?query %]")
  in
  match result with
  | Query_relation [] -> ()
  | Query_relation rows -> failf "unexpected block-content matches: %d" (List.length rows)
  | _ -> failwith "expected relation result"

let test_source_ref_property_malformed_lookup_keeps_upstream_error_order () =
  let rules =
    [ { rule_name = "ref-property-value"
      ; rule_params = [ "b"; "prop-e"; "val" ]
      ; rule_body =
          [ Pattern (QVar "prop-e", QAttr "db/ident", QVar "prop")
          ; Pattern (QVar "b", QVar "prop", QVar "pv")
          ; Rule ("ref->val", [ QVar "pv"; QVar "val" ])
          ]
      }
    ; { rule_name = "ref-property"
      ; rule_params = [ "b"; "prop"; "val" ]
      ; rule_body =
          [ Pattern (QVar "prop-e", QAttr "db/ident", QVar "prop")
          ; Rule ("ref-property-value", [ QVar "b"; QVar "prop-e"; QVar "val" ])
          ]
      }
    ; { rule_name = "ref->val"
      ; rule_params = [ "pv"; "val" ]
      ; rule_body = [ Pattern (QVar "pv", QAttr "block/title", QVar "val") ]
      }
    ]
  in
  let db =
    init_db
      [ datom ~e:1 ~a:"db/ident" ~v:(Keyword "logseq.property.table/ordered-columns") ()
      ; datom
          ~e:3
          ~a:"logseq.property.table/ordered-columns"
          ~v:(Vector [ Keyword "block/title"; Keyword "logseq.property/status"; Keyword "block/tags" ])
          ()
      ]
  in
  let expected = "Lookup ref should contain 2 elements: [:block/title :logseq.property/status :block/tags]" in
  try
    ignore
      (timed "source ref-property malformed lookup" 0.250 (fun () ->
         q_sources_string
           ~inputs:[ Arg_rules rules ]
           db
           []
           "[:find ?p
             :in $ %
             :where (ref-property ?b ?p \"bar\")
                    [?b :block/title \"Page1\"]]"));
    failwith "source ref-property malformed lookup should raise"
  with
  | Invalid_argument message when message = expected -> ()
  | Invalid_argument message ->
    failf "source ref-property malformed lookup raised %S, expected %S" message expected

let test_rule_call_uses_relation_context_for_many_bindings () =
  let count = 120_000 in
  let rules =
    [ { rule_name = "flagged"
      ; rule_params = [ "e" ]
      ; rule_body =
          [ Pattern (QVar "e", QAttr "item/flag?", QValue (Bool true))
          ; Pattern (QVar "e", QAttr "item/live?", QValue (Bool true))
          ; Pattern (QVar "e", QAttr "item/visible?", QValue (Bool true))
          ; Pattern (QVar "e", QAttr "item/indexed?", QValue (Bool true))
          ]
      }
    ]
  in
  let db =
    init_db
      ~schema:
        [ "item/value", one
        ; "item/flag?", one
        ; "item/live?", one
        ; "item/visible?", one
        ; "item/indexed?", one
        ]
      (List.concat
         (List.init count (fun index ->
            let entity = 1_000 + index in
            [ datom ~e:entity ~a:"item/value" ~v:(Int index) () ]
            @
            if index mod 2 = 0 then
              [ datom ~e:entity ~a:"item/flag?" ~v:(Bool true) ()
              ; datom ~e:entity ~a:"item/live?" ~v:(Bool true) ()
              ; datom ~e:entity ~a:"item/visible?" ~v:(Bool true) ()
              ; datom ~e:entity ~a:"item/indexed?" ~v:(Bool true) ()
              ]
            else
              [])))
  in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules
    ; where =
        [ Pattern (QVar "e", QAttr "item/value", QVar "v")
        ; Rule ("flagged", [ QVar "e" ])
        ]
    }
  in
  let rows =
    timed "rule call relation context" 3.000 (fun () ->
      q_sources db [] query)
  in
  if List.length rows <> count / 2 then
    failf "rule call should return %d rows, got %d" (count / 2) (List.length rows)

let test_source_rule_call_uses_relation_context_for_many_bindings () =
  let count = 120_000 in
  let rules =
    [ { rule_name = "flagged"
      ; rule_params = [ "e" ]
      ; rule_body =
          [ Pattern (QVar "e", QAttr "item/flag?", QValue (Bool true))
          ; Pattern (QVar "e", QAttr "item/live?", QValue (Bool true))
          ; Pattern (QVar "e", QAttr "item/visible?", QValue (Bool true))
          ; Pattern (QVar "e", QAttr "item/indexed?", QValue (Bool true))
          ]
      }
    ]
  in
  let db =
    init_db
      ~schema:[ "item/value", one ]
      (List.init count (fun index ->
         datom ~e:(1_000 + index) ~a:"item/value" ~v:(Int index) ()))
  in
  let source_datoms =
    List.filter_map
      (fun index ->
         if index mod 2 = 0 then
           let entity = 1_000 + index in
           Some
             [ datom ~e:entity ~a:"item/flag?" ~v:(Bool true) ()
             ; datom ~e:entity ~a:"item/live?" ~v:(Bool true) ()
             ; datom ~e:entity ~a:"item/visible?" ~v:(Bool true) ()
             ; datom ~e:entity ~a:"item/indexed?" ~v:(Bool true) ()
             ]
         else
           None)
      (List.init count Fun.id)
    |> List.concat
  in
  let source_db =
    init_db
      ~schema:[ "item/flag?", one; "item/live?", one; "item/visible?", one; "item/indexed?", one ]
      source_datoms
  in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules
    ; where =
        [ Pattern (QVar "e", QAttr "item/value", QVar "v")
        ; SourceRule ("flags", "flagged", [ QVar "e" ])
        ]
    }
  in
  let rows =
    timed "source rule call relation context" 3.000 (fun () ->
      q_sources db [ "flags", Db_source source_db ] query)
  in
  if List.length rows <> count / 2 then
    failf "source rule call should return %d rows, got %d" (count / 2) (List.length rows)

let test_top_level_or_uses_relation_context_for_many_bindings () =
  let count = 120_000 in
  let datoms =
    List.concat
      (List.init count (fun index ->
         let entity = 1_000 + index in
         [ datom ~e:entity ~a:"item/value" ~v:(Int index) () ]
         @
         if index mod 2 = 0 then
           [ datom ~e:entity ~a:"item/a" ~v:(Bool true) () ]
         else
           [ datom ~e:entity ~a:"item/b" ~v:(Bool true) () ]))
  in
  let db = init_db ~schema:[ "item/value", one; "item/a", one; "item/b", one ] datoms in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "item/value", QVar "v")
        ; Or
            [ [ Pattern (QVar "e", QAttr "item/a", QValue (Bool true)) ]
            ; [ Pattern (QVar "e", QAttr "item/b", QValue (Bool true)) ]
            ]
        ]
    }
  in
  let rows =
    timed "top-level or relation context" 3.000 (fun () ->
      q_sources db [] query)
  in
  if List.length rows <> count then
    failf "top-level or should return %d rows, got %d" count (List.length rows)

let test_top_level_or_join_uses_relation_context_for_many_bindings () =
  let count = 120_000 in
  let datoms =
    List.concat
      (List.init count (fun index ->
         let entity = 1_000 + index in
         [ datom ~e:entity ~a:"item/value" ~v:(Int index) () ]
         @
         if index mod 2 = 0 then
           [ datom ~e:entity ~a:"item/a" ~v:(Bool true) () ]
         else
           [ datom ~e:entity ~a:"item/b" ~v:(Bool true) () ]))
  in
  let db = init_db ~schema:[ "item/value", one; "item/a", one; "item/b", one ] datoms in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "item/value", QVar "v")
        ; OrJoin
            ( [ "e" ]
            , [ [ Pattern (QVar "e", QAttr "item/a", QValue (Bool true)) ]
              ; [ Pattern (QVar "e", QAttr "item/b", QValue (Bool true)) ]
              ]
            )
        ]
    }
  in
  let rows =
    timed "top-level or-join relation context" 3.000 (fun () ->
      q_sources db [] query)
  in
  if List.length rows <> count then
    failf "top-level or-join should return %d rows, got %d" count (List.length rows)

let test_top_level_or_join_required_uses_relation_context_for_many_bindings () =
  let count = 120_000 in
  let datoms =
    List.concat
      (List.init count (fun index ->
         let entity = 1_000 + index in
         let group = index mod 16 in
         [ datom ~e:entity ~a:"item/value" ~v:(Int index) ()
         ; datom ~e:entity ~a:"item/group" ~v:(Int group) ()
         ]
         @
         if index mod 2 = 0 then
           [ datom ~e:entity ~a:"item/a" ~v:(Bool true) () ]
         else
           [ datom ~e:entity ~a:"item/b" ~v:(Bool true) () ]))
  in
  let db =
    init_db ~schema:[ "item/value", one; "item/group", one; "item/a", one; "item/b", one ] datoms
  in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "item/value", QVar "v")
        ; Pattern (QVar "e", QAttr "item/group", QVar "g")
        ; OrJoinRequired
            ( [ "g" ]
            , [ "e" ]
            , [ [ Pattern (QVar "e", QAttr "item/group", QVar "g")
                ; Pattern (QVar "e", QAttr "item/a", QValue (Bool true))
                ]
              ; [ Pattern (QVar "e", QAttr "item/group", QVar "g")
                ; Pattern (QVar "e", QAttr "item/b", QValue (Bool true))
                ]
              ]
            )
        ]
    }
  in
  let rows =
    timed "top-level or-join required relation context" 3.000 (fun () ->
      q_sources db [] query)
  in
  if List.length rows <> count then
    failf "top-level or-join required should return %d rows, got %d" count (List.length rows)

let test_source_or_join_required_uses_relation_context_for_many_bindings () =
  let count = 120_000 in
  let db =
    init_db
      ~schema:[ "item/value", one; "item/group", one ]
      (List.concat
         (List.init count (fun index ->
            let entity = 1_000 + index in
            let group = index mod 16 in
            [ datom ~e:entity ~a:"item/value" ~v:(Int index) ()
            ; datom ~e:entity ~a:"item/group" ~v:(Int group) ()
            ])))
  in
  let source_db =
    init_db
      ~schema:[ "item/group", one; "item/a", one; "item/b", one ]
      (List.concat
         (List.init count (fun index ->
            let entity = 1_000 + index in
            let group = index mod 16 in
            [ datom ~e:entity ~a:"item/group" ~v:(Int group) () ]
            @
            if index mod 2 = 0 then
              [ datom ~e:entity ~a:"item/a" ~v:(Bool true) () ]
            else
              [ datom ~e:entity ~a:"item/b" ~v:(Bool true) () ])))
  in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "item/value", QVar "v")
        ; Pattern (QVar "e", QAttr "item/group", QVar "g")
        ; SourceOrJoinRequired
            ( "flags"
            , [ "g" ]
            , [ "e" ]
            , [ [ Pattern (QVar "e", QAttr "item/group", QVar "g")
                ; Pattern (QVar "e", QAttr "item/a", QValue (Bool true))
                ]
              ; [ Pattern (QVar "e", QAttr "item/group", QVar "g")
                ; Pattern (QVar "e", QAttr "item/b", QValue (Bool true))
                ]
              ]
            )
        ]
    }
  in
  let rows =
    timed "source or-join required relation context" 3.000 (fun () ->
      q_sources db [ "flags", Db_source source_db ] query)
  in
  if List.length rows <> count then
    failf "source or-join required should return %d rows, got %d" count (List.length rows)

let test_top_level_not_join_uses_relation_context_for_many_bindings () =
  let count = 600_000 in
  let datoms =
    List.concat
      (List.init count (fun index ->
         let entity = 1_000 + index in
         [ datom ~e:entity ~a:"item/value" ~v:(Int index) () ]
         @
         if index mod 2 = 0 then
           [ datom ~e:entity ~a:"item/excluded?" ~v:(Bool true) () ]
         else
           []))
  in
  let db = init_db ~schema:[ "item/value", one; "item/excluded?", one ] datoms in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "item/value", QVar "v")
        ; NotJoin ([ "e" ], [ Pattern (QVar "e", QAttr "item/excluded?", QValue (Bool true)) ])
        ]
    }
  in
  let rows =
    timed "top-level not-join relation context" 3.000 (fun () ->
      q_sources db [] query)
  in
  if List.length rows <> count / 2 then
    failf "top-level not-join should return %d rows, got %d" (count / 2) (List.length rows)

let test_source_not_join_uses_relation_context_for_many_bindings () =
  let count = 600_000 in
  let db =
    init_db
      ~schema:[ "item/value", one ]
      (List.init count (fun index ->
         datom ~e:(1_000 + index) ~a:"item/value" ~v:(Int index) ()))
  in
  let excluded_db =
    init_db
      ~schema:[ "item/excluded?", one ]
      (List.filter_map
         (fun index ->
            if index mod 2 = 0 then
              Some (datom ~e:(1_000 + index) ~a:"item/excluded?" ~v:(Bool true) ())
            else
              None)
         (List.init count Fun.id))
  in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "item/value", QVar "v")
        ; SourceNotJoin
            ("excluded", [ "e" ], [ Pattern (QVar "e", QAttr "item/excluded?", QValue (Bool true)) ])
        ]
    }
  in
  let rows =
    timed "source not-join relation context" 3.000 (fun () ->
      q_sources db [ "excluded", Db_source excluded_db ] query)
  in
  if List.length rows <> count / 2 then
    failf "source not-join should return %d rows, got %d" (count / 2) (List.length rows)

let test_relation_source_join_uses_relation_context_for_many_bindings () =
  let count = 8_000 in
  let db =
    init_db
      ~schema:[ "item/value", one ]
      (List.init count (fun index ->
         datom ~e:(1_000 + index) ~a:"item/value" ~v:(Int index) ()))
  in
  let labels =
    List.init count (fun index ->
      [ Result_value (Int index)
      ; Result_value (String (Printf.sprintf "label-%d" index))
      ])
  in
  let query =
    { find = [ Find_var "e"; Find_var "label" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "item/value", QVar "value")
        ; SourceRelationPattern ("labels", [ QVar "value"; QVar "label" ])
        ]
    }
  in
  let rows =
    timed "relation source join relation context" 0.120 (fun () ->
      q_sources db [ "labels", Relation_source labels ] query)
  in
  if List.length rows <> count then
    failf "relation source join should return %d rows, got %d" count (List.length rows)

let test_input_bound_predicate_uses_relation_rows () =
  let count = 80_000 in
  let db =
    init_db
      ~schema:[ "score/value", one ]
      (List.init count (fun index ->
         datom ~e:(1_000 + index) ~a:"score/value" ~v:(Int index) ()))
  in
  let threshold = 0 in
  let rows =
    timed "input-bound predicate relation rows" 0.100 (fun () ->
      q_sources_string
        ~inputs:[ Arg_scalar (Result_value (Int threshold)) ]
        db
        []
        "[:find ?e
          :in $ ?threshold
          :where [?e :score/value ?score]
                 [(> ?score ?threshold)]]")
  in
  if List.length rows <> count - 1 then
    failf "input-bound predicate should return %d rows, got %d" (count - 1) (List.length rows)

let test_source_clause_predicate_uses_relation_rows () =
  let count = 300_000 in
  let db =
    init_db
      ~schema:[ "score/value", one ]
      (List.init count (fun index ->
         datom ~e:(1_000 + index) ~a:"score/value" ~v:(Int index) ()))
  in
  let source_db = empty_db () in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "score/value", QVar "score")
        ; SourceClause ("scores", ComparisonPredicate (GreaterThan, QVar "score", QValue (Int 0)))
        ]
    }
  in
  let rows =
    timed "source clause predicate relation rows" 1.000 (fun () ->
      q_sources db [ "scores", Db_source source_db ] query)
  in
  if List.length rows <> count - 1 then
    failf "source clause predicate should return %d rows, got %d" (count - 1) (List.length rows)

let test_source_clause_function_uses_relation_rows () =
  let count = 300_000 in
  let db =
    init_db
      ~schema:[ "score/value", one ]
      (List.init count (fun index ->
         datom ~e:(1_000 + index) ~a:"score/value" ~v:(Int index) ()))
  in
  let source_db = empty_db () in
  let query =
    { find = [ Find_var "e"; Find_var "next" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "score/value", QVar "score")
        ; SourceClause ("scores", ArithmeticValue (AddNumbers, [ QVar "score"; QValue (Int 1) ], "next"))
        ]
    }
  in
  let rows =
    timed "source clause function relation rows" 1.000 (fun () ->
      q_sources db [ "scores", Db_source source_db ] query)
  in
  if List.length rows <> count then
    failf "source clause function should return %d rows, got %d" count (List.length rows)

let test_input_bound_predicate_uses_index_range () =
  let count = 300_000 in
  let db =
    init_db
      ~schema:[ "score/value", { one with indexed = true } ]
      (List.init count (fun index ->
         datom ~e:(1_000 + index) ~a:"score/value" ~v:(Int index) ()))
  in
  let threshold = count - 11 in
  let rows =
    timed "input-bound predicate index range" 0.020 (fun () ->
      q_string
        ~inputs:[ Arg_scalar (Result_value (Int threshold)) ]
        db
        "[:find ?e
          :in $ ?threshold
          :where [?e :score/value ?score]
                 [(> ?score ?threshold)]]")
  in
  if List.length rows <> 10 then
    failf "input-bound predicate range should return 10 rows, got %d" (List.length rows)

let test_same_entity_indexed_chain_uses_sparse_candidate_scan () =
  let count = 300_000 in
  let schema =
    [ "person/name", { one with indexed = true }
    ; "person/last-name", { one with indexed = true }
    ; "person/age", { one with indexed = true }
    ; "person/sex", { one with indexed = true }
    ]
  in
  let datoms =
    List.concat
      (List.init count (fun index ->
         let entity = 1_000 + index in
         [ datom ~e:entity ~a:"person/name" ~v:(String (if index mod 8 = 0 then "Ivan" else "Petr")) ()
         ; datom ~e:entity ~a:"person/last-name" ~v:(String (Printf.sprintf "L%d" (index mod 97))) ()
         ; datom ~e:entity ~a:"person/age" ~v:(Int (index mod 100)) ()
         ; datom ~e:entity ~a:"person/sex" ~v:(Keyword (if index mod 2 = 0 then "male" else "female")) ()
         ]))
  in
  let db = init_db ~schema datoms in
  let q3_rows =
    timed "same-entity indexed q3 chain" 0.070 (fun () ->
      q_string
        db
        "[:find ?e ?age
          :where [?e :person/name \"Ivan\"]
                 [?e :person/age ?age]
                 [?e :person/sex :male]]")
  in
  if List.length q3_rows <> count / 8 then
    failf "same-entity q3 should return %d rows, got %d" (count / 8) (List.length q3_rows);
  let q4_rows =
    timed "same-entity indexed q4 chain" 0.120 (fun () ->
      q_string
        db
        "[:find ?e ?last-name ?age
          :where [?e :person/name \"Ivan\"]
                 [?e :person/last-name ?last-name]
                 [?e :person/age ?age]
                 [?e :person/sex :male]]")
  in
  if List.length q4_rows <> count / 8 then
    failf "same-entity q4 should return %d rows, got %d" (count / 8) (List.length q4_rows)

let () =
  test_wildcard_pull_uses_upstream_value_order_for_scalar_ties ();
  test_comment_area_parent_join_uses_indexed_shape ();
  test_comment_parent_reverse_join_uses_ref_value_lookup ();
  test_source_comment_parent_join_uses_relation_ref_lookup ();
  test_source_namespace_value_join_uses_relation_functions ();
  test_wildcard_pull_single_attr_pattern_uses_bounded_entity_scan ();
  test_wildcard_pull_page_missing_query_uses_bounded_entity_scan ();
  test_attr_prefix_query_ignores_unrelated_duplicate_datoms ();
  test_tag_value_with_present_attr_uses_indexed_intersection ();
  test_tag_value_without_attr_uses_indexed_difference ();
  test_simple_not_uses_relation_antijoin ();
  test_source_not_uses_relation_antijoin ();
  test_tag_value_ident_without_attr_allows_reversed_clause_order ();
  test_page_ref_pairs_with_tagged_non_journal_pages_use_indexed_join ();
  test_source_page_ref_pairs_with_tagged_non_journal_pages_use_relation_evaluator ();
  test_source_incoming_ref_without_attr_uses_relation_antijoin ();
  test_missing_property_ident_rule_call_returns_empty_without_rule_scan ();
  test_source_missing_property_ident_rule_call_uses_rule_prefix_context ();
  test_has_property_with_missing_title_returns_empty_without_rule_scan ();
  test_source_has_property_with_missing_title_uses_rule_prefix_context ();
  test_has_property_with_bound_title_uses_rule_suffix_context ();
  test_ref_property_with_bound_title_uses_rule_suffix_context ();
  test_source_task_page_ref_literal_string_uses_rule_prefix_context ();
  test_scalar_title_query_rejects_non_string_input_without_title_scan ();
  test_exact_title_simple_pull_uses_entity_lookup_for_small_results ();
  test_block_content_rule_uses_title_scan_once ();
  test_source_ref_property_malformed_lookup_keeps_upstream_error_order ();
  test_rule_call_uses_relation_context_for_many_bindings ();
  test_source_rule_call_uses_relation_context_for_many_bindings ();
  test_top_level_or_uses_relation_context_for_many_bindings ();
  test_top_level_or_join_uses_relation_context_for_many_bindings ();
  test_top_level_or_join_required_uses_relation_context_for_many_bindings ();
  test_source_or_join_required_uses_relation_context_for_many_bindings ();
  test_top_level_not_join_uses_relation_context_for_many_bindings ();
  test_source_not_join_uses_relation_context_for_many_bindings ();
  test_relation_source_join_uses_relation_context_for_many_bindings ();
  test_input_bound_predicate_uses_relation_rows ();
  test_source_clause_predicate_uses_relation_rows ();
  test_source_clause_function_uses_relation_rows ();
  test_input_bound_predicate_uses_index_range ();
  test_same_entity_indexed_chain_uses_sparse_candidate_scan ()
