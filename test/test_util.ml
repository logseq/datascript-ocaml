open Datascript
module V = Internal.Datascript_types

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms ?e ?a ?v ?tx db index =
  datoms_seq ?e ?a ?v ?tx db index |> List.of_seq

let assert_equal_triples message expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v) actual in
  if actual <> expected then failf "%s" message

let indexed =
  Schema.spec ~cardinality:(One) ?unique:(None) ~indexed:(true) ~is_component:(false) ~no_history:(false) ?doc:(None) ?value_type:(None) ?tuple:(match (None, None) with Some a, _ -> Some (Tuple_attrs a) | None, Some t -> Some (Tuple_types t) | None, None -> None) ()

let test_util__value_semantics () =
  let nested =
    V.Map
      [ V.Keyword "b", V.Vector [ V.String "x"; V.String "y" ]
      ; V.Keyword "a", V.Set [ V.Int64 2L; V.Int64 1L; V.Int64 1L ]
      ]
  in
  let normalized =
    V.Map
      [ V.Keyword "a", V.Set [ V.Int64 1L; V.Int64 2L ]
      ; V.Keyword "b", V.Vector [ V.String "x"; V.String "y" ]
      ]
  in
  if normalized <> Internal.Util.normalize_value nested then
    failf "Internal.Util.normalize_value normalizes unordered values without losing vector shape";
  if Internal.Util.compare_value (V.Vector [ V.Int64 1L; V.Int64 2L ]) (V.List [ V.Int64 1L; V.Int64 2L ]) = 0 then
    failf "vectors and lists must remain distinct values"

let test_util__keyword_order_matches_upstream () =
  let normal = V.Map [ V.Keyword "id", V.String "robot_face"; V.Keyword "type", V.Keyword "emoji" ] in
  let inverted = V.Map [ V.Keyword "id", V.String "robot_face"; V.Keyword "emoji", V.Keyword "type" ] in
  let tabler =
    V.Map
      [ V.Keyword "color", V.String "inherit"
      ; V.Keyword "id", V.String "ListNumbers"
      ; V.Keyword "name", V.String "ListNumbers"
      ; V.Keyword "type", V.Keyword "tabler-icon"
      ]
  in
  let inverted_tabler =
    V.Map
      [ V.Keyword "color", V.String "inherit"
      ; V.Keyword "id", V.String "ListNumbers"
      ; V.Keyword "name", V.String "ListNumbers"
      ; V.Keyword "tabler-icon", V.Keyword "type"
      ]
  in
  let filters = V.Map [ V.Keyword "or?", V.Bool false; V.Keyword "filters", V.Vector [] ] in
  let status_filters = V.Map [ V.Keyword "or?", V.Bool false; V.Keyword "logseq.property/status", V.Vector [] ] in
  let filter_uuid = V.Uuid "00000002-1827-5820-8200-000000000000" in
  let nested_filters =
    V.Map
      [ V.Keyword "or?", V.Bool false
      ; V.Keyword "filters"
        , V.Vector
            [ V.Vector
                [ V.Keyword "logseq.property/status"
                ; V.Keyword "block/created-at"
                ; V.Vector [ V.String "~:is-not"; V.Vector [ filter_uuid ] ]
                ]
            ]
      ]
  in
  let nested_status_filters =
    V.Map
      [ V.Keyword "or?", V.Bool false
      ; V.Keyword "logseq.property/status"
        , V.Vector
            [ V.Vector
                [ V.Keyword "is-not"
                ; V.Keyword "filters"
                ; V.Vector [ V.String "^9"; V.Vector [ filter_uuid ] ]
                ]
            ]
      ]
  in
  if Internal.Util.compare_value normal inverted >= 0 then
    failf "map value ordering should match upstream DataScript value-compare";
  if Internal.Util.compare_value (Internal.Util.normalize_value normal) (Internal.Util.normalize_value inverted) >= 0 then
    failf "normalized map value ordering should match upstream DataScript value-compare";
  if Internal.Util.compare_value (Internal.Util.normalize_value tabler) (Internal.Util.normalize_value inverted_tabler) >= 0 then
    failf "normalized tabler map value ordering should match upstream DataScript value-compare";
  if Internal.Util.compare_value (Internal.Util.normalize_value filters) (Internal.Util.normalize_value status_filters) >= 0 then
    failf "normalized filter map value ordering should match upstream DataScript value-compare";
  if Internal.Util.compare_value (Internal.Util.normalize_value nested_filters) (Internal.Util.normalize_value nested_status_filters) >= 0 then
    failf "normalized nested filter map value ordering should match upstream DataScript value-compare"

let test_util__vector_values_in_db () =
  let vector = Vector [ Int64 1L; Map [ Keyword "tags", Vector [ Keyword "a"; Keyword "b" ] ] ] in
  let db =
    empty_db ~schema:[ "shape", indexed ] ()
    |> db_with [ Add (Entity_id (eid 1L), "shape", vector) ]
  in
  assert_equal_triples
    "vector values can be stored and looked up exactly"
    [ (eid 1L), "shape", vector ]
    (datoms ~a:"shape" ~v:vector db Avet);
  assert_equal_triples
    "list values do not match vector values with the same members"
    []
    (datoms ~a:"shape" ~v:(List [ Int64 1L; Map [ Keyword "tags", Vector [ Keyword "a"; Keyword "b" ] ] ]) db Avet)

let test_util__uuid_canonicalize () =
  let check expected input =
    if Internal.Util.uuid_canonicalize input <> expected then
      failf "uuid_canonicalize %S -> %S, want %S" input
        (Internal.Util.uuid_canonicalize input) expected
  in
  (* transit-js UUIDfromString("cli-sync-stress-user") — the case that
     produced ghost block/uuid duplicates *)
  check "0c00000c-000e-0000-0000-000000000000" "cli-sync-stress-user";
  (* identity on canonical uuid strings *)
  check "3b8e1234-5678-4a9b-8c1d-2e3f4a5b6c7d" "3b8e1234-5678-4a9b-8c1d-2e3f4a5b6c7d";
  (* uppercase hex parses the same as lowercase *)
  check "0c00000c-000e-0000-0000-000000000000" "CLI-SYNC-STRESS-USER";
  (* dashes are stripped before pairing, not positional *)
  check "0c00000c-000e-0000-0000-000000000000" "cli-syn-cstress-user";
  (* short input zero-pads the tail *)
  check "ab000000-0000-0000-0000-000000000000" "ab";
  (* extra leading chars shift the parse, matching substring windows *)
  check "aabbccdd-eeff-0011-2233-445566778899" "aabbccddeeff00112233445566778899aabb"

let () =
  test_util__value_semantics ();
  test_util__keyword_order_matches_upstream ();
  test_util__vector_values_in_db ();
  test_util__uuid_canonicalize ()
