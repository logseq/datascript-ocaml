open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms db index ?e ?a ?v ?tx () =
  datoms_seq db index ?e ?a ?v ?tx () |> List.of_seq

let assert_equal_value message expected actual =
  if not (Db.value_equal expected actual) then failf "%s" message

let assert_equal_triples message expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v) actual in
  if actual <> expected then failf "%s" message

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

let test_util__value_semantics () =
  let nested =
    Map
      [ Keyword "b", Vector [ String "x"; String "y" ]
      ; Keyword "a", Set [ Int 2; Int 1; Int 1 ]
      ]
  in
  let normalized =
    Map
      [ Keyword "a", Set [ Int 1; Int 2 ]
      ; Keyword "b", Vector [ String "x"; String "y" ]
      ]
  in
  assert_equal_value
    "Util.normalize_value normalizes unordered values without losing vector shape"
    normalized
    (Util.normalize_value nested);
  if Util.compare_value (Vector [ Int 1; Int 2 ]) (List [ Int 1; Int 2 ]) = 0 then
    failf "vectors and lists must remain distinct values"

let test_util__keyword_order_matches_upstream () =
  let normal = Map [ Keyword "id", String "robot_face"; Keyword "type", Keyword "emoji" ] in
  let inverted = Map [ Keyword "id", String "robot_face"; Keyword "emoji", Keyword "type" ] in
  let tabler =
    Map
      [ Keyword "color", String "inherit"
      ; Keyword "id", String "ListNumbers"
      ; Keyword "name", String "ListNumbers"
      ; Keyword "type", Keyword "tabler-icon"
      ]
  in
  let inverted_tabler =
    Map
      [ Keyword "color", String "inherit"
      ; Keyword "id", String "ListNumbers"
      ; Keyword "name", String "ListNumbers"
      ; Keyword "tabler-icon", Keyword "type"
      ]
  in
  let filters = Map [ Keyword "or?", Bool false; Keyword "filters", Vector [] ] in
  let status_filters = Map [ Keyword "or?", Bool false; Keyword "logseq.property/status", Vector [] ] in
  let filter_uuid = Uuid "00000002-1827-5820-8200-000000000000" in
  let nested_filters =
    Map
      [ Keyword "or?", Bool false
      ; Keyword "filters"
        , Vector
            [ Vector
                [ Keyword "logseq.property/status"
                ; Keyword "block/created-at"
                ; Vector [ String "~:is-not"; Vector [ filter_uuid ] ]
                ]
            ]
      ]
  in
  let nested_status_filters =
    Map
      [ Keyword "or?", Bool false
      ; Keyword "logseq.property/status"
        , Vector
            [ Vector
                [ Keyword "is-not"
                ; Keyword "filters"
                ; Vector [ String "^9"; Vector [ filter_uuid ] ]
                ]
            ]
      ]
  in
  if Util.compare_value normal inverted >= 0 then
    failf "map value ordering should match upstream DataScript value-compare";
  if Util.compare_value (Util.normalize_value normal) (Util.normalize_value inverted) >= 0 then
    failf "normalized map value ordering should match upstream DataScript value-compare";
  if Util.compare_value (Util.normalize_value tabler) (Util.normalize_value inverted_tabler) >= 0 then
    failf "normalized tabler map value ordering should match upstream DataScript value-compare";
  if Util.compare_value (Util.normalize_value filters) (Util.normalize_value status_filters) >= 0 then
    failf "normalized filter map value ordering should match upstream DataScript value-compare";
  if Util.compare_value (Util.normalize_value nested_filters) (Util.normalize_value nested_status_filters) >= 0 then
    failf "normalized nested filter map value ordering should match upstream DataScript value-compare"

let test_util__vector_values_in_db () =
  let vector = Vector [ Int 1; Map [ Keyword "tags", Vector [ Keyword "a"; Keyword "b" ] ] ] in
  let db =
    empty_db ~schema:[ "shape", indexed ] ()
    |> db_with [ Add (Entity_id 1, "shape", vector) ]
  in
  assert_equal_triples
    "vector values can be stored and looked up exactly"
    [ 1, "shape", vector ]
    (datoms db Avet ~a:"shape" ~v:vector ());
  assert_equal_triples
    "list values do not match vector values with the same members"
    []
    (datoms db Avet ~a:"shape" ~v:(List [ Int 1; Map [ Keyword "tags", Vector [ Keyword "a"; Keyword "b" ] ] ]) ())

let () =
  test_util__value_semantics ();
  test_util__keyword_order_matches_upstream ();
  test_util__vector_values_in_db ()
