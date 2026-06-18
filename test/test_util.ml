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
  test_util__vector_values_in_db ()
