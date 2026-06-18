open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms db index ?e ?a ?v ?tx () =
  datoms_seq db index ?e ?a ?v ?tx () |> List.of_seq

let assert_equal_triples label expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v) actual in
  if expected <> actual then failf "%s" label

let assert_equal_query_set label expected actual =
  let normalize rows = List.sort compare rows in
  if normalize expected <> normalize actual then failf "%s" label

let many =
  { cardinality = Many
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let test_issues__issue_262 () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "attr", One_value (String "A") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "attr", One_value (String "B") ] }
         ]
  in
  assert_equal_query_set
    "issue-262 vector captures each row binding without mutable aliasing"
    [ [ Result_value (String "A"); Result_value (Vector [ String "A" ]) ]
    ; [ Result_value (String "B"); Result_value (Vector [ String "B" ]) ]
    ]
    (q_string db "[:find ?a ?b :where [_ :attr ?a] [(vector ?a) ?b]]")

let test_issues__issue_331 () =
  let storage_value = memory_storage () in
  let schema_value = [ "aka", many ] in
  let db =
    empty_db ~schema:schema_value ~storage:storage_value ()
    |> db_with [ Add (Entity_id 1, "aka", String "Max") ]
  in
  let db = empty db in
  if schema db <> schema_value then failf "issue-331 empty db should preserve schema";
  if storage db = None then failf "issue-331 empty db should preserve storage";
  assert_equal_triples "issue-331 empty db should remove datoms" [] (datoms db Eavt ())

let test_issues__issue_330 () =
  let base =
    empty_db ~schema:[ "aka", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Maksim")
                 ; "age", One_value (Int 45)
                 ; "aka", One_value (Vector [ String "Max Otto von Stierlitz"; String "Jack Ryan" ])
                 ]
             }
         ]
  in
  let filtered = filter base (fun _ _ -> true) in
  if schema filtered <> schema base then failf "issue-330 filtered db should preserve schema";
  assert_equal_triples
    "issue-330 filtered db should expose the same datoms when predicate is always true"
    (datoms base Eavt () |> List.map (fun d -> d.e, d.a, d.v))
    (datoms filtered Eavt ())

let test_issues__issue_369 () =
  let left =
    empty_db () |> db_with [ Add (Entity_id 1, "attr", Keyword "aa") ]
  in
  let right =
    empty_db () |> db_with [ Add (Entity_id 1, "attr", String "aa") ]
  in
  let only_left, only_right, both = diff left right in
  assert_equal_triples
    "issue-369 diff keeps keyword values distinct from string values"
    [ 1, "attr", Keyword "aa" ]
    only_left;
  assert_equal_triples
    "issue-369 diff keeps string values distinct from keyword values"
    [ 1, "attr", String "aa" ]
    only_right;
  assert_equal_triples
    "issue-369 diff has no common datoms for same attr with different value types"
    []
    both

let test_issues__issue_381 () =
  let schema_value = [ "aka", many ] in
  let db = empty_db ~schema:schema_value () in
  if Datascript.schema db <> schema_value then failf "issue-381 schema should be exposed through public API"

let () =
  test_issues__issue_262 ();
  test_issues__issue_331 ();
  test_issues__issue_330 ();
  test_issues__issue_369 ();
  test_issues__issue_381 ()
