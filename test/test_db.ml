open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_int label expected actual =
  if expected <> actual then
    failf "%s: expected %d, got %d" label expected actual

let assert_equal_string label expected actual =
  if expected <> actual then
    failf "%s: expected %s, got %s" label expected actual

let rec debug_value = function
  | Nil -> "nil"
  | Int value -> string_of_int value
  | Float value -> string_of_float value
  | String value -> Printf.sprintf "%S" value
  | Symbol value -> value
  | Bool value -> string_of_bool value
  | Keyword value -> ":" ^ value
  | Uuid value -> "#uuid " ^ value
  | Instant value -> "#inst " ^ string_of_int value
  | Regex value -> "#\"" ^ value ^ "\""
  | Ref value -> "Ref " ^ string_of_int value
  | List values -> "[" ^ (values |> List.map debug_value |> String.concat " ") ^ "]"
  | Map entries ->
    "{"
    ^ (entries
       |> List.map (fun (key, value) -> debug_value key ^ " " ^ debug_value value)
       |> String.concat ", ")
    ^ "}"
  | Set values -> "#{" ^ (values |> List.map debug_value |> String.concat " ") ^ "}"
  | Tuple values ->
    "("
    ^ (values
       |> List.map (function Some value -> debug_value value | None -> "_")
       |> String.concat ", ")
    ^ ")"
  | TxRef -> "#datascript/tx"
  | Ref_to _ -> "Ref_to"

let assert_equal_triples label expected actual =
  let triples = List.map (fun d -> d.e, d.a, d.v) actual in
  if expected <> triples then
    let format triples =
      triples
      |> List.map (fun (e, a, v) -> Printf.sprintf "(%d, %s, %s)" e a (debug_value v))
      |> String.concat "; "
    in
    failf "%s: expected [%s], got [%s]" label (format expected) (format triples)

let test_db__test_uuid () =
  let first = squuid ~msec:1_710_000_123_456 () in
  let second = squuid ~msec:1_710_000_123_456 () in
  if first = second then failwith "squuid should include random bits";
  let first_uuid =
    match first with
    | Uuid uuid -> uuid
    | _ -> failwith "squuid should return a Uuid value"
  in
  assert_equal_int
    "squuid_time_millis returns the embedded second"
    1_710_000_123_000
    (squuid_time_millis first);
  assert_equal_string
    "squuid uses the timestamp as its first UUID segment"
    "65ec87fb"
    (String.sub first_uuid 0 8);
  assert_equal_int "squuid has UUID string length" 36 (String.length first_uuid);
  if first_uuid.[8] <> '-' || first_uuid.[13] <> '-' || first_uuid.[18] <> '-' || first_uuid.[23] <> '-' then
    failwith "squuid should use canonical UUID separators"

let test_db__test_diff () =
  let left =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "a", One_value (Int 1); "b", One_value (Int 2); "c", One_value (Int 4) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "a", One_value (Int 1) ] }
         ]
  in
  let right =
    empty_db ()
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "b", One_value (Int 3); "d", One_value (Int 5) ] } ]
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "a", One_value (Int 1) ] } ]
  in
  let only_left, only_right, both = diff left right in
  assert_equal_triples
    "db diff returns datoms only on the left"
    [ 1, "b", Int 2; 1, "c", Int 4; 2, "a", Int 1 ]
    only_left;
  assert_equal_triples
    "db diff returns datoms only on the right"
    [ 1, "b", Int 3; 1, "d", Int 5 ]
    only_right;
  assert_equal_triples
    "db diff returns datoms present in both dbs"
    [ 1, "a", Int 1 ]
    both;
  let typed_left =
    empty_db () |> db_with [ Add (Entity_id 1, "attr", Keyword "aa") ]
  in
  let typed_right =
    empty_db () |> db_with [ Add (Entity_id 1, "attr", String "aa") ]
  in
  let only_left, only_right, both = diff typed_left typed_right in
  assert_equal_triples
    "db diff keeps keyword values distinct from string values"
    [ 1, "attr", Keyword "aa" ]
    only_left;
  assert_equal_triples
    "db diff keeps string values distinct from keyword values"
    [ 1, "attr", String "aa" ]
    only_right;
  assert_equal_triples
    "db diff has no common datoms for same attr with different value types"
    []
    both

let () =
  test_db__test_uuid ();
  test_db__test_diff ()
