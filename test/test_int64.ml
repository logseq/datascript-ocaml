open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let expect label expected actual =
  if Int64.compare expected actual <> 0 then
    failf "%s: expected %s, got %s" label (Int64.to_string expected) (Int64.to_string actual)

let expect_true label cond = if not cond then failf "%s" label

let expect_value label expected actual = if not (Util.value_equal expected actual) then failf "%s" label

let number_attr = { cardinality = One; unique = None; indexed = false; is_component = false; no_history = false; doc = None; value_type = Some NumberType; tuple_attrs = None; tuple_types = None }

let instant_attr = { cardinality = One; unique = None; indexed = false; is_component = false; no_history = false; doc = None; value_type = Some InstantType; tuple_attrs = None; tuple_types = None }

let epoch_ms = 1_700_000_000_000L

let test_int64_values_roundtrip () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "epoch-ms", Int64 epoch_ms)
         ; Add (Entity_id 1, "int64-min", Int64 Int64.min_int)
         ; Add (Entity_id 1, "int64-max", Int64 Int64.max_int)
         ; Add (Entity_id 1, "negative", Int64 (-4_000_000_000_000L))
         ]
  in
  let values =
    datoms db Eavt ()
    |> List.of_seq
    |> List.map (fun d -> d.a, d.v)
  in
  (match List.assoc "epoch-ms" values with
   | Int64 value -> expect "epoch-ms stays Int64 without 32-bit overflow" epoch_ms value
   | Instant _ -> failwith "epoch-ms must not become Instant"
   | _ -> failwith "epoch-ms decoded to unexpected constructor");
  (match List.assoc "int64-min" values with
   | Int64 value -> expect "int64 min boundary" Int64.min_int value
   | _ -> failwith "int64 min decoded to unexpected constructor");
  (match List.assoc "int64-max" values with
   | Int64 value -> expect "int64 max boundary" Int64.max_int value
   | _ -> failwith "int64 max decoded to unexpected constructor");
  (match List.assoc "negative" values with
   | Int64 value -> expect "negative int64" (-4_000_000_000_000L) value
   | _ -> failwith "negative int64 decoded to unexpected constructor")

let test_int64_equality_and_ordering () =
  expect_true "Int64 1 is not equal to Float 1.0" (not (Util.value_equal (Int64 1L) (Float 1.0)));
  expect_true "Int64 1 orders with Float 1.0" (Util.compare_value (Int64 1L) (Float 1.0) = 0);
  expect_true "Int64 5 is comparable to Instant 5" (Util.compare_value (Int64 5L) (Instant 5L) = 0);
  expect_true "Int64 5 is never equal to Instant 5" (not (Util.value_equal (Int64 5L) (Instant 5L)));
  expect_true "Instant 4 sorts below Int64 5" (Util.compare_value (Instant 4L) (Int64 5L) < 0);
  expect_true "Int64 5 sorts below Instant 6" (Util.compare_value (Int64 5L) (Instant 6L) < 0);
  expect_true "Int64 ordering uses full 64-bit range" (Util.compare_value (Int64 Int64.max_int) (Int64 1L) > 0);
  expect_true "Int64 min sorts below max" (Util.compare_value (Int64 Int64.min_int) (Int64 Int64.max_int) < 0)

let test_int64_predicates () =
  expect_true "number? accepts Int64" (Built_ins.matches_value_predicate NumberValue (Int64 1L));
  expect_true "integer? accepts Int64" (Built_ins.matches_value_predicate IntegerValue (Int64 1L));
  expect_true "number? rejects Instant" (not (Built_ins.matches_value_predicate NumberValue (Instant 1L)));
  expect_true "integer? rejects Instant" (not (Built_ins.matches_value_predicate IntegerValue (Instant 1L)));
  expect_true "integer? rejects Float" (not (Built_ins.matches_value_predicate IntegerValue (Float 1.0)));
  expect_true "zero? on Int64" (Built_ins.matches_numeric_predicate ZeroNumber (Int64 0L));
  expect_true "even? on Int64" (Built_ins.matches_numeric_predicate EvenInteger (Int64 4L));
  expect_true "odd? on Int64" (Built_ins.matches_numeric_predicate OddInteger (Int64 3L))

let test_int64_edn () =
  (match read_edn "1700000000000" with
   | QueryFormInt value -> expect "EDN int literal parses to int64" 1_700_000_000_000L value
   | _ -> failwith "EDN int literal did not parse as int64");
  (match read_edn "9223372036854775807" with
   | QueryFormInt value -> expect "EDN int64 max literal" Int64.max_int value
   | _ -> failwith "EDN int64 max literal did not parse as int64");
  let db =
    empty_db ~schema:[ "created-at", instant_attr ] ()
    |> db_with_string "[{:db/id 1 :created-at #inst \"2024-03-09T16:02:03.456Z\"}]"
  in
  (match List.of_seq (datoms db Eavt ~a:"created-at" ()) with
   | [ datom ] -> expect_value "#inst transacts as Instant" (Instant 1_710_000_123_456L) datom.v
   | _ -> failwith "#inst datom missing");
  expect_true "Instant prints as #inst readably"
    (Built_ins.print_query_value ~readably:true (Instant 1_710_000_123_456L)
     = "#inst \"2024-03-09T16:02:03.456Z\"")

let test_int64_query () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "ms", Int64 1_000_000_000_000L)
         ; Add (Entity_id 2, "ms", Int64 1_700_000_000_000L)
         ; Add (Entity_id 3, "ms", Int64 9_223_372_036_854_775_000L)
         ]
  in
  (match q_string db "[:find ?v :where [?e :ms ?v] [(> ?v 1600000000000)]]" with
   | rows ->
     let values =
       List.filter_map
         (function [ Result_value (Int64 v) ] -> Some v | _ -> None)
         rows
       |> List.sort Int64.compare
     in
     if values <> [ 1_700_000_000_000L; 9_223_372_036_854_775_000L ] then
       failwith "range filter dropped int64 rows");
  (match q_string db "[:find ?v . :where [?e :ms ?v] [(> ?v 1699999999999)] [(< ?v 9223372036854775000)]]" with
   | [ [ Result_value (Int64 value) ] ] -> expect "compound int64 filter" 1_700_000_000_000L value
   | _ -> failwith "compound int64 filter returned unexpected result");
  (match q_string db "[:find (max ?v) . :where [_ :ms ?v]]" with
   | [ [ Result_value (Int64 value) ] ] -> expect "max aggregate on int64" 9_223_372_036_854_775_000L value
   | _ -> failwith "max aggregate on int64 returned unexpected result");
  (match q_string db "[:find ?v :where [?e :ms ?v] [(number? ?v)]]" with
   | rows when List.length rows = 3 -> ()
   | rows -> failf "number? query on int64 returned unexpected result (%d rows)" (List.length rows))

let test_int64_transit_codec () =
  let module T = Transit_native.Transit.Json in
  (match Datascript_sqlite_codec.value_to_transit (Int64 epoch_ms) with
   | T.Int64 value -> expect "Int64 encodes to Transit.Int64" epoch_ms value
   | _ -> failwith "Int64 must encode to Transit.Int64 unconditionally");
  (match Datascript_sqlite_codec.value_to_transit (Instant epoch_ms) with
   | T.Date value -> expect "Instant encodes to Transit.Date" epoch_ms value
   | _ -> failwith "Instant must encode to Transit.Date");
  expect_value "Transit.Int64 decodes to Int64" (Int64 epoch_ms) (Datascript_sqlite_codec.value_of_transit (T.Int64 epoch_ms));
  expect_value "legacy Transit.Int decodes to Int64" (Int64 42L) (Datascript_sqlite_codec.value_of_transit (T.Int 42));
  expect_value
    "legacy Transit.Big_int decodes to Int64"
    (Int64 epoch_ms)
    (Datascript_sqlite_codec.value_of_transit (T.Big_int "1700000000000"));
  expect_value "Transit.Date decodes to Instant" (Instant epoch_ms) (Datascript_sqlite_codec.value_of_transit (T.Date epoch_ms));
  expect_value
    "legacy ~m int64 tag decodes to Instant"
    (Instant epoch_ms)
    (Datascript_sqlite_codec.value_of_transit (T.Tagged ("m", T.Int64 epoch_ms)));
  expect_value
    "legacy ~m int tag decodes to Instant"
    (Instant 42L)
    (Datascript_sqlite_codec.value_of_transit (T.Tagged ("m", T.Int 42)))

let test_int64_storage_migration () =
  (* legacy builds stored plain integers as Instant; on restore only
     db.type/instant attrs may keep Instant *)
  let storage = memory_storage () in
  let schema = [ "count", number_attr; "created-at", instant_attr ] in
  let db =
    init_db ~schema ~storage
      [ Db.datom ~e:1 ~a:"count" ~v:(Instant epoch_ms) ()
      ; Db.datom ~e:1 ~a:"created-at" ~v:(Instant epoch_ms) ()
      ; Db.datom ~e:1 ~a:"untyped-time" ~v:(Instant epoch_ms) ()
      ; Db.datom ~e:2 ~a:"count" ~v:(Int64 9_223_372_036_854_775_000L) ()
      ]
  in
  store ~storage db;
  (match restore storage with
   | None -> failwith "restore failed"
   | Some restored ->
     let value_for e a =
       match List.of_seq (datoms restored Eavt ~e ~a ()) with
       | [ d ] -> d.v
       | _ -> failwith ("missing datom for " ^ a)
     in
     expect_value "legacy Instant under numeric attr migrates to Int64" (Int64 epoch_ms) (value_for 1 "count");
     expect_value "Instant under instant attr stays Instant" (Instant epoch_ms) (value_for 1 "created-at");
     expect_value "Instant under untyped attr migrates to Int64" (Int64 epoch_ms) (value_for 1 "untyped-time");
     expect_value "big Int64 survives kvs restore" (Int64 9_223_372_036_854_775_000L) (value_for 2 "count"))

let test_int64_lookup_ref_and_entity () =
  let db =
    empty_db ~schema:[ "email", { cardinality = One; unique = Some Identity; indexed = true; is_component = false; no_history = false; doc = None; value_type = None; tuple_attrs = None; tuple_types = None } ] ()
    |> db_with
         [ Add (Entity_id 1, "email", String "ivan@x")
         ; Add (Entity_id 1, "ms", Int64 epoch_ms)
         ]
  in
  (match q_string db "[:find ?v . :where [[:email \"ivan@x\"] :ms ?v]]" with
   | [ [ Result_value (Int64 value) ] ] -> expect "lookup-ref resolves int64 attr" epoch_ms value
   | _ -> failwith "lookup-ref query returned unexpected result");
  (match entity db (Entity_id 1) with
   | Some e ->
     (match entity_attr e "db/id" with
      | Some (One_value (Int64 id)) -> expect "db/id renders as Int64" 1L id
      | _ -> failwith "db/id was not Int64")
   | None -> failwith "entity lookup failed")

let () =
  test_int64_values_roundtrip ();
  test_int64_equality_and_ordering ();
  test_int64_predicates ();
  test_int64_edn ();
  test_int64_query ();
  test_int64_transit_codec ();
  test_int64_storage_migration ();
  test_int64_lookup_ref_and_entity ();
  print_endline "test_int64 ok"
