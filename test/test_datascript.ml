open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms db index ?e ?a ?v ?tx () =
  datoms_seq db index ?e ?a ?v ?tx () |> List.of_seq

let assert_bool message value =
  if not value then failwith message

let assert_equal_int label expected actual =
  if expected <> actual then
    failf "%s: expected %d, got %d" label expected actual

let assert_equal_value label expected actual =
  if expected <> actual then failf "%s: unexpected value" label

let assert_equal_datoms label expected actual =
  if expected <> actual then
    failf "%s: unexpected datoms" label

let kw name = Keyword name

let str_key name = String name

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
  | Vector values -> "#vector[" ^ (values |> List.map debug_value |> String.concat " ") ^ "]"
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

let rec debug_pulled_value = function
  | Pulled_scalar value -> debug_value value
  | Pulled_many values -> "[" ^ (values |> List.map debug_pulled_value |> String.concat " ") ^ "]"
  | Pulled_entity entity -> debug_pulled_entity entity

and debug_pulled_entity entity =
  "{"
  ^ (entity.pulled_attrs
     |> List.map (fun (attr, value) -> debug_value attr ^ " " ^ debug_pulled_value value)
     |> String.concat ", ")
  ^ "}"

let debug_pulled_attrs attrs =
  "{"
  ^ (attrs
     |> List.map (fun (attr, value) -> debug_value attr ^ " " ^ debug_pulled_value value)
     |> String.concat ", ")
  ^ "}"

let assert_equal_triples label expected actual =
  let triples = List.map (fun d -> d.e, d.a, d.v) actual in
  if expected <> triples then
    let format triples =
      triples
      |> List.map (fun (e, a, v) -> Printf.sprintf "(%d, %s, %s)" e a (debug_value v))
      |> String.concat "; "
    in
    failf "%s: expected [%s], got [%s]" label (format expected) (format triples)

let assert_equal_tx_value label expected actual =
  if expected <> actual then failf "%s: unexpected value" label

let assert_equal_pulled_attrs label expected entity =
  if expected <> entity.pulled_attrs then
    failf
      "%s: expected %s, got %s"
      label
      (debug_pulled_attrs expected)
      (debug_pulled_attrs entity.pulled_attrs)

let assert_equal_query label expected actual =
  if expected <> actual then failf "%s: unexpected query result" label

let assert_equal_query_set label expected actual =
  let normalize rows = List.sort_uniq compare rows in
  if normalize expected <> normalize actual then failf "%s: unexpected query result" label

let assert_equal_tempids label expected actual =
  if expected <> actual then
    let format tempids =
      tempids
      |> List.map (fun (tempid, entity_id) -> Printf.sprintf "%s=%d" tempid entity_id)
      |> String.concat "; "
    in
    failf "%s: expected [%s], got [%s]" label (format expected) (format actual)

let many =
  { cardinality = Many; unique = None; indexed = false; is_component = false; no_history = false; doc = None; value_type = None; tuple_attrs = None; tuple_types = None }

let indexed =
  { cardinality = One; unique = None; indexed = true; is_component = false; no_history = false; doc = None; value_type = None; tuple_attrs = None; tuple_types = None }

let unique_identity =
  { cardinality = One; unique = Some Identity; indexed = true; is_component = false; no_history = false; doc = None; value_type = None; tuple_attrs = None; tuple_types = None }

let unique_value =
  { cardinality = One; unique = Some Value; indexed = true; is_component = false; no_history = false; doc = None; value_type = None; tuple_attrs = None; tuple_types = None }

let ref_attr =
  { cardinality = One; unique = None; indexed = false; is_component = false; no_history = false; doc = None; value_type = Some RefType; tuple_attrs = None; tuple_types = None }

let ref_many =
  { cardinality = Many; unique = None; indexed = false; is_component = false; no_history = false; doc = None; value_type = Some RefType; tuple_attrs = None; tuple_types = None }

let component =
  { cardinality = One; unique = None; indexed = false; is_component = true; no_history = false; doc = None; value_type = Some RefType; tuple_attrs = None; tuple_types = None }

let component_many =
  { cardinality = Many; unique = None; indexed = false; is_component = true; no_history = false; doc = None; value_type = Some RefType; tuple_attrs = None; tuple_types = None }

let tuple attrs =
  { cardinality = One; unique = None; indexed = true; is_component = false; no_history = false; doc = None; value_type = Some TupleType; tuple_attrs = Some attrs; tuple_types = None }

let tuple_unindexed attrs =
  { cardinality = One; unique = None; indexed = false; is_component = false; no_history = false; doc = None; value_type = Some TupleType; tuple_attrs = Some attrs; tuple_types = None }

let tuple_unique_identity attrs =
  { cardinality = One; unique = Some Identity; indexed = true; is_component = false; no_history = false; doc = None; value_type = Some TupleType; tuple_attrs = Some attrs; tuple_types = None }

let typed_tuple types =
  { indexed with value_type = Some TupleType; tuple_types = Some types }

let assert_raises_invalid_arg label f =
  match f () with
  | exception Invalid_argument _ -> ()
  | exception exn -> failf "%s: expected Invalid_argument, got %s" label (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

let assert_raises_invalid_arg_message label expected f =
  match f () with
  | exception Invalid_argument message when message = expected -> ()
  | exception Invalid_argument message ->
    failf "%s: expected Invalid_argument(%S), got Invalid_argument(%S)" label expected message
  | exception exn -> failf "%s: expected Invalid_argument, got %s" label (Printexc.to_string exn)
  | _ -> failf "%s: expected Invalid_argument" label

let test_datom_defaults () =
  let d = datom ~e:1 ~a:"name" ~v:(String "Ivan") () in
  assert_equal_int "entity id" 1 d.e;
  assert_equal_int "default tx" tx0 d.tx;
  assert_bool "datoms are additions by default" d.added

let test_empty_db () =
  let db = empty_db () in
  assert_equal_datoms "empty eavt index" [] (datoms db Eavt ());
  assert_equal_datoms "empty aevt index" [] (datoms db Aevt ());
  assert_equal_datoms "empty avet index" [] (datoms db Avet ())

let test_init_db_and_indexes () =
  let ivan = datom ~e:1 ~a:"name" ~v:(String "Ivan") () in
  let likes_pizza = datom ~e:1 ~a:"likes" ~v:(String "pizza") () in
  let petr = datom ~e:2 ~a:"name" ~v:(String "Petr") () in
  let db = init_db ~schema:[ "likes", indexed; "name", indexed ] [ petr; likes_pizza; ivan ] in
  assert_equal_datoms
    "eavt sorts by entity, attribute, value, tx"
    [ likes_pizza; ivan; petr ]
    (datoms db Eavt ());
  assert_equal_datoms
    "aevt sorts by attribute, entity, value, tx"
    [ likes_pizza; ivan; petr ]
    (datoms db Aevt ());
  assert_equal_datoms
    "avet sorts by attribute, value, entity, tx"
    [ likes_pizza; ivan; petr ]
    (datoms db Avet ());
  assert_equal_datoms
    "eavt can filter by entity and attribute"
    [ likes_pizza ]
    (datoms db Eavt ~e:1 ~a:"likes" ());
  assert_equal_datoms
    "avet can filter by attribute and value"
    [ ivan ]
    (datoms db Avet ~a:"name" ~v:(String "Ivan") ())

let test_init_db_counts_ref_values_in_max_eid () =
  let db =
    init_db
      ~schema:[ "friend", ref_attr ]
      [ datom ~e:1 ~a:"friend" ~v:(Ref 5) () ]
  in
  let report = transact db [ Add (Temp_id "next", "name", String "Next") ] in
  assert_equal_int
    "init_db should allocate after entity ids referenced by datom values"
    6
    (Option.get (resolve_tempid report.tempids "next"))

let test_init_db_resolves_raw_ref_datoms_from_schema () =
  let db =
    init_db
      ~schema:[ "friend", ref_attr ]
      [ datom ~e:1 ~a:"name" ~v:(String "Ivan") ()
      ; datom ~e:1 ~a:"friend" ~v:(Int 2) ()
      ; datom ~e:2 ~a:"name" ~v:(String "Petr") ()
      ]
  in
  assert_equal_datoms
    "init_db should normalize raw numeric ref datoms by schema"
    [ datom ~e:1 ~a:"friend" ~v:(Ref 2) () ]
    (datoms db Eavt ~e:1 ~a:"friend" ());
  (match pull db [ Pull_ref ("friend", [ Pull_attr "name" ]) ] (Entity_id 1) with
   | Some entity ->
     assert_equal_pulled_attrs
       "pull should expand init_db raw numeric refs"
       [ kw "friend", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ] } ]
       entity
   | None -> failwith "expected pull to find init_db entity with raw ref");
  assert_equal_query
    "q should match init_db raw numeric refs as entity refs"
    [ [ Result_value (String "Petr") ] ]
    (q_string db "[:find ?name :where [1 :friend ?friend] [?friend :name ?name]]")

let test_datoms_returns_lazy_sequence () =
  let db =
    init_db
      [ datom ~e:1 ~a:"name" ~v:(String "Ivan") ()
      ; datom ~e:2 ~a:"name" ~v:(String "Petr") ()
      ]
  in
  let checked = ref 0 in
  let filtered =
    filter db (fun _ datom ->
      incr checked;
      if !checked > 1 then
        failwith "datoms should not scan past the first visible item before the first sequence step";
      datom.e = 1)
  in
  match Seq.uncons (datoms_seq filtered Eavt ()) with
  | Some (datom, _) ->
    if datom.e <> 1 then failf "expected first lazy datom for entity 1, got %d" datom.e
  | None -> failwith "expected one visible datom"

let test_datoms_slices_before_filtered_predicate () =
  let db =
    init_db
      [ datom ~e:1 ~a:"age" ~v:(Int 30) ()
      ; datom ~e:2 ~a:"name" ~v:(String "Petr") ()
      ]
  in
  let filtered =
    filter db (fun _ datom ->
      if datom.a <> "name" then
        failwith "datoms should slice the requested attribute before applying filtered-db predicates";
      true)
  in
  match Seq.uncons (datoms_seq filtered Aevt ~a:"name" ()) with
  | Some (datom, _) ->
    if datom.a <> "name" then failf "expected first lazy datom for :name, got %s" datom.a
  | None -> failwith "expected one name datom"

let test_raw_datom_counts_ref_values_in_max_eid () =
  let report =
    transact
      (empty_db ~schema:[ "friend", ref_attr ] ())
         [ Raw_datom (datom ~e:1 ~a:"friend" ~v:(Ref 5) ())
         ; Add (Temp_id "next", "name", String "Next")
         ]
  in
  assert_equal_int
    "Raw_datom should allocate after entity ids referenced by datom values"
    6
    (Option.get (resolve_tempid report.tempids "next"));
  let db =
    empty_db ~schema:[ "friend", ref_attr ] ()
    |> db_with
         [ Raw_datom (datom ~e:1 ~a:"friend" ~v:(Int 2) ())
         ; Raw_datom (datom ~e:2 ~a:"name" ~v:(String "Petr") ())
         ]
  in
  assert_equal_datoms
    "Raw_datom should normalize raw numeric ref values by schema"
    [ datom ~e:1 ~a:"friend" ~v:(Ref 2) () ]
    (datoms db Eavt ~e:1 ~a:"friend" ());
  match pull db [ Pull_ref ("friend", [ Pull_attr "name" ]) ] (Entity_id 1) with
  | Some entity ->
    assert_equal_pulled_attrs
      "pull should expand Raw_datom raw numeric refs"
      [ kw "friend", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ] } ]
      entity
  | None -> failwith "expected pull to find Raw_datom entity with raw ref"

let test_raw_datom_counts_tx_in_max_tx () =
  let db =
    empty_db ()
    |> db_with [ Raw_datom (datom ~tx:(tx0 + 5) ~e:1 ~a:"name" ~v:(String "Ivan") ()) ]
  in
  let report = transact db [ Add (Entity_id 2, "name", String "Petr") ] in
  assert_equal_datoms
    "Raw_datom should advance max tx for later transactions"
    [ datom ~tx:(tx0 + 6) ~e:2 ~a:"name" ~v:(String "Petr") () ]
    report.tx_data

let test_transact__test_with_datoms () =
  let db =
    empty_db ()
    |> db_with
         [ Raw_datom (datom ~e:1 ~a:"name" ~v:(String "Oleg") ())
         ; Raw_datom (datom ~tx:(tx0 + 1) ~e:1 ~a:"age" ~v:(Int 17) ())
         ; Raw_datom (datom ~tx:(tx0 + 2) ~e:1 ~a:"aka" ~v:(String "x") ())
         ]
  in
  assert_equal_datoms
    "Raw_datom assertions keep their own transaction numbers"
    [ datom ~tx:(tx0 + 1) ~e:1 ~a:"age" ~v:(Int 17) ()
    ; datom ~tx:(tx0 + 2) ~e:1 ~a:"aka" ~v:(String "x") ()
    ; datom ~e:1 ~a:"name" ~v:(String "Oleg") ()
    ]
    (datoms db Eavt ());
  let db =
    empty_db ()
    |> db_with
         [ Raw_datom (datom ~e:1 ~a:"name" ~v:(String "Oleg") ())
         ; Raw_datom (datom ~e:1 ~a:"age" ~v:(Int 17) ())
         ; Raw_datom (datom ~added:false ~e:1 ~a:"name" ~v:(String "Oleg") ())
         ]
  in
  assert_equal_datoms
    "Raw_datom retractions remove matching active facts"
    [ datom ~e:1 ~a:"age" ~v:(Int 17) () ]
    (datoms db Eavt ())

let test_find_datom_returns_first_index_match () =
  let ivan = datom ~e:1 ~a:"name" ~v:(String "Ivan") () in
  let likes_pizza = datom ~e:1 ~a:"likes" ~v:(String "pizza") () in
  let petr = datom ~e:2 ~a:"name" ~v:(String "Petr") () in
  let db = init_db [ petr; likes_pizza; ivan ] in
  if find_datom db Eavt ~e:1 ~a:"name" () <> Some ivan then
    failwith "find_datom should return first exact index match";
  if find_datom db Eavt ~e:42 () <> None then
    failwith "find_datom should return None when no datom matches"

let test_vaet_index_returns_ref_datoms_by_value () =
  let db =
    empty_db ~schema:[ "friend", ref_many; "spouse", ref_attr; "name", indexed ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "Ivan") ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "friend", Many_values [ Ref 1 ]
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 3)
             ; attrs =
                 [ "name", One_value (String "Oleg")
                 ; "friend", Many_values [ Ref 1; Ref 2 ]
                 ; "spouse", One_value (Ref 2)
                 ]
             }
         ]
  in
  assert_equal_triples
    "VAET indexes only ref values in value-attribute-entity order"
    [ 2, "friend", Ref 1
    ; 3, "friend", Ref 1
    ; 3, "friend", Ref 2
    ; 3, "spouse", Ref 2
    ]
    (datoms db Vaet ());
  assert_equal_triples
    "VAET supports target ref filtering"
    [ 2, "friend", Ref 1; 3, "friend", Ref 1 ]
    (datoms db Vaet ~v:(Ref 1) ());
  assert_equal_triples
    "VAET seek starts from the requested ref tuple"
    [ 3, "friend", Ref 2; 3, "spouse", Ref 2 ]
    (seek_datoms db Vaet ~v:(Ref 2) ());
  assert_equal_triples
    "VAET reverse seek walks backward from the requested ref tuple"
    [ 3, "spouse", Ref 2; 3, "friend", Ref 2; 3, "friend", Ref 1; 2, "friend", Ref 1 ]
    (rseek_datoms db Vaet ~v:(Ref 2) ~a:"spouse" ())

let test_incremental_writes_keep_public_datoms_indexes_correct () =
  let person id name age friend =
    Entity
      { db_id = Some (Entity_id id)
      ; attrs =
          [ "name", One_value (String name)
          ; "age", One_value (Int age)
          ; "friend", One_value (Ref friend)
          ]
      }
  in
  let db =
    List.fold_left
      (fun db entity -> db_with [ entity ] db)
      (empty_db ~schema:[ "name", indexed; "age", indexed; "friend", ref_attr ] ())
      [ person 1 "Ivan" 30 2
      ; person 2 "Petr" 20 1
      ; person 3 "Ivan" 40 1
      ]
  in
  assert_equal_triples
    "incremental EAVT remains sorted by entity"
    [ 1, "age", Int 30
    ; 1, "friend", Ref 2
    ; 1, "name", String "Ivan"
    ; 2, "age", Int 20
    ; 2, "friend", Ref 1
    ; 2, "name", String "Petr"
    ; 3, "age", Int 40
    ; 3, "friend", Ref 1
    ; 3, "name", String "Ivan"
    ]
    (datoms db Eavt ());
  assert_equal_triples
    "incremental AEVT attr slice remains correct"
    [ 1, "name", String "Ivan"; 2, "name", String "Petr"; 3, "name", String "Ivan" ]
    (datoms db Aevt ~a:"name" ());
  assert_equal_triples
    "incremental AVET value slice remains correct"
    [ 1, "name", String "Ivan"; 3, "name", String "Ivan" ]
    (datoms db Avet ~a:"name" ~v:(String "Ivan") ());
  assert_equal_triples
    "incremental VAET ref slice remains correct"
    [ 2, "friend", Ref 1; 3, "friend", Ref 1 ]
    (datoms db Vaet ~v:(Ref 1) ())

let test_index_range_returns_avet_values_between_bounds () =
  let db =
    empty_db ~schema:[ "age", indexed; "name", indexed ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 15) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Oleg"); "age", One_value (Int 20) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Sergey"); "age", One_value (Int 7) ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "name", One_value (String "Pavel"); "age", One_value (Int 45) ] }
         ; Entity { db_id = Some (Entity_id 5); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 20) ] }
         ]
  in
  assert_equal_triples
    "index_range supports string lower and upper bounds"
    [ 5, "name", String "Petr" ]
    (index_range db "name" ~start:(String "Pe") ~stop:(String "S") ());
  assert_equal_triples
    "index_range includes matching string stop bounds"
    [ 2, "name", String "Oleg"; 4, "name", String "Pavel"; 5, "name", String "Petr"; 3, "name", String "Sergey" ]
    (index_range db "name" ~start:(String "O") ~stop:(String "Sergey") ());
  assert_equal_triples
    "index_range supports open string start bounds"
    [ 1, "name", String "Ivan"; 2, "name", String "Oleg" ]
    (index_range db "name" ~stop:(String "P") ());
  assert_equal_triples
    "index_range supports open string stop bounds"
    [ 3, "name", String "Sergey" ]
    (index_range db "name" ~start:(String "R") ());
  assert_equal_triples
    "index_range supports fully open string bounds"
    [ 1, "name", String "Ivan"
    ; 2, "name", String "Oleg"
    ; 4, "name", String "Pavel"
    ; 5, "name", String "Petr"
    ; 3, "name", String "Sergey"
    ]
    (index_range db "name" ());
  assert_equal_triples
    "index_range supports repeated numeric values"
    [ 1, "age", Int 15; 2, "age", Int 20; 5, "age", Int 20 ]
    (index_range db "age" ~start:(Int 15) ~stop:(Int 20) ());
  assert_equal_triples
    "index_range supports wide numeric bounds"
    [ 3, "age", Int 7; 1, "age", Int 15; 2, "age", Int 20; 5, "age", Int 20; 4, "age", Int 45 ]
    (index_range db "age" ~start:(Int 7) ~stop:(Int 45) ());
  assert_equal_triples
    "index_range supports numeric bounds outside the indexed values"
    [ 3, "age", Int 7; 1, "age", Int 15; 2, "age", Int 20; 5, "age", Int 20; 4, "age", Int 45 ]
    (index_range db "age" ~start:(Int 0) ~stop:(Int 100) ());
  assert_raises_invalid_arg
    "index_range rejects unindexed attrs"
    (fun () -> ignore (index_range db "alias" ~start:(String "A") ~stop:(String "Z") ()));
  assert_raises_invalid_arg_message
    "index_range rejects unindexed attrs with upstream message"
    "Attribute :alias should be marked as :db/index true"
    (fun () -> ignore (index_range db "alias" ~start:(String "A") ~stop:(String "Z") ()))

let test_indexes_compare_keywords_like_datascript () =
  let db =
    empty_db ~schema:[ "tag", indexed ] ()
    |> db_with
         [ Add (Entity_id 1, "tag", Keyword "a-/b")
         ; Add (Entity_id 2, "tag", Keyword "a/b")
         ; Add (Entity_id 3, "tag", Keyword "a/c")
         ]
  in
  assert_equal_triples
    "AVET sorts keywords by namespace then name"
    [ 2, "tag", Keyword "a/b"; 3, "tag", Keyword "a/c"; 1, "tag", Keyword "a-/b" ]
    (datoms db Avet ~a:"tag" ());
  assert_equal_triples
    "index_range uses DataScript keyword ordering"
    [ 2, "tag", Keyword "a/b"; 3, "tag", Keyword "a/c" ]
    (index_range db "tag" ~start:(Keyword "a/b") ~stop:(Keyword "a/c") ());
  assert_equal_triples
    "seek_datoms uses DataScript keyword lower bounds"
    [ 3, "tag", Keyword "a/c"; 1, "tag", Keyword "a-/b" ]
    (seek_datoms db Avet ~a:"tag" ~v:(Keyword "a/c") ());
  assert_equal_triples
    "rseek_datoms uses DataScript keyword upper bounds"
    [ 3, "tag", Keyword "a/c"; 2, "tag", Keyword "a/b" ]
    (rseek_datoms db Avet ~a:"tag" ~v:(Keyword "a/c") ())

let test_indexes_compare_numbers_across_value_constructors () =
  let db =
    empty_db ~schema:[ "score", indexed ] ()
    |> db_with
         [ Add (Entity_id 1, "score", Int 100)
         ; Add (Entity_id 2, "score", Float 1.5)
         ; Add (Entity_id 3, "score", Int 2)
         ]
  in
  assert_equal_triples
    "AVET sorts int and float values numerically"
    [ 2, "score", Float 1.5; 3, "score", Int 2; 1, "score", Int 100 ]
    (datoms db Avet ~a:"score" ());
  assert_equal_triples
    "index_range compares int and float bounds numerically"
    [ 3, "score", Int 2 ]
    (index_range db "score" ~start:(Float 1.6) ~stop:(Float 99.9) ());
  assert_equal_triples
    "seek_datoms compares mixed numeric lower bounds numerically"
    [ 3, "score", Int 2; 1, "score", Int 100 ]
    (seek_datoms db Avet ~a:"score" ~v:(Float 1.6) ())

let test_transact__test_compare_numbers_js_issue_404 () =
  let db =
    empty_db ()
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "num", One_value (Float 42.5) ] } ]
    |> db_with [ Retract (Entity_id 1, "num", Some (Int 42)) ]
  in
  assert_equal_triples
    "retracting int 42 must not remove float 42.5"
    [ 1, "num", Float 42.5 ]
    (datoms db Eavt ())

let test_avet_exact_lookup_compares_entire_sequences () =
  let db =
    empty_db ~schema:[ "path", indexed ] ()
    |> db_with
         [ Add (Entity_id 1, "path", List [ Int 1; Int 2 ])
         ; Add (Entity_id 2, "path", List [ Int 1; Int 2; Int 3 ])
         ]
  in
  let entity_ids value =
    datoms db Avet ~a:"path" ~v:value ()
    |> List.map (fun datom -> datom.e)
  in
  if entity_ids (List [ Int 1 ]) <> [] then
    failwith "AVET exact lookup should not match shorter sequence prefixes";
  if entity_ids (List [ Int 1; Int 1 ]) <> [] then
    failwith "AVET exact lookup should not match different second sequence item";
  if entity_ids (List [ Int 1; Int 2 ]) <> [ 1 ] then
    failwith "AVET exact lookup should match the exact shorter sequence";
  if entity_ids (List [ Int 1; Int 2; Int 2 ]) <> [] then
    failwith "AVET exact lookup should not match a sequence between indexed values";
  if entity_ids (List [ Int 1; Int 2; Int 3 ]) <> [ 2 ] then
    failwith "AVET exact lookup should match the exact longer sequence";
  if entity_ids (List [ Int 1; Int 2; Int 3; Int 4 ]) <> [] then
    failwith "AVET exact lookup should not match longer sequence extensions"

let test_indexes_compare_mixed_value_types_like_datascript () =
  let db =
    empty_db ~schema:[ "value", indexed ] ()
    |> db_with
         [ Add (Entity_id 1, "value", String "z")
         ; Add (Entity_id 2, "value", Keyword "kind/name")
         ; Add (Entity_id 3, "value", Bool false)
         ; Add (Entity_id 4, "value", Int 7)
         ; Add (Entity_id 5, "value", String "a")
         ]
  in
  assert_equal_triples
    "AVET sorts mixed value types by DataScript class order"
    [ 2, "value", Keyword "kind/name"
    ; 3, "value", Bool false
    ; 4, "value", Int 7
    ; 5, "value", String "a"
    ; 1, "value", String "z"
    ]
    (datoms db Avet ~a:"value" ());
  assert_equal_triples
    "index_range follows DataScript mixed value type order"
    [ 3, "value", Bool false; 4, "value", Int 7 ]
    (index_range db "value" ~start:(Bool false) ~stop:(Int 99) ())

let test_avet_excludes_unindexed_scalar_attrs () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "age", One_value (Int 31)
                 ; "friend", One_value (Ref 2)
                 ]
             }
         ]
  in
  assert_equal_triples
    "AVET excludes unindexed and untyped attrs, including arbitrary Ref values"
    []
    (datoms db Avet ());
  assert_raises_invalid_arg
    "AVET datoms reject untyped ref-valued attrs"
    (fun () -> ignore (datoms db Avet ~a:"friend" ()));
  let db = empty_db ~schema:[ "friend", ref_attr ] () |> db_with [ Add (Entity_id 1, "friend", Ref 2) ] in
  assert_equal_triples
    "AVET includes schema ref attrs even without explicit index flag"
    [ 1, "friend", Ref 2 ]
    (datoms db Avet ());
  let db = empty_db ~schema:[ "age", indexed ] () |> db_with [ Add (Entity_id 1, "age", Int 31) ] in
  assert_equal_triples
    "AVET includes indexed scalar attrs"
    [ 1, "age", Int 31 ]
    (datoms db Avet ());
  assert_raises_invalid_arg
    "AVET datoms reject unindexed attrs"
    (fun () -> ignore (datoms db Avet ~a:"name" ()));
  assert_raises_invalid_arg_message
    "AVET datoms reject unindexed attrs with upstream message"
    "Attribute :name should be marked as :db/index true"
    (fun () -> ignore (datoms db Avet ~a:"name" ()));
  assert_raises_invalid_arg_message
    "AVET seek_datoms reject unindexed attrs with upstream message"
    "Attribute :name should be marked as :db/index true"
    (fun () -> ignore (seek_datoms db Avet ~a:"name" ()));
  assert_raises_invalid_arg_message
    "AVET rseek_datoms reject unindexed attrs with upstream message"
    "Attribute :name should be marked as :db/index true"
    (fun () -> ignore (rseek_datoms db Avet ~a:"name" ()))

let test_seek_datoms_scans_forward_from_index_tuple () =
  let db =
    init_db
      [ datom ~e:1 ~a:"likes" ~v:(String "fries") ()
      ; datom ~e:1 ~a:"likes" ~v:(String "pizza") ()
      ; datom ~e:1 ~a:"name" ~v:(String "Ivan") ()
      ; datom ~e:2 ~a:"likes" ~v:(String "pie") ()
      ]
  in
  assert_equal_triples
    "seek_datoms starts at the first datom greater than or equal to the tuple"
    [ 1, "likes", String "pizza"; 1, "name", String "Ivan"; 2, "likes", String "pie" ]
    (seek_datoms db Eavt ~e:1 ~a:"likes" ~v:(String "pizza") ());
  assert_equal_triples
    "seek_datoms supports prefix scans"
    [ 2, "likes", String "pie" ]
    (seek_datoms db Eavt ~e:2 ())

let test_rseek_datoms_scans_backward_from_index_tuple () =
  let db =
    init_db
      [ datom ~e:1 ~a:"likes" ~v:(String "fries") ()
      ; datom ~e:1 ~a:"likes" ~v:(String "pizza") ()
      ; datom ~e:1 ~a:"name" ~v:(String "Ivan") ()
      ; datom ~e:2 ~a:"likes" ~v:(String "pie") ()
      ]
  in
  assert_equal_triples
    "rseek_datoms starts at the first datom less than or equal to the tuple and walks backward"
    [ 1, "likes", String "pizza"; 1, "likes", String "fries" ]
    (rseek_datoms db Eavt ~e:1 ~a:"likes" ~v:(String "pizza") ())

let test_seek_datoms_continues_across_avet_attributes () =
  let db =
    init_db
      ~schema:[ "age", indexed; "name", indexed ]
      [ datom ~e:1 ~a:"name" ~v:(String "Petr") ()
      ; datom ~e:1 ~a:"age" ~v:(Int 44) ()
      ; datom ~e:2 ~a:"name" ~v:(String "Ivan") ()
      ; datom ~e:2 ~a:"age" ~v:(Int 25) ()
      ; datom ~e:3 ~a:"name" ~v:(String "Sergey") ()
      ; datom ~e:3 ~a:"age" ~v:(Int 11) ()
      ]
  in
  assert_equal_triples
    "seek_datoms on AVET starts at the value bound and continues into later attrs"
    [ 3, "age", Int 11
    ; 2, "age", Int 25
    ; 1, "age", Int 44
    ; 2, "name", String "Ivan"
    ; 1, "name", String "Petr"
    ; 3, "name", String "Sergey"
    ]
    (seek_datoms db Avet ~a:"age" ~v:(Int 10) ());
  assert_equal_triples
    "seek_datoms on AVET uses string prefix bounds"
    [ 1, "name", String "Petr"; 3, "name", String "Sergey" ]
    (seek_datoms db Avet ~a:"name" ~v:(String "P") ());
  assert_equal_triples
    "seek_datoms on AVET includes the exact lower bound"
    [ 1, "name", String "Petr"; 3, "name", String "Sergey" ]
    (seek_datoms db Avet ~a:"name" ~v:(String "Petr") ())

let test_rseek_datoms_continues_across_avet_attributes () =
  let db =
    init_db
      ~schema:[ "age", indexed; "name", indexed ]
      [ datom ~e:1 ~a:"name" ~v:(String "Petr") ()
      ; datom ~e:1 ~a:"age" ~v:(Int 44) ()
      ; datom ~e:2 ~a:"name" ~v:(String "Ivan") ()
      ; datom ~e:2 ~a:"age" ~v:(Int 25) ()
      ; datom ~e:3 ~a:"name" ~v:(String "Sergey") ()
      ; datom ~e:3 ~a:"age" ~v:(Int 11) ()
      ]
  in
  assert_equal_triples
    "rseek_datoms on AVET starts at the value bound and continues into earlier attrs"
    [ 1, "name", String "Petr"
    ; 2, "name", String "Ivan"
    ; 1, "age", Int 44
    ; 2, "age", Int 25
    ; 3, "age", Int 11
    ]
    (rseek_datoms db Avet ~a:"name" ~v:(String "Petr") ());
  assert_equal_triples
    "rseek_datoms on AVET uses the greatest value below a missing bound"
    [ 2, "age", Int 25; 3, "age", Int 11 ]
    (rseek_datoms db Avet ~a:"age" ~v:(Int 26) ());
  assert_equal_triples
    "rseek_datoms on AVET includes the exact upper bound"
    [ 2, "age", Int 25; 3, "age", Int 11 ]
    (rseek_datoms db Avet ~a:"age" ~v:(Int 25) ())

let test_upstream_index_api_parity_batch () =
  let db =
    empty_db ~schema:[ "age", indexed; "name", indexed ] ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Petr")
         ; Add (Entity_id 1, "age", Int 44)
         ; Add (Entity_id 2, "name", String "Ivan")
         ; Add (Entity_id 2, "age", Int 25)
         ; Add (Entity_id 3, "name", String "Sergey")
         ; Add (Entity_id 3, "age", Int 11)
         ]
  in
  assert_equal_triples
    "upstream index parity AEVT sort order"
    [ 1, "age", Int 44
    ; 2, "age", Int 25
    ; 3, "age", Int 11
    ; 1, "name", String "Petr"
    ; 2, "name", String "Ivan"
    ; 3, "name", String "Sergey"
    ]
    (datoms db Aevt ());
  assert_equal_triples
    "upstream index parity EAVT sort order"
    [ 1, "age", Int 44
    ; 1, "name", String "Petr"
    ; 2, "age", Int 25
    ; 2, "name", String "Ivan"
    ; 3, "age", Int 11
    ; 3, "name", String "Sergey"
    ]
    (datoms db Eavt ());
  assert_equal_triples
    "upstream index parity AVET sort order"
    [ 3, "age", Int 11
    ; 2, "age", Int 25
    ; 1, "age", Int 44
    ; 2, "name", String "Ivan"
    ; 1, "name", String "Petr"
    ; 3, "name", String "Sergey"
    ]
    (datoms db Avet ());
  assert_equal_triples
    "upstream index parity find_datom with empty prefix"
    [ 1, "age", Int 44 ]
    (match find_datom db Eavt () with
     | Some datom -> [ datom ]
     | None -> []);
  assert_equal_triples
    "upstream index parity find_datom with entity prefix"
    [ 2, "age", Int 25 ]
    (match find_datom db Eavt ~e:2 () with
     | Some datom -> [ datom ]
     | None -> []);
  assert_equal_triples
    "upstream index parity find_datom with exact tuple"
    [ 1, "name", String "Petr" ]
    (match find_datom db Eavt ~e:1 ~a:"name" ~v:(String "Petr") () with
     | Some datom -> [ datom ]
     | None -> []);
  if find_datom db Eavt ~e:1 ~a:"name" ~v:(String "Ivan") () <> None then
    failwith "find_datom should reject a mismatched exact tuple";
  if find_datom db Eavt ~e:4 () <> None then
    failwith "find_datom should return None for a missing entity prefix";
  if find_datom (empty_db ()) Eavt () <> None then
    failwith "find_datom on an empty db should return None";
  if find_datom (empty_db ~schema:[ "age", indexed ] ()) Eavt () <> None then
    failwith "find_datom on an empty indexed db should return None";
  assert_raises_invalid_arg_message
    "upstream index parity datoms rejects missing AVET attr"
    "Attribute :alias should be marked as :db/index true"
    (fun () -> ignore (datoms db Avet ~a:"alias" ()));
  assert_raises_invalid_arg_message
    "upstream index parity datoms rejects unindexed AVET exact lookup"
    "Attribute :alias should be marked as :db/index true"
    (fun () -> ignore (datoms db Avet ~a:"alias" ~v:(String "Ivan") ()));
  assert_raises_invalid_arg_message
    "upstream index parity datoms rejects unindexed AVET exact lookup with tx"
    "Attribute :alias should be marked as :db/index true"
    (fun () -> ignore (datoms db Avet ~a:"alias" ~v:(String "Ivan") ~tx:1 ()));
  let path_db =
    empty_db ~schema:[ "path", indexed ] ()
    |> db_with
         [ Add (Entity_id 1, "path", List [ Int 1; Int 2 ])
         ; Add (Entity_id 2, "path", List [ Int 1; Int 2; Int 3 ])
         ]
  in
  let path_entities value =
    datoms path_db Avet ~a:"path" ~v:value ()
    |> List.map (fun datom -> datom.e)
  in
  if path_entities (List [ Int 1 ]) <> [] then
    failwith "AVET sequence exact lookup should reject shorter prefixes";
  if path_entities (List [ Int 1; Int 2 ]) <> [ 1 ] then
    failwith "AVET sequence exact lookup should match the shorter stored value";
  if path_entities (List [ Int 1; Int 2; Int 3 ]) <> [ 2 ] then
    failwith "AVET sequence exact lookup should match the longer stored value";
  if path_entities (List [ Int 1; Int 2; Int 3; Int 4 ]) <> [] then
    failwith "AVET sequence exact lookup should reject longer extensions"

let test_db_with_adds_entities () =
  let schema = [ "aka", many ] in
  let db =
    empty_db ~schema ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "aka", Many_values [ String "IV"; String "Terrible" ]
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "age", One_value (Int 37)
                 ; "active", One_value (Bool false)
                 ]
             }
         ]
  in
  assert_equal_triples
    "db_with expands entity maps into datoms"
    [ 1, "aka", String "IV"
    ; 1, "aka", String "Terrible"
    ; 1, "name", String "Ivan"
    ; 2, "active", Bool false
    ; 2, "age", Int 37
    ; 2, "name", String "Petr"
    ]
    (datoms db Eavt ())

let test_with_tx_returns_transaction_report () =
  let db = empty_db () in
  let report = with_tx ~tx_meta:[ "op", Keyword "with" ] db [ Add (Entity_id 1, "name", String "Ivan") ] in
  assert_equal_triples "with_tx preserves db-before" [] (datoms report.db_before Eavt ());
  assert_equal_triples
    "with_tx exposes db-after"
    [ 1, "name", String "Ivan" ]
    (datoms report.db_after Eavt ());
  assert_equal_triples "with_tx leaves the input db immutable" [] (datoms db Eavt ());
  if report.tx_meta <> [ "op", Keyword "with" ] then failwith "with_tx should preserve tx metadata"

let test_entity_map_expands_collection_values_for_many_attrs () =
  let db =
    empty_db ~schema:[ "tag", many; "setting", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "tag", One_value (List [ String "a"; String "b" ])
                 ; "tag", One_value (Set [ String "b"; String "c" ])
                 ; "setting", One_value (Map [ Keyword "mode", String "dark" ])
                 ]
             }
         ]
  in
  assert_equal_triples
    "entity maps expand list and set values for cardinality-many attrs"
    [ 1, "setting", Map [ Keyword "mode", String "dark" ]
    ; 1, "tag", String "a"
    ; 1, "tag", String "b"
    ; 1, "tag", String "c"
    ]
    (datoms db Eavt ())

let test_entity_map_db_id_attr_is_not_stored () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "db/id", One_value (Int 99)
                 ; "name", One_value (String "Ivan")
                 ]
             }
         ]
  in
  assert_equal_triples
    "entity map db/id is metadata, not a stored datom"
    [ 1, "name", String "Ivan" ]
    (datoms db Eavt ())

let test_transact__test_with () =
  let schema = [ "aka", many ] in
  let db =
    empty_db ~schema ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan"); Add (Entity_id 1, "name", String "Petr")
         ; Add (Entity_id 1, "aka", String "Devil"); Add (Entity_id 1, "aka", String "Tupen")
         ]
    |> db_with
         [ Retract (Entity_id 1, "name", Some (String "Petr"))
         ; Retract (Entity_id 1, "aka", Some (String "Devil"))
         ]
  in
  assert_equal_triples
    "cardinality one values are replaced and retractions remove exact datoms"
    [ 1, "aka", String "Tupen" ]
    (datoms db Eavt ())

let test_transact__test_retract_fns_not_found () =
  let db =
    empty_db ~schema:[ "name", unique_identity; "aka", many ] ()
    |> db_with [ Add (Entity_id 1, "name", String "Ivan"); Add (Entity_id 1, "aka", String "Vanya") ]
  in
  let db =
    db_with
      [ Retract (Lookup_ref ("name", String "Petr"), "name", Some (String "Petr"))
      ; RetractAttr (Lookup_ref ("name", String "Petr"), "aka")
      ; RetractEntity (Lookup_ref ("name", String "Petr"))
      ]
      db
  in
  assert_equal_triples
    "missing lookup refs in retract operations are no-ops"
    [ 1, "aka", String "Vanya"; 1, "name", String "Ivan" ]
    (datoms db Eavt ())

let test_tuple_attrs_track_source_attrs () =
  let db =
    empty_db ~schema:[ "a+b", tuple [ "a"; "b" ] ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ]
             }
         ]
  in
  assert_equal_triples
    "tuple attrs are derived from their source attrs"
    [ 1, "a", String "A"
    ; 1, "a+b", Tuple [ Some (String "A"); Some (String "B") ]
    ; 1, "b", String "B"
    ]
    (datoms db Eavt ());
  let db =
    db
    |> db_with [ Add (Entity_id 1, "a", String "A2") ]
    |> db_with [ Retract (Entity_id 1, "b", Some (String "B")) ]
  in
  assert_equal_triples
    "tuple attrs update when source attrs change or disappear"
    [ 1, "a", String "A2"; 1, "a+b", Tuple [ Some (String "A2"); None ] ]
    (datoms db Eavt ())

let test_tuple_attrs_reject_direct_writes () =
  assert_raises_invalid_arg
    "tuple attrs cannot be directly asserted"
    (fun () ->
      ignore
        (empty_db ~schema:[ "a+b", tuple [ "a"; "b" ] ] ()
         |> db_with [ Add (Entity_id 1, "a+b", Tuple [ Some (String "A"); Some (String "B") ]) ]))

let test_tuple_attrs_ignore_direct_writes_that_match_sources () =
  let db =
    empty_db ~schema:[ "a+b", tuple [ "a"; "b" ] ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ]
             }
         ]
  in
  let db =
    db
    |> db_with [ Add (Entity_id 1, "a+b", Tuple [ Some (String "A"); Some (String "B") ]) ]
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "a+b", One_value (Tuple [ Some (String "A"); Some (String "B") ]) ]
             }
         ]
  in
  assert_equal_triples
    "direct tuple writes that match source attrs are ignored"
    [ 1, "a", String "A"
    ; 1, "a+b", Tuple [ Some (String "A"); Some (String "B") ]
    ; 1, "b", String "B"
    ]
    (datoms db Eavt ());
  assert_raises_invalid_arg
    "direct tuple writes with mismatched values are rejected"
    (fun () -> ignore (db_with [ Add (Entity_id 1, "a+b", Tuple [ Some (String "A"); Some (String "C") ]) ] db));
  assert_raises_invalid_arg
    "direct tuple writes with nils are rejected"
    (fun () -> ignore (db_with [ Add (Entity_id 1, "a+b", Tuple [ Some (String "A"); None ]) ] db))

let test_tuple_attrs_validate_entity_map_direct_writes_after_sources () =
  let db =
    empty_db ~schema:[ "a+b", tuple [ "a"; "b" ] ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "a", One_value (String "a"); "b", One_value (String "B") ]
             }
         ]
  in
  let db =
    db
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "a+b", One_value (Tuple [ Some (String "A"); Some (String "B") ])
                 ; "a", One_value (String "A")
                 ]
             }
         ]
  in
  assert_equal_triples
    "entity maps validate direct tuple writes against final source attrs"
    [ 1, "a", String "A"
    ; 1, "a+b", Tuple [ Some (String "A"); Some (String "B") ]
    ; 1, "b", String "B"
    ]
    (datoms db Eavt ())

let test_tuple_values_resolve_lookup_refs () =
  let db =
    empty_db ~schema:[ "name", unique_identity; "friend", ref_attr; "friend+label", tuple [ "friend"; "label" ] ] ()
    |> db_with
         [ Add (Entity_id 2, "name", String "Ivan")
         ; Add (Entity_id 1, "friend", Ref_to (Lookup_ref ("name", String "Ivan")))
         ; Add (Entity_id 1, "label", String "best")
         ]
    |> db_with
         [ Add
             ( Entity_id 1
             , "friend+label"
             , Tuple [ Some (Ref_to (Lookup_ref ("name", String "Ivan"))); Some (String "best") ] )
         ]
  in
  assert_equal_triples
    "tuple values resolve lookup refs before matching source attrs"
    [ 1, "friend", Ref 2
    ; 1, "friend+label", Tuple [ Some (Ref 2); Some (String "best") ]
    ; 1, "label", String "best"
    ; 2, "name", String "Ivan"
    ]
    (datoms db Eavt ())

let test_tuple_lookup_refs_resolve_nested_lookup_refs () =
  let db =
    empty_db
      ~schema:
        [ "name", unique_identity
        ; "ref", ref_attr
        ; "ref+name", tuple_unique_identity [ "ref"; "name" ]
        ]
      ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Petr"); "ref", One_value (Ref 1) ] }
         ]
  in
  let lookup =
    Lookup_ref
      ( "ref+name"
      , Tuple [ Some (Ref_to (Lookup_ref ("name", String "Ivan"))); Some (String "Petr") ] )
  in
  (match entity db lookup with
   | Some entity -> assert_equal_int "tuple lookup ref entity id" 3 entity.id
   | None -> failwith "expected tuple lookup ref to resolve nested lookup ref");
  (match entity db (Lookup_ref ("ref+name", List [ Int 1; String "Petr" ])) with
   | Some entity -> assert_equal_int "list tuple lookup ref entity id" 3 entity.id
   | None -> failwith "expected list tuple lookup ref to resolve");
  (match
     entity
       db
       (Lookup_ref ("ref+name", List [ List [ Keyword "name"; String "Ivan" ]; String "Petr" ]))
   with
   | Some entity -> assert_equal_int "list tuple nested lookup ref entity id" 3 entity.id
   | None -> failwith "expected list tuple nested lookup ref to resolve");
  (match pull db [ Pull_attr "name" ] lookup with
   | Some pulled ->
     assert_equal_pulled_attrs
       "pull accepts tuple lookup refs with nested lookup refs"
       [ kw "name", Pulled_scalar (String "Petr") ]
       pulled
   | None -> failwith "expected tuple lookup ref pull to resolve")

let test_edn_tuple_lookup_refs_resolve_nested_lookup_refs () =
  let db =
    empty_db
      ~schema:
        [ "name", unique_identity
        ; "ref", ref_attr
        ; "ref+name", tuple_unique_identity [ "ref"; "name" ]
        ]
      ()
    |> db_with_string
         "[{:db/id 1 :name \"Ivan\"}
           {:db/id 3 :name \"Petr\" :ref 1}]"
    |> db_with_string
         "[{:db/id [:ref+name [[:name \"Ivan\"] \"Petr\"]] :age 32}]"
  in
  assert_equal_triples
    "EDN list tuple lookup refs resolve nested lookup refs"
    [ 3, "age", Int 32
    ; 3, "name", String "Petr"
    ; 3, "ref", Ref 1
    ; 3, "ref+name", Tuple [ Some (Ref 1); Some (String "Petr") ]
    ]
    (datoms db Eavt ~e:3 ())

let test_tuple_attrs_are_indexed_by_default () =
  let db =
    empty_db ~schema:[ "a+b", tuple_unindexed [ "a"; "b" ] ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ]
             }
         ]
  in
  assert_equal_triples
    "tuple attrs are available in AVET even without an explicit index flag"
    [ 1, "a+b", Tuple [ Some (String "A"); Some (String "B") ] ]
    (datoms db Avet ~a:"a+b" ~v:(Tuple [ Some (String "A"); Some (String "B") ]) ())

let test_tuple_attrs_support_avet_range_bounds () =
  let tuple_value a b c = Tuple [ Some (String a); Some (String b); Some (String c) ] in
  let tuple_bound a b c = Tuple [ Some (String a); Some (String b); c ] in
  let db =
    empty_db ~schema:[ "a+b+c", tuple [ "a"; "b"; "c" ] ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "a", One_value (String "a"); "b", One_value (String "b"); "c", One_value (String "c") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "a", One_value (String "A"); "b", One_value (String "b"); "c", One_value (String "c") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "a", One_value (String "a"); "b", One_value (String "B"); "c", One_value (String "c") ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "a", One_value (String "A"); "b", One_value (String "B"); "c", One_value (String "c") ] }
         ; Entity { db_id = Some (Entity_id 5); attrs = [ "a", One_value (String "a"); "b", One_value (String "b"); "c", One_value (String "C") ] }
         ; Entity { db_id = Some (Entity_id 6); attrs = [ "a", One_value (String "A"); "b", One_value (String "b"); "c", One_value (String "C") ] }
         ; Entity { db_id = Some (Entity_id 7); attrs = [ "a", One_value (String "a"); "b", One_value (String "B"); "c", One_value (String "C") ] }
         ; Entity { db_id = Some (Entity_id 8); attrs = [ "a", One_value (String "A"); "b", One_value (String "B"); "c", One_value (String "C") ] }
         ]
  in
  assert_equal_triples
    "tuple attrs support exact AVET lookup"
    [ 6, "a+b+c", tuple_value "A" "b" "C" ]
    (datoms db Avet ~a:"a+b+c" ~v:(tuple_value "A" "b" "C") ());
  assert_equal_triples
    "tuple attrs support AVET index ranges"
    [ 8, "a+b+c", tuple_value "A" "B" "C"
    ; 4, "a+b+c", tuple_value "A" "B" "c"
    ; 6, "a+b+c", tuple_value "A" "b" "C"
    ; 2, "a+b+c", tuple_value "A" "b" "c"
    ]
    (index_range
       db
       "a+b+c"
       ~start:(tuple_value "A" "B" "C")
       ~stop:(tuple_value "A" "b" "c")
       ());
  assert_equal_triples
    "tuple attrs support nil components in AVET range bounds"
    [ 8, "a+b+c", tuple_value "A" "B" "C"
    ; 4, "a+b+c", tuple_value "A" "B" "c"
    ]
    (index_range
       db
       "a+b+c"
       ~start:(tuple_bound "A" "B" None)
       ~stop:(tuple_bound "A" "b" None)
       ())

let test_tuple_types_validate_direct_tuple_values () =
  let db =
    empty_db ~schema:[ "name+score", typed_tuple [ StringType; NumberType ] ] ()
    |> db_with [ Add (Entity_id 1, "name+score", Tuple [ Some (String "Ivan"); Some (Int 10) ]) ]
  in
  assert_equal_triples
    "typed tuple attrs accept matching direct tuple values"
    [ 1, "name+score", Tuple [ Some (String "Ivan"); Some (Int 10) ] ]
    (datoms db Eavt ());
  assert_raises_invalid_arg
    "typed tuple attrs reject wrong arity"
    (fun () ->
      ignore (db_with [ Add (Entity_id 2, "name+score", Tuple [ Some (String "Petr") ]) ] db));
  assert_raises_invalid_arg
    "typed tuple attrs reject wrong element types"
    (fun () ->
      ignore
        (db_with
           [ Add (Entity_id 2, "name+score", Tuple [ Some (String "Petr"); Some (String "high") ]) ]
           db))

let test_transact__test_db_fn_cas () =
  let db =
    empty_db ()
    |> db_with [ Add (Entity_id 1, "age", Int 31) ]
    |> db_with [ CompareAndSet (Entity_id 1, "age", Some (Int 31), Int 32) ]
  in
  assert_equal_triples
    "CompareAndSet updates when expected value matches"
    [ 1, "age", Int 32 ]
    (datoms db Eavt ());
  assert_raises_invalid_arg_message
    "CompareAndSet reports mismatched cardinality-one values like upstream"
    ":db.fn/cas failed on datom [1 :age 32], expected 31"
    (fun () -> ignore (db_with [ CompareAndSet (Entity_id 1, "age", Some (Int 31), Int 33) ] db));
  let db =
    db_with [ CompareAndSet (Entity_id 1, "nickname", None, String "p") ] db
  in
  assert_equal_triples
    "CompareAndSet can assert missing attributes with expected None"
    [ 1, "age", Int 32; 1, "nickname", String "p" ]
    (datoms db Eavt ())

let test_db_with_compare_and_set_on_many_attr () =
  let db =
    empty_db ~schema:[ "label", many ] ()
    |> db_with [ Add (Entity_id 1, "label", String "x"); Add (Entity_id 1, "label", String "y") ]
    |> db_with [ CompareAndSet (Entity_id 1, "label", Some (String "x"), String "z") ]
  in
  assert_equal_triples
    "CompareAndSet on many attrs succeeds when expected value is present"
    [ 1, "label", String "x"; 1, "label", String "y"; 1, "label", String "z" ]
    (datoms db Eavt ());
  assert_raises_invalid_arg_message
    "CompareAndSet reports mismatched cardinality-many values like upstream"
    ":db.fn/cas failed on datom [1 :label (\"x\" \"y\" \"z\")], expected \"missing\""
    (fun () -> ignore (db_with [ CompareAndSet (Entity_id 1, "label", Some (String "missing"), String "new") ] db))

let test_transact__test_retract_without_value_issue_339 () =
  let db =
    empty_db ~schema:[ "aka", many; "friend", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "age", One_value (Int 15)
                 ; "aka", Many_values [ String "X"; String "Y"; String "Z" ]
                 ; "friend", One_value (Ref 2)
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "age", One_value (Int 37)
                 ; "employed", One_value (Bool true)
                 ; "married", One_value (Bool false)
                 ]
             }
         ]
  in
  let retracted =
    db_with
      [ Retract (Entity_id 1, "name", None)
      ; Retract (Entity_id 1, "aka", None)
      ; Retract (Entity_id 2, "employed", None)
      ; Retract (Entity_id 2, "married", None)
      ]
      db
  in
  assert_equal_triples
    "Retract without a value removes true and false values"
    [ 1, "age", Int 15; 1, "friend", Ref 2; 2, "age", Int 37; 2, "name", String "Petr" ]
    (datoms retracted Eavt ());
  let unchanged = db_with [ Retract (Entity_id 2, "employed", Some (Bool false)) ] db in
  assert_equal_triples
    "Retract with a mismatched false value leaves true values intact"
    [ 2, "employed", Bool true ]
    (datoms unchanged Eavt ~e:2 ~a:"employed" ())

let test_transact__test_uncomparable_issue_356 () =
  let map1 = Map [ Keyword "map", Int 1 ] in
  let map2 = Map [ Keyword "map", Int 2 ] in
  let map3 = Map [ Keyword "map", Int 3 ] in
  let db =
    empty_db ~schema:[ "multi", many; "index", indexed ] ()
  in
  let single_db =
    db
    |> db_with [ Add (Entity_id 1, "single", map1) ]
    |> db_with [ Retract (Entity_id 1, "single", Some map1) ]
    |> db_with [ Add (Entity_id 1, "single", map2) ]
    |> db_with [ Add (Entity_id 1, "single", map3) ]
  in
  assert_equal_triples
    "map values can be replaced on cardinality-one attrs"
    [ 1, "single", map3 ]
    (datoms single_db Eavt ());
  assert_equal_triples
    "map values can be used as EAVT lookup values"
    [ 1, "single", map3 ]
    (datoms single_db Eavt ~e:1 ~a:"single" ~v:map3 ());
  assert_equal_triples
    "map values can be used as AEVT lookup values"
    [ 1, "single", map3 ]
    (datoms single_db Aevt ~a:"single" ~e:1 ~v:map3 ());
  let multi_db =
    db
    |> db_with
         [ Add (Entity_id 1, "multi", map1)
         ; Add (Entity_id 1, "multi", map1)
         ; Add (Entity_id 1, "multi", map2)
         ]
  in
  assert_equal_triples
    "map values can be stored in cardinality-many attrs without duplicating facts"
    [ 1, "multi", map1; 1, "multi", map2 ]
    (datoms multi_db Eavt ());
  assert_equal_triples
    "map values can be used as cardinality-many lookup values"
    [ 1, "multi", map2 ]
    (datoms multi_db Aevt ~a:"multi" ~e:1 ~v:map2 ());
  let indexed_db =
    db
    |> db_with [ Add (Entity_id 1, "index", map1) ]
    |> db_with [ Add (Entity_id 1, "index", map2) ]
    |> db_with [ Add (Entity_id 1, "index", map3) ]
  in
  assert_equal_triples
    "map values can be used in AVET lookup values"
    [ 1, "index", map3 ]
    (datoms indexed_db Avet ~a:"index" ~v:map3 ~e:1 ())

let test_nil_values_are_query_only () =
  assert_raises_invalid_arg
    "nil values cannot be stored as datom values"
    (fun () -> ignore (empty_db () |> db_with [ Add (Entity_id 1, "maybe", Nil) ]))

let test_list_values_can_be_indexed_exactly () =
  let path12 = List [ Int 1; Int 2 ] in
  let path123 = List [ Int 1; Int 2; Int 3 ] in
  let db =
    empty_db ~schema:[ "path", indexed ] ()
    |> db_with
         [ Add (Entity_id 1, "path", path12)
         ; Add (Entity_id 2, "path", path123)
         ]
  in
  assert_equal_triples
    "list values can be stored and found in indexed attrs"
    [ 1, "path", path12 ]
    (datoms db Avet ~a:"path" ~v:path12 ());
  assert_equal_triples
    "list values compare structurally for exact index lookups"
    [ 2, "path", path123 ]
    (datoms db Avet ~a:"path" ~v:path123 ());
  assert_equal_triples
    "shorter list prefixes are not exact AVET matches"
    []
    (datoms db Avet ~a:"path" ~v:(List [ Int 1 ]) ())

let test_list_values_use_datascript_length_first_ordering () =
  let single_high = List [ Int 2 ] in
  let pair_low = List [ Int 1; Int 100 ] in
  let pair_high = List [ Int 2; Int 0 ] in
  let db =
    empty_db ~schema:[ "path", indexed ] ()
    |> db_with
         [ Add (Entity_id 1, "path", pair_low)
         ; Add (Entity_id 2, "path", single_high)
         ; Add (Entity_id 3, "path", pair_high)
         ]
  in
  assert_equal_triples
    "AVET sorts sequential values by length before elements"
    [ 2, "path", single_high; 1, "path", pair_low; 3, "path", pair_high ]
    (datoms db Avet ~a:"path" ());
  assert_equal_triples
    "index_range uses length-first sequential ordering"
    [ 1, "path", pair_low; 3, "path", pair_high ]
    (index_range db "path" ~start:(List [ Int 0; Int 0 ]) ~stop:(List [ Int 9; Int 9 ]) ());
  assert_equal_triples
    "seek_datoms uses length-first sequential lower bounds"
    [ 1, "path", pair_low; 3, "path", pair_high ]
    (seek_datoms db Avet ~a:"path" ~v:(List [ Int 0; Int 0 ]) ())

let test_map_values_are_order_insensitive () =
  let ordered = Map [ Keyword "a", Int 1; Keyword "b", Int 2 ] in
  let reversed = Map [ Keyword "b", Int 2; Keyword "a", Int 1 ] in
  let db =
    empty_db ~schema:[ "index", indexed ] ()
    |> db_with [ Add (Entity_id 1, "index", ordered) ]
  in
  assert_equal_triples
    "map value index lookup ignores entry order"
    [ 1, "index", ordered ]
    (datoms db Avet ~a:"index" ~v:reversed ());
  let db = db_with [ Retract (Entity_id 1, "index", Some reversed) ] db in
  assert_equal_triples
    "map value retraction ignores entry order"
    []
    (datoms db Eavt ())

let test_init_db_normalizes_map_values () =
  let ordered = Map [ Keyword "a", Int 1; Keyword "b", Int 2 ] in
  let reversed = Map [ Keyword "b", Int 2; Keyword "a", Int 1 ] in
  let db =
    init_db
      ~schema:[ "index", indexed ]
      [ datom ~e:1 ~a:"index" ~v:reversed () ]
  in
  assert_equal_triples
    "init_db normalizes map values for later index lookups"
    [ 1, "index", ordered ]
    (datoms db Avet ~a:"index" ~v:ordered ())

let test_set_values_are_order_insensitive () =
  let ordered = Set [ Keyword "a"; Keyword "b" ] in
  let reversed = Set [ Keyword "b"; Keyword "a"; Keyword "a" ] in
  let db =
    empty_db ~schema:[ "tags", indexed ] ()
    |> db_with [ Add (Entity_id 1, "tags", reversed) ]
  in
  assert_equal_triples
    "set values are normalized when stored"
    [ 1, "tags", ordered ]
    (datoms db Eavt ());
  assert_equal_triples
    "set value index lookup ignores element order and duplicates"
    [ 1, "tags", ordered ]
    (datoms db Avet ~a:"tags" ~v:ordered ());
  let db = db_with [ Retract (Entity_id 1, "tags", Some ordered) ] db in
  assert_equal_triples
    "set value retraction ignores element order"
    []
    (datoms db Eavt ())

let test_init_db_normalizes_set_values () =
  let ordered = Set [ Keyword "a"; Keyword "b" ] in
  let reversed = Set [ Keyword "b"; Keyword "a"; Keyword "a" ] in
  let db =
    init_db
      ~schema:[ "tags", indexed ]
      [ datom ~e:1 ~a:"tags" ~v:reversed () ]
  in
  assert_equal_triples
    "init_db normalizes set values for later index lookups"
    [ 1, "tags", ordered ]
    (datoms db Avet ~a:"tags" ~v:ordered ())

let test_entid_normalizes_unordered_values () =
  let ordered_map = Map [ Keyword "a", Int 1; Keyword "b", Int 2 ] in
  let reversed_map = Map [ Keyword "b", Int 2; Keyword "a", Int 1 ] in
  let ordered_set = Set [ Keyword "a"; Keyword "b" ] in
  let reversed_set = Set [ Keyword "b"; Keyword "a"; Keyword "a" ] in
  let db =
    empty_db ~schema:[ "fingerprint", unique_identity; "tags", unique_identity ] ()
    |> db_with
         [ Add (Entity_id 1, "fingerprint", ordered_map)
         ; Add (Entity_id 2, "tags", reversed_set)
         ]
  in
  assert_equal_int
    "entid normalizes map lookup values"
    1
    (Option.get (entid db "fingerprint" reversed_map));
  assert_equal_int
    "entid normalizes set lookup values"
    2
    (Option.get (entid db "tags" ordered_set))

let test_transact__test_transitive_type_compare_issue_386 () =
  let uid_values =
    [ String "2LB4tlJGy"
    ; String "2ON453J0Z"
    ; String "2KqLLNbPg"
    ; String "2L0dcD7yy"
    ; String "2KqFNrhTZ"
    ; String "2KdQmItUD"
    ; String "2O8BcBfIL"
    ; String "2L4ZbI7nK"
    ; String "2KotiW36Z"
    ; String "2O4o-y5J8"
    ; String "2KimvuGko"
    ; String "dTR20ficj"
    ; String "wRmp6bXAx"
    ; String "rfL-iQOZm"
    ; String "tya6s422-"
    ; Int 45619
    ]
  in
  let db =
    List.fold_left
      (fun db uid ->
        db_with
          [ Entity
              { db_id = None
              ; attrs = [ "block/uid", One_value uid ]
              }
          ]
          db)
      (empty_db ~schema:[ "block/uid", unique_identity ] ())
      uid_values
  in
  datoms db Eavt ()
  |> List.iter (fun datom ->
    match entity db (Lookup_ref (datom.a, datom.v)) with
    | Some _ -> ()
    | None ->
      failf
        "unique identity mixed value lookup should resolve [%s %s]"
        datom.a
        (debug_value datom.v))

let test_tempids_are_rejected_in_non_add_ops () =
  let db = empty_db () |> db_with [ Add (Entity_id 1, "name", String "Ivan") ] in
  assert_raises_invalid_arg
    "Retract rejects tempids"
    (fun () -> ignore (db_with [ Retract (Temp_id "x", "name", Some (String "Ivan")) ] db));
  assert_raises_invalid_arg
    "RetractAttr rejects tempids"
    (fun () -> ignore (db_with [ RetractAttr (Temp_id "x", "name") ] db));
  assert_raises_invalid_arg
    "RetractEntity rejects tempids"
    (fun () -> ignore (db_with [ RetractEntity (Temp_id "x") ] db));
  assert_raises_invalid_arg
    "CompareAndSet rejects tempids"
    (fun () -> ignore (db_with [ CompareAndSet (Temp_id "x", "name", None, String "Petr") ] db))

let test_value_only_tempids_are_rejected () =
  assert_raises_invalid_arg_message
    "tempids used only as ref values are rejected like upstream"
    "Tempids used only as value in transaction: (missing)"
    (fun () ->
      ignore
        (empty_db ~schema:[ "friend", ref_attr ] ()
         |> db_with [ Add (Entity_id 1, "friend", Ref_to (Temp_id "missing")) ]));
  let report =
    transact
      (empty_db ~schema:[ "friend", ref_attr ] ())
      [ Add (Entity_id 1, "friend", Ref_to (Temp_id "friend"))
      ; Add (Temp_id "friend", "name", String "Petr")
      ]
  in
  assert_equal_triples
    "tempids used as both value and entity resolve normally"
    [ 1, "friend", Ref 2; 2, "name", String "Petr" ]
    (datoms report.db_after Eavt ());
  assert_equal_tempids
    "value tempid should be reported when it is also used as an entity"
    [ "db/current-tx", tx0 + 1; "friend", 2 ]
    report.tempids

let test_empty_entity_tempids_are_not_entity_usage () =
  let db = empty_db ~schema:[ "friend", ref_attr; "multi", many ] () in
  assert_raises_invalid_arg
    "empty entity maps should not define tempids used as ref values"
    (fun () ->
      ignore
        (db_with
           [ Entity { db_id = Some (Temp_id "empty"); attrs = [] }
           ; Add (Entity_id 2, "friend", Ref_to (Temp_id "empty"))
           ]
           db));
  assert_raises_invalid_arg
    "entity maps with empty many values should not define tempids used as ref values"
    (fun () ->
      ignore
        (db_with
           [ Entity { db_id = Some (Temp_id "empty"); attrs = [ "multi", Many_values [] ] }
           ; Add (Entity_id 2, "friend", Ref_to (Temp_id "empty"))
           ]
           db));
  assert_raises_invalid_arg
    "entity maps with empty list values for many attrs should not define tempids used as ref values"
    (fun () ->
      ignore
        (db_with
           [ Entity { db_id = Some (Temp_id "empty"); attrs = [ "multi", One_value (List []) ] }
           ; Add (Entity_id 2, "friend", Ref_to (Temp_id "empty"))
           ]
           db))

let test_tempid_generates_unique_entity_refs () =
  let first = tempid () in
  let second = tempid () in
  if first = second then failwith "tempid should generate unique refs";
  if tempid ~part:"db.part/tx" () <> CurrentTx then
    failwith "tempid should return current tx for db.part/tx";
  if tempid ~value:(-42) () <> Temp_id "-42" then
    failwith "tempid should accept explicit negative ids";
  if tempid ~value:42 () <> Entity_id 42 then
    failwith "tempid should accept explicit positive ids";
  if tempid ~part:"db.part/tx" ~value:(-42) () <> CurrentTx then
    failwith "tempid should prefer current tx part over explicit value";
  let db =
    empty_db ()
    |> db_with
         [ Add (first, "name", String "Ivan")
         ; Add (second, "name", String "Petr")
         ; Add (tempid ~value:(-43) (), "name", String "Oleg")
         ; Add (tempid ~part:"db.part/tx" (), "tx-prop", Bool true)
         ]
  in
  assert_equal_triples
    "generated tempids can be used as transaction entity refs"
    [ 1, "name", String "Ivan"
    ; 2, "name", String "Petr"
    ; 3, "name", String "Oleg"
    ; tx0 + 1, "tx-prop", Bool true
    ]
    (datoms db Eavt ())

let test_transact__test_db_fn () =
  let report =
    transact
      (empty_db ())
      [ Add (Entity_id 1, "name", String "Ivan")
      ; Call
          (fun db ->
            match entity_attr (Option.get (entity db (Entity_id 1))) "name" with
            | Some (One_value (String "Ivan")) -> [ Add (Entity_id 1, "seen", Bool true) ]
            | _ -> [])
      ; Call (fun _ -> [ Add (Temp_id "generated", "name", String "Generated") ])
      ]
  in
  assert_equal_triples
    "transaction function sees prior ops and expands into the same transaction"
    [ 1, "name", String "Ivan"; 1, "seen", Bool true; 2, "name", String "Generated" ]
    (datoms report.db_after Eavt ());
  assert_equal_datoms
    "transaction function expansions use the outer transaction id"
    [ datom ~tx:(tx0 + 1) ~e:1 ~a:"name" ~v:(String "Ivan") ()
    ; datom ~tx:(tx0 + 1) ~e:1 ~a:"seen" ~v:(Bool true) ()
    ; datom ~tx:(tx0 + 1) ~e:2 ~a:"name" ~v:(String "Generated") ()
    ]
    report.tx_data;
  assert_equal_tempids
    "transaction function tempids should be reported"
    [ "db/current-tx", tx0 + 1; "generated", 2 ]
    report.tempids

let test_transact__test_db_fn_returning_entity_without_db_id_issue_474 () =
  let report =
    transact
      (empty_db ())
      [ Call
          (fun _ ->
            [ Entity
                { db_id = None
                ; attrs = [ "foo", One_value (String "bar") ]
                }
            ])
      ]
  in
  assert_equal_triples
    "transaction functions can return entity maps without db/id"
    [ 1, "foo", String "bar" ]
    (datoms report.db_after Eavt ())

let test_transact__test_db_ident_fn () =
  let db =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "db/ident", One_value (Keyword "Petr")
                 ; "name", One_value (String "Petr")
                 ; "age", One_value (Int 31)
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "db/ident", One_value (Keyword "inc-age") ]
             }
         ; InstallTxFn
             ( Ident "inc-age"
             , fun db args ->
                 match args with
                 | [ String name ] ->
                   (match entid db "name" (String name) with
                    | Some entity_id ->
                      [ Add (Entity_id entity_id, "age", Int 32)
                      ; Add (Entity_id entity_id, "had-birthday", Bool true)
                      ]
                    | None -> invalid_arg ("No entity with name: " ^ name))
                 | _ -> invalid_arg "inc-age expects one name" )
         ]
  in
  assert_raises_invalid_arg
    "CallIdent rejects missing transaction function idents"
    (fun () -> ignore (db_with [ CallIdent (Ident "unknown-fn", []) ] db));
  assert_raises_invalid_arg
    "CallIdent rejects idents without installed transaction functions"
    (fun () -> ignore (db_with [ CallIdent (Ident "Petr", []) ] db));
  assert_raises_invalid_arg
    "CallIdent propagates transaction function errors"
    (fun () -> ignore (db_with [ CallIdent (Ident "inc-age", [ String "Bob" ]) ] db));
  let db = db_with [ CallIdent (Ident "inc-age", [ String "Petr" ]) ] db in
  assert_equal_triples
    "CallIdent invokes db/fn metadata by ident"
    [ 1, "age", Int 32
    ; 1, "db/ident", Keyword "Petr"
    ; 1, "had-birthday", Bool true
    ; 1, "name", String "Petr"
    ; 2, "db/ident", Keyword "inc-age"
    ]
    (datoms db Eavt ())

let test_transact__test_large_ids_issue_292 () =
  let too_large = 285_873_023_227_265 in
  let max_supported_entity_id = 0x7fffffff in
  let first_unsupported_entity_id = max_supported_entity_id + 1 in
  let highest_supported_message value =
    "Highest supported entity id is "
    ^ string_of_int max_supported_entity_id
    ^ ", got "
    ^ string_of_int value
  in
  let db =
    empty_db ()
    |> db_with [ Add (Entity_id max_supported_entity_id, "name", String "Max") ]
  in
  assert_equal_triples
    "Add accepts cljs max supported entity id"
    [ max_supported_entity_id, "name", String "Max" ]
    (datoms db Eavt ~e:max_supported_entity_id ~a:"name" ());
  assert_raises_invalid_arg_message
    "Add rejects entity ids above cljs max supported range"
    (highest_supported_message first_unsupported_entity_id)
    (fun () ->
      ignore
        (empty_db ()
         |> db_with [ Add (Entity_id first_unsupported_entity_id, "name", String "Too large") ]));
  assert_raises_invalid_arg
    "Add rejects negative explicit entity ids"
    (fun () -> ignore (empty_db () |> db_with [ Add (Entity_id (-1), "name", String "Negative") ]));
  assert_raises_invalid_arg
    "Ref values reject negative explicit entity ids"
    (fun () ->
      ignore
        (empty_db ~schema:[ "ref", ref_attr ] ()
         |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "ref", One_value (Ref (-1)) ] } ]));
  assert_raises_invalid_arg_message
    "Add rejects entity ids above the supported range"
    (highest_supported_message too_large)
    (fun () -> ignore (empty_db () |> db_with [ Add (Entity_id too_large, "name", String "Valerii") ]));
  assert_raises_invalid_arg_message
    "Entity maps reject entity ids above the supported range"
    (highest_supported_message too_large)
    (fun () ->
      ignore
        (empty_db ()
         |> db_with [ Entity { db_id = Some (Entity_id too_large); attrs = [ "name", One_value (String "Valerii") ] } ]));
  assert_raises_invalid_arg_message
    "Ref values reject entity ids above the supported range"
    (highest_supported_message too_large)
    (fun () ->
      ignore
        (empty_db ~schema:[ "ref", ref_attr ] ()
         |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "ref", One_value (Ref too_large) ] } ]))

let test_transact__test_tx_entity_ids_do_not_advance_max_eid () =
  let tx_entity_id = tx0 + 1 in
  let db =
    empty_db ()
    |> db_with [ Add (Entity_id tx_entity_id, "tx-prop", String "metadata") ]
  in
  assert_equal_int
    "explicit tx entity ids are valid but should not advance max_eid"
    0
    db.max_eid;
  let db =
    db
    |> db_with [ Entity { db_id = Some (Temp_id "next"); attrs = [ "name", One_value (String "Ivan") ] } ]
  in
  assert_equal_int "tempids should still allocate below tx0" 1 db.max_eid;
  assert_equal_triples
    "tempid allocation should start from the ordinary entity range"
    [ 1, "name", String "Ivan" ]
    (datoms db Eavt ~e:1 ~a:"name" ())

let test_init_db__test_tx_entity_ids_do_not_advance_max_eid () =
  let tx_entity_id = tx0 + 1 in
  let db =
    init_db
      [ datom ~tx:tx_entity_id ~e:tx_entity_id ~a:"tx-prop" ~v:(String "metadata") ()
      ; datom ~tx:tx_entity_id ~e:1 ~a:"created-at" ~v:(Ref tx_entity_id) ()
      ]
  in
  assert_equal_int
    "init_db should ignore tx entity ids when restoring max_eid"
    1
    db.max_eid

let test_transact__test_tempid_allocation_stops_before_tx0 () =
  let db = { (empty_db ()) with max_eid = tx0 - 1 } in
  assert_raises_invalid_arg
    "tempid allocation should not enter the transaction id range"
    (fun () ->
      ignore
        (db
         |> db_with [ Entity { db_id = Some (Temp_id "next"); attrs = [ "name", One_value (String "Ivan") ] } ]))

let test_db_with_allocates_tempids () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = None; attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = None; attrs = [ "name", One_value (String "Petr") ] }
         ]
  in
  assert_equal_triples
    "entities without ids are allocated from max entity id"
    [ 1, "name", String "Ivan"; 2, "name", String "Petr" ]
    (datoms db Eavt ())

let test_transact__test_resolve_eid () =
  let db =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "Ivan") ]
             }
         ]
    |> db_with
         [ Entity
             { db_id = Some (Temp_id "petr")
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "friend", One_value (Ref_to (Lookup_ref ("name", String "Ivan")))
                 ]
             }
         ; Add (Temp_id "ivan-referrer", "name", String "Oleg")
         ; Add (Temp_id "ivan-referrer", "friend", Ref_to (Lookup_ref ("name", String "Ivan")))
         ]
  in
  assert_equal_triples
    "tx resolves tempids and lookup refs"
    [ 1, "name", String "Ivan"
    ; 2, "friend", Ref 1
    ; 2, "name", String "Petr"
    ; 3, "friend", Ref 1
    ; 3, "name", String "Oleg"
    ]
    (datoms db Eavt ())

let test_transact__test_resolve_eid_refs () =
  let report =
    transact
      (empty_db ~schema:[ "friend", ref_many ] ())
      [ Entity
          { db_id = None
          ; attrs =
              [ "name", One_value (String "Sergey")
              ; "friend", Many_values [ Ref_to (Temp_id "ivan"); Ref_to (Temp_id "petr") ]
              ]
          }
      ; Add (Temp_id "ivan", "name", String "Ivan")
      ; Add (Temp_id "petr", "name", String "Petr")
      ]
  in
  assert_equal_triples
    "entity maps allocate their own entity before referenced tempids"
    [ 1, "friend", Ref 2
    ; 1, "friend", Ref 3
    ; 1, "name", String "Sergey"
    ; 2, "name", String "Ivan"
    ; 3, "name", String "Petr"
    ]
    (datoms report.db_after Eavt ());
  assert_equal_tempids
    "referenced tempids are allocated after the owner entity"
    [ "db/current-tx", tx0 + 1; "ivan", 2; "petr", 3 ]
    report.tempids

let test_db_ident_is_builtin_and_resolves_refs () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "db/ident", Keyword "ent1")
         ; Add (Entity_id 2, "db/ident", Keyword "ent2")
         ; Add (Ident "ent1", "ref", Ref_to (Ident "ent2"))
         ]
  in
  assert_equal_triples
    "db/ident is a built-in identity attr and idents resolve as entity refs"
    [ 1, "db/ident", Keyword "ent1"
    ; 1, "ref", Ref 2
    ; 2, "db/ident", Keyword "ent2"
    ]
    (datoms db Eavt ());
  if entid db "db/ident" (Keyword "ent1") <> Some 1 then
    failwith "entid should resolve built-in db/ident values";
  match entity db (Ident "ent1") with
  | Some entity ->
    assert_equal_tx_value
      "entity accepts ident refs"
      (Some
         (One_entity
            { db_id = Some (Entity_id 2)
            ; attrs = [ "db/ident", One_value (Keyword "ent2") ]
            }))
      (entity_attr entity "ref")
  | None -> failwith "expected ident entity ref to resolve"

let test_upstream_ident_parity_batch () =
  let db =
    empty_db ~schema:[ "ref", ref_attr ] ()
    |> db_with
         [ Add (Entity_id 1, "db/ident", Keyword "ent1")
         ; Add (Entity_id 2, "db/ident", Keyword "ent2")
         ; Add (Entity_id 2, "ref", Ref_to (Ident "ent1"))
         ]
  in
  if
    q_return_string db "[:find ?v . :where [:ent2 :ref ?v]]"
    <> Query_scalar (Some (Result_entity 1))
  then failwith "ident.cljc query should resolve ident in entity position";
  if
    q_return_string db "[:find ?f . :where [?f :ref :ent1]]"
    <> Query_scalar (Some (Result_entity 2))
  then failwith "ident.cljc query should resolve ident in ref value position";
  let db = db_with [ Add (Ident "ent1", "ref", Ref_to (Ident "ent2")) ] db in
  (match entity db (Ident "ent1") with
   | Some entity ->
     assert_equal_tx_value
       "ident.cljc transact resolves ident entity refs"
       (Some
          (One_entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "db/ident", One_value (Keyword "ent2")
                 ; "ref", One_value (Ref 1)
                 ]
             }))
       (entity_attr entity "ref")
   | None -> failwith "ident.cljc entity lookup by ident should resolve");
  match pull db [ Pull_id; Pull_attr "db/ident" ] (Ident "ent1") with
  | Some pulled ->
    assert_equal_pulled_attrs
      "ident.cljc pull resolves ident entity refs"
      [ kw "db/id", Pulled_scalar (Int 1)
      ; kw "db/ident", Pulled_scalar (Keyword "ent1")
      ]
      pulled
  | None -> failwith "ident.cljc pull by ident should resolve"

let test_entid_ref_resolves_entity_refs () =
  let db =
    empty_db ~schema:[ "email", unique_identity; "name", indexed ] ()
    |> db_with
         [ Add (Entity_id 1, "db/ident", Keyword "ent1")
         ; Add (Entity_id 1, "email", String "one@example.com")
         ; Add (Entity_id 2, "email", String "two@example.com")
         ; Add (Entity_id 2, "name", String "Two")
         ]
  in
  if entid_ref db (Entity_id 42) <> Some 42 then failwith "entid_ref should accept numeric entity ids";
  if entid_ref db (Ident "ent1") <> Some 1 then failwith "entid_ref should resolve db/ident refs";
  if entid_ref db (Ident "missing") <> None then failwith "entid_ref should return None for missing idents";
  if entid_ref db (Lookup_ref ("email", String "two@example.com")) <> Some 2 then
    failwith "entid_ref should resolve lookup refs";
  if entid_ref db (Lookup_ref ("email", String "missing@example.com")) <> None then
    failwith "entid_ref should return None for missing lookup refs";
  assert_raises_invalid_arg
    "entid_ref rejects lookup refs on non-unique attrs"
    (fun () -> ignore (entid_ref db (Lookup_ref ("name", String "Two"))))

let test_db_ident_rejects_duplicate_idents_by_default () =
  assert_raises_invalid_arg
    "db/ident is unique by default"
    (fun () ->
      ignore
        (empty_db ()
         |> db_with
              [ Add (Entity_id 1, "db/ident", Keyword "ent1")
              ; Add (Entity_id 2, "db/ident", Keyword "ent1")
              ]))

let test_transact_report_exposes_tempids () =
  let report =
    transact
      (empty_db ())
      [ Entity { db_id = Some (Temp_id "ivan"); attrs = [ "name", One_value (String "Ivan") ] }
      ; Entity { db_id = Some (Temp_id "petr"); attrs = [ "name", One_value (String "Petr") ] }
      ]
  in
  assert_equal_tempids
    "transact report should expose resolved tempids"
    [ "db/current-tx", tx0 + 1; "ivan", 1; "petr", 2 ]
    report.tempids

let test_resolve_tempid_reads_tx_report_tempids () =
  let report =
    transact
      (empty_db ())
      [ Entity { db_id = Some (Temp_id "ivan"); attrs = [ "name", One_value (String "Ivan") ] }
      ]
  in
  assert_equal_int
    "resolve_tempid finds transaction tempids"
    1
    (Option.get (resolve_tempid report.tempids "ivan"));
  assert_equal_int
    "resolve_tempid accepts a db argument for Datomic compatibility"
    1
    (Option.get (resolve_tempid ~db:report.db_after report.tempids "ivan"));
  assert_equal_int
    "resolve_tempid finds current transaction tempid"
    (tx0 + 1)
    (Option.get (resolve_tempid report.tempids "db/current-tx"));
  if resolve_tempid report.tempids "missing" <> None then
    failwith "resolve_tempid should return None for unknown tempids"

let test_transact__test_resolve_current_tx () =
  let report =
    transact
      (empty_db ())
      [ Add (CurrentTx, "prop1", String "prop1")
      ; Entity
          { db_id = Some CurrentTx
          ; attrs = [ "prop2", One_value (String "prop2") ]
          }
      ; Entity
          { db_id = Some (Temp_id "entity")
          ; attrs =
              [ "name", One_value (String "Ivan")
              ; "created-at", One_value TxRef
              ]
          }
      ]
  in
  assert_equal_triples
    "current transaction id can be used as an entity and ref value"
    [ 1, "created-at", Ref (tx0 + 1)
    ; 1, "name", String "Ivan"
    ; tx0 + 1, "prop1", String "prop1"
    ; tx0 + 1, "prop2", String "prop2"
    ]
    (datoms report.db_after Eavt ());
  assert_equal_tempids
    "current transaction id should be reported in tempids"
    [ "db/current-tx", tx0 + 1; "entity", 1 ]
    report.tempids

let test_current_tx_string_aliases_resolve_in_transactions () =
  let report =
    transact
      (empty_db ())
      [ Add (Temp_id "datomic.tx", "prop1", String "prop1")
      ; Entity
          { db_id = Some (Temp_id "datascript.tx")
          ; attrs = [ "prop2", One_value (String "prop2") ]
          }
      ; Entity
          { db_id = Some (Temp_id "entity")
          ; attrs =
              [ "name", One_value (String "Ivan")
              ; "created-at", One_value (Ref_to (Temp_id "datomic.tx"))
              ]
          }
      ]
  in
  assert_equal_triples
    "current transaction string aliases resolve as the tx entity"
    [ 1, "created-at", Ref (tx0 + 1)
    ; 1, "name", String "Ivan"
    ; tx0 + 1, "prop1", String "prop1"
    ; tx0 + 1, "prop2", String "prop2"
    ]
    (datoms report.db_after Eavt ());
  assert_equal_tempids
    "current transaction aliases should be reported in tempids"
    [ "db/current-tx", tx0 + 1
    ; "datomic.tx", tx0 + 1
    ; "datascript.tx", tx0 + 1
    ; "entity", 1
    ]
    report.tempids

let test_current_tx_string_aliases_can_be_value_only () =
  let report =
    transact
      (empty_db ())
      [ Entity
          { db_id = Some (Temp_id "entity")
          ; attrs =
              [ "name", One_value (String "Ivan")
              ; "created-at", One_value (Ref_to (Temp_id "datomic.tx"))
              ]
          }
      ]
  in
  assert_equal_triples
    "current transaction string aliases can be used only as ref values"
    [ 1, "created-at", Ref (tx0 + 1); 1, "name", String "Ivan" ]
    (datoms report.db_after Eavt ());
  assert_equal_tempids
    "value-only current transaction aliases should be reported in tempids"
    [ "db/current-tx", tx0 + 1; "datomic.tx", tx0 + 1; "entity", 1 ]
    report.tempids

let test_current_tx_colon_string_alias_resolves_in_transactions () =
  let report =
    transact
      (empty_db ())
      [ Add (Temp_id ":db/current-tx", "prop", String "prop")
      ; Entity
          { db_id = Some (Temp_id "entity")
          ; attrs = [ "created-at", One_value (Ref_to (Temp_id ":db/current-tx")) ]
          }
      ]
  in
  assert_equal_triples
    "colon string current transaction alias resolves as the tx entity"
    [ 1, "created-at", Ref (tx0 + 1); tx0 + 1, "prop", String "prop" ]
    (datoms report.db_after Eavt ());
  assert_equal_tempids
    "colon string current transaction alias should be reported in tempids"
    [ "db/current-tx", tx0 + 1; ":db/current-tx", tx0 + 1; "entity", 1 ]
    report.tempids

let test_transact_report_exposes_resolved_tx_datoms () =
  let report =
    transact
      (empty_db ())
      [ Add (Entity_id 1, "name", String "Ivan")
      ; Add (Entity_id 1, "age", Int 31)
      ]
  in
  assert_equal_datoms
    "tx_data exposes datoms with the transaction id"
    [ datom ~tx:(tx0 + 1) ~e:1 ~a:"name" ~v:(String "Ivan") ()
    ; datom ~tx:(tx0 + 1) ~e:1 ~a:"age" ~v:(Int 31) ()
    ]
    report.tx_data

let test_transact_report_exposes_tx_meta () =
  let report =
    transact
      ~tx_meta:[ "source", String "test"; "retry", Bool false ]
      (empty_db ())
      [ Add (Entity_id 1, "name", String "Ivan") ]
  in
  if report.tx_meta <> [ "source", String "test"; "retry", Bool false ] then
    failwith "transact report should expose tx meta"

let test_transact_report_exposes_cardinality_one_retractions () =
  let db = empty_db () |> db_with [ Add (Entity_id 1, "name", String "Ivan") ] in
  let report = transact db [ Add (Entity_id 1, "name", String "Petr") ] in
  assert_equal_datoms
    "tx_data exposes replacement as retract plus add"
    [ datom ~tx:(tx0 + 2) ~added:false ~e:1 ~a:"name" ~v:(String "Ivan") ()
    ; datom ~tx:(tx0 + 2) ~e:1 ~a:"name" ~v:(String "Petr") ()
    ]
    report.tx_data

let test_history_exposes_additions_and_retractions () =
  let db =
    empty_db ()
    |> db_with [ Add (Entity_id 1, "name", String "Ivan") ]
    |> db_with [ Add (Entity_id 1, "name", String "Petr") ]
  in
  let historical_db = history db in
  assert_bool "history marks db as historical" (is_history historical_db);
  assert_equal_datoms
    "history exposes add and retract datoms"
    [ datom ~tx:(tx0 + 1) ~e:1 ~a:"name" ~v:(String "Ivan") ()
    ; datom ~tx:(tx0 + 2) ~added:false ~e:1 ~a:"name" ~v:(String "Ivan") ()
    ; datom ~tx:(tx0 + 2) ~e:1 ~a:"name" ~v:(String "Petr") ()
    ]
    (datoms historical_db Eavt ());
  assert_equal_triples
    "active db still exposes only current facts"
    [ 1, "name", String "Petr" ]
    (datoms db Eavt ())

let test_no_history_schema_omits_attr_from_history () =
  let no_history = { indexed with no_history = true } in
  let db =
    empty_db ~schema:[ "name", indexed; "secret", no_history ] ()
    |> db_with [ Add (Entity_id 1, "name", String "Ivan"); Add (Entity_id 1, "secret", String "one") ]
    |> db_with [ Add (Entity_id 1, "name", String "Petr"); Add (Entity_id 1, "secret", String "two") ]
  in
  assert_equal_triples
    "active db keeps current no-history values"
    [ 1, "name", String "Petr"; 1, "secret", String "two" ]
    (datoms db Eavt ());
  assert_equal_datoms
    "history excludes db/noHistory attrs"
    [ datom ~tx:(tx0 + 1) ~e:1 ~a:"name" ~v:(String "Ivan") ()
    ; datom ~tx:(tx0 + 2) ~added:false ~e:1 ~a:"name" ~v:(String "Ivan") ()
    ; datom ~tx:(tx0 + 2) ~e:1 ~a:"name" ~v:(String "Petr") ()
    ]
    (datoms (history db) Eavt ())

let test_schema_transactions_install_no_history () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 520)
             ; attrs =
                 [ "db/ident", One_value (Keyword "secret")
                 ; "db/noHistory", One_value (Bool true)
                 ; "db/index", One_value (Bool true)
                 ]
             }
         ; Add (Entity_id 1, "secret", String "one")
         ]
    |> db_with [ Add (Entity_id 1, "secret", String "two") ]
  in
  if List.assoc_opt "secret" (schema db) <> Some { indexed with no_history = true } then
    failwith "schema transaction should install no-history attrs";
  assert_equal_triples
    "schema-installed no-history attrs stay out of history"
    []
    (datoms (history db) Eavt ~a:"secret" ())

let test_schema_transactions_install_doc () =
  let documented = { indexed with doc = Some "Display name" } in
  let db = empty_db ~schema:[ "name", documented ] () in
  if List.assoc_opt "name" (schema db) <> Some documented then
    failwith "direct schema should preserve docs";
  let db =
    db
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 521)
             ; attrs =
                 [ "db/ident", One_value (Keyword "nickname")
                 ; "db/doc", One_value (String "Casual display name")
                 ; "db/index", One_value (Bool true)
                 ]
             }
         ]
  in
  if List.assoc_opt "nickname" (schema db) <> Some { indexed with doc = Some "Casual display name" } then
    failwith "schema transaction should install db/doc"

let test_schema_transactions_install_tuple_types () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 526)
             ; attrs =
                 [ "db/ident", One_value (Keyword "name+score")
                 ; "db/valueType", One_value (Keyword "db.type/tuple")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                 ; "db/tupleTypes", Many_values [ Keyword "db.type/string"; Keyword "db.type/number" ]
                 ; "db/index", One_value (Bool true)
                 ]
             }
         ; Add (Entity_id 1, "name+score", Tuple [ Some (String "Ivan"); Some (Float 10.5) ])
         ]
  in
  if List.assoc_opt "name+score" (schema db) <> Some (typed_tuple [ StringType; NumberType ]) then
    failwith "schema transaction should install db/tupleTypes";
  assert_raises_invalid_arg
    "schema-installed tupleTypes reject mismatched values"
    (fun () ->
      ignore
        (db_with
           [ Add (Entity_id 2, "name+score", Tuple [ Some (String "Petr"); Some (String "high") ]) ]
           db))

let test_datoms_filter_by_tx_component () =
  let db =
    empty_db ()
    |> db_with [ Add (Entity_id 1, "age", Int 31) ]
    |> db_with [ Add (Entity_id 1, "age", Int 32) ]
  in
  let history_db = history db in
  assert_equal_triples
    "datoms filters by tx component"
    [ 1, "age", Int 31 ]
    (datoms history_db Eavt ~tx:(tx0 + 1) ());
  (match find_datom history_db Eavt ~tx:(tx0 + 2) () with
   | Some datom -> assert_equal_tx_value "find_datom supports tx component" (Int 31) datom.v
   | None -> failwith "expected tx datom");
  assert_equal_triples
    "seek_datoms supports tx component bounds"
    [ 1, "age", Int 32 ]
    (seek_datoms history_db Eavt ~e:1 ~a:"age" ~v:(Int 32) ~tx:(tx0 + 2) ())

let test_reverse_ref_helpers () =
  if is_reverse_ref "friend" then failwith "plain attr should not be reverse";
  if not (is_reverse_ref "_friend") then failwith "underscore attr should be reverse";
  if is_reverse_ref "user/friend" then failwith "plain namespaced attr should not be reverse";
  if not (is_reverse_ref "user/_friend") then failwith "namespaced underscore attr should be reverse";
  if reverse_ref "friend" <> "_friend" then failwith "reverse_ref should add underscore";
  if reverse_ref "_friend" <> "friend" then failwith "reverse_ref should remove underscore";
  if reverse_ref "user/friend" <> "user/_friend" then failwith "reverse_ref should preserve namespace";
  if reverse_ref "user/_friend" <> "user/friend" then failwith "reverse_ref should preserve namespace when reversing back"

let test_entity_maps_expand_reverse_attrs () =
  let db =
    empty_db ~schema:[ "friend", ref_many; "person/child", ref_many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "_friend", Many_values [ Ref 2; Ref 3 ]
                 ; "person/_child", One_value (Ref 4)
                 ]
             }
         ]
  in
  assert_equal_triples
    "entity map reverse attrs add refs from target entities to the current entity"
    [ 1, "name", String "Ivan"
    ; 2, "friend", Ref 1
    ; 3, "friend", Ref 1
    ; 4, "person/child", Ref 1
    ]
    (datoms db Eavt ());
  assert_equal_datoms
    "entity map reverse attrs are not stored under the reverse attr name"
    []
    (datoms db Aevt ~a:"_friend" ());
  assert_equal_datoms
    "namespaced entity map reverse attrs are not stored under the reverse attr name"
    []
    (datoms db Aevt ~a:"person/_child" ())

let test_entity_maps_reject_non_ref_reverse_attr_values () =
  assert_raises_invalid_arg
    "entity map reverse attrs require ref values"
    (fun () ->
      ignore
        (empty_db ~schema:[ "friend", ref_attr ] ()
         |> db_with
              [ Entity
                  { db_id = Some (Entity_id 1)
                  ; attrs = [ "_friend", One_value (String "not-a-ref") ]
                  }
              ]))

let test_entity_maps_reject_reverse_attrs_without_ref_schema () =
  assert_raises_invalid_arg
    "entity map reverse attrs require a ref schema on the forward attr"
    (fun () ->
      ignore
        (empty_db ()
         |> db_with
              [ Entity
                  { db_id = Some (Entity_id 1)
                  ; attrs = [ "_friend", One_value (Ref 2) ]
                  }
              ]))

let test_entity_maps_expand_nested_entity_values () =
  let db =
    empty_db ~schema:[ "profile", ref_attr; "child", ref_many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; ( "profile"
                   , One_entity
                       { db_id = Some (Entity_id 2)
                       ; attrs = [ "email", One_value (String "ivan@example.com") ]
                       } )
                 ; ( "child"
                   , Many_entities
                       [ { db_id = Some (Entity_id 3)
                         ; attrs = [ "name", One_value (String "David") ]
                         }
                       ; { db_id = None
                         ; attrs = [ "name", One_value (String "Thomas") ]
                         }
                       ] )
                 ]
             }
         ]
  in
  assert_equal_triples
    "entity map nested entities are transacted and linked from the owning entity"
    [ 1, "child", Ref 3
    ; 1, "child", Ref 4
    ; 1, "name", String "Ivan"
    ; 1, "profile", Ref 2
    ; 2, "email", String "ivan@example.com"
    ; 3, "name", String "David"
    ; 4, "name", String "Thomas"
    ]
    (datoms db Eavt ())

let test_entity_map_with_only_nested_ref_allocates_nested_first () =
  let db =
    empty_db ~schema:[ "profile", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = None
             ; attrs =
                 [ ( "profile"
                   , One_entity
                       { db_id = None
                       ; attrs = [ "email", One_value (String "ivan@example.com") ]
                       } )
                 ]
             }
         ]
  in
  assert_equal_triples
    "anonymous entity maps with only a nested forward ref allocate the nested entity first"
    [ 1, "email", String "ivan@example.com"
    ; 2, "profile", Ref 1
    ]
    (datoms db Eavt ())

let test_entity_maps_expand_reverse_nested_entity_values () =
  let db =
    empty_db ~schema:[ "friend", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; ( "_friend"
                   , One_entity
                       { db_id = Some (Entity_id 2)
                       ; attrs = [ "name", One_value (String "Petr") ]
                       } )
                 ]
             }
         ]
  in
  assert_equal_triples
    "entity map reverse nested entities are linked back to the owning entity"
    [ 1, "name", String "Ivan"
    ; 2, "friend", Ref 1
    ; 2, "name", String "Petr"
    ]
    (datoms db Eavt ())

let test_entity_maps_expand_many_reverse_nested_entity_values () =
  let db =
    empty_db ~schema:[ "profile", ref_many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "email", One_value (String "ivan@example.com")
                 ; ( "_profile"
                   , Many_entities
                       [ { db_id = None; attrs = [ "name", One_value (String "Ivan") ] }
                       ; { db_id = None; attrs = [ "name", One_value (String "Petr") ] }
                       ] )
                 ]
             }
         ]
  in
  assert_equal_triples
    "entity map many reverse nested entities are linked back to the owning entity"
    [ 1, "email", String "ivan@example.com"
    ; 2, "name", String "Ivan"
    ; 2, "profile", Ref 1
    ; 3, "name", String "Petr"
    ; 3, "profile", Ref 1
    ]
    (datoms db Eavt ())

let test_upstream_components_and_explode_parity_batch () =
  let component_db =
    empty_db ~schema:[ "profile", component ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "Ivan"); "profile", One_value (Ref 3) ]
             }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "email", One_value (String "@3") ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "email", One_value (String "@4") ] }
         ]
  in
  assert_equal_query
    "components.cljc retractEntity removes component children"
    []
    (q_string
       (db_with [ RetractEntity (Entity_id 1) ] component_db)
       "[:find ?a ?v :where [3 ?a ?v]]");
  assert_equal_query
    "components.cljc retractAttribute removes component values"
    []
    (q_string
       (db_with [ RetractAttr (Entity_id 1, "profile") ] component_db)
       "[:find ?a ?v :where [3 ?a ?v]]");
  assert_equal_query
    "components.cljc reverse component navigation exposes owner"
    [ [ Result_entity 1 ] ]
    (q_string component_db "[:find ?owner :where [3 :_profile ?owner]]");
  let component_many_db =
    empty_db ~schema:[ "profile", component_many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "profile", Many_values [ Ref 3; Ref 4 ]
                 ]
             }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "email", One_value (String "@3") ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "email", One_value (String "@4") ] }
         ]
  in
  assert_equal_query
    "components.cljc multival retractEntity removes all component children"
    []
    (q_string
       ~inputs:[ Arg_collection [ Result_entity 1; Result_entity 3; Result_entity 4 ] ]
       (db_with [ RetractEntity (Entity_id 1) ] component_many_db)
       "[:find ?a ?v
         :in [?e ...]
         :where [?e ?a ?v]]");
  assert_equal_query
    "components.cljc multival retractAttribute removes all component values"
    []
    (q_string
       ~inputs:[ Arg_collection [ Result_entity 3; Result_entity 4 ] ]
       (db_with [ RetractAttr (Entity_id 1, "profile") ] component_many_db)
       "[:find ?a ?v
         :in [?e ...]
         :where [?e ?a ?v]]");
  let exploded =
    empty_db ~schema:[ "aka", many; "also", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "age", One_value (Int 16)
                 ; "aka", One_value (List [ String "Devil"; String "Tupen" ])
                 ; "also", One_value (String "ok")
                 ]
             }
         ]
  in
  assert_equal_query
    "explode.cljc expands sequential cardinality-many entity values"
    [ [ Result_value (String "Devil") ]; [ Result_value (String "Tupen") ] ]
    (q_string exploded "[:find ?v :where [1 :aka ?v]]");
  assert_equal_query
    "explode.cljc preserves scalar values on cardinality-many attrs"
    [ [ Result_value (String "ok") ] ]
    (q_string exploded "[:find ?v :where [1 :also ?v]]");
  let ref_many_db =
    empty_db ~schema:[ "children", ref_many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "children", One_value (List [ Int (-2); Int (-3) ])
                 ]
             }
         ; Entity { db_id = Some (Temp_id "-2"); attrs = [ "name", One_value (String "Petr") ] }
         ; Entity { db_id = Some (Temp_id "-3"); attrs = [ "name", One_value (String "Evgeny") ] }
         ]
  in
  assert_equal_query_set
    "explode.cljc expands sequential ref-many tempids"
    [ [ Result_value (String "Petr") ]; [ Result_value (String "Evgeny") ] ]
    (q_string
       ref_many_db
       "[:find ?n
         :where [_ :children ?e]
                [?e :name ?n]]");
  let reverse_ref_db =
    empty_db ~schema:[ "children", ref_many ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "_children", One_value (Ref 1) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Evgeny"); "_children", One_value (Ref 1) ] }
         ]
  in
  assert_equal_query_set
    "explode.cljc expands reverse ref attrs into forward refs"
    [ [ Result_value (String "Petr") ]; [ Result_value (String "Evgeny") ] ]
    (q_string
       reverse_ref_db
       "[:find ?n
         :where [_ :children ?e]
                [?e :name ?n]]")

let test_init_db_preserves_uuid_and_instant_values () =
  let uuid = Uuid "65ec87fb-0000-0000-0000-000000000001" in
  let instant = Instant 1_710_000_123_456 in
  let db =
    init_db
      ~schema:[ "uuid", indexed; "created-at", indexed ]
      [ datom ~e:1 ~a:"uuid" ~v:uuid ()
      ; datom ~e:1 ~a:"created-at" ~v:instant ()
      ]
  in
  assert_equal_triples
    "init_db preserves uuid and instant values"
    [ 1, "created-at", instant; 1, "uuid", uuid ]
    (datoms db Eavt ());
  assert_equal_triples
    "init_db stores uuid and instant values in history"
    [ 1, "created-at", instant; 1, "uuid", uuid ]
    (datoms (history db) Eavt ())

let test_q_finds_values () =
  let db =
    empty_db ~schema:[ "likes", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "likes", Many_values [ String "pizza"; String "fries" ]
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "likes", Many_values [ String "pizza"; String "pie" ]
                 ]
             }
         ]
  in
  let query =
    { find = [ Find_var "value" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QWildcard, QAttr "likes", QVar "value") ]
    }
  in
  assert_equal_query
    "q returns unique sorted rows"
    [ [ Result_value (String "fries") ]
    ; [ Result_value (String "pie") ]
    ; [ Result_value (String "pizza") ]
    ]
    (q db query)

let test_parse_query_finds_values () =
  let db =
    empty_db ~schema:[ "likes", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "Ivan"); "likes", Many_values [ String "pizza"; String "fries" ] ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "Petr"); "likes", Many_values [ String "pizza"; String "pie" ] ]
             }
         ]
  in
  let map_query =
    QueryFormMap
      [ QueryFormKeyword "find", QueryFormVector [ QueryFormSymbol "?value" ]
      ; ( QueryFormKeyword "where"
        , QueryFormVector
            [ QueryFormVector
                [ QueryFormSymbol "_"; QueryFormKeyword "likes"; QueryFormSymbol "?value" ]
            ] )
      ]
  in
  assert_equal_query
    "parse_query parses map-form find/where data patterns"
    [ [ Result_value (String "fries") ]
    ; [ Result_value (String "pie") ]
    ; [ Result_value (String "pizza") ]
    ]
    (q db (parse_query map_query));
  let vector_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "likes"; QueryFormString "pizza" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ]
  in
  assert_equal_query
    "parse_query parses vector-form queries"
    [ [ Result_value (String "Ivan") ]; [ Result_value (String "Petr") ] ]
    (q db (parse_query vector_query));
  assert_raises_invalid_arg
    "parse_query rejects empty data patterns"
    (fun () ->
       ignore
         (parse_query
            (QueryFormMap
               [ QueryFormKeyword "find", QueryFormVector [ QueryFormSymbol "?value" ]
               ; QueryFormKeyword "where", QueryFormVector [ QueryFormVector [] ]
               ])))

let test_edn_reader_parses_query_and_pull_strings () =
  if
    read_edn "'[:find ?name :where [?e :name ?name]]"
    <> QueryFormVector
         [ QueryFormKeyword "find"
         ; QueryFormSymbol "?name"
         ; QueryFormKeyword "where"
         ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
         ]
  then
    failwith "read_edn should parse quoted query vectors";
  let db =
    empty_db ~schema:[ "friend", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "friend", One_value (Ref 2)
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "Petr") ]
             }
         ]
  in
  assert_equal_query
    "parse_query_string parses and executes EDN query strings"
    [ [ Result_value (String "Ivan") ]; [ Result_value (String "Petr") ] ]
    (q db (parse_query_string "'[:find ?name :where [?e :name ?name]]"));
  (match pull db (parse_pull_pattern_string db "[:name {:friend [:name]}]") (Entity_id 1) with
   | Some entity ->
     assert_equal_pulled_attrs
       "parse_pull_pattern_string parses nested EDN pull patterns"
       [ kw "friend", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ] }
       ; kw "name", Pulled_scalar (String "Ivan")
       ]
       entity
   | None -> failwith "expected EDN pull pattern to find entity")

let test_edn_string_top_level_apis () =
  let db =
    empty_db ~schema:[ "friend", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "age", One_value (Int 31)
                 ; "friend", One_value (Ref 2)
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 19) ]
             }
         ]
  in
  assert_equal_query
    "q_string parses and executes EDN query strings directly"
    [ [ Result_value (String "Ivan") ]; [ Result_value (String "Petr") ] ]
    (q_string db "[:find ?name :where [?e :name ?name]]");
  if
    q_return_string db "[:find [?name ...] :where [?e :name ?name]]"
    <> Query_collection [ Result_value (String "Ivan"); Result_value (String "Petr") ]
  then failwith "q_return_string should parse return shape and execute query";
  if
    q_return_map_string db "[:find ?name ?age :keys name age :where [?e :name ?name] [?e :age ?age]]"
    <> Query_relation_maps
         [ [ Keyword "age", Result_value (Int 31); Keyword "name", Result_value (String "Ivan") ]
         ; [ Keyword "age", Result_value (Int 19); Keyword "name", Result_value (String "Petr") ]
         ]
  then failwith "q_return_map_string should parse return map labels and execute query";
  (match pull_string db "[:name {:friend [:name]}]" (Entity_id 1) with
   | Some entity ->
     assert_equal_pulled_attrs
       "pull_string parses and executes EDN pull strings directly"
       [ kw "friend", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ] }
       ; kw "name", Pulled_scalar (String "Ivan")
       ]
       entity
   | None -> failwith "expected pull_string to find entity");
  let pulled =
    pull_many_string db "[:name]" [ Entity_id 2; Entity_id 99; Entity_id 1 ]
    |> List.map (function
      | Some entity -> Some (entity.pulled_id, List.assoc_opt (kw "name") entity.pulled_attrs)
      | None -> None)
  in
  if
    pulled
    <> [ Some (2, Some (Pulled_scalar (String "Petr")))
       ; None
       ; Some (1, Some (Pulled_scalar (String "Ivan")))
       ]
  then failwith "pull_many_string should preserve requested order and missing entities"

let test_query__test_symbol_comparison () =
  assert_equal_query
    "query.cljc test-symbol-comparison matches plain symbols in relation sources"
    [ [ Result_value (Int 2) ] ]
    (q_sources_string
       (empty_db ())
       [ ( "$"
         , Relation_source
             [ [ Result_value (Int 1); Result_attr "s"; Result_value (Symbol "a") ]
             ; [ Result_value (Int 2); Result_attr "s"; Result_value (Symbol "b") ]
             ] )
       ]
       "[:find ?e
         :in $
         :where [?e :s b]]");
  let db =
    empty_db ()
    |> db_with_string "[{:db/id 1 :s a} {:db/id 2 :s b}]"
  in
  if
    q_return_string db "[:find [?e ...] :where [?e :s b]]"
    <> Query_collection [ Result_entity 2 ]
  then failwith "q_return_string should match plain symbol constants"

let test_db_with_string_matches_upstream_validation_messages () =
  let db = empty_db ~schema:[ "profile", ref_attr; "id", unique_identity ] () in
  assert_raises_invalid_arg_message
    "db_with_string rejects invalid db/id values like upstream"
    "Expected number, string or lookup ref for :db/id"
    (fun () -> ignore (db_with_string "[{:db/id #\"\" :name \"Ivan\"}]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects nil attrs in db/add like upstream"
    "Bad entity attribute"
    (fun () -> ignore (db_with_string "[[:db/add -1 nil \"Ivan\"]]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects non-keyword attrs in db/add like upstream"
    "Bad entity attribute"
    (fun () -> ignore (db_with_string "[[:db/add -1 17 \"Ivan\"]]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects non-keyword attrs in entity maps like upstream"
    "Bad entity attribute"
    (fun () -> ignore (db_with_string "[{:db/id -1 17 \"Ivan\"}]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects nil values in db/add like upstream"
    "Cannot store nil as a value"
    (fun () -> ignore (db_with_string "[[:db/add -1 :name nil]]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects nil values in entity maps like upstream"
    "Cannot store nil as a value"
    (fun () -> ignore (db_with_string "[{:db/id -1 :name nil}]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects nil unique values like upstream"
    "Cannot store nil as a value"
    (fun () -> ignore (db_with_string "[[:db/add -1 :id nil]]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects nil unique values after tempid reuse like upstream"
    "Cannot store nil as a value"
    (fun () -> ignore (db_with_string "[{:db/id -1 :id \"A\"} {:db/id -1 :id nil}]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects nil entity ids like upstream"
    "Expected number or lookup ref for entity id"
    (fun () -> ignore (db_with_string "[[:db/add nil :name \"Ivan\"]]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects map entity ids like upstream"
    "Expected number or lookup ref for entity id"
    (fun () -> ignore (db_with_string "[[:db/add {} :name \"Ivan\"]]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects malformed ref entity ids like upstream"
    "Expected number or lookup ref for entity id"
    (fun () -> ignore (db_with_string "[[:db/add -1 :profile #\"regexp\"]]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects malformed ref entity map values like upstream"
    "Expected number or lookup ref for entity id"
    (fun () -> ignore (db_with_string "[{:db/id -1 :profile #\"regexp\"}]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects tempids in retracts like upstream"
    "Tempids are allowed in :db/add only"
    (fun () -> ignore (db_with_string "[[:db/retract -1 :name \"Ivan\"]]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects unknown operations like upstream"
    "Unknown operation"
    (fun () -> ignore (db_with_string "[[\"aaa\" :name \"Ivan\"]]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects malformed operation vectors like upstream"
    "Bad entity type at"
    (fun () -> ignore (db_with_string "[:db/add \"aaa\" :name \"Ivan\"]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects malformed transaction data like upstream"
    "Bad transaction data"
    (fun () -> ignore (db_with_string "{:profile \"aaa\"}" db))

let test_edn_reader_parses_transaction_and_schema_strings () =
  let db =
    empty_db ~schema:[ "aka", many; "friend", ref_attr ] ()
    |> db_with_string
         "[{:db/id -1
            :name \"Ivan\"
            :aka [\"Vanya\" \"Van\"]}
           [:db/add -2 :name \"Petr\"]
           [:db/add -2 :friend [:db/id -1]]]"
  in
  assert_equal_triples
    "db_with_string transacts entity maps and db/add vectors"
    [ 1, "aka", String "Van"
    ; 1, "aka", String "Vanya"
    ; 1, "name", String "Ivan"
    ; 2, "friend", Ref 1
    ; 2, "name", String "Petr"
    ]
    (datoms db Eavt ());
  assert_raises_invalid_arg
    "db_with_string rejects top-level entity maps as tx-data"
    (fun () -> ignore (empty_db () |> db_with_string "{:name \"Ivan\"}"));
  let explicit_tx = tx0 + 7 in
  let explicit_tx_db =
    db_with_string
      (Printf.sprintf "[[:db/add 3 :name \"Oleg\" %d]]" explicit_tx)
      db
  in
  assert_equal_datoms
    "db_with_string preserves explicit tx on db/add vectors"
    [ datom ~tx:explicit_tx ~e:3 ~a:"name" ~v:(String "Oleg") () ]
    (datoms explicit_tx_db Eavt ~e:3 ());
  let explicit_retract_tx = tx0 + 8 in
  let explicit_retract_db =
    db_with_string
      (Printf.sprintf "[[:db/retract 3 :name \"Oleg\" %d]]" explicit_retract_tx)
      explicit_tx_db
  in
  assert_equal_datoms
    "db_with_string preserves explicit tx on db/retract vectors in history"
    [ datom ~tx:explicit_tx ~e:3 ~a:"name" ~v:(String "Oleg") ()
    ; datom ~tx:explicit_retract_tx ~added:false ~e:3 ~a:"name" ~v:(String "Oleg") ()
    ]
    (datoms (history explicit_retract_db) Eavt ~e:3 ());
  let shorthand_db =
    empty_db ~schema:[ "aka", many ] ()
    |> db_with_string
         "[[:add 1 :name \"Short\"]
           [:add 1 :aka \"Alias\"]
           [:retract 1 :aka \"Alias\"]]"
  in
  assert_equal_triples
    "db_with_string accepts add/retract operation shorthands"
    [ 1, "name", String "Short" ]
    (datoms shorthand_db Eavt ());
  assert_equal_triples
    "db_with_string skips nil entries in transaction vectors"
    [ 4, "name", String "Nina"; 5, "name", String "Sergey" ]
    (datoms
       (empty_db ()
        |> db_with_string "[[:db/add 4 :name \"Nina\"] nil [:db/add 5 :name \"Sergey\"]]")
       Eavt
       ());
  let tagged_datom_db =
    empty_db ()
    |> db_with_string
         "[#datascript/Datom [6 :name \"Tagged\" 17 true]
           #datascript/Datom [6 :name \"Tagged\" 18 false]
           #datascript/Datom [7 :name \"Default\"]
           #datascript/Datom [8 :name \"ExplicitTx\" 19]]"
  in
  assert_equal_datoms
    "db_with_string accepts tagged datom literals in tx vectors"
    [ datom ~tx:17 ~e:6 ~a:"name" ~v:(String "Tagged") ()
    ; datom ~tx:18 ~added:false ~e:6 ~a:"name" ~v:(String "Tagged") ()
    ; datom ~e:7 ~a:"name" ~v:(String "Default") ()
    ; datom ~tx:19 ~e:8 ~a:"name" ~v:(String "ExplicitTx") ()
    ]
    (datoms (history tagged_datom_db) Eavt ());
  let ref_report =
    transact_string
      (empty_db ~schema:[ "friend", ref_many; "created-at", ref_attr ] ())
      "[[:db/add -1 :name \"Ivan\"]
        [:db/add -2 :name \"Petr\"]
        [:db/add -1 :friend -2]
        {:db/id 3 :friend 1}
        [:db/add 4 :created-at :db/current-tx]]"
  in
  assert_equal_triples
    "db_with_string resolves scalar EDN values for ref attrs"
    [ 3, "friend", Ref 1
    ; 4, "created-at", Ref (tx0 + 1)
    ; 5, "friend", Ref 6
    ; 5, "name", String "Ivan"
    ; 6, "name", String "Petr"
    ]
    (datoms ref_report.db_after Eavt ());
  assert_equal_tempids
    "db_with_string reports tempids from scalar EDN ref values"
    [ "db/current-tx", tx0 + 1; "-1", 5; "-2", 6 ]
    ref_report.tempids;
  let tx_alias_report =
    transact_string
      (empty_db ~schema:[ "created-at", ref_attr ] ())
      "[[:db/add datomic.tx :source \"datomic\"]
        {:db/id datascript.tx :kind \"datascript\"}
        [:db/add 1 :created-at datomic.tx]]"
  in
  assert_equal_triples
    "db_with_string resolves EDN current transaction aliases"
    [ 1, "created-at", Ref (tx0 + 1)
    ; tx0 + 1, "kind", String "datascript"
    ; tx0 + 1, "source", String "datomic"
    ]
    (datoms tx_alias_report.db_after Eavt ());
  assert_equal_tempids
    "db_with_string reports EDN current transaction aliases"
    [ "db/current-tx", tx0 + 1; "datomic.tx", tx0 + 1; "datascript.tx", tx0 + 1 ]
    tx_alias_report.tempids;
  let lookup_ref_entity_map_db =
    empty_db ~schema:[ "name", unique_identity; "friend", ref_attr; "tag", many ] ()
    |> db_with_string
         "[{:db/id 1 :name \"Ivan\"}
           {:db/id 2 :name \"Petr\"}
           {:db/id 1 :friend [:name \"Petr\"] :tag [:a :b]}]"
  in
  assert_equal_triples
    "db_with_string resolves lookup refs in entity map ref attrs and still expands many attrs"
    [ 1, "friend", Ref 2
    ; 1, "name", String "Ivan"
    ; 1, "tag", Keyword "a"
    ; 1, "tag", Keyword "b"
    ; 2, "name", String "Petr"
    ]
    (datoms lookup_ref_entity_map_db Eavt ());
  let reverse_lookup_ref_entity_map_db =
    empty_db ~schema:[ "name", unique_identity; "friend", ref_attr ] ()
    |> db_with_string
         "[{:db/id 1 :name \"Ivan\"}
           {:db/id 2 :name \"Petr\"}
           {:db/id 2 :_friend [:name \"Ivan\"]}]"
  in
  assert_equal_triples
    "db_with_string resolves lookup refs in entity map reverse ref attrs"
    [ 1, "friend", Ref 2; 1, "name", String "Ivan"; 2, "name", String "Petr" ]
    (datoms reverse_lookup_ref_entity_map_db Eavt ());
  assert_raises_invalid_arg_message
    "db_with_string reports unresolved entity-map lookup refs like upstream"
    "Nothing found for entity id [:name \"Oleg\"]"
    (fun () ->
      ignore
        (empty_db ~schema:[ "name", unique_identity ] ()
         |> db_with_string
              "[{:db/id 1 :name \"Ivan\"}
                {:db/id [:name \"Oleg\"] :age 10}]"));
  assert_raises_invalid_arg_message
    "db_with_string reports unresolved db/add lookup refs like upstream"
    "Nothing found for entity id [:name \"Oleg\"]"
    (fun () ->
      ignore
        (empty_db ~schema:[ "name", unique_identity ] ()
         |> db_with_string
              "[{:db/id 1 :name \"Ivan\"}
                [:db/add [:name \"Oleg\"] :age 10]]"));
  let set_nested_entity_db =
    empty_db ~schema:[ "profile", ref_many ] ()
    |> db_with_string
         "[{:db/id 1 :name \"Ivan\" :profile #{{:email \"ivan@example.com\"}}}]"
  in
  assert_equal_triples
    "db_with_string expands EDN set values containing nested entity maps"
    [ 1, "name", String "Ivan"
    ; 1, "profile", Ref 2
    ; 2, "email", String "ivan@example.com"
    ]
    (datoms set_nested_entity_db Eavt ());
  let mixed_nested_ref_db =
    empty_db ~schema:[ "email", unique_identity; "profile", ref_many ] ()
    |> db_with_string
         "[{:db/id 1 :name \"Ivan\"}
           {:db/id 2 :email \"existing@example.com\"}
           {:db/id 1
            :profile [{:email \"new@example.com\"}
                      [:email \"existing@example.com\"]]}]"
  in
  assert_equal_triples
    "db_with_string expands EDN collections mixing nested entity maps and refs"
    [ 1, "name", String "Ivan"
    ; 1, "profile", Ref 2
    ; 1, "profile", Ref 3
    ; 2, "email", String "existing@example.com"
    ; 3, "email", String "new@example.com"
    ]
    (datoms mixed_nested_ref_db Eavt ());
  let namespaced_map_schema =
    schema_of_edn_string
      "#:person{:profile #:db{:valueType :db.type/ref
                              :cardinality :db.cardinality/many}}"
  in
  if List.assoc_opt "person/profile" namespaced_map_schema <> Some ref_many then
    failwith "schema_of_edn_string should parse EDN namespaced maps";
  let namespaced_map_db =
    empty_db ~schema:(("person/email", unique_identity) :: namespaced_map_schema) ()
    |> db_with_string
         "[#:person{:db/id 1
                    :name \"Ivan\"
                    :profile [#:person{:email \"new@example.com\"}]}
           #:person{:db/id 2
                    :email \"existing@example.com\"}
           #:person{:db/id 1
                    :profile [:person/email \"existing@example.com\"]}]"
  in
  assert_equal_triples
    "db_with_string parses EDN namespaced maps in transactions"
    [ 1, "person/name", String "Ivan"
    ; 1, "person/profile", Ref 2
    ; 1, "person/profile", Ref 3
    ; 2, "person/email", String "existing@example.com"
    ; 3, "person/email", String "new@example.com"
    ]
    (datoms namespaced_map_db Eavt ());
  let special_float_db =
    empty_db ()
    |> db_with_string
         "[[:db/add 1 :nan ##NaN]
           [:db/add 1 :inf ##Inf]
           [:db/add 1 :ninf ##-Inf]]"
  in
  let special_float attr =
    match datoms special_float_db Eavt ~e:1 ~a:attr () with
    | [ { v = Float value; _ } ] -> value
    | _ -> failf "db_with_string should parse %s as a float" attr
  in
  if classify_float (special_float "nan") <> FP_nan then
    failwith "db_with_string should parse ##NaN";
  if special_float "inf" <> Float.infinity then
    failwith "db_with_string should parse ##Inf";
  if special_float "ninf" <> Float.neg_infinity then
    failwith "db_with_string should parse ##-Inf";
  let retracted =
    db_with_string "[[:db/retract 1 :aka \"Van\"]]" db
  in
  assert_equal_triples
    "db_with_string transacts db/retract vectors"
    [ 1, "aka", String "Vanya"
    ; 1, "name", String "Ivan"
    ; 2, "friend", Ref 1
    ; 2, "name", String "Petr"
    ]
    (datoms retracted Eavt ());
  let cas_db =
    db_with_string "[[:db/cas 2 :name \"Petr\" \"Pyotr\"]]" retracted
  in
  assert_equal_triples
    "db_with_string transacts db/cas operation vectors"
    [ 1, "aka", String "Vanya"
    ; 1, "name", String "Ivan"
    ; 2, "friend", Ref 1
    ; 2, "name", String "Pyotr"
    ]
    (datoms cas_db Eavt ());
  assert_raises_invalid_arg
    "db_with_string db/cas fails when the expected value does not match"
    (fun () -> ignore (db_with_string "[[:db/cas 2 :name \"Petr\" \"Pavel\"]]" cas_db));
  let retract_attr_db =
    db_with_string "[[:db.fn/retractAttribute 1 :aka]]" cas_db
  in
  assert_equal_triples
    "db_with_string transacts db.fn/retractAttribute"
    [ 1, "name", String "Ivan"; 2, "friend", Ref 1; 2, "name", String "Pyotr" ]
    (datoms retract_attr_db Eavt ());
  let retract_entity_db =
    db_with_string "[[:db.fn/retractEntity 1]]" retract_attr_db
  in
  assert_equal_triples
    "db_with_string transacts db.fn/retractEntity and removes incoming refs"
    [ 2, "name", String "Pyotr" ]
    (datoms retract_entity_db Eavt ());
  let parsed_schema =
    schema_of_edn_string
      "{:aka {:db/cardinality :db.cardinality/many
              :db/index true}
        :friend {:db/valueType :db.type/ref
                 :db/cardinality :db.cardinality/one
                 :db/isComponent true}
        :email {:db/unique :db.unique/identity}
        :legacy {:db/cardinality :db.cardinality/many
                 :db/tupleType :db.type/string}
        :installed {:db/valueType :db.type/ref
                    :db.install/_attribute :db.part/db}}"
  in
  if List.assoc_opt "aka" parsed_schema <> Some { many with indexed = true } then
    failwith "schema_of_edn_string should parse cardinality and index";
  if List.assoc_opt "friend" parsed_schema <> Some { ref_attr with is_component = true } then
    failwith "schema_of_edn_string should parse ref component attrs";
  if List.assoc_opt "email" parsed_schema <> Some unique_identity then
    failwith "schema_of_edn_string should parse unique identity attrs";
  if List.assoc_opt "legacy" parsed_schema <> Some many then
    failwith "schema_of_edn_string should ignore compatible db/tupleType";
  if List.assoc_opt "installed" parsed_schema <> Some ref_attr then
    failwith "schema_of_edn_string should ignore db.install/_attribute";
  let schema_db =
    empty_db ~schema:parsed_schema ()
    |> db_with_string
         "[{:db/id 10
            :aka [\"main\" \"alt\"]
            :email \"ivan@example.com\"
            :friend {:name \"Nested\"}}]"
  in
  assert_equal_triples
    "schema parsed from EDN controls later EDN transactions"
    [ 10, "aka", String "alt"
    ; 10, "aka", String "main"
    ; 10, "email", String "ivan@example.com"
    ; 10, "friend", Ref 11
    ; 11, "name", String "Nested"
    ]
    (datoms schema_db Eavt ());
  let collection_value_db =
    empty_db ~schema:[ "path", indexed; "aka", many; "friend", ref_many ] ()
    |> db_with_string
         "[{:db/id 1
            :path [1 2]
            :tags #{:red :blue}
            :aka [\"main\" \"alt\"]}
           {:db/id 2 :_friend [1]}]"
  in
  assert_equal_triples
    "db_with_string stores collection values on cardinality-one attrs"
    [ 1, "path", Vector [ Int 1; Int 2 ]; 1, "tags", Set [ Keyword "blue"; Keyword "red" ] ]
    (datoms collection_value_db Eavt ~e:1 ~a:"path" ()
     @ datoms collection_value_db Eavt ~e:1 ~a:"tags" ());
  assert_equal_triples
    "db_with_string still expands collection values for many and reverse attrs"
    [ 1, "aka", String "alt"
    ; 1, "aka", String "main"
    ; 1, "friend", Ref 2
    ]
    (datoms collection_value_db Eavt ~e:1 ~a:"aka" ()
     @ datoms collection_value_db Eavt ~e:1 ~a:"friend" ())

let test_edn_reader_parses_common_literals () =
  (match read_edn "\"left\\bright\\fform\\u0020feed\\u0041\"" with
   | QueryFormString value ->
     let expected =
       "left"
       ^ String.make 1 (Char.chr 8)
       ^ "right"
       ^ String.make 1 (Char.chr 12)
       ^ "form feedA"
     in
     if value <> expected then failwith "read_edn should parse EDN string escape sequences"
   | _ -> failwith "read_edn should parse escaped EDN strings");
  assert_raises_invalid_arg
    "read_edn rejects incomplete unicode string escapes"
    (fun () -> ignore (read_edn "\"bad\\u12\""));
  let db =
    empty_db
      ~schema:
        [ "tags", many
        ; "uuid", { indexed with value_type = Some UuidType }
        ; "created-at", { indexed with value_type = Some InstantType }
        ]
      ()
    |> db_with_string
         "[{:db/id 1
            :tags #{:admin :user}
            :pattern #\"[a-z]+[0-9]+\"
            :uuid #uuid \"65ec87fb-0000-0000-0000-000000000001\"
            :created-at #inst \"2024-03-09T16:02:03.456Z\"}
           {:db/id 2
            :created-at #inst \"2024-03-09T17:02:03.456+01:00\"}
           {:db/id 3
            :created-at #inst \"2024-03-09T13:32:03.456-02:30\"}
           {:db/id 4
            :created-at #inst \"2024-03-09T17:02:03.456+0100\"}]"
  in
  assert_equal_triples
    "db_with_string parses EDN set, regex, uuid, and instant literals"
    [ 1, "created-at", Instant 1_710_000_123_456
    ; 1, "pattern", Regex "[a-z]+[0-9]+"
    ; 1, "tags", Keyword "admin"
    ; 1, "tags", Keyword "user"
    ; 1, "uuid", Uuid "65ec87fb-0000-0000-0000-000000000001"
    ; 2, "created-at", Instant 1_710_000_123_456
    ; 3, "created-at", Instant 1_710_000_123_456
    ; 4, "created-at", Instant 1_710_000_123_456
    ]
    (datoms db Eavt ());
  assert_equal_query
    "parse_query_string accepts regex literals in query constants"
    [ [ Result_value (String "abc123") ] ]
    (q
       db
       (parse_query_string
          "[:find ?match
            :where [_ :pattern ?pattern]
                   [(re-find #\"[a-z]+[0-9]+\" \"abc123\") ?match]]"))

let test_edn_reader_ignores_discard_and_metadata () =
  if
    read_edn "#_ :discarded ^:query [:find ?name :where #_ [?e :skip ?value] [?e :name ?name]]"
    <> QueryFormVector
         [ QueryFormKeyword "find"
         ; QueryFormSymbol "?name"
         ; QueryFormKeyword "where"
         ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
         ]
  then failwith "read_edn should ignore discard forms and metadata";
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "Petr"); "skip", One_value (String "ignored") ]
             }
         ]
  in
  assert_equal_query
    "q_string ignores EDN discard forms and metadata in query strings"
    [ [ Result_value (String "Ivan") ]; [ Result_value (String "Petr") ] ]
    (q_string
       db
       "^:datascript/query
        [:find ?name
         :where #_ [?e :skip ?value]
                [?e :name ?name]]");
  assert_equal_triples
    "db_with_string ignores EDN discard forms and metadata in tx strings"
    [ 1, "name", String "Kept" ]
    (datoms
       (empty_db ()
        |> db_with_string "^:tx [#_ [:db/add 1 :name \"Discarded\"] [:db/add 1 :name \"Kept\"]]")
       Eavt
       ~a:"name"
       ())

let test_parse_query_comparison_predicates () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 1, "age", Int 31)
         ; Add (Entity_id 2, "name", String "Petr")
         ; Add (Entity_id 2, "age", Int 17)
         ]
  in
  let query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormSymbol "?age" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol ">"; QueryFormSymbol "?age"; QueryFormInt 18 ] ]
      ]
  in
  assert_equal_query
    "parse_query parses comparison predicate expressions"
    [ [ Result_value (String "Ivan") ] ]
    (q db (parse_query query));
  let vector_call_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormSymbol "?age" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector
          [ QueryFormVector [ QueryFormSymbol ">"; QueryFormSymbol "?age"; QueryFormInt 18 ] ]
      ]
  in
  assert_equal_query
    "parse_query parses vector-form comparison predicate calls"
    [ [ Result_value (String "Ivan") ] ]
    (q db (parse_query vector_call_query));
  assert_raises_invalid_arg_message
    "parse_query validates vector-form predicate arity"
    "predicate requires one argument: empty?"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?name"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
               ; QueryFormVector
                   [ QueryFormVector
                       [ QueryFormSymbol "empty?"; QueryFormSymbol "?name"; QueryFormString "extra" ]
                   ]
               ])))

let test_parse_query_equality_predicates () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 2, "name", String "Petr")
         ; Add (Entity_id 3, "name", String "Oleg")
         ]
  in
  let equal_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "_"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "="; QueryFormSymbol "?name"; QueryFormString "Ivan" ] ]
      ]
  in
  assert_equal_query
    "parse_query parses equality predicate expressions"
    [ [ Result_value (String "Ivan") ] ]
    (q db (parse_query equal_query));
  let not_equal_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "_"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "!="; QueryFormSymbol "?name"; QueryFormString "Petr" ] ]
      ]
  in
  assert_equal_query
    "parse_query parses inequality predicate aliases"
    [ [ Result_value (String "Ivan") ]; [ Result_value (String "Oleg") ] ]
    (q db (parse_query not_equal_query))

let test_parse_query_arithmetic_functions () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "left", Int 2)
         ; Add (Entity_id 1, "right", Int 5)
         ]
  in
  let query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?sum"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "left"; QueryFormSymbol "?left" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "right"; QueryFormSymbol "?right" ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "+"; QueryFormSymbol "?left"; QueryFormSymbol "?right" ]
          ; QueryFormSymbol "?sum"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses arithmetic function expressions"
    [ [ Result_value (Int 7) ] ]
    (q db (parse_query query));
  let vector_call_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?sum"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "left"; QueryFormSymbol "?left" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "right"; QueryFormSymbol "?right" ]
      ; QueryFormVector
          [ QueryFormVector
              [ QueryFormSymbol "+"; QueryFormSymbol "?left"; QueryFormSymbol "?right" ]
          ; QueryFormSymbol "?sum"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses vector-form arithmetic function calls"
    [ [ Result_value (Int 7) ] ]
    (q db (parse_query vector_call_query))

let test_parse_query_transaction_patterns () =
  let current_db =
    empty_db ()
    |> db_with [ Add (Entity_id 1, "name", String "Ivan") ]
  in
  let tx_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormSymbol "?tx"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormSymbol "?e"
          ; QueryFormKeyword "name"
          ; QueryFormSymbol "?name"
          ; QueryFormSymbol "?tx"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses four-term datom patterns"
    [ [ Result_value (String "Ivan"); Result_entity (tx0 + 1) ] ]
    (q current_db (parse_query tx_query));
  let history_db =
    current_db
    |> db_with [ Retract (Entity_id 1, "name", Some (String "Ivan")) ]
    |> history
  in
  let op_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?tx"
      ; QueryFormSymbol "?op"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormInt 1
          ; QueryFormKeyword "name"
          ; QueryFormString "Ivan"
          ; QueryFormSymbol "?tx"
          ; QueryFormSymbol "?op"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses five-term history datom patterns"
    [ [ Result_entity (tx0 + 1); Result_value (Keyword "db/add") ]
    ; [ Result_entity (tx0 + 2); Result_value (Keyword "db/retract") ]
    ]
    (q history_db (parse_query op_query))

let test_parse_query_source_qualified_patterns () =
  let names =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 1, "email", String "ivan@example.com")
         ; Add (Entity_id 2, "name", String "Petr")
         ; Add (Entity_id 2, "email", String "petr@example.com")
         ]
  in
  let scores =
    empty_db ()
    |> db_with
         [ Add (Entity_id 10, "email", String "ivan@example.com")
         ; Add (Entity_id 10, "score", Int 7)
         ; Add (Entity_id 11, "email", String "olga@example.com")
         ; Add (Entity_id 11, "score", Int 9)
         ]
  in
  let source_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormSymbol "?score"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$scores"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector [ QueryFormSymbol "$"; QueryFormSymbol "?e"; QueryFormKeyword "email"; QueryFormSymbol "?email" ]
      ; QueryFormVector [ QueryFormSymbol "$scores"; QueryFormSymbol "?s"; QueryFormKeyword "email"; QueryFormSymbol "?email" ]
      ; QueryFormVector [ QueryFormSymbol "$scores"; QueryFormSymbol "?s"; QueryFormKeyword "score"; QueryFormSymbol "?score" ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified database patterns"
    [ [ Result_value (String "Ivan"); Result_value (Int 7) ] ]
    (q_sources names [ "scores", Db_source scores ] (parse_query source_query));
  let source_call_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormSymbol "?next"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$scores"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector [ QueryFormSymbol "$"; QueryFormSymbol "?e"; QueryFormKeyword "email"; QueryFormSymbol "?email" ]
      ; QueryFormVector [ QueryFormSymbol "$scores"; QueryFormSymbol "?s"; QueryFormKeyword "email"; QueryFormSymbol "?email" ]
      ; QueryFormVector [ QueryFormSymbol "$scores"; QueryFormSymbol "?s"; QueryFormKeyword "score"; QueryFormSymbol "?score" ]
      ; QueryFormVector
          [ QueryFormSymbol "$scores"
          ; QueryFormVector [ QueryFormSymbol ">"; QueryFormSymbol "?score"; QueryFormInt 6 ]
          ]
      ; QueryFormVector
          [ QueryFormSymbol "$scores"
          ; QueryFormVector [ QueryFormSymbol "+"; QueryFormSymbol "?score"; QueryFormInt 1 ]
          ; QueryFormSymbol "?next"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified predicate and function clauses"
    [ [ Result_value (String "Ivan"); Result_value (Int 8) ] ]
    (q_sources names [ "scores", Db_source scores ] (parse_query source_call_query));
  assert_raises_invalid_arg
    "parse_query rejects undeclared sources on source-qualified predicates"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?name"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
               ; QueryFormVector
                   [ QueryFormSymbol "$scores"
                   ; QueryFormVector [ QueryFormSymbol "string?"; QueryFormSymbol "?name" ]
                   ]
               ])));
  let emails =
    [ [ Result_value (String "Ivan"); Result_value (String "ivan@example.com") ]
    ; [ Result_value (String "Petr"); Result_value (String "petr@example.com") ]
    ]
  in
  let relation_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormSymbol "?email"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$emails"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "_"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector [ QueryFormSymbol "$emails"; QueryFormSymbol "?name"; QueryFormSymbol "?email" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "email"; QueryFormSymbol "?email" ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified relation patterns"
    [ [ Result_entity 1; Result_value (String "ivan@example.com") ]
    ; [ Result_entity 2; Result_value (String "petr@example.com") ]
    ]
    (q_sources names [ "emails", Relation_source emails ] (parse_query relation_query))

let test_parse_query_find_pull_expressions () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 1, "age", Int 31)
         ; Add (Entity_id 2, "name", String "Petr")
         ; Add (Entity_id 2, "age", Int 22)
         ]
  in
  let query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormList
          [ QueryFormSymbol "pull"
          ; QueryFormSymbol "?e"
          ; QueryFormVector [ QueryFormKeyword "name" ]
          ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
      ]
  in
  assert_equal_query
    "parse_query parses pull find expressions"
    [ [ Result_pull { pulled_id = 1; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Ivan") ] } ] ]
    (q db (parse_query query));
  let vector_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormVector
          [ QueryFormSymbol "pull"
          ; QueryFormSymbol "?e"
          ; QueryFormVector [ QueryFormKeyword "name" ]
          ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
      ]
  in
  assert_equal_query
    "parse_query parses vector-form pull find expressions"
    [ [ Result_pull { pulled_id = 1; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Ivan") ] } ] ]
    (q db (parse_query vector_query));
  let dynamic_pattern_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormList
          [ QueryFormSymbol "pull"
          ; QueryFormSymbol "?e"
          ; QueryFormSymbol "?pattern"
          ]
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "?pattern"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
      ]
  in
  assert_equal_query
    "parse_query supports dynamic pull find patterns"
    [ [ Result_pull { pulled_id = 1; pulled_attrs = [ Keyword "age", Pulled_scalar (Int 31) ] } ] ]
    (q
       ~inputs:[ Arg_scalar (Result_value (List [ Keyword "age" ])) ]
       db
       (parse_query dynamic_pattern_query));
  let plain_dynamic_pattern_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormList
          [ QueryFormSymbol "pull"
          ; QueryFormSymbol "?e"
          ; QueryFormSymbol "pattern"
          ]
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "pattern"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
      ]
  in
  assert_equal_query
    "parse_query supports plain-symbol dynamic pull pattern inputs"
    [ [ Result_pull { pulled_id = 1; pulled_attrs = [ Keyword "age", Pulled_scalar (Int 31) ] } ] ]
    (q
       ~inputs:[ Arg_scalar (Result_value (List [ Keyword "age" ])) ]
       db
       (parse_query plain_dynamic_pattern_query));
  let people =
    empty_db ()
    |> db_with [ Add (Entity_id 10, "name", String "Oleg") ]
  in
  let source_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormList
          [ QueryFormSymbol "pull"
          ; QueryFormSymbol "$people"
          ; QueryFormSymbol "?e"
          ; QueryFormVector [ QueryFormKeyword "name" ]
          ]
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$people"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$people"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Oleg" ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified pull find expressions"
    [ [ Result_pull { pulled_id = 10; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Oleg") ] } ] ]
    (q_sources db [ "people", Db_source people ] (parse_query source_query));
  let default_source_pull_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormList
          [ QueryFormSymbol "pull"
          ; QueryFormSymbol "?e"
          ; QueryFormVector [ QueryFormKeyword "name" ]
          ]
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Oleg" ]
      ]
  in
  assert_equal_query
    "parse_query pull find expressions use the overridden default source"
    [ [ Result_pull { pulled_id = 10; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Oleg") ] } ] ]
    (q_sources (empty_db ()) [ "$", Db_source people ] (parse_query default_source_pull_query));
  let default_source_dynamic_pull_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormList
          [ QueryFormSymbol "pull"
          ; QueryFormSymbol "?e"
          ; QueryFormSymbol "?pattern"
          ]
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "?pattern"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Oleg" ]
      ]
  in
  assert_equal_query
    "parse_query dynamic pull find expressions use the overridden default source"
    [ [ Result_pull { pulled_id = 10; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Oleg") ] } ] ]
    (q_sources
       ~inputs:[ Arg_scalar (Result_value (List [ Keyword "name" ])) ]
       (empty_db ())
       [ "$", Db_source people ]
       (parse_query default_source_dynamic_pull_query));
  let lookup_db =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Petr")
         ; Add (Entity_id 1, "age", Int 44)
         ; Add (Entity_id 2, "name", String "Ivan")
         ; Add (Entity_id 2, "age", Int 25)
         ; Add (Entity_id 3, "name", String "Oleg")
         ; Add (Entity_id 3, "age", Int 11)
         ]
  in
  let lookup_ref_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?ref"
      ; QueryFormSymbol "?age"
      ; QueryFormList
          [ QueryFormSymbol "pull"
          ; QueryFormSymbol "?ref"
          ; QueryFormVector [ QueryFormKeyword "db/id"; QueryFormKeyword "name" ]
          ]
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormVector [ QueryFormSymbol "?ref"; QueryFormSymbol "..." ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?ref"; QueryFormKeyword "age"; QueryFormSymbol "?age" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol ">="; QueryFormSymbol "?age"; QueryFormInt 18 ] ]
      ]
  in
  assert_equal_query
    "parse_query resolves lookup-ref collection inputs in pull find expressions"
    [ [ Result_value (List [ Keyword "name"; String "Ivan" ])
      ; Result_value (Int 25)
      ; Result_pull
          { pulled_id = 2
          ; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2); kw "name", Pulled_scalar (String "Ivan") ]
          }
      ]
    ; [ Result_value (List [ Keyword "name"; String "Petr" ])
      ; Result_value (Int 44)
      ; Result_pull
          { pulled_id = 1
          ; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 1); kw "name", Pulled_scalar (String "Petr") ]
          }
      ]
    ]
    (q
       ~inputs:
         [ Arg_collection
             [ Result_value (List [ Keyword "name"; String "Ivan" ])
             ; Result_value (List [ Keyword "name"; String "Oleg" ])
             ; Result_value (List [ Keyword "name"; String "Petr" ])
             ]
         ]
       lookup_db
       (parse_query lookup_ref_query))

let test_parse_query_missing_and_get_else_clauses () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 1, "height", Int 180)
         ; Add (Entity_id 2, "name", String "Petr")
         ]
  in
  let missing_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "missing?"
              ; QueryFormSymbol "?e"
              ; QueryFormKeyword "height"
              ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses missing? clauses"
    [ [ Result_value (String "Petr") ] ]
    (q db (parse_query missing_query));
  let get_else_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormSymbol "?height"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "get-else"
              ; QueryFormSymbol "?e"
              ; QueryFormKeyword "height"
              ; QueryFormString "Unknown"
              ]
          ; QueryFormSymbol "?height"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses get-else clauses"
    [ [ Result_value (String "Ivan"); Result_value (Int 180) ]
    ; [ Result_value (String "Petr"); Result_value (String "Unknown") ]
    ]
    (q db (parse_query get_else_query));
  let nil_default_get_else_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?height"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "get-else"
              ; QueryFormInt 2
              ; QueryFormKeyword "height"
              ; QueryFormNil
              ]
          ; QueryFormSymbol "?height"
          ]
      ]
  in
  assert_raises_invalid_arg
    "parse_query get-else rejects nil defaults"
    (fun () -> ignore (q db (parse_query nil_default_get_else_query)));
  let people =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 2, "name", String "Petr")
         ; Add (Entity_id 2, "height", Int 175)
         ]
  in
  let source_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormSymbol "?height"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$people"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$people"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "missing?"
              ; QueryFormSymbol "$people"
              ; QueryFormSymbol "?e"
              ; QueryFormKeyword "weight"
              ]
          ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "get-else"
              ; QueryFormSymbol "$people"
              ; QueryFormSymbol "?e"
              ; QueryFormKeyword "height"
              ; QueryFormString "Unknown"
              ]
          ; QueryFormSymbol "?height"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified missing? and get-else clauses"
    [ [ Result_value (String "Ivan"); Result_value (String "Unknown") ]
    ; [ Result_value (String "Petr"); Result_value (Int 175) ]
    ]
    (q_sources db [ "people", Db_source people ] (parse_query source_query))

let test_parse_query_get_some_and_get_clauses () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 1, "age", Int 15)
         ; Add (Entity_id 2, "name", String "Petr")
         ; Add (Entity_id 2, "age", Int 22)
         ; Add (Entity_id 2, "height", Int 240)
         ]
  in
  let get_some_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormSymbol "?attr"
      ; QueryFormSymbol "?value"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "_" ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "get-some"
              ; QueryFormSymbol "$"
              ; QueryFormSymbol "?e"
              ; QueryFormKeyword "height"
              ; QueryFormKeyword "age"
              ]
          ; QueryFormVector [ QueryFormSymbol "?attr"; QueryFormSymbol "?value" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses get-some clauses with tuple outputs"
    [ [ Result_entity 1; Result_attr "age"; Result_value (Int 15) ]
    ; [ Result_entity 2; Result_attr "height"; Result_value (Int 240) ]
    ]
    (q db (parse_query get_some_query));
  let source_db =
    empty_db ()
    |> db_with [ Add (Entity_id 10, "name", String "Oleg"); Add (Entity_id 10, "age", Int 37) ]
  in
  let source_get_some_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?attr"
      ; QueryFormSymbol "?value"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$people"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$people"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Oleg" ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "get-some"
              ; QueryFormSymbol "$people"
              ; QueryFormSymbol "?e"
              ; QueryFormKeyword "height"
              ; QueryFormKeyword "age"
              ]
          ; QueryFormVector [ QueryFormSymbol "?attr"; QueryFormSymbol "?value" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified get-some clauses"
    [ [ Result_attr "age"; Result_value (Int 37) ] ]
    (q_sources db [ "people", Db_source source_db ] (parse_query source_get_some_query));
  let get_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?value"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "get"
              ; QueryFormMap [ QueryFormKeyword "a", QueryFormInt 1; QueryFormKeyword "b", QueryFormInt 2 ]
              ; QueryFormKeyword "b"
              ]
          ; QueryFormSymbol "?value"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses get clauses"
    [ [ Result_value (Int 2) ] ]
    (q db (parse_query get_query));
  assert_raises_invalid_arg
    "parse_query rejects get-some without attributes"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?value"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormList [ QueryFormSymbol "get-some"; QueryFormSymbol "?e" ]
                   ; QueryFormVector [ QueryFormSymbol "?attr"; QueryFormSymbol "?value" ]
                   ]
               ])))

let test_parse_query_collection_value_clauses () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "label", String "empty")
         ; Add (Entity_id 1, "items", List [])
         ; Add (Entity_id 2, "label", String "full")
         ; Add (Entity_id 2, "items", List [ Keyword "a"; Keyword "b" ])
         ]
  in
  let count_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?label"
      ; QueryFormSymbol "?count"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "items"; QueryFormSymbol "?items" ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "count"; QueryFormSymbol "?items" ]
          ; QueryFormSymbol "?count"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses count clauses"
    [ [ Result_value (String "empty"); Result_value (Int 0) ]
    ; [ Result_value (String "full"); Result_value (Int 2) ]
    ]
    (q db (parse_query count_query));
  let empty_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?label"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "items"; QueryFormSymbol "?items" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "empty?"; QueryFormSymbol "?items" ] ]
      ]
  in
  assert_equal_query
    "parse_query parses empty? clauses"
    [ [ Result_value (String "empty") ] ]
    (q db (parse_query empty_query));
  let not_empty_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?label"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "items"; QueryFormSymbol "?items" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "not-empty"; QueryFormSymbol "?items" ] ]
      ]
  in
  assert_equal_query
    "parse_query parses not-empty clauses"
    [ [ Result_value (String "full") ] ]
    (q db (parse_query not_empty_query));
  let contains_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?label"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "items"; QueryFormSymbol "?items" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "contains?"; QueryFormSymbol "?items"; QueryFormInt 1 ] ]
      ]
  in
  assert_equal_query
    "parse_query parses contains? clauses"
    [ [ Result_value (String "full") ] ]
    (q db (parse_query contains_query));
  let get_default_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?present"
      ; QueryFormSymbol "?fallback"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "get"
              ; QueryFormMap [ QueryFormKeyword "answer", QueryFormInt 42 ]
              ; QueryFormKeyword "answer"
              ; QueryFormString "missing"
              ]
          ; QueryFormSymbol "?present"
          ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "get"
              ; QueryFormMap [ QueryFormKeyword "answer", QueryFormInt 42 ]
              ; QueryFormKeyword "missing"
              ; QueryFormString "fallback"
              ]
          ; QueryFormSymbol "?fallback"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses get default clauses"
    [ [ Result_value (Int 42); Result_value (String "fallback") ] ]
    (q db (parse_query get_default_query))

let test_parse_query_type_and_numeric_predicates () =
  let type_db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "label", String "bool")
         ; Add (Entity_id 1, "value", Bool true)
         ; Add (Entity_id 2, "label", String "float")
         ; Add (Entity_id 2, "value", Float 2.5)
         ; Add (Entity_id 3, "label", String "int")
         ; Add (Entity_id 3, "value", Int 1)
         ; Add (Entity_id 4, "label", String "keyword")
         ; Add (Entity_id 4, "value", Keyword "user/name")
         ; Add (Entity_id 5, "label", String "string")
         ; Add (Entity_id 5, "value", String "Ivan")
         ]
  in
  let labels_with_predicate predicate =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?label"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "value"; QueryFormSymbol "?value" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol predicate; QueryFormSymbol "?value" ] ]
      ]
  in
  assert_equal_query
    "parse_query parses number? predicates"
    [ [ Result_value (String "float") ]; [ Result_value (String "int") ] ]
    (q type_db (parse_query (labels_with_predicate "number?")));
  assert_equal_query
    "parse_query parses integer? predicates"
    [ [ Result_value (String "int") ] ]
    (q type_db (parse_query (labels_with_predicate "integer?")));
  assert_equal_query
    "parse_query parses string? predicates"
    [ [ Result_value (String "string") ] ]
    (q type_db (parse_query (labels_with_predicate "string?")));
  assert_equal_query
    "parse_query parses boolean? predicates"
    [ [ Result_value (String "bool") ] ]
    (q type_db (parse_query (labels_with_predicate "boolean?")));
  assert_equal_query
    "parse_query parses keyword? predicates"
    [ [ Result_value (String "keyword") ] ]
    (q type_db (parse_query (labels_with_predicate "keyword?")));
  let numeric_db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "label", String "float-positive")
         ; Add (Entity_id 1, "value", Float 1.5)
         ; Add (Entity_id 2, "label", String "float-zero")
         ; Add (Entity_id 2, "value", Float 0.0)
         ; Add (Entity_id 3, "label", String "negative")
         ; Add (Entity_id 3, "value", Int (-2))
         ; Add (Entity_id 4, "label", String "odd-negative")
         ; Add (Entity_id 4, "value", Int (-1))
         ; Add (Entity_id 5, "label", String "positive")
         ; Add (Entity_id 5, "value", Int 3)
         ; Add (Entity_id 6, "label", String "zero")
         ; Add (Entity_id 6, "value", Int 0)
         ; Add (Entity_id 7, "label", String "string")
         ; Add (Entity_id 7, "value", String "0")
         ]
  in
  assert_equal_query
    "parse_query parses zero? predicates"
    [ [ Result_value (String "float-zero") ]; [ Result_value (String "zero") ] ]
    (q numeric_db (parse_query (labels_with_predicate "zero?")));
  assert_equal_query
    "parse_query parses pos? predicates"
    [ [ Result_value (String "float-positive") ]; [ Result_value (String "positive") ] ]
    (q numeric_db (parse_query (labels_with_predicate "pos?")));
  assert_equal_query
    "parse_query parses neg? predicates"
    [ [ Result_value (String "negative") ]; [ Result_value (String "odd-negative") ] ]
    (q numeric_db (parse_query (labels_with_predicate "neg?")));
  assert_equal_query
    "parse_query parses even? predicates"
    [ [ Result_value (String "negative") ]; [ Result_value (String "zero") ] ]
    (q numeric_db (parse_query (labels_with_predicate "even?")));
  assert_equal_query
    "parse_query parses odd? predicates"
    [ [ Result_value (String "odd-negative") ]; [ Result_value (String "positive") ] ]
    (q numeric_db (parse_query (labels_with_predicate "odd?")));
  assert_raises_invalid_arg
    "parse_query rejects type predicates with the wrong arity"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?value"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormList
                       [ QueryFormSymbol "number?"
                       ; QueryFormSymbol "?value"
                       ; QueryFormInt 1
                       ]
                   ]
               ])))

let test_parse_query_variadic_comparison_predicates () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "label", String "ascending")
         ; Add (Entity_id 1, "x", Int 1)
         ; Add (Entity_id 1, "y", Int 2)
         ; Add (Entity_id 1, "z", Int 3)
         ; Add (Entity_id 2, "label", String "descending")
         ; Add (Entity_id 2, "x", Int 3)
         ; Add (Entity_id 2, "y", Int 2)
         ; Add (Entity_id 2, "z", Int 1)
         ; Add (Entity_id 3, "label", String "equal")
         ; Add (Entity_id 3, "x", Int 2)
         ; Add (Entity_id 3, "y", Int 2)
         ; Add (Entity_id 3, "z", Int 2)
         ]
  in
  let labels_with_predicate predicate args =
    QueryFormVector
      ([ QueryFormKeyword "find"
       ; QueryFormSymbol "?label"
       ; QueryFormKeyword "where"
       ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
       ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "x"; QueryFormSymbol "?x" ]
       ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "y"; QueryFormSymbol "?y" ]
       ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "z"; QueryFormSymbol "?z" ]
       ]
       @ [ QueryFormVector (QueryFormList (QueryFormSymbol predicate :: args) :: []) ])
  in
  assert_equal_query
    "parse_query parses variadic < predicates"
    [ [ Result_value (String "ascending") ] ]
    (q db (parse_query (labels_with_predicate "<" [ QueryFormSymbol "?x"; QueryFormSymbol "?y"; QueryFormSymbol "?z" ])));
  assert_equal_query
    "parse_query parses variadic > predicates"
    [ [ Result_value (String "descending") ] ]
    (q db (parse_query (labels_with_predicate ">" [ QueryFormSymbol "?x"; QueryFormSymbol "?y"; QueryFormSymbol "?z" ])));
  assert_equal_query
    "parse_query parses one-argument comparison predicates"
    [ [ Result_value (String "ascending") ]; [ Result_value (String "descending") ]; [ Result_value (String "equal") ] ]
    (q db (parse_query (labels_with_predicate "<=" [ QueryFormSymbol "?x" ])));
  assert_raises_invalid_arg
    "parse_query rejects zero-argument comparison predicates"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?label"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormList [ QueryFormSymbol "<" ] ]
               ])))

let test_parse_query_boolean_predicates () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "label", String "false")
         ; Add (Entity_id 1, "value", Bool false)
         ; Add (Entity_id 2, "label", String "int")
         ; Add (Entity_id 2, "value", Int 1)
         ; Add (Entity_id 3, "label", String "nil")
         ; Add (Entity_id 4, "label", String "true")
         ; Add (Entity_id 4, "value", Bool true)
         ]
  in
  let labels_with_predicate predicate =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?label"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "value"; QueryFormSymbol "?value" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol predicate; QueryFormSymbol "?value" ] ]
      ]
  in
  assert_equal_query
    "parse_query parses true? predicates"
    [ [ Result_value (String "true") ] ]
    (q db (parse_query (labels_with_predicate "true?")));
  assert_equal_query
    "parse_query parses false? predicates"
    [ [ Result_value (String "false") ] ]
    (q db (parse_query (labels_with_predicate "false?")));
  assert_equal_query
    "parse_query parses nil? predicates"
    [ [ Result_value (String "nil") ] ]
    (q
       db
       (parse_query
          (QueryFormVector
             [ QueryFormKeyword "find"
             ; QueryFormSymbol "?label"
             ; QueryFormKeyword "where"
             ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormString "nil" ]
             ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
             ; QueryFormVector [ QueryFormList [ QueryFormSymbol "nil?"; QueryFormNil ] ]
             ])));
  assert_equal_query
    "parse_query parses some? predicates"
    [ [ Result_value (String "false") ]
    ; [ Result_value (String "int") ]
    ; [ Result_value (String "true") ]
    ]
    (q db (parse_query (labels_with_predicate "some?")));
  let not_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?label"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "value"; QueryFormSymbol "?value" ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "not"; QueryFormSymbol "?value" ]
          ; QueryFormSymbol "?negated"
          ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "true?"; QueryFormSymbol "?negated" ] ]
      ]
  in
  assert_equal_query
    "parse_query parses not function clauses"
    [ [ Result_value (String "false") ] ]
    (q db (parse_query not_query));
  let not_predicate_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?label"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "value"; QueryFormSymbol "?value" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "not"; QueryFormSymbol "?value" ] ]
      ]
  in
  assert_equal_query
    "parse_query parses not predicate clauses"
    [ [ Result_value (String "false") ] ]
    (q db (parse_query not_predicate_query));
  let and_predicate_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?label"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "value"; QueryFormSymbol "?value" ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "and"; QueryFormBool true; QueryFormSymbol "?value" ] ]
      ]
  in
  assert_equal_query
    "parse_query parses and predicate clauses"
    [ [ Result_value (String "int") ]; [ Result_value (String "true") ] ]
    (q db (parse_query and_predicate_query));
  let or_predicate_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?label"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "value"; QueryFormSymbol "?value" ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "or"; QueryFormBool false; QueryFormSymbol "?value" ] ]
      ]
  in
  assert_equal_query
    "parse_query parses or predicate clauses"
    [ [ Result_value (String "int") ]; [ Result_value (String "true") ] ]
    (q db (parse_query or_predicate_query));
  let not_truthiness_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?nil-not"
      ; QueryFormSymbol "?int-not"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "not"; QueryFormNil ]; QueryFormSymbol "?nil-not" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "not"; QueryFormInt 1 ]; QueryFormSymbol "?int-not" ]
      ]
  in
  assert_equal_query
    "parse_query not function uses Clojure truthiness"
    [ [ Result_value (Bool true); Result_value (Bool false) ] ]
    (q db (parse_query not_truthiness_query));
  let and_or_value_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?and-value"
      ; QueryFormSymbol "?and-falsey"
      ; QueryFormSymbol "?or-value"
      ; QueryFormSymbol "?or-falsey"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "and"; QueryFormBool true; QueryFormString "kept" ]
          ; QueryFormSymbol "?and-value"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "and"; QueryFormBool true; QueryFormNil; QueryFormString "ignored" ]
          ; QueryFormSymbol "?and-falsey"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "or"; QueryFormBool false; QueryFormString "fallback" ]
          ; QueryFormSymbol "?or-value"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "or"; QueryFormNil; QueryFormBool false ]
          ; QueryFormSymbol "?or-falsey"
          ]
      ]
  in
  assert_equal_query
    "parse_query and/or functions return Clojure-style values"
    [ [ Result_value (String "kept")
      ; Result_value Nil
      ; Result_value (String "fallback")
      ; Result_value (Bool false)
      ]
    ]
    (q db (parse_query and_or_value_query));
  assert_raises_invalid_arg
    "parse_query rejects boolean predicates with the wrong arity"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?value"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormList
                       [ QueryFormSymbol "true?"
                       ; QueryFormSymbol "?value"
                       ; QueryFormBool true
                       ]
                   ]
               ])))

let test_parse_query_core_value_functions () =
  let query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?same"
      ; QueryFormSymbol "?and"
      ; QueryFormSymbol "?or"
      ; QueryFormSymbol "?compare"
      ; QueryFormSymbol "?min"
      ; QueryFormSymbol "?max"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "identity"; QueryFormKeyword "user/name" ]
          ; QueryFormSymbol "?same"
          ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "and"
              ; QueryFormBool true
              ; QueryFormKeyword "user/name"
              ; QueryFormBool false
              ]
          ; QueryFormSymbol "?and"
          ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "or"
              ; QueryFormBool false
              ; QueryFormKeyword "user/name"
              ; QueryFormBool true
              ]
          ; QueryFormSymbol "?or"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "compare"; QueryFormInt 1; QueryFormInt 2 ]
          ; QueryFormSymbol "?compare"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "min"; QueryFormInt 3; QueryFormInt 1; QueryFormInt 2 ]
          ; QueryFormSymbol "?min"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "max"; QueryFormInt 3; QueryFormInt 1; QueryFormInt 2 ]
          ; QueryFormSymbol "?max"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses identity, boolean, compare, and extremum value functions"
    [ [ Result_value (Keyword "user/name")
      ; Result_value (Bool false)
      ; Result_value (Keyword "user/name")
      ; Result_value (Int (-1))
      ; Result_value (Int 1)
      ; Result_value (Int 3)
      ]
    ]
    (q (empty_db ()) (parse_query query));
  assert_raises_invalid_arg
    "parse_query rejects compare with the wrong arity"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?compare"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormList [ QueryFormSymbol "compare"; QueryFormInt 1 ]
                   ; QueryFormSymbol "?compare"
                   ]
               ])))

let test_parse_query_random_and_identity_predicates () =
  let random_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?rand"
      ; QueryFormSymbol "?rand_int"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "rand" ]; QueryFormSymbol "?rand" ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "rand-int"; QueryFormInt 10 ]
          ; QueryFormSymbol "?rand_int"
          ]
      ]
  in
  (match q (empty_db ()) (parse_query random_query) with
   | [ [ Result_value (Float rand); Result_value (Int rand_int) ] ] ->
     if rand < 0.0 || rand >= 1.0 then failwith "parse_query rand should be in [0, 1)";
     if rand_int < 0 || rand_int >= 10 then failwith "parse_query rand-int should be in [0, n)"
   | _ -> failwith "parse_query random functions should produce one row");
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "label", String "different")
         ; Add (Entity_id 1, "a", Int 1)
         ; Add (Entity_id 1, "b", Int 2)
         ; Add (Entity_id 1, "c", Int 1)
         ; Add (Entity_id 1, "d", Int 3)
         ; Add (Entity_id 2, "label", String "same")
         ; Add (Entity_id 2, "a", Int 1)
         ; Add (Entity_id 2, "b", Int 2)
         ; Add (Entity_id 2, "c", Float 1.0)
         ; Add (Entity_id 2, "d", Int 2)
         ]
  in
  let base_where =
    [ QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
    ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "a"; QueryFormSymbol "?a" ]
    ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "b"; QueryFormSymbol "?b" ]
    ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "c"; QueryFormSymbol "?c" ]
    ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "d"; QueryFormSymbol "?d" ]
    ]
  in
  let differ_query =
    QueryFormVector
      ([ QueryFormKeyword "find"; QueryFormSymbol "?label"; QueryFormKeyword "where" ]
       @ base_where
       @ [ QueryFormVector
             [ QueryFormList
                 [ QueryFormSymbol "-differ?"
                 ; QueryFormSymbol "?a"
                 ; QueryFormSymbol "?b"
                 ; QueryFormSymbol "?c"
                 ; QueryFormSymbol "?d"
                 ]
             ]
         ])
  in
  assert_equal_query
    "parse_query parses -differ? predicates"
    [ [ Result_value (String "different") ] ]
    (q db (parse_query differ_query));
  let identical_query =
    QueryFormVector
      ([ QueryFormKeyword "find"; QueryFormSymbol "?label"; QueryFormKeyword "where" ]
       @ base_where
       @ [ QueryFormVector
             [ QueryFormList
                 [ QueryFormSymbol "identical?"
                 ; QueryFormSymbol "?b"
                 ; QueryFormSymbol "?d"
                 ]
             ]
         ])
  in
  assert_equal_query
    "parse_query parses identical? predicates"
    [ [ Result_value (String "same") ] ]
    (q db (parse_query identical_query));
  let complement_query =
    QueryFormVector
      ([ QueryFormKeyword "find"; QueryFormSymbol "?label"; QueryFormKeyword "where" ]
       @ base_where
       @ [ QueryFormVector
             [ QueryFormList
                 [ QueryFormList [ QueryFormSymbol "complement"; QueryFormSymbol "even?" ]
                 ; QueryFormSymbol "?a"
                 ]
             ]
         ])
  in
  assert_equal_query
    "parse_query parses complement unary predicates"
    [ [ Result_value (String "different") ]; [ Result_value (String "same") ] ]
    (q db (parse_query complement_query));
  let complement_equality_query =
    QueryFormVector
      ([ QueryFormKeyword "find"; QueryFormSymbol "?label"; QueryFormKeyword "where" ]
       @ base_where
       @ [ QueryFormVector
             [ QueryFormList
                 [ QueryFormList [ QueryFormSymbol "complement"; QueryFormSymbol "=" ]
                 ; QueryFormSymbol "?b"
                 ; QueryFormSymbol "?d"
                 ]
             ]
         ])
  in
  assert_equal_query
    "parse_query parses complement variadic predicates"
    [ [ Result_value (String "different") ] ]
    (q db (parse_query complement_equality_query));
  assert_raises_invalid_arg
    "parse_query rejects identical? with the wrong arity"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?x"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormList [ QueryFormSymbol "identical?"; QueryFormSymbol "?x" ] ]
               ])))
  ;
  assert_raises_invalid_arg
    "parse_query rejects complement with a non-predicate target"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?x"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormList
                       [ QueryFormList [ QueryFormSymbol "complement"; QueryFormSymbol "+" ]
                       ; QueryFormSymbol "?x"
                       ]
                   ]
               ])))

let test_parse_query_string_predicates_and_transforms () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "label", String "alpha")
         ; Add (Entity_id 1, "text", String "alphabet")
         ; Add (Entity_id 2, "label", String "beta")
         ; Add (Entity_id 2, "text", String "betamax")
         ; Add (Entity_id 3, "label", String "gamma")
         ; Add (Entity_id 3, "text", String "gamma")
         ]
  in
  let labels_with_predicate predicate needle =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?label"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "text"; QueryFormSymbol "?text" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol predicate; QueryFormSymbol "?text"; QueryFormString needle ] ]
      ]
  in
  assert_equal_query
    "parse_query parses clojure.string/includes? predicates"
    [ [ Result_value (String "alpha") ]; [ Result_value (String "beta") ] ]
    (q db (parse_query (labels_with_predicate "clojure.string/includes?" "bet")));
  assert_equal_query
    "parse_query parses clojure.string/starts-with? predicates"
    [ [ Result_value (String "alpha") ] ]
    (q db (parse_query (labels_with_predicate "clojure.string/starts-with?" "alp")));
  assert_equal_query
    "parse_query parses clojure.string/ends-with? predicates"
    [ [ Result_value (String "beta") ] ]
    (q db (parse_query (labels_with_predicate "clojure.string/ends-with?" "max")));
  let transform_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?lower"
      ; QueryFormSymbol "?upper"
      ; QueryFormSymbol "?capitalized"
      ; QueryFormSymbol "?reversed"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "clojure.string/lower-case"; QueryFormString "dAtA" ]
          ; QueryFormSymbol "?lower"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "clojure.string/upper-case"; QueryFormString "dAtA" ]
          ; QueryFormSymbol "?upper"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "clojure.string/capitalize"; QueryFormString "dAtA" ]
          ; QueryFormSymbol "?capitalized"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "clojure.string/reverse"; QueryFormString "dAtA" ]
          ; QueryFormSymbol "?reversed"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses clojure.string transform functions"
    [ [ Result_value (String "data")
      ; Result_value (String "DATA")
      ; Result_value (String "Data")
      ; Result_value (String "AtAd")
      ]
    ]
    (q db (parse_query transform_query));
  assert_raises_invalid_arg
    "parse_query rejects string predicates with the wrong arity"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?text"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormList
                       [ QueryFormSymbol "clojure.string/includes?"
                       ; QueryFormSymbol "?text"
                       ]
                   ]
               ])))

let test_parse_query_string_trim_index_and_subs () =
  let trim_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?trimmed"
      ; QueryFormSymbol "?left"
      ; QueryFormSymbol "?right"
      ; QueryFormSymbol "?newline"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "clojure.string/trim"; QueryFormString "  data  \n" ]
          ; QueryFormSymbol "?trimmed"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "clojure.string/triml"; QueryFormString "  data  \n" ]
          ; QueryFormSymbol "?left"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "clojure.string/trimr"; QueryFormString "  data  \n" ]
          ; QueryFormSymbol "?right"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "clojure.string/trim-newline"; QueryFormString "  data  \n" ]
          ; QueryFormSymbol "?newline"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses clojure.string trim functions"
    [ [ Result_value (String "data")
      ; Result_value (String "data  \n")
      ; Result_value (String "  data")
      ; Result_value (String "  data  ")
      ]
    ]
    (q (empty_db ()) (parse_query trim_query));
  let index_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?first"
      ; QueryFormSymbol "?last"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "clojure.string/index-of"; QueryFormString "bananas"; QueryFormString "na" ]
          ; QueryFormSymbol "?first"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "clojure.string/last-index-of"; QueryFormString "bananas"; QueryFormString "na" ]
          ; QueryFormSymbol "?last"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses clojure.string index functions"
    [ [ Result_value (Int 2); Result_value (Int 4) ] ]
    (q (empty_db ()) (parse_query index_query));
  let subs_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?part"
      ; QueryFormSymbol "?suffix"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "subs"; QueryFormString "datascript"; QueryFormInt 4; QueryFormInt 10 ]
          ; QueryFormSymbol "?part"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "subs"; QueryFormString "datascript"; QueryFormInt 4 ]
          ; QueryFormSymbol "?suffix"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses subs functions"
    [ [ Result_value (String "script"); Result_value (String "script") ] ]
    (q (empty_db ()) (parse_query subs_query));
  assert_raises_invalid_arg
    "parse_query rejects subs with the wrong arity"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?part"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormList [ QueryFormSymbol "subs"; QueryFormString "data" ]
                   ; QueryFormSymbol "?part"
                   ]
               ])))

let test_parse_query_string_build_replace_regex_and_split () =
  let string_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?built"
      ; QueryFormSymbol "?joined"
      ; QueryFormSymbol "?joined-plain"
      ; QueryFormSymbol "?replaced"
      ; QueryFormSymbol "?first"
      ; QueryFormSymbol "?regex-replaced"
      ; QueryFormSymbol "?regex-first"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "str"
              ; QueryFormString "score="
              ; QueryFormInt 42
              ; QueryFormBool true
              ]
          ; QueryFormSymbol "?built"
          ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "clojure.string/join"
              ; QueryFormString ","
              ; QueryFormVector [ QueryFormString "red"; QueryFormString "green"; QueryFormString "blue" ]
              ]
          ; QueryFormSymbol "?joined"
          ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "clojure.string/join"
              ; QueryFormVector [ QueryFormString "red"; QueryFormString "green"; QueryFormString "blue" ]
              ]
          ; QueryFormSymbol "?joined-plain"
          ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "clojure.string/replace"
              ; QueryFormString "banana"
              ; QueryFormString "na"
              ; QueryFormString "NA"
              ]
          ; QueryFormSymbol "?replaced"
          ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "clojure.string/replace-first"
              ; QueryFormString "banana"
              ; QueryFormString "na"
              ; QueryFormString "NA"
              ]
          ; QueryFormSymbol "?first"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "re-pattern"; QueryFormString "[ae]" ]
          ; QueryFormSymbol "?vowels"
          ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "clojure.string/replace"
              ; QueryFormString "banana"
              ; QueryFormSymbol "?vowels"
              ; QueryFormString "*"
              ]
          ; QueryFormSymbol "?regex-replaced"
          ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "clojure.string/replace-first"
              ; QueryFormString "banana"
              ; QueryFormSymbol "?vowels"
              ; QueryFormString "*"
              ]
          ; QueryFormSymbol "?regex-first"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses str, join, and replace string functions"
    [ [ Result_value (String "score=42true")
      ; Result_value (String "red,green,blue")
      ; Result_value (String "redgreenblue")
      ; Result_value (String "baNANA")
      ; Result_value (String "baNAna")
      ; Result_value (String "b*n*n*")
      ; Result_value (String "b*nana")
      ]
    ]
    (q (empty_db ()) (parse_query string_query));
  let regex_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?match"
      ; QueryFormSymbol "?full"
      ; QueryFormSymbol "?matches"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "re-pattern"; QueryFormString "[0-9]+" ]
          ; QueryFormSymbol "?digits"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "re-find"; QueryFormSymbol "?digits"; QueryFormString "abc123def" ]
          ; QueryFormSymbol "?match"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "re-matches"; QueryFormString "[a-z]+[0-9]+"; QueryFormString "abc123" ]
          ; QueryFormSymbol "?full"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "re-seq"; QueryFormSymbol "?digits"; QueryFormString "a1b22c333" ]
          ; QueryFormSymbol "?matches"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses regex string functions"
    [ [ Result_value (String "123")
      ; Result_value (String "abc123")
      ; Result_value (List [ String "1"; String "22"; String "333" ])
      ]
    ]
    (q (empty_db ()) (parse_query regex_query));
  let split_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?parts"
      ; QueryFormSymbol "?regex-parts"
      ; QueryFormSymbol "?limited-parts"
      ; QueryFormSymbol "?lines"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "clojure.string/split"; QueryFormString "red,green,blue"; QueryFormString "," ]
          ; QueryFormSymbol "?parts"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "re-pattern"; QueryFormString "[,;]" ]
          ; QueryFormSymbol "?separator"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "clojure.string/split"; QueryFormString "red,green;blue"; QueryFormSymbol "?separator" ]
          ; QueryFormSymbol "?regex-parts"
          ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "clojure.string/split"
              ; QueryFormString "red,green;blue"
              ; QueryFormSymbol "?separator"
              ; QueryFormInt 2
              ]
          ; QueryFormSymbol "?limited-parts"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "clojure.string/split-lines"; QueryFormString "first\nsecond\r\nthird" ]
          ; QueryFormSymbol "?lines"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses split and split-lines string functions"
    [ [ Result_value (List [ String "red"; String "green"; String "blue" ])
      ; Result_value (List [ String "red"; String "green"; String "blue" ])
      ; Result_value (List [ String "red"; String "green;blue" ])
      ; Result_value (List [ String "first"; String "second"; String "third" ])
      ]
    ]
    (q (empty_db ()) (parse_query split_query));
  let blank_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?label"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "label"; QueryFormSymbol "?label" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "text"; QueryFormSymbol "?text" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "clojure.string/blank?"; QueryFormSymbol "?text" ] ]
      ]
  in
  let blank_db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "label", String "empty")
         ; Add (Entity_id 1, "text", String "")
         ; Add (Entity_id 2, "label", String "space")
         ; Add (Entity_id 2, "text", String " \t\n")
         ; Add (Entity_id 3, "label", String "word")
         ; Add (Entity_id 3, "text", String " data ")
         ]
  in
  assert_equal_query
    "parse_query parses clojure.string/blank? predicates"
    [ [ Result_value (String "empty") ]; [ Result_value (String "space") ] ]
    (q blank_db (parse_query blank_query));
  assert_raises_invalid_arg
    "parse_query rejects replace with the wrong arity"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?value"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormList
                       [ QueryFormSymbol "clojure.string/replace"
                       ; QueryFormString "banana"
                       ; QueryFormString "na"
                       ]
                   ; QueryFormSymbol "?value"
                   ]
               ])))

let test_parse_query_collection_constructors () =
  let collection_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?vector"
      ; QueryFormSymbol "?list"
      ; QueryFormSymbol "?set"
      ; QueryFormSymbol "?hash_map"
      ; QueryFormSymbol "?array_map"
      ; QueryFormSymbol "?tuple"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "vector"; QueryFormKeyword "db/add"; QueryFormInt (-1); QueryFormKeyword "attr"; QueryFormInt 12 ]
          ; QueryFormSymbol "?vector"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "list"; QueryFormInt 2; QueryFormInt 1; QueryFormInt 1 ]
          ; QueryFormSymbol "?list"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "set"; QueryFormInt 2; QueryFormInt 1; QueryFormInt 1 ]
          ; QueryFormSymbol "?set"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "hash-map"; QueryFormKeyword "left"; QueryFormInt 1; QueryFormKeyword "right"; QueryFormInt 2 ]
          ; QueryFormSymbol "?hash_map"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "array-map"; QueryFormKeyword "right"; QueryFormInt 2; QueryFormKeyword "left"; QueryFormInt 1 ]
          ; QueryFormSymbol "?array_map"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "tuple"; QueryFormInt 1; QueryFormInt 2 ]
          ; QueryFormSymbol "?tuple"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses collection constructor functions"
    [ [ Result_value (Vector [ Keyword "db/add"; Int (-1); Keyword "attr"; Int 12 ])
      ; Result_value (List [ Int 2; Int 1; Int 1 ])
      ; Result_value (Set [ Int 1; Int 2 ])
      ; Result_value (Map [ Keyword "left", Int 1; Keyword "right", Int 2 ])
      ; Result_value (Map [ Keyword "left", Int 1; Keyword "right", Int 2 ])
      ; Result_value (Tuple [ Some (Int 1); Some (Int 2) ])
      ]
    ]
    (q (empty_db ()) (parse_query collection_query));
  let range_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?x"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "range"; QueryFormInt 1; QueryFormInt 8; QueryFormInt 3 ]; QueryFormSymbol "?x" ]
      ]
  in
  assert_equal_query
    "parse_query parses range functions"
    [ [ Result_value (Int 1) ]; [ Result_value (Int 4) ]; [ Result_value (Int 7) ] ]
    (q (empty_db ()) (parse_query range_query));
  let untuple_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?right"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "tuple"; QueryFormString "left"; QueryFormString "right" ]
          ; QueryFormSymbol "?pair"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "untuple"; QueryFormSymbol "?pair" ]
          ; QueryFormVector [ QueryFormSymbol "?left"; QueryFormSymbol "?right" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses untuple function outputs"
    [ [ Result_value (String "right") ] ]
    (q (empty_db ()) (parse_query untuple_query));
  assert_raises_invalid_arg
    "parse_query rejects hash-map odd argument counts"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?m"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormList [ QueryFormSymbol "hash-map"; QueryFormKeyword "left"; QueryFormInt 1; QueryFormKeyword "right" ]
                   ; QueryFormSymbol "?m"
                   ]
               ])))

let test_parse_query_ground_and_value_metadata_functions () =
  let value_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?op"
      ; QueryFormSymbol "?name"
      ; QueryFormSymbol "?namespace"
      ; QueryFormSymbol "?keyword"
      ; QueryFormSymbol "?qualified"
      ; QueryFormSymbol "?type"
      ; QueryFormSymbol "?meta"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "ground"; QueryFormKeyword "db/add" ]
          ; QueryFormSymbol "?op"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "name"; QueryFormKeyword "user/name" ]
          ; QueryFormSymbol "?name"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "namespace"; QueryFormKeyword "user/name" ]
          ; QueryFormSymbol "?namespace"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "keyword"; QueryFormString "user/email" ]
          ; QueryFormSymbol "?keyword"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "keyword"; QueryFormString "user"; QueryFormString "score" ]
          ; QueryFormSymbol "?qualified"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "type"; QueryFormString "plain" ]
          ; QueryFormSymbol "?type"
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "meta"; QueryFormString "plain" ]
          ; QueryFormSymbol "?meta"
          ]
      ]
  in
  assert_equal_query
    "parse_query parses ground, keyword, name, namespace, type, and meta functions"
    [ [ Result_value (Keyword "db/add")
      ; Result_value (String "name")
      ; Result_value (String "user")
      ; Result_value (Keyword "user/email")
      ; Result_value (Keyword "user/score")
      ; Result_value (Keyword "type/string")
      ; Result_value Nil
      ]
    ]
    (q (empty_db ()) (parse_query value_query));
  let tuple_ground_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?a"
      ; QueryFormSymbol "?c"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "ground"
              ; QueryFormVector [ QueryFormKeyword "a"; QueryFormKeyword "b"; QueryFormKeyword "c" ]
              ]
          ; QueryFormVector [ QueryFormSymbol "?a"; QueryFormSymbol "_"; QueryFormSymbol "?c" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses ground tuple destructuring"
    [ [ Result_value (Keyword "a"); Result_value (Keyword "c") ] ]
    (q (empty_db ()) (parse_query tuple_ground_query));
  let collection_ground_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?vowel"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "ground"
              ; QueryFormVector
                  [ QueryFormKeyword "a"; QueryFormKeyword "e"; QueryFormKeyword "i" ]
              ]
          ; QueryFormVector [ QueryFormSymbol "?vowel"; QueryFormSymbol "..." ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses ground collection bindings"
    [ [ Result_value (Keyword "a") ]
    ; [ Result_value (Keyword "e") ]
    ; [ Result_value (Keyword "i") ]
    ]
    (q (empty_db ()) (parse_query collection_ground_query));
  let relation_ground_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?x"
      ; QueryFormSymbol "?z"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "ground"
              ; QueryFormVector
                  [ QueryFormVector [ QueryFormKeyword "a"; QueryFormKeyword "b"; QueryFormKeyword "c" ]
                  ; QueryFormVector [ QueryFormKeyword "d"; QueryFormKeyword "e"; QueryFormKeyword "f" ]
                  ]
              ]
          ; QueryFormVector
              [ QueryFormVector [ QueryFormSymbol "?x"; QueryFormSymbol "_"; QueryFormSymbol "?z" ] ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses ground relation bindings"
    [ [ Result_value (Keyword "a"); Result_value (Keyword "c") ]
    ; [ Result_value (Keyword "d"); Result_value (Keyword "f") ]
    ]
    (q (empty_db ()) (parse_query relation_ground_query));
  assert_raises_invalid_arg
    "parse_query rejects keyword with the wrong arity"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?keyword"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormList
                       [ QueryFormSymbol "keyword"
                       ; QueryFormString "too"
                       ; QueryFormString "many"
                       ; QueryFormString "args"
                       ]
                   ; QueryFormSymbol "?keyword"
                   ]
               ])))

let test_parse_query_aggregate_find_expressions () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "color", String "red")
         ; Add (Entity_id 1, "heads", Int 3)
         ; Add (Entity_id 2, "color", String "red")
         ; Add (Entity_id 2, "heads", Int 1)
         ; Add (Entity_id 3, "color", String "blue")
         ; Add (Entity_id 3, "heads", Int 2)
         ]
  in
  let query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?color"
      ; QueryFormList [ QueryFormSymbol "sum"; QueryFormSymbol "?heads" ]
      ; QueryFormList [ QueryFormSymbol "min"; QueryFormSymbol "?heads" ]
      ; QueryFormList [ QueryFormSymbol "max"; QueryFormSymbol "?heads" ]
      ; QueryFormList [ QueryFormSymbol "count"; QueryFormSymbol "?heads" ]
      ; QueryFormList [ QueryFormSymbol "count-distinct"; QueryFormSymbol "?heads" ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "color"; QueryFormSymbol "?color" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "heads"; QueryFormSymbol "?heads" ]
      ]
  in
  assert_equal_query
    "parse_query parses aggregate find expressions"
    [ [ Result_value (String "blue")
      ; Result_value (Int 2)
      ; Result_value (Int 2)
      ; Result_value (Int 2)
      ; Result_value (Int 1)
      ; Result_value (Int 1)
      ]
    ; [ Result_value (String "red")
      ; Result_value (Int 4)
      ; Result_value (Int 1)
      ; Result_value (Int 3)
      ; Result_value (Int 2)
      ; Result_value (Int 2)
      ]
    ]
    (q db (parse_query query));
  let vector_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?color"
      ; QueryFormVector [ QueryFormSymbol "sum"; QueryFormSymbol "?heads" ]
      ; QueryFormVector [ QueryFormSymbol "count"; QueryFormSymbol "?heads" ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "color"; QueryFormSymbol "?color" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "heads"; QueryFormSymbol "?heads" ]
      ]
  in
  assert_equal_query
    "parse_query parses vector-form aggregate find expressions"
    [ [ Result_value (String "blue"); Result_value (Int 2); Result_value (Int 1) ]
    ; [ Result_value (String "red"); Result_value (Int 4); Result_value (Int 2) ]
    ]
    (q db (parse_query vector_query));
  assert_raises_invalid_arg
    "parse_query rejects aggregate find expressions with the wrong arity"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormList [ QueryFormSymbol "sum" ]
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "heads"; QueryFormSymbol "?heads" ]
               ])))

let test_parse_query_extended_aggregate_find_expressions () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "color", String "red")
         ; Add (Entity_id 1, "amount", Int 1)
         ; Add (Entity_id 2, "color", String "red")
         ; Add (Entity_id 2, "amount", Int 2)
         ; Add (Entity_id 3, "color", String "red")
         ; Add (Entity_id 3, "amount", Int 3)
         ; Add (Entity_id 4, "color", String "red")
         ; Add (Entity_id 4, "amount", Int 4)
         ; Add (Entity_id 5, "color", String "blue")
         ; Add (Entity_id 5, "amount", Int 7)
         ; Add (Entity_id 6, "color", String "blue")
         ; Add (Entity_id 6, "amount", Int 8)
         ]
  in
  let min_max_n_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?color"
      ; QueryFormList [ QueryFormSymbol "max"; QueryFormInt 3; QueryFormSymbol "?amount" ]
      ; QueryFormList [ QueryFormSymbol "min"; QueryFormInt 3; QueryFormSymbol "?amount" ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "color"; QueryFormSymbol "?color" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "amount"; QueryFormSymbol "?amount" ]
      ]
  in
  assert_equal_query
    "parse_query parses min/max n aggregate find expressions"
    [ [ Result_value (String "blue")
      ; Result_value (Tuple [ Some (Int 7); Some (Int 8) ])
      ; Result_value (Tuple [ Some (Int 7); Some (Int 8) ])
      ]
    ; [ Result_value (String "red")
      ; Result_value (Tuple [ Some (Int 2); Some (Int 3); Some (Int 4) ])
      ; Result_value (Tuple [ Some (Int 1); Some (Int 2); Some (Int 3) ])
      ]
    ]
    (q db (parse_query min_max_n_query));
  let stats_db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "sample", Int 10)
         ; Add (Entity_id 2, "sample", Int 15)
         ; Add (Entity_id 3, "sample", Int 20)
         ; Add (Entity_id 4, "sample", Int 35)
         ; Add (Entity_id 5, "sample", Int 75)
         ]
  in
  let stats_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormList [ QueryFormSymbol "avg"; QueryFormSymbol "?sample" ]
      ; QueryFormList [ QueryFormSymbol "median"; QueryFormSymbol "?sample" ]
      ; QueryFormList [ QueryFormSymbol "variance"; QueryFormSymbol "?sample" ]
      ; QueryFormList [ QueryFormSymbol "stddev"; QueryFormSymbol "?sample" ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "sample"; QueryFormSymbol "?sample" ]
      ]
  in
  assert_equal_query
    "parse_query parses statistical aggregate find expressions"
    [ [ Result_value (Float 31.0)
      ; Result_value (Float 20.0)
      ; Result_value (Float 554.0)
      ; Result_value (Float 23.53720459187964)
      ]
    ]
    (q stats_db (parse_query stats_query));
  let distinct_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?color"
      ; QueryFormList [ QueryFormSymbol "distinct"; QueryFormSymbol "?amount" ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "color"; QueryFormSymbol "?color" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "amount"; QueryFormSymbol "?amount" ]
      ]
  in
  assert_equal_query
    "parse_query parses distinct aggregate find expressions"
    [ [ Result_value (String "blue"); Result_value (Set [ Int 7; Int 8 ]) ]
    ; [ Result_value (String "red"); Result_value (Set [ Int 1; Int 2; Int 3; Int 4 ]) ]
    ]
    (q db (parse_query distinct_query));
  let parameterized_min_max_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?color"
      ; QueryFormList [ QueryFormSymbol "max"; QueryFormSymbol "?amount"; QueryFormSymbol "?x" ]
      ; QueryFormList [ QueryFormSymbol "min"; QueryFormSymbol "?amount"; QueryFormSymbol "?x" ]
      ; QueryFormKeyword "in"
      ; QueryFormVector
          [ QueryFormVector [ QueryFormSymbol "?color"; QueryFormSymbol "?x" ] ]
      ; QueryFormSymbol "?amount"
      ]
  in
  assert_equal_query
    "parse_query parses aggregate parameter passing"
    [ [ Result_value (String "blue")
      ; Result_value (Tuple [ Some (Int 7); Some (Int 8) ])
      ; Result_value (Tuple [ Some (Int 7); Some (Int 8) ])
      ]
    ; [ Result_value (String "red")
      ; Result_value (Tuple [ Some (Int 3); Some (Int 4); Some (Int 5) ])
      ; Result_value (Tuple [ Some (Int 1); Some (Int 2); Some (Int 3) ])
      ]
    ]
    (q
       ~inputs:
         [ Arg_relation
             [ [ Result_value (String "red"); Result_value (Int 1) ]
             ; [ Result_value (String "red"); Result_value (Int 2) ]
             ; [ Result_value (String "red"); Result_value (Int 3) ]
             ; [ Result_value (String "red"); Result_value (Int 4) ]
             ; [ Result_value (String "red"); Result_value (Int 5) ]
             ; [ Result_value (String "blue"); Result_value (Int 7) ]
             ; [ Result_value (String "blue"); Result_value (Int 8) ]
             ]
         ; Arg_scalar (Result_value (Int 3))
         ]
       (empty_db ())
       (parse_query parameterized_min_max_query));
  let random_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormList [ QueryFormSymbol "rand"; QueryFormSymbol "?x" ]
      ; QueryFormList [ QueryFormSymbol "rand"; QueryFormInt 5; QueryFormSymbol "?x" ]
      ; QueryFormList [ QueryFormSymbol "sample"; QueryFormInt 2; QueryFormSymbol "?x" ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "x"; QueryFormSymbol "?x" ]
      ]
  in
  let random_db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "x", Int 1)
         ; Add (Entity_id 2, "x", Int 2)
         ; Add (Entity_id 3, "x", Int 3)
         ]
  in
  (match q random_db (parse_query random_query) with
   | [ [ Result_value rand_value; Result_value (Tuple rand_values); Result_value (Tuple sample_values) ] ] ->
     let values = [ Int 1; Int 2; Int 3 ] in
     let member value = List.mem value values in
     if not (member rand_value) then failwith "parse_query rand aggregate returned a value outside the input";
     let rand_values = List.map Option.get rand_values in
     if List.length rand_values <> 5 || not (List.for_all member rand_values) then
       failwith "parse_query rand n aggregate returned invalid values";
     let sample_values = List.map Option.get sample_values in
     if List.length sample_values <> 2 || not (List.for_all member sample_values) then
       failwith "parse_query sample aggregate returned invalid values"
   | _ -> failwith "parse_query random aggregates should produce one row");
  assert_raises_invalid_arg
    "parse_query rejects min n aggregate with non-literal and non-variable amount"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormList [ QueryFormSymbol "min"; QueryFormString "n"; QueryFormSymbol "?x" ]
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormList [ QueryFormSymbol "ground"; QueryFormVector [ QueryFormInt 1 ] ]; QueryFormSymbol "?x" ]
               ])))

let test_parse_query_not_and_not_join_clauses () =
  let likes_db =
    empty_db ~schema:[ "likes", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "likes", Many_values [ String "pizza"; String "fries" ]
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "likes", Many_values [ String "pie" ]
                 ]
             }
         ]
  in
  let not_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector
          [ QueryFormSymbol "not"
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "likes"; QueryFormString "pie" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses not clauses"
    [ [ Result_value (String "Ivan") ] ]
    (q likes_db (parse_query not_query));
  let direct_not_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormList
          [ QueryFormSymbol "not"
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "likes"; QueryFormString "pie" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses direct list not clauses"
    [ [ Result_value (String "Ivan") ] ]
    (q likes_db (parse_query direct_not_query));
  let releases_db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "artist", One_value (String "A"); "year", One_value (Int 1970) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "artist", One_value (String "A"); "year", One_value (Int 1971) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "artist", One_value (String "B"); "year", One_value (Int 1971) ] }
         ]
  in
  let not_join_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?release"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "artist"; QueryFormSymbol "?artist" ]
      ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "year"; QueryFormSymbol "?year" ]
      ; QueryFormVector
          [ QueryFormSymbol "not-join"
          ; QueryFormVector [ QueryFormSymbol "?artist" ]
          ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "year"; QueryFormInt 1970 ]
          ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "artist"; QueryFormSymbol "?artist" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses not-join clauses"
    [ [ Result_entity 3 ] ]
    (q releases_db (parse_query not_join_query));
  assert_raises_invalid_arg
    "parse_query rejects not-join without join variables"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormSymbol "not-join"
                   ; QueryFormVector []
                   ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
                   ]
               ])))

let test_parse_query_or_and_or_join_clauses () =
  let people_db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 2, "name", String "Petr")
         ; Add (Entity_id 3, "name", String "Oleg")
         ]
  in
  let or_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormSymbol "or"
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Oleg" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses or clauses"
    [ [ Result_entity 1 ]; [ Result_entity 3 ] ]
    (q people_db (parse_query or_query));
  let releases_db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "artist", String "A")
         ; Add (Entity_id 1, "year", Int 1970)
         ; Add (Entity_id 2, "artist", String "A")
         ; Add (Entity_id 2, "year", Int 1971)
         ; Add (Entity_id 3, "artist", String "B")
         ; Add (Entity_id 3, "year", Int 1971)
         ]
  in
  let or_join_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?release"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "artist"; QueryFormSymbol "?artist" ]
      ; QueryFormVector
          [ QueryFormSymbol "or-join"
          ; QueryFormVector [ QueryFormSymbol "?artist" ]
          ; QueryFormVector
              [ QueryFormSymbol "and"
              ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "year"; QueryFormInt 1970 ]
              ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "artist"; QueryFormSymbol "?artist" ]
              ]
          ; QueryFormVector
              [ QueryFormSymbol "and"
              ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "year"; QueryFormInt 1972 ]
              ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "artist"; QueryFormSymbol "?artist" ]
              ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses or-join clauses with and branches"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q releases_db (parse_query or_join_query));
  let direct_or_join_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?release"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "artist"; QueryFormSymbol "?artist" ]
      ; QueryFormList
          [ QueryFormSymbol "or-join"
          ; QueryFormVector [ QueryFormSymbol "?artist" ]
          ; QueryFormList
              [ QueryFormSymbol "and"
              ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "year"; QueryFormInt 1970 ]
              ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "artist"; QueryFormSymbol "?artist" ]
              ]
          ; QueryFormList
              [ QueryFormSymbol "and"
              ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "year"; QueryFormInt 1972 ]
              ; QueryFormVector [ QueryFormSymbol "?release"; QueryFormKeyword "artist"; QueryFormSymbol "?artist" ]
              ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses direct list or-join clauses with direct and branches"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q releases_db (parse_query direct_or_join_query));
  let source_or_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$people"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormSymbol "$people"
          ; QueryFormList
              [ QueryFormSymbol "or"
              ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
              ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Oleg" ]
              ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified direct list or clauses"
    [ [ Result_entity 1 ]; [ Result_entity 3 ] ]
    (q_sources (empty_db ()) [ "people", Db_source people_db ] (parse_query source_or_query));
  assert_raises_invalid_arg
    "parse_query rejects or without branches"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "or" ]
               ])))

let test_parse_query_rules () =
  let db =
    empty_db ~schema:[ "follow", many ] ()
    |> db_with
         [ Add (Entity_id 1, "name", String "root")
         ; Add (Entity_id 1, "follow", Ref 2)
         ; Add (Entity_id 2, "follow", Ref 3)
         ; Add (Entity_id 2, "follow", Ref 4)
         ]
  in
  let simple_rule_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?from"
      ; QueryFormSymbol "?to"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ] ]
      ; QueryFormKeyword "rules"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ]
          ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "follow"; QueryFormSymbol "?to" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses simple rules and rule invocations"
    [ [ Result_entity 1; Result_entity 2 ]
    ; [ Result_entity 2; Result_entity 3 ]
    ; [ Result_entity 2; Result_entity 4 ]
    ]
    (q db (parse_query simple_rule_query));
  let vector_rule_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?from"
      ; QueryFormSymbol "?to"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ]
      ; QueryFormKeyword "rules"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ]
          ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "follow"; QueryFormSymbol "?to" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses vector rule invocations"
    [ [ Result_entity 1; Result_entity 2 ]
    ; [ Result_entity 2; Result_entity 3 ]
    ; [ Result_entity 2; Result_entity 4 ]
    ]
    (q db (parse_query vector_rule_query));
  let direct_rule_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?from"
      ; QueryFormSymbol "?to"
      ; QueryFormKeyword "where"
      ; QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ]
      ; QueryFormKeyword "rules"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ]
          ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "follow"; QueryFormSymbol "?to" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses direct list rule invocations"
    [ [ Result_entity 1; Result_entity 2 ]
    ; [ Result_entity 2; Result_entity 3 ]
    ; [ Result_entity 2; Result_entity 4 ]
    ]
    (q db (parse_query direct_rule_query));
  let parent_db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "name", String "root")
         ; Add (Entity_id 1, "parent", Ref 2)
         ; Add (Entity_id 2, "parent", Ref 3)
         ; Add (Entity_id 3, "parent", Ref 4)
         ]
  in
  let recursive_rule_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?ancestor"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?root"; QueryFormKeyword "name"; QueryFormString "root" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "ancestor"; QueryFormSymbol "?root"; QueryFormSymbol "?ancestor" ] ]
      ; QueryFormKeyword "rules"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "ancestor"; QueryFormSymbol "?descendant"; QueryFormSymbol "?ancestor" ]
          ; QueryFormVector [ QueryFormSymbol "?descendant"; QueryFormKeyword "parent"; QueryFormSymbol "?ancestor" ]
          ]
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "ancestor"; QueryFormSymbol "?descendant"; QueryFormSymbol "?ancestor" ]
          ; QueryFormVector [ QueryFormSymbol "?descendant"; QueryFormKeyword "parent"; QueryFormSymbol "?parent" ]
          ; QueryFormVector [ QueryFormList [ QueryFormSymbol "ancestor"; QueryFormSymbol "?parent"; QueryFormSymbol "?ancestor" ] ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses recursive rules"
    [ [ Result_entity 2 ]; [ Result_entity 3 ]; [ Result_entity 4 ] ]
    (q parent_db (parse_query recursive_rule_query));
  let source_rule_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?to"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$links"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$links"; QueryFormSymbol "?from"; QueryFormKeyword "name"; QueryFormString "root" ]
      ; QueryFormVector [ QueryFormSymbol "$links"; QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ] ]
      ; QueryFormKeyword "rules"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ]
          ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "follow"; QueryFormSymbol "?to" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified rule invocations"
    [ [ Result_entity 2 ] ]
    (q_sources (empty_db ()) [ "links", Db_source db ] (parse_query source_rule_query));
  let source_vector_rule_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?to"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$links"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$links"; QueryFormSymbol "?from"; QueryFormKeyword "name"; QueryFormString "root" ]
      ; QueryFormVector
          [ QueryFormSymbol "$links"
          ; QueryFormSymbol "follows"
          ; QueryFormSymbol "?from"
          ; QueryFormSymbol "?to"
          ]
      ; QueryFormKeyword "rules"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ]
          ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "follow"; QueryFormSymbol "?to" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified vector rule invocations"
    [ [ Result_entity 2 ] ]
    (q_sources (empty_db ()) [ "links", Db_source db ] (parse_query source_vector_rule_query));
  let source_wrapped_vector_rule_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?to"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$links"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$links"; QueryFormSymbol "?from"; QueryFormKeyword "name"; QueryFormString "root" ]
      ; QueryFormVector
          [ QueryFormSymbol "$links"
          ; QueryFormVector
              [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ]
          ]
      ; QueryFormKeyword "rules"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ]
          ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "follow"; QueryFormSymbol "?to" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified wrapped vector rule invocations"
    [ [ Result_entity 2 ] ]
    (q_sources (empty_db ()) [ "links", Db_source db ] (parse_query source_wrapped_vector_rule_query));
  let source_list_rule_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?to"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$links"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$links"; QueryFormSymbol "?from"; QueryFormKeyword "name"; QueryFormString "root" ]
      ; QueryFormList
          [ QueryFormSymbol "$links"
          ; QueryFormSymbol "follows"
          ; QueryFormSymbol "?from"
          ; QueryFormSymbol "?to"
          ]
      ; QueryFormKeyword "rules"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ]
          ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "follow"; QueryFormSymbol "?to" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified list rule invocations"
    [ [ Result_entity 2 ] ]
    (q_sources (empty_db ()) [ "links", Db_source db ] (parse_query source_list_rule_query));
  let required_rule_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?to"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "root-follow"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ] ]
      ; QueryFormKeyword "rules"
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "root-follow"
              ; QueryFormVector [ QueryFormSymbol "?from" ]
              ; QueryFormSymbol "?to"
              ]
          ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "name"; QueryFormString "root" ]
          ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "follow"; QueryFormSymbol "?to" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses rule heads with required vars"
    [ [ Result_entity 2 ] ]
    (q db (parse_query required_rule_query));
  let list_required_rule_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?to"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "root-follow"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ] ]
      ; QueryFormKeyword "rules"
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "root-follow"
              ; QueryFormList [ QueryFormSymbol "?from" ]
              ; QueryFormSymbol "?to"
              ]
          ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "name"; QueryFormString "root" ]
          ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "follow"; QueryFormSymbol "?to" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses rule heads with list-form required vars"
    [ [ Result_entity 2 ] ]
    (q db (parse_query list_required_rule_query));
  let symbol_db = db_with [ Add (Entity_id 1, "marker", Symbol "root") ] db in
  let literal_rule_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?from"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "has-marker"; QueryFormSymbol "?from"; QueryFormSymbol "root" ] ]
      ; QueryFormKeyword "rules"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "has-marker"; QueryFormSymbol "?entity"; QueryFormSymbol "?marker" ]
          ; QueryFormVector [ QueryFormSymbol "?entity"; QueryFormKeyword "marker"; QueryFormSymbol "?marker" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses symbol literal rule invocation arguments"
    [ [ Result_entity 1 ] ]
    (q symbol_db (parse_query literal_rule_query));
  assert_raises_invalid_arg
    "parse_query rejects rules without body clauses"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormList [ QueryFormSymbol "lonely"; QueryFormSymbol "?e" ] ]
               ; QueryFormKeyword "rules"
               ; QueryFormVector [ QueryFormList [ QueryFormSymbol "lonely"; QueryFormSymbol "?e" ] ]
               ])))
  ;
  assert_raises_invalid_arg
    "parse_query rejects duplicate rule vars across required and free vars"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?to"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormList [ QueryFormSymbol "bad"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ] ]
               ; QueryFormKeyword "rules"
               ; QueryFormVector
                   [ QueryFormList
                       [ QueryFormSymbol "bad"
                       ; QueryFormVector [ QueryFormSymbol "?from" ]
                       ; QueryFormSymbol "?from"
                       ]
                   ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "follow"; QueryFormSymbol "?to" ]
                   ]
               ])))

let test_parse_query_source_qualified_composite_clauses () =
  let names =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 2, "name", String "Oleg")
         ; Add (Entity_id 3, "name", String "Petr")
         ]
  in
  let ages =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "age", Int 10)
         ; Add (Entity_id 2, "age", Int 20)
         ; Add (Entity_id 3, "score", Int 1)
         ]
  in
  let sources = [ "names", Db_source names; "ages", Db_source ages ] in
  let source_not_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$names"
      ; QueryFormSymbol "$ages"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$names"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector
          [ QueryFormSymbol "$ages"
          ; QueryFormVector
              [ QueryFormSymbol "not"
              ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormInt 10 ]
              ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified not clauses"
    [ [ Result_entity 2 ]; [ Result_entity 3 ] ]
    (q_sources (empty_db ()) sources (parse_query source_not_query));
  let source_not_join_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$names"
      ; QueryFormSymbol "$ages"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$names"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector
          [ QueryFormSymbol "$ages"
          ; QueryFormVector
              [ QueryFormSymbol "not-join"
              ; QueryFormVector [ QueryFormSymbol "?e" ]
              ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormInt 10 ]
              ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified not-join clauses"
    [ [ Result_entity 2 ]; [ Result_entity 3 ] ]
    (q_sources (empty_db ()) sources (parse_query source_not_join_query));
  let source_or_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$names"
      ; QueryFormSymbol "$ages"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$names"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector
          [ QueryFormSymbol "$ages"
          ; QueryFormVector
              [ QueryFormSymbol "or"
              ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormInt 10 ]
              ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormInt 20 ]
              ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified or clauses"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q_sources (empty_db ()) sources (parse_query source_or_query));
  let source_or_join_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$names"
      ; QueryFormSymbol "$ages"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$names"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector
          [ QueryFormSymbol "$ages"
          ; QueryFormVector
              [ QueryFormSymbol "or-join"
              ; QueryFormVector [ QueryFormSymbol "?e" ]
              ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormInt 10 ]
              ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormInt 20 ]
              ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified or-join clauses"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q_sources (empty_db ()) sources (parse_query source_or_join_query));
  let source_list_not_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$names"
      ; QueryFormSymbol "$ages"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$names"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormList
          [ QueryFormSymbol "$ages"
          ; QueryFormSymbol "not"
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormInt 10 ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified list-form not clauses"
    [ [ Result_entity 2 ]; [ Result_entity 3 ] ]
    (q_sources (empty_db ()) sources (parse_query source_list_not_query));
  let source_list_not_join_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$names"
      ; QueryFormSymbol "$ages"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$names"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormList
          [ QueryFormSymbol "$ages"
          ; QueryFormSymbol "not-join"
          ; QueryFormVector [ QueryFormSymbol "?e" ]
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormInt 10 ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified list-form not-join clauses"
    [ [ Result_entity 2 ]; [ Result_entity 3 ] ]
    (q_sources (empty_db ()) sources (parse_query source_list_not_join_query));
  let source_list_or_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$names"
      ; QueryFormSymbol "$ages"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$names"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormList
          [ QueryFormSymbol "$ages"
          ; QueryFormSymbol "or"
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormInt 10 ]
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormInt 20 ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified list-form or clauses"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q_sources (empty_db ()) sources (parse_query source_list_or_query));
  let source_list_or_join_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$names"
      ; QueryFormSymbol "$ages"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$names"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormList
          [ QueryFormSymbol "$ages"
          ; QueryFormSymbol "or-join"
          ; QueryFormVector [ QueryFormSymbol "?e" ]
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormInt 10 ]
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormInt 20 ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified list-form or-join clauses"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q_sources (empty_db ()) sources (parse_query source_list_or_join_query));
  assert_raises_invalid_arg
    "parse_query rejects source-qualified or without branches"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "$ages"; QueryFormVector [ QueryFormSymbol "or" ] ]
               ])))

let test_parse_query_in_bindings () =
  let db =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 1, "first", String "Ivan")
         ; Add (Entity_id 1, "last", String "Petrov")
         ; Add (Entity_id 1, "age", Int 31)
         ; Add (Entity_id 2, "name", String "Petr")
         ; Add (Entity_id 2, "first", String "Petr")
         ; Add (Entity_id 2, "last", String "Ivanov")
         ; Add (Entity_id 2, "age", Int 25)
         ; Add (Entity_id 3, "name", String "Oleg")
         ; Add (Entity_id 3, "first", String "Oleg")
         ; Add (Entity_id 3, "last", String "Petrov")
         ; Add (Entity_id 3, "age", Int 44)
         ]
  in
  let scalar_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "?wanted"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?wanted" ]
      ]
  in
  assert_equal_query
    "parse_query parses scalar :in bindings"
    [ [ Result_entity 1 ] ]
    (q ~inputs:[ Arg_scalar (Result_value (String "Ivan")) ] db (parse_query scalar_query));
  let entity_ref_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?age"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "?person"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?person"; QueryFormKeyword "age"; QueryFormSymbol "?age" ]
      ]
  in
  assert_equal_query
    "parse_query parses entity-ref :in arguments"
    [ [ Result_value (Int 31) ] ]
    (q ~inputs:[ Arg_entity_ref (Lookup_ref ("name", String "Ivan")) ] db (parse_query entity_ref_query));
  let collection_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormVector [ QueryFormSymbol "?wanted"; QueryFormSymbol "..." ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?wanted" ]
      ]
  in
  assert_equal_query
    "parse_query parses collection :in bindings"
    [ [ Result_entity 1 ]; [ Result_entity 3 ] ]
    (q
       ~inputs:
         [ Arg_collection
             [ Result_value (String "Ivan")
             ; Result_value (String "Oleg")
             ]
       ]
       db
       (parse_query collection_query));
  assert_equal_query
    "parse_query accepts scalar collection values for collection :in bindings"
    [ [ Result_entity 1 ]; [ Result_entity 3 ] ]
    (q
       ~inputs:[ Arg_scalar (Result_value (List [ String "Ivan"; String "Oleg" ])) ]
       db
       (parse_query collection_query));
  let nested_collection_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?tag"
      ; QueryFormKeyword "in"
      ; QueryFormVector
          [ QueryFormVector [ QueryFormSymbol "?tag"; QueryFormSymbol "..." ]
          ; QueryFormSymbol "..."
          ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "string?"; QueryFormSymbol "?tag" ] ]
      ]
  in
  assert_equal_query
    "parse_query parses nested collection :in bindings"
    [ [ Result_value (String "blue") ]
    ; [ Result_value (String "green") ]
    ; [ Result_value (String "red") ]
    ]
    (q
       ~inputs:
         [ Arg_collection
             [ Result_value (List [ String "blue"; String "green" ])
             ; Result_value (List [])
             ; Result_value (List [ String "red" ])
             ]
         ]
       db
       (parse_query nested_collection_query));
  let collection_list_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormList [ QueryFormSymbol "?wanted"; QueryFormSymbol "..." ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?wanted" ]
      ]
  in
  assert_equal_query
    "parse_query parses list-form collection :in bindings"
    [ [ Result_entity 1 ]; [ Result_entity 3 ] ]
    (q
       ~inputs:
         [ Arg_collection
             [ Result_value (String "Ivan")
             ; Result_value (String "Oleg")
             ]
         ]
       db
       (parse_query collection_list_query));
  let ignored_scalar_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "_"
      ; QueryFormSymbol "?wanted"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?wanted" ]
      ]
  in
  assert_equal_query
    "parse_query parses ignored scalar :in bindings"
    [ [ Result_entity 1 ] ]
    (q
       ~inputs:
         [ Arg_scalar (Result_value (String "ignored"))
         ; Arg_scalar (Result_value (String "Ivan"))
         ]
       db
       (parse_query ignored_scalar_query));
  let tuple_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormVector [ QueryFormSymbol "?first"; QueryFormSymbol "?last" ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "first"; QueryFormSymbol "?first" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "last"; QueryFormSymbol "?last" ]
      ]
  in
  assert_equal_query
    "parse_query parses tuple :in bindings"
    [ [ Result_entity 1 ] ]
    (q
       ~inputs:[ Arg_tuple [ Result_value (String "Ivan"); Result_value (String "Petrov") ] ]
       db
       (parse_query tuple_query));
  assert_equal_query
    "parse_query accepts scalar sequential values for tuple :in bindings"
    [ [ Result_entity 1 ] ]
    (q
       ~inputs:[ Arg_scalar (Result_value (List [ String "Ivan"; String "Petrov" ])) ]
       db
       (parse_query tuple_query));
  let nested_tuple_collection_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormSymbol "?tag"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormVector
          [ QueryFormSymbol "?first"
          ; QueryFormVector [ QueryFormSymbol "?tag"; QueryFormSymbol "..." ]
          ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "first"; QueryFormSymbol "?first" ]
      ]
  in
  assert_equal_query
    "parse_query parses nested collection bindings inside tuple :in bindings"
    [ [ Result_entity 1; Result_value (String "blue") ]
    ; [ Result_entity 1; Result_value (String "green") ]
    ]
    (q
       ~inputs:
         [ Arg_tuple
             [ Result_value (String "Ivan")
             ; Result_value (List [ String "blue"; String "green" ])
             ]
         ]
       db
       (parse_query nested_tuple_collection_query));
  assert_equal_query
    "parse_query accepts scalar sequential values for nested tuple :in bindings"
    [ [ Result_entity 1; Result_value (String "blue") ]
    ; [ Result_entity 1; Result_value (String "green") ]
    ]
    (q
       ~inputs:
         [ Arg_scalar (Result_value (List [ String "Ivan"; List [ String "blue"; String "green" ] ])) ]
       db
       (parse_query nested_tuple_collection_query));
  let relation_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormVector [ QueryFormVector [ QueryFormSymbol "?first"; QueryFormSymbol "?last" ] ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "first"; QueryFormSymbol "?first" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "last"; QueryFormSymbol "?last" ]
      ]
  in
  assert_equal_query
    "parse_query parses relation :in bindings"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q
       ~inputs:
         [ Arg_relation
             [ [ Result_value (String "Ivan"); Result_value (String "Petrov") ]
             ; [ Result_value (String "Petr"); Result_value (String "Ivanov") ]
             ]
       ]
       db
       (parse_query relation_query));
  assert_equal_query
    "parse_query accepts collection arguments for relation :in bindings"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q
       ~inputs:
         [ Arg_collection
             [ Result_value (List [ String "Ivan"; String "Petrov" ])
             ; Result_value (List [ String "Petr"; String "Ivanov" ])
             ]
       ]
       db
       (parse_query relation_query));
  assert_equal_query
    "parse_query accepts scalar collection values for relation :in bindings"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q
       ~inputs:
         [ Arg_scalar
             (Result_value
                (List
                   [ List [ String "Ivan"; String "Petrov" ]
                   ; List [ String "Petr"; String "Ivanov" ]
                   ]))
         ]
       db
       (parse_query relation_query));
  let map_relation_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?k"
      ; QueryFormSymbol "?v"
      ; QueryFormKeyword "in"
      ; QueryFormVector
          [ QueryFormVector [ QueryFormSymbol "?k"; QueryFormSymbol "?v" ]
          ; QueryFormSymbol "..."
          ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol ">"; QueryFormSymbol "?v"; QueryFormInt 1 ] ]
      ]
  in
  assert_equal_query
    "parse_query accepts map scalar arguments for relation :in bindings"
    [ [ Result_value (Keyword "b"); Result_value (Int 2) ]
    ; [ Result_value (Keyword "c"); Result_value (Int 3) ]
    ]
    (q
       ~inputs:
         [ Arg_scalar
             (Result_value (Map [ Keyword "a", Int 1; Keyword "b", Int 2; Keyword "c", Int 3 ]))
         ]
       db
       (parse_query map_relation_query));
  let nested_relation_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormSymbol "?tag"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormVector
          [ QueryFormVector
              [ QueryFormSymbol "?first"
              ; QueryFormVector [ QueryFormSymbol "?tag"; QueryFormSymbol "..." ]
              ]
          ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "first"; QueryFormSymbol "?first" ]
      ]
  in
  assert_equal_query
    "parse_query parses nested collection bindings inside relation :in bindings"
    [ [ Result_entity 1; Result_value (String "blue") ]
    ; [ Result_entity 1; Result_value (String "green") ]
    ; [ Result_entity 2; Result_value (String "red") ]
    ]
    (q
       ~inputs:
         [ Arg_relation
             [ [ Result_value (String "Ivan")
               ; Result_value (List [ String "blue"; String "green" ])
               ]
             ; [ Result_value (String "Petr")
               ; Result_value (List [ String "red" ])
               ]
             ]
         ]
       db
       (parse_query nested_relation_query));
  assert_equal_query
    "parse_query accepts collection arguments for nested relation :in bindings"
    [ [ Result_entity 1; Result_value (String "blue") ]
    ; [ Result_entity 1; Result_value (String "green") ]
    ; [ Result_entity 2; Result_value (String "red") ]
    ]
    (q
       ~inputs:
         [ Arg_collection
             [ Result_value (List [ String "Ivan"; List [ String "blue"; String "green" ] ])
             ; Result_value (List [ String "Petr"; List [ String "red" ] ])
             ]
       ]
       db
       (parse_query nested_relation_query));
  assert_equal_query
    "parse_query accepts scalar collection values for nested relation :in bindings"
    [ [ Result_entity 1; Result_value (String "blue") ]
    ; [ Result_entity 1; Result_value (String "green") ]
    ; [ Result_entity 2; Result_value (String "red") ]
    ]
    (q
       ~inputs:
         [ Arg_scalar
             (Result_value
                (List
                   [ List [ String "Ivan"; List [ String "blue"; String "green" ] ]
                   ; List [ String "Petr"; List [ String "red" ] ]
                   ]))
         ]
       db
       (parse_query nested_relation_query));
  let map_nested_relation_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?k"
      ; QueryFormSymbol "?min"
      ; QueryFormSymbol "?max"
      ; QueryFormKeyword "in"
      ; QueryFormVector
          [ QueryFormVector
              [ QueryFormSymbol "?k"
              ; QueryFormVector [ QueryFormSymbol "?min"; QueryFormSymbol "?max" ]
              ]
          ; QueryFormSymbol "..."
          ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol ">"; QueryFormSymbol "?max"; QueryFormSymbol "?min" ] ]
      ]
  in
  assert_equal_query
    "parse_query accepts map scalar arguments for nested relation :in bindings"
    [ [ Result_value (Keyword "a"); Result_value (Int 1); Result_value (Int 4) ]
    ; [ Result_value (Keyword "b"); Result_value (Int 5); Result_value (Int 7) ]
    ]
    (q
       ~inputs:
         [ Arg_scalar
             (Result_value
                (Map
                   [ Keyword "a", List [ Int 1; Int 4 ]
                   ; Keyword "b", List [ Int 5; Int 7 ]
                   ; Keyword "c", List [ Int 3; Int 3 ]
                   ]))
         ]
       db
       (parse_query map_nested_relation_query));
  assert_raises_invalid_arg_message
    "q rejects nested relation rows with mismatched arity"
    "relation input row arity mismatch"
    (fun () ->
       ignore
         (q
            ~inputs:[ Arg_relation [ [ Result_value (String "Ivan") ] ] ]
            db
            (parse_query nested_relation_query)));
  let relation_ellipsis_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormVector
          [ QueryFormVector [ QueryFormSymbol "?first"; QueryFormSymbol "?last" ]
          ; QueryFormSymbol "..."
          ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "first"; QueryFormSymbol "?first" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "last"; QueryFormSymbol "?last" ]
      ]
  in
  assert_equal_query
    "parse_query parses relation :in bindings with ellipsis"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q
       ~inputs:
         [ Arg_relation
             [ [ Result_value (String "Ivan"); Result_value (String "Petrov") ]
             ; [ Result_value (String "Petr"); Result_value (String "Ivanov") ]
             ]
         ]
       db
       (parse_query relation_ellipsis_query));
  let relation_list_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormList
          [ QueryFormList [ QueryFormSymbol "?first"; QueryFormSymbol "?last" ]
          ; QueryFormSymbol "..."
          ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "first"; QueryFormSymbol "?first" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "last"; QueryFormSymbol "?last" ]
      ]
  in
  assert_equal_query
    "parse_query parses list-form relation :in bindings"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q
       ~inputs:
         [ Arg_relation
             [ [ Result_value (String "Ivan"); Result_value (String "Petrov") ]
             ; [ Result_value (String "Petr"); Result_value (String "Ivanov") ]
             ]
         ]
       db
       (parse_query relation_list_query));
  assert_raises_invalid_arg
    "q rejects missing parsed :in arguments"
    (fun () -> ignore (q db (parse_query scalar_query)));
  assert_raises_invalid_arg
    "q rejects extra parsed :in arguments"
    (fun () ->
       ignore
         (q
            ~inputs:
              [ Arg_scalar (Result_value (String "Ivan"))
              ; Arg_scalar (Result_value (String "Petr"))
              ]
            db
            (parse_query scalar_query)))

let test_parse_query_input_helper_parsers () =
  assert_equal_value
    "parse_binding parses scalar variables"
    (Bind_scalar "x")
    (parse_binding (QueryFormSymbol "?x"));
  assert_equal_value
    "parse_binding parses ignored bindings"
    Bind_ignore
    (parse_binding (QueryFormSymbol "_"));
  assert_equal_value
    "parse_binding parses collection bindings"
    (Bind_collection (Bind_scalar "x"))
    (parse_binding (QueryFormVector [ QueryFormSymbol "?x"; QueryFormSymbol "..." ]));
  assert_equal_value
    "parse_binding parses single-value tuple bindings"
    (Bind_tuple [ Bind_scalar "x" ])
    (parse_binding (QueryFormVector [ QueryFormSymbol "?x" ]));
  assert_equal_value
    "parse_binding parses multi-value tuple bindings"
    (Bind_tuple [ Bind_scalar "x"; Bind_scalar "y" ])
    (parse_binding (QueryFormVector [ QueryFormSymbol "?x"; QueryFormSymbol "?y" ]));
  assert_equal_value
    "parse_binding parses ignored tuple elements"
    (Bind_tuple [ Bind_ignore; Bind_scalar "y" ])
    (parse_binding (QueryFormVector [ QueryFormSymbol "_"; QueryFormSymbol "?y" ]));
  assert_equal_value
    "parse_binding parses nested collection tuple bindings"
    (Bind_collection (Bind_tuple [ Bind_ignore; Bind_collection (Bind_scalar "x") ]))
    (parse_binding
       (QueryFormVector
          [ QueryFormVector
              [ QueryFormSymbol "_"
              ; QueryFormVector [ QueryFormSymbol "?x"; QueryFormSymbol "..." ]
              ]
          ; QueryFormSymbol "..."
          ]));
  assert_equal_value
    "parse_binding parses relation-style tuple bindings"
    (Bind_collection (Bind_tuple [ Bind_scalar "a"; Bind_scalar "b"; Bind_scalar "c" ]))
    (parse_binding
       (QueryFormVector
          [ QueryFormVector [ QueryFormSymbol "?a"; QueryFormSymbol "?b"; QueryFormSymbol "?c" ] ]));
  assert_raises_invalid_arg
    "parse_binding rejects invalid binding forms"
    (fun () -> ignore (parse_binding (QueryFormKeyword "db/id")));
  assert_equal_value
    "parse_in parses scalar, source, ignore, collection, and relation declarations"
    [ Input_scalar_decl "x"
    ; Input_source_decl "$"
    ; Input_source_decl "users"
    ; Input_ignore_decl
    ; Input_collection_decl "name"
    ; Input_relation_decl [ "a"; "b"; "c" ]
    ]
    (parse_in
       (QueryFormVector
          [ QueryFormSymbol "?x"
          ; QueryFormSymbol "$"
          ; QueryFormSymbol "$users"
          ; QueryFormSymbol "_"
          ; QueryFormVector [ QueryFormSymbol "?name"; QueryFormSymbol "..." ]
          ; QueryFormVector
              [ QueryFormVector [ QueryFormSymbol "?a"; QueryFormSymbol "?b"; QueryFormSymbol "?c" ]
              ; QueryFormSymbol "..."
              ]
          ]));
  assert_equal_value
    "parse_in parses nested relation declarations"
    [ Input_source_decl "$"
    ; Input_nested_relation_decl [ Bind_ignore; Bind_collection (Bind_scalar "x") ]
    ]
    (parse_in
       (QueryFormVector
          [ QueryFormSymbol "$"
          ; QueryFormVector
              [ QueryFormVector
                  [ QueryFormSymbol "_"
                  ; QueryFormVector [ QueryFormSymbol "?x"; QueryFormSymbol "..." ]
                  ]
              ; QueryFormSymbol "..."
              ]
          ]));
  assert_equal_value
    "parse_in parses rules vars"
    [ Input_rules_decl; Input_scalar_decl "x" ]
    (parse_in (QueryFormVector [ QueryFormSymbol "%"; QueryFormSymbol "?x" ]));
  assert_raises_invalid_arg
    "parse_in rejects non-sequential forms"
    (fun () -> ignore (parse_in (QueryFormKeyword "bad")));
  assert_equal_value
    "parse_with parses variable declarations"
    [ "x"; "y" ]
    (parse_with (QueryFormVector [ QueryFormSymbol "?x"; QueryFormSymbol "?y" ]));
  assert_raises_invalid_arg
    "parse_with rejects ignored bindings"
    (fun () -> ignore (parse_with (QueryFormVector [ QueryFormSymbol "?x"; QueryFormSymbol "_" ])))

let test_parse_query_find_helper_parser () =
  assert_equal_value
    "parse_find parses relation find specs"
    (Return_relation, [ Find_var "a"; Find_var "b" ])
    (parse_find (QueryFormVector [ QueryFormSymbol "?a"; QueryFormSymbol "?b" ]));
  assert_equal_value
    "parse_find parses collection find specs"
    (Return_collection, [ Find_var "a" ])
    (parse_find
       (QueryFormVector
          [ QueryFormVector [ QueryFormSymbol "?a"; QueryFormSymbol "..." ] ]));
  assert_equal_value
    "parse_find parses scalar find specs"
    (Return_scalar, [ Find_var "a" ])
    (parse_find (QueryFormVector [ QueryFormSymbol "?a"; QueryFormSymbol "." ]));
  assert_equal_value
    "parse_find parses tuple find specs"
    (Return_tuple, [ Find_var "a"; Find_var "b" ])
    (parse_find
       (QueryFormVector
          [ QueryFormVector [ QueryFormSymbol "?a"; QueryFormSymbol "?b" ] ]));
  assert_equal_value
    "parse_find parses relation aggregate find specs"
    (Return_relation, [ Find_var "a"; Find_aggregate (Count, [ QVar "b" ]) ])
    (parse_find
       (QueryFormVector
          [ QueryFormSymbol "?a"
          ; QueryFormList [ QueryFormSymbol "count"; QueryFormSymbol "?b" ]
          ]));
  assert_equal_value
    "parse_find parses collection aggregate find specs"
    (Return_collection, [ Find_aggregate (Count, [ QVar "a" ]) ])
    (parse_find
       (QueryFormVector
          [ QueryFormVector
              [ QueryFormList [ QueryFormSymbol "count"; QueryFormSymbol "?a" ]
              ; QueryFormSymbol "..."
              ]
          ]));
  assert_equal_value
    "parse_find parses scalar custom aggregate find specs"
    (Return_scalar, [ Find_aggregate (CustomVar "f", [ QVar "a" ]) ])
    (parse_find
       (QueryFormVector
          [ QueryFormList [ QueryFormSymbol "aggregate"; QueryFormSymbol "?f"; QueryFormSymbol "?a" ]
          ; QueryFormSymbol "."
          ]));
  assert_equal_value
    "parse_find parses tuple custom aggregate find specs"
    (Return_tuple, [ Find_aggregate (CustomVar "f", [ QVar "a" ]); Find_var "b" ])
    (parse_find
       (QueryFormVector
          [ QueryFormVector
              [ QueryFormList [ QueryFormSymbol "aggregate"; QueryFormSymbol "?f"; QueryFormSymbol "?a" ]
              ; QueryFormSymbol "?b"
              ]
          ]));
  assert_equal_value
    "parse_find parses parameterized aggregate find specs"
    (Return_scalar, [ Find_aggregate (MaxN 3, [ QVar "a" ]) ])
    (parse_find
       (QueryFormVector
          [ QueryFormList [ QueryFormSymbol "max"; QueryFormInt 3; QueryFormSymbol "?a" ]
          ; QueryFormSymbol "."
          ]));
  assert_equal_value
    "parse_find preserves structured aggregate arguments"
    (Return_scalar, [ Find_aggregate (Count, [ QVar "b"; QValue (Int 1); QSource "x" ]) ])
    (parse_find
       (QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "count"
              ; QueryFormSymbol "?b"
              ; QueryFormInt 1
              ; QueryFormSymbol "$x"
              ]
          ; QueryFormSymbol "."
          ]));
  assert_raises_invalid_arg
    "parse_find rejects invalid forms"
    (fun () -> ignore (parse_find (QueryFormKeyword "find")))

let test_parse_query_or_join_required_vars () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "age", One_value (Int 10) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "age", One_value (Int 11) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg") ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "age", One_value (Int 10); "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 5); attrs = [ "age", One_value (Int 11); "name", One_value (String "Oleg") ] }
         ]
  in
  let query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "?a"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormSymbol "or-join"
          ; QueryFormVector [ QueryFormVector [ QueryFormSymbol "?a" ]; QueryFormSymbol "?e" ]
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormSymbol "?a" ]
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Oleg" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses or-join required vars"
    [ [ Result_entity 1 ]; [ Result_entity 3 ]; [ Result_entity 4 ]; [ Result_entity 5 ] ]
    (q ~inputs:[ Arg_scalar (Result_value (Int 10)) ] db (parse_query query));
  let list_rule_vars_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "?a"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormSymbol "or-join"
          ; QueryFormList [ QueryFormList [ QueryFormSymbol "?a" ]; QueryFormSymbol "?e" ]
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormSymbol "?a" ]
          ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Oleg" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses or-join list-form required vars"
    [ [ Result_entity 1 ]; [ Result_entity 3 ]; [ Result_entity 4 ]; [ Result_entity 5 ] ]
    (q ~inputs:[ Arg_scalar (Result_value (Int 10)) ] db (parse_query list_rule_vars_query));
  let source_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$people"
      ; QueryFormSymbol "?a"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormSymbol "$people"
          ; QueryFormVector
              [ QueryFormSymbol "or-join"
              ; QueryFormVector [ QueryFormVector [ QueryFormSymbol "?a" ]; QueryFormSymbol "?e" ]
              ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormSymbol "?a" ]
              ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Oleg" ]
              ]
          ]
      ]
  in
  assert_equal_query
    "parse_query parses source-qualified or-join required vars"
    [ [ Result_entity 1 ]; [ Result_entity 3 ]; [ Result_entity 4 ]; [ Result_entity 5 ] ]
    (q_sources
       ~inputs:[ Arg_scalar (Result_value (Int 10)) ]
       (empty_db ())
       [ "people", Db_source db ]
       (parse_query source_query));
  assert_raises_invalid_arg
    "parse_query rejects duplicate or-join rule vars"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "where"
               ; QueryFormVector
                   [ QueryFormSymbol "or-join"
                   ; QueryFormVector [ QueryFormVector [ QueryFormSymbol "?e" ]; QueryFormSymbol "?e" ]
                   ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
                   ]
               ])))

let test_parse_query_where_clause_validation_messages () =
  let query_with_where_clause clause =
    QueryFormVector
      [ QueryFormKeyword "find"; QueryFormSymbol "?e"; QueryFormKeyword "where"; clause ]
  in
  assert_raises_invalid_arg_message
    "parse_query reports empty not-join vars like upstream"
    "Join variables should not be empty"
    (fun () ->
       ignore
         (parse_query
            (query_with_where_clause
               (QueryFormVector
                  [ QueryFormSymbol "not-join"
                  ; QueryFormVector []
                  ; QueryFormVector [ QueryFormSymbol "?e" ]
                  ]))));
  assert_raises_invalid_arg_message
    "parse_query reports not with no free vars like upstream"
    "Join variables should not be empty"
    (fun () ->
       ignore
         (parse_query
            (query_with_where_clause
               (QueryFormVector [ QueryFormSymbol "not"; QueryFormVector [ QueryFormSymbol "_" ] ]))));
  assert_raises_invalid_arg_message
    "parse_query reports malformed not-join like upstream"
    "Cannot parse 'not-join' clause"
    (fun () ->
       ignore
         (parse_query
            (query_with_where_clause
               (QueryFormVector [ QueryFormSymbol "not-join"; QueryFormVector [ QueryFormSymbol "?e" ] ]))));
  assert_raises_invalid_arg_message
    "parse_query reports empty not like upstream"
    "Cannot parse 'not' clause"
    (fun () -> ignore (parse_query (query_with_where_clause (QueryFormVector [ QueryFormSymbol "not" ]))));
  assert_raises_invalid_arg_message
    "parse_query reports empty or rule vars like upstream"
    "Join variables should not be empty"
    (fun () ->
       ignore
         (parse_query
            (query_with_where_clause
               (QueryFormVector [ QueryFormSymbol "or"; QueryFormVector [ QueryFormSymbol "_" ] ]))));
  assert_raises_invalid_arg_message
    "parse_query reports malformed or-join rule vars like upstream"
    "Cannot parse rule-vars"
    (fun () ->
       ignore
         (parse_query
            (query_with_where_clause
               (QueryFormVector
                  [ QueryFormSymbol "or-join"; QueryFormVector []; QueryFormVector [ QueryFormSymbol "?e" ] ]))));
  assert_raises_invalid_arg_message
    "parse_query reports malformed or-join like upstream"
    "Cannot parse 'or-join' clause"
    (fun () ->
       ignore
         (parse_query
            (query_with_where_clause
               (QueryFormVector [ QueryFormSymbol "or-join"; QueryFormVector [ QueryFormSymbol "?e" ] ]))));
  assert_raises_invalid_arg_message
    "parse_query reports empty or like upstream"
    "Cannot parse 'or' clause"
    (fun () -> ignore (parse_query (query_with_where_clause (QueryFormVector [ QueryFormSymbol "or" ]))))

let test_parse_query_validates_structure () =
  assert_raises_invalid_arg
    "parse_query rejects duplicate :in variables"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "in"
               ; QueryFormSymbol "$"
               ; QueryFormSymbol "?e"
               ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormSymbol "..." ]
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
               ])));
  assert_raises_invalid_arg
    "parse_query rejects duplicate default :in sources"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "in"
               ; QueryFormSymbol "$"
               ; QueryFormSymbol "$"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "$"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
               ])));
  assert_raises_invalid_arg
    "parse_query rejects duplicate named :in sources"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "in"
               ; QueryFormSymbol "$"
               ; QueryFormSymbol "$people"
               ; QueryFormSymbol "$people"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "$people"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
               ])));
  assert_raises_invalid_arg
    "parse_query rejects duplicate :in rules vars"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?to"
               ; QueryFormKeyword "in"
               ; QueryFormSymbol "$"
               ; QueryFormSymbol "%"
               ; QueryFormSymbol "%"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ] ]
               ; QueryFormKeyword "rules"
               ; QueryFormVector
                   [ QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ]
                   ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "follow"; QueryFormSymbol "?to" ]
                   ]
               ])));
  assert_raises_invalid_arg
    "parse_query rejects :find variables missing from :where and :in"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?missing"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
               ])));
  assert_raises_invalid_arg
    "parse_query rejects rule branches with mismatched arity"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?to"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ] ]
               ; QueryFormKeyword "rules"
               ; QueryFormVector
                   [ QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from"; QueryFormSymbol "?to" ]
                   ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "follow"; QueryFormSymbol "?to" ]
                   ]
               ; QueryFormVector
                   [ QueryFormList [ QueryFormSymbol "follows"; QueryFormSymbol "?from" ]
                   ; QueryFormVector [ QueryFormSymbol "?from"; QueryFormKeyword "follow"; QueryFormSymbol "?to" ]
                   ]
               ])))
  ;
  assert_raises_invalid_arg
    "parse_query rejects external rule calls without rules input"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "where"
               ; QueryFormList [ QueryFormSymbol "known"; QueryFormSymbol "?e" ]
               ])));
  assert_raises_invalid_arg_message
    "parse_query rejects vector rule invocations without arguments"
    "rule-expr requires at least one argument"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "in"
               ; QueryFormSymbol "%"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "known" ]
               ])));
  assert_raises_invalid_arg_message
    "parse_query rejects list rule invocations without arguments"
    "rule-expr requires at least one argument"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "in"
               ; QueryFormSymbol "%"
               ; QueryFormKeyword "where"
               ; QueryFormList [ QueryFormSymbol "known" ]
               ])));
  assert_raises_invalid_arg_message
    "parse_query rejects source-qualified list rule invocations without arguments"
    "rule-expr requires at least one argument"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "in"
               ; QueryFormSymbol "$"
               ; QueryFormSymbol "$people"
               ; QueryFormSymbol "%"
               ; QueryFormKeyword "where"
               ; QueryFormList [ QueryFormSymbol "$people"; QueryFormSymbol "known" ]
               ])));
  assert_raises_invalid_arg_message
    "parse_query rejects source-qualified vector rule invocations without arguments"
    "rule-expr requires at least one argument"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "in"
               ; QueryFormSymbol "$"
               ; QueryFormSymbol "$people"
               ; QueryFormSymbol "%"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "$people"; QueryFormSymbol "known" ]
               ])));
  assert_raises_invalid_arg
    "parse_query rejects undeclared named sources in where"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?e"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "$people"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
               ])));
  assert_raises_invalid_arg_message
    "parse_query rejects undeclared named sources inside dynamic predicate args"
    "Where uses unknown source vars: [$missing]"
    (fun () ->
       ignore
         (parse_query_string
            "[:find ?x
              :in ?pred
              :where [(ground 1) ?x]
                     [(?pred $missing ?x)]]"));
  assert_raises_invalid_arg_message
    "parse_query rejects undeclared default source inside dynamic predicate args"
    "Where uses unknown source vars: [$]"
    (fun () ->
       ignore
         (parse_query_string
            "[:find ?x
              :in $named ?pred
              :where [$named ?x]
                     [(?pred $ ?x)]]"));
  assert_raises_invalid_arg
    "parse_query rejects undeclared named sources in pull find"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormList
                   [ QueryFormSymbol "pull"
                   ; QueryFormSymbol "$people"
                   ; QueryFormSymbol "?e"
                   ; QueryFormVector [ QueryFormKeyword "name" ]
                   ]
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
               ])))

let test_parse_query_with_vars () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "monster", One_value (String "Medusa"); "heads", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "monster", One_value (String "Cyclops"); "heads", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "monster", One_value (String "Chimera"); "heads", One_value (Int 1) ] }
         ]
  in
  let aggregate_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormList [ QueryFormSymbol "count"; QueryFormSymbol "?heads" ]
      ; QueryFormKeyword "with"
      ; QueryFormSymbol "?monster"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "monster"; QueryFormSymbol "?monster" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "heads"; QueryFormSymbol "?heads" ]
      ]
  in
  assert_equal_query
    "parse_query applies :with vars to aggregate duplicate preservation"
    [ [ Result_value (Int 3) ] ]
    (q db (parse_query aggregate_query));
  let relation_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?heads"
      ; QueryFormKeyword "with"
      ; QueryFormSymbol "?monster"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "monster"; QueryFormSymbol "?monster" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "heads"; QueryFormSymbol "?heads" ]
      ]
  in
  assert_equal_query
    "parse_query applies :with vars to non-aggregate duplicate preservation"
    [ [ Result_value (Int 1) ]; [ Result_value (Int 1) ]; [ Result_value (Int 1) ] ]
    (q db (parse_query relation_query));
  assert_raises_invalid_arg
    "parse_query rejects duplicate :with variables"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?heads"
               ; QueryFormKeyword "with"
               ; QueryFormSymbol "?monster"
               ; QueryFormSymbol "?monster"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "monster"; QueryFormSymbol "?monster" ]
               ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "heads"; QueryFormSymbol "?heads" ]
               ])));
  assert_raises_invalid_arg
    "parse_query rejects variables shared by :find and :with"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?heads"
               ; QueryFormKeyword "with"
               ; QueryFormSymbol "?heads"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "heads"; QueryFormSymbol "?heads" ]
               ])))
  ;
  assert_raises_invalid_arg
    "parse_query rejects :with variables missing from :where and :in"
    (fun () ->
       ignore
         (parse_query
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?heads"
               ; QueryFormKeyword "with"
               ; QueryFormSymbol "?missing"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "heads"; QueryFormSymbol "?heads" ]
               ])))

let test_parse_query_matches_upstream_validation_messages () =
  assert_raises_invalid_arg_message
    "parse_query reports unknown find vars like upstream"
    "Query for unknown vars: [?e]"
    (fun () -> ignore (parse_query_string "[:find ?e :where [?x]]"));
  assert_raises_invalid_arg_message
    "parse_query reports unknown with vars like upstream"
    "Query for unknown vars: [?f]"
    (fun () -> ignore (parse_query_string "[:find ?e :with ?f :where [?e]]"));
  assert_raises_invalid_arg_message
    "parse_query reports multiple unknown vars like upstream"
    "Query for unknown vars: [?t]"
    (fun () -> ignore (parse_query_string "[:find ?e ?x ?t :in ?x :where [?e]]"));
  assert_raises_invalid_arg_message
    "parse_query reports find/with overlap like upstream"
    ":find and :with should not use same variables: [?e]"
    (fun () -> ignore (parse_query_string "[:find ?x ?e :with ?y ?e :where [?x ?e ?y]]"));
  assert_raises_invalid_arg_message
    "parse_query reports duplicate default source vars like upstream"
    "Vars used in :in should be distinct"
    (fun () -> ignore (parse_query_string "[:find ?e :in $ $ ?x :where [?e]]"));
  assert_raises_invalid_arg_message
    "parse_query reports duplicate scalar input vars like upstream"
    "Vars used in :in should be distinct"
    (fun () -> ignore (parse_query_string "[:find ?e :in ?x $ ?x :where [?e]]"));
  assert_raises_invalid_arg_message
    "parse_query reports duplicate rules vars like upstream"
    "Vars used in :in should be distinct"
    (fun () -> ignore (parse_query_string "[:find ?e :in $ % ?x % :where [?e]]"));
  assert_raises_invalid_arg_message
    "parse_query reports duplicate with vars like upstream"
    "Vars used in :with should be distinct"
    (fun () -> ignore (parse_query_string "[:find ?n :with ?e ?f ?e :where [?e ?f ?n]]"));
  assert_raises_invalid_arg_message
    "parse_query reports unknown where source vars like upstream"
    "Where uses unknown source vars: [$1]"
    (fun () -> ignore (parse_query_string "[:find ?x :where [$1 ?x]]"));
  assert_raises_invalid_arg_message
    "parse_query reports unknown declared source vars like upstream"
    "Where uses unknown source vars: [$2]"
    (fun () -> ignore (parse_query_string "[:find ?x :in $1 :where [$2 ?x]]"));
  assert_raises_invalid_arg_message
    "parse_query reports missing rules var like upstream"
    "Missing rules var '%' in :in"
    (fun () -> ignore (parse_query_string "[:find ?e :where (rule ?e)]"))

let test_parse_query_with_and_rules_match_upstream_messages () =
  assert_raises_invalid_arg_message
    "parse_query rejects :with placeholders like upstream parser"
    "Cannot parse :with clause"
    (fun () -> ignore (parse_query_string "[:find ?e :with ?e _ :where [?e]]"));
  assert_raises_invalid_arg_message
    "parse_query rejects rules without body clauses like upstream parser-rules"
    "Rule branch should have clauses"
    (fun () ->
       ignore
         (parse_query_string
            "[:find ?e :in % :where (rule ?e)
             :rules [[(rule ?x)]]]"));
  assert_raises_invalid_arg_message
    "parse_query rejects rules with arity mismatch like upstream parser-rules"
    "Arity mismatch"
    (fun () ->
       ignore
         (parse_query_string
            "[:find ?e :in % :where (rule ?e)
             :rules [[(rule ?x) [_]]
                     [(rule ?x ?y) [_]]]]"));
  assert_raises_invalid_arg_message
    "parse_query rejects duplicate rule vars like upstream parser-rules"
    "Rule variables should be distinct"
    (fun () ->
       ignore
         (parse_query_string
            "[:find ?e :in % :where (rule ?e)
             :rules [[(rule ?x ?y ?x) [_]]]]"));
  assert_raises_invalid_arg_message
    "parse_query rejects duplicate required rule vars like upstream parser-rules"
    "Rule variables should be distinct"
    (fun () ->
       ignore
         (parse_query_string
            "[:find ?e :in % :where (rule ?e)
             :rules [[(rule [?x ?y] ?z ?x) [_]]]]"));
  assert_raises_invalid_arg_message
    "parse_query rejects non-query vars in rule heads like upstream query-rules issue-300"
    "Cannot parse var, expected symbol starting with ?, got: $e1"
    (fun () ->
       ignore
         (parse_query_string
            "[:find ?e :in $ % :where [?e]
             :rules [[(rule $e1 ?e2)
                      [?e1 :ref ?e2]]]]"))

let test_q_input_arity_matches_upstream_validation_messages () =
  let db = empty_db () in
  assert_raises_invalid_arg_message
    "q reports too few supplied input args like upstream query-v3"
    "Wrong number of arguments for bindings [$ ?a], 2 required, 1 provided"
    (fun () ->
       ignore
         (q_string db "[:find ?a :in $ ?a]" ~inputs:[]));
  assert_raises_invalid_arg_message
    "q reports too many supplied input args for explicit source input like upstream query-v3"
    "Wrong number of arguments for bindings [$], 1 required, 2 provided"
    (fun () ->
       ignore
         (q_string
            db
            "[:find ?a :in $ :where [?a]]"
            ~inputs:[ Arg_scalar (Result_value (Int 1)) ]));
  assert_raises_invalid_arg_message
    "q reports too many supplied input args for inferred default source like upstream query-v3"
    "Wrong number of arguments for bindings [$], 1 required, 2 provided"
    (fun () ->
       ignore
         (q_string
            db
            "[:find ?a :where [?a]]"
            ~inputs:[ Arg_scalar (Result_value (Int 1)) ]))

let test_q_input_binding_matches_upstream_validation_messages () =
  let db = empty_db () in
  assert_raises_invalid_arg_message
    "q reports scalar supplied to tuple binding like upstream"
    "Cannot bind value :a to tuple [?a ?b]"
    (fun () ->
       ignore
         (q_string
            db
            "[:find ?a ?b :in [?a ?b]]"
            ~inputs:[ Arg_scalar (Result_value (Keyword "a")) ]));
  assert_raises_invalid_arg_message
    "q reports scalar supplied to collection binding like upstream"
    "Cannot bind value :a to collection [?a ...]"
    (fun () ->
       ignore
         (q_string
            db
            "[:find ?a :in [?a ...]]"
            ~inputs:[ Arg_scalar (Result_value (Keyword "a")) ]));
  assert_raises_invalid_arg_message
    "q reports short tuple input like upstream"
    "Not enough elements in a collection [:a] to bind tuple [?a ?b]"
    (fun () ->
       ignore
         (q_string
            db
            "[:find ?a ?b :in [?a ?b]]"
            ~inputs:[ Arg_scalar (Result_value (List [ Keyword "a" ])) ]))

let test_parse_query_map_sections_accept_list_sequences () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 31) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 24) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 17) ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 44) ] }
         ]
  in
  let query =
    QueryFormMap
      [ QueryFormKeyword "find", QueryFormList [ QueryFormList [ QueryFormSymbol "count"; QueryFormSymbol "?age" ] ]
      ; QueryFormKeyword "in", QueryFormList [ QueryFormSymbol "$"; QueryFormSymbol "?wanted" ]
      ; QueryFormKeyword "with", QueryFormList [ QueryFormSymbol "?e" ]
      ; QueryFormKeyword "where",
        QueryFormList
          [ QueryFormList
              [ QueryFormSymbol "adult-name"
              ; QueryFormSymbol "?e"
              ; QueryFormSymbol "?wanted"
              ; QueryFormSymbol "?age"
              ]
          ]
      ; QueryFormKeyword "rules",
        QueryFormList
          [ QueryFormList
              [ QueryFormList
                  [ QueryFormSymbol "adult-name"
                  ; QueryFormSymbol "?entity"
                  ; QueryFormSymbol "?name"
                  ; QueryFormSymbol "?age"
                  ]
              ; QueryFormVector [ QueryFormSymbol "?entity"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
              ; QueryFormVector [ QueryFormSymbol "?entity"; QueryFormKeyword "age"; QueryFormSymbol "?age" ]
              ; QueryFormVector [ QueryFormList [ QueryFormSymbol ">="; QueryFormSymbol "?age"; QueryFormInt 18 ] ]
              ]
          ]
      ]
  in
  assert_equal_query
    "parse_query accepts list sequences in map query sections"
    [ [ Result_value (Int 2) ] ]
    (q ~inputs:[ Arg_scalar (Result_value (String "Ivan")) ] db (parse_query query))

let test_parse_query_concatenates_repeated_sections () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 31) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 44) ] }
         ]
  in
  let query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormInt 31 ]
      ]
  in
  assert_equal_query
    "parse_query should concatenate repeated :where sections"
    [ [ Result_value (String "Ivan") ] ]
    (q db (parse_query query));
  let with_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormList [ QueryFormSymbol "count"; QueryFormSymbol "?age" ]
      ; QueryFormKeyword "with"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "with"
      ; QueryFormSymbol "?name"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormSymbol "?age" ]
      ]
  in
  let parsed = parse_query with_query in
  if parsed.with_vars <> [ "e"; "name" ] then
    failwith "parse_query should concatenate repeated :with sections"

let test_q_binds_transaction_id_in_patterns () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ]
  in
  let query =
    { find = [ Find_var "name"; Find_var "tx" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ PatternTx (QVar "e", QAttr "name", QVar "name", QVar "tx") ]
    }
  in
  assert_equal_query
    "q binds transaction ids from four-term datom patterns"
    [ [ Result_value (String "Ivan"); Result_entity (tx0 + 1) ] ]
    (q db query)

let test_q_binds_transaction_operation_in_history_patterns () =
  let db =
    empty_db ()
    |> db_with [ Add (Entity_id 1, "name", String "Ivan") ]
    |> db_with [ Retract (Entity_id 1, "name", Some (String "Ivan")) ]
    |> history
  in
  let query =
    { find = [ Find_var "tx"; Find_var "op" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ PatternTxOp
            ( QEntity 1
            , QAttr "name"
            , QValue (String "Ivan")
            , QVar "tx"
            , QVar "op" )
        ]
    }
  in
  assert_equal_query
    "q binds transaction operations from five-term history patterns"
    [ [ Result_entity (tx0 + 1); Result_value (Keyword "db/add") ]
    ; [ Result_entity (tx0 + 2); Result_value (Keyword "db/retract") ]
    ]
    (q db query)

let test_q_joins_clauses () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "friend", One_value (Ref 2)
                 ]
             }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ]
  in
  let query =
    { find = [ Find_var "friend_name" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QEntity 1, QAttr "friend", QVar "friend")
        ; Pattern (QVar "friend", QAttr "name", QVar "friend_name")
        ]
    }
  in
  assert_equal_query
    "q joins variable bindings across clauses"
    [ [ Result_value (String "Petr") ] ]
    (q db query)

let test_q_short_data_patterns_match_upstream () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 15) ]
             }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "age", One_value (Int 37) ] }
         ]
  in
  assert_equal_query
    "q treats one-term data patterns as entity wildcards"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q_string db "[:find ?e :where [?e]]");
  assert_equal_query
    "q treats two-term data patterns as value wildcards"
    [ [ Result_entity 1 ] ]
    (q_string db "[:find ?e :where [?e :name]]")

let test_q_upstream_query_cljc_parity_batch () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 15) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 37) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 37) ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "age", One_value (Int 15) ] }
         ]
  in
  assert_equal_query
    "query.cljc test-joins finds entities with an attr"
    [ [ Result_entity 1 ]; [ Result_entity 2 ]; [ Result_entity 3 ] ]
    (q_string db "[:find ?e :where [?e :name]]");
  assert_equal_query
    "query.cljc test-joins joins constants and variables"
    [ [ Result_entity 1; Result_value (Int 15) ]
    ; [ Result_entity 3; Result_value (Int 37) ]
    ]
    (q_string
       db
       "[:find ?e ?v
         :where [?e :name \"Ivan\"]
                [?e :age ?v]]");
  assert_equal_query
    "query.cljc test-joins self-joins shared values"
    [ [ Result_entity 1; Result_entity 1 ]
    ; [ Result_entity 1; Result_entity 3 ]
    ; [ Result_entity 2; Result_entity 2 ]
    ; [ Result_entity 3; Result_entity 1 ]
    ; [ Result_entity 3; Result_entity 3 ]
    ]
    (q_string
       db
       "[:find ?e1 ?e2
         :where [?e1 :name ?n]
                [?e2 :name ?n]]");
  let many_db =
    empty_db ~schema:[ "aka", many ] ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 1, "aka", String "ivolga")
         ; Add (Entity_id 1, "aka", String "pi")
         ; Add (Entity_id 2, "name", String "Petr")
         ; Add (Entity_id 2, "aka", String "porosenok")
         ; Add (Entity_id 2, "aka", String "pi")
         ]
  in
  assert_equal_query
    "query.cljc test-q-many joins cardinality-many attrs"
    [ [ Result_value (String "Ivan"); Result_value (String "Ivan") ]
    ; [ Result_value (String "Ivan"); Result_value (String "Petr") ]
    ; [ Result_value (String "Petr"); Result_value (String "Ivan") ]
    ; [ Result_value (String "Petr"); Result_value (String "Petr") ]
    ]
    (q_string
       many_db
       "[:find ?n1 ?n2
         :where [?e1 :aka ?x]
                [?e2 :aka ?x]
                [?e1 :name ?n1]
                [?e2 :name ?n2]]");
  assert_equal_query
    "query.cljc test-built-in-get binds map inputs as relation rows"
    [ [ Result_value (Map [ Keyword "d", Int 2 ]); Result_value (Int 2) ] ]
    (q_string
       ~inputs:
         [ Arg_scalar
             (Result_value
                (Map
                   [ Keyword "a", Map [ Keyword "b", Int 1 ]
                   ; Keyword "c", Map [ Keyword "d", Int 2 ]
                   ]))
         ; Arg_scalar (Result_value (Keyword "d"))
         ]
       (empty_db ())
       "[:find ?m ?m-value
         :in [[?k ?m] ...] ?m-key
         :where [(get ?m ?m-key) ?m-value]]")

let test_query__test_joins () =
  test_q_upstream_query_cljc_parity_batch ()

let test_query__test_q_many () =
  test_q_upstream_query_cljc_parity_batch ()

let test_query__test_q_coll () =
  let relation =
    Relation_source
      [ [ Result_entity 1; Result_attr "name"; Result_value (String "Ivan") ]
      ; [ Result_entity 1; Result_attr "age"; Result_value (Int 19) ]
      ; [ Result_entity 1; Result_attr "aka"; Result_value (String "dragon_killer_94") ]
      ; [ Result_entity 1; Result_attr "aka"; Result_value (String "-=autobot=-") ]
      ]
  in
  assert_equal_query
    "query.cljc test-q-coll queries relation source datoms"
    [ [ Result_value (String "Ivan"); Result_value (Int 19) ] ]
    (q_sources_string
       (empty_db ())
       [ "$", relation ]
       "[:find ?n ?a
         :in $
         :where [?e :aka \"dragon_killer_94\"]
                [?e :name ?n]
                [?e :age ?a]]");
  let long_relation =
    Relation_source
      [ [ Result_entity 1
        ; Result_attr "name"
        ; Result_value (String "Ivan")
        ; Result_value (Int 945)
        ; Result_value (Keyword "db/add")
        ]
      ; [ Result_entity 1
        ; Result_attr "age"
        ; Result_value (Int 39)
        ; Result_value (Int 999)
        ; Result_value (Keyword "db/retract")
        ]
      ]
  in
  assert_equal_query
    "query.cljc test-q-coll matches short patterns over long tuples"
    [ [ Result_entity 1; Result_value (String "Ivan") ] ]
    (q_sources_string
       (empty_db ())
       [ "$", long_relation ]
       "[:find ?e ?v
         :in $
         :where [?e :name ?v]]");
  assert_equal_query
    "query.cljc test-q-coll matches full long tuples"
    [ [ Result_entity 1
      ; Result_attr "age"
      ; Result_value (Int 39)
      ; Result_value (Int 999)
      ]
    ]
    (q_sources_string
       (empty_db ())
       [ "$", long_relation ]
       "[:find ?e ?a ?v ?t
         :in $
         :where [?e ?a ?v ?t :db/retract]]")

let test_query__test_q_in () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 15) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 37) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 37) ] }
         ]
  in
  assert_equal_query
    "query.cljc test-q-in binds scalar attr and value inputs"
    [ [ Result_entity 1 ]; [ Result_entity 3 ] ]
    (q_string
       ~inputs:[ Arg_scalar (Result_attr "name"); Arg_scalar (Result_value (String "Ivan")) ]
       db
       "[:find ?e :in $ ?attr ?value :where [?e ?attr ?value]]");
  assert_equal_query
    "query.cljc test-q-in supports named db inputs"
    [ [ Result_attr "age"; Result_value (Int 15) ]
    ; [ Result_attr "name"; Result_value (String "Ivan") ]
    ]
    (q_sources_string
       ~inputs:[ Arg_scalar (Result_entity 1) ]
       (empty_db ())
       [ "db", Db_source db ]
       "[:find ?a ?v
         :in $db ?e
         :where [$db ?e ?a ?v]]");
  assert_equal_query
    "query.cljc test-q-in joins a db with a relation source"
    [ [ Result_entity 1; Result_value (String "ivan@mail.ru") ]
    ; [ Result_entity 2; Result_value (String "petr@gmail.com") ]
    ; [ Result_entity 3; Result_value (String "ivan@mail.ru") ]
    ]
    (q_sources_string
       db
       [ ( "b"
         , Relation_source
             [ [ Result_value (String "Ivan"); Result_value (String "ivan@mail.ru") ]
             ; [ Result_value (String "Petr"); Result_value (String "petr@gmail.com") ]
             ] )
       ]
       "[:find ?e ?email
         :in $ $b
         :where [?e :name ?n]
                [$b ?n ?email]]");
  assert_equal_query
    "query.cljc test-q-in supports queries without db sources"
    [ [ Result_value (Int 10); Result_value (Int 20) ] ]
    (q_string
       ~inputs:[ Arg_scalar (Result_value (Int 10)); Arg_scalar (Result_value (Int 20)) ]
       (empty_db ())
       "[:find ?a ?b :in ?a ?b]")

let test_query__test_bindings () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 15) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 37) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 37) ] }
         ]
  in
  assert_equal_query
    "query.cljc test-bindings handles relation bindings"
    [ [ Result_entity 1; Result_value (String "ivan@mail.ru") ]
    ; [ Result_entity 2; Result_value (String "petr@gmail.com") ]
    ; [ Result_entity 3; Result_value (String "ivan@mail.ru") ]
    ]
    (q_string
       ~inputs:
         [ Arg_relation
             [ [ Result_value (String "Ivan"); Result_value (String "ivan@mail.ru") ]
             ; [ Result_value (String "Petr"); Result_value (String "petr@gmail.com") ]
             ]
         ]
       db
       "[:find ?e ?email
         :in $ [[?n ?email]]
         :where [?e :name ?n]]");
  assert_equal_query
    "query.cljc test-bindings handles tuple bindings"
    [ [ Result_entity 3 ] ]
    (q_string
       ~inputs:[ Arg_tuple [ Result_value (String "Ivan"); Result_value (Int 37) ] ]
       db
       "[:find ?e
         :in $ [?name ?age]
         :where [?e :name ?name]
                [?e :age ?age]]");
  assert_equal_query
    "query.cljc test-bindings handles collection bindings"
    [ [ Result_attr "age"; Result_value (Int 15) ]
    ; [ Result_attr "name"; Result_value (String "Ivan") ]
    ]
    (q_string
       ~inputs:
         [ Arg_scalar (Result_entity 1)
         ; Arg_collection [ Result_attr "name"; Result_attr "age" ]
         ]
       db
       "[:find ?attr ?value
         :in $ ?e [?attr ...]
         :where [?e ?attr ?value]]");
  assert_equal_query
    "query.cljc test-bindings treats empty collection inputs as empty relations"
    []
    (q_sources_string
       ~inputs:[ Arg_collection [] ]
       (empty_db ())
       [ "$", Relation_source [ [ Result_entity 1; Result_attr "name"; Result_value (String "Ivan") ] ] ]
       "[:find ?id
         :in $ [?id ...]
         :where [?id :age _]]");
  assert_equal_query
    "query.cljc test-bindings supports input placeholders"
    [ [ Result_value (Keyword "x"); Result_value (Keyword "z") ] ]
    (q_string
       ~inputs:[ Arg_tuple [ Result_value (Keyword "x"); Result_value (Keyword "y"); Result_value (Keyword "z") ] ]
       (empty_db ())
       "[:find ?x ?z :in [?x _ ?z]]");
  assert_equal_query
    "query.cljc test-bindings supports relation input placeholders"
    [ [ Result_value (Keyword "a"); Result_value (Keyword "c") ]
    ; [ Result_value (Keyword "x"); Result_value (Keyword "z") ]
    ]
    (q_string
       ~inputs:
         [ Arg_relation
             [ [ Result_value (Keyword "x"); Result_value (Keyword "y"); Result_value (Keyword "z") ]
             ; [ Result_value (Keyword "a"); Result_value (Keyword "b"); Result_value (Keyword "c") ]
             ]
         ]
       (empty_db ())
       "[:find ?x ?z :in [[?x _ ?z]]]");
  assert_raises_invalid_arg_message
    "query.cljc test-bindings reports scalar supplied to tuple"
    "Cannot bind value :a to tuple [?a ?b]"
    (fun () ->
       ignore
         (q_string
            ~inputs:[ Arg_scalar (Result_value (Keyword "a")) ]
            (empty_db ())
            "[:find ?a ?b :in [?a ?b]]"));
  assert_raises_invalid_arg_message
    "query.cljc test-bindings reports scalar supplied to collection"
    "Cannot bind value :a to collection [?a ...]"
    (fun () ->
       ignore
         (q_string
            ~inputs:[ Arg_scalar (Result_value (Keyword "a")) ]
            (empty_db ())
            "[:find ?a :in [?a ...]]"));
  assert_raises_invalid_arg_message
    "query.cljc test-bindings reports short tuple inputs"
    "Not enough elements in a collection [:a] to bind tuple [?a ?b]"
    (fun () ->
       ignore
         (q_string
            ~inputs:[ Arg_scalar (Result_value (List [ Keyword "a" ])) ]
            (empty_db ())
            "[:find ?a ?b :in [?a ?b]]"))

let test_query__test_nested_bindings () =
  assert_equal_query
    "query.cljc test-nested-bindings handles map relation inputs"
    [ [ Result_value (Keyword "b"); Result_value (Int 2) ]
    ; [ Result_value (Keyword "c"); Result_value (Int 3) ]
    ]
    (q_string
       ~inputs:[ Arg_scalar (Result_value (Map [ Keyword "a", Int 1; Keyword "b", Int 2; Keyword "c", Int 3 ])) ]
       (empty_db ())
       "[:find ?k ?v
         :in [[?k ?v] ...]
         :where [(> ?v 1)]]");
  let minmax = function
    | [ Result_value (List values) ] ->
      (match values with
       | [] -> None
       | first :: rest ->
         let min_value, max_value =
           List.fold_left
             (fun (min_value, max_value) -> function
                | Int value -> min min_value value, max max_value value
                | _ -> min_value, max_value)
             (match first with
              | Int value -> value, value
              | _ -> 0, 0)
             rest
         in
         Some [ Result_value (Int min_value); Result_value (Int max_value) ])
    | _ -> None
  in
  assert_equal_query
    "query.cljc test-nested-bindings handles dynamic tuple outputs"
    [ [ Result_value (Keyword "a"); Result_value (Int 1); Result_value (Int 4) ]
    ; [ Result_value (Keyword "b"); Result_value (Int 5); Result_value (Int 7) ]
    ]
    (q_string
       ~inputs:
         [ Arg_scalar
             (Result_value
                (Map
                   [ Keyword "a", List [ Int 1; Int 2; Int 3; Int 4 ]
                   ; Keyword "b", List [ Int 5; Int 6; Int 7 ]
                   ; Keyword "c", List [ Int 3 ]
                   ]))
         ; Arg_function minmax
         ]
       (empty_db ())
       "[:find ?k ?min ?max
         :in [[?k ?v] ...] ?minmax
         :where [(?minmax ?v) [?min ?max]]
                [(> ?max ?min)]]");
  let range_values = function
    | [ Result_value (Int min_value); Result_value (Int max_value) ] ->
      let rec collect value acc =
        if value >= max_value then List.rev acc
        else collect (value + 1) (Int value :: acc)
      in
      Some [ Result_value (List (collect min_value [])) ]
    | _ -> None
  in
  assert_equal_query
    "query.cljc test-nested-bindings handles dynamic collection outputs"
    [ [ Result_value (Keyword "a"); Result_value (Int 2) ]
    ; [ Result_value (Keyword "a"); Result_value (Int 4) ]
    ; [ Result_value (Keyword "a"); Result_value (Int 6) ]
    ; [ Result_value (Keyword "b"); Result_value (Int 2) ]
    ]
    (q_string
       ~inputs:
         [ Arg_scalar
             (Result_value
                (Map
                   [ Keyword "a", List [ Int 1; Int 7 ]
                   ; Keyword "b", List [ Int 2; Int 4 ]
                   ]))
         ; Arg_function range_values
         ]
       (empty_db ())
       "[:find ?k ?x
         :in [[?k [?min ?max]] ...] ?range
         :where [(?range ?min ?max) [?x ...]]
                [(even? ?x)]]")

let test_query__test_built_in_get () =
  test_q_upstream_query_cljc_parity_batch ()

let test_query__test_join_unrelated () =
  let five = function
    | [] -> Some [ Result_value (Int 5) ]
    | _ -> None
  in
  assert_equal_query
    "query.cljc test-join-unrelated filters unrelated dynamic function rows"
    []
    (q_string
       ~inputs:[ Arg_function five ]
       (empty_db () |> db_with [ Entity { db_id = None; attrs = [ "person/name", One_value (String "Joe") ] } ])
       "[:find ?name
         :in $ ?my-fn
         :where [?e :person/name ?name]
                [(?my-fn) ?result]
                [(< ?result 3)]]")

let test_query__test_constant_substitution () =
  let attrs = [ "a"; "b"; "c" ] in
  let rec entities entity_id acc =
    if entity_id = 0 then acc
    else
      let attrs =
        attrs
        |> List.map (fun attr -> attr, One_value (String (string_of_int entity_id ^ attr)))
      in
      entities (entity_id - 1) (Entity { db_id = Some (Entity_id entity_id); attrs } :: acc)
  in
  let db =
    empty_db ~schema:[ "a", indexed; "b", indexed; "c", indexed ] ()
    |> db_with (entities 10 [])
  in
  assert_equal_query
    "query.cljc test-constant-substitution resolves entity+attr constants"
    [ [ Result_value (String "5b") ] ]
    (q_string db "[:find ?v :where [5 :b ?v]]");
  assert_equal_query
    "query.cljc test-constant-substitution resolves entity+value constants"
    [ [ Result_attr "b" ] ]
    (q_string db "[:find ?a :where [5 ?a \"5b\"]]");
  assert_equal_query
    "query.cljc test-constant-substitution resolves attr+value constants"
    [ [ Result_entity 5 ] ]
    (q_string db "[:find ?e :where [?e :b \"5b\"]]");
  assert_equal_query
    "query.cljc test-constant-substitution accepts entity and attr inputs"
    [ [ Result_entity 5; Result_attr "b"; Result_value (String "5b") ] ]
    (q_string
       ~inputs:[ Arg_scalar (Result_entity 5); Arg_scalar (Result_attr "b") ]
       db
       "[:find ?e ?a ?v
         :in $ ?e ?a
         :where [?e ?a ?v]]");
  assert_equal_query
    "query.cljc test-constant-substitution self-joins attr and value inputs"
    [ [ Result_entity 5; Result_attr "b"; Result_value (String "5b") ] ]
    (q_string
       ~inputs:[ Arg_scalar (Result_attr "b"); Arg_scalar (Result_value (String "5b")) ]
       db
       "[:find ?e2 ?a ?v
         :in $ ?a ?v
         :where [?e ?a ?v]
                [?e2 ?a ?v]]");
  assert_equal_query
    "query.cljc test-constant-substitution resolves all attrs for entity input"
    [ [ Result_attr "a"; Result_value (String "5a") ]
    ; [ Result_attr "b"; Result_value (String "5b") ]
    ; [ Result_attr "c"; Result_value (String "5c") ]
    ]
    (q_string
       ~inputs:[ Arg_scalar (Result_entity 5) ]
       db
       "[:find ?a ?v
         :in $ ?e
         :where [?e ?a ?v]]");
  assert_equal_query
    "query.cljc test-constant-substitution resolves entity+attr from value constants"
    [ [ Result_entity 5; Result_attr "b" ] ]
    (q_string db "[:find ?e ?a :where [?e ?a \"5b\"]]")

let test_q_reverse_ref_patterns () =
  let db =
    empty_db ~schema:[ "parent", ref_attr; "person/parent", ref_attr ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "parent", One_value (Ref 1) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Anna"); "person/parent", One_value (Ref 1) ] }
         ]
  in
  let reverse_query =
    { find = [ Find_var "parent_name"; Find_var "child_name" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "parent", QAttr "_parent", QVar "child")
        ; Pattern (QVar "parent", QAttr "name", QVar "parent_name")
        ; Pattern (QVar "child", QAttr "name", QVar "child_name")
        ]
    }
  in
  assert_equal_query
    "q patterns support reverse ref attrs"
    [ [ Result_value (String "Ivan"); Result_value (String "Petr") ] ]
    (q db reverse_query);
  let namespaced_reverse_query =
    { find = [ Find_var "child_name" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QEntity 1, QAttr "person/_parent", QVar "child")
        ; Pattern (QVar "child", QAttr "name", QVar "child_name")
        ]
    }
  in
  assert_equal_query
    "q reverse ref patterns preserve namespaces"
    [ [ Result_value (String "Anna") ] ]
    (q db namespaced_reverse_query);
  let source_reverse_query =
    { find = [ Find_var "child" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ SourcePattern ("people", QEntity 1, QAttr "_parent", QVar "child") ]
    }
  in
  assert_equal_query
    "q source patterns support reverse ref attrs"
    [ [ Result_entity 2 ] ]
    (q_sources (empty_db ()) [ "people", Db_source db ] source_reverse_query)

let test_q_predicates_filter_bound_values () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 31) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 19) ] }
         ]
  in
  let adult = function
    | [ Result_value (Int age) ] -> age >= 21
    | _ -> false
  in
  let query =
    { find = [ Find_var "name" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "name", QVar "name")
        ; Pattern (QVar "e", QAttr "age", QVar "age")
        ; Predicate ("adult?", [ QVar "age" ], adult)
        ]
    }
  in
  assert_equal_query
    "q predicates filter bound values"
    [ [ Result_value (String "Ivan") ] ]
    (q db query)

let test_q_predicates_without_free_variables_filter_all_rows () =
  let query predicate =
    { find = [ Find_var "x" ]
    ; inputs =
        [ Input_collection
            ( "x"
            , [ Result_value (Keyword "a")
              ; Result_value (Keyword "b")
              ; Result_value (Keyword "c")
              ] )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ Predicate ("constant?", [], (fun _ -> predicate)) ]
    }
  in
  assert_equal_query
    "q predicates without free variables keep all rows when true"
    [ [ Result_value (Keyword "a") ]
    ; [ Result_value (Keyword "b") ]
    ; [ Result_value (Keyword "c") ]
    ]
    (q (empty_db ()) (query true));
  assert_equal_query
    "q predicates without free variables drop all rows when false"
    []
    (q (empty_db ()) (query false))

let test_q_functions_bind_derived_values () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 31) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 19) ] }
         ]
  in
  let decade = function
    | [ Result_value (Int age) ] -> Some [ Result_value (Int (age / 10 * 10)) ]
    | _ -> None
  in
  let query =
    { find = [ Find_var "name"; Find_var "decade" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "name", QVar "name")
        ; Pattern (QVar "e", QAttr "age", QVar "age")
        ; Function ("decade", [ QVar "age" ], [ "decade" ], decade)
        ]
    }
  in
  assert_equal_query
    "q functions bind derived values"
    [ [ Result_value (String "Ivan"); Result_value (Int 30) ]
    ; [ Result_value (String "Petr"); Result_value (Int 10) ]
    ]
    (q db query)

let test_q_functions_filter_on_none () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 31) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 19) ] }
         ]
  in
  let adult_label = function
    | [ Result_value (Int age) ] when age >= 21 -> Some [ Result_value (String "adult") ]
    | [ Result_value (Int _) ] -> None
    | _ -> None
  in
  let query =
    { find = [ Find_var "name"; Find_var "label" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "name", QVar "name")
        ; Pattern (QVar "e", QAttr "age", QVar "age")
        ; Function ("adult-label", [ QVar "age" ], [ "label" ], adult_label)
        ]
    }
  in
  assert_equal_query
    "q functions filter rows by returning None"
    [ [ Result_value (String "Ivan"); Result_value (String "adult") ] ]
    (q db query)

let test_q_function_binding_conflicts_filter_rows () =
  let identity value _ = Some [ Result_value value ] in
  let tuple value _ =
    match value with
    | Tuple values -> Some (List.map (function Some value -> Result_value value | None -> Result_value (Keyword "nil")) values)
    | _ -> None
  in
  assert_equal_query
    "q filters rows when two functions bind one var to conflicting values"
    []
    (q
       (empty_db ())
       { find = [ Find_var "n" ]
       ; inputs = []
       ; with_vars = []
       ; rules = []
       ; where =
           [ Function ("identity", [], [ "n" ], identity (Int 1))
           ; Function ("identity", [], [ "n" ], identity (Int 2))
           ]
       });
  assert_equal_query
    "q filters rows when destructured function outputs conflict"
    []
    (q
       (empty_db ())
       { find = [ Find_var "n"; Find_var "x" ]
       ; inputs = []
       ; with_vars = []
       ; rules = []
       ; where =
           [ Function ("identity", [], [ "n"; "x" ], tuple (Tuple [ Some (Int 3); Some (Int 4) ]))
           ; Function ("identity", [], [ "n"; "x" ], tuple (Tuple [ Some (Int 1); Some (Int 2) ]))
           ]
       });
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "age", Int 15)
         ; Add (Entity_id 2, "age", Int 35)
         ]
  in
  assert_equal_query
    "q filters rows when relation bindings conflict with later function outputs"
    []
    (q
       db
       { find = [ Find_var "age" ]
       ; inputs = []
       ; with_vars = []
       ; rules = []
       ; where =
           [ Pattern (QWildcard, QAttr "age", QVar "age")
           ; Function ("identity", [], [ "age" ], identity (Int 100))
           ]
       });
  assert_equal_query
    "q filters rows when function outputs conflict with later relation bindings"
    []
    (q
       db
       { find = [ Find_var "age" ]
       ; inputs = []
       ; with_vars = []
       ; rules = []
       ; where =
           [ Function ("identity", [], [ "age" ], identity (Int 100))
           ; Pattern (QWildcard, QAttr "age", QVar "age")
           ]
       })

let test_q_function_bindings_interact_with_rules () =
  let identity value _ = Some [ Result_value value ] in
  let rules =
    [ { rule_name = "my-vals"
      ; rule_params = [ "x" ]
      ; rule_body = [ Function ("identity", [], [ "x" ], identity (Int 1)) ]
      }
    ; { rule_name = "my-vals"
      ; rule_params = [ "x" ]
      ; rule_body = [ Function ("identity", [], [ "x" ], identity (Int 2)) ]
      }
    ; { rule_name = "my-vals"
      ; rule_params = [ "x" ]
      ; rule_body = [ Function ("identity", [], [ "x" ], identity (Int 3)) ]
      }
    ]
  in
  assert_equal_query
    "q rule bindings are filtered by prior function bindings"
    [ [ Result_value (Int 2) ] ]
    (q
       (empty_db ())
       { find = [ Find_var "n" ]
       ; inputs = []
       ; with_vars = []
       ; rules
       ; where =
           [ Function ("identity", [], [ "n" ], identity (Int 2))
           ; Rule ("my-vals", [ QVar "n" ])
           ]
       });
  assert_equal_query
    "q function bindings are filtered by prior rule bindings"
    [ [ Result_value (Int 2) ] ]
    (q
       (empty_db ())
       { find = [ Find_var "n" ]
       ; inputs = []
       ; with_vars = []
       ; rules
       ; where =
           [ Rule ("my-vals", [ QVar "n" ])
           ; Function ("identity", [], [ "n" ], identity (Int 2))
           ]
       })

let test_q_parsed_rule_inputs_interact_with_function_bindings () =
  let rules =
    [ { rule_name = "my-vals"
      ; rule_params = [ "x" ]
      ; rule_body = [ IdentityValue (QValue (Int 1), "x") ]
      }
    ; { rule_name = "my-vals"
      ; rule_params = [ "x" ]
      ; rule_body = [ IdentityValue (QValue (Int 2), "x") ]
      }
    ; { rule_name = "my-vals"
      ; rule_params = [ "x" ]
      ; rule_body = [ IdentityValue (QValue (Int 3), "x") ]
      }
    ]
  in
  assert_equal_query
    "q_string applies rules supplied through % after prior function bindings"
    [ [ Result_value (Int 2) ] ]
    (q_string
       ~inputs:[ Arg_rules rules ]
       (empty_db ())
       "[:find ?n
         :in $ %
         :where [(identity 2) ?n]
                (my-vals ?n)]");
  assert_equal_query
    "q_string applies function bindings after rules supplied through %"
    [ [ Result_value (Int 2) ] ]
    (q_string
       ~inputs:[ Arg_rules rules ]
       (empty_db ())
       "[:find ?n
         :in $ %
         :where (my-vals ?n)
                [(identity 2) ?n]]");
  assert_equal_query
    "q_string filters conflicting scalar function bindings"
    []
    (q_string
       (empty_db ())
       "[:find ?n
         :where [(identity 1) ?n]
                [(identity 2) ?n]]");
  assert_equal_query
    "q_string filters conflicting destructured function bindings"
    []
    (q_string
       (empty_db ())
       "[:find ?n ?x
         :where [(identity [3 4]) [?n ?x]]
                [(identity [1 2]) [?n ?x]]]");
  let db =
    empty_db ()
    |> db_with
      [ Add (Entity_id 1, "age", Int 15)
      ; Add (Entity_id 2, "age", Int 35)
      ]
  in
  assert_equal_query
    "q_string does not run functions when relation inputs are empty"
    []
    (q_string
       db
       "[:find ?e ?y
         :where [?e :salary ?x]
                [(+ ?x 100) ?y]]")

let test_q_predicates_and_functions_reject_unbound_inputs () =
  assert_raises_invalid_arg
    "q predicates reject unbound variables"
    (fun () ->
      ignore
        (q
           (empty_db ())
           { find = [ Find_var "x" ]
           ; inputs = []
           ; with_vars = []
           ; rules = []
           ; where = [ Predicate ("zero?", [ QVar "x" ], (fun _ -> true)) ]
           }));
  assert_raises_invalid_arg
    "q functions reject unbound variables"
    (fun () ->
      ignore
        (q
           (empty_db ())
           { find = [ Find_var "x" ]
           ; inputs = []
           ; with_vars = []
           ; rules = []
           ; where =
               [ Function
                   ( "inc"
                   , [ QVar "x" ]
                   , [ "y" ]
                   , (function
                     | [ Result_value (Int x) ] -> Some [ Result_value (Int (x + 1)) ]
                     | _ -> None) )
               ]
           }))

let test_q_builtin_get_else_get_some_and_missing () =
  let db =
    empty_db ~schema:[ "parent", ref_attr ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 15) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 22); "height", One_value (Int 240); "parent", One_value (Ref 1) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Slava"); "age", One_value (Int 37); "parent", One_value (Ref 2) ] }
         ]
  in
  let get_else_query =
    { find = [ Find_var "e"; Find_var "age"; Find_var "height" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "age", QVar "age")
        ; GetElse (QVar "e", "height", Int 300, "height")
        ]
    }
  in
  assert_equal_query
    "q get-else returns existing values or the default"
    [ [ Result_entity 1; Result_value (Int 15); Result_value (Int 300) ]
    ; [ Result_entity 2; Result_value (Int 22); Result_value (Int 240) ]
    ; [ Result_entity 3; Result_value (Int 37); Result_value (Int 300) ]
    ]
    (q db get_else_query);
  assert_raises_invalid_arg
    "q get-else rejects nil defaults"
    (fun () ->
       ignore
         (q
            db
            { find = [ Find_var "height" ]
            ; inputs = []
            ; with_vars = []
            ; rules = []
            ; where = [ GetElse (QEntity 1, "height", Nil, "height") ]
            }));
  let get_some_query =
    { find = [ Find_var "e"; Find_var "attr"; Find_var "value" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "name", QWildcard)
        ; GetSome (QVar "e", [ "height"; "age" ], "attr", "value")
        ]
    }
  in
  assert_equal_query
    "q get-some returns the first present attr and value"
    [ [ Result_entity 1; Result_attr "age"; Result_value (Int 15) ]
    ; [ Result_entity 2; Result_attr "height"; Result_value (Int 240) ]
    ; [ Result_entity 3; Result_attr "age"; Result_value (Int 37) ]
    ]
    (q db get_some_query);
  let missing_query =
    { find = [ Find_var "e"; Find_var "age" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "age", QVar "age")
        ; Missing (QVar "e", "height")
        ]
    }
  in
  assert_equal_query
    "q missing filters entities that have no value for attr"
    [ [ Result_entity 1; Result_value (Int 15) ]; [ Result_entity 3; Result_value (Int 37) ] ]
    (q db missing_query);
  let reverse_missing_query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "age", QWildcard)
        ; Missing (QVar "e", "_parent")
        ]
    }
  in
  assert_equal_query
    "q missing supports reverse refs"
    [ [ Result_entity 3 ] ]
    (q db reverse_missing_query);
  let source_missing_query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ SourcePattern ("people", QVar "e", QAttr "age", QWildcard)
        ; SourceMissing ("people", QVar "e", "height")
        ]
    }
  in
  assert_equal_query
    "q source missing evaluates against the named source"
    [ [ Result_entity 1 ]; [ Result_entity 3 ] ]
    (q_sources (empty_db ()) [ "people", Db_source db ] source_missing_query);
  let source_get_else_query =
    { find = [ Find_var "e"; Find_var "height" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ SourcePattern ("people", QVar "e", QAttr "age", QWildcard)
        ; SourceGetElse ("people", QVar "e", "height", Int 300, "height")
        ]
    }
  in
  assert_equal_query
    "q source get-else evaluates against the named source"
    [ [ Result_entity 1; Result_value (Int 300) ]
    ; [ Result_entity 2; Result_value (Int 240) ]
    ; [ Result_entity 3; Result_value (Int 300) ]
    ]
    (q_sources (empty_db ()) [ "people", Db_source db ] source_get_else_query);
  let source_get_some_query =
    { find = [ Find_var "e"; Find_var "attr"; Find_var "value" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ SourcePattern ("people", QVar "e", QAttr "name", QWildcard)
        ; SourceGetSome ("people", QVar "e", [ "height"; "age" ], "attr", "value")
        ]
    }
  in
  assert_equal_query
    "q source get-some evaluates against the named source"
    [ [ Result_entity 1; Result_attr "age"; Result_value (Int 15) ]
    ; [ Result_entity 2; Result_attr "height"; Result_value (Int 240) ]
    ; [ Result_entity 3; Result_attr "age"; Result_value (Int 37) ]
    ]
    (q_sources (empty_db ()) [ "people", Db_source db ] source_get_some_query)

let test_q_builtin_get_map_values () =
  let query =
    { find = [ Find_var "value"; Find_var "fallback" ]
    ; inputs =
        [ Input_relation
            ( [ "label"; "m" ]
            , [ [ Result_value (Keyword "a"); Result_value (Map [ Keyword "b", Int 1 ]) ]
              ; [ Result_value (Keyword "c"); Result_value (Map [ Keyword "d", Int 2 ]) ]
              ]
            )
        ; Input_scalar ("key", Result_value (Keyword "d"))
        ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ GetValue (QVar "m", QVar "key", "value")
        ; GetDefaultValue (QVar "m", QValue (Keyword "missing"), QValue (String "fallback"), "fallback")
        ]
    }
  in
  assert_equal_query
    "q get returns map values and default values for missing keys"
    [ [ Result_value (Int 2); Result_value (String "fallback") ] ]
    (q (empty_db ()) query);
  let collection_query =
    { find = [ Find_var "label"; Find_var "value" ]
    ; inputs =
        [ Input_relation
            ( [ "label"; "coll"; "key"; "default" ]
            , [ [ Result_value (String "list")
                ; Result_value (List [ String "zero"; String "one" ])
                ; Result_value (Int 1)
                ; Result_value (String "missing")
                ]
              ; [ Result_value (String "set")
                ; Result_value (Set [ Keyword "present" ])
                ; Result_value (Keyword "present")
                ; Result_value (String "missing")
                ]
              ; [ Result_value (String "tuple-nil")
                ; Result_value (Tuple [ Some (Int 10); None; Some (Int 30) ])
                ; Result_value (Int 1)
                ; Result_value (String "missing")
                ]
              ; [ Result_value (String "default")
                ; Result_value (List [ String "zero" ])
                ; Result_value (Int 4)
                ; Result_value (String "missing")
                ]
              ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ GetDefaultValue (QVar "coll", QVar "key", QVar "default", "value") ]
    }
  in
  assert_equal_query
    "q get returns indexed collection values, set keys, and defaults"
    [ [ Result_value (String "default"); Result_value (String "missing") ]
    ; [ Result_value (String "list"); Result_value (String "one") ]
    ; [ Result_value (String "set"); Result_value (Keyword "present") ]
    ; [ Result_value (String "tuple-nil"); Result_value Nil ]
    ]
    (q (empty_db ()) collection_query)

let test_q_builtin_count_values () =
  let query =
    { find = [ Find_var "x"; Find_var "count" ]
    ; inputs =
        [ Input_collection
            ( "x"
            , [ Result_value (String "a")
              ; Result_value (String "abc")
              ; Result_value (List [ Int 1; Int 2 ])
              ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ CountValue (QVar "x", "count") ]
    }
  in
  assert_equal_query
    "q count returns string and collection sizes"
    [ [ Result_value (String "a"); Result_value (Int 1) ]
    ; [ Result_value (String "abc"); Result_value (Int 3) ]
    ; [ Result_value (List [ Int 1; Int 2 ]); Result_value (Int 2) ]
    ]
    (q (empty_db ()) query)

let test_q_builtin_empty_and_not_empty_values () =
  let inputs =
    [ Input_collection
        ( "x"
        , [ Result_value (String "")
          ; Result_value (String "a")
          ; Result_value (List [])
          ; Result_value (List [ Int 1 ])
          ; Result_value (Set [])
          ; Result_value (Set [ Int 1 ])
          ; Result_value (Map [])
          ; Result_value (Map [ Keyword "a", Int 1 ])
          ]
        )
    ]
  in
  let empty_query = { find = [ Find_var "x" ]; inputs; with_vars = []; rules = []; where = [ EmptyValue (QVar "x") ] } in
  assert_equal_query
    "q empty? filters empty values"
    [ [ Result_value (String "") ]
    ; [ Result_value (List []) ]
    ; [ Result_value (Map []) ]
    ; [ Result_value (Set []) ]
    ]
    (q (empty_db ()) empty_query);
  let not_empty_query =
    { find = [ Find_var "x" ]; inputs; with_vars = []; rules = []; where = [ NotEmptyValue (QVar "x") ] }
  in
  assert_equal_query
    "q not-empty filters non-empty values"
    [ [ Result_value (String "a") ]
    ; [ Result_value (List [ Int 1 ]) ]
    ; [ Result_value (Map [ Keyword "a", Int 1 ]) ]
    ; [ Result_value (Set [ Int 1 ]) ]
    ]
    (q (empty_db ()) not_empty_query)

let test_q_builtin_contains_values () =
  let query =
    { find = [ Find_var "label" ]
    ; inputs =
        [ Input_relation
            ( [ "label"; "coll"; "key" ]
            , [ [ Result_value (String "map"); Result_value (Map [ Keyword "a", Int 1 ]); Result_value (Keyword "a") ]
              ; [ Result_value (String "map-miss"); Result_value (Map [ Keyword "a", Int 1 ]); Result_value (Keyword "b") ]
              ; [ Result_value (String "set"); Result_value (Set [ Int 1; Int 2 ]); Result_value (Int 2) ]
              ; [ Result_value (String "set-miss"); Result_value (Set [ Int 1; Int 2 ]); Result_value (Int 3) ]
              ; [ Result_value (String "list"); Result_value (List [ String "a"; String "b" ]); Result_value (Int 1) ]
              ; [ Result_value (String "list-miss"); Result_value (List [ String "a"; String "b" ]); Result_value (Int 2) ]
              ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ ContainsValue (QVar "coll", QVar "key") ]
    }
  in
  assert_equal_query
    "q contains? filters map keys, set members, and list indexes"
    [ [ Result_value (String "list") ]; [ Result_value (String "map") ]; [ Result_value (String "set") ] ]
    (q (empty_db ()) query)

let test_q_builtin_value_type_predicates () =
  let inputs =
    [ Input_relation
        ( [ "label"; "x" ]
        , [ [ Result_value (String "bool"); Result_value (Bool true) ]
          ; [ Result_value (String "float"); Result_value (Float 2.5) ]
          ; [ Result_value (String "int"); Result_value (Int 1) ]
          ; [ Result_value (String "keyword"); Result_value (Keyword "user/name") ]
          ; [ Result_value (String "string"); Result_value (String "Ivan") ]
          ]
        )
    ]
  in
  let labels predicate =
    q (empty_db ()) { find = [ Find_var "label" ]; inputs; with_vars = []; rules = []; where = [ ValuePredicate (predicate, QVar "x") ] }
  in
  assert_equal_query
    "q number? filters int and float values"
    [ [ Result_value (String "float") ]; [ Result_value (String "int") ] ]
    (labels NumberValue);
  assert_equal_query
    "q integer? filters integer values"
    [ [ Result_value (String "int") ] ]
    (labels IntegerValue);
  assert_equal_query
    "q string? filters string values"
    [ [ Result_value (String "string") ] ]
    (labels StringValue);
  assert_equal_query
    "q boolean? filters boolean values"
    [ [ Result_value (String "bool") ] ]
    (labels BooleanValue);
  assert_equal_query
    "q keyword? filters keyword values"
    [ [ Result_value (String "keyword") ] ]
    (labels KeywordValue)

let test_q_builtin_numeric_predicates () =
  let inputs =
    [ Input_relation
        ( [ "label"; "x" ]
        , [ [ Result_value (String "float-positive"); Result_value (Float 1.5) ]
          ; [ Result_value (String "float-zero"); Result_value (Float 0.0) ]
          ; [ Result_value (String "negative"); Result_value (Int (-2)) ]
          ; [ Result_value (String "odd-negative"); Result_value (Int (-1)) ]
          ; [ Result_value (String "positive"); Result_value (Int 3) ]
          ; [ Result_value (String "zero"); Result_value (Int 0) ]
          ; [ Result_value (String "string"); Result_value (String "0") ]
          ]
        )
    ]
  in
  let labels predicate =
    q (empty_db ()) { find = [ Find_var "label" ]; inputs; with_vars = []; rules = []; where = [ NumericPredicate (predicate, QVar "x") ] }
  in
  assert_equal_query
    "q zero? filters numeric zero values"
    [ [ Result_value (String "float-zero") ]; [ Result_value (String "zero") ] ]
    (labels ZeroNumber);
  assert_equal_query
    "q pos? filters positive numeric values"
    [ [ Result_value (String "float-positive") ]; [ Result_value (String "positive") ] ]
    (labels PositiveNumber);
  assert_equal_query
    "q neg? filters negative numeric values"
    [ [ Result_value (String "negative") ]; [ Result_value (String "odd-negative") ] ]
    (labels NegativeNumber);
  assert_equal_query
    "q even? filters even integers"
    [ [ Result_value (String "negative") ]; [ Result_value (String "zero") ] ]
    (labels EvenInteger);
  assert_equal_query
    "q odd? filters odd integers"
    [ [ Result_value (String "odd-negative") ]; [ Result_value (String "positive") ] ]
    (labels OddInteger)

let test_q_builtin_comparison_predicates () =
  let inputs =
    [ Input_relation
        ( [ "label"; "x"; "y" ]
        , [ [ Result_value (String "equal"); Result_value (Int 2); Result_value (Float 2.0) ]
          ; [ Result_value (String "greater"); Result_value (Int 3); Result_value (Int 2) ]
          ; [ Result_value (String "keyword"); Result_value (Keyword "user/name"); Result_value (Keyword "user/score") ]
          ; [ Result_value (String "less"); Result_value (Int 1); Result_value (Int 2) ]
          ]
        )
    ]
  in
  let labels predicate =
    q
      (empty_db ())
      { find = [ Find_var "label" ]; inputs; with_vars = []; rules = []; where = [ ComparisonPredicate (predicate, QVar "x", QVar "y") ] }
  in
  assert_equal_query
    "q < filters values using DataScript ordering"
    [ [ Result_value (String "keyword") ]; [ Result_value (String "less") ] ]
    (labels LessThan);
  assert_equal_query
    "q > filters values using DataScript ordering"
    [ [ Result_value (String "greater") ] ]
    (labels GreaterThan);
  assert_equal_query
    "q <= includes equal values"
    [ [ Result_value (String "equal") ]; [ Result_value (String "keyword") ]; [ Result_value (String "less") ] ]
    (labels LessOrEqual);
  assert_equal_query
    "q >= includes equal values"
    [ [ Result_value (String "equal") ]; [ Result_value (String "greater") ] ]
    (labels GreaterOrEqual)

let test_q_builtin_variadic_comparison_predicates () =
  let inputs =
    [ Input_relation
        ( [ "label"; "x"; "y"; "z" ]
        , [ [ Result_value (String "ascending"); Result_value (Int 1); Result_value (Int 2); Result_value (Int 3) ]
          ; [ Result_value (String "descending"); Result_value (Int 3); Result_value (Int 2); Result_value (Int 1) ]
          ; [ Result_value (String "equal"); Result_value (Int 2); Result_value (Int 2); Result_value (Int 2) ]
          ]
        )
    ]
  in
  let labels predicate terms =
    q
      (empty_db ())
      { find = [ Find_var "label" ]
      ; inputs
      ; with_vars = []
      ; rules = []
      ; where = [ ComparisonPredicateN (predicate, terms) ]
      }
  in
  assert_equal_query
    "q variadic < checks adjacent values"
    [ [ Result_value (String "ascending") ] ]
    (labels LessThan [ QVar "x"; QVar "y"; QVar "z" ]);
  assert_equal_query
    "q variadic > checks adjacent values"
    [ [ Result_value (String "descending") ] ]
    (labels GreaterThan [ QVar "x"; QVar "y"; QVar "z" ]);
  assert_equal_query
    "q variadic <= includes equal adjacent values"
    [ [ Result_value (String "ascending") ]; [ Result_value (String "equal") ] ]
    (labels LessOrEqual [ QVar "x"; QVar "y"; QVar "z" ]);
  assert_equal_query
    "q one-argument comparison predicates are true"
    [ [ Result_value (String "ascending") ]; [ Result_value (String "descending") ]; [ Result_value (String "equal") ] ]
    (labels LessThan [ QVar "x" ])

let test_q_builtin_equality_predicates () =
  let inputs =
    [ Input_relation
        ( [ "label"; "x"; "y"; "z" ]
        , [ [ Result_value (String "all-equal"); Result_value (Int 1); Result_value (Float 1.0); Result_value (Int 1) ]
          ; [ Result_value (String "different"); Result_value (Int 1); Result_value (Int 2); Result_value (Int 1) ]
          ; [ Result_value (String "keyword-equal"); Result_value (Keyword "a/b"); Result_value (Keyword "a/b"); Result_value (Keyword "a/b") ]
          ]
        )
    ]
  in
  let labels predicate =
    q
      (empty_db ())
      { find = [ Find_var "label" ]
      ; inputs
      ; with_vars = []
      ; rules = []
      ; where = [ EqualityPredicate (predicate, [ QVar "x"; QVar "y"; QVar "z" ]) ]
      }
  in
  assert_equal_query
    "q = filters rows whose values are all equal"
    [ [ Result_value (String "all-equal") ]; [ Result_value (String "keyword-equal") ] ]
    (labels EqualValues);
  assert_equal_query
    "q not= filters rows with at least one differing value"
    [ [ Result_value (String "different") ] ]
    (labels NotEqualValues)

let test_q_builtin_arithmetic_values () =
  let query =
    { find =
        [ Find_var "sum"
        ; Find_var "difference"
        ; Find_var "product"
        ; Find_var "quotient"
        ; Find_var "incremented"
        ; Find_var "decremented"
        ]
    ; inputs =
        [ Input_scalar ("x", Result_value (Int 6))
        ; Input_scalar ("y", Result_value (Float 2.5))
        ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ ArithmeticValue (AddNumbers, [ QVar "x"; QVar "y" ], "sum")
        ; ArithmeticValue (SubtractNumbers, [ QVar "x"; QVar "y" ], "difference")
        ; ArithmeticValue (MultiplyNumbers, [ QVar "x"; QVar "y" ], "product")
        ; ArithmeticValue (DivideNumbers, [ QVar "x"; QValue (Int 2) ], "quotient")
        ; ArithmeticValue (IncrementNumber, [ QVar "x" ], "incremented")
        ; ArithmeticValue (DecrementNumber, [ QVar "x" ], "decremented")
        ]
    }
  in
  assert_equal_query
    "q arithmetic built-ins derive numeric values"
    [ [ Result_value (Float 8.5)
      ; Result_value (Float 3.5)
      ; Result_value (Float 15.0)
      ; Result_value (Int 3)
      ; Result_value (Int 7)
      ; Result_value (Int 5)
      ]
    ]
    (q (empty_db ()) query)

let test_q_builtin_integer_arithmetic_values () =
  let query =
    { find = [ Find_var "quotient"; Find_var "remainder"; Find_var "modulo" ]
    ; inputs =
        [ Input_scalar ("x", Result_value (Int (-7)))
        ; Input_scalar ("y", Result_value (Int 3))
        ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ ArithmeticValue (QuotientNumbers, [ QVar "x"; QVar "y" ], "quotient")
        ; ArithmeticValue (RemainderNumbers, [ QVar "x"; QVar "y" ], "remainder")
        ; ArithmeticValue (ModuloNumbers, [ QVar "x"; QVar "y" ], "modulo")
        ]
    }
  in
  assert_equal_query
    "q integer arithmetic built-ins derive quot rem and mod values"
    [ [ Result_value (Int (-2)); Result_value (Int (-1)); Result_value (Int 2) ] ]
    (q (empty_db ()) query)

let test_q_builtin_compare_min_max_values () =
  let query =
    { find = [ Find_var "comparison"; Find_var "least"; Find_var "greatest"; Find_var "keyword-min" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ CompareValue (QValue (Keyword "user/name"), QValue (Keyword "user/score"), "comparison")
        ; ExtremumValue (MinimumValue, [ QValue (Int 3); QValue (Float 2.5); QValue (Int 4) ], "least")
        ; ExtremumValue (MaximumValue, [ QValue (Int 3); QValue (Float 2.5); QValue (Int 4) ], "greatest")
        ; ExtremumValue (MinimumValue, [ QValue (Keyword "user/score"); QValue (Keyword "user/name") ], "keyword-min")
        ]
    }
  in
  assert_equal_query
    "q compare min and max use DataScript value ordering"
    [ [ Result_value (Int (-1))
      ; Result_value (Float 2.5)
      ; Result_value (Int 4)
      ; Result_value (Keyword "user/name")
      ]
    ]
    (q (empty_db ()) query)

let test_q_builtin_boolean_predicates () =
  let inputs =
    [ Input_relation
        ( [ "label"; "x" ]
        , [ [ Result_value (String "false"); Result_value (Bool false) ]
          ; [ Result_value (String "int"); Result_value (Int 1) ]
          ; [ Result_value (String "nil"); Result_value Nil ]
          ; [ Result_value (String "true"); Result_value (Bool true) ]
          ]
        )
    ]
  in
  let labels where =
    q (empty_db ()) { find = [ Find_var "label" ]; inputs; with_vars = []; rules = []; where }
  in
  assert_equal_query
    "q true? filters true values"
    [ [ Result_value (String "true") ] ]
    (labels [ BooleanPredicate (TrueValue, QVar "x") ]);
  assert_equal_query
    "q false? filters false values"
    [ [ Result_value (String "false") ] ]
    (labels [ BooleanPredicate (FalseValue, QVar "x") ]);
  assert_equal_query
    "q some? filters bound values"
    [ [ Result_value (String "false") ]; [ Result_value (String "int") ]; [ Result_value (String "true") ] ]
    (labels [ BooleanPredicate (SomeValue, QVar "x") ]);
  assert_equal_query
    "q nil? filters nil values"
    [ [ Result_value (String "nil") ] ]
    (labels [ BooleanPredicate (NilValue, QVar "x") ]);
  assert_equal_query
    "q not derives boolean negation"
    [ [ Result_value (String "false") ]; [ Result_value (String "nil") ] ]
    (labels [ BooleanNotValue (QVar "x", "negated"); BooleanPredicate (TrueValue, QVar "negated") ])

let test_q_builtin_identity_and_boolean_values () =
  let query =
    { find = [ Find_var "same"; Find_var "and"; Find_var "and-falsey"; Find_var "or"; Find_var "or-falsey" ]
    ; inputs = [ Input_scalar ("x", Result_value (Keyword "user/name")) ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ IdentityValue (QVar "x", "same")
        ; BooleanAndValue ([ QValue (Bool true); QVar "x"; QValue (String "kept") ], "and")
        ; BooleanAndValue ([ QValue (Bool true); QValue Nil; QValue (String "ignored") ], "and-falsey")
        ; BooleanOrValue ([ QValue (Bool false); QVar "x"; QValue (Bool true) ], "or")
        ; BooleanOrValue ([ QValue Nil; QValue (Bool false) ], "or-falsey")
        ]
    }
  in
  assert_equal_query
    "q identity and boolean value built-ins derive values"
    [ [ Result_value (Keyword "user/name")
      ; Result_value (String "kept")
      ; Result_value Nil
      ; Result_value (Keyword "user/name")
      ; Result_value (Bool false)
      ]
    ]
    (q (empty_db ()) query)

let test_q_builtin_random_values () =
  match
    q
      (empty_db ())
      { find = [ Find_var "rand"; Find_var "rand-int" ]
      ; inputs = []
      ; with_vars = []
      ; rules = []
      ; where = [ RandomValue "rand"; RandomIntValue (QValue (Int 10), "rand-int") ]
      }
  with
  | [ [ Result_value (Float rand); Result_value (Int rand_int) ] ] ->
    if rand < 0.0 || rand >= 1.0 then failwith "rand should be in [0, 1)";
    if rand_int < 0 || rand_int >= 10 then failwith "rand-int should be in [0, n)"
  | _ -> failwith "unexpected random query result"

let test_q_builtin_differ_and_identical_predicates () =
  let inputs =
    [ Input_relation
        ( [ "label"; "a"; "b"; "c"; "d" ]
        , [ [ Result_value (String "different"); Result_value (Int 1); Result_value (Int 2); Result_value (Int 1); Result_value (Int 3) ]
          ; [ Result_value (String "same"); Result_value (Int 1); Result_value (Int 2); Result_value (Float 1.0); Result_value (Int 2) ]
          ]
        )
    ]
  in
  assert_equal_query
    "q -differ? filters rows where argument halves differ"
    [ [ Result_value (String "different") ] ]
    (q
       (empty_db ())
       { find = [ Find_var "label" ]
       ; inputs
       ; with_vars = []
       ; rules = []
       ; where = [ DifferPredicate [ QVar "a"; QVar "b"; QVar "c"; QVar "d" ] ]
       });
  assert_equal_query
    "q identical? filters structurally identical typed values"
    [ [ Result_value (String "same") ] ]
    (q
       (empty_db ())
       { find = [ Find_var "label" ]
       ; inputs
       ; with_vars = []
       ; rules = []
       ; where = [ IdenticalPredicate (QVar "b", QVar "d") ]
       })

let test_q_builtin_type_values () =
  let query =
    { find = [ Find_var "label"; Find_var "type" ]
    ; inputs =
        [ Input_relation
            ( [ "label"; "x" ]
            , [ [ Result_value (String "bool"); Result_value (Bool true) ]
              ; [ Result_value (String "int"); Result_value (Int 1) ]
              ; [ Result_value (String "keyword"); Result_value (Keyword "user/name") ]
              ; [ Result_value (String "list"); Result_value (List [ Int 1 ]) ]
              ; [ Result_value (String "string"); Result_value (String "Ivan") ]
              ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ TypeValue (QVar "x", "type") ]
    }
  in
  assert_equal_query
    "q type derives stable type keywords"
    [ [ Result_value (String "bool"); Result_value (Keyword "type/bool") ]
    ; [ Result_value (String "int"); Result_value (Keyword "type/int") ]
    ; [ Result_value (String "keyword"); Result_value (Keyword "type/keyword") ]
    ; [ Result_value (String "list"); Result_value (Keyword "type/list") ]
    ; [ Result_value (String "string"); Result_value (Keyword "type/string") ]
    ]
    (q (empty_db ()) query)

let test_q_builtin_name_and_namespace_values () =
  let name_query =
    { find = [ Find_var "label"; Find_var "name" ]
    ; inputs =
        [ Input_relation
            ( [ "label"; "x" ]
            , [ [ Result_value (String "keyword"); Result_value (Keyword "user/name") ]
              ; [ Result_value (String "string"); Result_value (String "plain") ]
              ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ NameValue (QVar "x", "name") ]
    }
  in
  assert_equal_query
    "q name derives names from keywords and strings"
    [ [ Result_value (String "keyword"); Result_value (String "name") ]
    ; [ Result_value (String "string"); Result_value (String "plain") ]
    ]
    (q (empty_db ()) name_query);
  let namespace_query =
    { find = [ Find_var "label"; Find_var "namespace" ]
    ; inputs =
        [ Input_relation
            ( [ "label"; "x" ]
            , [ [ Result_value (String "namespaced"); Result_value (Keyword "user/name") ]
              ; [ Result_value (String "plain"); Result_value (Keyword "plain") ]
              ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ NamespaceValue (QVar "x", "namespace") ]
    }
  in
  assert_equal_query
    "q namespace derives namespaces from namespaced keywords"
    [ [ Result_value (String "namespaced"); Result_value (String "user") ] ]
    (q (empty_db ()) namespace_query)

let test_q_builtin_keyword_from_name_values () =
  let query =
    { find = [ Find_var "label"; Find_var "keyword" ]
    ; inputs =
        [ Input_relation
            ( [ "label"; "x" ]
            , [ [ Result_value (String "from-string"); Result_value (String "user/name") ]
              ; [ Result_value (String "from-keyword"); Result_value (Keyword "user/email") ]
              ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ KeywordFromName (QVar "x", "keyword") ]
    }
  in
  assert_equal_query
    "q keyword derives keyword values from strings and keywords"
    [ [ Result_value (String "from-keyword"); Result_value (Keyword "user/email") ]
    ; [ Result_value (String "from-string"); Result_value (Keyword "user/name") ]
    ]
    (q (empty_db ()) query);
  let namespaced_query =
    { find = [ Find_var "keyword" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ KeywordFromNamespaceName (QValue (String "user"), QValue (String "name"), "keyword") ]
    }
  in
  assert_equal_query
    "q keyword derives a namespaced keyword from namespace and name strings"
    [ [ Result_value (Keyword "user/name") ] ]
    (q (empty_db ()) namespaced_query);
  let non_string_query =
    { find = [ Find_var "keyword" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ KeywordFromNamespaceName (QValue (Keyword "user"), QValue (String "name"), "keyword") ]
    }
  in
  assert_equal_query
    "q keyword ignores non-string namespace inputs"
    []
    (q (empty_db ()) non_string_query)

let test_q_builtin_meta_values () =
  let query =
    { find = [ Find_var "meta" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ MetaValue (QValue (String "plain"), "meta") ]
    }
  in
  assert_equal_query
    "q meta derives nil for values without metadata"
    [ [ Result_value Nil ] ]
    (q (empty_db ()) query)

let test_q_builtin_string_predicates () =
  let inputs =
    [ Input_relation
        ( [ "label"; "s" ]
        , [ [ Result_value (String "alpha"); Result_value (String "alphabet") ]
          ; [ Result_value (String "beta"); Result_value (String "betamax") ]
          ; [ Result_value (String "gamma"); Result_value (String "gamma") ]
          ]
        )
    ]
  in
  let labels where =
    q (empty_db ()) { find = [ Find_var "label" ]; inputs; with_vars = []; rules = []; where }
  in
  assert_equal_query
    "q includes? filters strings containing a substring"
    [ [ Result_value (String "alpha") ]; [ Result_value (String "beta") ] ]
    (labels [ StringIncludesValue (QVar "s", QValue (String "bet")) ]);
  assert_equal_query
    "q starts-with? filters strings with a prefix"
    [ [ Result_value (String "alpha") ] ]
    (labels [ StringStartsWithValue (QVar "s", QValue (String "alp")) ]);
  assert_equal_query
    "q ends-with? filters strings with a suffix"
    [ [ Result_value (String "beta") ] ]
    (labels [ StringEndsWithValue (QVar "s", QValue (String "max")) ])

let test_q_builtin_string_transforms () =
  let query =
    { find = [ Find_var "lower"; Find_var "upper"; Find_var "capitalized"; Find_var "reversed" ]
    ; inputs = [ Input_scalar ("s", Result_value (String "dAtA")) ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ StringLowerCaseValue (QVar "s", "lower")
        ; StringUpperCaseValue (QVar "s", "upper")
        ; StringCapitalizeValue (QVar "s", "capitalized")
        ; StringReverseValue (QVar "s", "reversed")
        ]
    }
  in
  assert_equal_query
    "q string transform built-ins derive string values"
    [ [ Result_value (String "data")
      ; Result_value (String "DATA")
      ; Result_value (String "Data")
      ; Result_value (String "AtAd")
      ]
    ]
    (q (empty_db ()) query)

let test_q_builtin_string_trim_values () =
  let query =
    { find = [ Find_var "trimmed"; Find_var "left"; Find_var "right"; Find_var "newline" ]
    ; inputs = [ Input_scalar ("s", Result_value (String "  data  \n")) ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ StringTrimValue (QVar "s", "trimmed")
        ; StringTrimLeftValue (QVar "s", "left")
        ; StringTrimRightValue (QVar "s", "right")
        ; StringTrimNewlineValue (QVar "s", "newline")
        ]
    }
  in
  assert_equal_query
    "q string trim built-ins derive trimmed string values"
    [ [ Result_value (String "data")
      ; Result_value (String "data  \n")
      ; Result_value (String "  data")
      ; Result_value (String "  data  ")
      ]
    ]
    (q (empty_db ()) query)

let test_q_builtin_string_index_values () =
  let query =
    { find = [ Find_var "label"; Find_var "first"; Find_var "last" ]
    ; inputs =
        [ Input_relation
            ( [ "label"; "s"; "needle" ]
            , [ [ Result_value (String "hit"); Result_value (String "bananas"); Result_value (String "na") ]
              ; [ Result_value (String "miss"); Result_value (String "bananas"); Result_value (String "zz") ]
              ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ StringIndexOfValue (QVar "s", QVar "needle", "first")
        ; StringLastIndexOfValue (QVar "s", QVar "needle", "last")
        ]
    }
  in
  assert_equal_query
    "q string index built-ins derive first and last match positions"
    [ [ Result_value (String "hit"); Result_value (Int 2); Result_value (Int 4) ] ]
    (q (empty_db ()) query)

let test_q_builtin_string_substring_values () =
  let query =
    { find = [ Find_var "part"; Find_var "suffix" ]
    ; inputs = [ Input_scalar ("s", Result_value (String "datascript")) ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ StringSubstringValue (QVar "s", QValue (Int 4), Some (QValue (Int 10)), "part")
        ; StringSubstringValue (QVar "s", QValue (Int 4), None, "suffix")
        ]
    }
  in
  assert_equal_query
    "q subs derives bounded and suffix substrings"
    [ [ Result_value (String "script"); Result_value (String "script") ] ]
    (q (empty_db ()) query);
  assert_raises_invalid_arg
    "q subs rejects out-of-range indexes"
    (fun () ->
       ignore
         (q
            (empty_db ())
            { find = [ Find_var "part" ]
            ; inputs = [ Input_scalar ("s", Result_value (String "data")) ]
            ; with_vars = []
            ; rules = []
            ; where = [ StringSubstringValue (QVar "s", QValue (Int 3), Some (QValue (Int 5)), "part") ]
            }))

let test_q_builtin_string_build_and_join_values () =
  let str_query =
    { find = [ Find_var "s" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ StringBuildValue ([ QValue (String "score="); QValue (Int 42); QValue (Bool true) ], "s") ]
    }
  in
  assert_equal_query
    "q str derives a string from scalar values"
    [ [ Result_value (String "score=42true") ] ]
    (q (empty_db ()) str_query);
  let join_query =
    { find = [ Find_var "s"; Find_var "plain" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ StringJoinValue
            ( QValue (String ",")
            , QValue (List [ String "red"; String "green"; String "blue" ])
            , "s" )
        ; StringJoinPlainValue
            ( QValue (List [ String "red"; String "green"; String "blue" ])
            , "plain" )
        ]
    }
  in
  assert_equal_query
    "q string join derives joined strings from a collection"
    [ [ Result_value (String "red,green,blue"); Result_value (String "redgreenblue") ] ]
    (q (empty_db ()) join_query)

let test_q_builtin_print_string_values () =
  let query =
    { find = [ Find_var "printed"; Find_var "readable"; Find_var "line"; Find_var "readable_line" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ PrintStringValue ([ QValue (String "hi"); QValue (Keyword "user/name"); QValue (Int 2) ], "printed")
        ; PrStringValue ([ QValue (String "hi"); QValue (Keyword "user/name"); QValue (Int 2) ], "readable")
        ; PrintLineStringValue ([ QValue (String "hi"); QValue (Int 2) ], "line")
        ; PrnStringValue ([ QValue (String "hi"); QValue (Int 2) ], "readable_line")
        ]
    }
  in
  assert_equal_query
    "q print string built-ins derive readable and non-readable strings"
    [ [ Result_value (String "hi :user/name 2")
      ; Result_value (String "\"hi\" :user/name 2")
      ; Result_value (String "hi 2\n")
      ; Result_value (String "\"hi\" 2\n")
      ]
    ]
    (q (empty_db ()) query)

let test_q_builtin_string_replace_regex_values () =
  let query =
    { find = [ Find_var "all"; Find_var "first" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ RePatternValue (QValue (String "[ae]"), "vowels")
        ; StringReplaceValue (QValue (String "banana"), QVar "vowels", QValue (String "*"), "all")
        ; StringReplaceFirstValue (QValue (String "banana"), QVar "vowels", QValue (String "*"), "first")
        ]
    }
  in
  assert_equal_query
    "q string replace accepts regex patterns"
    [ [ Result_value (String "b*n*n*"); Result_value (String "b*nana") ] ]
    (q (empty_db ()) query)

let test_q_builtin_string_replace_values () =
  let query =
    { find = [ Find_var "all"; Find_var "first" ]
    ; inputs = [ Input_scalar ("s", Result_value (String "banana")) ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ StringReplaceValue (QVar "s", QValue (String "na"), QValue (String "NA"), "all")
        ; StringReplaceFirstValue (QVar "s", QValue (String "na"), QValue (String "NA"), "first")
        ]
    }
  in
  assert_equal_query
    "q string replace built-ins derive replaced strings"
    [ [ Result_value (String "baNANA"); Result_value (String "baNAna") ] ]
    (q (empty_db ()) query)

let test_q_builtin_string_escape_values () =
  let query =
    { find = [ Find_var "escaped"; Find_var "unchanged" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ StringEscapeValue
            ( QValue (String "a.b/c")
            , QValue (Map [ String ".", String "\\."; String "/", String "\\/" ])
            , "escaped" )
        ; StringEscapeValue
            ( QValue (String "abc")
            , QValue (Map [ String ".", String "\\." ])
            , "unchanged" )
        ]
    }
  in
  assert_equal_query
    "q string escape replaces mapped characters and preserves others"
    [ [ Result_value (String "a\\.b\\/c"); Result_value (String "abc") ] ]
    (q (empty_db ()) query)

let test_q_builtin_regex_values () =
  let query =
    { find = [ Find_var "match"; Find_var "full"; Find_var "matches" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ RePatternValue (QValue (String "[0-9]+"), "digits")
        ; ReFindValue (QVar "digits", QValue (String "abc123def"), "match")
        ; ReMatchesValue (QValue (Regex "[a-z]+[0-9]+"), QValue (String "abc123"), "full")
        ; ReSeqValue (QVar "digits", QValue (String "a1b22c333"), "matches")
        ]
    }
  in
  assert_equal_query
    "q regex built-ins derive pattern, first match, full match, and match sequence values"
    [ [ Result_value (String "123")
      ; Result_value (String "abc123")
      ; Result_value (List [ String "1"; String "22"; String "333" ])
      ]
    ]
    (q (empty_db ()) query);
  let no_match_query =
    { find = [ Find_var "match" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ ReFindValue (QValue (Regex "[0-9]+"), QValue (String "abcdef"), "match") ]
    }
  in
  assert_equal_query
    "q re-find produces no rows when the pattern does not match"
    []
    (q (empty_db ()) no_match_query)

let test_query__test_built_in_regex () =
  assert_equal_query
    "query.cljc test-built-in-regex matches regex patterns from inputs"
    [ [ Result_value (String "aXb") ]; [ Result_value (String "abcX") ] ]
    (q_string
       ~inputs:
         [ Arg_collection
             [ Result_value (String "abc")
             ; Result_value (String "abcX")
             ; Result_value (String "aXb")
             ]
         ; Arg_scalar (Result_value (String "X"))
         ]
       (empty_db ())
       "[:find ?name
         :in [?name ...] ?key
         :where [(re-pattern ?key) ?pattern]
                [(re-find ?pattern ?name)]]")

let test_q_builtin_string_blank_and_split_values () =
  let blank_query =
    { find = [ Find_var "label" ]
    ; inputs =
        [ Input_relation
            ( [ "label"; "s" ]
            , [ [ Result_value (String "empty"); Result_value (String "") ]
              ; [ Result_value (String "space"); Result_value (String " \t\n") ]
              ; [ Result_value (String "word"); Result_value (String " data ") ]
              ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ StringBlankValue (QVar "s") ]
    }
  in
  assert_equal_query
    "q blank? filters blank strings"
    [ [ Result_value (String "empty") ]; [ Result_value (String "space") ] ]
    (q (empty_db ()) blank_query);
  let split_query =
    { find = [ Find_var "parts"; Find_var "regex-parts"; Find_var "limited-parts"; Find_var "lines" ]
    ; inputs =
        [ Input_scalar ("csv", Result_value (String "red,green,blue"))
        ; Input_scalar ("mixed", Result_value (String "red,green;blue"))
        ; Input_scalar ("text", Result_value (String "first\nsecond\r\nthird"))
        ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ StringSplitValue (QVar "csv", QValue (String ","), "parts")
        ; RePatternValue (QValue (String "[,;]"), "separator")
        ; StringSplitValue (QVar "mixed", QVar "separator", "regex-parts")
        ; StringSplitLimitValue (QVar "mixed", QVar "separator", QValue (Int 2), "limited-parts")
        ; StringSplitLinesValue (QVar "text", "lines")
        ]
    }
  in
  assert_equal_query
    "q split and split-lines derive string lists"
    [ [ Result_value (List [ String "red"; String "green"; String "blue" ])
      ; Result_value (List [ String "red"; String "green"; String "blue" ])
      ; Result_value (List [ String "red"; String "green;blue" ])
      ; Result_value (List [ String "first"; String "second"; String "third" ])
      ]
    ]
    (q (empty_db ()) split_query)

let test_q_builtin_vector_values () =
  let query =
    { find = [ Find_var "tx_data" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Ground (Keyword "db/add", "op")
        ; VectorValue ([ QVar "op"; QValue (Int (-1)); QAttr "attr"; QValue (Int 12) ], "tx_data")
        ]
    }
  in
  assert_equal_query
    "q vector builds a vector value from bound terms"
    [ [ Result_value (Vector [ Keyword "db/add"; Int (-1); Keyword "attr"; Int 12 ]) ] ]
    (q (empty_db ()) query)

let test_q_builtin_vector_captures_bound_row_values () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "attr", One_value (String "A") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "attr", One_value (String "B") ] }
         ]
  in
  let query =
    { find = [ Find_var "a"; Find_var "b" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QWildcard, QAttr "attr", QVar "a")
        ; VectorValue ([ QVar "a" ], "b")
        ]
    }
  in
  assert_equal_query_set
    "q vector captures each row's current binding"
    [ [ Result_value (String "A"); Result_value (Vector [ String "A" ]) ]
    ; [ Result_value (String "B"); Result_value (Vector [ String "B" ]) ]
    ]
    (q db query)

let test_q_builtin_hash_map_values () =
  let query =
    { find = [ Find_var "m" ]
    ; inputs =
        [ Input_scalar ("left", Result_value (Int 1))
        ; Input_scalar ("right", Result_value (Int 2))
        ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ HashMapValue
            ( [ QAttr "left"; QVar "left"; QAttr "right"; QVar "right" ]
            , "m" )
        ]
    }
  in
  assert_equal_query
    "q hash-map builds a map value from bound key/value terms"
    [ [ Result_value (Map [ Keyword "left", Int 1; Keyword "right", Int 2 ]) ] ]
    (q (empty_db ()) query);
  let array_map_query =
    { find = [ Find_var "m" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ ArrayMapValue
            ( [ QAttr "right"; QValue (Int 2); QAttr "left"; QValue (Int 1) ]
            , "m" )
        ]
    }
  in
  assert_equal_query
    "q array-map builds a normalized map value from key/value terms"
    [ [ Result_value (Map [ Keyword "left", Int 1; Keyword "right", Int 2 ]) ] ]
    (q (empty_db ()) array_map_query);
  let odd_query =
    { find = [ Find_var "m" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ HashMapValue ([ QAttr "left"; QValue (Int 1); QAttr "right" ], "m") ]
    }
  in
  assert_raises_invalid_arg
    "q hash-map rejects odd argument counts"
    (fun () -> ignore (q (empty_db ()) odd_query))

let test_q_builtin_list_and_set_values () =
  let list_query =
    { find = [ Find_var "xs" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ ListValue ([ QValue (Int 2); QValue (Int 1); QValue (Int 1) ], "xs") ]
    }
  in
  assert_equal_query
    "q list builds an ordered list value"
    [ [ Result_value (List [ Int 2; Int 1; Int 1 ]) ] ]
    (q (empty_db ()) list_query);
  let set_query =
    { find = [ Find_var "xs" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ SetValue ([ QValue (Int 2); QValue (Int 1); QValue (Int 1) ], "xs") ]
    }
  in
  assert_equal_query
    "q set builds a normalized set value"
    [ [ Result_value (Set [ Int 1; Int 2 ]) ] ]
    (q (empty_db ()) set_query)

let test_q_builtin_range_values () =
  let end_query =
    { find = [ Find_var "x" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ RangeEndValue (QValue (Int 3), "x") ]
    }
  in
  assert_equal_query
    "q range expands a single positive end bound from zero"
    [ [ Result_value (Int 0) ]; [ Result_value (Int 1) ]; [ Result_value (Int 2) ] ]
    (q (empty_db ()) end_query);
  let negative_end_query =
    { find = [ Find_var "x" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ RangeEndValue (QValue (Int (-2)), "x") ]
    }
  in
  assert_equal_query
    "q range with a negative single end bound is empty"
    []
    (q (empty_db ()) negative_end_query);
  let query =
    { find = [ Find_var "x" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ RangeValue (QValue (Int 1), QValue (Int 4), "x") ]
    }
  in
  assert_equal_query
    "q range expands an integer range into result rows"
    [ [ Result_value (Int 1) ]; [ Result_value (Int 2) ]; [ Result_value (Int 3) ] ]
    (q (empty_db ()) query);
  let empty_query =
    { find = [ Find_var "x" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ RangeValue (QValue (Int 4), QValue (Int 1), "x") ]
    }
  in
  assert_equal_query
    "q range with descending bounds and default step is empty"
    []
    (q (empty_db ()) empty_query);
  let stepped_query =
    { find = [ Find_var "x" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ RangeStepValue (QValue (Int 1), QValue (Int 8), QValue (Int 3), "x") ]
    }
  in
  assert_equal_query
    "q range expands with a positive step"
    [ [ Result_value (Int 1) ]; [ Result_value (Int 4) ]; [ Result_value (Int 7) ] ]
    (q (empty_db ()) stepped_query);
  let descending_query =
    { find = [ Find_var "x" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ RangeStepValue (QValue (Int 8), QValue (Int 1), QValue (Int (-3)), "x") ]
    }
  in
  assert_equal_query
    "q range expands with a negative step"
    [ [ Result_value (Int 2) ]; [ Result_value (Int 5) ]; [ Result_value (Int 8) ] ]
    (q (empty_db ()) descending_query);
  assert_raises_invalid_arg
    "q range rejects zero step"
    (fun () ->
       ignore
         (q
            (empty_db ())
            { find = [ Find_var "x" ]
            ; inputs = []
            ; with_vars = []
            ; rules = []
            ; where = [ RangeStepValue (QValue (Int 1), QValue (Int 4), QValue (Int 0), "x") ]
            }))

let test_q_builtin_tuple_and_untuple () =
  let tuple_query =
    { find = [ Find_var "pair" ]
    ; inputs =
        [ Input_scalar ("a", Result_value (Int 1))
        ; Input_scalar ("b", Result_value (Int 2))
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ TupleFunction ([ QVar "a"; QVar "b" ], "pair") ]
    }
  in
  assert_equal_query
    "q tuple builds a tuple value from bound terms"
    [ [ Result_value (Tuple [ Some (Int 1); Some (Int 2) ]) ] ]
    (q (empty_db ()) tuple_query);
  let untuple_query =
    { find = [ Find_var "b" ]
    ; inputs =
        [ Input_scalar
            ( "pair"
            , Result_value (Tuple [ Some (String "left"); Some (String "right") ])
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ UntupleFunction (QVar "pair", [ "a"; "b" ]) ]
    }
  in
  assert_equal_query
    "q untuple binds tuple elements to vars"
    [ [ Result_value (String "right") ] ]
    (q (empty_db ()) untuple_query)

let test_q_untuple_ignores_placeholder_outputs () =
  let query =
    { find = [ Find_var "label" ]
    ; inputs =
        [ Input_scalar
            ( "pair"
            , Result_value (Tuple [ Some (String "left"); Some (String "right") ])
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ Ground (String "kept", "label")
        ; UntupleFunction (QVar "pair", [ "_"; "_" ])
        ]
    }
  in
  assert_equal_query
    "q untuple ignores placeholder outputs"
    [ [ Result_value (String "kept") ] ]
    (q (empty_db ()) query)

let test_q_untuple_accepts_list_values () =
  let query =
    { find = [ Find_var "right" ]
    ; inputs =
        [ Input_scalar
            ( "pair"
            , Result_value (List [ String "left"; String "right" ])
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ UntupleFunction (QVar "pair", [ "left"; "right" ]) ]
    }
  in
  assert_equal_query
    "q untuple destructures list values like sequential tuples"
    [ [ Result_value (String "right") ] ]
    (q (empty_db ()) query)

let test_q_builtin_ground_bindings () =
  let scalar_query =
    { find = [ Find_var "op" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Ground (Keyword "db/add", "op") ]
    }
  in
  assert_equal_query
    "q ground binds scalar values"
    [ [ Result_value (Keyword "db/add") ] ]
    (q (empty_db ()) scalar_query);
  let collection_query =
    { find = [ Find_var "vowel" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ GroundCollection ([ Keyword "a"; Keyword "e"; Keyword "i" ], "vowel") ]
    }
  in
  assert_equal_query
    "q ground binds each collection value"
    [ [ Result_value (Keyword "a") ]
    ; [ Result_value (Keyword "e") ]
    ; [ Result_value (Keyword "i") ]
    ]
    (q (empty_db ()) collection_query);
  let tuple_query =
    { find = [ Find_var "a"; Find_var "c" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ GroundTuple ([ Keyword "a"; Keyword "b"; Keyword "c" ], [ "a"; "_"; "c" ]) ]
    }
  in
  assert_equal_query
    "q ground destructures tuple values and ignores wildcard placeholders"
    [ [ Result_value (Keyword "a"); Result_value (Keyword "c") ] ]
    (q (empty_db ()) tuple_query);
  let relation_query =
    { find = [ Find_var "x"; Find_var "z" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ GroundRelation
            ( [ [ Keyword "a"; Keyword "b"; Keyword "c" ]
              ; [ Keyword "d"; Keyword "e"; Keyword "f" ]
              ]
            , [ "x"; "_"; "z" ] )
        ]
    }
  in
  assert_equal_query
    "q ground destructures relation rows and ignores wildcard placeholders"
    [ [ Result_value (Keyword "a"); Result_value (Keyword "c") ]
    ; [ Result_value (Keyword "d"); Result_value (Keyword "f") ]
    ]
    (q (empty_db ()) relation_query);
  assert_equal_query
    "q_string ground destructures dynamic tuple inputs"
    [ [ Result_value (Keyword "a"); Result_value (Keyword "c") ] ]
    (q_string
       ~inputs:[ Arg_scalar (Result_value (List [ Keyword "a"; Keyword "b"; Keyword "c" ])) ]
       (empty_db ())
       "[:find ?a ?c
         :in ?in
         :where [(ground ?in) [?a _ ?c]]]");
  assert_equal_query
    "q_string ground accepts wildcard outputs for dynamic inputs"
    [ [ Result_value (Keyword "a") ] ]
    (q_string
       ~inputs:[ Arg_scalar (Result_value (Keyword "a")) ]
       (empty_db ())
       "[:find ?in
         :in ?in
         :where [(ground ?in) _]]");
  assert_equal_query
    "q_string ground destructures dynamic relation inputs"
    [ [ Result_value (Keyword "a"); Result_value (Keyword "c") ]
    ; [ Result_value (Keyword "d"); Result_value (Keyword "f") ]
    ]
    (q_string
       ~inputs:
         [ Arg_scalar
             (Result_value
                (List
                   [ List [ Keyword "a"; Keyword "b"; Keyword "c" ]
                   ; List [ Keyword "d"; Keyword "e"; Keyword "f" ]
                   ]))
         ]
       (empty_db ())
       "[:find ?x ?z
         :in ?in
         :where [(ground ?in) [[?x _ ?z] ...]]]");
  assert_equal_query
    "q_string ground over empty collection input returns no rows"
    []
    (q_string
       ~inputs:[ Arg_collection [] ]
       (empty_db ())
       "[:find ?in
         :in [?in ...]
         :where [(ground ?in) _]]")

let test_q_builtin_function_insufficient_bindings_match_upstream_messages () =
  let db = empty_db () in
  assert_raises_invalid_arg_message
    "q predicate reports unknown callables like upstream query-fns"
    "Unknown predicate 'fun in [(fun ?e)]"
    (fun () ->
       ignore
         (q_string
            ~inputs:[ Arg_collection [ Result_value (Int 1) ] ]
            db
            "[:find ?e
              :in [?e ...]
              :where [(fun ?e)]]"));
  assert_raises_invalid_arg_message
    "q function reports unknown callables like upstream query-fns"
    "Unknown function 'fun in [(fun ?e) ?x]"
    (fun () ->
       ignore
         (q_string
            ~inputs:[ Arg_collection [ Result_value (Int 1) ] ]
            db
            "[:find ?e ?x
              :in [?e ...]
              :where [(fun ?e) ?x]]"));
  assert_raises_invalid_arg_message
    "q predicate reports insufficient bindings like upstream query-fns"
    "Insufficient bindings: #{?x} not bound in [(zero? ?x)]"
    (fun () -> ignore (q_string db "[:find ?x :where [(zero? ?x)]]"));
  assert_raises_invalid_arg_message
    "q function reports insufficient bindings like upstream query-fns"
    "Insufficient bindings: #{?x} not bound in [(inc ?x) ?y]"
    (fun () -> ignore (q_string db "[:find ?x :where [(inc ?x) ?y]]"))

let test_q_not_filters_matching_bindings () =
  let db =
    empty_db ~schema:[ "likes", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "likes", Many_values [ String "pizza"; String "fries" ]
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "likes", Many_values [ String "pie" ]
                 ]
             }
         ]
  in
  let query =
    { find = [ Find_var "name" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "name", QVar "name")
        ; Not [ Pattern (QVar "e", QAttr "likes", QValue (String "pie")) ]
        ]
    }
  in
  assert_equal_query
    "q not removes bindings whose nested clauses match"
    [ [ Result_value (String "Ivan") ] ]
    (q db query)

let test_q_not_rejects_clauses_without_outer_bindings () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Oleg") ] }
         ]
  in
  assert_raises_invalid_arg
    "q not rejects clauses before their vars are bound"
    (fun () ->
      ignore
        (q
           db
           { find = [ Find_var "e" ]
           ; inputs = []
           ; with_vars = []
           ; rules = []
           ; where =
               [ Not [ Pattern (QVar "e", QAttr "name", QValue (String "Ivan")) ]
               ; Pattern (QVar "e", QAttr "name", QWildcard)
               ]
           }));
  assert_raises_invalid_arg
    "q not rejects clauses with no vars bound in the outer context"
    (fun () ->
      ignore
        (q
           db
           { find = [ Find_var "e" ]
           ; inputs = []
           ; with_vars = []
           ; rules = []
           ; where =
               [ Pattern (QVar "e", QAttr "name", QWildcard)
               ; Not [ Pattern (QVar "other", QAttr "name", QValue (String "Ivan")) ]
               ]
           }))

let test_q_not_insufficient_bindings_match_upstream_messages () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 10) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Oleg"); "age", One_value (Int 20) ] }
         ]
  in
  assert_raises_invalid_arg_message
    "q not reports fully unbound clause vars like upstream"
    "Insufficient bindings: none of #{?e} is bound in (not [?e :name \"Ivan\"])"
    (fun () ->
       ignore (q_string db "[:find ?e :where (not [?e :name \"Ivan\"]) [?e :name]]"));
  assert_raises_invalid_arg_message
    "q nested not reports unbound nested vars like upstream"
    "Insufficient bindings: none of #{?a} is bound in (not [1 :age ?a])"
    (fun () ->
       ignore
         (q_string
            db
            "[:find ?e :where
              [?e :name]
              (not-join [?e]
                (not [1 :age ?a])
                [?e :age ?a])]"));
  assert_raises_invalid_arg_message
    "q not reports unbound vars independent of outer vars like upstream"
    "Insufficient bindings: none of #{?a} is bound in (not [?a :name \"Ivan\"])"
    (fun () ->
       ignore (q_string db "[:find ?e :where [?e :name] (not [?a :name \"Ivan\"])]"))

let test_q_not_join_projects_join_variables () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "artist", One_value (String "A"); "year", One_value (Int 1970) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "artist", One_value (String "A"); "year", One_value (Int 1971) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "artist", One_value (String "B"); "year", One_value (Int 1971) ] }
         ]
  in
  let query =
    { find = [ Find_var "release" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "release", QAttr "artist", QVar "artist")
        ; Pattern (QVar "release", QAttr "year", QVar "year")
        ; NotJoin
            ( [ "artist" ]
            , [ Pattern (QVar "release", QAttr "year", QValue (Int 1970))
              ; Pattern (QVar "release", QAttr "artist", QVar "artist")
              ]
            )
        ]
    }
  in
  assert_equal_query
    "q not-join only carries listed join vars into nested clauses"
    [ [ Result_entity 3 ] ]
    (q db query)

let test_q_not_join_rejects_unbound_join_vars () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] } ]
  in
  assert_raises_invalid_arg
    "q not-join requires join vars to be bound by previous clauses"
    (fun () ->
      ignore
        (q
           db
           { find = [ Find_var "e" ]
           ; inputs = []
           ; with_vars = []
           ; rules = []
           ; where =
               [ NotJoin
                   ( [ "e" ]
                   , [ Pattern (QVar "e", QAttr "name", QValue (String "Ivan")) ]
                   )
               ; Pattern (QVar "e", QAttr "name", QWildcard)
               ]
           }))

let test_q_not_matches_upstream_edge_cases () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 10) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 20) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg"); "age", One_value (Int 10) ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "name", One_value (String "Oleg"); "age", One_value (Int 20) ] }
         ; Entity { db_id = Some (Entity_id 5); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 10) ] }
         ; Entity { db_id = Some (Entity_id 6); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 20) ] }
         ]
  in
  assert_equal_query
    "q not handles const minus empty nested result"
    [ [ Result_entity 3 ] ]
    (q_string db "[:find ?e :where [?e :name \"Oleg\"] [?e :age 10] (not [?e :age 20])]");
  assert_equal_query
    "q not handles const minus matching const"
    []
    (q_string db "[:find ?e :where [?e :name \"Oleg\"] [?e :age 10] (not [?e :age 10])]");
  assert_equal_query
    "q not handles relation minus const"
    [ [ Result_entity 4 ] ]
    (q_string db "[:find ?e :where [?e :name \"Oleg\"] (not [?e :age 10])]");
  assert_equal_query_set
    "q not handles two relations minus two relations"
    [ [ Result_entity 2; Result_entity 1 ]
    ; [ Result_entity 6; Result_entity 5 ]
    ; [ Result_entity 1; Result_entity 1 ]
    ; [ Result_entity 2; Result_entity 2 ]
    ; [ Result_entity 5; Result_entity 5 ]
    ; [ Result_entity 6; Result_entity 6 ]
    ; [ Result_entity 2; Result_entity 5 ]
    ; [ Result_entity 1; Result_entity 5 ]
    ; [ Result_entity 2; Result_entity 6 ]
    ; [ Result_entity 6; Result_entity 1 ]
    ; [ Result_entity 5; Result_entity 1 ]
    ; [ Result_entity 6; Result_entity 2 ]
    ]
    (q_string
       db
       "[:find ?e ?e2
         :where [?e :name \"Ivan\"]
                [?e2 :name \"Ivan\"]
                (not [?e :age 10]
                     [?e2 :age 20])]");
  assert_equal_query_set
    "q not handles two relations minus relation plus const"
    [ [ Result_entity 2; Result_entity 3 ]
    ; [ Result_entity 1; Result_entity 3 ]
    ; [ Result_entity 2; Result_entity 4 ]
    ; [ Result_entity 6; Result_entity 3 ]
    ; [ Result_entity 5; Result_entity 3 ]
    ; [ Result_entity 6; Result_entity 4 ]
    ]
    (q_string
       db
       "[:find ?e ?e2
         :where [?e :name \"Ivan\"]
                [?e2 :name \"Oleg\"]
                (not [?e :age 10]
                     [?e2 :age 20])]");
  assert_equal_query_set
    "q not handles two relations minus two constants"
    [ [ Result_entity 4; Result_entity 3 ]
    ; [ Result_entity 3; Result_entity 3 ]
    ; [ Result_entity 4; Result_entity 4 ]
    ]
    (q_string
       db
       "[:find ?e ?e2
         :where [?e :name \"Oleg\"]
                [?e2 :name \"Oleg\"]
                (not [?e :age 10]
                     [?e2 :age 20])]")

let test_q_or_unions_branch_results () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg") ] }
         ]
  in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Or
            [ [ Pattern (QVar "e", QAttr "name", QValue (String "Ivan")) ]
            ; [ Pattern (QVar "e", QAttr "name", QValue (String "Oleg")) ]
            ]
        ]
    }
  in
  assert_equal_query
    "q or unions branch results"
    [ [ Result_entity 1 ]; [ Result_entity 3 ] ]
    (q db query)

let test_q_or_rejects_branches_with_different_free_vars () =
  assert_raises_invalid_arg
    "q or rejects branches with different free vars"
    (fun () ->
      ignore
        (q
           (empty_db ())
           { find = [ Find_var "e" ]
           ; inputs = []
           ; with_vars = []
           ; rules = []
           ; where =
               [ Or
                   [ [ Pattern (QVar "e", QAttr "name", QWildcard) ]
                   ; [ Pattern (QVar "e", QAttr "age", QVar "age") ]
                   ]
               ]
           }))

let test_q_or_matches_upstream_error_messages () =
  let db = empty_db () in
  assert_raises_invalid_arg_message
    "q or reports free-var branch mismatch like upstream"
    "All clauses in 'or' must use same set of free vars, had [#{?e} #{?a ?e}] in (or [?e :name _] [?e :age ?a])"
    (fun () ->
       ignore
         (q_string
            db
            "[:find ?e
              :where (or [?e :name _]
                         [?e :age ?a])]"));
  assert_raises_invalid_arg_message
    "q or-join reports required vars not bound like upstream"
    "Insufficient bindings: #{?e} not bound in (or-join [[?e]] [?e :name \"Ivan\"])"
    (fun () ->
       ignore
         (q_string
            db
            "[:find ?e
              :where (or-join [[?e]]
                       [?e :name \"Ivan\"])]"))

let test_q_or_allows_branch_vars_bound_by_outer_clauses () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 10) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ]
  in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "name", QVar "name")
        ; Or
            [ [ Pattern (QVar "e", QAttr "age", QValue (Int 10)) ]
            ; [ Pattern (QVar "e", QAttr "name", QVar "name") ]
            ]
        ]
    }
  in
  assert_equal_query
    "q or compares only branch vars not already bound outside the or"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q db query)

let test_q_or_join_projects_join_variables () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "artist", One_value (String "A"); "year", One_value (Int 1970) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "artist", One_value (String "A"); "year", One_value (Int 1971) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "artist", One_value (String "B"); "year", One_value (Int 1971) ] }
         ]
  in
  let query =
    { find = [ Find_var "release" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "release", QAttr "artist", QVar "artist")
        ; OrJoin
            ( [ "artist" ]
            , [ [ Pattern (QVar "release", QAttr "year", QValue (Int 1970))
                ; Pattern (QVar "release", QAttr "artist", QVar "artist")
                ]
              ; [ Pattern (QVar "release", QAttr "year", QValue (Int 1972))
                ; Pattern (QVar "release", QAttr "artist", QVar "artist")
                ]
              ]
            )
        ]
    }
  in
  assert_equal_query
    "q or-join only carries listed join vars into branch clauses"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q db query)

let test_q_or_join_binds_listed_branch_variables () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ]
  in
  let query =
    { find = [ Find_var "e"; Find_var "name" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ OrJoin
            ( [ "e"; "name" ]
            , [ [ Pattern (QVar "e", QAttr "name", QVar "name") ] ] )
        ]
    }
  in
  assert_equal_query
    "q or-join propagates listed vars bound inside branches"
    [ [ Result_entity 1; Result_value (String "Ivan") ]
    ; [ Result_entity 2; Result_value (String "Petr") ]
    ]
    (q db query)

let test_q_or_join_rejects_branches_missing_unbound_listed_vars () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 10) ] }
         ]
  in
  assert_raises_invalid_arg
    "q or-join requires every branch to bind listed vars that are not bound outside"
    (fun () ->
      ignore
        (q
           db
           { find = [ Find_var "e"; Find_var "name" ]
           ; inputs = []
           ; with_vars = []
           ; rules = []
           ; where =
               [ OrJoin
                   ( [ "e"; "name" ]
                   , [ [ Pattern (QVar "e", QAttr "name", QVar "name") ]
                     ; [ Pattern (QVar "e", QAttr "age", QWildcard) ]
                     ]
                   )
               ]
           }))

let test_q_or_join_constant_substitution () =
  let db =
    empty_db ~schema:[ "parent", ref_attr ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Oleg"); "parent", One_value (Ref 1) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Petr"); "parent", One_value (Ref 2) ] }
         ]
  in
  let reachable_query =
    { find = [ Find_var "name"; Find_var "x"; Find_var "y" ]
    ; inputs = [ Input_scalar ("name", Result_value (String "Ivan")) ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "x", QAttr "name", QVar "name")
        ; OrJoin
            ( [ "x"; "y" ]
            , [ [ Pattern (QVar "x", QAttr "parent", QVar "z")
                ; Pattern (QVar "z", QAttr "parent", QVar "y")
                ]
              ; [ Pattern (QVar "y", QAttr "parent", QVar "x") ]
              ]
            )
        ]
    }
  in
  assert_equal_query
    "q or-join preserves constants substituted before the branch"
    [ [ Result_value (String "Ivan"); Result_entity 1; Result_entity 2 ] ]
    (q db reachable_query);
  let empty_query =
    { reachable_query with
      where =
        [ Pattern (QVar "x", QAttr "name", QVar "name")
        ; OrJoin
            ( [ "x"; "y" ]
            , [ [ Pattern (QVar "x", QAttr "parent", QVar "z")
                ; Pattern (QVar "z", QAttr "parent", QVar "y")
                ]
              ; [ Pattern (QVar "x", QAttr "parent", QVar "y") ]
              ]
            )
        ]
    }
  in
  assert_equal_query
    "q or-join constants should not leak unrelated branch bindings"
    []
    (q db empty_query)

let test_q_or_join_required_vars_use_outer_bindings () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "age", One_value (Int 10) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "age", One_value (Int 11) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg") ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "age", One_value (Int 10); "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 5); attrs = [ "age", One_value (Int 11); "name", One_value (String "Oleg") ] }
         ]
  in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = [ Input_scalar ("a", Result_value (Int 10)) ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ OrJoinRequired
            ( [ "a" ]
            , [ "e" ]
            , [ [ Pattern (QVar "e", QAttr "age", QVar "a") ]
              ; [ Pattern (QVar "e", QAttr "name", QValue (String "Oleg")) ]
              ]
            )
        ]
    }
  in
  assert_equal_query
    "q or-join required vars are projected from outer bindings without requiring branches to bind them"
    [ [ Result_entity 1 ]; [ Result_entity 3 ]; [ Result_entity 4 ]; [ Result_entity 5 ] ]
    (q db query);
  assert_raises_invalid_arg
    "q or-join required vars must be bound before the clause"
    (fun () ->
      ignore
        (q
           db
           { query with
             inputs = []
           }))

let test_q_source_qualified_composite_clauses () =
  let names =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Oleg") ] }
         ]
  in
  let ages =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "age", One_value (Int 10) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "age", One_value (Int 20) ] }
         ]
  in
  let source_not_query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ SourcePattern ("names", QVar "e", QAttr "name", QWildcard)
        ; SourceNot ("ages", [ Pattern (QVar "e", QAttr "age", QValue (Int 10)) ])
        ]
    }
  in
  assert_equal_query
    "q source-qualified not changes the default source for nested clauses"
    [ [ Result_entity 2 ] ]
    (q_sources (empty_db ()) [ "names", Db_source names; "ages", Db_source ages ] source_not_query);
  let source_or_query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ SourcePattern ("names", QVar "e", QAttr "name", QWildcard)
        ; SourceOr
            ( "ages"
            , [ [ Pattern (QVar "e", QAttr "age", QValue (Int 10)) ]
              ; [ Pattern (QVar "e", QAttr "age", QValue (Int 20)) ]
              ]
            )
        ]
    }
  in
  assert_equal_query
    "q source-qualified or changes the default source for branch clauses"
    [ [ Result_entity 1 ]; [ Result_entity 2 ] ]
    (q_sources (empty_db ()) [ "names", Db_source names; "ages", Db_source ages ] source_or_query)

let test_q_not_or_upstream_source_and_relation_batch () =
  let names =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Oleg") ] }
         ]
  in
  let ages =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "age", One_value (Int 10) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "age", One_value (Int 20) ] }
         ]
  in
  assert_equal_query
    "q source-qualified nested not keeps the selected default source"
    [ [ Result_entity 1 ] ]
    (q_sources_string
       names
       [ "ages", Db_source ages ]
       "[:find ?e
         :in $ $ages
         :where [?e :name]
                ($ages not (not [?e :age 10]))]");
  assert_equal_query
    "q source-qualified nested not can override the nested default source"
    [ [ Result_entity 1 ] ]
    (q_sources_string
       names
       [ "ages", Db_source ages ]
       "[:find ?e
         :in $ $ages
         :where [?e :name]
                ($ages not ($ not [?e :name \"Ivan\"]))]");
  assert_equal_query
    "q source-qualified nested or keeps the selected default source"
    [ [ Result_entity 1 ] ]
    (q_sources_string
       names
       [ "ages", Db_source ages ]
       "[:find ?e
         :in $ $ages
         :where [?e :name]
                ($ages or (or [?e :age 10]))]");
  assert_equal_query
    "q source-qualified nested or can override the nested default source"
    [ [ Result_entity 1 ] ]
    (q_sources_string
       names
       [ "ages", Db_source ages ]
       "[:find ?e
         :in $ $ages
         :where [?e :name]
                ($ages or ($ or [?e :name \"Ivan\"]))]");
  let xs =
    Relation_source
      [ [ Result_value (Keyword "a1"); Result_value (Keyword "b1"); Result_value (Keyword "c1") ]
      ; [ Result_value (Keyword "a2"); Result_value (Keyword "b2"); Result_value (Keyword "c2") ]
      ; [ Result_value (Keyword "a3"); Result_value (Keyword "b3"); Result_value (Keyword "c3") ]
      ]
  in
  let ys =
    Relation_source
      [ [ Result_value (Keyword "a1"); Result_value (Keyword "b1"); Result_value (Keyword "d1") ]
      ; [ Result_value (Keyword "a2"); Result_value (Keyword "b2*"); Result_value (Keyword "d2") ]
      ; [ Result_value (Keyword "a4"); Result_value (Keyword "b4"); Result_value (Keyword "c4") ]
      ]
  in
  assert_equal_query
    "q or-join relation sources join only by listed vars and keep outer row values"
    [ [ Result_value (Keyword "a1"); Result_value (Keyword "b1"); Result_value (Keyword "c1") ]
    ; [ Result_value (Keyword "a2"); Result_value (Keyword "b2"); Result_value (Keyword "c2") ]
    ]
    (q_sources
       (empty_db ())
       [ "xs", xs; "ys", ys ]
       { find = [ Find_var "a"; Find_var "b"; Find_var "c" ]
       ; inputs = []
       ; with_vars = []
       ; rules = []
       ; where =
           [ SourceRelationPattern ("xs", [ QVar "a"; QVar "b"; QVar "c" ])
           ; OrJoin
               ( [ "a" ]
               , [ [ SourceRelationPattern ("ys", [ QVar "a"; QVar "b"; QVar "d" ]) ] ] )
           ]
       });
  assert_equal_query
    "q or-join relation sources can bind projected vars from branches with different arity"
    [ [ Result_value (Keyword "a1"); Result_value (Keyword "c1") ]
    ; [ Result_value (Keyword "a2"); Result_value (Keyword "c2") ]
    ]
    (q_sources
       (empty_db ())
       [ ( "xs"
         , Relation_source
             [ [ Result_value (Keyword "a1"); Result_value (Keyword "b1"); Result_value (Keyword "c1") ] ] )
       ; ( "ys"
         , Relation_source
             [ [ Result_value (Keyword "a2"); Result_value (Keyword "c2") ] ] )
       ]
       { find = [ Find_var "a"; Find_var "c" ]
       ; inputs = []
       ; with_vars = []
       ; rules = []
       ; where =
           [ OrJoin
               ( [ "a"; "c" ]
               , [ [ SourceRelationPattern ("xs", [ QVar "a"; QVar "b"; QVar "c" ]) ]
                 ; [ SourceRelationPattern ("ys", [ QVar "a"; QVar "c" ]) ]
                 ] )
           ]
       })

let test_q_with_scalar_inputs () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ]
  in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = [ Input_scalar ("wanted", Result_value (String "Ivan")) ]
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "name", QVar "wanted") ]
    }
  in
  assert_equal_query
    "q binds scalar inputs before evaluating clauses"
    [ [ Result_entity 1 ] ]
    (q db query)

let test_q_with_entity_ref_inputs () =
  let db =
    empty_db ~schema:[ "name", unique_identity; "friend", ref_attr ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 31) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "friend", One_value (Ref 1) ] }
         ]
  in
  let by_entity =
    { find = [ Find_var "age" ]
    ; inputs = [ Input_entity_ref ("person", Lookup_ref ("name", String "Ivan")) ]
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "person", QAttr "age", QVar "age") ]
    }
  in
  assert_equal_query
    "q resolves lookup-ref scalar inputs for entity positions"
    [ [ Result_value (Int 31) ] ]
    (q db by_entity);
  let by_value =
    { find = [ Find_var "friend" ]
    ; inputs = [ Input_entity_ref ("target", Lookup_ref ("name", String "Ivan")) ]
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "friend", QAttr "friend", QVar "target") ]
    }
  in
  assert_equal_query
    "q resolves lookup-ref scalar inputs for ref value positions"
    [ [ Result_entity 2 ] ]
    (q db by_value)

let test_q_with_lookup_ref_collection_inputs () =
  let db =
    empty_db ~schema:[ "name", unique_identity; "friend", ref_attr ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "friend", One_value (Ref 1) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg") ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "name", One_value (String "Sergey"); "friend", One_value (Ref 3) ] }
         ]
  in
  let scalar_query =
    { find = [ Find_var "friend"; Find_var "target" ]
    ; inputs = [ Input_scalar ("target", Result_value (Ref_to (Lookup_ref ("name", String "Ivan")))) ]
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "friend", QAttr "friend", QVar "target") ]
    }
  in
  assert_equal_query
    "q preserves lookup refs bound through scalar inputs"
    [ [ Result_entity 2; Result_value (Ref_to (Lookup_ref ("name", String "Ivan"))) ] ]
    (q db scalar_query);
  let query =
    { find = [ Find_var "friend"; Find_var "target" ]
    ; inputs =
        [ Input_collection
            ( "target"
            , [ Result_value (Ref_to (Lookup_ref ("name", String "Ivan")))
              ; Result_value (Ref_to (Lookup_ref ("name", String "Oleg")))
              ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "friend", QAttr "friend", QVar "target") ]
    }
  in
  assert_equal_query
    "q resolves lookup refs inside collection inputs"
    [ [ Result_entity 2; Result_value (Ref_to (Lookup_ref ("name", String "Ivan"))) ]
    ; [ Result_entity 4; Result_value (Ref_to (Lookup_ref ("name", String "Oleg"))) ]
    ]
    (q db query)

let test_q_with_lookup_ref_inputs_in_entity_builtins () =
  let db =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 15) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 22); "height", One_value (Int 240) ] }
         ]
  in
  let get_else_query =
    { find = [ Find_var "person"; Find_var "height" ]
    ; inputs = [ Input_scalar ("person", Result_value (Ref_to (Lookup_ref ("name", String "Ivan")))) ]
    ; with_vars = []
    ; rules = []
    ; where = [ GetElse (QVar "person", "height", String "Unknown", "height") ]
    }
  in
  assert_equal_query
    "q get-else resolves preserved lookup-ref scalar inputs"
    [ [ Result_value (Ref_to (Lookup_ref ("name", String "Ivan"))); Result_value (String "Unknown") ] ]
    (q db get_else_query);
  let direct_get_else_query =
    { find = [ Find_var "height" ]
    ; inputs = [ Input_entity_ref ("person", Lookup_ref ("name", String "Ivan")) ]
    ; with_vars = []
    ; rules = []
    ; where = [ GetElse (QVar "person", "height", String "Unknown", "height") ]
    }
  in
  assert_equal_query
    "q get-else resolves lookup-ref entity inputs like upstream issue-445"
    [ [ Result_value (String "Unknown") ] ]
    (q db direct_get_else_query);
  let get_some_query =
    { find = [ Find_var "person"; Find_var "attr"; Find_var "value" ]
    ; inputs = [ Input_scalar ("person", Result_value (Ref_to (Lookup_ref ("name", String "Petr")))) ]
    ; with_vars = []
    ; rules = []
    ; where = [ GetSome (QVar "person", [ "weight"; "age"; "height" ], "attr", "value") ]
    }
  in
  assert_equal_query
    "q get-some resolves preserved lookup-ref scalar inputs"
    [ [ Result_value (Ref_to (Lookup_ref ("name", String "Petr"))); Result_attr "age"; Result_value (Int 22) ] ]
    (q db get_some_query);
  let direct_get_some_query =
    { find = [ Find_var "person"; Find_var "attr"; Find_var "value" ]
    ; inputs = [ Input_entity_ref ("person", Lookup_ref ("name", String "Petr")) ]
    ; with_vars = []
    ; rules = []
    ; where = [ GetSome (QVar "person", [ "weight"; "age"; "height" ], "attr", "value") ]
    }
  in
  assert_equal_query
    "q get-some resolves lookup-ref entity inputs like upstream issue-445"
    [ [ Result_entity 2; Result_attr "age"; Result_value (Int 22) ] ]
    (q db direct_get_some_query)

let test_q_with_relation_inputs () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg") ] }
         ]
  in
  let query =
    { find = [ Find_var "name" ]
    ; inputs =
        [ Input_relation
            ( [ "name" ]
            , [ [ Result_value (String "Ivan") ]
              ; [ Result_value (String "Oleg") ]
              ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QWildcard, QAttr "name", QVar "name") ]
    }
  in
  assert_equal_query
    "q joins relation inputs with db clauses"
    [ [ Result_value (String "Ivan") ]; [ Result_value (String "Oleg") ] ]
    (q db query)

let test_q_with_collection_inputs () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg") ] }
         ]
  in
  let query =
    { find = [ Find_var "name" ]
    ; inputs =
        [ Input_collection
            ( "name"
            , [ Result_value (String "Ivan")
              ; Result_value (String "Oleg")
              ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QWildcard, QAttr "name", QVar "name") ]
    }
  in
  assert_equal_query
    "q binds collection inputs as one binding per value"
    [ [ Result_value (String "Ivan") ]; [ Result_value (String "Oleg") ] ]
    (q db query)

let test_q_with_tuple_inputs () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "first", One_value (String "Ivan"); "last", One_value (String "Petrov") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "first", One_value (String "Oleg"); "last", One_value (String "Ivanov") ] }
         ]
  in
  let query =
    { find = [ Find_var "e" ]
    ; inputs =
        [ Input_tuple
            ( [ "first"; "last" ]
            , [ Result_value (String "Ivan"); Result_value (String "Petrov") ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "first", QVar "first")
        ; Pattern (QVar "e", QAttr "last", QVar "last")
        ]
    }
  in
  assert_equal_query
    "q binds tuple inputs as a single multi-var binding"
    [ [ Result_entity 1 ] ]
    (q db query)

let test_q_with_dynamic_callable_inputs () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 31) ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 19) ]
             }
         ]
  in
  let adult = function
    | [ Result_value (Int age) ] -> age >= 21
    | _ -> false
  in
  assert_equal_query
    "q_string accepts dynamic predicate inputs in call position"
    [ [ Result_value (String "Ivan") ] ]
    (q_string
       ~inputs:[ Arg_predicate adult ]
       db
       "[:find ?name
         :in ?adult
         :where [?e :name ?name]
                [?e :age ?age]
                [(?adult ?age)]]");
  let excited_name = function
    | [ Result_value (String "Petr") ] -> None
    | [ Result_value (String name) ] -> Some [ Result_value (String (name ^ "!")) ]
    | _ -> None
  in
  assert_equal_query
    "q_string accepts dynamic function inputs and filters nil results"
    [ [ Result_value (String "Ivan!") ] ]
    (q_string
       ~inputs:[ Arg_function excited_name ]
       db
       "[:find ?label
         :in ?label-fn
         :where [?e :name ?name]
                [(?label-fn ?name) ?label]]");
  let five = function
    | [] -> Some [ Result_value (Int 5) ]
    | _ -> None
  in
  assert_equal_query
    "q_string filters unrelated joins after zero-argument dynamic functions like upstream issue-385"
    []
    (q_string
       ~inputs:[ Arg_function five ]
       (empty_db () |> db_with [ Entity { db_id = None; attrs = [ "person/name", One_value (String "Joe") ] } ])
       "[:find ?name
         :in ?my-fn
         :where [?e :person/name ?name]
                [(?my-fn) ?result]
                [(< ?result 3)]]");
  assert_equal_query
    "q_string skips dynamic predicates when their binding relation is empty like upstream issue-180"
    []
    (q_string
       (empty_db () |> db_with [ Add (Entity_id 1, "age", Int 20) ])
       "[:find ?e ?age
         :where [_ :pred ?pred]
                [?e :age ?age]
                [(?pred ?age)]]");
  let age_matches = function
    | [ Result_db source_db; Result_entity entity_id; Result_value (Int expected_age) ] ->
      (match entity source_db (Entity_id entity_id) with
       | Some entity -> entity_attr entity "age" = Some (One_value (Int expected_age))
       | None -> false)
    | _ -> false
  in
  assert_equal_query
    "q_string passes the default db source to dynamic predicate inputs"
    [ [ Result_value (String "Ivan") ]; [ Result_value (String "Petr") ] ]
    (q_string
       ~inputs:[ Arg_predicate age_matches ]
       db
       "[:find ?name
         :in $ ?age-matches
         :where [?e :name ?name]
                [?e :age ?age]
                [(?age-matches $ ?e ?age)]]");
  assert_equal_query
    "q_sources_string passes named db sources to dynamic predicate inputs"
    [ [ Result_value (String "Ivan") ]; [ Result_value (String "Petr") ] ]
    (q_sources_string
       ~inputs:[ Arg_predicate age_matches ]
       (empty_db ())
       [ "people", Db_source db ]
       "[:find ?name
         :in $people ?age-matches
         :where [$people ?e :name ?name]
                [$people ?e :age ?age]
                [(?age-matches $people ?e ?age)]]");
  let range_values = function
    | [ Result_value (Int min_value); Result_value (Int max_value) ] ->
      let rec collect value acc =
        if value >= max_value then List.rev acc
        else collect (value + 1) (Int value :: acc)
      in
      Some [ Result_value (List (collect min_value [])) ]
    | _ -> None
  in
  assert_equal_query
    "q_string accepts dynamic function inputs with collection output bindings"
    [ [ Result_value (Keyword "a"); Result_value (Int 2) ]
    ; [ Result_value (Keyword "a"); Result_value (Int 4) ]
    ; [ Result_value (Keyword "a"); Result_value (Int 6) ]
    ; [ Result_value (Keyword "b"); Result_value (Int 2) ]
    ]
    (q_string
       ~inputs:
         [ Arg_relation
             [ [ Result_value (Keyword "a"); Result_value (List [ Int 1; Int 7 ]) ]
             ; [ Result_value (Keyword "b"); Result_value (List [ Int 2; Int 4 ]) ]
             ]
         ; Arg_function range_values
         ]
       (empty_db ())
       "[:find ?k ?x
         :in [[?k [?min ?max]] ...] ?range
         :where [(?range ?min ?max) [?x ...]]
                [(even? ?x)]]");
  let rows = function
    | [ Result_value (List rows) ] -> Some [ Result_value (List rows) ]
    | _ -> None
  in
  assert_equal_query
    "q_string accepts dynamic function inputs with relation output bindings"
    [ [ Result_value (Keyword "a"); Result_value (Keyword "c") ]
    ; [ Result_value (Keyword "d"); Result_value (Keyword "f") ]
    ]
    (q_string
       ~inputs:
         [ Arg_scalar
             (Result_value
                (List
                   [ List [ Keyword "a"; Keyword "b"; Keyword "c" ]
                   ; List [ Keyword "d"; Keyword "e"; Keyword "f" ]
                   ]))
         ; Arg_function rows
         ]
       (empty_db ())
       "[:find ?x ?z
         :in ?rows ?as-relation
         :where [(?as-relation ?rows) [[?x _ ?z] ...]]]")

let test_q_nested_relation_map_inputs () =
  assert_equal_query
    "q_string supports queries with only scalar inputs and no db source"
    [ [ Result_value (Int 10); Result_value (Int 20) ] ]
    (q_string
       ~inputs:[ Arg_scalar (Result_value (Int 10)); Arg_scalar (Result_value (Int 20)) ]
       (empty_db ())
       "[:find ?a ?b :in ?a ?b]");
  assert_equal_query
    "q_string binds plain map inputs as relation rows"
    [ [ Result_value (Keyword "b"); Result_value (Int 2) ]
    ; [ Result_value (Keyword "c"); Result_value (Int 3) ]
    ]
    (q_string
       ~inputs:
         [ Arg_scalar
             (Result_value
                (Map [ Keyword "a", Int 1; Keyword "b", Int 2; Keyword "c", Int 3 ]))
         ]
       (empty_db ())
       "[:find ?k ?v
         :in [[?k ?v] ...]
         :where [(> ?v 1)]]");
  let minmax = function
    | [ Result_value (List values) ] ->
      (match values with
       | [] -> None
       | first :: rest ->
         let min_value, max_value =
           List.fold_left
             (fun (min_value, max_value) -> function
                | Int value -> min min_value value, max max_value value
                | _ -> min_value, max_value)
             (match first with
              | Int value -> value, value
              | _ -> 0, 0)
             rest
         in
         Some [ Result_value (Int min_value); Result_value (Int max_value) ])
    | _ -> None
  in
  assert_equal_query
    "q_string binds map relation rows through dynamic tuple outputs"
    [ [ Result_value (Keyword "a"); Result_value (Int 1); Result_value (Int 4) ]
    ; [ Result_value (Keyword "b"); Result_value (Int 5); Result_value (Int 7) ]
    ]
    (q_string
       ~inputs:
         [ Arg_scalar
             (Result_value
                (Map
                   [ Keyword "a", List [ Int 1; Int 2; Int 4 ]
                   ; Keyword "b", List [ Int 5; Int 7 ]
                   ]))
         ; Arg_function minmax
         ]
       (empty_db ())
       "[:find ?k ?min ?max
         :in [[?k ?v] ...] ?minmax
         :where [(?minmax ?v) [?min ?max]]
                [(> ?max ?min)]]");
  let range_values = function
    | [ Result_value (Int min_value); Result_value (Int max_value) ] ->
      let rec collect value acc =
        if value >= max_value then List.rev acc
        else collect (value + 1) (Int value :: acc)
      in
      Some [ Result_value (List (collect min_value [])) ]
    | _ -> None
  in
  assert_equal_query
    "q_string binds nested map relation rows through dynamic collection outputs"
    [ [ Result_value (Keyword "a"); Result_value (Int 2) ]
    ; [ Result_value (Keyword "a"); Result_value (Int 4) ]
    ; [ Result_value (Keyword "a"); Result_value (Int 6) ]
    ; [ Result_value (Keyword "b"); Result_value (Int 2) ]
    ]
    (q_string
       ~inputs:
         [ Arg_scalar
             (Result_value
                (Map
                   [ Keyword "a", List [ Int 1; Int 7 ]
                   ; Keyword "b", List [ Int 2; Int 4 ]
                   ]))
         ; Arg_function range_values
         ]
       (empty_db ())
       "[:find ?k ?x
         :in [[?k [?min ?max]] ...] ?range
         :where [(?range ?min ?max) [?x ...]]
                [(even? ?x)]]");
  assert_equal_query
    "q_string binds map inputs as nested relation rows"
    [ [ Result_value (Keyword "b"); Result_value (Int 2) ]
    ; [ Result_value (Keyword "c"); Result_value (Int 3) ]
    ]
    (q_string
       ~inputs:
         [ Arg_scalar
             (Result_value
                (Map
                   [ Keyword "a", List [ Int 1 ]
                   ; Keyword "b", List [ Int 2 ]
                   ; Keyword "c", List [ Int 3 ]
                   ]))
         ]
       (empty_db ())
       "[:find ?k ?v
         :in [[?k [?v]] ...]
         :where [(> ?v 1)]]")

let test_q_with_dynamic_callable_inputs_in_rules () =
  let follow_db =
    empty_db ~schema:[ "follow", many ] ()
    |> db_with
         [ Add (Entity_id 1, "follow", Ref 2)
         ; Add (Entity_id 2, "follow", Ref 3)
         ; Add (Entity_id 2, "follow", Ref 4)
         ; Add (Entity_id 4, "follow", Ref 6)
         ]
  in
  let even_entity = function
    | [ Result_entity entity_id ] -> entity_id mod 2 = 0
    | _ -> false
  in
  assert_equal_query
    "q_string passes dynamic predicate inputs through rule parameters"
    [ [ Result_entity 2; Result_entity 4 ]; [ Result_entity 4; Result_entity 6 ] ]
    (q_string
       ~inputs:[ Arg_predicate even_entity ]
       follow_db
       "{:find [?from ?to]
         :in [$ % ?even]
         :where [(match ?even ?from ?to)]
         :rules [[(match ?pred ?from ?to)
                  [?from :follow ?to]
                  [(?pred ?from)]
                  [(?pred ?to)]]]}");
  let name_db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ]
  in
  let display_name = function
    | [ Result_value (String "Petr") ] -> None
    | [ Result_value (String name) ] -> Some [ Result_value (String (name ^ "!")) ]
    | _ -> None
  in
  assert_equal_query
    "q_string passes dynamic function inputs through rule parameters"
    [ [ Result_value (String "Ivan!") ] ]
    (q_string
       ~inputs:[ Arg_function display_name ]
       name_db
       "{:find [?label]
         :in [$ % ?label-fn]
         :where [(label ?label-fn ?label)]
         :rules [[(label ?f ?label)
                  [?e :name ?name]
                  [(?f ?name) ?label]]]}")

let test_q_input_placeholders_ignore_values () =
  let collection_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?x"
      ; QueryFormKeyword "in"
      ; QueryFormVector [ QueryFormSymbol "_"; QueryFormSymbol "..." ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "ground"; QueryFormKeyword "ok" ]; QueryFormSymbol "?x" ]
      ]
  in
  assert_equal_query
    "q collection input placeholders consume but ignore all values"
    [ [ Result_value (Keyword "ok") ] ]
    (q
       ~inputs:[ Arg_collection [ Result_value (Keyword "ignored-1"); Result_value (Keyword "ignored-2") ] ]
       (empty_db ())
       (parse_query collection_query));
  let tuple_query =
    { find = [ Find_var "x"; Find_var "z" ]
    ; inputs =
        [ Input_tuple
            ( [ "x"; "_"; "_"; "z" ]
            , [ Result_value (Keyword "x"); Result_value (Keyword "ignored-1"); Result_value (Keyword "ignored-2"); Result_value (Keyword "z") ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = []
    }
  in
  assert_equal_query
    "q tuple input placeholders ignore all values at placeholder positions"
    [ [ Result_value (Keyword "x"); Result_value (Keyword "z") ] ]
    (q (empty_db ()) tuple_query);
  let relation_query =
    { find = [ Find_var "x"; Find_var "z" ]
    ; inputs =
        [ Input_relation
            ( [ "x"; "_"; "_"; "z" ]
            , [ [ Result_value (Keyword "a"); Result_value (Int 1); Result_value (Int 2); Result_value (Keyword "b") ]
              ; [ Result_value (Keyword "c"); Result_value (Int 3); Result_value (Int 4); Result_value (Keyword "d") ]
              ]
            )
        ]
    ; with_vars = []
    ; rules = []
    ; where = []
    }
  in
  assert_equal_query
    "q relation input placeholders ignore each row value at placeholder positions"
    [ [ Result_value (Keyword "a"); Result_value (Keyword "b") ]
    ; [ Result_value (Keyword "c"); Result_value (Keyword "d") ]
    ]
    (q (empty_db ()) relation_query)

let test_q_return_shapes () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 31) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 37) ] }
         ]
  in
  let query =
    { find = [ Find_var "name"; Find_var "age" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "name", QVar "name"); Pattern (QVar "e", QAttr "age", QVar "age") ]
    }
  in
  if
    q_return db Return_relation query
    <> Query_relation
         [ [ Result_value (String "Ivan"); Result_value (Int 31) ]
         ; [ Result_value (String "Petr"); Result_value (Int 37) ]
         ]
  then failwith "q_return relation should preserve q rows";
  if
    q_return db Return_collection { query with find = [ Find_var "name" ] }
    <> Query_collection [ Result_value (String "Ivan"); Result_value (String "Petr") ]
  then failwith "q_return collection should return first column values";
  if
    q_return db Return_tuple query
    <> Query_tuple (Some [ Result_value (String "Ivan"); Result_value (Int 31) ])
  then failwith "q_return tuple should return first row";
  if
    q_return db Return_scalar { query with find = [ Find_var "name" ] }
    <> Query_scalar (Some (Result_value (String "Ivan")))
  then failwith "q_return scalar should return first value"

let test_parse_query_return_shapes () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 31) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 37) ] }
         ]
  in
  let collection_form =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormVector [ QueryFormSymbol "?name"; QueryFormSymbol "..." ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "_"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ]
  in
  let return, query = parse_query_return collection_form in
  if return <> Return_collection then failwith "parse_query_return should detect find collection";
  if
    q_return db return query
    <> Query_collection [ Result_value (String "Ivan"); Result_value (String "Petr") ]
  then failwith "parse_query_return collection should produce collection output";
  if
    q_return db Return_collection (parse_query collection_form)
    <> Query_collection [ Result_value (String "Ivan"); Result_value (String "Petr") ]
  then failwith "parse_query should accept find collection syntax";
  let list_collection_form =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormList [ QueryFormSymbol "?name"; QueryFormSymbol "..." ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "_"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ]
  in
  let return, query = parse_query_return list_collection_form in
  if return <> Return_collection then
    failwith "parse_query_return should detect list-form find collection";
  if
    q_return db return query
    <> Query_collection [ Result_value (String "Ivan"); Result_value (String "Petr") ]
  then failwith "parse_query_return list collection should produce collection output";
  let tuple_form =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormVector [ QueryFormSymbol "?name"; QueryFormSymbol "?age" ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormInt 1; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector [ QueryFormInt 1; QueryFormKeyword "age"; QueryFormSymbol "?age" ]
      ]
  in
  let return, query = parse_query_return tuple_form in
  if return <> Return_tuple then failwith "parse_query_return should detect find tuple";
  if
    q_return db return query
    <> Query_tuple (Some [ Result_value (String "Ivan"); Result_value (Int 31) ])
  then failwith "parse_query_return tuple should produce tuple output";
  let list_tuple_form =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormList [ QueryFormSymbol "?name"; QueryFormSymbol "?age" ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormInt 1; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector [ QueryFormInt 1; QueryFormKeyword "age"; QueryFormSymbol "?age" ]
      ]
  in
  let return, query = parse_query_return list_tuple_form in
  if return <> Return_tuple then failwith "parse_query_return should detect list-form find tuple";
  if
    q_return db return query
    <> Query_tuple (Some [ Result_value (String "Ivan"); Result_value (Int 31) ])
  then failwith "parse_query_return list tuple should produce tuple output";
  let scalar_form =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormSymbol "."
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormInt 1; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ]
  in
  let return, query = parse_query_return scalar_form in
  if return <> Return_scalar then failwith "parse_query_return should detect find scalar";
  if q_return db return query <> Query_scalar (Some (Result_value (String "Ivan"))) then
    failwith "parse_query_return scalar should produce scalar output";
  let aggregate_collection_form =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormVector
          [ QueryFormList [ QueryFormSymbol "count"; QueryFormSymbol "?name" ]
          ; QueryFormSymbol "..."
          ]
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "_"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ]
  in
  let return, query = parse_query_return aggregate_collection_form in
  if return <> Return_collection then failwith "parse_query_return should detect aggregate find collection";
  if q_return db return query <> Query_collection [ Result_value (Int 2) ] then
    failwith "parse_query_return aggregate collection should produce collection output";
  if
    q_return_string db "[:find [(count ?name)] :where [_ :name ?name]]"
    <> Query_tuple (Some [ Result_value (Int 2) ])
  then failwith "q_return_string aggregate tuple find spec should produce tuple output";
  if
    q_return_string db "[:find (count ?name) . :where [_ :name ?name]]"
    <> Query_scalar (Some (Result_value (Int 2)))
  then failwith "q_return_string aggregate scalar find spec should produce scalar output"

let test_q_return_find_specs_match_upstream_cases () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 44) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 25) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Sergey"); "age", One_value (Int 11) ] }
         ]
  in
  if
    q_return_string db "[:find [?name ...] :where [_ :name ?name]]"
    <> Query_collection
         [ Result_value (String "Ivan")
         ; Result_value (String "Petr")
         ; Result_value (String "Sergey")
         ]
  then failwith "q_return_string collection find spec should return all names";
  let expected_rows =
    [ [ Result_value (String "Petr"); Result_value (Int 44) ]
    ; [ Result_value (String "Ivan"); Result_value (Int 25) ]
    ; [ Result_value (String "Sergey"); Result_value (Int 11) ]
    ]
  in
  (match q_return_string db "[:find [?name ?age] :where [?e :name ?name] [?e :age ?age]]" with
   | Query_tuple (Some row) ->
     assert_bool "tuple find spec should cut multiple results to one row" (List.mem row expected_rows)
   | _ -> failwith "tuple find spec should return one tuple");
  (match q_return_string db "[:find ?name . :where [_ :name ?name]]" with
   | Query_scalar (Some name) ->
     assert_bool
       "scalar find spec should cut multiple results to one value"
       (List.mem name [ Result_value (String "Ivan"); Result_value (String "Petr"); Result_value (String "Sergey") ])
   | _ -> failwith "scalar find spec should return one value");
  if
    q_return_string db "[:find [(count ?name) ...] :where [_ :name ?name]]"
    <> Query_collection [ Result_value (Int 3) ]
  then failwith "aggregate collection find spec should return aggregate value";
  if
    q_return_string db "[:find [(count ?name)] :where [_ :name ?name]]"
    <> Query_tuple (Some [ Result_value (Int 3) ])
  then failwith "aggregate tuple find spec should return aggregate value";
  if
    q_return_string db "[:find (count ?name) . :where [_ :name ?name]]"
    <> Query_scalar (Some (Result_value (Int 3)))
  then failwith "aggregate scalar find spec should return aggregate value"

let test_q_return_map_shapes () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 31) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 37) ] }
         ]
  in
  let query =
    { find = [ Find_var "name"; Find_var "age" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "name", QVar "name"); Pattern (QVar "e", QAttr "age", QVar "age") ]
    }
  in
  if
    q_return_map db Return_relation (Return_keys [ "n"; "a" ]) query
    <> Query_relation_maps
         [ [ Keyword "a", Result_value (Int 31); Keyword "n", Result_value (String "Ivan") ]
         ; [ Keyword "a", Result_value (Int 37); Keyword "n", Result_value (String "Petr") ]
         ]
  then failwith "q_return_map relation should map rows by labels";
  if
    q_return_map db Return_relation (Return_syms [ "n"; "a" ]) query
    <> Query_relation_maps
         [ [ Symbol "a", Result_value (Int 31); Symbol "n", Result_value (String "Ivan") ]
         ; [ Symbol "a", Result_value (Int 37); Symbol "n", Result_value (String "Petr") ]
         ]
  then failwith "q_return_map relation should support :syms labels";
  if
    q_return_map db Return_relation (Return_strs [ "n"; "a" ]) query
    <> Query_relation_maps
         [ [ String "a", Result_value (Int 31); String "n", Result_value (String "Ivan") ]
         ; [ String "a", Result_value (Int 37); String "n", Result_value (String "Petr") ]
         ]
  then failwith "q_return_map relation should support :strs labels";
  if
    q_return_map db Return_tuple (Return_strs [ "name"; "age" ]) query
    <> Query_tuple_map (Some [ String "age", Result_value (Int 31); String "name", Result_value (String "Ivan") ])
  then failwith "q_return_map tuple should map the first row by labels";
  if
    q_return_map db Return_tuple (Return_syms [ "name"; "age" ]) query
    <> Query_tuple_map (Some [ Symbol "age", Result_value (Int 31); Symbol "name", Result_value (String "Ivan") ])
  then failwith "q_return_map :syms should preserve symbol labels";
  if
    q_return_map_string db "[:find [?name ?age] :syms name age :where [1 :name ?name] [1 :age ?age]]"
    <> Query_tuple_map (Some [ Symbol "age", Result_value (Int 31); Symbol "name", Result_value (String "Ivan") ])
  then failwith "q_return_map_string :syms should parse symbol labels";
  assert_raises_invalid_arg
    "q_return_map rejects mismatched label count"
    (fun () -> ignore (q_return_map db Return_relation (Return_keys [ "name" ]) query));
  assert_raises_invalid_arg
    "q_return_map rejects collection returns"
    (fun () -> ignore (q_return_map db Return_collection (Return_keys [ "name" ]) query))

let test_q_return_map_string_upstream_shape_batch () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 44) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 25) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Sergey"); "age", One_value (Int 11) ] }
         ]
  in
  let expected_keys =
    Query_relation_maps
      [ [ Keyword "a", Result_value (Int 25); Keyword "n", Result_value (String "Ivan") ]
      ; [ Keyword "a", Result_value (Int 44); Keyword "n", Result_value (String "Petr") ]
      ; [ Keyword "a", Result_value (Int 11); Keyword "n", Result_value (String "Sergey") ]
      ]
  in
  if
    q_return_map_string
      db
      "[:find ?name ?age
        :keys n a
        :where [?e :name ?name]
               [?e :age ?age]]"
    <> expected_keys
  then failwith "q_return_map_string should execute upstream :keys relation maps";
  let expected_syms =
    Query_relation_maps
      [ [ Symbol "a", Result_value (Int 25); Symbol "n", Result_value (String "Ivan") ]
      ; [ Symbol "a", Result_value (Int 44); Symbol "n", Result_value (String "Petr") ]
      ; [ Symbol "a", Result_value (Int 11); Symbol "n", Result_value (String "Sergey") ]
      ]
  in
  if
    q_return_map_string
      db
      "[:find ?name ?age
        :syms n a
        :where [?e :name ?name]
               [?e :age ?age]]"
    <> expected_syms
  then failwith "q_return_map_string should execute upstream :syms relation maps";
  let expected_strs =
    Query_relation_maps
      [ [ String "a", Result_value (Int 25); String "n", Result_value (String "Ivan") ]
      ; [ String "a", Result_value (Int 44); String "n", Result_value (String "Petr") ]
      ; [ String "a", Result_value (Int 11); String "n", Result_value (String "Sergey") ]
      ]
  in
  if
    q_return_map_string
      db
      "[:find ?name ?age
        :strs n a
        :where [?e :name ?name]
               [?e :age ?age]]"
    <> expected_strs
  then failwith "q_return_map_string should execute upstream :strs relation maps";
  if
    q_return_map_string
      db
      "[:find [?name ?age]
        :keys n a
        :where [?e :name ?name]
               [(= ?name \"Ivan\")]
               [?e :age ?age]]"
    <> Query_tuple_map (Some [ Keyword "a", Result_value (Int 25); Keyword "n", Result_value (String "Ivan") ])
  then failwith "q_return_map_string should execute upstream tuple :keys maps"

let test_parse_query_return_map_shapes () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 31) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 37) ] }
         ]
  in
  let keys_form =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormSymbol "?age"
      ; QueryFormKeyword "keys"
      ; QueryFormSymbol "n"
      ; QueryFormSymbol "a"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormSymbol "?age" ]
      ]
  in
  let return, return_map, query = parse_query_return_map keys_form in
  if return <> Return_relation then failwith "parse_query_return_map should preserve relation return shape";
  if return_map <> Some (Return_keys [ "n"; "a" ]) then
    failwith "parse_query_return_map should parse :keys labels";
  if
    q_return_map db return (Option.get return_map) query
    <> Query_relation_maps
         [ [ Keyword "a", Result_value (Int 31); Keyword "n", Result_value (String "Ivan") ]
         ; [ Keyword "a", Result_value (Int 37); Keyword "n", Result_value (String "Petr") ]
         ]
  then failwith "parsed :keys query should produce relation maps";
  let tuple_form =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormVector [ QueryFormSymbol "?name"; QueryFormSymbol "?age" ]
      ; QueryFormKeyword "strs"
      ; QueryFormSymbol "name"
      ; QueryFormSymbol "age"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormInt 1; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ; QueryFormVector [ QueryFormInt 1; QueryFormKeyword "age"; QueryFormSymbol "?age" ]
      ]
  in
  let return, return_map, query = parse_query_return_map tuple_form in
  if return <> Return_tuple then failwith "parse_query_return_map should preserve tuple return shape";
  if return_map <> Some (Return_strs [ "name"; "age" ]) then
    failwith "parse_query_return_map should parse :strs labels";
  if
    q_return_map db return (Option.get return_map) query
    <> Query_tuple_map (Some [ String "age", Result_value (Int 31); String "name", Result_value (String "Ivan") ])
  then failwith "parsed :strs tuple query should produce a tuple map";
  assert_raises_invalid_arg
    "parse_query_return_map rejects multiple return map clauses"
    (fun () ->
       ignore
         (parse_query_return_map
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?name"
               ; QueryFormKeyword "keys"
               ; QueryFormSymbol "n"
               ; QueryFormKeyword "strs"
               ; QueryFormSymbol "name"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "_"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
               ])));
  assert_raises_invalid_arg
    "parse_query_return_map rejects mismatched labels"
    (fun () ->
       ignore
         (parse_query_return_map
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?name"
               ; QueryFormSymbol "?age"
               ; QueryFormKeyword "keys"
               ; QueryFormSymbol "n"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
               ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "age"; QueryFormSymbol "?age" ]
               ])));
  assert_raises_invalid_arg
    "parse_query_return_map rejects collection find return maps"
    (fun () ->
       ignore
         (parse_query_return_map
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormVector [ QueryFormSymbol "?name"; QueryFormSymbol "..." ]
               ; QueryFormKeyword "keys"
               ; QueryFormSymbol "n"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "_"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
               ])));
  assert_raises_invalid_arg
    "parse_query_return_map rejects scalar find return maps"
    (fun () ->
       ignore
         (parse_query_return_map
            (QueryFormVector
               [ QueryFormKeyword "find"
               ; QueryFormSymbol "?name"
               ; QueryFormSymbol "."
               ; QueryFormKeyword "keys"
               ; QueryFormSymbol "n"
               ; QueryFormKeyword "where"
               ; QueryFormVector [ QueryFormSymbol "_"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
               ])))

let test_q_resolves_lookup_refs_in_patterns () =
  let db =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "friend", One_value (Ref 1) ] }
         ]
  in
  let by_entity =
    { find = [ Find_var "name" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QLookupRef ("name", String "Ivan"), QAttr "name", QVar "name") ]
    }
  in
  assert_equal_query
    "q resolves lookup refs in entity position"
    [ [ Result_value (String "Ivan") ] ]
    (q db by_entity);
  let by_ref_value =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "friend", QValue (Ref_to (Lookup_ref ("name", String "Ivan")))) ]
    }
  in
  assert_equal_query
    "q resolves lookup refs in ref value position"
    [ [ Result_entity 2 ] ]
    (q db by_ref_value)

let test_parse_query_resolves_lookup_refs_in_patterns () =
  let db =
    empty_db ~schema:[ "name", unique_identity; "friend", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 31) ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "Petr"); "friend", One_value (Ref 1) ]
             }
         ]
  in
  let entity_lookup_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?age"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormVector [ QueryFormKeyword "name"; QueryFormString "Ivan" ]
          ; QueryFormKeyword "age"
          ; QueryFormSymbol "?age"
          ]
      ]
  in
  assert_equal_query
    "parse_query resolves lookup refs in entity position"
    [ [ Result_value (Int 31) ] ]
    (q db (parse_query entity_lookup_query));
  let ref_value_lookup_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?friend"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormSymbol "?friend"
          ; QueryFormKeyword "friend"
          ; QueryFormVector [ QueryFormKeyword "name"; QueryFormString "Ivan" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query resolves lookup refs in ref value position"
    [ [ Result_entity 2 ] ]
    (q db (parse_query ref_value_lookup_query));
  let source_lookup_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?age"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$people"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormSymbol "$people"
          ; QueryFormVector [ QueryFormKeyword "name"; QueryFormString "Ivan" ]
          ; QueryFormKeyword "age"
          ; QueryFormSymbol "?age"
          ]
      ]
  in
  assert_equal_query
    "parse_query resolves source-qualified lookup refs"
    [ [ Result_value (Int 31) ] ]
    (q_sources (empty_db ()) [ "people", Db_source db ] (parse_query source_lookup_query))

let test_q_with_multiple_sources () =
  let db1 =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "email", One_value (String "ivan@example.com") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "email", One_value (String "petr@example.com") ] }
         ]
  in
  let db2 =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 10); attrs = [ "email", One_value (String "ivan@example.com"); "score", One_value (Int 7) ] }
         ; Entity { db_id = Some (Entity_id 11); attrs = [ "email", One_value (String "olga@example.com"); "score", One_value (Int 9) ] }
         ]
  in
  let query =
    { find = [ Find_var "name"; Find_var "score" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "name", QVar "name")
        ; Pattern (QVar "e", QAttr "email", QVar "email")
        ; SourcePattern ("scores", QVar "s", QAttr "email", QVar "email")
        ; SourcePattern ("scores", QVar "s", QAttr "score", QVar "score")
        ]
    }
  in
  assert_equal_query
    "q_sources joins facts across named database sources"
    [ [ Result_value (String "Ivan"); Result_value (Int 7) ] ]
    (q_sources db1 [ "scores", Db_source db2 ] query)

let test_q_with_relation_source () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg") ] }
         ]
  in
  let emails =
    [ [ Result_value (String "Ivan"); Result_value (String "ivan@example.com"); Result_value (String "primary") ]
    ; [ Result_value (String "Petr"); Result_value (String "petr@example.com"); Result_value (String "primary") ]
    ]
  in
  let query =
    { find = [ Find_var "e"; Find_var "email" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "name", QVar "name")
        ; SourcePattern ("emails", QVar "name", QVar "email", QWildcard)
        ]
    }
  in
  assert_equal_query
    "q_sources can join a database with a named relation source"
    [ [ Result_entity 1; Result_value (String "ivan@example.com") ]
    ; [ Result_entity 2; Result_value (String "petr@example.com") ]
    ]
    (q_sources db [ "emails", Relation_source emails ] query)

let test_q_with_relation_source_arbitrary_arity () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg") ] }
         ]
  in
  let emails =
    [ [ Result_value (String "Ivan"); Result_value (String "ivan@example.com") ]
    ; [ Result_value (String "Petr"); Result_value (String "petr@example.com") ]
    ]
  in
  let query =
    { find = [ Find_var "e"; Find_var "email" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "name", QVar "name")
        ; SourceRelationPattern ("emails", [ QVar "name"; QVar "email" ])
        ]
    }
  in
  assert_equal_query
    "q_sources can match arbitrary-arity relation source rows"
    [ [ Result_entity 1; Result_value (String "ivan@example.com") ]
    ; [ Result_entity 2; Result_value (String "petr@example.com") ]
    ]
    (q_sources db [ "emails", Relation_source emails ] query);
  assert_equal_query
    "parse_query treats source-like symbols inside source patterns as constants"
    [ [ Result_entity 1; Result_value (String "matched") ] ]
    (q_sources_string
       (empty_db ())
       [ "facts", Relation_source [ [ Result_entity 1; Result_value (Symbol "$src-sym"); Result_value (String "matched") ] ] ]
       "[:find ?e ?v
         :in $facts
         :where [$facts ?e $src-sym ?v]]")

let test_q_sources_default_source () =
  let db =
    empty_db ()
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] } ]
  in
  let query =
    { find = [ Find_var "name" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ SourcePattern ("$", QVar "e", QAttr "name", QVar "name") ]
    }
  in
  assert_equal_query
    "q_sources resolves explicit default source"
    [ [ Result_value (String "Ivan") ] ]
    (q_sources db [] query);
  let relation_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?x"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$"; QueryFormSymbol "?x" ]
      ]
  in
  assert_equal_query
    "q_sources allows relation sources to override the default source"
    [ [ Result_value (Int 1) ]; [ Result_value (Int 2) ] ]
    (q_sources
       (empty_db ())
       [ "$", Relation_source [ [ Result_value (Int 1) ]; [ Result_value (Int 2) ] ] ]
       (parse_query relation_query));
  let unqualified_relation_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?x"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?x" ]
      ]
  in
  assert_equal_query
    "parse_query matches unqualified one-column patterns against default relation sources"
    [ [ Result_value (Int 1) ]; [ Result_value (Int 2) ] ]
    (q_sources
       (empty_db ())
       [ "$", Relation_source [ [ Result_value (Int 1) ]; [ Result_value (Int 2) ] ] ]
       (parse_query unqualified_relation_query));
  let unqualified_three_column_relation_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormSymbol "?email"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?name"; QueryFormSymbol "?email"; QueryFormSymbol "?kind" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "="; QueryFormSymbol "?kind"; QueryFormKeyword "primary" ] ]
      ]
  in
  assert_equal_query
    "parse_query matches unqualified three-column patterns against default relation sources"
    [ [ Result_value (String "Ivan"); Result_value (String "ivan@example.com") ]
    ; [ Result_value (String "Petr"); Result_value (String "petr@example.com") ]
    ]
    (q_sources
       (empty_db ())
       [ ( "$"
         , Relation_source
             [ [ Result_value (String "Ivan"); Result_value (String "ivan@example.com"); Result_value (Keyword "primary") ]
             ; [ Result_value (String "Oleg"); Result_value (String "oleg@example.com"); Result_value (Keyword "secondary") ]
             ; [ Result_value (String "Petr"); Result_value (String "petr@example.com"); Result_value (Keyword "primary") ]
             ] )
       ]
       (parse_query unqualified_three_column_relation_query));
  let long_tuple_relation_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormSymbol "?a"
      ; QueryFormSymbol "?v"
      ; QueryFormSymbol "?t"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormSymbol "?e"
          ; QueryFormSymbol "?a"
          ; QueryFormSymbol "?v"
          ; QueryFormSymbol "?t"
          ; QueryFormKeyword "db/retract"
          ]
      ]
  in
  assert_equal_query
    "parse_query matches long relation tuples with constant trailing terms"
    [ [ Result_value (Int 1)
      ; Result_attr "age"
      ; Result_value (Int 39)
      ; Result_value (Int 999)
      ]
    ]
    (q_sources
       (empty_db ())
       [ ( "$"
         , Relation_source
             [ [ Result_value (Int 1)
               ; Result_attr "name"
               ; Result_value (String "Ivan")
               ; Result_value (Int 945)
               ; Result_value (Keyword "db/add")
               ]
             ; [ Result_value (Int 1)
               ; Result_attr "age"
               ; Result_value (Int 39)
               ; Result_value (Int 999)
               ; Result_value (Keyword "db/retract")
               ]
             ] )
       ]
       (parse_query long_tuple_relation_query));
  let long_tuple_prefix_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormSymbol "?v"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?v" ]
      ]
  in
  assert_equal_query
    "parse_query matches shorter patterns against long relation tuple prefixes"
    [ [ Result_value (Int 1); Result_value (String "Ivan") ] ]
    (q_sources
       (empty_db ())
       [ ( "$"
         , Relation_source
             [ [ Result_value (Int 1)
               ; Result_attr "name"
               ; Result_value (String "Ivan")
               ; Result_value (Int 945)
               ; Result_value (Keyword "db/add")
               ]
             ; [ Result_value (Int 1)
               ; Result_attr "age"
               ; Result_value (Int 39)
               ; Result_value (Int 999)
               ; Result_value (Keyword "db/retract")
               ]
             ] )
       ]
       (parse_query long_tuple_prefix_query));
  let override_db =
    empty_db ()
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] } ]
  in
  let tx_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormSymbol "?e"
          ; QueryFormKeyword "name"
          ; QueryFormSymbol "?name"
          ; QueryFormSymbol "?tx"
          ]
      ]
  in
  assert_equal_query
    "parse_query matches four-term patterns against overridden default db sources"
    [ [ Result_value (String "Ivan") ] ]
    (q_sources (empty_db ()) [ "$", Db_source override_db ] (parse_query tx_query));
  let lookup_db =
    override_db |> db_with [ Add (Entity_id 1, "height", Int 180) ]
  in
  let get_else_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?height"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "get-else"
              ; QueryFormSymbol "?e"
              ; QueryFormKeyword "height"
              ; QueryFormInt 300
              ]
          ; QueryFormSymbol "?height"
          ]
      ]
  in
  assert_equal_query
    "parse_query unqualified get-else uses the overridden default source"
    [ [ Result_value (Int 180) ] ]
    (q_sources (empty_db ()) [ "$", Db_source lookup_db ] (parse_query get_else_query));
  let get_some_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?attr"
      ; QueryFormSymbol "?value"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
      ; QueryFormVector
          [ QueryFormList
              [ QueryFormSymbol "get-some"
              ; QueryFormSymbol "?e"
              ; QueryFormKeyword "height"
              ; QueryFormKeyword "name"
              ]
          ; QueryFormVector [ QueryFormSymbol "?attr"; QueryFormSymbol "?value" ]
          ]
      ]
  in
  assert_equal_query
    "parse_query unqualified get-some uses the overridden default source"
    [ [ Result_attr "height"; Result_value (Int 180) ] ]
    (q_sources (empty_db ()) [ "$", Db_source lookup_db ] (parse_query get_some_query));
  let missing_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?e"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormString "Ivan" ]
      ; QueryFormVector [ QueryFormList [ QueryFormSymbol "missing?"; QueryFormSymbol "?e"; QueryFormKeyword "height" ] ]
      ]
  in
  assert_equal_query
    "parse_query unqualified missing? uses the overridden default source"
    []
    (q_sources (empty_db ()) [ "$", Db_source lookup_db ] (parse_query missing_query))

let test_parse_query_infers_default_source_input () =
  let db =
    empty_db ()
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] } ]
  in
  let ordinary_query_form =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ]
  in
  let ordinary_parsed = parse_query ordinary_query_form in
  if ordinary_parsed.inputs <> [ Input_source_decl "$" ] then
    failwith "parse_query should infer default $ input when where uses ordinary data patterns";
  assert_equal_query
    "ordinary parsed query should use inferred default source"
    [ [ Result_value (String "Ivan") ] ]
    (q db ordinary_parsed);
  let query_form =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?name"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "$"; QueryFormSymbol "?e"; QueryFormKeyword "name"; QueryFormSymbol "?name" ]
      ]
  in
  let parsed = parse_query query_form in
  if parsed.inputs <> [ Input_source_decl "$" ] then
    failwith "parse_query should infer default $ input when where uses explicit default source";
  assert_equal_query
    "parsed query should use inferred default source"
    [ [ Result_value (String "Ivan") ] ]
    (q db parsed)

let test_q_sources_unknown_source_rejected () =
  let db = empty_db () in
  let query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ SourcePattern ("missing", QVar "e", QAttr "name", QValue (String "Ivan")) ]
    }
  in
  assert_raises_invalid_arg
    "q_sources rejects unknown named source"
    (fun () -> ignore (q_sources db [] query))

let test_q_sources_lookup_ref_uses_named_source () =
  let db = empty_db () in
  let source =
    empty_db ~schema:[ "email", unique_identity ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 10); attrs = [ "email", One_value (String "ivan@example.com"); "score", One_value (Int 7) ] }
         ]
  in
  let query =
    { find = [ Find_var "score" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ SourcePattern ("scores", QLookupRef ("email", String "ivan@example.com"), QAttr "score", QVar "score") ]
    }
  in
  assert_equal_query
    "q_sources resolves lookup refs against the named source"
    [ [ Result_value (Int 7) ] ]
    (q_sources db [ "scores", Db_source source ] query)

let test_q_resolves_idents_in_patterns () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "db/ident", Keyword "ent1")
         ; Add (Entity_id 2, "db/ident", Keyword "ent2")
         ; Add (Ident "ent2", "ref", Ref_to (Ident "ent1"))
         ]
  in
  assert_equal_query
    "query resolves idents in entity position"
    [ [ Result_entity 1 ] ]
    (q
       db
       { find = [ Find_var "v" ]
       ; inputs = []
       ; with_vars = []
       ; rules = []
       ; where = [ Pattern (QIdent "ent2", QAttr "ref", QVar "v") ]
       });
  assert_equal_query
    "query resolves idents in ref value position"
    [ [ Result_entity 2 ] ]
    (q
       db
       { find = [ Find_var "f" ]
       ; inputs = []
       ; with_vars = []
       ; rules = []
       ; where = [ Pattern (QVar "f", QAttr "ref", QIdent "ent1") ]
       })

let test_parse_query_resolves_idents_in_patterns () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "db/ident", Keyword "ent1")
         ; Add (Entity_id 2, "db/ident", Keyword "ent2")
         ; Add (Ident "ent2", "ref", Ref_to (Ident "ent1"))
         ]
  in
  let entity_ident_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?v"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormKeyword "ent2"; QueryFormKeyword "ref"; QueryFormSymbol "?v" ]
      ]
  in
  assert_equal_query
    "parse_query resolves keyword idents in entity position"
    [ [ Result_entity 1 ] ]
    (q db (parse_query entity_ident_query));
  let ref_value_ident_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?f"
      ; QueryFormKeyword "where"
      ; QueryFormVector [ QueryFormSymbol "?f"; QueryFormKeyword "ref"; QueryFormKeyword "ent1" ]
      ]
  in
  assert_equal_query
    "parse_query resolves keyword idents in ref value position"
    [ [ Result_entity 2 ] ]
    (q db (parse_query ref_value_ident_query));
  let source_ident_query =
    QueryFormVector
      [ QueryFormKeyword "find"
      ; QueryFormSymbol "?v"
      ; QueryFormKeyword "in"
      ; QueryFormSymbol "$"
      ; QueryFormSymbol "$idents"
      ; QueryFormKeyword "where"
      ; QueryFormVector
          [ QueryFormSymbol "$idents"
          ; QueryFormKeyword "ent2"
          ; QueryFormKeyword "ref"
          ; QueryFormSymbol "?v"
          ]
      ]
  in
  assert_equal_query
    "parse_query resolves source-qualified keyword idents"
    [ [ Result_entity 1 ] ]
    (q_sources (empty_db ()) [ "idents", Db_source db ] (parse_query source_ident_query))

let test_q_find_pull_expressions () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ]
  in
  let query =
    { find = [ Find_pull ("e", [ Pull_attr "name" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "name", QValue (String "Ivan")) ]
    }
  in
  assert_equal_query
    "q supports pull expressions in find"
    [ [ Result_pull { pulled_id = 1; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Ivan") ] } ] ]
    (q db query)

let test_q_return_shapes_with_pull_expressions () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 44) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 25) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg"); "age", One_value (Int 11) ] }
         ]
  in
  let pulled_ivan =
    Result_pull { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Ivan") ] }
  in
  let scalar_query =
    { find = [ Find_pull ("e", [ Pull_attr "name" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "age", QValue (Int 25)) ]
    }
  in
  if q_return db Return_scalar scalar_query <> Query_scalar (Some pulled_ivan) then
    failwith "q_return scalar should support pull expressions";
  let collection_query =
    { scalar_query with where = [ Pattern (QVar "e", QAttr "age", QVar "age") ] }
  in
  if
    q_return db Return_collection collection_query
    <> Query_collection
         [ Result_pull { pulled_id = 1; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ] }
         ; pulled_ivan
         ; Result_pull { pulled_id = 3; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Oleg") ] }
         ]
  then failwith "q_return collection should support pull expressions";
  let tuple_query =
    { find = [ Find_var "e"; Find_pull ("e", [ Pull_attr "name" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "age", QValue (Int 25)) ]
    }
  in
  if q_return db Return_tuple tuple_query <> Query_tuple (Some [ Result_entity 2; pulled_ivan ]) then
    failwith "q_return tuple should support pull expressions"

let test_q_find_pull_uses_named_source () =
  let db =
    empty_db ()
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] } ]
  in
  let source =
    empty_db ()
    |> db_with [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Petr") ] } ]
  in
  let query =
    { find = [ Find_var "e"; Find_pull_source ("people", "e", [ Pull_attr "name" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ SourcePattern ("people", QVar "e", QAttr "name", QValue (String "Petr")) ]
    }
  in
  assert_equal_query
    "q pull find expressions can read from a named source"
    [ [ Result_entity 1
      ; Result_pull { pulled_id = 1; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ] }
      ]
    ]
    (q_sources db [ "people", Db_source source ] query)

let test_q_with_aggregates () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 3) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "color", One_value (String "blue"); "heads", One_value (Int 2) ] }
         ]
  in
  let query =
    { find =
        [ Find_var "color"
        ; Find_aggregate (Sum, [ QVar "heads" ])
        ; Find_aggregate (Min, [ QVar "heads" ])
        ; Find_aggregate (Max, [ QVar "heads" ])
        ; Find_aggregate (Count, [ QVar "heads" ])
        ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "color", QVar "color")
        ; Pattern (QVar "e", QAttr "heads", QVar "heads")
        ]
    }
  in
  assert_equal_query
    "q aggregates group by non-aggregate find vars"
    [ [ Result_value (String "blue"); Result_value (Int 2); Result_value (Int 2); Result_value (Int 2); Result_value (Int 1) ]
    ; [ Result_value (String "red"); Result_value (Int 4); Result_value (Int 1); Result_value (Int 3); Result_value (Int 2) ]
    ]
    (q db query)

let test_q_aggregates_with_pull_expressions () =
  let db =
    empty_db ~schema:[ "value", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "value", Many_values [ Int 10; Int 20; Int 30; Int 40 ]
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "value", Many_values [ Int 14; Int 16 ]
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 3)
             ; attrs = [ "name", One_value (String "Oleg"); "value", One_value (Int 1) ]
             }
         ]
  in
  let query =
    { find =
        [ Find_var "e"
        ; Find_pull ("e", [ Pull_attr "name" ])
        ; Find_aggregate (Min, [ QVar "v" ])
        ; Find_aggregate (Max, [ QVar "v" ])
        ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "value", QVar "v") ]
    }
  in
  assert_equal_query
    "q aggregates can be combined with pull expressions"
    [ [ Result_entity 1
      ; Result_pull { pulled_id = 1; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ] }
      ; Result_value (Int 10)
      ; Result_value (Int 40)
      ]
    ; [ Result_entity 2
      ; Result_pull { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Ivan") ] }
      ; Result_value (Int 14)
      ; Result_value (Int 16)
      ]
    ; [ Result_entity 3
      ; Result_pull { pulled_id = 3; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Oleg") ] }
      ; Result_value (Int 1)
      ; Result_value (Int 1)
      ]
    ]
    (q db query)

let test_q_with_interleaved_aggregates () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 3) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "color", One_value (String "blue"); "heads", One_value (Int 2) ] }
         ]
  in
  let query =
    { find = [ Find_aggregate (Count, [ QVar "heads" ]); Find_var "color"; Find_aggregate (Sum, [ QVar "heads" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "color", QVar "color")
        ; Pattern (QVar "e", QAttr "heads", QVar "heads")
        ]
    }
  in
  assert_equal_query
    "q preserves interleaved aggregate find order"
    [ [ Result_value (Int 1); Result_value (String "blue"); Result_value (Int 2) ]
    ; [ Result_value (Int 2); Result_value (String "red"); Result_value (Int 4) ]
    ]
    (q db query)

let test_q_aggregates_relation_inputs_with_with_vars () =
  let monsters =
    [ [ Result_value (String "Cerberus"); Result_value (Int 3) ]
    ; [ Result_value (String "Medusa"); Result_value (Int 1) ]
    ; [ Result_value (String "Cyclops"); Result_value (Int 1) ]
    ; [ Result_value (String "Chimera"); Result_value (Int 1) ]
    ]
  in
  let relation_input = Input_relation ([ "monster"; "heads" ], monsters) in
  let sum_query =
    { find = [ Find_aggregate (Sum, [ QVar "heads" ]) ]
    ; inputs = [ relation_input ]
    ; with_vars = []
    ; rules = []
    ; where = []
    }
  in
  assert_equal_query
    "q aggregate relation inputs deduplicate values without with vars"
    [ [ Result_value (Int 4) ] ]
    (q_with (empty_db ()) [] sum_query);
  let multi_aggregate_query =
    { find =
        [ Find_aggregate (Sum, [ QVar "heads" ])
        ; Find_aggregate (Min, [ QVar "heads" ])
        ; Find_aggregate (Max, [ QVar "heads" ])
        ; Find_aggregate (Count, [ QVar "heads" ])
        ; Find_aggregate (CountDistinct, [ QVar "heads" ])
        ]
    ; inputs = [ relation_input ]
    ; with_vars = []
    ; rules = []
    ; where = []
    }
  in
  assert_equal_query
    "q aggregate relation inputs preserve with-var-distinguished rows"
    [ [ Result_value (Int 6)
      ; Result_value (Int 1)
      ; Result_value (Int 3)
      ; Result_value (Int 4)
      ; Result_value (Int 2)
      ]
    ]
    (q_with (empty_db ()) [ "monster" ] multi_aggregate_query)

let test_q_with_preserves_non_aggregate_duplicates () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "monster", One_value (String "Medusa"); "heads", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "monster", One_value (String "Cyclops"); "heads", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "monster", One_value (String "Chimera"); "heads", One_value (Int 1) ] }
         ]
  in
  let query =
    { find = [ Find_var "heads" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "monster", QVar "monster")
        ; Pattern (QVar "e", QAttr "heads", QVar "heads")
        ]
    }
  in
  assert_equal_query
    "q without with vars deduplicates non-aggregate rows"
    [ [ Result_value (Int 1) ] ]
    (q_with db [] query);
  assert_equal_query
    "q_with preserves non-aggregate duplicates distinguished by with vars"
    [ [ Result_value (Int 1) ]; [ Result_value (Int 1) ]; [ Result_value (Int 1) ] ]
    (q_with db [ "monster" ] query)

let test_q_count_distinct_aggregate () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 3) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 3) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 1) ] }
         ]
  in
  let query =
    { find = [ Find_var "color"; Find_aggregate (Count, [ QVar "heads" ]); Find_aggregate (CountDistinct, [ QVar "heads" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "color", QVar "color")
        ; Pattern (QVar "e", QAttr "heads", QVar "heads")
        ]
    }
  in
  assert_equal_query
    "q count-distinct aggregates unique values within each group"
    [ [ Result_value (String "red"); Result_value (Int 3); Result_value (Int 2) ] ]
    (q db query)

let test_q_distinct_aggregate () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 3) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 3) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 1) ] }
         ]
  in
  let query =
    { find = [ Find_var "color"; Find_aggregate (Distinct, [ QVar "heads" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "color", QVar "color")
        ; Pattern (QVar "e", QAttr "heads", QVar "heads")
        ]
    }
  in
  assert_equal_query
    "q distinct aggregate returns unique values as a set"
    [ [ Result_value (String "red"); Result_value (Set [ Int 1; Int 3 ]) ] ]
    (q db query)

let test_q_min_max_use_keyword_comparator () =
  let min_max_query =
    { find = [ Find_aggregate (Min, [ QVar "x" ]); Find_aggregate (Max, [ QVar "x" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ GroundCollection ([ Keyword "a-/b"; Keyword "a/b" ], "x") ]
    }
  in
  assert_equal_query
    "q min and max compare keywords by namespace then name"
    [ [ Result_value (Keyword "a/b"); Result_value (Keyword "a-/b") ] ]
    (q (empty_db ()) min_max_query);
  let min_max_n_query =
    { find = [ Find_aggregate (MinN 2, [ QVar "x" ]); Find_aggregate (MaxN 2, [ QVar "x" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ GroundCollection ([ Keyword "a/b"; Keyword "a-/b"; Keyword "a/c" ], "x") ]
    }
  in
  assert_equal_query
    "q minN and maxN compare keywords by namespace then name"
    [ [ Result_value (Tuple [ Some (Keyword "a/b"); Some (Keyword "a/c") ])
      ; Result_value (Tuple [ Some (Keyword "a/c"); Some (Keyword "a-/b") ])
      ]
    ]
    (q (empty_db ()) min_max_n_query)

let test_q_with_vars_preserve_aggregate_duplicates () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 3) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 3) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 1) ] }
         ]
  in
  let query =
    { find = [ Find_var "color"; Find_aggregate (Count, [ QVar "heads" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "color", QVar "color")
        ; Pattern (QVar "e", QAttr "heads", QVar "heads")
        ]
    }
  in
  assert_equal_query
    "q_with without with vars deduplicates aggregate input tuples"
    [ [ Result_value (String "red"); Result_value (Int 2) ] ]
    (q_with db [] query);
  assert_equal_query
    "q_with preserves duplicates distinguished by with vars"
    [ [ Result_value (String "red"); Result_value (Int 3) ] ]
    (q_with db [ "e" ] query)

let test_q_avg_aggregate () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 2) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "color", One_value (String "red"); "heads", One_value (Int 3) ] }
         ]
  in
  let query =
    { find = [ Find_var "color"; Find_aggregate (Avg, [ QVar "heads" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "color", QVar "color")
        ; Pattern (QVar "e", QAttr "heads", QVar "heads")
        ]
    }
  in
  assert_equal_query
    "q avg aggregates numeric values"
    [ [ Result_value (String "red"); Result_value (Float 2.0) ] ]
    (q db query)

let test_q_sum_aggregate_accepts_float_values () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "amount", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "amount", One_value (Float 2.5) ] }
         ]
  in
  let query =
    { find = [ Find_aggregate (Sum, [ QVar "amount" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "amount", QVar "amount") ]
    }
  in
  assert_equal_query
    "q sum accepts mixed numeric values"
    [ [ Result_value (Float 3.5) ] ]
    (q db query)

let test_q_statistical_aggregates () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "sample", One_value (Int 10) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "sample", One_value (Int 15) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "sample", One_value (Int 20) ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "sample", One_value (Int 35) ] }
         ; Entity { db_id = Some (Entity_id 5); attrs = [ "sample", One_value (Int 75) ] }
         ]
  in
  let query =
    { find =
        [ Find_aggregate (Median, [ QVar "sample" ])
        ; Find_aggregate (Variance, [ QVar "sample" ])
        ; Find_aggregate (Stddev, [ QVar "sample" ])
        ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "sample", QVar "sample") ]
    }
  in
  assert_equal_query
    "q supports median, variance, and stddev aggregates"
    [ [ Result_value (Float 20.0)
      ; Result_value (Float 554.0)
      ; Result_value (Float 23.53720459187964)
      ]
    ]
    (q db query)

let test_q_min_n_and_max_n_aggregates () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "color", One_value (String "red"); "amount", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "color", One_value (String "red"); "amount", One_value (Int 2) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "color", One_value (String "red"); "amount", One_value (Int 3) ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "color", One_value (String "red"); "amount", One_value (Int 4) ] }
         ; Entity { db_id = Some (Entity_id 5); attrs = [ "color", One_value (String "blue"); "amount", One_value (Int 7) ] }
         ; Entity { db_id = Some (Entity_id 6); attrs = [ "color", One_value (String "blue"); "amount", One_value (Int 8) ] }
         ]
  in
  let query =
    { find = [ Find_var "color"; Find_aggregate (MaxN 3, [ QVar "amount" ]); Find_aggregate (MinN 3, [ QVar "amount" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "color", QVar "color")
        ; Pattern (QVar "e", QAttr "amount", QVar "amount")
        ]
    }
  in
  assert_equal_query
    "q supports min n and max n aggregates"
    [ [ Result_value (String "blue")
      ; Result_value (Tuple [ Some (Int 7); Some (Int 8) ])
      ; Result_value (Tuple [ Some (Int 7); Some (Int 8) ])
      ]
    ; [ Result_value (String "red")
      ; Result_value (Tuple [ Some (Int 2); Some (Int 3); Some (Int 4) ])
      ; Result_value (Tuple [ Some (Int 1); Some (Int 2); Some (Int 3) ])
      ]
    ]
    (q db query)

let test_q_rand_and_sample_aggregates () =
  let values = [ Int 1; Int 2; Int 3 ] in
  let member value = List.mem value values in
  let query =
    { find =
        [ Find_aggregate (Rand, [ QVar "x" ])
        ; Find_aggregate (RandN 5, [ QVar "x" ])
        ; Find_aggregate (Sample 2, [ QVar "x" ])
        ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ GroundCollection (values, "x") ]
    }
  in
  match q (empty_db ()) query with
  | [ [ Result_value rand_value; Result_value (Tuple rand_values); Result_value (Tuple sample_values) ] ] ->
    if not (member rand_value) then
      failwith "rand aggregate should return a value from the input";
    let rand_values = List.map Option.get rand_values in
    if List.length rand_values <> 5 || not (List.for_all member rand_values) then
      failwith "rand n aggregate should return n values from the input";
    let sample_values = List.map Option.get sample_values in
    if List.length sample_values <> 2 then
      failwith "sample aggregate should return the requested number of values";
    if List.length (List.sort_uniq compare sample_values) <> 2 then
      failwith "sample aggregate should not repeat values";
    if not (List.for_all member sample_values) then
      failwith "sample aggregate should return values from the input"
  | _ -> failwith "unexpected rand/sample aggregate result"

let test_q_custom_aggregates () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "color", One_value (String "red"); "amount", One_value (Int 1) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "color", One_value (String "red"); "amount", One_value (Int 2) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "color", One_value (String "red"); "amount", One_value (Int 3) ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "color", One_value (String "blue"); "amount", One_value (Int 5) ] }
         ]
  in
  let reverse_tuple values =
    values
    |> List.sort compare
    |> List.rev
    |> List.map (function
      | Result_value value -> Some value
      | _ -> invalid_arg "expected values")
    |> fun values -> Result_value (Tuple values)
  in
  let query =
    { find = [ Find_var "color"; Find_aggregate (Custom reverse_tuple, [ QVar "amount" ]) ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "color", QVar "color")
        ; Pattern (QVar "e", QAttr "amount", QVar "amount")
        ]
    }
  in
  assert_equal_query
    "q supports custom aggregate functions"
    [ [ Result_value (String "blue"); Result_value (Tuple [ Some (Int 5) ]) ]
    ; [ Result_value (String "red"); Result_value (Tuple [ Some (Int 3); Some (Int 2); Some (Int 1) ]) ]
    ]
    (q db query);
  assert_equal_query
    "q_string parses custom aggregate inputs"
    [ [ Result_value (String "blue"); Result_value (Tuple [ Some (Int 5) ]) ]
    ; [ Result_value (String "red"); Result_value (Tuple [ Some (Int 3); Some (Int 2); Some (Int 1) ]) ]
    ]
    (q_string
       ~inputs:[ Arg_aggregate reverse_tuple ]
       db
       "[:find ?color (aggregate ?agg ?amount)
         :in $ ?agg
         :where [?e :color ?color]
                [?e :amount ?amount]]");
  if
    q_return_string
      ~inputs:[ Arg_aggregate reverse_tuple ]
      db
      "[:find (aggregate ?agg ?amount) .
        :in $ ?agg
        :where [?e :amount ?amount]]"
    <> Query_scalar (Some (Result_value (Tuple [ Some (Int 5); Some (Int 3); Some (Int 2); Some (Int 1) ])))
  then failwith "q_return_string should parse scalar custom aggregate inputs";
  let scaled_sum = function
    | Result_value (Int factor) :: values ->
      values
      |> List.fold_left
           (fun total -> function
             | Result_value (Int value) -> total + value
             | _ -> invalid_arg "expected integer aggregate values")
           0
      |> fun total -> Result_value (Int (factor * total))
    | _ -> invalid_arg "expected integer aggregate factor"
  in
  assert_equal_query
    "q_string passes extra custom aggregate arguments before grouped values"
    [ [ Result_value (String "blue"); Result_value (Int 50) ]
    ; [ Result_value (String "red"); Result_value (Int 60) ]
    ]
    (q_string
       ~inputs:[ Arg_aggregate scaled_sum ]
       db
       "[:find ?color (aggregate ?agg 10 ?amount)
         :in $ ?agg
         :where [?e :color ?color]
                [?e :amount ?amount]]");
  if
    q_return_string
      ~inputs:[ Arg_aggregate scaled_sum; Arg_scalar (Result_value (Int 2)) ]
      db
      "[:find (aggregate ?agg ?factor ?amount) .
        :in $ ?agg ?factor
        :where [?e :amount ?amount]]"
    <> Query_scalar (Some (Result_value (Int 22)))
  then failwith "q_return_string should pass variable aggregate arguments"

let test_q_rejects_unknown_rules () =
  assert_raises_invalid_arg
    "q rejects unknown rule invocations"
    (fun () ->
      ignore
        (q
           (empty_db ())
           { find = [ Find_var "e" ]
           ; inputs = []
           ; with_vars = []
           ; rules = []
           ; where = [ Rule ("missing", [ QVar "e" ]) ]
           }))

let test_q_rules_accept_false_arguments () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "attr", Bool true)
         ; Add (Entity_id 2, "attr", Bool false)
         ]
  in
  let rules =
    [ { rule_name = "is"
      ; rule_params = [ "id"; "value" ]
      ; rule_body = [ Pattern (QVar "id", QAttr "attr", QVar "value") ]
      }
    ]
  in
  let query value =
    { find = [ Find_var "id" ]
    ; inputs = []
    ; with_vars = []
    ; rules
    ; where = [ Rule ("is", [ QVar "id"; QValue value ]) ]
    }
  in
  assert_equal_query
    "q rules accept true literal arguments"
    [ [ Result_entity 1 ] ]
    (q db (query (Bool true)));
  assert_equal_query
    "q rules accept false literal arguments"
    [ [ Result_entity 2 ] ]
    (q db (query (Bool false)))

let test_q_with_rules () =
  let db =
    empty_db ~schema:[ "follow", many ] ()
    |> db_with
         [ Add (Entity_id 1, "follow", Ref 2)
         ; Add (Entity_id 2, "follow", Ref 3)
         ; Add (Entity_id 2, "follow", Ref 4)
         ]
  in
  let query =
    { find = [ Find_var "from"; Find_var "to" ]
    ; inputs = []
    ; with_vars = []
    ; rules =
        [ { rule_name = "follows"
          ; rule_params = [ "from"; "to" ]
          ; rule_body = [ Pattern (QVar "from", QAttr "follow", QVar "to") ]
          }
        ]
    ; where = [ Rule ("follows", [ QVar "from"; QVar "to" ]) ]
    }
  in
  assert_equal_query
    "q evaluates simple rules"
    [ [ Result_entity 1; Result_entity 2 ]
    ; [ Result_entity 2; Result_entity 3 ]
    ; [ Result_entity 2; Result_entity 4 ]
    ]
    (q db query)

let test_q_rule_context_is_isolated_from_outer_context () =
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
  let query =
    { find = [ Find_var "x" ]
    ; inputs = []
    ; with_vars = []
    ; rules =
        [ { rule_name = "rule"
          ; rule_params = [ "e" ]
          ; rule_body = [ Pattern (QWildcard, QVar "e", QWildcard) ]
          }
        ]
    ; where =
        [ Pattern (QVar "e", QWildcard, QWildcard)
        ; Rule ("rule", [ QVar "x" ])
        ]
    }
  in
  assert_equal_query
    "q rule context is isolated from outer bindings"
    [ [ Result_attr "follow" ] ]
    (q db query)

let test_q_regular_clauses_join_with_rules () =
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
  let even_entity = function
    | [ Result_entity entity_id ] -> entity_id mod 2 = 0
    | _ -> false
  in
  let query =
    { find = [ Find_var "y"; Find_var "x" ]
    ; inputs = []
    ; with_vars = []
    ; rules =
        [ { rule_name = "rule"
          ; rule_params = [ "a"; "b" ]
          ; rule_body = [ Pattern (QVar "a", QAttr "follow", QVar "b") ]
          }
        ]
    ; where =
        [ Pattern (QWildcard, QWildcard, QVar "x")
        ; Rule ("rule", [ QVar "x"; QVar "y" ])
        ; Predicate ("even?", [ QVar "x" ], even_entity)
        ]
    }
  in
  assert_equal_query_set
    "q joins regular clauses with rule invocations"
    [ [ Result_entity 3; Result_entity 2 ]
    ; [ Result_entity 6; Result_entity 4 ]
    ; [ Result_entity 4; Result_entity 2 ]
    ]
    (q db query)

let test_q_rule_branches_match_upstream () =
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
  let query =
    { find = [ Find_var "to_entity" ]
    ; inputs = [ Input_scalar_decl "from_entity" ]
    ; with_vars = []
    ; rules =
        [ { rule_name = "follow"
          ; rule_params = [ "from"; "to_entity" ]
          ; rule_body = [ Pattern (QVar "from", QAttr "follow", QVar "to_entity") ]
          }
        ; { rule_name = "follow"
          ; rule_params = [ "from"; "to_entity" ]
          ; rule_body =
              [ Pattern (QVar "from", QAttr "follow", QVar "via")
              ; Pattern (QVar "via", QAttr "follow", QVar "to_entity")
              ]
          }
        ]
    ; where = [ Rule ("follow", [ QVar "from_entity"; QVar "to_entity" ]) ]
    }
  in
  assert_equal_query_set
    "q rule branches union direct and branch-local results"
    [ [ Result_entity 2 ]; [ Result_entity 3 ]; [ Result_entity 4 ] ]
    (q ~inputs:[ Arg_scalar (Result_entity 1) ] db query)

let test_q_can_call_same_dynamic_predicate_rule_twice () =
  let db =
    empty_db ()
    |> db_with [ Add (Entity_id 1, "attr", String "a") ]
  in
  let always_true _ = true in
  assert_equal_query
    "q can call the same dynamic predicate rule twice"
    []
    (q_string
       ~inputs:[ Arg_predicate always_true ]
       db
       "{:find [?p]
         :in [$ % ?fn]
         :where [(rule ?p ?fn \"a\")
                 (rule ?p ?fn \"b\")]
         :rules [[(rule ?p ?fn ?x)
                  [?p :attr ?x]
                  [(?fn ?x)]]]}")

let test_q_with_recursive_rules () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "parent", Ref 2)
         ; Add (Entity_id 2, "parent", Ref 3)
         ; Add (Entity_id 3, "parent", Ref 4)
         ]
  in
  let query =
    { find = [ Find_var "ancestor" ]
    ; inputs = []
    ; with_vars = []
    ; rules =
        [ { rule_name = "ancestor"
          ; rule_params = [ "descendant"; "ancestor" ]
          ; rule_body = [ Pattern (QVar "descendant", QAttr "parent", QVar "ancestor") ]
          }
        ; { rule_name = "ancestor"
          ; rule_params = [ "descendant"; "ancestor" ]
          ; rule_body =
              [ Pattern (QVar "descendant", QAttr "parent", QVar "parent")
              ; Rule ("ancestor", [ QVar "parent"; QVar "ancestor" ])
              ]
          }
        ]
    ; where = [ Rule ("ancestor", [ QEntity 1; QVar "ancestor" ]) ]
    }
  in
  assert_equal_query
    "q evaluates recursive rules"
    [ [ Result_entity 2 ]; [ Result_entity 3 ]; [ Result_entity 4 ] ]
    (q db query)

let test_q_with_symmetric_recursive_rules () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "follow", Ref 2)
         ; Add (Entity_id 2, "follow", Ref 3)
         ]
  in
  let query =
    { find = [ Find_var "from"; Find_var "to" ]
    ; inputs = []
    ; with_vars = []
    ; rules =
        [ { rule_name = "follow"
          ; rule_params = [ "from"; "to" ]
          ; rule_body = [ Pattern (QVar "from", QAttr "follow", QVar "to") ]
          }
        ; { rule_name = "follow"
          ; rule_params = [ "from"; "to" ]
          ; rule_body = [ Rule ("follow", [ QVar "to"; QVar "from" ]) ]
          }
        ]
    ; where = [ Rule ("follow", [ QVar "from"; QVar "to" ]) ]
    }
  in
  assert_equal_query
    "q evaluates symmetric recursive rules without looping"
    [ [ Result_entity 1; Result_entity 2 ]
    ; [ Result_entity 2; Result_entity 1 ]
    ; [ Result_entity 2; Result_entity 3 ]
    ; [ Result_entity 3; Result_entity 2 ]
    ]
    (q db query)

let test_q_with_mutually_recursive_rules () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 0, "f1", Ref 1)
         ; Add (Entity_id 1, "f2", Ref 2)
         ; Add (Entity_id 2, "f1", Ref 3)
         ; Add (Entity_id 3, "f2", Ref 4)
         ; Add (Entity_id 4, "f1", Ref 5)
         ; Add (Entity_id 5, "f2", Ref 6)
         ]
  in
  let query =
    { find = [ Find_var "from"; Find_var "to" ]
    ; inputs = []
    ; with_vars = []
    ; rules =
        [ { rule_name = "f1"
          ; rule_params = [ "from"; "to" ]
          ; rule_body = [ Pattern (QVar "from", QAttr "f1", QVar "to") ]
          }
        ; { rule_name = "f1"
          ; rule_params = [ "from"; "to" ]
          ; rule_body =
              [ Pattern (QVar "t", QAttr "f1", QVar "to")
              ; Rule ("f2", [ QVar "from"; QVar "t" ])
              ]
          }
        ; { rule_name = "f2"
          ; rule_params = [ "from"; "to" ]
          ; rule_body = [ Pattern (QVar "from", QAttr "f2", QVar "to") ]
          }
        ; { rule_name = "f2"
          ; rule_params = [ "from"; "to" ]
          ; rule_body =
              [ Pattern (QVar "t", QAttr "f2", QVar "to")
              ; Rule ("f1", [ QVar "from"; QVar "t" ])
              ]
          }
        ]
    ; where = [ Rule ("f1", [ QVar "from"; QVar "to" ]) ]
    }
  in
  assert_equal_query
    "q evaluates mutually recursive rules without looping"
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
    (q db query)

let test_q_source_qualified_rules () =
  let sexes =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "sex", One_value (Keyword "male") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Darya"); "sex", One_value (Keyword "female") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg"); "sex", One_value (Keyword "male") ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "name", One_value (String "Igor"); "sex", One_value (Keyword "male") ] }
         ]
  in
  let ages =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 10); attrs = [ "name", One_value (String "Ivan"); "age", One_value (Int 15) ] }
         ; Entity { db_id = Some (Entity_id 11); attrs = [ "name", One_value (String "Oleg"); "age", One_value (Int 66) ] }
         ; Entity { db_id = Some (Entity_id 12); attrs = [ "name", One_value (String "Darya"); "age", One_value (Int 32) ] }
         ]
  in
  let adult = function
    | [ Result_value (Int age) ] -> age >= 18
    | _ -> false
  in
  let query =
    { find = [ Find_var "name" ]
    ; inputs = []
    ; with_vars = []
    ; rules =
        [ { rule_name = "male"
          ; rule_params = [ "name" ]
          ; rule_body =
              [ Pattern (QVar "person", QAttr "name", QVar "name")
              ; Pattern (QVar "person", QAttr "sex", QValue (Keyword "male"))
              ]
          }
        ; { rule_name = "adult"
          ; rule_params = [ "name" ]
          ; rule_body =
              [ Pattern (QVar "person", QAttr "name", QVar "name")
              ; Pattern (QVar "person", QAttr "age", QVar "age")
              ; Predicate ("adult?", [ QVar "age" ], adult)
              ]
          }
        ]
    ; where =
        [ SourceRule ("sexes", "male", [ QVar "name" ])
        ; SourceRule ("ages", "adult", [ QVar "name" ])
        ]
    }
  in
  assert_equal_query
    "q evaluates source-qualified rules against the invocation source"
    [ [ Result_value (String "Oleg") ] ]
    (q_sources (empty_db ()) [ "sexes", Db_source sexes; "ages", Db_source ages ] query)

let test_query_fns__test_query_fns () =
  test_q_predicates_without_free_variables_filter_all_rows ();
  test_q_builtin_get_else_get_some_and_missing ();
  test_q_builtin_get_map_values ();
  test_q_builtin_count_values ();
  test_q_builtin_comparison_predicates ();
  test_q_builtin_variadic_comparison_predicates ();
  test_q_builtin_vector_values ();
  test_q_builtin_hash_map_values ();
  test_q_with_dynamic_callable_inputs ();
  test_q_functions_bind_derived_values ();
  test_q_function_binding_conflicts_filter_rows ();
  test_q_function_bindings_interact_with_rules ();
  test_q_parsed_rule_inputs_interact_with_function_bindings ();
  test_q_functions_filter_on_none ();
  test_q_builtin_ground_bindings ()

let test_query_fns__test_predicates () =
  test_q_predicates_filter_bound_values ();
  test_q_builtin_value_type_predicates ();
  test_q_builtin_numeric_predicates ();
  test_q_builtin_comparison_predicates ();
  test_q_builtin_variadic_comparison_predicates ();
  test_q_builtin_equality_predicates ();
  test_q_builtin_boolean_predicates ();
  test_q_builtin_differ_and_identical_predicates ()

let test_query_fns__test_symbol_resolution () =
  assert_equal_query
    "query_fns.cljc test-symbol-resolution resolves a callable query function"
    [ [ Result_value (Int 42) ] ]
    (q_string
       ~inputs:[ Arg_function (fun _ -> Some [ Result_value (Int 42) ]) ]
       (empty_db ())
       "[:find ?x
         :in ?f
         :where [(?f) ?x]]")

let test_query_aggregates__test_aggregates () =
  test_q_with_aggregates ();
  test_q_aggregates_with_pull_expressions ();
  test_q_with_interleaved_aggregates ();
  test_q_aggregates_relation_inputs_with_with_vars ();
  test_q_with_preserves_non_aggregate_duplicates ();
  test_q_count_distinct_aggregate ();
  test_q_distinct_aggregate ();
  test_q_min_max_use_keyword_comparator ();
  test_q_with_vars_preserve_aggregate_duplicates ();
  test_q_avg_aggregate ();
  test_q_sum_aggregate_accepts_float_values ();
  test_q_statistical_aggregates ();
  test_q_min_n_and_max_n_aggregates ();
  test_q_rand_and_sample_aggregates ();
  test_q_custom_aggregates ()

let test_query_not__test_not () =
  test_q_not_filters_matching_bindings ();
  test_q_not_matches_upstream_edge_cases ()

let test_query_not__test_not_join () =
  test_q_not_join_projects_join_variables ();
  test_q_not_join_rejects_unbound_join_vars ()

let test_query_not__test_default_source () =
  test_q_source_qualified_composite_clauses ();
  test_q_not_or_upstream_source_and_relation_batch ()

let test_query_not__test_impl_edge_cases () =
  test_q_not_matches_upstream_edge_cases ()

let test_query_not__test_insufficient_bindings () =
  test_q_not_insufficient_bindings_match_upstream_messages ();
  test_q_not_rejects_clauses_without_outer_bindings ()

let test_query_or__test_or () =
  test_q_or_unions_branch_results ();
  test_q_or_allows_branch_vars_bound_by_outer_clauses ();
  test_q_or_join_required_vars_use_outer_bindings ()

let test_query_or__test_or_join () =
  test_q_or_join_projects_join_variables ();
  test_q_or_join_binds_listed_branch_variables ();
  test_q_or_join_rejects_branches_missing_unbound_listed_vars ();
  test_q_or_join_required_vars_use_outer_bindings ();
  test_q_not_or_upstream_source_and_relation_batch ()

let test_query_or__test_default_source () =
  test_q_source_qualified_composite_clauses ();
  test_q_not_or_upstream_source_and_relation_batch ()

let test_query_or__test_const_substitution () =
  test_q_or_join_constant_substitution ()

let test_query_or__test_errors () =
  test_q_or_matches_upstream_error_messages ();
  test_q_or_rejects_branches_with_different_free_vars ()

let test_query_rules__test_rules () =
  test_q_with_rules ();
  test_q_regular_clauses_join_with_rules ();
  test_q_rule_context_is_isolated_from_outer_context ();
  test_q_rule_branches_match_upstream ();
  test_q_with_recursive_rules ();
  test_q_with_symmetric_recursive_rules ();
  test_q_with_mutually_recursive_rules ();
  test_q_with_dynamic_callable_inputs_in_rules ();
  test_q_can_call_same_dynamic_predicate_rule_twice ();
  test_q_source_qualified_rules ();
  test_q_rejects_unknown_rules ()

let test_query_rules__test_false_arguments () =
  test_q_rules_accept_false_arguments ()

let test_query_rules__test_rule_performance_on_larger_datasets () =
  let status_for i =
    match i mod 3 with
    | 0 -> "started"
    | 1 -> "pending"
    | _ -> "stopped"
  in
  let db =
    db_with
      (List.init 5000 (fun i ->
         let eid = i + 1 in
         Entity
           { db_id = Some (Entity_id eid)
           ; attrs =
               [ "item/id", One_value (Int eid)
               ; "item/status", One_value (String (status_for eid))
               ]
           }))
      (empty_db ())
  in
  let inline_query =
    { find = [ Find_var "e" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "item/status", QVar "status")
        ; Ground (String "pending", "status")
        ]
    }
  in
  let rule_query =
    { inline_query with
      rules =
        [ { rule_name = "pending?"
          ; rule_params = [ "status" ]
          ; rule_body = [ Ground (String "pending", "status") ]
          }
        ]
    ; where =
        [ Pattern (QVar "e", QAttr "item/status", QVar "status")
        ; Rule ("pending?", [ QVar "status" ])
        ]
    }
  in
  assert_equal_query
    "query_rules.cljc performance case keeps rule and inline results equivalent on larger inputs"
    (q db inline_query)
    (q db rule_query)

let test_unique_tuple_identity_upserts_entity_maps () =
  let db =
    empty_db ~schema:[ "a+b", tuple_unique_identity [ "a"; "b" ] ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ]
             }
         ]
    |> db_with
         [ Entity
             { db_id = None
             ; attrs =
                 [ "a", One_value (String "A")
                 ; "b", One_value (String "B")
                 ; "name", One_value (String "updated")
                 ]
             }
         ]
  in
  assert_equal_triples
    "entity maps upsert through unique tuple attrs derived from source attrs"
    [ 1, "a", String "A"
    ; 1, "a+b", Tuple [ Some (String "A"); Some (String "B") ]
    ; 1, "b", String "B"
    ; 1, "name", String "updated"
    ]
    (datoms db Eavt ())

let test_unique_tuple_identity_updates_multiple_sources_atomically () =
  let db =
    empty_db ~schema:[ "a+b", tuple_unique_identity [ "a"; "b" ] ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "a", One_value (String "a"); "b", One_value (String "b") ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "a", One_value (String "A"); "b", One_value (String "b") ]
             }
         ; Entity
             { db_id = Some (Entity_id 3)
             ; attrs = [ "a", One_value (String "a"); "b", One_value (String "B") ]
             }
         ]
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ]
             }
         ]
  in
  assert_equal_triples
    "entity maps update tuple source attrs against the final tuple value"
    [ 1, "a", String "A"
    ; 1, "a+b", Tuple [ Some (String "A"); Some (String "B") ]
    ; 1, "b", String "B"
    ; 2, "a", String "A"
    ; 2, "a+b", Tuple [ Some (String "A"); Some (String "b") ]
    ; 2, "b", String "b"
    ; 3, "a", String "a"
    ; 3, "a+b", Tuple [ Some (String "a"); Some (String "B") ]
    ; 3, "b", String "B"
    ]
    (datoms db Eavt ())

let test_add_tempid_upserts_by_unique_tuple_sources () =
  let db =
    empty_db ~schema:[ "a+b", tuple_unique_identity [ "a"; "b" ] ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "a", One_value (String "A")
                 ; "b", One_value (String "B")
                 ; "name", One_value (String "Ivan")
                 ]
             }
         ]
  in
  let report =
    transact
      db
      [ Add (Temp_id "person", "a", String "A")
      ; Add (Temp_id "person", "b", String "B")
      ; Add (Temp_id "person", "name", String "Oleg")
      ]
  in
  assert_equal_triples
    "Add tempid source attrs upsert through unique tuple identity"
    [ 1, "a", String "A"
    ; 1, "a+b", Tuple [ Some (String "A"); Some (String "B") ]
    ; 1, "b", String "B"
    ; 1, "name", String "Oleg"
    ]
    (datoms report.db_after Eavt ());
  assert_equal_tempids
    "tuple source tempid resolves to existing entity"
    [ "db/current-tx", tx0 + 2; "person", 1 ]
    report.tempids

let test_transact__test_tempid_ref_issue_295 () =
  let db =
    empty_db ~schema:[ "name", unique_identity; "ref", ref_attr ] ()
    |> db_with [ Entity { db_id = None; attrs = [ "name", One_value (String "Alice") ] } ]
  in
  let report =
    transact
      db
      [ Entity { db_id = Some (Temp_id "user"); attrs = [ "name", One_value (String "Alice") ] }
      ; Entity
          { db_id = None
          ; attrs = [ "age", One_value (Int 36); "ref", One_value (Ref_to (Temp_id "user")) ]
          }
      ]
  in
  assert_equal_triples
    "entity map tempid upsert remaps later ref values"
    [ 1, "name", String "Alice"; 2, "age", Int 36; 2, "ref", Ref 1 ]
    (datoms report.db_after Eavt ());
  assert_equal_tempids
    "entity map tempid resolves to existing unique identity entity"
    [ "db/current-tx", tx0 + 2; "user", 1 ]
    report.tempids

let test_unique_value_rejects_duplicate_values () =
  let db =
    empty_db ~schema:[ "name", unique_value ] ()
    |> db_with [ Add (Entity_id 1, "name", String "Ivan"); Add (Entity_id 2, "name", String "Petr") ]
  in
  assert_raises_invalid_arg
    "unique value cannot be reused by another entity"
    (fun () -> ignore (db_with [ Add (Entity_id 3, "name", String "Ivan") ] db));
  assert_raises_invalid_arg
    "unique value cannot be reused from entity maps"
    (fun () ->
      ignore
        (db_with
           [ Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Petr") ] } ]
           db));
  ignore (db_with [ Add (Entity_id 3, "name", String "Igor") ] db)

let test_db_with_string_unique_value_matches_upstream_validation () =
  let db =
    empty_db ~schema:[ "name", unique_value ] ()
    |> db_with_string
         "[[:db/add 1 :name \"Ivan\"]
           [:db/add 2 :name \"Petr\"]]"
  in
  assert_raises_invalid_arg_message
    "db_with_string rejects duplicate unique values in db/add vectors"
    "unique constraint"
    (fun () -> ignore (db_with_string "[[:db/add 3 :name \"Ivan\"]]" db));
  assert_raises_invalid_arg_message
    "db_with_string rejects duplicate unique values in entity maps"
    "unique constraint"
    (fun () -> ignore (db_with_string "[{:db/add 3 :name \"Petr\"}]" db));
  ignore (db_with_string "[[:db/add 3 :name \"Igor\"]]" db);
  ignore (db_with_string "[[:db/add 3 :nick \"Ivan\"]]" db)

let test_transact__test_transact_bang () =
  let conn = create_conn ~schema:[ "aka", many ] () in
  let report =
    transact_conn
      conn
      [ Add (Entity_id 1, "name", String "Ivan")
      ; Add (Entity_id 1, "aka", String "IV")
      ; Add (Entity_id 1, "aka", String "Terrible")
      ]
  in
  assert_equal_triples
    "report exposes db-before"
    []
    (datoms report.db_before Eavt ());
  assert_equal_triples
    "report exposes db-after"
    [ 1, "aka", String "IV"; 1, "aka", String "Terrible"; 1, "name", String "Ivan" ]
    (datoms report.db_after Eavt ());
  assert_equal_triples
    "connection points to db-after"
    [ 1, "aka", String "IV"; 1, "aka", String "Terrible"; 1, "name", String "Ivan" ]
    (datoms (conn_db conn) Eavt ())

let test_connection_auto_listener_keys () =
  let conn = create_conn () in
  let seen = ref [] in
  if listen conn "listener-1" (fun report -> seen := ("manual", report.tx_data) :: !seen) <> "listener-1" then
    failwith "listen should preserve explicit listener keys";
  let first_key = listen_auto conn (fun report -> seen := ("first", report.tx_data) :: !seen) in
  let second_key = listen_auto conn (fun report -> seen := ("second", report.tx_data) :: !seen) in
  if first_key = "listener-1" then failwith "listen_auto should not collide with explicit listener keys";
  if first_key = second_key then failwith "listen_auto should generate unique listener keys";
  ignore (transact_conn conn [ Add (Entity_id 1, "name", String "Ivan") ]);
  if List.length !seen <> 3 then failwith "listen_auto should register listener callbacks";
  unlisten conn first_key;
  ignore (transact_conn conn [ Add (Entity_id 2, "name", String "Petr") ]);
  if List.length !seen <> 5 then failwith "unlisten should remove auto-keyed listeners";
  let bang_key = listen_bang_auto conn (fun report -> seen := ("bang", report.tx_data) :: !seen) in
  if bang_key = "listener-1" || bang_key = first_key || bang_key = second_key then
    failwith "listen_bang_auto should generate a fresh listener key";
  ignore (transact_bang conn [ Add (Entity_id 3, "name", String "Oleg") ]);
  if List.length !seen <> 8 then failwith "listen_bang_auto should register listener callbacks";
  unlisten_bang conn bang_key;
  ignore (transact_bang conn [ Add (Entity_id 4, "name", String "Dima") ]);
  if List.length !seen <> 10 then failwith "unlisten_bang should remove auto-keyed listeners"

let test_bang_connection_api_aliases () =
  let conn = create_conn () in
  let seen = ref [] in
  if listen_bang conn "capture" (fun report -> seen := report.tx_data :: !seen) <> "capture" then
    failwith "listen_bang should return listener key";
  let report =
    transact_bang
      ~tx_meta:[ "op", Keyword "transact!" ]
      conn
      [ Add (Entity_id 1, "name", String "Ivan") ]
  in
  assert_equal_triples
    "transact_bang updates conn db"
    [ 1, "name", String "Ivan" ]
    (datoms (conn_db conn) Eavt ());
  if report.tx_meta <> [ "op", Keyword "transact!" ] then
    failwith "transact_bang should preserve tx metadata";
  if List.length !seen <> 1 then failwith "listen_bang should receive transact_bang reports";
  unlisten_bang conn "capture";
  ignore (transact_bang conn [ Add (Entity_id 2, "name", String "Petr") ]);
  if List.length !seen <> 1 then failwith "unlisten_bang should stop listener delivery";
  let replacement = empty_db () |> db_with [ Add (Entity_id 3, "name", String "Sergey") ] in
  let reset_db = reset_conn_bang ~tx_meta:[ "op", Keyword "reset!" ] conn replacement in
  assert_equal_triples
    "reset_conn_bang updates conn db"
    [ 3, "name", String "Sergey" ]
    (datoms reset_db Eavt ());
  let schema_db = reset_schema_bang conn [ "tag", many ] in
  if schema schema_db <> [ "tag", many ] then failwith "reset_schema_bang should return db with new schema";
  if schema (conn_db conn) <> [ "tag", many ] then failwith "reset_schema_bang should update conn db"

let test_connection_reports_strip_skip_store_metadata () =
  let storage = memory_storage () in
  let conn = create_conn ~storage () in
  let seen_meta = ref [] in
  ignore (listen conn "capture" (fun report -> seen_meta := report.tx_meta :: !seen_meta));
  let report =
    transact_conn
      ~tx_meta:[ "skip-store?", Bool true; "source", String "manual" ]
      conn
      [ Add (Entity_id 1, "name", String "Ivan") ]
  in
  if report.tx_meta <> [ "source", String "manual" ] then
    failwith "transact_conn should strip skip-store? from reports";
  if !seen_meta <> [ [ "source", String "manual" ] ] then
    failwith "listeners should receive tx metadata without skip-store?"

let test_transact__test_retract_fns () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "friend", One_value (Ref 2)
                 ]
             }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ]
    |> db_with [ RetractEntity (Entity_id 2) ]
  in
  assert_equal_triples
    "RetractEntity removes entity facts and incoming refs"
    [ 1, "name", String "Ivan" ]
    (datoms db Eavt ())

let test_transact__test_transient_issue_294 () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "a1", One_value (Int 1)
                 ; "a2", One_value (Int 2)
                 ; "a3", One_value (Int 3)
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "a1", One_value (Int 1)
                 ; "a2", One_value (Int 2)
                 ; "a3", One_value (Int 3)
                 ]
             }
         ]
  in
  let report = transact db [ RetractEntity (Entity_id 1); RetractEntity (Entity_id 2) ] in
  assert_equal_datoms
    "RetractEntity tx_data reports retractions in EAVT order"
    [ datom ~tx:(tx0 + 2) ~added:false ~e:1 ~a:"a1" ~v:(Int 1) ()
    ; datom ~tx:(tx0 + 2) ~added:false ~e:1 ~a:"a2" ~v:(Int 2) ()
    ; datom ~tx:(tx0 + 2) ~added:false ~e:1 ~a:"a3" ~v:(Int 3) ()
    ; datom ~tx:(tx0 + 2) ~added:false ~e:2 ~a:"a1" ~v:(Int 1) ()
    ; datom ~tx:(tx0 + 2) ~added:false ~e:2 ~a:"a2" ~v:(Int 2) ()
    ; datom ~tx:(tx0 + 2) ~added:false ~e:2 ~a:"a3" ~v:(Int 3) ()
    ]
    report.tx_data

let test_retract_attr_removes_all_attribute_values () =
  let db =
    empty_db ~schema:[ "aka", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "aka", Many_values [ String "IV"; String "Terrible" ]
                 ]
             }
         ]
    |> db_with [ RetractAttr (Entity_id 1, "aka") ]
  in
  assert_equal_triples
    "RetractAttr removes all values for one entity attribute"
    [ 1, "name", String "Ivan" ]
    (datoms db Eavt ())

let test_retract_entity_recursively_removes_components () =
  let db =
    empty_db ~schema:[ "profile", component ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "Ivan"); "profile", One_value (Ref 3) ]
             }
         ; Entity
             { db_id = Some (Entity_id 3)
             ; attrs = [ "email", One_value (String "ivan@example.com"); "profile", One_value (Ref 4) ]
             }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "email", One_value (String "nested@example.com") ] }
         ; Entity { db_id = Some (Entity_id 5); attrs = [ "email", One_value (String "unrelated@example.com") ] }
         ]
    |> db_with [ RetractEntity (Entity_id 1) ]
  in
  assert_equal_triples
    "RetractEntity recursively removes component children"
    [ 5, "email", String "unrelated@example.com" ]
    (datoms db Eavt ())

let test_retract_attr_removes_component_values () =
  let db =
    empty_db ~schema:[ "profiles", component_many ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "profiles", Many_values [ Ref 3; Ref 4 ] ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "email", One_value (String "a@example.com") ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "email", One_value (String "b@example.com") ] }
         ; Entity { db_id = Some (Entity_id 5); attrs = [ "email", One_value (String "kept@example.com") ] }
         ]
    |> db_with [ RetractAttr (Entity_id 1, "profiles") ]
  in
  assert_equal_triples
    "RetractAttr removes component values"
    [ 5, "email", String "kept@example.com" ]
    (datoms db Eavt ())

let test_pull_selects_requested_attributes () =
  let db =
    empty_db ~schema:[ "aka", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "age", One_value (Int 31)
                 ; "aka", Many_values [ String "IV"; String "Terrible" ]
                 ]
             }
         ]
  in
  match pull db [ Pull_attr "name"; Pull_attr "aka" ] (Entity_id 1) with
  | None -> failwith "expected pull to find entity"
  | Some entity ->
    assert_equal_int "pulled entity id" 1 entity.pulled_id;
    assert_equal_pulled_attrs
      "pull selects requested attrs"
      [ kw "aka", Pulled_many [ Pulled_scalar (String "IV"); Pulled_scalar (String "Terrible") ]
      ; kw "name", Pulled_scalar (String "Ivan")
      ]
      entity

let test_parse_pull_pattern_selects_attributes_and_refs () =
  let db =
    empty_db ~schema:[ "aka", many; "friend", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "aka", Many_values [ String "IV"; String "Terrible" ]
                 ; "friend", One_value (Ref 2)
                 ]
             }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ]
  in
  let pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormKeyword "name"
         ; QueryFormKeyword "aka"
         ; QueryFormMap [ QueryFormKeyword "friend", QueryFormVector [ QueryFormKeyword "name" ] ]
         ])
  in
  (match pull db pattern (Entity_id 1) with
   | None -> failwith "expected parsed pull pattern to find entity"
   | Some entity ->
     assert_equal_pulled_attrs
       "parse_pull_pattern parses attr and nested ref selectors"
       [ kw "aka", Pulled_many [ Pulled_scalar (String "IV"); Pulled_scalar (String "Terrible") ]
       ; kw "friend", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ] }
       ; kw "name", Pulled_scalar (String "Ivan")
       ]
       entity);
  let wildcard_pattern = parse_pull_pattern db (QueryFormVector [ QueryFormSymbol "*" ]) in
  (match pull db wildcard_pattern (Entity_id 2) with
   | None -> failwith "expected wildcard parsed pull pattern to find entity"
   | Some entity ->
     assert_equal_pulled_attrs
       "parse_pull_pattern parses wildcard selector"
       [ kw "db/id", Pulled_scalar (Int 2); kw "name", Pulled_scalar (String "Petr") ]
       entity);
  let string_wildcard_pattern = parse_pull_pattern db (QueryFormVector [ QueryFormString "*" ]) in
  (match pull db string_wildcard_pattern (Entity_id 2) with
   | None -> failwith "expected string wildcard parsed pull pattern to find entity"
   | Some entity ->
     assert_equal_pulled_attrs
       "parse_pull_pattern parses string wildcard selector"
       [ kw "db/id", Pulled_scalar (Int 2); kw "name", Pulled_scalar (String "Petr") ]
       entity);
  let keyword_wildcard_pattern = parse_pull_pattern db (QueryFormVector [ QueryFormKeyword "*" ]) in
  (match pull db keyword_wildcard_pattern (Entity_id 2) with
   | None -> failwith "expected keyword wildcard parsed pull pattern to find entity"
   | Some entity ->
     assert_equal_pulled_attrs
       "parse_pull_pattern parses keyword wildcard selector"
       [ kw "db/id", Pulled_scalar (Int 2); kw "name", Pulled_scalar (String "Petr") ]
       entity)

let test_parse_pull_pattern_accepts_top_level_lists () =
  let db = empty_db () |> db_with [ Add (Entity_id 1, "name", String "Ivan") ] in
  let pattern = parse_pull_pattern db (QueryFormList [ QueryFormKeyword "name" ]) in
  match pull db pattern (Entity_id 1) with
  | None -> failwith "expected list-form pull pattern to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "parse_pull_pattern accepts list-form top-level patterns"
      [ kw "name", Pulled_scalar (String "Ivan") ]
      entity

let test_parse_pull_pattern_accepts_string_db_id () =
  let db = empty_db () |> db_with [ Add (Entity_id 1, "name", String "Ivan") ] in
  let assert_pull label pattern expected =
    match pull db pattern (Entity_id 1) with
    | None -> failf "expected %s to pull the entity" label
    | Some entity -> assert_equal_pulled_attrs label expected entity
  in
  assert_pull
    "parse_pull_pattern treats string :db/id as db/id"
    (parse_pull_pattern db (QueryFormVector [ QueryFormString ":db/id" ]))
    [ kw "db/id", Pulled_scalar (Int 1) ];
  (match pull_string db "[\":db/id\"]" (Entity_id 1) with
   | None -> failwith "expected pull_string string :db/id to pull the entity"
   | Some entity ->
     assert_equal_pulled_attrs
       "pull_string treats string :db/id as db/id"
       [ kw "db/id", Pulled_scalar (Int 1) ]
       entity);
  assert_pull
    "parse_pull_pattern aliases string :db/id"
    (parse_pull_pattern
       db
       (QueryFormVector
          [ QueryFormVector [ QueryFormString ":db/id"; QueryFormKeyword "as"; QueryFormKeyword "id" ] ]))
    [ kw "id", Pulled_scalar (Int 1) ];
  assert_pull
    "parse_pull_pattern applies vector xform to string :db/id"
    (parse_pull_pattern
       db
       (QueryFormVector
          [ QueryFormVector [ QueryFormString ":db/id"; QueryFormKeyword "xform"; QueryFormSymbol "vector" ] ]))
    [ kw "db/id", Pulled_many [ Pulled_scalar (Int 1) ] ]

let test_parse_pull_pattern_aliases_attributes () =
  let db = empty_db () |> db_with [ Add (Entity_id 1, "name", String "Ivan") ] in
  let pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormVector [ QueryFormKeyword "name"; QueryFormKeyword "as"; QueryFormKeyword "display/name" ] ])
  in
  match pull db pattern (Entity_id 1) with
  | None -> failwith "expected parsed pull alias pattern to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "parse_pull_pattern parses :as attr expressions"
      [ kw "display/name", Pulled_scalar (String "Ivan") ]
      entity

let test_parse_pull_pattern_accepts_upstream_alias_value_forms () =
  let db = empty_db () |> db_with [ Add (Entity_id 1, "name", String "Ivan") ] in
  let assert_alias pattern expected_key =
    match pull_string db pattern (Entity_id 1) with
    | Some entity ->
      assert_equal_pulled_attrs
        ("parse_pull_pattern preserves alias key for " ^ pattern)
        [ expected_key, Pulled_scalar (String "Ivan") ]
        entity
    | None -> failf "expected alias pattern %s to pull the entity" pattern
  in
  assert_alias "[(:name :as :display/name)]" (kw "display/name");
  assert_alias "[(:name :as \"display-name\")]" (str_key "display-name");
  assert_alias "[(:name :as 123)]" (Int 123);
  assert_alias "[(:name :as nil)]" Nil

let test_parse_pull_pattern_defaults_attributes () =
  let db = empty_db () |> db_with [ Add (Entity_id 1, "name", String "Ivan") ] in
  let pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormVector [ QueryFormKeyword "missing"; QueryFormKeyword "default"; QueryFormString "fallback" ] ])
  in
  match pull db pattern (Entity_id 1) with
  | None -> failwith "expected parsed pull default pattern to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "parse_pull_pattern parses :default attr expressions"
      [ kw "missing", Pulled_scalar (String "fallback") ]
      entity

let test_parse_pull_pattern_limits_attributes () =
  let db =
    empty_db ~schema:[ "aka", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "aka", Many_values [ String "IV"; String "Terrible"; String "Tsar" ] ]
             }
         ]
  in
  let pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormVector [ QueryFormKeyword "aka"; QueryFormKeyword "limit"; QueryFormInt 2 ] ])
  in
  match pull db pattern (Entity_id 1) with
  | None -> failwith "expected parsed pull limit pattern to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "parse_pull_pattern parses :limit attr expressions"
      [ kw "aka", Pulled_many [ Pulled_scalar (String "IV"); Pulled_scalar (String "Terrible") ] ]
      entity

let test_parse_pull_pattern_legacy_limit_and_default () =
  let db =
    empty_db ~schema:[ "aka", many; "friend", ref_many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "aka", Many_values [ String "IV"; String "Terrible"; String "Tsar" ]
                 ; "friend", Many_values [ Ref 2; Ref 3 ]
                 ]
             }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Oleg") ] }
         ]
  in
  let pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormVector [ QueryFormString "limit"; QueryFormKeyword "aka"; QueryFormInt 1 ]
         ; QueryFormVector [ QueryFormString "default"; QueryFormKeyword "missing"; QueryFormString "fallback" ]
         ])
  in
  match pull db pattern (Entity_id 1) with
  | None -> failwith "expected parsed legacy pull pattern to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "parse_pull_pattern parses legacy limit/default expressions"
      [ kw "aka", Pulled_many [ Pulled_scalar (String "IV") ]; kw "missing", Pulled_scalar (String "fallback") ]
      entity;
  let list_pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormList [ QueryFormSymbol "limit"; QueryFormKeyword "aka"; QueryFormInt 2 ]
         ; QueryFormList [ QueryFormSymbol "default"; QueryFormKeyword "missing"; QueryFormString "fallback" ]
         ; QueryFormMap
             [ ( QueryFormList [ QueryFormSymbol "limit"; QueryFormKeyword "friend"; QueryFormInt 1 ]
               , QueryFormVector [ QueryFormKeyword "name" ] )
             ]
         ])
  in
  (match pull db list_pattern (Entity_id 1) with
   | None -> failwith "expected parsed list legacy pull pattern to find entity"
   | Some entity ->
     assert_equal_pulled_attrs
       "parse_pull_pattern parses list-form legacy limit/default expressions"
       [ kw "aka", Pulled_many [ Pulled_scalar (String "IV"); Pulled_scalar (String "Terrible") ]
       ; kw "friend", Pulled_many [ Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ] } ]
       ; kw "missing", Pulled_scalar (String "fallback")
       ]
       entity);
  (match
    pull_string
      db
      "[(limit :aka 2) (default :missing \"fallback\") {(limit :friend 1) [:name]}]"
      (Entity_id 1)
  with
  | None -> failwith "expected pull_string legacy list pull pattern to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "pull_string parses EDN list-form legacy limit/default expressions"
      [ kw "aka", Pulled_many [ Pulled_scalar (String "IV"); Pulled_scalar (String "Terrible") ]
      ; kw "friend", Pulled_many [ Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ] } ]
      ; kw "missing", Pulled_scalar (String "fallback")
      ]
      entity);
  (match
     pull_string
       db
       "[[\"limit\" :aka 2] [\"default\" :missing \"fallback\"] { [\"limit\" :friend 1] [:name]}]"
       (Entity_id 1)
   with
   | None -> failwith "expected pull_string vector string legacy pull pattern to find entity"
   | Some entity ->
     assert_equal_pulled_attrs
       "pull_string parses vector string legacy limit/default expressions"
       [ kw "aka", Pulled_many [ Pulled_scalar (String "IV"); Pulled_scalar (String "Terrible") ]
       ; kw "friend", Pulled_many [ Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ] } ]
       ; kw "missing", Pulled_scalar (String "fallback")
       ]
       entity)

let test_parse_pull_pattern_list_form_attr_options () =
  let db =
    empty_db ~schema:[ "aka", many; "friend", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "aka", Many_values [ String "IV"; String "Terrible" ]
                 ; "friend", One_value (Ref 2)
                 ]
             }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ]
  in
  match
    pull_string
      db
      "[(:name :as \"Name\") (:missing :default \"fallback\") (:aka :limit 1 :as :alias) {(:friend :as :buddy) [:name]}]"
      (Entity_id 1)
  with
  | None -> failwith "expected pull_string list-form attr options to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "pull_string parses list-form attr options"
      [ str_key "Name", Pulled_scalar (String "Ivan")
      ; kw "alias", Pulled_many [ Pulled_scalar (String "IV") ]
      ; kw "buddy", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ] }
      ; kw "missing", Pulled_scalar (String "fallback")
      ]
      entity

let test_parse_pull_pattern_multi_entry_map_specs () =
  let db =
    empty_db ~schema:[ "friend", ref_attr; "spouse", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "friend", One_value (Ref 2)
                 ; "spouse", One_value (Ref 3)
                 ]
             }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Anna") ] }
         ]
  in
  match pull_string db "[{:friend [:name] :spouse [:name]}]" (Entity_id 1) with
  | None -> failwith "expected pull_string multi-entry map specs to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "pull_string parses multi-entry map specs"
      [ kw "friend", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ] }
      ; kw "spouse", Pulled_entity { pulled_id = 3; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Anna") ] }
      ]
      entity

let test_parse_pull_pattern_rejects_reserved_string_attr_names () =
  let db = empty_db ~schema:[ "limit", indexed; "default", indexed ] () in
  assert_raises_invalid_arg
    "parse_pull_pattern rejects reserved string limit as attr name"
    (fun () -> ignore (parse_pull_pattern db (QueryFormVector [ QueryFormString "limit" ])));
  assert_raises_invalid_arg
    "parse_pull_pattern rejects reserved string default as attr name"
    (fun () -> ignore (parse_pull_pattern db (QueryFormVector [ QueryFormString "default" ])))

let test_parse_pull_pattern_validates_limits () =
  let db = empty_db ~schema:[ "aka", many; "name", indexed; "child", ref_attr ] () in
  assert_raises_invalid_arg
    "parse_pull_pattern rejects zero limits"
    (fun () ->
       ignore
         (parse_pull_pattern
            db
            (QueryFormVector [ QueryFormVector [ QueryFormKeyword "aka"; QueryFormKeyword "limit"; QueryFormInt 0 ] ])));
  assert_raises_invalid_arg
    "parse_pull_pattern rejects negative limits"
    (fun () ->
       ignore
         (parse_pull_pattern
            db
            (QueryFormVector
               [ QueryFormVector [ QueryFormKeyword "aka"; QueryFormKeyword "limit"; QueryFormInt (-1) ] ])));
  assert_raises_invalid_arg
    "parse_pull_pattern rejects limits on cardinality-one attrs"
    (fun () ->
       ignore
         (parse_pull_pattern
            db
            (QueryFormVector [ QueryFormVector [ QueryFormKeyword "name"; QueryFormKeyword "limit"; QueryFormInt 1 ] ])));
  assert_raises_invalid_arg
    "parse_pull_pattern rejects nil limits on cardinality-one refs"
    (fun () ->
       ignore
         (parse_pull_pattern
            db
            (QueryFormVector [ QueryFormVector [ QueryFormKeyword "child"; QueryFormKeyword "limit"; QueryFormNil ] ])));
  assert_raises_invalid_arg
    "parse_pull_pattern validates legacy limits"
    (fun () ->
       ignore
         (parse_pull_pattern
            db
            (QueryFormVector [ QueryFormVector [ QueryFormString "limit"; QueryFormKeyword "name"; QueryFormInt 1 ] ])))

let test_parse_pull_pattern_unlimited_limits () =
  let many_akas =
    List.init 1001 (fun index -> String (Printf.sprintf "aka-%04d" index))
  in
  let child_refs = List.init 1001 (fun index -> Ref (index + 2)) in
  let child_entities =
    List.init 1001 (fun index ->
      Entity
        { db_id = Some (Entity_id (index + 2))
        ; attrs = [ "name", One_value (String (Printf.sprintf "child-%04d" index)) ]
        })
  in
  let db =
    empty_db ~schema:[ "aka", many; "child", ref_many ] ()
    |> db_with
         (Entity
            { db_id = Some (Entity_id 1)
            ; attrs = [ "aka", Many_values many_akas; "child", Many_values child_refs ]
            }
          :: child_entities)
  in
  let attr_option_pattern =
    parse_pull_pattern
      db
      (QueryFormVector [ QueryFormVector [ QueryFormKeyword "aka"; QueryFormKeyword "limit"; QueryFormNil ] ])
  in
  (match pull db attr_option_pattern (Entity_id 1) with
   | Some { pulled_attrs = [ Keyword "aka", Pulled_many values ]; _ } ->
     assert_equal_int "parse_pull_pattern parses :limit nil attr options" 1001 (List.length values)
   | _ -> failwith "expected unlimited attr option pull");
  let legacy_pattern =
    parse_pull_pattern
      db
      (QueryFormVector [ QueryFormVector [ QueryFormString "limit"; QueryFormKeyword "aka"; QueryFormNil ] ])
  in
  (match pull db legacy_pattern (Entity_id 1) with
   | Some { pulled_attrs = [ Keyword "aka", Pulled_many values ]; _ } ->
     assert_equal_int "parse_pull_pattern parses legacy nil limits" 1001 (List.length values)
   | _ -> failwith "expected legacy unlimited pull");
  let ref_pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormMap
             [ ( QueryFormVector [ QueryFormKeyword "child"; QueryFormKeyword "limit"; QueryFormNil ]
               , QueryFormVector [ QueryFormKeyword "db/id" ] )
             ]
         ])
  in
  match pull db ref_pattern (Entity_id 1) with
  | Some { pulled_attrs = [ Keyword "child", Pulled_many values ]; _ } ->
    assert_equal_int "parse_pull_pattern parses :limit nil ref map specs" 1001 (List.length values)
  | _ -> failwith "expected unlimited ref pull"

let test_parse_pull_pattern_xforms_attributes () =
  let db =
    empty_db ~schema:[ "aka", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "kind", One_value (Keyword "user/name")
                 ; "age", One_value (Int 42)
                 ; "aka", Many_values [ String "Ivan"; String "Vanya" ]
                 ]
             }
         ]
  in
  let pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormVector
             [ QueryFormKeyword "kind"
             ; QueryFormKeyword "xform"
             ; QueryFormSymbol "name"
             ; QueryFormKeyword "as"
             ; QueryFormKeyword "kind/name"
             ]
         ; QueryFormVector
             [ QueryFormKeyword "kind"
             ; QueryFormKeyword "xform"
             ; QueryFormSymbol "namespace"
             ; QueryFormKeyword "as"
             ; QueryFormKeyword "kind/ns"
             ]
         ; QueryFormVector
             [ QueryFormKeyword "age"
             ; QueryFormKeyword "xform"
             ; QueryFormSymbol "str"
             ; QueryFormKeyword "as"
             ; QueryFormKeyword "age/text"
             ]
         ; QueryFormVector
             [ QueryFormKeyword "missing"
             ; QueryFormKeyword "default"
             ; QueryFormKeyword "fallback/value"
             ; QueryFormKeyword "xform"
             ; QueryFormSymbol "name"
             ; QueryFormKeyword "as"
             ; QueryFormKeyword "missing/name"
             ]
         ])
  in
  (match pull db pattern (Entity_id 1) with
   | None -> failwith "expected parsed xform pull to find entity"
   | Some entity ->
     assert_equal_pulled_attrs
       "parse_pull_pattern parses built-in :xform attr expressions"
       [ kw "age/text", Pulled_scalar (String "42")
       ; kw "kind/name", Pulled_scalar (String "name")
       ; kw "kind/ns", Pulled_scalar (String "user")
       ; kw "missing/name", Pulled_scalar (Keyword "fallback/value")
       ]
       entity);
  let vector_pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormVector [ QueryFormKeyword "kind"; QueryFormKeyword "xform"; QueryFormSymbol "vector" ]
         ; QueryFormVector [ QueryFormKeyword "aka"; QueryFormKeyword "xform"; QueryFormSymbol "vector" ]
         ])
  in
  (match pull db vector_pattern (Entity_id 1) with
   | None -> failwith "expected parsed vector xform pull to find entity"
   | Some entity ->
     assert_equal_pulled_attrs
       "parse_pull_pattern resolves built-in vector xform"
       [ kw "aka", Pulled_many [ Pulled_many [ Pulled_scalar (String "Ivan"); Pulled_scalar (String "Vanya") ] ]
       ; kw "kind", Pulled_many [ Pulled_scalar (Keyword "user/name") ]
       ]
       entity);
  assert_raises_invalid_arg
    "parse_pull_pattern rejects unknown xform symbols"
    (fun () ->
       ignore
         (parse_pull_pattern
            db
            (QueryFormVector
               [ QueryFormVector
                   [ QueryFormKeyword "kind"
                   ; QueryFormKeyword "xform"
                   ; QueryFormSymbol "missing/xform"
                   ]
               ])))

let test_parse_pull_pattern_xforms_ref_map_specs () =
  let db =
    empty_db ~schema:[ "child", ref_many; "father", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "child", Many_values [ Ref 2; Ref 3 ]
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "David"); "father", One_value (Ref 1) ]
             }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Thomas") ] }
         ]
  in
  let pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormMap
             [ ( QueryFormVector
                   [ QueryFormKeyword "child"
                   ; QueryFormKeyword "xform"
                   ; QueryFormSymbol "identity"
                   ; QueryFormKeyword "as"
                   ; QueryFormKeyword "profile/children"
                   ]
               , QueryFormVector [ QueryFormKeyword "name" ] )
             ]
         ; QueryFormMap
             [ ( QueryFormVector
                   [ QueryFormKeyword "_father"
                   ; QueryFormKeyword "xform"
                   ; QueryFormSymbol "identity"
                   ; QueryFormKeyword "as"
                   ; QueryFormKeyword "profile/parents"
                   ]
               , QueryFormVector [ QueryFormKeyword "name" ] )
             ]
         ])
  in
  match pull db pattern (Entity_id 1) with
  | None -> failwith "expected parsed ref xform pull to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "parse_pull_pattern parses :xform ref map specs"
      [ kw "profile/children", Pulled_many
          [ Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "David") ] }
          ; Pulled_entity { pulled_id = 3; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Thomas") ] }
          ]
      ; kw "profile/parents", Pulled_many
          [ Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "David") ] } ]
      ]
      entity

let test_parse_pull_pattern_validates_map_spec_refs () =
  let db = empty_db ~schema:[ "name", indexed; "aka", many; "child", ref_many ] () in
  assert_raises_invalid_arg
    "parse_pull_pattern rejects map specs for non-ref attrs"
    (fun () ->
       ignore
         (parse_pull_pattern
            db
            (QueryFormVector
               [ QueryFormMap [ QueryFormKeyword "name", QueryFormVector [ QueryFormKeyword "db/id" ] ] ])));
  assert_raises_invalid_arg
    "parse_pull_pattern rejects map specs for unknown attrs"
    (fun () ->
       ignore
         (parse_pull_pattern
            db
            (QueryFormVector
               [ QueryFormMap [ QueryFormKeyword "missing", QueryFormVector [ QueryFormKeyword "db/id" ] ] ])));
  assert_raises_invalid_arg
    "parse_pull_pattern rejects reverse map specs for non-ref attrs"
    (fun () ->
       ignore
         (parse_pull_pattern
            db
            (QueryFormVector
               [ QueryFormMap [ QueryFormKeyword "_aka", QueryFormVector [ QueryFormKeyword "db/id" ] ] ])));
  assert_raises_invalid_arg
    "parse_pull_pattern rejects recursive map specs for non-ref attrs"
    (fun () ->
       ignore
         (parse_pull_pattern
            db
            (QueryFormVector [ QueryFormMap [ QueryFormKeyword "name", QueryFormSymbol "..." ] ])))

let test_parse_pull_pattern_validates_reverse_attrs () =
  let db = empty_db ~schema:[ "aka", many; "child", ref_many ] () in
  assert_raises_invalid_arg
    "parse_pull_pattern rejects reverse attrs for non-ref attrs"
    (fun () ->
       ignore (parse_pull_pattern db (QueryFormVector [ QueryFormKeyword "_aka" ])));
  assert_raises_invalid_arg
    "parse_pull_pattern rejects reverse attrs for unknown attrs"
    (fun () ->
       ignore (parse_pull_pattern db (QueryFormVector [ QueryFormKeyword "_missing" ])));
  let pattern = parse_pull_pattern db (QueryFormVector [ QueryFormKeyword "_child" ]) in
  if pattern <> [ Pull_attr "_child" ] then failwith "parse_pull_pattern should accept reverse ref attrs"

let test_parse_pull_pattern_expands_reverse_refs () =
  let db =
    empty_db ~schema:[ "father", ref_attr ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Petr") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "David"); "father", One_value (Ref 1) ] }
         ]
  in
  let pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormMap [ QueryFormKeyword "_father", QueryFormVector [ QueryFormKeyword "name" ] ] ])
  in
  match pull db pattern (Entity_id 1) with
  | None -> failwith "expected parsed reverse pull pattern to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "parse_pull_pattern parses reverse ref map specs"
      [ kw "father", Pulled_many
          [ Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "David") ] } ]
      ]
      entity

let test_parse_pull_pattern_recursive_refs () =
  let db =
    empty_db ~schema:[ "part", ref_many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "Part A"); "part", Many_values [ Ref 2; Ref 3 ] ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "Part A.A"); "part", Many_values [ Ref 4 ] ]
             }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Part A.B") ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "name", One_value (String "Part A.A.A") ] }
         ]
  in
  let pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormKeyword "name"; QueryFormMap [ QueryFormKeyword "part", QueryFormInt 2 ] ])
  in
  match pull db pattern (Entity_id 1) with
  | None -> failwith "expected parsed recursive pull pattern to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "parse_pull_pattern parses numeric recursive map specs"
      [ kw "name", Pulled_scalar (String "Part A")
      ; kw "part", Pulled_many
          [ Pulled_entity
              { pulled_id = 2
              ; pulled_attrs =
                  [ kw "name", Pulled_scalar (String "Part A.A")
                  ; kw "part", Pulled_many
                      [ Pulled_entity
                          { pulled_id = 4
                          ; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Part A.A.A") ]
                          }
                      ]
                  ]
              }
          ; Pulled_entity
              { pulled_id = 3
              ; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Part A.B") ]
              }
          ]
      ]
      entity

let test_parse_pull_pattern_recursive_refs_preserve_context () =
  let db =
    empty_db ~schema:[ "part", ref_many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "label", One_value (String "Part A"); "part", Many_values [ Ref 2 ] ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "label", One_value (String "Part A.A"); "part", Many_values [ Ref 3 ] ]
             }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "label", One_value (String "Part A.A.A") ] }
         ]
  in
  let pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormKeyword "label"; QueryFormMap [ QueryFormKeyword "part", QueryFormInt 2 ] ])
  in
  match pull db pattern (Entity_id 1) with
  | None -> failwith "expected parsed recursive pull pattern to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "parse_pull_pattern recursive specs preserve the surrounding selector context"
      [ kw "label", Pulled_scalar (String "Part A")
      ; kw "part", Pulled_many
          [ Pulled_entity
              { pulled_id = 2
              ; pulled_attrs =
                  [ kw "label", Pulled_scalar (String "Part A.A")
                  ; kw "part", Pulled_many
                      [ Pulled_entity
                          { pulled_id = 3
                          ; pulled_attrs = [ Keyword "label", Pulled_scalar (String "Part A.A.A") ]
                          }
                      ]
                  ]
              }
          ]
      ]
      entity

let test_parse_pull_pattern_recursive_string_ellipsis () =
  let db =
    empty_db ~schema:[ "part", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "A"); "part", One_value (Ref 2) ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "B"); "part", One_value (Ref 1) ]
             }
         ]
  in
  let pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormKeyword "name"; QueryFormMap [ QueryFormKeyword "part", QueryFormString "..." ] ])
  in
  match pull db pattern (Entity_id 1) with
  | None -> failwith "expected parsed recursive string ellipsis pull"
  | Some entity ->
    assert_equal_pulled_attrs
      "parse_pull_pattern treats string ellipsis as recursive pull"
      [ kw "name", Pulled_scalar (String "A")
      ; kw "part", Pulled_entity
          { pulled_id = 2
          ; pulled_attrs =
              [ kw "name", Pulled_scalar (String "B")
              ; kw "part", Pulled_entity
                  { pulled_id = 1
                  ; pulled_attrs =
                      [ kw "name", Pulled_scalar (String "A")
                      ; kw "part", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2) ] }
                      ]
                  }
              ]
          }
      ]
      entity

let test_pull_aliases_selected_attributes () =
  let db =
    empty_db ~schema:[ "friend", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "friend", One_value (Ref 2)
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "Petr") ]
             }
         ]
  in
  match
    pull
      db
      [ Pull_as (Pull_attr "name", kw "display/name")
      ; Pull_as (Pull_ref ("friend", [ Pull_attr "name" ]), kw "profile/friend")
      ]
      (Entity_id 1)
  with
  | None -> failwith "expected pull to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "pull aliases scalar and nested ref attributes"
      [ ( kw "display/name"
        , Pulled_scalar (String "Ivan") )
      ; ( kw "profile/friend"
        , Pulled_entity
            { pulled_id = 2
            ; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ]
            } )
      ]
      entity

let test_pull_later_duplicate_keys_replace_earlier_values () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "Ivan"); "nickname", One_value (String "Vanya") ]
             }
         ]
  in
  (match
     pull
       db
       [ Pull_attr "name"
       ; Pull_as (Pull_attr "nickname", kw "name")
       ; Pull_as (Pull_attr "name", kw "display")
       ; Pull_as (Pull_attr "nickname", kw "display")
       ]
       (Entity_id 1)
   with
   | None -> failwith "expected pull with duplicate keys to find entity"
   | Some entity ->
     assert_equal_pulled_attrs
       "pull keeps the last value for duplicate output keys"
       [ kw "display", Pulled_scalar (String "Vanya"); kw "name", Pulled_scalar (String "Vanya") ]
       entity);
  let parsed_pattern =
    parse_pull_pattern
      db
      (QueryFormVector
         [ QueryFormVector [ QueryFormKeyword "name"; QueryFormKeyword "as"; QueryFormKeyword "label" ]
         ; QueryFormVector [ QueryFormKeyword "nickname"; QueryFormKeyword "as"; QueryFormKeyword "label" ]
         ])
  in
  match pull db parsed_pattern (Entity_id 1) with
  | None -> failwith "expected parsed duplicate alias pull to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "parsed pull keeps the last selector for duplicate aliases"
      [ kw "label", Pulled_scalar (String "Vanya") ]
      entity

let test_pull_transforms_selected_attributes () =
  let db =
    empty_db ~schema:[ "aka", many; "child", ref_many; "parent", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "aka", Many_values [ String "IV"; String "Terrible" ]
                 ]
             }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ]
  in
  let wrap = function
    | Pulled_scalar value -> Pulled_many [ Pulled_scalar value ]
    | value -> Pulled_many [ value ]
  in
  let missing_label = function
    | Pulled_scalar Nil | Pulled_many [] -> Pulled_scalar (String "missing")
    | value -> value
  in
  match
    pull
      db
      [ Pull_attr_xform ("name", wrap)
      ; Pull_attr_xform ("aka", wrap)
      ; Pull_attr_xform ("unknown", missing_label)
      ]
      (Entity_id 1)
  with
  | None -> failwith "expected pull to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "pull applies xform to scalar and many attrs"
      [ kw "aka", Pulled_many [ Pulled_many [ Pulled_scalar (String "IV"); Pulled_scalar (String "Terrible") ] ]
      ; kw "name", Pulled_many [ Pulled_scalar (String "Ivan") ]
      ; kw "unknown", Pulled_scalar (String "missing")
      ]
      entity;
  let wrap_nil = function
    | Pulled_scalar Nil -> Pulled_many [ Pulled_scalar Nil ]
    | value -> value
  in
  match
    pull
      db
      [ Pull_attr_xform ("unknown", wrap_nil)
      ; Pull_ref_xform ("child", [ Pull_attr "name" ], wrap_nil)
      ; Pull_reverse_ref_xform ("parent", [ Pull_attr "name" ], wrap_nil)
      ]
      (Entity_id 1)
  with
  | None -> failwith "expected missing xform pull to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "pull xform receives nil for missing attrs and refs"
      [ kw "child", Pulled_many [ Pulled_scalar Nil ]
      ; kw "parent", Pulled_many [ Pulled_scalar Nil ]
      ; kw "unknown", Pulled_many [ Pulled_scalar Nil ]
      ]
      entity

let test_pull_default_takes_precedence_over_xform () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ]
  in
  let wrap = function
    | Pulled_scalar value -> Pulled_many [ Pulled_scalar value ]
    | value -> Pulled_many [ value ]
  in
  match
    pull
      db
      [ Pull_attr_default_xform ("name", String "fallback", wrap)
      ; Pull_attr_default_xform ("unknown", String "fallback", wrap)
      ]
      (Entity_id 1)
  with
  | None -> failwith "expected pull to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "pull default takes precedence over xform for missing attrs"
      [ kw "name", Pulled_many [ Pulled_scalar (String "Ivan") ]
      ; kw "unknown", Pulled_scalar (String "fallback")
      ]
      entity

let test_pull_attr_default_and_limit () =
  let db =
    empty_db ~schema:[ "aka", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "aka", Many_values [ String "IV"; String "Terrible"; String "Tsar" ]
                 ]
             }
         ]
  in
  match
    pull
      db
      [ Pull_attr_default ("missing", String "n/a")
      ; Pull_attr_default ("name", String "fallback")
      ; Pull_attr_limit ("aka", 2)
      ]
      (Entity_id 1)
  with
  | None -> failwith "expected pull to find entity"
  | Some entity ->
    assert_equal_pulled_attrs
      "pull supports defaults for missing attrs and limits many attrs"
      [ kw "aka", Pulled_many [ Pulled_scalar (String "IV"); Pulled_scalar (String "Terrible") ]
      ; kw "missing", Pulled_scalar (String "n/a")
      ; kw "name", Pulled_scalar (String "Ivan")
      ]
      entity

let test_pull_ref_default_expands_existing_refs () =
  let db =
    empty_db ~schema:[ "child", ref_attr ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Petr") ] }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "David"); "child", One_value (Ref 3) ]
             }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Thomas") ] }
         ]
  in
  (match pull db [ Pull_ref_default ("child", [ Pull_attr "name" ], String "[child]") ] (Entity_id 1) with
   | Some entity ->
     assert_equal_pulled_attrs
       "pull ref default returns default when the ref attr is missing"
       [ kw "child", Pulled_scalar (String "[child]") ]
       entity
   | None -> failwith "expected default pull result");
  match pull db [ Pull_ref_default ("child", [ Pull_attr "name" ], String "[child]") ] (Entity_id 2) with
  | Some entity ->
    assert_equal_pulled_attrs
      "pull ref default expands existing ref attrs"
      [ kw "child", Pulled_entity { pulled_id = 3; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Thomas") ] } ]
      entity
  | None -> failwith "expected ref pull result"

let test_pull_reverse_ref_default_expands_existing_refs () =
  let db =
    empty_db ~schema:[ "child", ref_attr ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Petr") ] }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "David"); "child", One_value (Ref 3) ]
             }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Thomas") ] }
         ]
  in
  (match pull db [ Pull_reverse_ref_default ("child", [ Pull_attr "name" ], String "[parent]") ] (Entity_id 1) with
   | Some entity ->
     assert_equal_pulled_attrs
       "pull reverse ref default returns default when no incoming refs exist"
       [ kw "child", Pulled_scalar (String "[parent]") ]
       entity
   | None -> failwith "expected reverse default pull result");
  match pull db [ Pull_reverse_ref_default ("child", [ Pull_attr "name" ], String "[parent]") ] (Entity_id 3) with
  | Some entity ->
    assert_equal_pulled_attrs
      "pull reverse ref default expands existing incoming refs"
      [ kw "child", Pulled_many [ Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "David") ] } ] ]
      entity
  | None -> failwith "expected reverse ref pull result"

let test_pull_applies_default_limit () =
  let many_akas =
    List.init 1001 (fun index -> String (Printf.sprintf "aka-%04d" index))
  in
  let db =
    empty_db ~schema:[ "aka", many ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "aka", Many_values many_akas ] } ]
  in
  (match pull db [ Pull_attr "aka" ] (Entity_id 1) with
   | Some { pulled_attrs = [ Keyword "aka", Pulled_many values ]; _ } ->
     assert_equal_int "pull default limit" 1000 (List.length values)
   | _ -> failwith "expected default-limited many attr");
  (match pull db [ Pull_attr_limit ("aka", 1001) ] (Entity_id 1) with
   | Some { pulled_attrs = [ Keyword "aka", Pulled_many values ]; _ } ->
     assert_equal_int "pull explicit limit can increase default" 1001 (List.length values)
   | _ -> failwith "expected explicit-limited many attr");
  (match pull db [ Pull_attr_unlimited "aka" ] (Entity_id 1) with
   | Some { pulled_attrs = [ Keyword "aka", Pulled_many values ]; _ } ->
     assert_equal_int "pull explicit unlimited limit" 1001 (List.length values)
   | _ -> failwith "expected explicit-limited many attr");
  let child_refs = List.init 1001 (fun index -> Ref (index + 2)) in
  let child_entities =
    List.init 1001 (fun index ->
      Entity
        { db_id = Some (Entity_id (index + 2))
        ; attrs = [ "name", One_value (String (Printf.sprintf "child-%04d" index)) ]
        })
  in
  let ref_db =
    empty_db ~schema:[ "child", ref_many ] ()
    |> db_with
         (Entity
            { db_id = Some (Entity_id 1); attrs = [ "child", Many_values child_refs ] }
          :: child_entities)
  in
  (match pull ref_db [ Pull_ref ("child", [ Pull_id ]) ] (Entity_id 1) with
   | Some { pulled_attrs = [ Keyword "child", Pulled_many values ]; _ } ->
     assert_equal_int "pull default limit applies to many ref expansion" 1000 (List.length values)
   | _ -> failwith "expected default-limited many ref attr");
  (match pull ref_db [ Pull_ref_limit ("child", [ Pull_id ], 1001) ] (Entity_id 1) with
   | Some { pulled_attrs = [ Keyword "child", Pulled_many values ]; _ } ->
     assert_equal_int "pull explicit limit can increase many ref expansion" 1001 (List.length values)
   | _ -> failwith "expected explicit-limited many ref attr");
  let reverse_db =
    empty_db ~schema:[ "parent", ref_attr ] ()
    |> db_with
         (Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "root") ] }
          :: List.init 1001 (fun index ->
            Entity
              { db_id = Some (Entity_id (index + 2))
              ; attrs = [ "parent", One_value (Ref 1) ]
              }))
  in
  (match pull reverse_db [ Pull_reverse_ref ("parent", [ Pull_id ]) ] (Entity_id 1) with
   | Some { pulled_attrs = [ Keyword "parent", Pulled_many values ]; _ } ->
     assert_equal_int "pull default limit applies to reverse ref expansion" 1000 (List.length values)
   | _ -> failwith "expected default-limited reverse ref attr");
  (match pull reverse_db [ Pull_reverse_ref_limit ("parent", [ Pull_id ], 1001) ] (Entity_id 1) with
   | Some { pulled_attrs = [ Keyword "parent", Pulled_many values ]; _ } ->
     assert_equal_int "pull explicit limit can increase reverse ref expansion" 1001 (List.length values)
   | _ -> failwith "expected explicit-limited reverse ref attr")

let test_pull_drops_empty_results () =
  let db =
    empty_db ~schema:[ "child", ref_many ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Petr"); "child", Many_values [ Ref 2; Ref 3 ] ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "David") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Thomas") ] }
         ]
  in
  if pull db [ Pull_attr "missing" ] (Entity_id 1) <> None then
    failwith "pull should return None when no selectors produce attrs";
  (match pull db [ Pull_attr "name"; Pull_ref ("child", [ Pull_attr "missing" ]) ] (Entity_id 1) with
   | None -> failwith "expected parent pull result"
   | Some entity ->
     assert_equal_pulled_attrs
       "pull should drop ref entities whose nested selector returns no attrs"
       [ kw "name", Pulled_scalar (String "Petr") ]
       entity)

let test_pull_drops_empty_cardinality_one_ref_results () =
  let db =
    empty_db ~schema:[ "father", ref_attr ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Petr") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "David"); "father", One_value (Ref 1) ] }
         ]
  in
  (match pull db [ Pull_attr "name"; Pull_ref ("father", [ Pull_attr "missing" ]) ] (Entity_id 2) with
   | None -> failwith "expected child pull result"
   | Some entity ->
     assert_equal_pulled_attrs
       "pull should drop cardinality-one ref when nested selector returns no attrs"
       [ kw "name", Pulled_scalar (String "David") ]
       entity)

let test_pull_expands_forward_and_reverse_refs () =
  let db =
    empty_db ~schema:[ "child", ref_many; "father", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "child", Many_values [ Ref 2; Ref 3 ]
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "name", One_value (String "David")
                 ; "father", One_value (Ref 1)
                 ]
             }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Thomas") ] }
         ]
  in
  (match pull db [ Pull_attr "name"; Pull_ref ("child", [ Pull_attr "name" ]) ] (Entity_id 1) with
   | None -> failwith "expected forward ref pull"
   | Some entity ->
     assert_equal_pulled_attrs
       "pull expands forward refs"
       [ kw "child", Pulled_many
           [ Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "David") ] }
           ; Pulled_entity { pulled_id = 3; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Thomas") ] }
           ]
       ; kw "name", Pulled_scalar (String "Petr")
       ]
       entity);
  (match pull db [ Pull_attr "name"; Pull_reverse_ref ("father", [ Pull_attr "name" ]) ] (Entity_id 1) with
   | None -> failwith "expected reverse ref pull"
   | Some entity ->
     assert_equal_pulled_attrs
       "pull expands reverse refs"
       [ kw "father", Pulled_many
           [ Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "David") ] } ]
       ; kw "name", Pulled_scalar (String "Petr")
       ]
       entity);
  let ref_names = function
    | Pulled_entity { pulled_attrs; _ } ->
      (match List.assoc_opt (kw "name") pulled_attrs with
       | Some (Pulled_scalar name) -> Pulled_scalar name
       | _ -> Pulled_many [])
    | Pulled_many values ->
      Pulled_many
        (List.filter_map
           (function
             | Pulled_entity { pulled_attrs; _ } ->
               (match List.assoc_opt (kw "name") pulled_attrs with
                | Some (Pulled_scalar name) -> Some (Pulled_scalar name)
                | _ -> None)
             | _ -> None)
           values)
    | value -> value
  in
  (match
     pull
       db
       [ Pull_ref_xform ("child", [ Pull_attr "name" ], ref_names)
       ; Pull_reverse_ref_xform ("father", [ Pull_attr "name" ], ref_names)
       ; Pull_ref_xform
           ( "unknown-child"
           , [ Pull_attr "name" ]
           , (function
             | Pulled_scalar Nil | Pulled_many [] -> Pulled_scalar (String "missing")
             | value -> value) )
       ]
       (Entity_id 1)
   with
   | None -> failwith "expected xformed ref pull"
   | Some entity ->
     assert_equal_pulled_attrs
       "pull xform transforms forward and reverse ref expansions"
       [ kw "child", Pulled_many [ Pulled_scalar (String "David"); Pulled_scalar (String "Thomas") ]
       ; kw "father", Pulled_many [ Pulled_scalar (String "David") ]
       ; kw "unknown-child", Pulled_scalar (String "missing")
       ]
       entity)

let test_pull_reverse_component_returns_single_entity () =
  let db =
    empty_db ~schema:[ "profile", component ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "profile", One_value (Ref 2) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "email", One_value (String "ivan@example.com") ] }
         ]
  in
  match pull db [ Pull_reverse_ref ("profile", [ Pull_attr "name" ]) ] (Entity_id 2) with
  | Some entity ->
    assert_equal_pulled_attrs
      "reverse component pull returns a single entity"
      [ kw "profile", Pulled_entity { pulled_id = 1; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Ivan") ] } ]
      entity
  | None -> failwith "expected pull result"

let test_pull_component_attr_expands_recursively () =
  let db =
    empty_db ~schema:[ "profile", component ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "profile", One_value (Ref 2) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "email", One_value (String "ivan@example.com"); "profile", One_value (Ref 3) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "email", One_value (String "nested@example.com") ] }
         ]
  in
  match pull db [ Pull_attr "profile" ] (Entity_id 1) with
  | Some entity ->
    assert_equal_pulled_attrs
      "component attr pull expands recursively"
      [ ( kw "profile"
        , Pulled_entity
            { pulled_id = 2
            ; pulled_attrs =
                [ kw "db/id", Pulled_scalar (Int 2)
                ; kw "email", Pulled_scalar (String "ivan@example.com")
                ; ( kw "profile"
                  , Pulled_entity
                      { pulled_id = 3
                      ; pulled_attrs =
                          [ kw "db/id", Pulled_scalar (Int 3)
                          ; kw "email", Pulled_scalar (String "nested@example.com")
                          ]
                      } )
                ]
            } )
      ]
      entity
  | None -> failwith "expected pull result"

let test_pull_component_attr_returns_id_stub_for_cycles () =
  let db =
    empty_db ~schema:[ "profile", component ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "Ivan"); "profile", One_value (Ref 2) ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "email", One_value (String "ivan@example.com"); "profile", One_value (Ref 1) ]
             }
         ]
  in
  match pull db [ Pull_attr "profile" ] (Entity_id 1) with
  | Some entity ->
    assert_equal_pulled_attrs
      "component attr pull returns id-only stubs for cycles"
      [ ( kw "profile"
        , Pulled_entity
            { pulled_id = 2
            ; pulled_attrs =
                [ kw "db/id", Pulled_scalar (Int 2)
                ; kw "email", Pulled_scalar (String "ivan@example.com")
                ; kw "profile", Pulled_entity { pulled_id = 1; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 1) ] }
                ]
            } )
      ]
      entity
  | None -> failwith "expected pull result"

let test_pull_nested_component_can_expand_reverse_component_ref () =
  let db =
    empty_db ~schema:[ "ref", component ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "1"); "ref", One_value (Ref 2) ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "2"); "ref", One_value (Ref 3) ]
             }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "3") ] }
         ]
  in
  match
    pull
      db
      [ Pull_attr "name"
      ; Pull_ref
          ( "ref"
          , [ Pull_attr "name"
            ; Pull_ref
                ( "ref"
                , [ Pull_attr "name"; Pull_as (Pull_reverse_ref ("ref", [ Pull_attr "name" ]), kw "_ref") ] )
            ] )
      ]
      (Entity_id 1)
  with
  | Some entity ->
    assert_equal_pulled_attrs
      "nested component pull can expand reverse component refs"
      [ kw "name", Pulled_scalar (String "1")
      ; ( kw "ref"
        , Pulled_entity
            { pulled_id = 2
            ; pulled_attrs =
                [ kw "name", Pulled_scalar (String "2")
                ; ( kw "ref"
                  , Pulled_entity
                      { pulled_id = 3
                      ; pulled_attrs =
                          [ ( kw "_ref"
                            , Pulled_entity
                                { pulled_id = 2; pulled_attrs = [ Keyword "name", Pulled_scalar (String "2") ] } )
                          ; kw "name", Pulled_scalar (String "3")
                          ]
                      } )
                ]
            } )
      ]
      entity
  | None -> failwith "expected pull result"

let test_pull_id_and_wildcard () =
  let db =
    empty_db ~schema:[ "aka", many; "child", ref_many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "aka", Many_values [ String "Devil"; String "Tupen" ]
                 ; "child", Many_values [ Ref 2; Ref 3 ]
                 ]
             }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "David") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Thomas") ] }
         ]
  in
  (match pull db [ Pull_id ] (Entity_id 1) with
   | None -> failwith "expected db/id pull"
   | Some entity ->
     assert_equal_pulled_attrs
       "Pull_id returns db/id"
       [ kw "db/id", Pulled_scalar (Int 1) ]
       entity);
  (match pull db [ Pull_wildcard ] (Entity_id 1) with
   | None -> failwith "expected wildcard pull"
   | Some entity ->
     assert_equal_pulled_attrs
       "Pull_wildcard returns all current attrs and shallow refs"
       [ kw "aka", Pulled_many [ Pulled_scalar (String "Devil"); Pulled_scalar (String "Tupen") ]
       ; kw "child", Pulled_many
           [ Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2) ] }
           ; Pulled_entity { pulled_id = 3; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 3) ] }
           ]
       ; kw "db/id", Pulled_scalar (Int 1)
       ; kw "name", Pulled_scalar (String "Petr")
       ]
       entity);
  (match pull_string db "[[:aka :as :alias] [:name :as :first-name] *]" (Entity_id 1) with
   | None -> failwith "expected parsed alias wildcard pull"
   | Some entity ->
     assert_equal_pulled_attrs
       "Pull_wildcard does not re-emit attrs selected with aliases"
       [ kw "alias", Pulled_many [ Pulled_scalar (String "Devil"); Pulled_scalar (String "Tupen") ]
       ; kw "child", Pulled_many
           [ Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2) ] }
           ; Pulled_entity { pulled_id = 3; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 3) ] }
           ]
       ; kw "db/id", Pulled_scalar (Int 1)
       ; kw "first-name", Pulled_scalar (String "Petr")
       ]
       entity);
  match pull db [ Pull_attr "_child" ] (Entity_id 2) with
  | None -> failwith "expected reverse shallow pull"
  | Some entity ->
    assert_equal_pulled_attrs
      "Pull_attr returns db/id stubs for shallow reverse refs"
      [ kw "_child", Pulled_many
          [ Pulled_entity { pulled_id = 1; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 1) ] } ]
      ]
      entity

let test_pull_reports_visitor_events () =
  let db =
    empty_db ~schema:[ "child", ref_many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "child", Many_values [ Ref 2; Ref 3 ]
                 ]
             }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "David") ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Thomas") ] }
         ]
  in
  let trace = ref [] in
  let visitor event = trace := event :: !trace in
  ignore (pull ~visitor db [ Pull_attr "name"; Pull_attr "_child"; Pull_wildcard ] (Entity_id 2));
  if
    List.rev !trace
    <> [ PullVisitAttr (2, "name")
       ; PullVisitReverse ("child", 2)
       ; PullVisitWildcard 2
       ]
  then
    failwith "pull should report attr, reverse, and wildcard visitor events";
  trace := [];
  ignore (pull ~visitor db [ Pull_wildcard; Pull_attr "missing" ] (Entity_id 1));
  if
    List.rev !trace
    <> [ PullVisitWildcard 1
       ; PullVisitAttr (1, "child")
       ; PullVisitAttr (1, "name")
       ; PullVisitAttr (1, "missing")
       ]
  then
    failwith "pull wildcard should report expanded attr visitor events";
  trace := [];
  ignore (pull ~visitor db [ Pull_ref ("child", [ Pull_attr "name" ]) ] (Entity_id 1));
  if List.rev !trace <> [ PullVisitAttr (1, "child"); PullVisitAttr (2, "name"); PullVisitAttr (3, "name") ] then
    failwith "pull visitor should report nested ref traversal events";
  trace := [];
  ignore (pull_string ~visitor db "[:name {:child [:name]}]" (Entity_id 1));
  if List.rev !trace <> [ PullVisitAttr (1, "name"); PullVisitAttr (1, "child"); PullVisitAttr (2, "name"); PullVisitAttr (3, "name") ] then
    failwith "pull_string should forward visitor events";
  trace := [];
  ignore (pull_many_string ~visitor db "[:name]" [ Entity_id 2; Entity_id 3 ]);
  if List.rev !trace <> [ PullVisitAttr (2, "name"); PullVisitAttr (3, "name") ] then
    failwith "pull_many_string should forward visitor events"

let test_pull_recursive_ref_with_depth_limit () =
  let db =
    empty_db ~schema:[ "part", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "Part A"); "part", Many_values [ Ref 2; Ref 3 ] ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "Part A.A"); "part", Many_values [ Ref 4 ] ]
             }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Part A.B") ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "name", One_value (String "Part A.A.A") ] }
         ]
  in
  match pull db [ Pull_attr "name"; Pull_recursive_ref ("part", [ Pull_attr "name" ], Some 2) ] (Entity_id 1) with
  | None -> failwith "expected recursive pull"
  | Some entity ->
    assert_equal_pulled_attrs
      "recursive pull respects depth limit"
      [ kw "name", Pulled_scalar (String "Part A")
      ; kw "part", Pulled_many
          [ Pulled_entity
              { pulled_id = 2
              ; pulled_attrs =
                  [ kw "name", Pulled_scalar (String "Part A.A")
                  ; kw "part", Pulled_many
                      [ Pulled_entity
                          { pulled_id = 4
                          ; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Part A.A.A") ]
                          }
                      ]
                  ]
              }
          ; Pulled_entity
              { pulled_id = 3
              ; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Part A.B") ]
              }
          ]
      ]
      entity

let test_pull_recursive_ref_avoids_cycles () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "A"); "part", One_value (Ref 2) ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "B"); "part", One_value (Ref 1) ]
             }
         ]
  in
  match pull db [ Pull_attr "name"; Pull_recursive_ref ("part", [ Pull_attr "name" ], None) ] (Entity_id 1) with
  | None -> failwith "expected recursive pull"
  | Some entity ->
    assert_equal_pulled_attrs
      "recursive pull expands the seen root once before returning id stubs"
      [ kw "name", Pulled_scalar (String "A")
      ; kw "part", Pulled_entity
          { pulled_id = 2
          ; pulled_attrs =
              [ kw "name", Pulled_scalar (String "B")
              ; kw "part", Pulled_entity
                  { pulled_id = 1
                  ; pulled_attrs =
                      [ kw "name", Pulled_scalar (String "A")
                      ; kw "part", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2) ] }
                      ]
                  }
              ]
          }
      ]
      entity

let test_pull_recursive_refs_share_pattern_context () =
  let db =
    empty_db ~schema:[ "part", ref_attr; "spec", ref_attr ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "name", One_value (String "Root"); "part", One_value (Ref 2) ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "name", One_value (String "Part"); "spec", One_value (Ref 3) ]
             }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "name", One_value (String "Spec") ] }
         ]
  in
  match
    pull
      db
      [ Pull_attr "name"
      ; Pull_recursive_ref ("part", [ Pull_attr "name" ], None)
      ; Pull_recursive_ref ("spec", [ Pull_attr "name" ], None)
      ]
      (Entity_id 1)
  with
  | None -> failwith "expected recursive pull"
  | Some entity ->
    assert_equal_pulled_attrs
      "recursive pull keeps sibling recursive branches in child entities"
      [ kw "name", Pulled_scalar (String "Root")
      ; kw "part", Pulled_entity
          { pulled_id = 2
          ; pulled_attrs =
              [ kw "name", Pulled_scalar (String "Part")
              ; kw "spec", Pulled_entity
                  { pulled_id = 3; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Spec") ] }
              ]
          }
      ]
      entity

let test_pull_recursive_ref_depth_preserves_sibling_context () =
  let db =
    empty_db ~schema:[ "friend", ref_attr; "enemy", ref_attr ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "friend", One_value (Ref 2) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "enemy", One_value (Ref 3) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "friend", One_value (Ref 4) ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "name", One_value (String "Leaf") ] }
         ]
  in
  match
    pull
      db
      [ Pull_id
      ; Pull_recursive_ref ("friend", [ Pull_id ], Some 2)
      ; Pull_recursive_ref ("enemy", [ Pull_id ], Some 1)
      ]
      (Entity_id 1)
  with
  | None -> failwith "expected recursive pull"
  | Some entity ->
    assert_equal_pulled_attrs
      "exhausting one recursive attr depth should preserve sibling recursive attrs"
      [ kw "db/id", Pulled_scalar (Int 1)
      ; kw "friend", Pulled_entity
          { pulled_id = 2
          ; pulled_attrs =
              [ kw "db/id", Pulled_scalar (Int 2)
              ; kw "enemy", Pulled_entity
                  { pulled_id = 3
                  ; pulled_attrs =
                      [ kw "db/id", Pulled_scalar (Int 3)
                      ; kw "friend", Pulled_entity
                          { pulled_id = 4; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 4) ] }
                      ]
                  }
              ]
          }
      ]
      entity

let test_pull_dual_recursion_respects_independent_depths () =
  let db =
    empty_db ~schema:[ "friend", ref_attr; "enemy", ref_attr ] ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "friend", One_value (Ref 2) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "enemy", One_value (Ref 3) ] }
         ; Entity { db_id = Some (Entity_id 3); attrs = [ "friend", One_value (Ref 4) ] }
         ; Entity { db_id = Some (Entity_id 4); attrs = [ "enemy", One_value (Ref 5) ] }
         ; Entity { db_id = Some (Entity_id 5); attrs = [ "friend", One_value (Ref 6) ] }
         ; Entity { db_id = Some (Entity_id 6); attrs = [ "enemy", One_value (Ref 7) ] }
         ]
  in
  (match
     pull
       db
       [ Pull_id
       ; Pull_recursive_ref ("friend", [ Pull_id ], None)
       ]
       (Entity_id 1)
   with
   | None -> failwith "expected recursive pull"
   | Some entity ->
     assert_equal_pulled_attrs
       "unbounded recursion follows only the selected recursive attr"
       [ kw "db/id", Pulled_scalar (Int 1)
       ; kw "friend", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2) ] }
       ]
       entity);
  (match
     pull
       db
       [ Pull_id
       ; Pull_recursive_ref ("friend", [ Pull_id ], Some 1)
       ; Pull_recursive_ref ("enemy", [ Pull_id ], Some 1)
       ]
       (Entity_id 1)
   with
   | None -> failwith "expected recursive pull"
   | Some entity ->
     assert_equal_pulled_attrs
       "dual recursion follows each attr at depth one"
       [ kw "db/id", Pulled_scalar (Int 1)
       ; kw "friend", Pulled_entity
           { pulled_id = 2
           ; pulled_attrs =
               [ kw "db/id", Pulled_scalar (Int 2)
               ; kw "enemy", Pulled_entity { pulled_id = 3; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 3) ] }
               ]
           }
       ]
       entity);
  match
    pull
      db
      [ Pull_id
      ; Pull_recursive_ref ("friend", [ Pull_id ], Some 2)
      ; Pull_recursive_ref ("enemy", [ Pull_id ], Some 2)
      ]
      (Entity_id 1)
  with
  | None -> failwith "expected recursive pull"
  | Some entity ->
    assert_equal_pulled_attrs
      "dual recursion tracks attr depths independently"
      [ kw "db/id", Pulled_scalar (Int 1)
      ; kw "friend", Pulled_entity
          { pulled_id = 2
          ; pulled_attrs =
              [ kw "db/id", Pulled_scalar (Int 2)
              ; kw "enemy", Pulled_entity
                  { pulled_id = 3
                  ; pulled_attrs =
                      [ kw "db/id", Pulled_scalar (Int 3)
                      ; kw "friend", Pulled_entity
                          { pulled_id = 4
                          ; pulled_attrs =
                              [ kw "db/id", Pulled_scalar (Int 4)
                              ; kw "enemy", Pulled_entity
                                  { pulled_id = 5; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 5) ] }
                              ]
                          }
                      ]
                  }
              ]
          }
      ]
      entity

let test_pull_dual_recursion_tracks_cycles_per_branch () =
  let db =
    empty_db ~schema:[ "part", ref_attr; "spec", ref_attr ] ()
    |> db_with
         [ Add (Entity_id 1, "part", Ref 2)
         ; Add (Entity_id 2, "part", Ref 3)
         ; Add (Entity_id 3, "part", Ref 1)
         ; Add (Entity_id 1, "spec", Ref 2)
         ; Add (Entity_id 2, "spec", Ref 1)
         ]
  in
  match
    pull
      db
      [ Pull_id
      ; Pull_recursive_ref ("part", [ Pull_id ], None)
      ; Pull_recursive_ref ("spec", [ Pull_id ], None)
      ]
      (Entity_id 1)
  with
  | None -> failwith "expected recursive pull"
  | Some entity ->
    assert_equal_pulled_attrs
      "dual recursion tracks seen ids independently for sibling branches"
      [ kw "db/id", Pulled_scalar (Int 1)
      ; kw "part", Pulled_entity
          { pulled_id = 2
          ; pulled_attrs =
              [ kw "db/id", Pulled_scalar (Int 2)
              ; kw "part", Pulled_entity
                  { pulled_id = 3
                  ; pulled_attrs =
                      [ kw "db/id", Pulled_scalar (Int 3)
                      ; kw "part", Pulled_entity
                          { pulled_id = 1
                          ; pulled_attrs =
                              [ kw "db/id", Pulled_scalar (Int 1)
                              ; kw "part", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2) ] }
                              ; kw "spec", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2) ] }
                              ]
                          }
                      ]
                  }
              ; kw "spec", Pulled_entity
                  { pulled_id = 1
                  ; pulled_attrs =
                      [ kw "db/id", Pulled_scalar (Int 1)
                      ; kw "part", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2) ] }
                      ; kw "spec", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2) ] }
                      ]
                  }
              ]
          }
      ; kw "spec", Pulled_entity
          { pulled_id = 2
          ; pulled_attrs =
              [ kw "db/id", Pulled_scalar (Int 2)
              ; kw "part", Pulled_entity
                  { pulled_id = 3
                  ; pulled_attrs =
                      [ kw "db/id", Pulled_scalar (Int 3)
                      ; kw "part", Pulled_entity
                          { pulled_id = 1
                          ; pulled_attrs =
                              [ kw "db/id", Pulled_scalar (Int 1)
                              ; kw "part", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2) ] }
                              ; kw "spec", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2) ] }
                              ]
                          }
                      ]
                  }
              ; kw "spec", Pulled_entity
                  { pulled_id = 1
                  ; pulled_attrs =
                      [ kw "db/id", Pulled_scalar (Int 1)
                      ; kw "part", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2) ] }
                      ; kw "spec", Pulled_entity { pulled_id = 2; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 2) ] }
                      ]
                  }
              ]
          }
      ]
      entity

let test_pull_deep_recursion_reaches_leaf () =
  let start = 100 in
  let depth = 3000 in
  let rec build value acc =
    if value >= depth then
      acc
    else
      build
        (value + 1)
        (Add (Entity_id value, "name", String ("Person-" ^ string_of_int value))
         :: Add (Entity_id (value - 1), "friend", Ref value)
         :: acc)
  in
  let db =
    empty_db ~schema:[ "friend", ref_attr ] ()
    |> db_with
         (Add (Entity_id start, "name", String ("Person-" ^ string_of_int start))
          :: build (start + 1) [])
  in
  let rec last_friend_name remaining = function
    | Pulled_entity entity when remaining = 0 ->
      (match List.assoc_opt (kw "name") entity.pulled_attrs with
       | Some (Pulled_scalar (String name)) -> Some name
       | _ -> None)
    | Pulled_entity entity ->
      (match List.assoc_opt (kw "friend") entity.pulled_attrs with
       | Some next -> last_friend_name (remaining - 1) next
       | None -> None)
    | Pulled_scalar _ | Pulled_many _ -> None
  in
  match pull db [ Pull_attr "name"; Pull_recursive_ref ("friend", [ Pull_attr "name" ], None) ] (Entity_id start) with
  | None -> failwith "expected deep recursive pull"
  | Some entity ->
    let edge_count = depth - start - 1 in
    let expected = "Person-" ^ string_of_int (depth - 1) in
    (match last_friend_name edge_count (Pulled_entity entity) with
     | Some actual when actual = expected -> ()
     | Some actual -> failf "deep recursive pull expected %s, got %s" expected actual
     | None -> failwith "deep recursive pull should reach the final leaf")

let test_pull_recursive_reverse_ref () =
  let db =
    empty_db ~schema:[ "friend", ref_many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 4)
             ; attrs = [ "friend", Many_values [ Ref 5 ]; "name", One_value (String "Lucy") ]
             }
         ; Entity
             { db_id = Some (Entity_id 5)
             ; attrs = [ "friend", Many_values [ Ref 6 ]; "name", One_value (String "Elizabeth") ]
             }
         ; Entity
             { db_id = Some (Entity_id 6)
             ; attrs = [ "friend", Many_values [ Ref 7 ]; "name", One_value (String "Matthew") ]
             }
         ; Entity
             { db_id = Some (Entity_id 7)
             ; attrs = [ "friend", Many_values [ Ref 8 ]; "name", One_value (String "Eunan") ]
             }
         ; Entity { db_id = Some (Entity_id 8); attrs = [ "name", One_value (String "Kerri") ] }
         ]
  in
  (match pull db [ Pull_id; Pull_recursive_ref ("_friend", [ Pull_id ], None) ] (Entity_id 8) with
   | None -> failwith "expected reverse recursive pull"
   | Some entity ->
     assert_equal_pulled_attrs
       "recursive pull follows reverse refs"
       [ kw "_friend", Pulled_many
           [ Pulled_entity
               { pulled_id = 7
               ; pulled_attrs =
                   [ kw "_friend", Pulled_many
                       [ Pulled_entity
                           { pulled_id = 6
                           ; pulled_attrs =
                               [ kw "_friend", Pulled_many
                                   [ Pulled_entity
                                       { pulled_id = 5
                                       ; pulled_attrs =
                                           [ kw "_friend", Pulled_many
                                               [ Pulled_entity
                                                   { pulled_id = 4
                                                   ; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 4) ]
                                                   }
                                               ]
                                           ; kw "db/id", Pulled_scalar (Int 5)
                                           ]
                                       }
                                   ]
                               ; kw "db/id", Pulled_scalar (Int 6)
                               ]
                           }
                       ]
                   ; kw "db/id", Pulled_scalar (Int 7)
                   ]
               }
           ]
       ; kw "db/id", Pulled_scalar (Int 8)
       ]
       entity);
  match pull db [ Pull_id; Pull_recursive_ref ("_friend", [ Pull_id ], Some 2) ] (Entity_id 8) with
  | None -> failwith "expected reverse recursive pull"
  | Some entity ->
    assert_equal_pulled_attrs
      "recursive pull limits reverse refs"
      [ kw "_friend", Pulled_many
          [ Pulled_entity
              { pulled_id = 7
              ; pulled_attrs =
                  [ kw "_friend", Pulled_many
                      [ Pulled_entity
                          { pulled_id = 6
                          ; pulled_attrs = [ Keyword "db/id", Pulled_scalar (Int 6) ]
                          }
                      ]
                  ; kw "db/id", Pulled_scalar (Int 7)
                  ]
              }
          ]
      ; kw "db/id", Pulled_scalar (Int 8)
      ]
      entity

let test_pull_many_preserves_missing_entities () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan") ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ]
  in
  let names =
    pull_many db [ Pull_attr "name" ] [ Entity_id 2; Entity_id 99; Entity_id 1 ]
    |> List.map (function
      | Some entity -> Some (entity.pulled_id, List.assoc_opt (kw "name") entity.pulled_attrs)
      | None -> None)
  in
  if
    names
    <> [ Some (2, Some (Pulled_scalar (String "Petr")))
       ; None
       ; Some (1, Some (Pulled_scalar (String "Ivan")))
       ]
  then failwith "pull_many should preserve requested order and missing entities"

let test_pull_reads_filtered_serialized_and_reinitialized_dbs () =
  let schema = [ "name", unique_identity; "aka", many ] in
  let db =
    empty_db ~schema ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "aka", Many_values [ String "Devil"; String "Tupen" ]
                 ]
             }
         ]
  in
  let assert_petr label expected_aka db =
    match pull db [ Pull_attr "name"; Pull_attr "aka" ] (Entity_id 1) with
    | None -> failf "%s should pull Petr" label
    | Some entity ->
      assert_equal_pulled_attrs
        label
        [ kw "aka", Pulled_many (List.map (fun value -> Pulled_scalar (String value)) expected_aka)
        ; kw "name", Pulled_scalar (String "Petr")
        ]
        entity
  in
  let filtered = filter db (fun _ datom -> datom.v <> String "Tupen") in
  assert_petr "pull reads filtered db views" [ "Devil" ] filtered;
  let restored = db |> serializable |> from_serializable in
  assert_petr "pull reads serialized and restored dbs" [ "Devil"; "Tupen" ] restored;
  let reinitialized = init_db ~schema (datoms db Eavt ()) in
  assert_petr "pull reads dbs reinitialized from datoms" [ "Devil"; "Tupen" ] reinitialized

let test_filter_limits_read_apis () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "active", One_value (Bool true) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "active", One_value (Bool false) ] }
         ]
  in
  let filtered = filter db (fun _ datom -> datom.e = 1) in
  assert_bool "filter marks db as filtered" (is_filtered filtered);
  assert_equal_triples
    "filtered datoms only include matching datoms"
    [ 1, "active", Bool true; 1, "name", String "Ivan" ]
    (datoms filtered Eavt ());
  (match entity filtered (Entity_id 2) with
   | None -> ()
   | Some _ -> failwith "filtered entity should hide filtered-out datoms");
  (match pull filtered [ Pull_attr "name" ] (Entity_id 1) with
   | Some entity ->
     assert_equal_pulled_attrs
       "filtered pull reads visible attrs"
       [ kw "name", Pulled_scalar (String "Ivan") ]
       entity
   | None -> failwith "filtered pull should find visible entity");
  let query =
    { find = [ Find_var "name" ]
    ; inputs = []
    ; with_vars = []
    ; rules = []
    ; where = [ Pattern (QWildcard, QAttr "name", QVar "name") ]
    }
  in
  assert_equal_query
    "filtered query only sees visible datoms"
    [ [ Result_value (String "Ivan") ] ]
    (q filtered query)

let test_filter_composes_and_rejects_writes () =
  let db =
    empty_db ()
    |> db_with
         [ Entity { db_id = Some (Entity_id 1); attrs = [ "name", One_value (String "Ivan"); "active", One_value (Bool true) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr"); "active", One_value (Bool true) ] }
         ]
  in
  let filtered =
    filter db (fun _ datom -> datom.a = "name")
    |> fun db -> filter db (fun _ datom -> datom.e = 2)
  in
  assert_equal_triples
    "filtered db composes predicates"
    [ 2, "name", String "Petr" ]
    (datoms filtered Eavt ());
  assert_equal_triples
    "unfiltered_db restores the full active datom set"
    [ 1, "active", Bool true; 1, "name", String "Ivan"; 2, "active", Bool true; 2, "name", String "Petr" ]
    (datoms (unfiltered_db filtered) Eavt ());
  assert_raises_invalid_arg
    "db_with rejects filtered db writes"
    (fun () -> ignore (db_with [ Add (Entity_id 3, "name", String "Oleg") ] filtered))

let test_filter_predicates_read_unfiltered_db_like_upstream () =
  let db =
    empty_db ~schema:[ "aka", many ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "aka", Many_values [ String "I"; String "Great" ]
                 ; "password", One_value (String "<SECRET>")
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "aka", Many_values [ String "Terrible"; String "IV" ]
                 ; "password", One_value (String "<PROTECTED>")
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 3)
             ; attrs =
                 [ "name", One_value (String "Nikolai")
                 ; "aka", Many_values [ String "II" ]
                 ; "password", One_value (String "<UNKNOWN>")
                 ]
             }
         ]
  in
  let aka_count db entity_id =
    match entity db (Entity_id entity_id) with
    | Some entity ->
      (match entity_attr entity "aka" with
       | Some (Many_values values) -> List.length values
       | Some (One_value _) -> 1
       | Some (One_entity _) -> 1
       | Some (Many_entities values) -> List.length values
       | None -> 0)
    | None -> 0
  in
  let remove_ivan _ datom = datom.e <> 2 in
  let long_akas udb datom =
    datom.a <> "aka" || aka_count udb datom.e <= 1 ||
    (match datom.v with
     | String value -> String.length value >= 4
     | _ -> false)
  in
  let aka_query = "[:find ?v :where [_ :aka ?v]]" in
  assert_equal_query_set
    "filter predicate can read unfiltered entity attrs"
    [ [ Result_value (String "Great") ]
    ; [ Result_value (String "Terrible") ]
    ; [ Result_value (String "II") ]
    ]
    (q_string (filter db long_akas) aka_query);
  assert_equal_query_set
    "composed filter predicates read the same unfiltered base"
    [ [ Result_value (String "Great") ]; [ Result_value (String "II") ] ]
    (q_string (filter db remove_ivan |> fun db -> filter db long_akas) aka_query)

let test_filter_and_entity_upstream_edge_parity_batch () =
  let db =
    empty_db ~schema:[ "name", unique_identity; "aka", many; "age", indexed; "password", indexed ] ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs =
                 [ "name", One_value (String "Petr")
                 ; "age", One_value (Int 44)
                 ; "aka", Many_values [ String "I"; String "Great" ]
                 ; "password", One_value (String "<SECRET>")
                 ; "huh?", One_value (Bool false)
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs =
                 [ "name", One_value (String "Ivan")
                 ; "age", One_value (Int 25)
                 ; "aka", Many_values [ String "Terrible"; String "IV" ]
                 ; "password", One_value (String "<PROTECTED>")
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 3)
             ; attrs =
                 [ "name", One_value (String "Nikolai")
                 ; "age", One_value (Int 7)
                 ; "aka", Many_values [ String "II" ]
                 ; "password", One_value (String "<UNKNOWN>")
                 ]
             }
         ]
  in
  let query attr = Printf.sprintf "[:find ?v :where [_ :%s ?v]]" attr in
  let name_query = query "name" in
  let aka_query = query "aka" in
  let password_query = query "password" in
  let names db = q_string db name_query in
  let remove_pass _ datom = datom.a <> "password" in
  let remove_ivan _ datom = datom.e <> 2 in
  let age_of db entity_id =
    match entity db (Entity_id entity_id) with
    | Some entity ->
      (match entity_attr entity "age" with
       | Some (One_value (Int age)) -> Some age
       | _ -> None)
    | None -> None
  in
  let aka_count db entity_id =
    match entity db (Entity_id entity_id) with
    | Some entity ->
      (match entity_attr entity "aka" with
       | Some (Many_values values) -> List.length values
       | Some (One_value _) -> 1
       | _ -> 0)
    | None -> 0
  in
  let has_age unfiltered_db datom = Option.is_some (age_of unfiltered_db datom.e) in
  let adult unfiltered_db datom =
    match age_of unfiltered_db datom.e with
    | Some age -> age >= 18
    | None -> false
  in
  let long_akas unfiltered_db datom =
    datom.a <> "aka" || aka_count unfiltered_db datom.e <= 1 ||
    match datom.v with
    | String value -> String.length value >= 4
    | _ -> false
  in
  assert_equal_query_set
    "filter remove-pass hides password values from queries"
    []
    (q_string (filter db remove_pass) password_query);
  assert_equal_triples
    "filter remove-pass hides password values from AVET index reads"
    []
    (datoms (filter db remove_pass) Avet ~a:"password" ());
  assert_equal_query_set
    "filter remove-ivan hides one entity across query results"
    [ [ Result_value (String "Petr") ]; [ Result_value (String "Nikolai") ] ]
    (names (filter db remove_ivan));
  assert_equal_triples
    "filter remove-ivan hides one entity across direct EAVT reads"
    []
    (datoms (filter db remove_ivan) Eavt ~e:2 ());
  assert_equal_query_set
    "filter long-akas can inspect the unfiltered db"
    [ [ Result_value (String "Great") ]
    ; [ Result_value (String "Terrible") ]
    ; [ Result_value (String "II") ]
    ]
    (q_string (filter db long_akas) aka_query);
  assert_equal_query_set
    "composed filters still pass the unfiltered db to later predicates"
    [ [ Result_value (String "Great") ]; [ Result_value (String "II") ] ]
    (q_string (filter db remove_ivan |> fun db -> filter db long_akas) aka_query);
  assert_equal_query_set
    "double filters support predicates that inspect entity attrs"
    [ [ Result_value (String "Petr") ]; [ Result_value (String "Ivan") ] ]
    (names (filter db has_age |> fun db -> filter db adult));
  (match entity (filter db remove_pass) (Entity_id 1) with
   | None -> failwith "filtered db should still expose visible entity attrs"
   | Some entity ->
     assert_equal_tx_value
       "filtered entity hides removed attr"
       None
       (entity_attr entity "password");
     assert_equal_tx_value
       "filtered entity preserves false values"
       (Some (One_value (Bool false)))
       (entity_attr entity "huh?"));
  (match entity db (Lookup_ref ("name", String "missing")) with
   | None -> ()
   | Some _ -> failwith "missing unique lookup-ref entity should return None");
  (match pull db [ Pull_attr "huh?" ] (Entity_id 1) with
   | Some entity ->
     assert_equal_pulled_attrs
       "pull preserves false scalar values"
       [ kw "huh?", Pulled_scalar (Bool false) ]
       entity
   | None -> failwith "expected pull to find entity with false value")

let test_schema_and_with_schema () =
  let db =
    empty_db ~schema:[ "name", unique_identity ] ()
    |> db_with [ Add (Entity_id 1, "name", String "Ivan") ]
  in
  if schema db <> [ "name", unique_identity ] then failwith "schema should return db schema";
  let db = with_schema db [ "tag", many ] in
  if schema db <> [ "tag", many ] then failwith "with_schema should replace schema";
  let db = db_with [ Add (Entity_id 1, "tag", String "a"); Add (Entity_id 1, "tag", String "b") ] db in
  assert_equal_triples
    "with_schema changes subsequent cardinality behavior"
    [ 1, "name", String "Ivan"; 1, "tag", String "a"; 1, "tag", String "b" ]
    (datoms db Eavt ())

let test_schema_transactions_install_schema_attrs () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 100)
             ; attrs =
                 [ "db/ident", One_value (Keyword "aka")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/many")
                 ; "db/index", One_value (Bool true)
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 101)
             ; attrs =
                 [ "db/ident", One_value (Keyword "friend")
                 ; "db/valueType", One_value (Keyword "db.type/ref")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                 ; "db/isComponent", One_value (Bool true)
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 102)
             ; attrs =
                 [ "db/ident", One_value (Keyword "email")
                 ; "db/unique", One_value (Keyword "db.unique/identity")
                 ]
             }
         ]
  in
  if List.assoc_opt "aka" (schema db) <> Some { indexed with cardinality = Many } then
    failwith "schema transaction should install cardinality and index";
  if List.assoc_opt "friend" (schema db) <> Some component then
    failwith "schema transaction should install ref component attrs";
  if List.assoc_opt "email" (schema db) <> Some unique_identity then
    failwith "schema transaction should install unique identity attrs";
  let db =
    db
    |> db_with
         [ Add (Entity_id 1, "aka", String "Ivan")
         ; Add (Entity_id 1, "aka", String "Vanya")
         ; Add (Entity_id 1, "friend", Ref 2)
         ; Add (Entity_id 2, "email", String "petr@example.com")
         ]
  in
  assert_equal_triples
    "installed schema affects following transactions"
    [ 1, "aka", String "Ivan"
    ; 1, "aka", String "Vanya"
    ; 1, "friend", Ref 2
    ]
    (datoms db Eavt ~e:1 ());
  assert_equal_triples
    "installed unique identity schema stores indexed values"
    [ 2, "email", String "petr@example.com"
    ]
    (datoms db Eavt ~e:2 ());
  if entid db "email" (String "petr@example.com") <> Some 2 then
    failwith "installed unique identity schema should support entid"

let test_schema_accepts_db_type_alias () =
  let parsed_schema = schema_of_edn_string "{:friend {:db/type :db.type/ref}}" in
  if List.assoc_opt "friend" parsed_schema <> Some ref_attr then
    failwith "schema_of_edn_string should treat :db/type as :db/valueType";
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 100)
             ; attrs =
                 [ "db/ident", One_value (Keyword "friend")
                 ; "db/type", One_value (Keyword "db.type/ref")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                 ]
             }
         ; Entity { db_id = Some (Entity_id 1); attrs = [ "friend", One_value (Int 2) ] }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "name", One_value (String "Petr") ] }
         ]
  in
  if List.assoc_opt "friend" (schema db) <> Some ref_attr then
    failwith "schema transactions should treat db/type as db/valueType";
  assert_equal_triples
    "db/type schema alias should normalize ref values"
    [ 1, "friend", Ref 2 ]
    (datoms db Eavt ~e:1 ~a:"friend" ())

let test_schema_transactions_install_tuple_attrs () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 200)
             ; attrs =
                 [ "db/ident", One_value (Keyword "a+b")
                 ; "db/valueType", One_value (Keyword "db.type/tuple")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                 ; "db/tupleAttrs", Many_values [ Keyword "a"; Keyword "b" ]
                 ; "db/index", One_value (Bool true)
                 ]
             }
         ]
  in
  if List.assoc_opt "a+b" (schema db) <> Some (tuple [ "a"; "b" ]) then
    failwith "schema transaction should install tuple attrs";
  let db =
    db
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "a", One_value (String "A"); "b", One_value (String "B") ]
             }
         ]
  in
  assert_equal_triples
    "tuple attrs installed by schema transaction are maintained"
    [ 1, "a", String "A"
    ; 1, "a+b", Tuple [ Some (String "A"); Some (String "B") ]
    ; 1, "b", String "B"
    ]
    (datoms db Eavt ~e:1 ())

let test_schema_transactions_affect_same_transaction () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 300)
             ; attrs =
                 [ "db/ident", One_value (Keyword "aka")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/many")
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 301)
             ; attrs =
                 [ "db/ident", One_value (Keyword "friend")
                 ; "db/valueType", One_value (Keyword "db.type/ref")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                 ]
             }
         ; Add (Entity_id 1, "aka", String "Ivan")
         ; Add (Entity_id 1, "aka", String "Vanya")
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "_friend", One_value (Ref 1); "name", One_value (String "Petr") ]
             }
         ]
  in
  if List.assoc_opt "aka" (schema db) <> Some { many with indexed = false } then
    failwith "same transaction should install cardinality many schema";
  if List.assoc_opt "friend" (schema db) <> Some ref_attr then
    failwith "same transaction should install ref schema";
  assert_equal_triples
    "schema installed in a transaction affects later ops in the same transaction"
    [ 1, "aka", String "Ivan"
    ; 1, "aka", String "Vanya"
    ; 1, "friend", Ref 2
    ]
    (datoms db Eavt ~e:1 ())

let test_schema_add_datoms_affect_same_transaction () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 400, "db/ident", Keyword "aka")
         ; Add (Entity_id 400, "db/cardinality", Keyword "db.cardinality/many")
         ; Add (Entity_id 401, "db/ident", Keyword "friend")
         ; Add (Entity_id 401, "db/valueType", Keyword "db.type/ref")
         ; Add (Entity_id 401, "db/cardinality", Keyword "db.cardinality/one")
         ; Add (Entity_id 1, "aka", String "Ivan")
         ; Add (Entity_id 1, "aka", String "Vanya")
         ; Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "_friend", One_value (Ref 1); "name", One_value (String "Petr") ]
             }
         ]
  in
  if List.assoc_opt "aka" (schema db) <> Some { many with indexed = false } then
    failwith "Add schema datoms should install cardinality many schema";
  if List.assoc_opt "friend" (schema db) <> Some ref_attr then
    failwith "Add schema datoms should install ref schema";
  assert_equal_triples
    "schema installed by Add datoms affects later ops in the same transaction"
    [ 1, "aka", String "Ivan"
    ; 1, "aka", String "Vanya"
    ; 1, "friend", Ref 2
    ]
    (datoms db Eavt ~e:1 ())

let test_later_schema_entities_do_not_affect_earlier_ops () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "aka", String "Ivan")
         ; Add (Entity_id 1, "aka", String "Vanya")
         ; Entity
             { db_id = Some (Entity_id 500)
             ; attrs =
                 [ "db/ident", One_value (Keyword "aka")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/many")
                 ]
             }
         ]
  in
  if List.assoc_opt "aka" (schema db) <> Some { many with indexed = false } then
    failwith "later schema entity should still install schema";
  assert_equal_triples
    "later schema entity must not change earlier cardinality-one replacement"
    [ 1, "aka", String "Vanya" ]
    (datoms db Eavt ~e:1 ())

let test_later_schema_add_datoms_do_not_affect_earlier_ops () =
  let db =
    empty_db ()
    |> db_with
         [ Add (Entity_id 1, "aka", String "Ivan")
         ; Add (Entity_id 1, "aka", String "Vanya")
         ; Add (Entity_id 501, "db/ident", Keyword "aka")
         ; Add (Entity_id 501, "db/cardinality", Keyword "db.cardinality/many")
         ]
  in
  if List.assoc_opt "aka" (schema db) <> Some { many with indexed = false } then
    failwith "later schema datoms should still install schema";
  assert_equal_triples
    "later schema datoms must not change earlier cardinality-one replacement"
    [ 1, "aka", String "Vanya" ]
    (datoms db Eavt ~e:1 ())

let test_schema_retractions_affect_later_ops () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 510)
             ; attrs =
                 [ "db/ident", One_value (Keyword "aka")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/many")
                 ]
             }
         ]
    |> db_with
         [ Retract (Entity_id 510, "db/cardinality", Some (Keyword "db.cardinality/many"))
         ; Add (Entity_id 1, "aka", String "Ivan")
         ; Add (Entity_id 1, "aka", String "Vanya")
         ]
  in
  if List.assoc_opt "aka" (schema db) <> None then
    failwith "schema retraction should remove attr schema when no schema fields remain";
  assert_equal_triples
    "schema retraction must affect later cardinality behavior"
    [ 1, "aka", String "Vanya" ]
    (datoms db Eavt ~e:1 ())

let test_schema_ident_retraction_removes_schema_attr () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 511)
             ; attrs =
                 [ "db/ident", One_value (Keyword "friend")
                 ; "db/valueType", One_value (Keyword "db.type/ref")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                 ]
             }
         ]
    |> db_with
         [ Retract (Entity_id 511, "db/ident", Some (Keyword "friend"))
         ; Add (Entity_id 1, "friend", String "not-a-ref")
         ]
  in
  if List.assoc_opt "friend" (schema db) <> None then
    failwith "db/ident retraction should remove the schema attr";
  assert_equal_triples
    "db/ident retraction must affect later value validation"
    [ 1, "friend", String "not-a-ref" ]
    (datoms db Eavt ~e:1 ())

let test_schema_entity_retractions_remove_schema_attrs () =
  let base =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 512)
             ; attrs =
                 [ "db/ident", One_value (Keyword "friend")
                 ; "db/valueType", One_value (Keyword "db.type/ref")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 513)
             ; attrs =
                 [ "db/ident", One_value (Keyword "aka")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/many")
                 ]
             }
         ]
  in
  let db =
    base
    |> db_with
         [ RetractEntity (Entity_id 512)
         ; Add (Entity_id 1, "friend", String "not-a-ref")
         ]
  in
  if List.assoc_opt "friend" (schema db) <> None then
    failwith "RetractEntity should remove schema attrs";
  assert_equal_triples
    "RetractEntity schema removal must affect later validation"
    [ 1, "friend", String "not-a-ref" ]
    (datoms db Eavt ~e:1 ());
  let db =
    base
    |> db_with
         [ RetractAttr (Entity_id 513, "db/ident")
         ; Add (Entity_id 2, "aka", String "Ivan")
         ; Add (Entity_id 2, "aka", String "Vanya")
         ]
  in
  if List.assoc_opt "aka" (schema db) <> None then
    failwith "RetractAttr db/ident should remove schema attrs";
  assert_equal_triples
    "RetractAttr db/ident schema removal must affect later cardinality"
    [ 2, "aka", String "Vanya" ]
    (datoms db Eavt ~e:2 ())

let test_schema_field_retractions_drop_stale_field_values () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 514)
             ; attrs =
                 [ "db/ident", One_value (Keyword "friend")
                 ; "db/valueType", One_value (Keyword "db.type/ref")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                 ]
             }
         ]
    |> db_with
         [ Retract (Entity_id 514, "db/valueType", Some (Keyword "db.type/ref"))
         ; Add (Entity_id 1, "friend", String "not-a-ref")
         ]
  in
  if List.assoc_opt "friend" (schema db) <> Some { indexed with indexed = false } then
    failwith "db/valueType retraction should remove stale ref type from schema";
  assert_equal_triples
    "db/valueType retraction must affect later value validation"
    [ 1, "friend", String "not-a-ref" ]
    (datoms db Eavt ~e:1 ())

let test_schema_unique_retraction_allows_later_duplicates () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 515)
             ; attrs =
                 [ "db/ident", One_value (Keyword "email")
                 ; "db/unique", One_value (Keyword "db.unique/value")
                 ]
             }
         ; Add (Entity_id 1, "email", String "same@example.com")
         ]
    |> db_with
         [ Retract (Entity_id 515, "db/unique", Some (Keyword "db.unique/value"))
         ; Add (Entity_id 2, "email", String "same@example.com")
         ]
  in
  let triples = datoms db Eavt ~a:"email" () |> List.map (fun d -> d.e, d.a, d.v) in
  if triples <> [ 1, "email", String "same@example.com"; 2, "email", String "same@example.com" ] then
    failf
      "db/unique retraction must allow later duplicate values: got %d datoms, first entity %d"
      (List.length triples)
      (match triples with
       | (entity_id, _, _) :: _ -> entity_id
       | [] -> -1)

let test_schema_index_retraction_removes_later_avet_access () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 516)
             ; attrs =
                 [ "db/ident", One_value (Keyword "email")
                 ; "db/index", One_value (Bool true)
                 ]
             }
         ; Add (Entity_id 1, "email", String "first@example.com")
         ]
    |> db_with
         [ Retract (Entity_id 516, "db/index", Some (Bool true))
         ; Add (Entity_id 2, "email", String "second@example.com")
         ]
  in
  assert_raises_invalid_arg
    "db/index retraction should remove later AVET access"
    (fun () -> ignore (datoms db Avet ~a:"email" ()))

let test_schema_component_retraction_stops_later_recursive_retracts () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 517)
             ; attrs =
                 [ "db/ident", One_value (Keyword "profile")
                 ; "db/valueType", One_value (Keyword "db.type/ref")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                 ; "db/isComponent", One_value (Bool true)
                 ]
             }
         ]
    |> db_with
         [ Retract (Entity_id 517, "db/isComponent", Some (Bool true))
         ; Entity
             { db_id = Some (Entity_id 1)
             ; attrs = [ "profile", One_value (Ref 2) ]
             }
         ; Entity { db_id = Some (Entity_id 2); attrs = [ "email", One_value (String "ivan@example.com") ] }
         ; RetractEntity (Entity_id 1)
         ]
  in
  if List.assoc_opt "profile" (schema db) <> Some ref_attr then
    failwith "db/isComponent retraction should keep ref schema but remove component setting";
  assert_equal_triples
    "db/isComponent retraction must stop later recursive retracts"
    [ 2, "email", String "ivan@example.com" ]
    (datoms db Eavt ~e:2 ())

let test_schema_transactions_reject_invalid_schema_entities () =
  assert_raises_invalid_arg
    "schema transaction with valueType requires db/ident"
    (fun () ->
      ignore
        (empty_db ()
         |> db_with
              [ Entity
                  { db_id = Some (Entity_id 1)
                  ; attrs =
                      [ "db/valueType", One_value (Keyword "db.type/ref")
                      ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                      ]
                  }
              ]));
  assert_raises_invalid_arg
    "schema transaction with valueType requires db/cardinality"
    (fun () ->
      ignore
        (empty_db ()
         |> db_with
              [ Entity
                  { db_id = Some (Entity_id 1)
                  ; attrs =
                      [ "db/ident", One_value (Keyword "friend")
                      ; "db/valueType", One_value (Keyword "db.type/ref")
                      ]
                  }
              ]));
  assert_raises_invalid_arg
    "schema transaction cannot install db namespace attrs"
    (fun () ->
      ignore
        (empty_db ()
         |> db_with
              [ Entity
                  { db_id = Some (Entity_id 1)
                  ; attrs =
                      [ "db/ident", One_value (Keyword "db/user")
                      ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                      ]
                  }
              ]))

let test_schema_validation_rejects_invalid_specs () =
  let component_without_ref =
    { cardinality = One
    ; unique = None
    ; indexed = false
    ; is_component = true
    ; no_history = false
    ; doc = None
    ; value_type = None
    ; tuple_attrs = None
  ; tuple_types = None
    }
  in
  let tuple_without_attrs =
    { cardinality = One
    ; unique = None
    ; indexed = false
    ; is_component = false
    ; no_history = false
    ; doc = None
    ; value_type = Some TupleType
    ; tuple_attrs = None
  ; tuple_types = None
    }
  in
  let many_tuple = { (tuple [ "a"; "b" ]) with cardinality = Many } in
  assert_raises_invalid_arg
    "component attrs require ref value type"
    (fun () -> ignore (empty_db ~schema:[ "profile", component_without_ref ] ()));
  assert_raises_invalid_arg
    "tuple value type requires tuple_attrs"
    (fun () -> ignore (empty_db ~schema:[ "a+b", tuple_without_attrs ] ()));
  assert_raises_invalid_arg
    "tuple attrs cannot be empty"
    (fun () -> ignore (empty_db ~schema:[ "empty", tuple [] ] ()));
  assert_raises_invalid_arg
    "tuple attrs must be cardinality one"
    (fun () -> ignore (empty_db ~schema:[ "a+b", many_tuple ] ()));
  assert_raises_invalid_arg
    "tuple attrs cannot depend on cardinality many source attrs"
    (fun () -> ignore (empty_db ~schema:[ "a", many; "a+b", tuple [ "a"; "b" ] ] ()));
  assert_raises_invalid_arg
    "tuple attrs cannot depend on another tuple attr"
    (fun () -> ignore (empty_db ~schema:[ "a+b", tuple [ "a"; "b" ]; "a+b+c", tuple [ "a+b"; "c" ] ] ()));
  assert_raises_invalid_arg
    "with_schema validates replacement schema"
    (fun () -> ignore (with_schema (empty_db ()) [ "profile", component_without_ref ]))

let test_ref_schema_rejects_non_ref_values () =
  assert_raises_invalid_arg
    "Add rejects non-ref values for RefType attrs"
    (fun () -> ignore (empty_db ~schema:[ "friend", ref_attr ] () |> db_with [ Add (Entity_id 1, "friend", String "Petr") ]));
  assert_raises_invalid_arg
    "entity maps reject non-ref values for RefType attrs"
    (fun () ->
      ignore
        (empty_db ~schema:[ "friend", ref_attr ] ()
         |> db_with
              [ Entity
                  { db_id = Some (Entity_id 1)
                  ; attrs = [ "friend", One_value (String "Petr") ]
                  }
              ]));
  assert_equal_triples
    "RefType attrs accept refs"
    [ 1, "friend", Ref 2 ]
    (empty_db ~schema:[ "friend", ref_attr ] ()
     |> db_with [ Add (Entity_id 1, "friend", Ref 2) ]
     |> fun db -> datoms db Eavt ())

let test_scalar_value_type_schema_validates_values () =
  let string_attr = { indexed with value_type = Some StringType } in
  let keyword_attr = { indexed with value_type = Some KeywordType } in
  let number_attr = { indexed with value_type = Some NumberType } in
  let db =
    empty_db ~schema:[ "name", string_attr; "tag", keyword_attr; "score", number_attr ] ()
    |> db_with
         [ Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 1, "tag", Keyword "user/admin")
         ; Add (Entity_id 1, "score", Int 10)
         ; Add (Entity_id 1, "score", Float 10.5)
         ]
  in
  assert_equal_triples
    "scalar valueType attrs accept matching values"
    [ 1, "name", String "Ivan"; 1, "score", Float 10.5; 1, "tag", Keyword "user/admin" ]
    (datoms db Eavt ());
  assert_raises_invalid_arg
    "StringType rejects non-string values"
    (fun () -> ignore (db_with [ Add (Entity_id 1, "name", Keyword "ivan") ] db));
  assert_raises_invalid_arg
    "KeywordType rejects non-keyword values"
    (fun () -> ignore (db_with [ Add (Entity_id 1, "tag", String "admin") ] db));
  assert_raises_invalid_arg
    "NumberType rejects non-number values"
    (fun () -> ignore (db_with [ Add (Entity_id 1, "score", String "10") ] db))

let test_schema_transactions_install_scalar_value_types () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 522)
             ; attrs =
                 [ "db/ident", One_value (Keyword "name")
                 ; "db/valueType", One_value (Keyword "db.type/string")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                 ; "db/index", One_value (Bool true)
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 523)
             ; attrs =
                 [ "db/ident", One_value (Keyword "score")
                 ; "db/valueType", One_value (Keyword "db.type/number")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                 ; "db/index", One_value (Bool true)
                 ]
             }
         ; Add (Entity_id 1, "name", String "Ivan")
         ; Add (Entity_id 1, "score", Float 4.5)
         ]
  in
  if List.assoc_opt "name" (schema db) <> Some { indexed with value_type = Some StringType } then
    failwith "schema transaction should install db.type/string";
  if List.assoc_opt "score" (schema db) <> Some { indexed with value_type = Some NumberType } then
    failwith "schema transaction should install db.type/number";
  assert_raises_invalid_arg
    "schema-installed scalar valueType rejects mismatched values"
    (fun () -> ignore (db_with [ Add (Entity_id 1, "name", Keyword "ivan") ] db))

let test_uuid_and_instant_value_type_schema_validates_values () =
  let uuid_attr = { indexed with value_type = Some UuidType } in
  let instant_attr = { indexed with value_type = Some InstantType } in
  let db =
    empty_db ~schema:[ "uuid", uuid_attr; "created-at", instant_attr ] ()
    |> db_with
         [ Add (Entity_id 1, "uuid", Uuid "65ec87fb-0000-0000-0000-000000000001")
         ; Add (Entity_id 1, "created-at", Instant 1_710_000_123_456)
         ]
  in
  assert_equal_triples
    "uuid and instant valueType attrs accept matching values"
    [ 1, "created-at", Instant 1_710_000_123_456
    ; 1, "uuid", Uuid "65ec87fb-0000-0000-0000-000000000001"
    ]
    (datoms db Eavt ());
  assert_raises_invalid_arg
    "UuidType rejects non-uuid values"
    (fun () -> ignore (db_with [ Add (Entity_id 1, "uuid", String "65ec87fb") ] db));
  assert_raises_invalid_arg
    "InstantType rejects non-instant values"
    (fun () -> ignore (db_with [ Add (Entity_id 1, "created-at", Int 1_710_000_123_456) ] db))

let test_schema_transactions_install_uuid_and_instant_value_types () =
  let db =
    empty_db ()
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 524)
             ; attrs =
                 [ "db/ident", One_value (Keyword "uuid")
                 ; "db/valueType", One_value (Keyword "db.type/uuid")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                 ; "db/index", One_value (Bool true)
                 ]
             }
         ; Entity
             { db_id = Some (Entity_id 525)
             ; attrs =
                 [ "db/ident", One_value (Keyword "created-at")
                 ; "db/valueType", One_value (Keyword "db.type/instant")
                 ; "db/cardinality", One_value (Keyword "db.cardinality/one")
                 ; "db/index", One_value (Bool true)
                 ]
             }
         ; Add (Entity_id 1, "uuid", Uuid "65ec87fb-0000-0000-0000-000000000001")
         ; Add (Entity_id 1, "created-at", Instant 1_710_000_123_456)
         ]
  in
  if List.assoc_opt "uuid" (schema db) <> Some { indexed with value_type = Some UuidType } then
    failwith "schema transaction should install db.type/uuid";
  if List.assoc_opt "created-at" (schema db) <> Some { indexed with value_type = Some InstantType } then
    failwith "schema transaction should install db.type/instant";
  assert_raises_invalid_arg
    "schema-installed uuid valueType rejects mismatched values"
    (fun () -> ignore (db_with [ Add (Entity_id 1, "uuid", String "not-a-uuid-value") ] db))

let () =
  test_datom_defaults ();
  test_empty_db ();
  test_init_db_and_indexes ();
  test_init_db_counts_ref_values_in_max_eid ();
  test_init_db_resolves_raw_ref_datoms_from_schema ();
  test_datoms_returns_lazy_sequence ();
  test_datoms_slices_before_filtered_predicate ();
  test_raw_datom_counts_ref_values_in_max_eid ();
  test_raw_datom_counts_tx_in_max_tx ();
  test_transact__test_with_datoms ();
  test_find_datom_returns_first_index_match ();
  test_vaet_index_returns_ref_datoms_by_value ();
  test_incremental_writes_keep_public_datoms_indexes_correct ();
  test_index_range_returns_avet_values_between_bounds ();
  test_indexes_compare_keywords_like_datascript ();
  test_indexes_compare_numbers_across_value_constructors ();
  test_transact__test_compare_numbers_js_issue_404 ();
  test_avet_exact_lookup_compares_entire_sequences ();
  test_indexes_compare_mixed_value_types_like_datascript ();
  test_avet_excludes_unindexed_scalar_attrs ();
  test_seek_datoms_scans_forward_from_index_tuple ();
  test_rseek_datoms_scans_backward_from_index_tuple ();
  test_seek_datoms_continues_across_avet_attributes ();
  test_rseek_datoms_continues_across_avet_attributes ();
  test_upstream_index_api_parity_batch ();
  test_db_with_adds_entities ();
  test_with_tx_returns_transaction_report ();
  test_entity_map_expands_collection_values_for_many_attrs ();
  test_entity_map_db_id_attr_is_not_stored ();
  test_transact__test_with ();
  test_transact__test_retract_fns_not_found ();
  test_tuple_attrs_track_source_attrs ();
  test_tuple_attrs_reject_direct_writes ();
  test_tuple_attrs_ignore_direct_writes_that_match_sources ();
  test_tuple_attrs_validate_entity_map_direct_writes_after_sources ();
  test_tuple_values_resolve_lookup_refs ();
  test_tuple_lookup_refs_resolve_nested_lookup_refs ();
  test_edn_tuple_lookup_refs_resolve_nested_lookup_refs ();
  test_tuple_attrs_are_indexed_by_default ();
  test_tuple_attrs_support_avet_range_bounds ();
  test_tuple_types_validate_direct_tuple_values ();
  test_transact__test_db_fn_cas ();
  test_db_with_compare_and_set_on_many_attr ();
  test_transact__test_retract_without_value_issue_339 ();
  test_transact__test_uncomparable_issue_356 ();
  test_nil_values_are_query_only ();
  test_list_values_can_be_indexed_exactly ();
  test_list_values_use_datascript_length_first_ordering ();
  test_map_values_are_order_insensitive ();
  test_init_db_normalizes_map_values ();
  test_set_values_are_order_insensitive ();
  test_init_db_normalizes_set_values ();
  test_entid_normalizes_unordered_values ();
  test_transact__test_transitive_type_compare_issue_386 ();
  test_tempids_are_rejected_in_non_add_ops ();
  test_value_only_tempids_are_rejected ();
  test_empty_entity_tempids_are_not_entity_usage ();
  test_tempid_generates_unique_entity_refs ();
  test_transact__test_db_fn ();
  test_transact__test_db_fn_returning_entity_without_db_id_issue_474 ();
  test_transact__test_db_ident_fn ();
  test_transact__test_large_ids_issue_292 ();
  test_transact__test_tx_entity_ids_do_not_advance_max_eid ();
  test_init_db__test_tx_entity_ids_do_not_advance_max_eid ();
  test_transact__test_tempid_allocation_stops_before_tx0 ();
  test_db_with_allocates_tempids ();
  test_transact__test_resolve_eid ();
  test_transact__test_resolve_eid_refs ();
  test_db_ident_is_builtin_and_resolves_refs ();
  test_upstream_ident_parity_batch ();
  test_entid_ref_resolves_entity_refs ();
  test_db_ident_rejects_duplicate_idents_by_default ();
  test_transact_report_exposes_tempids ();
  test_resolve_tempid_reads_tx_report_tempids ();
  test_transact__test_resolve_current_tx ();
  test_current_tx_string_aliases_resolve_in_transactions ();
  test_current_tx_string_aliases_can_be_value_only ();
  test_current_tx_colon_string_alias_resolves_in_transactions ();
  test_transact_report_exposes_resolved_tx_datoms ();
  test_transact_report_exposes_tx_meta ();
  test_transact_report_exposes_cardinality_one_retractions ();
  test_history_exposes_additions_and_retractions ();
  test_no_history_schema_omits_attr_from_history ();
  test_datoms_filter_by_tx_component ();
  test_reverse_ref_helpers ();
  test_entity_maps_expand_reverse_attrs ();
  test_entity_maps_reject_non_ref_reverse_attr_values ();
  test_entity_maps_reject_reverse_attrs_without_ref_schema ();
  test_entity_maps_expand_nested_entity_values ();
  test_entity_map_with_only_nested_ref_allocates_nested_first ();
  test_entity_maps_expand_reverse_nested_entity_values ();
  test_entity_maps_expand_many_reverse_nested_entity_values ();
  test_upstream_components_and_explode_parity_batch ();
  test_init_db_preserves_uuid_and_instant_values ();
  test_q_finds_values ();
  test_parse_query_finds_values ();
  test_edn_reader_parses_query_and_pull_strings ();
  test_edn_string_top_level_apis ();
  test_query__test_symbol_comparison ();
  test_db_with_string_matches_upstream_validation_messages ();
  test_edn_reader_parses_transaction_and_schema_strings ();
  test_edn_reader_parses_common_literals ();
  test_edn_reader_ignores_discard_and_metadata ();
  test_parse_query_comparison_predicates ();
  test_parse_query_equality_predicates ();
  test_parse_query_arithmetic_functions ();
  test_parse_query_transaction_patterns ();
  test_parse_query_source_qualified_patterns ();
  test_parse_query_find_pull_expressions ();
  test_parse_query_missing_and_get_else_clauses ();
  test_parse_query_get_some_and_get_clauses ();
  test_parse_query_collection_value_clauses ();
  test_parse_query_type_and_numeric_predicates ();
  test_parse_query_variadic_comparison_predicates ();
  test_parse_query_boolean_predicates ();
  test_parse_query_core_value_functions ();
  test_parse_query_random_and_identity_predicates ();
  test_parse_query_string_predicates_and_transforms ();
  test_parse_query_string_trim_index_and_subs ();
  test_parse_query_string_build_replace_regex_and_split ();
  test_parse_query_collection_constructors ();
  test_parse_query_ground_and_value_metadata_functions ();
  test_parse_query_aggregate_find_expressions ();
  test_parse_query_extended_aggregate_find_expressions ();
  test_parse_query_not_and_not_join_clauses ();
  test_parse_query_or_and_or_join_clauses ();
  test_parse_query_rules ();
  test_parse_query_source_qualified_composite_clauses ();
  test_parse_query_in_bindings ();
  test_parse_query_input_helper_parsers ();
  test_parse_query_find_helper_parser ();
  test_parse_query_or_join_required_vars ();
  test_parse_query_where_clause_validation_messages ();
  test_parse_query_validates_structure ();
  test_parse_query_with_vars ();
  test_parse_query_matches_upstream_validation_messages ();
  test_parse_query_with_and_rules_match_upstream_messages ();
  test_q_input_arity_matches_upstream_validation_messages ();
  test_q_input_binding_matches_upstream_validation_messages ();
  test_parse_query_map_sections_accept_list_sequences ();
  test_parse_query_concatenates_repeated_sections ();
  test_q_binds_transaction_id_in_patterns ();
  test_q_binds_transaction_operation_in_history_patterns ();
  test_q_joins_clauses ();
  test_q_short_data_patterns_match_upstream ();
  test_q_upstream_query_cljc_parity_batch ();
  test_query__test_joins ();
  test_query__test_q_many ();
  test_query__test_q_coll ();
  test_query__test_q_in ();
  test_query__test_bindings ();
  test_query__test_nested_bindings ();
  test_query__test_built_in_get ();
  test_query__test_join_unrelated ();
  test_query__test_constant_substitution ();
  test_q_reverse_ref_patterns ();
  test_q_predicates_filter_bound_values ();
  test_q_predicates_without_free_variables_filter_all_rows ();
  test_q_functions_bind_derived_values ();
  test_q_functions_filter_on_none ();
  test_q_function_binding_conflicts_filter_rows ();
  test_q_function_bindings_interact_with_rules ();
  test_q_parsed_rule_inputs_interact_with_function_bindings ();
  test_q_predicates_and_functions_reject_unbound_inputs ();
  test_q_builtin_get_else_get_some_and_missing ();
  test_q_builtin_get_map_values ();
  test_q_builtin_count_values ();
  test_q_builtin_empty_and_not_empty_values ();
  test_q_builtin_contains_values ();
  test_q_builtin_value_type_predicates ();
  test_q_builtin_numeric_predicates ();
  test_q_builtin_comparison_predicates ();
  test_q_builtin_variadic_comparison_predicates ();
  test_q_builtin_equality_predicates ();
  test_q_builtin_arithmetic_values ();
  test_q_builtin_integer_arithmetic_values ();
  test_q_builtin_compare_min_max_values ();
  test_q_builtin_boolean_predicates ();
  test_q_builtin_identity_and_boolean_values ();
  test_q_builtin_random_values ();
  test_q_builtin_differ_and_identical_predicates ();
  test_q_builtin_type_values ();
  test_q_builtin_name_and_namespace_values ();
  test_q_builtin_keyword_from_name_values ();
  test_q_builtin_meta_values ();
  test_q_builtin_string_predicates ();
  test_q_builtin_string_transforms ();
  test_q_builtin_string_trim_values ();
  test_q_builtin_string_index_values ();
  test_q_builtin_string_substring_values ();
  test_q_builtin_string_build_and_join_values ();
  test_q_builtin_print_string_values ();
  test_q_builtin_string_replace_values ();
  test_q_builtin_string_replace_regex_values ();
  test_q_builtin_string_escape_values ();
  test_q_builtin_regex_values ();
  test_query__test_built_in_regex ();
  test_q_builtin_string_blank_and_split_values ();
  test_q_builtin_vector_values ();
  test_q_builtin_vector_captures_bound_row_values ();
  test_q_builtin_hash_map_values ();
  test_q_builtin_list_and_set_values ();
  test_q_builtin_range_values ();
  test_q_builtin_tuple_and_untuple ();
  test_q_untuple_ignores_placeholder_outputs ();
  test_q_untuple_accepts_list_values ();
  test_q_builtin_ground_bindings ();
  test_q_builtin_function_insufficient_bindings_match_upstream_messages ();
  test_q_not_filters_matching_bindings ();
  test_q_not_rejects_clauses_without_outer_bindings ();
  test_q_not_insufficient_bindings_match_upstream_messages ();
  test_q_not_join_projects_join_variables ();
  test_q_not_join_rejects_unbound_join_vars ();
  test_q_not_matches_upstream_edge_cases ();
  test_q_or_unions_branch_results ();
  test_q_or_rejects_branches_with_different_free_vars ();
  test_q_or_matches_upstream_error_messages ();
  test_q_or_allows_branch_vars_bound_by_outer_clauses ();
  test_q_or_join_projects_join_variables ();
  test_q_or_join_binds_listed_branch_variables ();
  test_q_or_join_rejects_branches_missing_unbound_listed_vars ();
  test_q_or_join_constant_substitution ();
  test_q_or_join_required_vars_use_outer_bindings ();
  test_q_source_qualified_composite_clauses ();
  test_q_not_or_upstream_source_and_relation_batch ();
  test_q_with_scalar_inputs ();
  test_q_with_entity_ref_inputs ();
  test_q_with_lookup_ref_collection_inputs ();
  test_q_with_lookup_ref_inputs_in_entity_builtins ();
  test_q_with_relation_inputs ();
  test_q_with_collection_inputs ();
  test_q_with_tuple_inputs ();
  test_q_with_dynamic_callable_inputs ();
  test_q_nested_relation_map_inputs ();
  test_q_with_dynamic_callable_inputs_in_rules ();
  test_q_input_placeholders_ignore_values ();
  test_q_return_shapes ();
  test_parse_query_return_shapes ();
  test_q_return_find_specs_match_upstream_cases ();
  test_q_return_map_shapes ();
  test_q_return_map_string_upstream_shape_batch ();
  test_parse_query_return_map_shapes ();
  test_q_resolves_lookup_refs_in_patterns ();
  test_parse_query_resolves_lookup_refs_in_patterns ();
  test_q_with_multiple_sources ();
  test_q_with_relation_source ();
  test_q_with_relation_source_arbitrary_arity ();
  test_q_sources_default_source ();
  test_parse_query_infers_default_source_input ();
  test_q_sources_unknown_source_rejected ();
  test_q_sources_lookup_ref_uses_named_source ();
  test_q_resolves_idents_in_patterns ();
  test_parse_query_resolves_idents_in_patterns ();
  test_q_find_pull_expressions ();
  test_q_return_shapes_with_pull_expressions ();
  test_q_find_pull_uses_named_source ();
  test_q_with_aggregates ();
  test_q_aggregates_with_pull_expressions ();
  test_q_with_interleaved_aggregates ();
  test_q_aggregates_relation_inputs_with_with_vars ();
  test_q_with_preserves_non_aggregate_duplicates ();
  test_q_count_distinct_aggregate ();
  test_q_distinct_aggregate ();
  test_q_min_max_use_keyword_comparator ();
  test_q_with_vars_preserve_aggregate_duplicates ();
  test_q_avg_aggregate ();
  test_q_sum_aggregate_accepts_float_values ();
  test_q_statistical_aggregates ();
  test_q_min_n_and_max_n_aggregates ();
  test_q_rand_and_sample_aggregates ();
  test_q_custom_aggregates ();
  test_q_rejects_unknown_rules ();
  test_q_rules_accept_false_arguments ();
  test_q_with_rules ();
  test_q_rule_context_is_isolated_from_outer_context ();
  test_q_regular_clauses_join_with_rules ();
  test_q_rule_branches_match_upstream ();
  test_q_can_call_same_dynamic_predicate_rule_twice ();
  test_q_with_recursive_rules ();
  test_q_with_symmetric_recursive_rules ();
  test_q_with_mutually_recursive_rules ();
  test_q_source_qualified_rules ();
  test_query_fns__test_query_fns ();
  test_query_fns__test_predicates ();
  test_query_fns__test_symbol_resolution ();
  test_query_aggregates__test_aggregates ();
  test_query_not__test_not ();
  test_query_not__test_not_join ();
  test_query_not__test_default_source ();
  test_query_not__test_impl_edge_cases ();
  test_query_not__test_insufficient_bindings ();
  test_query_or__test_or ();
  test_query_or__test_or_join ();
  test_query_or__test_default_source ();
  test_query_or__test_const_substitution ();
  test_query_or__test_errors ();
  test_query_rules__test_rules ();
  test_query_rules__test_false_arguments ();
  test_query_rules__test_rule_performance_on_larger_datasets ();
  test_unique_tuple_identity_upserts_entity_maps ();
  test_unique_tuple_identity_updates_multiple_sources_atomically ();
  test_add_tempid_upserts_by_unique_tuple_sources ();
  test_transact__test_tempid_ref_issue_295 ();
  test_unique_value_rejects_duplicate_values ();
  test_db_with_string_unique_value_matches_upstream_validation ();
  test_transact__test_transact_bang ();
  test_connection_auto_listener_keys ();
  test_bang_connection_api_aliases ();
  test_connection_reports_strip_skip_store_metadata ();
  test_transact__test_retract_fns ();
  test_transact__test_transient_issue_294 ();
  test_retract_attr_removes_all_attribute_values ();
  test_retract_entity_recursively_removes_components ();
  test_retract_attr_removes_component_values ();
  test_pull_selects_requested_attributes ();
  test_parse_pull_pattern_selects_attributes_and_refs ();
  test_parse_pull_pattern_accepts_top_level_lists ();
  test_parse_pull_pattern_accepts_string_db_id ();
  test_parse_pull_pattern_aliases_attributes ();
  test_parse_pull_pattern_accepts_upstream_alias_value_forms ();
  test_parse_pull_pattern_defaults_attributes ();
  test_parse_pull_pattern_limits_attributes ();
  test_parse_pull_pattern_legacy_limit_and_default ();
  test_parse_pull_pattern_list_form_attr_options ();
  test_parse_pull_pattern_multi_entry_map_specs ();
  test_parse_pull_pattern_rejects_reserved_string_attr_names ();
  test_parse_pull_pattern_validates_limits ();
  test_parse_pull_pattern_unlimited_limits ();
  test_parse_pull_pattern_xforms_attributes ();
  test_parse_pull_pattern_xforms_ref_map_specs ();
  test_parse_pull_pattern_validates_map_spec_refs ();
  test_parse_pull_pattern_validates_reverse_attrs ();
  test_parse_pull_pattern_expands_reverse_refs ();
  test_parse_pull_pattern_recursive_refs ();
  test_parse_pull_pattern_recursive_refs_preserve_context ();
  test_parse_pull_pattern_recursive_string_ellipsis ();
  test_pull_aliases_selected_attributes ();
  test_pull_later_duplicate_keys_replace_earlier_values ();
  test_pull_transforms_selected_attributes ();
  test_pull_default_takes_precedence_over_xform ();
  test_pull_attr_default_and_limit ();
  test_pull_ref_default_expands_existing_refs ();
  test_pull_reverse_ref_default_expands_existing_refs ();
  test_pull_applies_default_limit ();
  test_pull_drops_empty_results ();
  test_pull_drops_empty_cardinality_one_ref_results ();
  test_pull_expands_forward_and_reverse_refs ();
  test_pull_reverse_component_returns_single_entity ();
  test_pull_component_attr_expands_recursively ();
  test_pull_component_attr_returns_id_stub_for_cycles ();
  test_pull_nested_component_can_expand_reverse_component_ref ();
  test_pull_id_and_wildcard ();
  test_pull_reports_visitor_events ();
  test_pull_recursive_ref_with_depth_limit ();
  test_pull_recursive_ref_avoids_cycles ();
  test_pull_recursive_refs_share_pattern_context ();
  test_pull_recursive_ref_depth_preserves_sibling_context ();
  test_pull_dual_recursion_respects_independent_depths ();
  test_pull_dual_recursion_tracks_cycles_per_branch ();
  test_pull_deep_recursion_reaches_leaf ();
  test_pull_recursive_reverse_ref ();
  test_pull_many_preserves_missing_entities ();
  test_pull_reads_filtered_serialized_and_reinitialized_dbs ();
  test_filter_limits_read_apis ();
  test_filter_composes_and_rejects_writes ();
  test_filter_predicates_read_unfiltered_db_like_upstream ();
  test_filter_and_entity_upstream_edge_parity_batch ();
  test_schema_and_with_schema ();
  test_schema_transactions_install_schema_attrs ();
  test_schema_accepts_db_type_alias ();
  test_schema_transactions_install_no_history ();
  test_schema_transactions_install_doc ();
  test_schema_transactions_install_tuple_types ();
  test_schema_transactions_install_tuple_attrs ();
  test_schema_transactions_affect_same_transaction ();
  test_schema_add_datoms_affect_same_transaction ();
  test_later_schema_entities_do_not_affect_earlier_ops ();
  test_later_schema_add_datoms_do_not_affect_earlier_ops ();
  test_schema_retractions_affect_later_ops ();
  test_schema_ident_retraction_removes_schema_attr ();
  test_schema_entity_retractions_remove_schema_attrs ();
  test_schema_field_retractions_drop_stale_field_values ();
  test_schema_unique_retraction_allows_later_duplicates ();
  test_schema_index_retraction_removes_later_avet_access ();
  test_schema_component_retraction_stops_later_recursive_retracts ();
  test_schema_transactions_reject_invalid_schema_entities ();
  test_schema_validation_rejects_invalid_specs ();
  test_ref_schema_rejects_non_ref_values ();
  test_scalar_value_type_schema_validates_values ();
  test_schema_transactions_install_scalar_value_types ();
  test_uuid_and_instant_value_type_schema_validates_values ();
  test_schema_transactions_install_uuid_and_instant_value_types ()
