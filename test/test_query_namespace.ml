open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_query label expected actual =
  if expected <> actual then failf "%s: unexpected query result" label

let assert_equal_query_rows label expected actual =
  if expected <> actual then failf "%s: unexpected query rows" label

let assert_equal_inputs label expected actual =
  if expected <> actual then failf "%s: unexpected bound query inputs" label

let assert_equal_rules label expected actual =
  if expected <> actual then failf "%s: unexpected query rules" label

let assert_equal_query_option label expected actual =
  if expected <> actual then failf "%s: unexpected optional query result" label

let assert_equal_int_option label expected actual =
  if expected <> actual then failf "%s: unexpected optional integer result" label

let assert_equal_bool label expected actual =
  if expected <> actual then failf "%s: expected %b but got %b" label expected actual

let assert_equal_grouped_bindings label expected actual =
  if expected <> actual then failf "%s: unexpected grouped bindings" label

let assert_equal_string_list label expected actual =
  if expected <> actual then failf "%s: expected a different string list" label

let assert_equal_string label expected actual =
  if expected <> actual then failf "%s: expected %S but got %S" label expected actual

let assert_equal_aggregate label expected actual =
  if expected <> actual then failf "%s: expected a different aggregate" label

let assert_equal_terms label expected actual =
  if expected <> actual then failf "%s: expected different aggregate terms" label

let assert_raises_invalid_arg label f =
  match f () with
  | _ -> failf "%s: expected Invalid_argument" label
  | exception Invalid_argument _ -> ()

let assert_raises_invalid_arg_message label expected f =
  match f () with
  | _ -> failf "%s: expected Invalid_argument" label
  | exception Invalid_argument actual when actual = expected -> ()
  | exception Invalid_argument actual ->
    failf "%s: expected Invalid_argument %S but got %S" label expected actual
  | exception exn -> failf "%s: unexpected exception %s" label (Printexc.to_string exn)

let test_query_namespace__test_public_query_api () =
  let db =
    empty_db ()
    |> db_with [ Add (Entity_id 1, "name", String "Ivan"); Add (Entity_id 2, "name", String "Oleg") ]
  in
  assert_equal_query
    "Query.q_string exposes relation query API"
    [ [ Result_value (String "Ivan") ]; [ Result_value (String "Oleg") ] ]
    (Query.q_string db "[:find ?name :where [_ :name ?name]]");
  assert_equal_query
    "Query.q_sources_string exposes named sources"
    [ [ Result_value (String "Ivan") ] ]
    (Query.q_sources_string
       (empty_db ())
       [ "people", Db_source db ]
       "[:find ?name :in $people :where [$people _ :name ?name] [(= ?name \"Ivan\")]]");
  if
    Query.q_return_map_string db "[:find ?e ?name :keys id name :where [?e :name ?name]]"
    <> Query_relation_maps
         [ [ Keyword "id", Result_entity 1; Keyword "name", Result_value (String "Ivan") ]
         ; [ Keyword "id", Result_entity 2; Keyword "name", Result_value (String "Oleg") ]
         ]
  then
    failwith "Query.q_return_map_string should expose return-map query API"

let test_query_namespace__test_query_result_helpers () =
  let add_datom = datom ~e:1 ~a:"name" ~v:(String "Ivan") ~tx:7 ~added:true () in
  let retract_datom = datom ~e:1 ~a:"name" ~v:(String "Ivan") ~tx:8 ~added:false () in
  assert_equal_query_option
    "result_of_datom_e returns entity results"
    (Result_entity 1)
    (Query.result_of_datom_e add_datom);
  assert_equal_query_option
    "result_of_datom_a returns attr results"
    (Result_attr "name")
    (Query.result_of_datom_a add_datom);
  assert_equal_query_option
    "result_of_datom_v returns value results"
    (Result_value (String "Ivan"))
    (Query.result_of_datom_v add_datom);
  assert_equal_query_option
    "result_of_datom_tx returns tx entity results"
    (Result_entity 7)
    (Query.result_of_datom_tx add_datom);
  assert_equal_query_option
    "result_of_datom_op returns add op keywords"
    (Result_value (Keyword "db/add"))
    (Query.result_of_datom_op add_datom);
  assert_equal_query_option
    "result_of_datom_op returns retract op keywords"
    (Result_value (Keyword "db/retract"))
    (Query.result_of_datom_op retract_datom);
  assert_equal_query_option
    "result_of_ref turns ref values into entity results"
    (Result_entity 42)
    (Query.result_of_ref (Result_value (Ref 42)));
  assert_equal_query_option
    "result_of_ref leaves non-ref results unchanged"
    (Result_value (String "Ivan"))
    (Query.result_of_ref (Result_value (String "Ivan")));
  let validate_entity_id entity_id =
    if entity_id <= 0 then invalid_arg "invalid entity id";
    entity_id
  in
  let result_resolution_context =
    { Query.validate_entity_id
    ; resolve_query_value =
        (function
          | Keyword "known-ident" -> Some (Ref 42)
          | Symbol "missing" -> None
          | value -> Some value)
    ; lookup_ref_entity_id =
        (fun attr value ->
           match attr, value with
           | "name", String "Ivan" -> Some 101
           | _ -> None)
    }
  in
  assert_equal_int_option
    "entity_id_of_resolved_query_result accepts entity results"
    (Some 42)
    (Query.entity_id_of_resolved_query_result ~validate_entity_id (Some (Result_entity 42)));
  assert_equal_int_option
    "entity_id_of_resolved_query_result validates integer results"
    (Some 43)
    (Query.entity_id_of_resolved_query_result ~validate_entity_id (Some (Result_value (Int 43))));
  assert_equal_int_option
    "entity_id_of_resolved_query_result accepts ref values"
    (Some 44)
    (Query.entity_id_of_resolved_query_result ~validate_entity_id (Some (Result_value (Ref 44))));
  assert_equal_int_option
    "entity_id_of_resolved_query_result rejects non-entity values"
    None
    (Query.entity_id_of_resolved_query_result ~validate_entity_id (Some (Result_value (String "Ivan"))));
  assert_equal_int_option
    "entity_id_of_resolved_query_result rejects missing values"
    None
    (Query.entity_id_of_resolved_query_result ~validate_entity_id None);
  assert_raises_invalid_arg "entity_id_of_resolved_query_result validates integer ids" (fun () ->
    ignore (Query.entity_id_of_resolved_query_result ~validate_entity_id (Some (Result_value (Int 0)))));
  assert_equal_query_option
    "resolved_query_result resolves value results through the context"
    (Some (Result_entity 42))
    (Query.resolved_query_result result_resolution_context (Result_value (Keyword "known-ident")));
  assert_equal_query_option
    "resolved_query_result drops values that cannot resolve"
    None
    (Query.resolved_query_result result_resolution_context (Result_value (Symbol "missing")));
  assert_equal_query_option
    "resolved_query_result drops db results"
    None
    (Query.resolved_query_result result_resolution_context (Result_db (empty_db ())));
  assert_equal_int_option
    "lookup_ref_entity_id_of_value resolves vector lookup refs"
    (Some 101)
    (Query.lookup_ref_entity_id_of_value
       result_resolution_context
       (Vector [ Keyword "name"; String "Ivan" ]));
  assert_equal_int_option
    "lookup_ref_entity_id_of_value rejects non lookup-ref values"
    None
    (Query.lookup_ref_entity_id_of_value result_resolution_context (String "Ivan"));
  assert_equal_int_option
    "query_result_entity_id prefers lookup-ref values"
    (Some 101)
    (Query.query_result_entity_id
       result_resolution_context
       (Result_value (Vector [ Keyword "name"; String "Ivan" ])));
  assert_equal_int_option
    "query_result_entity_id falls back to resolved values"
    (Some 42)
    (Query.query_result_entity_id result_resolution_context (Result_value (Keyword "known-ident")));
  assert_equal_bool
    "query_results_equivalent compares identical db results by physical identity"
    true
    (let db = empty_db () in
     Query.query_results_equivalent result_resolution_context (Result_db db) (Result_db db));
  assert_equal_bool
    "query_results_equivalent rejects different db results"
    false
    (Query.query_results_equivalent result_resolution_context (Result_db (empty_db ())) (Result_db (empty_db ())));
  assert_equal_bool
    "query_results_equivalent compares lookup refs through entity ids"
    true
    (Query.query_results_equivalent
       result_resolution_context
       (Result_value (Vector [ Keyword "name"; String "Ivan" ]))
       (Result_entity 101));
  assert_equal_bool
    "query_results_equivalent compares resolved values"
    true
    (Query.query_results_equivalent result_resolution_context (Result_value (Keyword "known-ident")) (Result_entity 42));
  assert_equal_query_option
    "bind_var adds unbound vars"
    (Some [ "e", Result_entity 42 ])
    (Query.bind_var result_resolution_context "e" (Result_entity 42) []);
  assert_equal_query_option
    "bind_var accepts equivalent bound values"
    (Some [ "e", Result_value (Keyword "known-ident") ])
    (Query.bind_var
       result_resolution_context
       "e"
       (Result_entity 42)
       [ "e", Result_value (Keyword "known-ident") ]);
  assert_equal_query_option
    "bind_var rejects conflicting bound values"
    None
    (Query.bind_var result_resolution_context "e" (Result_entity 99) [ "e", Result_entity 42 ]);
  assert_equal_bool
    "result_matches_entity accepts equivalent entity ids"
    true
    (Query.result_matches_entity result_resolution_context 42 (Result_value (Keyword "known-ident")));
  assert_equal_bool
    "result_matches_entity rejects mismatches"
    false
    (Query.result_matches_entity result_resolution_context 99 (Result_value (Keyword "known-ident")))

let test_query_namespace__test_query_matching_helpers () =
  let result_resolution_context =
    { Query.validate_entity_id = (fun entity_id -> entity_id)
    ; resolve_query_value =
        (function
          | Keyword "known-ident" -> Some (Ref 42)
          | value -> Some value)
    ; lookup_ref_entity_id =
        (fun attr value ->
           match attr, value with
           | "name", String "Ivan" -> Some 101
           | _ -> None)
    }
  in
  let match_context =
    { Query.result_resolution_context
    ; source_db = empty_db ()
    ; ident_entity_id = (function "known-ident" -> Some 42 | _ -> None)
    ; unresolved_lookup_ref_message = (fun attr _ -> "missing lookup ref: " ^ attr)
    ; value_equal = Util.value_equal
    ; coerce_tuple_lookup_value =
        (fun attr value ->
           match attr, value with
           | "tuple", Vector [ left; right ] -> Tuple [ Some left; Some right ]
           | _ -> value)
    }
  in
  let base_binding = [ "existing", Result_value (String "kept") ] in
  assert_equal_query_option
    "match_query_term keeps bindings for wildcards"
    (Some base_binding)
    (Query.match_query_term match_context QWildcard (Result_entity 1) base_binding);
  assert_equal_query_option
    "match_query_term matches entity terms through result equivalence"
    (Some [])
    (Query.match_query_term match_context (QEntity 42) (Result_value (Keyword "known-ident")) []);
  assert_equal_query_option
    "match_query_term resolves ident terms through the context"
    (Some [])
    (Query.match_query_term match_context (QIdent "known-ident") (Result_entity 42) []);
  assert_equal_query_option
    "match_query_term resolves lookup ref terms through the context"
    (Some [])
    (Query.match_query_term match_context (QLookupRef ("name", String "Ivan")) (Result_entity 101) []);
  assert_raises_invalid_arg_message
    "match_query_term reports missing lookup refs"
    "missing lookup ref: name"
    (fun () ->
       ignore
         (Query.match_query_term
            match_context
            (QLookupRef ("name", String "Missing"))
            (Result_entity 101)
            []));
  assert_equal_query_option
    "match_query_term matches attrs"
    (Some [])
    (Query.match_query_term match_context (QAttr "name") (Result_attr "name") []);
  assert_equal_query_option
    "match_query_term matches literal values"
    (Some [])
    (Query.match_query_term match_context (QValue (String "Ivan")) (Result_value (String "Ivan")) []);
  assert_equal_query_option
    "match_query_term matches ref values against entity results"
    (Some [])
    (Query.match_query_term match_context (QValue (Ref 42)) (Result_entity 42) []);
  assert_equal_query_option
    "match_query_term matches keyword idents against entity results"
    (Some [])
    (Query.match_query_term match_context (QValue (Keyword "known-ident")) (Result_entity 42) []);
  assert_equal_query_option
    "match_query_term binds vars after normalizing refs"
    (Some [ "e", Result_entity 42 ])
    (Query.match_query_term match_context (QVar "e") (Result_value (Ref 42)) []);
  let name_datom = datom ~e:1 ~a:"name" ~v:(String "Ivan") ~tx:7 ~added:true () in
  assert_equal_query_option
    "match_pattern_clause matches datoms"
    (Some [ "v", Result_value (String "Ivan"); "e", Result_entity 1 ])
    (Query.match_pattern_clause match_context [] (QVar "e") (QAttr "name") (QVar "v") name_datom);
  assert_equal_query_option
    "match_pattern_tx_clause matches tx terms"
    (Some [ "tx", Result_entity 7; "v", Result_value (String "Ivan"); "e", Result_entity 1 ])
    (Query.match_pattern_tx_clause
       match_context
       []
       (QVar "e")
       (QAttr "name")
       (QVar "v")
       (QVar "tx")
       name_datom);
  let tuple_datom =
    datom ~e:2 ~a:"tuple" ~v:(Tuple [ Some (String "a"); Some (String "b") ]) ~tx:8 ~added:true ()
  in
  assert_equal_query_option
    "match_value_term_for_datom_attr coerces tuple lookup values"
    (Some [])
    (Query.match_value_term_for_datom_attr
       match_context
       []
       (QValue (Vector [ String "a"; String "b" ]))
       tuple_datom);
  let reverse_datom = datom ~e:1 ~a:"parent" ~v:(Ref 2) ~tx:9 ~added:true () in
  assert_equal_query_option
    "match_reverse_pattern_clause matches reverse refs"
    (Some [ "parent", Result_entity 1 ])
    (Query.match_reverse_pattern_clause match_context [] (QEntity 2) "_parent" (QVar "parent") reverse_datom);
  assert_equal_query_option
    "eval_query_term reads bound vars"
    (Some (Result_value (String "kept")))
    (Query.eval_query_term match_context base_binding (QVar "existing"));
  assert_equal_query_option
    "eval_query_term resolves entity terms"
    (Some (Result_entity 42))
    (Query.eval_query_term match_context [] (QEntity 42));
  assert_equal_query_option
    "eval_query_term resolves ident terms"
    (Some (Result_entity 42))
    (Query.eval_query_term match_context [] (QIdent "known-ident"));
  assert_equal_query_option
    "eval_query_term resolves lookup ref terms"
    (Some (Result_entity 101))
    (Query.eval_query_term match_context [] (QLookupRef ("name", String "Ivan")));
  assert_raises_invalid_arg_message
    "eval_query_term reports missing lookup refs"
    "missing lookup ref: name"
    (fun () -> ignore (Query.eval_query_term match_context [] (QLookupRef ("name", String "Missing"))));
  assert_equal_query_option
    "eval_query_term resolves literal values"
    (Some (Result_value (Ref 42)))
    (Query.eval_query_term match_context [] (QValue (Keyword "known-ident")));
  assert_equal_query_option
    "eval_query_term returns default source db"
    (Some (Result_db match_context.source_db))
    (Query.eval_query_term match_context [] (QSource "$"));
  assert_raises_invalid_arg_message
    "eval_query_term rejects named sources without source context"
    "source term requires query source context: other"
    (fun () -> ignore (Query.eval_query_term match_context [] (QSource "other")));
  assert_equal_query_option
    "eval_query_term drops wildcards"
    None
    (Query.eval_query_term match_context [] QWildcard);
  assert_equal_query_option
    "collect_query_terms evaluates all terms"
    (Some [ Result_value (String "kept"); Result_entity 42 ])
    (Query.collect_query_terms match_context base_binding [ QVar "existing"; QEntity 42 ]);
  assert_equal_query_option
    "collect_query_terms drops collections with wildcards"
    None
    (Query.collect_query_terms match_context base_binding [ QVar "existing"; QWildcard ]);
  assert_equal_query
    "collect_query_terms_exn returns evaluated terms"
    [ Result_value (String "kept"); Result_entity 42 ]
    (Query.collect_query_terms_exn match_context base_binding [ QVar "existing"; QEntity 42 ]);
  assert_raises_invalid_arg_message
    "collect_query_terms_exn rejects insufficient bindings"
    "insufficient bindings"
    (fun () -> ignore (Query.collect_query_terms_exn match_context base_binding [ QWildcard ]));
  assert_equal_int_option
    "query_term_entity_id returns entity ids for evaluated terms"
    (Some 42)
    (Query.query_term_entity_id match_context [] (QValue (Keyword "known-ident")))

let test_query_namespace__test_source_matching_helpers () =
  let result_resolution_context =
    { Query.validate_entity_id = (fun entity_id -> entity_id)
    ; resolve_query_value = (fun value -> Some value)
    ; lookup_ref_entity_id = (fun _ _ -> None)
    }
  in
  let match_context =
    { Query.result_resolution_context
    ; source_db = empty_db ()
    ; ident_entity_id = (fun _ -> None)
    ; unresolved_lookup_ref_message = (fun attr _ -> "missing lookup ref: " ^ attr)
    ; value_equal = Util.value_equal
    ; coerce_tuple_lookup_value = (fun _ value -> value)
    }
  in
  let name_datom = datom ~e:1 ~a:"name" ~v:(String "Ivan") ~tx:7 ~added:true () in
  let source_context =
    { Query.match_context
    ; pattern_datoms = (fun _ _ -> [ name_datom ])
    ; match_data_pattern =
        (fun _ bindings e_term a_term v_term datom ->
           Query.match_pattern_clause match_context bindings e_term a_term v_term datom)
    ; match_data_pattern_tx =
        (fun _ bindings e_term a_term v_term tx_term datom ->
           Query.match_pattern_tx_clause match_context bindings e_term a_term v_term tx_term datom)
    ; match_data_pattern_tx_op =
        (fun _ bindings e_term a_term v_term tx_term op_term datom ->
           let ( let* ) = Option.bind in
           let* bindings =
             Query.match_pattern_tx_clause match_context bindings e_term a_term v_term tx_term datom
           in
           Query.match_query_term match_context op_term (Query.result_of_datom_op datom) bindings)
    }
  in
  let root_db = empty_db () in
  let other_db = empty_db () in
  (match Query.source root_db [ "other", Db_source other_db ] "$" with
   | Db_source db when db == root_db -> ()
   | _ -> failwith "source should default $ to the root db");
  (match Query.source_db root_db [ "other", Db_source other_db ] "other" with
   | db when db == other_db -> ()
   | _ -> failwith "source_db should return named database sources");
  assert_raises_invalid_arg_message
    "source rejects unknown names"
    "unknown query source: missing"
    (fun () -> ignore (Query.source root_db [] "missing"));
  assert_raises_invalid_arg_message
    "source_db rejects relation sources"
    "query source is not a database: rows"
    (fun () -> ignore (Query.source_db root_db [ "rows", Relation_source [] ] "rows"));
  assert_equal_query_option
    "match_relation_row binds each relation column"
    (Some [ "age", Result_value (Int 42); "name", Result_value (String "Ivan") ])
    (Query.match_relation_row
       source_context
       []
       [ QVar "name"; QVar "age" ]
       [ Result_value (String "Ivan"); Result_value (Int 42) ]);
  assert_raises_invalid_arg_message
    "match_relation_row rejects short rows"
    "source relation row arity mismatch"
    (fun () ->
       ignore
         (Query.match_relation_row
            source_context
            []
            [ QVar "name"; QVar "age" ]
            [ Result_value (String "Ivan") ]));
  assert_equal_query_rows
    "match_query_source_pattern matches database source triples"
    [ [ "name", Result_value (String "Ivan"); "e", Result_entity 1 ] ]
    (Query.match_query_source_pattern
       source_context
       root_db
       (Db_source root_db)
       []
       [ QVar "e"; QAttr "name"; QVar "name" ]);
  assert_equal_query_rows
    "match_query_source_pattern matches relation source rows"
    [ [ "name", Result_value (String "Ivan") ] ]
    (Query.match_query_source_pattern
       source_context
       root_db
       (Relation_source [ [ Result_value (String "Ivan") ] ])
       []
       [ QVar "name" ]);
  assert_raises_invalid_arg_message
    "match_query_source_pattern rejects database arity mismatch"
    "database source patterns expect 3, 4, or 5 terms"
    (fun () ->
       ignore
         (Query.match_query_source_pattern
            source_context
            root_db
            (Db_source root_db)
            []
            [ QVar "e"; QAttr "name" ]));
  assert_equal_query_rows
    "match_relation_source_pattern expands short database entity patterns"
    [ [ "e", Result_entity 1 ] ]
    (Query.match_relation_source_pattern source_context root_db [] "$" [] [ QVar "e" ]);
  assert_equal_query_rows
    "match_relation_source_pattern coerces short database attr values"
    [ [ "e", Result_entity 1 ] ]
    (Query.match_relation_source_pattern
       source_context
       root_db
       []
       "$"
       []
       [ QVar "e"; QValue (Keyword "name") ]);
  assert_equal_query_rows
    "match_source_pattern uses named relation sources"
    [ [ "name", Result_value (String "Ivan") ] ]
    (Query.match_source_pattern
       source_context
       root_db
       [ "rows", Relation_source [ [ Result_value (String "Ivan") ] ] ]
       "rows"
       []
       [ QVar "name" ])

let test_query_namespace__test_aggregate_helpers () =
  if not (Query.has_aggregates [ Find_aggregate (Sum, [ QVar "amount" ]) ]) then
    failwith "Query.has_aggregates should detect aggregate find specs";
  if Query.has_aggregates [ Find_var "amount" ] then
    failwith "Query.has_aggregates should ignore non-aggregate find specs";
  assert_equal_aggregate
    "dynamic min amount resolves from the first group binding"
    (MinN 2)
    (Query.resolve_dynamic_aggregate
       (MinNVar "n")
       [ [ "n", Result_value (Int 2); "amount", Result_value (Int 10) ] ]);
  assert_raises_invalid_arg "dynamic aggregate amount must be non-negative" (fun () ->
    ignore (Query.resolve_dynamic_aggregate (SampleVar "n") [ [ "n", Result_value (Int (-1)) ] ]));
  assert_raises_invalid_arg "dynamic aggregate amount must be bound" (fun () ->
    ignore (Query.resolve_dynamic_aggregate (MaxNVar "n") []));
  assert_equal_string_list
    "aggregate_param_vars reports amount variables"
    [ "n" ]
    (Query.aggregate_param_vars (RandNVar "n"));
  assert_equal_string_list
    "aggregate_callable_vars reports custom aggregate variables"
    [ "agg" ]
    (Query.aggregate_callable_vars (CustomVar "agg"));
  assert_equal_terms
    "split_aggregate_terms returns extra args and the value term"
    ([ QVar "n"; QValue (String "tag") ], QVar "amount")
    (Query.split_aggregate_terms [ QVar "n"; QValue (String "tag"); QVar "amount" ]);
  assert_raises_invalid_arg "split_aggregate_terms rejects empty terms" (fun () ->
    ignore (Query.split_aggregate_terms []));
  assert_equal_query
    "custom aggregate receives extra args before values"
    [ Result_value (Int 9); Result_value (Int 1); Result_value (Int 2) ]
    (Query.aggregate_input_values
       (Custom (fun values -> Result_value (Int (List.length values))))
       [ Result_value (Int 9) ]
       [ Result_value (Int 1); Result_value (Int 2) ]);
  assert_equal_query
    "built-in aggregate ignores extra args after parse-time resolution"
    [ Result_value (Int 1); Result_value (Int 2) ]
    (Query.aggregate_input_values
       Sum
       [ Result_value (Int 9) ]
       [ Result_value (Int 1); Result_value (Int 2) ]);
  let match_context =
    { Query.result_resolution_context =
        { validate_entity_id = (fun entity_id -> entity_id)
        ; resolve_query_value = (fun value -> Some value)
        ; lookup_ref_entity_id = (fun _ _ -> None)
        }
    ; source_db = empty_db ()
    ; ident_entity_id = (fun _ -> None)
    ; unresolved_lookup_ref_message = (fun attr _ -> "missing lookup ref: " ^ attr)
    ; value_equal = Util.value_equal
    ; coerce_tuple_lookup_value = (fun _ value -> value)
    }
  in
  let default_db = empty_db () in
  let other_db = empty_db () in
  let sources = [ "other", Db_source other_db ] in
  (match Query.eval_query_term_with_sources match_context default_db sources [] (QSource "$") with
   | Some (Result_db db) when db == default_db -> ()
   | _ -> failwith "eval_query_term_with_sources should resolve the default source db");
  (match Query.eval_query_term_with_sources match_context default_db sources [] (QSource "other") with
   | Some (Result_db db) when db == other_db -> ()
   | _ -> failwith "eval_query_term_with_sources should resolve named source dbs");
  assert_equal_query_option
    "eval_query_term_with_sources delegates non-source terms"
    (Some (Result_value (String "Ivan")))
    (Query.eval_query_term_with_sources
       match_context
       default_db
       sources
       [ "name", Result_value (String "Ivan") ]
       (QVar "name"));
  assert_raises_invalid_arg_message
    "eval_query_term_with_sources rejects unknown sources"
    "unknown query source: missing"
    (fun () -> ignore (Query.eval_query_term_with_sources match_context default_db sources [] (QSource "missing")));
  assert_equal_query
    "collect_dynamic_query_terms_exn evaluates vars and sources"
    [ Result_value (String "Ivan"); Result_db other_db ]
    (Query.collect_dynamic_query_terms_exn
       match_context
       default_db
       sources
       [ "name", Result_value (String "Ivan") ]
       [ QVar "name"; QSource "other" ]);
  assert_raises_invalid_arg_message
    "collect_dynamic_query_terms_exn reports unbound terms"
    "unbound query variable"
    (fun () ->
       ignore
         (Query.collect_dynamic_query_terms_exn match_context default_db sources [] [ QVar "missing" ]));
  (match
     Query.aggregate_extra_args
       match_context
       default_db
       sources
       [ [ "n", Result_value (Int 2); "amount", Result_value (Int 10) ]
       ; [ "n", Result_value (Int 3); "amount", Result_value (Int 20) ]
       ]
       [ QVar "n"; QSource "other"; QVar "amount" ]
   with
   | [ Result_value (Int 2); Result_db db ] when db == other_db -> ()
   | _ -> failwith "aggregate_extra_args should use first group binding and resolve source args");
  assert_equal_query
    "aggregate_values evaluates the aggregate value term for every group binding"
    [ Result_value (Int 10); Result_value (Int 20) ]
    (Query.aggregate_values
       match_context
       default_db
       sources
       [ [ "amount", Result_value (Int 10) ]; [ "amount", Result_value (Int 20) ] ]
       [ QVar "amount" ]);
  assert_equal_query
    "aggregate_values drops bindings where the value term is unbound"
    [ Result_value (Int 10) ]
    (Query.aggregate_values
       match_context
       default_db
       sources
       [ [ "amount", Result_value (Int 10) ]; [ "name", Result_value (String "missing") ] ]
       [ QVar "amount" ]);
  assert_raises_invalid_arg_message
    "aggregate_extra_args rejects unbound extra args"
    "insufficient aggregate argument bindings"
    (fun () ->
       ignore
         (Query.aggregate_extra_args
            match_context
            default_db
            sources
            [ [ "amount", Result_value (Int 10) ] ]
            [ QVar "missing"; QVar "amount" ]))

let test_query_namespace__test_find_grouping_helpers () =
  let binding =
    [ "name", Result_value (String "Ivan")
    ; "age", Result_value (Int 30)
    ; "city", Result_value (String "Berlin")
    ]
  in
  assert_equal_query_option
    "collect_find_vars preserves requested order"
    (Some [ Result_value (Int 30); Result_value (String "Ivan") ])
    (Query.collect_find_vars binding [ "age"; "name" ]);
  assert_equal_query_option
    "collect_find_vars returns None when a requested var is missing"
    None
    (Query.collect_find_vars binding [ "age"; "missing" ]);
  assert_equal_grouped_bindings
    "group_by_key prepends later rows in the same group"
    [ ( [ Result_value (String "Ivan") ]
      , [ [ "age", Result_value (Int 31) ]; [ "age", Result_value (Int 30) ] ] )
    ; ( [ Result_value (String "Oleg") ]
      , [ [ "age", Result_value (Int 40) ] ] )
    ]
    (Query.group_by_key
       [ [ Result_value (String "Ivan") ], [ "age", Result_value (Int 30) ]
       ; [ Result_value (String "Oleg") ], [ "age", Result_value (Int 40) ]
       ; [ Result_value (String "Ivan") ], [ "age", Result_value (Int 31) ]
       ]);
  assert_equal_string_list
    "grouping_vars_of_find includes non-aggregate find vars"
    [ "city"; "entity"; "pattern" ]
    (Query.grouping_vars_of_find
       [ Find_var "city"
       ; Find_pull ("entity", [ Pull_id ])
       ; Find_pull_var ("entity", "pattern")
       ; Find_aggregate (Count, [ QVar "age" ])
       ])

let test_query_namespace__test_input_label_helpers () =
  assert_equal_string "query_input_var_label adds a question mark" "?name" (Query.query_input_var_label "name");
  assert_equal_string
    "query_input_var_label preserves existing query prefixes"
    "$source"
    (Query.query_input_var_label "$source");
  assert_equal_string
    "query_input_binding_string formats nested tuple bindings"
    "[?name [_ ...] [?city ?country]]"
    (Query.query_input_binding_string
       (Bind_tuple
          [ Bind_scalar "name"
          ; Bind_collection Bind_ignore
          ; Bind_tuple [ Bind_scalar "city"; Bind_scalar "?country" ]
          ]));
  assert_equal_string
    "query_input_decl_binding_string formats relation declarations"
    "[[?name ?age]]"
    (Query.query_input_decl_binding_string (Input_relation_decl [ "name"; "age" ]));
  assert_equal_string
    "query_input_binding_label formats rules declarations"
    "%"
    (Query.query_input_binding_label Input_rules_decl);
  assert_equal_string
    "query_input_binding_label formats bound scalar inputs"
    "?name"
    (Query.query_input_binding_label (Input_scalar ("name", Result_value (String "Ivan"))));
  if not (Query.query_input_consumes_argument ~consume_rules:true Input_rules_decl) then
    failwith "rules input should consume an argument when requested";
  if Query.query_input_consumes_argument ~consume_rules:false Input_rules_decl then
    failwith "rules input should not consume an argument when rules are implicit";
  if not (Query.query_input_consumes_argument ~consume_rules:false (Input_tuple_decl [ "name" ])) then
    failwith "tuple declarations should consume query input arguments";
  if Query.query_input_consumes_argument ~consume_rules:true (Input_source_decl "$other") then
    failwith "source declarations should not consume query input arguments"

let test_query_namespace__test_input_shape_helpers () =
  assert_equal_query_option
    "values_of_collection_result unwraps vectors"
    (Some [ Result_value (String "a"); Result_value (String "b") ])
    (Query.values_of_collection_result (Result_value (Vector [ String "a"; String "b" ])));
  assert_equal_query_option
    "values_of_collection_result drops tuple nil slots"
    (Some [ Result_value (Int 1); Result_value (Int 3) ])
    (Query.values_of_collection_result (Result_value (Tuple [ Some (Int 1); None; Some (Int 3) ])));
  assert_equal_query_option
    "values_of_collection_result rejects scalar values"
    None
    (Query.values_of_collection_result (Result_value (String "not-a-collection")));
  assert_equal_query
    "row_of_collection_result preserves tuple nil slots"
    [ Result_value (Int 1); Result_value Nil; Result_value (Int 3) ]
    (Query.row_of_collection_result (Result_value (Tuple [ Some (Int 1); None; Some (Int 3) ])));
  assert_equal_query
    "row_of_collection_result wraps scalar values"
    [ Result_value (String "scalar") ]
    (Query.row_of_collection_result (Result_value (String "scalar")));
  assert_equal_query
    "row_of_scalar_sequence unwraps scalar sequence values"
    [ Result_value (Keyword "left"); Result_value (Keyword "right") ]
    (Query.row_of_scalar_sequence (Result_value (List [ Keyword "left"; Keyword "right" ])));
  assert_raises_invalid_arg "row_of_scalar_sequence rejects non-sequence scalars" (fun () ->
    ignore (Query.row_of_scalar_sequence (Result_value (Keyword "value"))));
  assert_equal_query_rows
    "rows_of_map_entries converts map entries to relation rows"
    [ [ Result_value (Keyword "a"); Result_value (Int 1) ]
    ; [ Result_value (Keyword "b"); Result_value (Vector [ Int 2; Int 3 ]) ]
    ]
    (Query.rows_of_map_entries
       [ Keyword "a", Int 1; Keyword "b", Vector [ Int 2; Int 3 ] ])

let test_query_namespace__test_input_binding_helpers () =
  let input_context =
    { Query.resolve_query_input_result =
        (function
          | Result_value (Keyword "drop") -> None
          | result -> Some result)
    ; bind_var =
        (fun var value bindings ->
           Query.bind_var
             { validate_entity_id = (fun entity_id -> entity_id)
             ; resolve_query_value = (fun value -> Some value)
             ; lookup_ref_entity_id = (fun _ _ -> None)
             }
             var
             value
             bindings)
    ; entity_id_of_ref =
        (function
          | Ident "known" -> Some 42
          | _ -> None)
    }
  in
  assert_equal_query_option
    "bind_relation_row binds relation columns"
    (Some [ "age", Result_value (Int 30); "name", Result_value (String "Ivan") ])
    (Query.bind_relation_row
       input_context
       []
       [ "name"; "age" ]
       [ Result_value (String "Ivan"); Result_value (Int 30) ]);
  assert_raises_invalid_arg_message
    "bind_relation_row rejects row arity mismatch"
    "relation input row arity mismatch"
    (fun () -> ignore (Query.bind_relation_row input_context [] [ "name" ] []));
  assert_equal_query_rows
    "apply_query_input binds scalar inputs"
    [ [ "name", Result_value (String "Ivan") ] ]
    (Query.apply_query_input input_context [ [] ] (Input_scalar ("name", Result_value (String "Ivan"))));
  assert_equal_query_rows
    "apply_query_input binds entity ref inputs"
    [ [ "e", Result_entity 42 ] ]
    (Query.apply_query_input input_context [ [] ] (Input_entity_ref ("e", Ident "known")));
  assert_equal_query_rows
    "apply_query_input expands relation inputs"
    [ [ "age", Result_value (Int 30); "name", Result_value (String "Ivan") ]
    ; [ "age", Result_value (Int 40); "name", Result_value (String "Oleg") ]
    ]
    (Query.apply_query_input
       input_context
       [ [] ]
       (Input_relation
          ( [ "name"; "age" ]
          , [ [ Result_value (String "Ivan"); Result_value (Int 30) ]
            ; [ Result_value (String "Oleg"); Result_value (Int 40) ]
            ] )));
  let query_input_of_arg decl arg =
    match decl, arg with
    | Input_ignore_decl, _ -> Input_ignore
    | Input_scalar_decl var, Arg_scalar value -> Input_scalar (var, value)
    | Input_tuple_decl vars, Arg_tuple row -> Input_tuple (vars, row)
    | Input_rules_decl, Arg_rules rules -> Input_rules rules
    | _ -> invalid_arg "test conversion does not support this binding"
  in
  assert_equal_inputs
    "bind_query_inputs skips source declarations and binds scalar args"
    [ Input_source_decl "$other"; Input_scalar ("name", Result_value (String "Ivan")) ]
    (Query.bind_query_inputs
       ~query_input_of_arg
       ~consume_rules:false
       [ Input_source_decl "$other"; Input_scalar_decl "name" ]
       [ Arg_scalar (Result_value (String "Ivan")) ]);
  assert_equal_inputs
    "bind_query_inputs preserves already bound inputs"
    [ Input_collection_ignore [ Result_value (Int 1) ]; Input_ignore ]
    (Query.bind_query_inputs
       ~query_input_of_arg
       ~consume_rules:false
       [ Input_collection_ignore [ Result_value (Int 1) ]; Input_ignore_decl ]
       [ Arg_scalar (Result_value (String "ignored")) ]);
  assert_equal_inputs
    "bind_query_inputs skips rules declarations when rules are implicit"
    [ Input_rules_decl; Input_scalar ("name", Result_value (String "Oleg")) ]
    (Query.bind_query_inputs
       ~query_input_of_arg
       ~consume_rules:false
       [ Input_rules_decl; Input_scalar_decl "name" ]
       [ Arg_scalar (Result_value (String "Oleg")) ]);
  assert_equal_inputs
    "bind_query_inputs consumes rules declarations when requested"
    [ Input_rules []; Input_scalar ("name", Result_value (String "Oleg")) ]
    (Query.bind_query_inputs
       ~query_input_of_arg
       ~consume_rules:true
       [ Input_rules_decl; Input_scalar_decl "name" ]
       [ Arg_rules []; Arg_scalar (Result_value (String "Oleg")) ]);
  assert_raises_invalid_arg "bind_query_inputs rejects too few args" (fun () ->
    ignore
      (Query.bind_query_inputs
         ~query_input_of_arg
         ~consume_rules:false
         [ Input_scalar_decl "name" ]
         []));
  assert_raises_invalid_arg "bind_query_inputs rejects too many args" (fun () ->
    ignore
      (Query.bind_query_inputs
         ~query_input_of_arg
         ~consume_rules:false
         [ Input_scalar_decl "name" ]
         [ Arg_scalar (Result_value (String "Ivan")); Arg_scalar (Result_value (String "extra")) ]))

let test_query_namespace__test_callable_helpers () =
  let predicate = function
    | [ Result_value (Int value) ] -> value > 10
    | _ -> false
  in
  let query_function = function
    | [ Result_value (Int value) ] -> Some [ Result_value (Int (value + 1)) ]
    | _ -> None
  in
  let aggregate = function
    | values -> Result_value (Int (List.length values))
  in
  let callables =
    Query.empty_query_callables
    |> fun callables -> { callables with Query.callable_predicates = [ "large?", predicate ] }
    |> fun callables -> { callables with Query.callable_functions = [ "inc", query_function ] }
    |> fun callables -> { callables with Query.callable_aggregates = [ "count-values", aggregate ] }
    |> fun callables -> Query.alias_callable callables "bigger?" "large?"
  in
  (match Query.callable_predicate callables "bigger?" with
   | Some f ->
     if not (f [ Result_value (Int 11) ]) then failwith "callable_predicate should resolve aliases"
   | None -> failwith "callable_predicate should find aliased predicates");
  (match Query.callable_function callables "inc" with
   | Some f ->
     if f [ Result_value (Int 1) ] <> Some [ Result_value (Int 2) ] then
       failwith "callable_function should return stored functions"
   | None -> failwith "callable_function should find stored functions");
  (match Query.resolve_callable_aggregate callables (CustomVar "count-values") with
   | Custom f ->
     if f [ Result_value (Int 1); Result_value (Int 2) ] <> Result_value (Int 2) then
       failwith "resolve_callable_aggregate should return stored aggregate functions"
   | _ -> failwith "resolve_callable_aggregate should resolve custom aggregate variables");
  if not (Query.has_callable callables "bigger?") then failwith "has_callable should resolve aliases";
  assert_raises_invalid_arg "resolve_callable_aggregate rejects unknown custom aggregates" (fun () ->
    ignore (Query.resolve_callable_aggregate callables (CustomVar "missing")));
  let rule = { rule_name = "parent"; rule_params = [ "e" ]; rule_body = [] } in
  assert_equal_rules
    "query_rules_of_inputs extracts supplied rules"
    [ rule ]
    (Query.query_rules_of_inputs [ Input_rules [ rule ]; Input_ignore ]);
  let callables =
    Query.query_callables_of_inputs
      [ Input_predicate ("large?", predicate)
      ; Input_function ("inc", query_function)
      ; Input_aggregate ("count-values", aggregate)
      ; Input_ignore
      ]
  in
  if not (Query.has_callable callables "large?") then
    failwith "query_callables_of_inputs should collect predicate inputs";
  if not (Query.has_callable callables "inc") then
    failwith "query_callables_of_inputs should collect function inputs";
  if not (Query.has_callable callables "count-values") then
    failwith "query_callables_of_inputs should collect aggregate inputs"

let test_query_namespace__test_rule_helpers () =
  let parent_1 = { rule_name = "parent"; rule_params = [ "e" ]; rule_body = [] } in
  let parent_2 = { rule_name = "parent"; rule_params = [ "e"; "child" ]; rule_body = [] } in
  let ancestor = { rule_name = "ancestor"; rule_params = [ "e"; "child" ]; rule_body = [] } in
  assert_equal_rules
    "matching_rules filters by name and arity"
    [ parent_2 ]
    (Query.matching_rules [ parent_1; parent_2; ancestor ] "parent" 2);
  assert_equal_rules
    "matching_rules_exn returns matching rules"
    [ ancestor ]
    (Query.matching_rules_exn [ parent_1; parent_2; ancestor ] "ancestor" 2);
  assert_raises_invalid_arg "matching_rules_exn rejects missing rules" (fun () ->
    ignore (Query.matching_rules_exn [ parent_1 ] "missing" 1));
  assert_equal_grouped_bindings
    "project_binding keeps only requested vars"
    [ [ "name", Result_value (String "Ivan"); "age", Result_value (Int 30) ] ]
    [ Query.project_binding
        [ "name"; "age" ]
        [ "name", Result_value (String "Ivan")
        ; "city", Result_value (String "Berlin")
        ; "age", Result_value (Int 30)
        ]
    ];
  let predicate = function
    | [ Result_value (Int value) ] -> value > 10
    | _ -> false
  in
  let callables =
    Query.empty_query_callables
    |> fun callables -> { callables with Query.callable_predicates = [ "large?", predicate ] }
  in
  let aliased =
    Query.rule_invocation_callables
      callables
      []
      { rule_name = "large-rule"; rule_params = [ "p" ]; rule_body = [] }
      [ QVar "large?" ]
  in
  if not (Query.has_callable aliased "p") then
    failwith "rule_invocation_callables should alias unbound callable args to rule params";
  let unchanged =
    Query.rule_invocation_callables
      callables
      [ "large?", Result_value (Bool true) ]
      { rule_name = "large-rule"; rule_params = [ "p" ]; rule_body = [] }
      [ QVar "large?" ]
  in
  if Query.has_callable unchanged "p" then
    failwith "rule_invocation_callables should not alias already-bound vars";
  if not (Query.clause_calls_rule "parent" (Rule ("parent", [ QVar "e" ]))) then
    failwith "clause_calls_rule should detect direct rule calls";
  if not (Query.clause_calls_rule "parent" (SourceRule ("other", "parent", [ QVar "e" ]))) then
    failwith "clause_calls_rule should detect sourced rule calls";
  if
    not
      (Query.clause_calls_rule
         "parent"
         (Not [ SourceClause ("other", Rule ("parent", [ QVar "e" ])) ]))
  then
    failwith "clause_calls_rule should recurse through not/source clauses";
  if
    not
      (Query.clause_calls_rule
         "parent"
         (OrJoinRequired
            ( [ "e" ]
            , [ "name" ]
            , [ [ Pattern (QVar "e", QAttr "name", QVar "name") ]
              ; [ Rule ("parent", [ QVar "e" ]) ]
              ] )))
  then
    failwith "clause_calls_rule should recurse through or-join branches";
  if Query.clause_calls_rule "parent" (DynamicPredicate ("parent", [ QVar "e" ])) then
    failwith "clause_calls_rule should ignore predicate names";
  let recursive_parent =
    { rule_name = "parent"
    ; rule_params = [ "e"; "child" ]
    ; rule_body =
        [ Rule ("parent", [ QVar "e"; QVar "middle" ])
        ; Rule ("parent", [ QVar "middle"; QVar "child" ])
        ]
    }
  in
  let terminal_parent =
    { rule_name = "parent"
    ; rule_params = [ "e"; "child" ]
    ; rule_body = [ Pattern (QVar "e", QAttr "parent", QVar "child") ]
    }
  in
  let rule_call_key = "", "parent", [ Some (Result_entity 1); Some (Result_entity 2) ] in
  assert_equal_rules
    "matching_rules_for_call returns all candidates when call is not active"
    [ recursive_parent; terminal_parent ]
    (Query.matching_rules_for_call
       []
       rule_call_key
       [ recursive_parent; terminal_parent ]
       "parent"
       2);
  assert_equal_rules
    "matching_rules_for_call filters recursive candidates when call is active"
    [ terminal_parent ]
    (Query.matching_rules_for_call
       [ rule_call_key ]
       rule_call_key
       [ recursive_parent; terminal_parent ]
       "parent"
       2)

let test_query_namespace__test_variable_discovery_helpers () =
  assert_equal_string_list
    "vars_of_query_term returns vars only"
    [ "e" ]
    (Query.vars_of_query_term (QVar "e"));
  assert_equal_string_list
    "vars_of_query_term ignores literals"
    []
    (Query.vars_of_query_term (QValue (String "Ivan")));
  assert_equal_string_list
    "vars_of_query_terms sorts and deduplicates vars"
    [ "a"; "e"; "v" ]
    (Query.vars_of_query_terms [ QVar "v"; QVar "e"; QVar "v"; QVar "a"; QWildcard ]);
  assert_equal_string_list
    "vars_of_clause includes data pattern vars"
    [ "a"; "e"; "v" ]
    (Query.vars_of_clause (Pattern (QVar "e", QVar "a", QVar "v")));
  assert_equal_string_list
    "vars_of_clause includes function outputs and inputs"
    [ "out"; "x"; "y" ]
    (Query.vars_of_clause (Function ("f", [ QVar "x"; QVar "y" ], [ "out" ], fun _ -> None)));
  assert_equal_string_list
    "vars_of_clause drops ignored ground outputs"
    [ "value"; "source" ]
    (Query.vars_of_clause (GroundTermTuple (QVar "source", [ "_"; "value" ])));
  assert_equal_string_list
    "vars_of_clause includes not-join vars and body vars"
    [ "e"; "name" ]
    (Query.vars_of_clause
       (NotJoin ([ "e" ], [ Pattern (QVar "e", QAttr "name", QVar "name") ])));
  assert_equal_string_list
    "vars_of_clause includes required or-join vars and branch vars"
    [ "e"; "name"; "other" ]
    (Query.vars_of_clause
       (OrJoinRequired
          ( [ "e" ]
          , [ "other" ]
          , [ [ Pattern (QVar "e", QAttr "name", QVar "name") ]
            ; [ Pattern (QVar "other", QAttr "name", QVar "name") ]
            ] )));
  assert_equal_string_list
    "vars_of_clause delegates through source clauses"
    [ "e"; "v" ]
    (Query.vars_of_clause (SourceClause ("$", Pattern (QVar "e", QAttr "name", QVar "v"))))

let test_query_namespace__test_source_discovery_helpers () =
  assert_equal_string_list
    "named_source returns a singleton source list"
    [ "other" ]
    (Query.named_source "other");
  assert_equal_string_list
    "sources_of_query_term returns only source terms"
    [ "other" ]
    (Query.sources_of_query_term (QSource "other"));
  assert_equal_string_list
    "sources_of_query_terms preserves repeated source references"
    [ "a"; "b"; "a" ]
    (Query.sources_of_query_terms [ QSource "a"; QVar "ignored"; QSource "b"; QSource "a" ]);
  assert_equal_string_list
    "sources_of_optional_query_term handles optional terms"
    [ "other" ]
    (Query.sources_of_optional_query_term (Some (QSource "other")));
  assert_equal_string_list
    "sources_of_optional_query_term returns empty sources for None"
    []
    (Query.sources_of_optional_query_term None);
  assert_equal_string_list
    "sources_of_clause includes explicit sources and nested term sources"
    [ "people"; "needle" ]
    (Query.sources_of_clause
       (SourcePattern ("people", QVar "e", QAttr "name", QSource "needle")));
  assert_equal_string_list
    "sources_of_clause recurses through branch clauses"
    [ "outer"; "inner"; "dynamic" ]
    (Query.sources_of_clause
       (SourceOr
          ( "outer"
          , [ [ Pattern (QSource "inner", QAttr "name", QVar "name") ]
            ; [ DynamicPredicate ("pred", [ QSource "dynamic" ]) ]
            ] )));
  assert_equal_string_list
    "sources_of_find_spec includes pull sources"
    [ "pull-db" ]
    (Query.sources_of_find_spec (Find_pull_source ("pull-db", "e", [ Pull_id ])));
  assert_equal_string_list
    "sources_of_find_spec includes aggregate term sources"
    [ "amounts" ]
    (Query.sources_of_find_spec (Find_aggregate (Sum, [ QSource "amounts"; QVar "amount" ])))

let test_query_namespace__test_rule_source_analysis_helpers () =
  if not (Query.has_rule_clause (Rule ("parent", [ QVar "e" ]))) then
    failwith "has_rule_clause should detect direct rule clauses";
  if
    not
      (Query.has_rule_clause
         (SourceOr
            ( "other"
            , [ [ Pattern (QVar "e", QAttr "name", QVar "name") ]
              ; [ SourceRule ("other", "parent", [ QVar "e" ]) ]
              ] )))
  then
    failwith "has_rule_clause should recurse through source/or branches";
  if Query.has_rule_clause (DynamicPredicate ("parent", [ QVar "e" ])) then
    failwith "has_rule_clause should ignore dynamic predicates before rule resolution";
  assert_equal_string_list
    "rule_names sorts and deduplicates rule names"
    [ "ancestor"; "parent" ]
    (Query.rule_names
       [ { rule_name = "parent"; rule_params = [ "e" ]; rule_body = [] }
       ; { rule_name = "ancestor"; rule_params = [ "e" ]; rule_body = [] }
       ; { rule_name = "parent"; rule_params = [ "e"; "child" ]; rule_body = [] }
       ]);
  if
    Query.resolve_dynamic_rule_clause [ "parent" ] (DynamicPredicate ("parent", [ QVar "e" ]))
    <> Rule ("parent", [ QVar "e" ])
  then
    failwith "resolve_dynamic_rule_clause should resolve matching dynamic predicates";
  if
    Query.resolve_dynamic_rule_clause
      [ "parent" ]
      (SourceClause ("other", DynamicPredicate ("parent", [ QVar "e" ])))
    <> SourceRule ("other", "parent", [ QVar "e" ])
  then
    failwith "resolve_dynamic_rule_clause should preserve sources for resolved rules";
  if
    Query.resolve_dynamic_rule_clause
      [ "parent" ]
      (Not [ Or [ [ DynamicPredicate ("parent", [ QVar "e" ]) ] ] ])
    <> Not [ Or [ [ Rule ("parent", [ QVar "e" ]) ] ] ]
  then
    failwith "resolve_dynamic_rule_clause should recurse through nested branches";
  if
    Query.resolve_dynamic_rule_clause [ "parent" ] (DynamicPredicate ("large?", [ QVar "e" ]))
    <> DynamicPredicate ("large?", [ QVar "e" ])
  then
    failwith "resolve_dynamic_rule_clause should leave non-rule predicates unchanged";
  let rule =
    { rule_name = "ancestor"
    ; rule_params = [ "e"; "child" ]
    ; rule_body =
        [ DynamicPredicate ("parent", [ QVar "e"; QVar "child" ])
        ; DynamicPredicate ("large?", [ QVar "child" ])
        ]
    }
  in
  assert_equal_rules
    "resolve_dynamic_rule resolves every clause in a rule body"
    [ { rule with
        rule_body =
          [ Rule ("parent", [ QVar "e"; QVar "child" ])
          ; DynamicPredicate ("large?", [ QVar "child" ])
          ]
      }
    ]
    [ Query.resolve_dynamic_rule [ "parent" ] rule ];
  if not (Query.find_spec_uses_default_source (Find_pull_source ("$", "e", [ Pull_id ]))) then
    failwith "find_spec_uses_default_source should detect explicit default pull sources";
  if Query.find_spec_uses_default_source (Find_pull_source ("other", "e", [ Pull_id ])) then
    failwith "find_spec_uses_default_source should ignore named pull sources";
  if Query.find_spec_uses_default_source (Find_pull ("e", [ Pull_id ])) then
    failwith "find_spec_uses_default_source should ignore implicit pull specs";
  if not (Query.clause_uses_default_source (Pattern (QVar "e", QAttr "name", QVar "name"))) then
    failwith "clause_uses_default_source should treat bare patterns as default-source clauses";
  if
    Query.clause_uses_default_source
      (SourceClause ("other", SourcePattern ("other", QVar "e", QAttr "name", QVar "name")))
  then
    failwith "clause_uses_default_source should ignore named-only source clauses";
  if
    not
      (Query.clause_uses_default_source
         (SourceNot
            ( "other"
            , [ SourcePattern ("other", QVar "e", QAttr "name", QVar "name")
              ; Pattern (QVar "e", QAttr "age", QVar "age")
              ] )))
  then
    failwith "clause_uses_default_source should recurse through nested clauses";
  assert_equal_inputs
    "infer_default_inputs adds default source for queries without explicit :in"
    [ Input_source_decl "$"; Input_scalar_decl "name" ]
    (Query.infer_default_inputs
       None
       [ Find_pull_source ("$", "e", [ Pull_id ]) ]
       []
       [ Input_scalar_decl "name" ]);
  assert_equal_inputs
    "infer_default_inputs does not add default source when :in is explicit"
    [ Input_scalar_decl "name" ]
    (Query.infer_default_inputs
       (Some (QueryFormVector []))
       [ Find_pull_source ("$", "e", [ Pull_id ]) ]
       []
       [ Input_scalar_decl "name" ]);
  assert_equal_inputs
    "infer_default_inputs leaves named-source-only queries unchanged"
    [ Input_source_decl "$other" ]
    (Query.infer_default_inputs
       None
       []
       [ SourcePattern ("other", QVar "e", QAttr "name", QVar "name") ]
       [ Input_source_decl "$other" ])

let test_query_namespace__test_query_validation_helpers () =
  assert_equal_string_list
    "query_term_vars preserves query var order and duplicates"
    [ "e"; "e"; "name" ]
    (Query.query_term_vars [ QVar "e"; QSource "other"; QVar "e"; QVar "name"; QWildcard ]);
  assert_equal_string_list
    "vars_of_find_spec includes aggregate input vars and dynamic aggregate vars"
    [ "amount"; "n" ]
    (Query.vars_of_find_spec (Find_aggregate (MinNVar "n", [ QVar "amount" ])));
  assert_equal_string_list
    "vars_of_input_binding walks nested bindings"
    [ "name"; "city"; "country" ]
    (Query.vars_of_input_binding
       (Bind_tuple
          [ Bind_scalar "name"
          ; Bind_ignore
          ; Bind_collection (Bind_tuple [ Bind_scalar "city"; Bind_scalar "country" ])
          ]));
  assert_equal_string_list
    "vars_of_input drops ignored tuple columns"
    [ "name"; "age" ]
    (Query.vars_of_input (Input_relation_decl [ "name"; "_"; "age" ]));
  assert_equal_string_list
    "vars_of_input extracts nested tuple bindings"
    [ "name"; "city" ]
    (Query.vars_of_input
       (Input_nested_tuple_decl [ Bind_scalar "name"; Bind_collection (Bind_scalar "city") ]));
  (match Query.source_of_input (Input_source_decl "$other") with
   | Some "$other" -> ()
   | _ -> failwith "source_of_input should return declared source names");
  (match Query.source_of_input (Input_scalar_decl "name") with
   | None -> ()
   | Some _ -> failwith "source_of_input should ignore non-source inputs");
  Query.ensure_distinct_input_vars [ Input_scalar_decl "name"; Input_relation_decl [ "age" ] ];
  assert_raises_invalid_arg_message
    "ensure_distinct_input_vars rejects repeated vars"
    "Vars used in :in should be distinct"
    (fun () -> Query.ensure_distinct_input_vars [ Input_scalar_decl "name"; Input_tuple_decl [ "name" ] ]);
  Query.ensure_distinct_input_sources [ Input_source_decl "$"; Input_source_decl "$other" ];
  assert_raises_invalid_arg_message
    "ensure_distinct_input_sources rejects repeated sources"
    "Vars used in :in should be distinct"
    (fun () -> Query.ensure_distinct_input_sources [ Input_source_decl "$"; Input_source_decl "$" ]);
  assert_equal_string "format_query_vars prints query vars" "[?age ?name]" (Query.format_query_vars [ "age"; "name" ]);
  assert_equal_string "format_source_vars prints source vars" "[$ $other]" (Query.format_source_vars [ "$"; "other" ]);
  let valid_query =
    { find = [ Find_var "e" ]
    ; inputs = [ Input_source_decl "$" ]
    ; with_vars = [ "name" ]
    ; rules = []
    ; where = [ Pattern (QVar "e", QAttr "name", QVar "name") ]
    }
  in
  if Query.validate_query valid_query <> valid_query then
    failwith "validate_query should return valid queries unchanged";
  assert_raises_invalid_arg_message
    "validate_query rejects unknown find vars"
    "Query for unknown vars: [?missing]"
    (fun () -> ignore (Query.validate_query { valid_query with find = [ Find_var "missing" ] }));
  assert_raises_invalid_arg_message
    "validate_query rejects unknown with vars"
    "Query for unknown vars: [?missing]"
    (fun () -> ignore (Query.validate_query { valid_query with with_vars = [ "missing" ] }));
  assert_raises_invalid_arg_message
    "validate_query rejects shared find and with vars"
    ":find and :with should not use same variables: [?e]"
    (fun () -> ignore (Query.validate_query { valid_query with with_vars = [ "e" ] }));
  assert_raises_invalid_arg_message
    "validate_query rejects undeclared sources"
    "Where uses unknown source vars: [$other]"
    (fun () ->
       ignore
         (Query.validate_query
            { valid_query with
              inputs = [ Input_source_decl "$" ]
            ; where = [ SourcePattern ("other", QVar "e", QAttr "name", QVar "name") ]
            }))

let test_query_namespace__test_return_map_validation_helpers () =
  assert_equal_string "return_map_name formats :keys" "keys" (Query.return_map_name (Return_keys [ "name" ]));
  if Query.return_map_label_count (Return_syms [ "name"; "age" ]) <> 2 then
    failwith "return_map_label_count should count labels";
  let query =
    { find = [ Find_var "name"; Find_var "age" ]
    ; inputs = [ Input_source_decl "$" ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ Pattern (QVar "e", QAttr "name", QVar "name")
        ; Pattern (QVar "e", QAttr "age", QVar "age")
        ]
    }
  in
  if Query.validate_query_return_map Return_relation None query <> None then
    failwith "validate_query_return_map should preserve absent return maps";
  if
    Query.validate_query_return_map Return_tuple (Some (Return_strs [ "name"; "age" ])) query
    <> Some (Return_strs [ "name"; "age" ])
  then
    failwith "validate_query_return_map should return valid return maps";
  assert_raises_invalid_arg_message
    "validate_query_return_map rejects collection returns"
    ":keys does not work with collection :find"
    (fun () ->
       ignore (Query.validate_query_return_map Return_collection (Some (Return_keys [ "name"; "age" ])) query));
  assert_raises_invalid_arg_message
    "validate_query_return_map rejects scalar returns"
    ":syms does not work with single-scalar :find"
    (fun () ->
       ignore (Query.validate_query_return_map Return_scalar (Some (Return_syms [ "name"; "age" ])) query));
  assert_raises_invalid_arg_message
    "validate_query_return_map rejects label count mismatch"
    "Count of :strs must match count of :find"
    (fun () ->
       ignore (Query.validate_query_return_map Return_relation (Some (Return_strs [ "name" ])) query))

let test_query_namespace__test_query_string_helpers () =
  let value_to_string = function
    | String value -> "\"" ^ value ^ "\""
    | Keyword value -> ":" ^ value
    | Bool value -> if value then "true" else "false"
    | Int value -> string_of_int value
    | value -> failf "unexpected value in test printer: %s" (string_of_int (Hashtbl.hash value))
  in
  assert_equal_string
    "query_term_string formats vars"
    "?name"
    (Query.query_term_string ~value_to_string (QVar "name"));
  assert_equal_string
    "query_term_string formats lookup refs through the value printer"
    "[:user/email \"a@example.com\"]"
    (Query.query_term_string
       ~value_to_string
       (QLookupRef ("user/email", String "a@example.com")));
  assert_equal_string
    "query_term_string formats named sources"
    "$other"
    (Query.query_term_string ~value_to_string (QSource "other"));
  assert_equal_string
    "query_output_var_string preserves wildcards"
    "_"
    (Query.query_output_var_string "_");
  assert_equal_string
    "query_output_binding_string formats tuple outputs"
    "[?name _]"
    (Query.query_output_binding_string [ "name"; "_" ]);
  assert_equal_string
    "query_call_string formats callable invocations"
    "(get ?profile :prefs)"
    (Query.query_call_string
       ~value_to_string
       "get"
       [ QVar "profile"; QValue (Keyword "prefs") ]);
  assert_equal_string
    "numeric_predicate_symbol formats odd?"
    "odd?"
    (Query.numeric_predicate_symbol OddInteger);
  assert_equal_string
    "arithmetic_op_symbol formats modulo"
    "mod"
    (Query.arithmetic_op_symbol ModuloNumbers)

let test_query_namespace__test_query_clause_string_helpers () =
  let value_to_string = function
    | String value -> "\"" ^ value ^ "\""
    | Keyword value -> ":" ^ value
    | Bool value -> if value then "true" else "false"
    | Int value -> string_of_int value
    | value -> failf "unexpected value in test printer: %s" (string_of_int (Hashtbl.hash value))
  in
  assert_equal_string
    "query_clause_string formats data patterns"
    "[?e :name \"Ivan\"]"
    (Query.query_clause_string
       ~value_to_string
       (Pattern (QVar "e", QAttr "name", QValue (String "Ivan"))));
  assert_equal_string
    "query_clause_string formats source relation patterns"
    "[$other ?name :active]"
    (Query.query_clause_string
       ~value_to_string
       (SourceRelationPattern ("other", [ QVar "name"; QValue (Keyword "active") ])));
  assert_equal_string
    "query_clause_string formats dynamic collection functions"
    "[(children ?e) [?child ...]]"
    (Query.query_clause_string
       ~value_to_string
       (DynamicFunctionCollection ("children", [ QVar "e" ], "child")));
  assert_equal_string
    "query_clause_string formats dynamic relation functions"
    "[(pairs ?e) [[?left _]]]"
    (Query.query_clause_string
       ~value_to_string
       (DynamicFunctionRelation ("pairs", [ QVar "e" ], [ "left"; "_" ])));
  assert_equal_string
    "query_clause_string formats not clauses"
    "(not [?e :hidden true])"
    (Query.query_clause_string
       ~value_to_string
       (Not [ Pattern (QVar "e", QAttr "hidden", QValue (Bool true)) ]));
  assert_equal_string
    "query_clause_string formats required or-join vars"
    "(or-join [[?e] ?name] [?e :name ?name] (and [?other :name ?name] [?other :kind :person]))"
    (Query.query_clause_string
       ~value_to_string
       (OrJoinRequired
          ( [ "e" ]
          , [ "name" ]
          , [ [ Pattern (QVar "e", QAttr "name", QVar "name") ]
            ; [ Pattern (QVar "other", QAttr "name", QVar "name")
              ; Pattern (QVar "other", QAttr "kind", QValue (Keyword "person"))
              ]
            ] )));
  assert_equal_string
    "query_clause_string formats unknown clauses by var count"
    "<2-var clause>"
    (Query.query_clause_string
       ~value_to_string
       (NotJoin ([ "e" ], [ Pattern (QVar "e", QAttr "name", QVar "name") ])));
  assert_equal_string
    "query_var_set_string formats query variables"
    "#{?a ?b}"
    (Query.query_var_set_string [ "a"; "b" ]);
  assert_equal_string
    "query_var_sets_string formats query var set collections"
    "[#{?a} #{}]"
    (Query.query_var_sets_string [ [ "a" ]; [] ])

let test_query_namespace__test_binding_validation_helpers () =
  let value_to_string = function
    | String value -> "\"" ^ value ^ "\""
    | Keyword value -> ":" ^ value
    | Bool value -> if value then "true" else "false"
    | Int value -> string_of_int value
    | value -> failf "unexpected value in test printer: %s" (string_of_int (Hashtbl.hash value))
  in
  let binding = [ "e", Result_entity 1 ] in
  assert_equal_string_list
    "unbound_vars_of_terms returns sorted missing vars"
    [ "a"; "v" ]
    (Query.unbound_vars_of_terms binding [ QVar "v"; QVar "e"; QVar "a"; QVar "v" ]);
  Query.ensure_query_terms_bound binding [ QVar "e"; QValue (String "Ivan") ] "[?e :name \"Ivan\"]";
  assert_raises_invalid_arg_message
    "ensure_query_terms_bound reports missing vars"
    "Insufficient bindings: #{?a ?v} not bound in [?e ?a ?v]"
    (fun () ->
       Query.ensure_query_terms_bound binding [ QVar "e"; QVar "a"; QVar "v" ] "[?e ?a ?v]");
  Query.ensure_not_has_outer_binding
    ~value_to_string
    binding
    [ Pattern (QVar "e", QAttr "name", QValue (String "Ivan")) ];
  assert_raises_invalid_arg_message
    "ensure_not_has_outer_binding reports not clauses with no outer vars"
    "Insufficient bindings: none of #{?e ?name} is bound in (not [?e :name ?name])"
    (fun () ->
       Query.ensure_not_has_outer_binding
         ~value_to_string
         []
         [ Pattern (QVar "e", QAttr "name", QVar "name") ]);
  let branch_a = [ Pattern (QVar "e", QAttr "name", QVar "name") ] in
  let branch_b = [ Pattern (QVar "e", QAttr "age", QVar "age") ] in
  assert_equal_string_list
    "vars_of_branch collects branch vars"
    [ "e"; "name" ]
    (Query.vars_of_branch branch_a);
  assert_equal_string_list
    "free_vars_of_branch subtracts bound vars"
    [ "name" ]
    (Query.free_vars_of_branch [ "e" ] branch_a);
  assert_raises_invalid_arg_message
    "ensure_or_branch_vars_match reports mismatched free vars"
    "All clauses in 'or' must use same set of free vars, had [#{?name} #{?age}] in (or [?e :name ?name] [?e :age ?age])"
    (fun () -> Query.ensure_or_branch_vars_match ~value_to_string binding [ branch_a; branch_b ]);
  Query.ensure_join_vars_bound binding [ "e" ];
  assert_raises_invalid_arg_message
    "ensure_join_vars_bound keeps its legacy message"
    "insufficient bindings"
    (fun () -> Query.ensure_join_vars_bound binding [ "missing" ]);
  Query.ensure_join_vars_bound_in_clause binding [ "e" ] "(or-join [?e] ...)";
  assert_raises_invalid_arg_message
    "ensure_join_vars_bound_in_clause reports missing vars"
    "Insufficient bindings: #{?missing} not bound in (or-join [?missing] ...)"
    (fun () ->
       Query.ensure_join_vars_bound_in_clause binding [ "missing" ] "(or-join [?missing] ...)");
  Query.ensure_or_join_branches_cover_listed_vars binding [ "name" ] [ branch_a ];
  assert_raises_invalid_arg_message
    "ensure_or_join_branches_cover_listed_vars requires listed vars in every branch"
    "or branches must use same free vars"
    (fun () ->
       Query.ensure_or_join_branches_cover_listed_vars binding [ "name" ] [ branch_a; branch_b ])

let () =
  test_query_namespace__test_public_query_api ();
  test_query_namespace__test_query_result_helpers ();
  test_query_namespace__test_query_matching_helpers ();
  test_query_namespace__test_source_matching_helpers ();
  test_query_namespace__test_aggregate_helpers ();
  test_query_namespace__test_find_grouping_helpers ();
  test_query_namespace__test_input_label_helpers ();
  test_query_namespace__test_input_shape_helpers ();
  test_query_namespace__test_input_binding_helpers ();
  test_query_namespace__test_callable_helpers ();
  test_query_namespace__test_rule_helpers ();
  test_query_namespace__test_variable_discovery_helpers ();
  test_query_namespace__test_source_discovery_helpers ();
  test_query_namespace__test_rule_source_analysis_helpers ();
  test_query_namespace__test_query_validation_helpers ();
  test_query_namespace__test_return_map_validation_helpers ();
  test_query_namespace__test_query_string_helpers ();
  test_query_namespace__test_query_clause_string_helpers ();
  test_query_namespace__test_binding_validation_helpers ()
