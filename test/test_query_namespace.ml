open Datascript
module DT = Internal.Datascript_types

let failf fmt = Printf.ksprintf failwith fmt

let query_result_equal left right =
  match left, right with
  | DT.Result_db left, DT.Result_db right -> left == right
  | DT.Result_db _, _ | _, DT.Result_db _ -> false
  | _ -> left = right

let rec query_result_list_equal left right =
  match left, right with
  | [], [] -> true
  | left :: left_rest, right :: right_rest ->
    query_result_equal left right && query_result_list_equal left_rest right_rest
  | [], _ :: _ | _ :: _, [] -> false

let query_result_option_equal left right =
  match left, right with
  | None, None -> true
  | Some left, Some right -> query_result_equal left right
  | None, Some _ | Some _, None -> false

let assert_equal_query label expected actual =
  if not (List.length expected = List.length actual && List.for_all2 query_result_list_equal expected actual) then
    failf "%s: unexpected query result" label

let assert_equal_query_row label expected actual =
  if not (query_result_list_equal expected actual) then failf "%s: unexpected query row" label

let assert_equal_query_rows label expected actual =
  if expected <> actual then failf "%s: unexpected query rows" label

let assert_equal_inputs label expected actual =
  if expected <> actual then failf "%s: unexpected bound query inputs" label

let assert_equal_rules label expected actual =
  if expected <> actual then failf "%s: unexpected query rules" label

let assert_equal_query_option label expected actual =
  if expected <> actual then failf "%s: unexpected optional query result" label

let assert_equal_query_result_option label expected actual =
  if not (query_result_option_equal expected actual) then failf "%s: unexpected optional query result" label

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
    Internal.empty_db ()
    |> Internal.db_with [ DT.Add (DT.Entity_id (1L), "name", DT.String "Ivan"); DT.Add (DT.Entity_id (2L), "name", DT.String "Oleg") ]
  in
  assert_equal_query
    "q_string exposes relation query API"
    [ [ DT.Result_value (DT.String "Ivan") ]; [ DT.Result_value (DT.String "Oleg") ] ]
    (Internal.q_string db "[:find ?name :where [_ :name ?name]]");
  assert_equal_query
    "q_sources_string exposes named sources"
    [ [ DT.Result_value (DT.String "Ivan") ] ]
    (Internal.q_sources_string
       (Internal.empty_db ())
       [ "people", DT.Db_source db ]
       "[:find ?name :in $people :where [$people _ :name ?name] [(= ?name \"Ivan\")]]");
  if
    Internal.q_return_map_string db "[:find ?e ?name :keys id name :where [?e :name ?name]]"
    <> Query_relation_maps
         [ [ DT.Keyword "id", DT.Result_entity (1L); DT.Keyword "name", DT.Result_value (DT.String "Ivan") ]
         ; [ DT.Keyword "id", DT.Result_entity (2L); DT.Keyword "name", DT.Result_value (DT.String "Oleg") ]
         ]
  then
    failwith "q_return_map_string should expose return-map query API"

let test_query_namespace__test_query_result_helpers () =
  let add_datom = { DT.e = 1L; DT.a = "name"; DT.v = DT.String "Ivan"; DT.tx = 7L; DT.added = true } in
  let retract_datom = { add_datom with DT.tx = 8L; DT.added = false } in
  (if (DT.Result_entity 1L) <> (Internal.Query.result_of_datom_e add_datom) then failf "result_of_datom_e returns entityresults");
  (if (DT.Result_attr "name") <> (Internal.Query.result_of_datom_a add_datom) then failf "result_of_datom_a returns attr results");
  (if (DT.Result_value (DT.String "Ivan")) <> (Internal.Query.result_of_datom_v add_datom) then failf "result_of_datom_v returns value results");
  (if (DT.Result_entity 7L) <> (Internal.Query.result_of_datom_tx add_datom) then failf "result_of_datom_tx returns tx entityresults");
  (if (DT.Result_value (DT.Keyword "db/add")) <> (Internal.Query.result_of_datom_op add_datom) then failf "result_of_datom_op returns add op keywords");
  (if (DT.Result_value (DT.Keyword "db/retract")) <> (Internal.Query.result_of_datom_op retract_datom) then failf "result_of_datom_op returns retract op keywords");
  (if (DT.Result_entity 42L) <> (Internal.Query.result_of_ref (DT.Result_value (DT.Ref 42L))) then failf "result_of_ref turns ref values into entityresults");
  (if (DT.Result_value (DT.String "Ivan")) <> (Internal.Query.result_of_ref (DT.Result_value (DT.String "Ivan"))) then failf "result_of_ref leaves non-ref results unchanged");
  let validate_entity_id entity_id =
    if entity_id <= 0L then invalid_arg "invalid entityid";
    entity_id
  in
  let result_resolution_context =
    { Internal.Query.validate_entity_id
    ; Internal.Query.resolve_query_value =
        (function
          | DT.Keyword "known-ident" -> Some (DT.Ref 42L)
          | DT.Symbol "missing" -> None
          | value -> Some value)
    ; Internal.Query.lookup_ref_entity_id =
        (fun attr value ->
           match attr, value with
           | "name", DT.String "Ivan" -> Some 101L
           | _ -> None)
    }
  in
  assert_equal_int_option
    "entity_id_of_resolved_query_result accepts entityresults"
    (Some 42)
    (Option.map Int64.to_int (Internal.Query.entity_id_of_resolved_query_result ~validate_entity_id (Some (DT.Result_entity 42L))));
  assert_equal_int_option
    "entity_id_of_resolved_query_result validates integer results"
    (Some 43)
    (Option.map Int64.to_int (Internal.Query.entity_id_of_resolved_query_result ~validate_entity_id (Some (DT.Result_value (DT.Int64 43L)))));
  assert_equal_int_option
    "entity_id_of_resolved_query_result accepts ref values"
    (Some 44)
    (Option.map Int64.to_int (Internal.Query.entity_id_of_resolved_query_result ~validate_entity_id (Some (DT.Result_value (DT.Ref 44L)))));
  assert_equal_int_option
    "entity_id_of_resolved_query_result rejects non-entityvalues"
    None
    (Option.map Int64.to_int (Internal.Query.entity_id_of_resolved_query_result ~validate_entity_id (Some (DT.Result_value (DT.String "Ivan")))));
  assert_equal_int_option
    "entity_id_of_resolved_query_result rejects missing values"
    None
    (Option.map Int64.to_int (Internal.Query.entity_id_of_resolved_query_result ~validate_entity_id None));
  assert_raises_invalid_arg "entity_id_of_resolved_query_result validates integer ids" (fun () ->
    ignore (Option.map Int64.to_int (Internal.Query.entity_id_of_resolved_query_result ~validate_entity_id (Some (DT.Result_value (DT.Int64 0L))))));
  assert_equal_query_option
    "resolved_query_result resolves value results through the context"
    (Some (DT.Result_entity 42L))
    (Internal.Query.resolved_query_result result_resolution_context (DT.Result_value (DT.Keyword "known-ident")));
  assert_equal_query_option
    "resolved_query_result drops values that cannot resolve"
    None
    (Internal.Query.resolved_query_result result_resolution_context (DT.Result_value (DT.Symbol "missing")));
  assert_equal_query_option
    "resolved_query_result drops db results"
    None
    (Internal.Query.resolved_query_result result_resolution_context (DT.Result_db (Internal.empty_db ())));
  assert_equal_int_option
    "lookup_ref_entity_id_of_value resolves vector lookup refs"
    (Some 101)
    (Option.map Int64.to_int (Internal.Query.lookup_ref_entity_id_of_value
       result_resolution_context
       (DT.Vector [ DT.Keyword "name"; DT.String "Ivan" ])));
  assert_equal_int_option
    "lookup_ref_entity_id_of_value rejects non lookup-ref values"
    None
    (Internal.Query.lookup_ref_entity_id_of_value result_resolution_context (DT.String "Ivan"));
  assert_equal_int_option
    "query_result_entity_id prefers lookup-ref values"
    (Some 101)
    (Option.map Int64.to_int (Internal.Query.query_result_entity_id
       result_resolution_context
       (DT.Result_value (DT.Vector [ DT.Keyword "name"; DT.String "Ivan" ]))));
  assert_equal_int_option
    "query_result_entity_id falls back to resolved values"
    (Some 42)
    (Option.map Int64.to_int (Internal.Query.query_result_entity_id result_resolution_context (DT.Result_value (DT.Keyword "known-ident"))));
  assert_equal_bool
    "query_results_equivalent compares identical db results by physical identity"
    true
    (let db = Internal.empty_db () in
     Internal.Query.query_results_equivalent result_resolution_context (DT.Result_db db) (DT.Result_db db));
  assert_equal_bool
    "query_results_equivalent rejects different db results"
    false
    (Internal.Query.query_results_equivalent result_resolution_context (DT.Result_db (Internal.empty_db ())) (DT.Result_db (Internal.empty_db ())));
  assert_equal_bool
    "query_results_equivalent compares lookup refs through entityids"
    true
    (Internal.Query.query_results_equivalent
       result_resolution_context
       (DT.Result_value (DT.Vector [ DT.Keyword "name"; DT.String "Ivan" ]))
       (DT.Result_entity 101L));
  assert_equal_bool
    "query_results_equivalent compares resolved values"
    true
    (Internal.Query.query_results_equivalent result_resolution_context (DT.Result_value (DT.Keyword "known-ident")) (DT.Result_entity 42L));
  assert_equal_bool
    "query_results_equivalent compares keyword attr values with attrs"
    true
    (Internal.Query.query_results_equivalent
       result_resolution_context
       (DT.Result_value (DT.Keyword "user.property/foo"))
       (DT.Result_attr "user.property/foo"));
  assert_equal_query_option
    "bind_var adds unbound vars"
    (Some [ "e", DT.Result_entity 42L ])
    (Internal.Query.bind_var result_resolution_context "e" (DT.Result_entity 42L) []);
  assert_equal_query_option
    "bind_var accepts equivalent bound values"
    (Some [ "e", DT.Result_value (DT.Keyword "known-ident") ])
    (Internal.Query.bind_var
       result_resolution_context
       "e"
       (DT.Result_entity 42L)
       [ "e", DT.Result_value (DT.Keyword "known-ident") ]);
  assert_equal_query_option
    "bind_var rejects conflicting bound values"
    None
    (Internal.Query.bind_var result_resolution_context "e" (DT.Result_entity 99L) [ "e", DT.Result_entity 42L ]);
  assert_equal_bool
    "result_matches_entity accepts equivalent entityids"
    true
    (Internal.Query.result_matches_entity result_resolution_context 42L (DT.Result_value (DT.Keyword "known-ident")));
  assert_equal_bool
    "result_matches_entity rejects mismatches"
    false
    (Internal.Query.result_matches_entity result_resolution_context 99L (DT.Result_value (DT.Keyword "known-ident")))

let test_query_namespace__test_query_matching_helpers () =
  let result_resolution_context =
    { Internal.Query.validate_entity_id = (fun entity_id -> entity_id)
    ; resolve_query_value =
        (function
          | DT.Keyword "known-ident" -> Some (DT.Ref 42L)
          | value -> Some value)
    ; lookup_ref_entity_id =
        (fun attr value ->
           match attr, value with
           | "name", DT.String "Ivan" -> Some 101L
           | _ -> None)
    }
  in
  let match_context =
    { Internal.Query.result_resolution_context
    ; source_db = Internal.empty_db ()
    ; ident_entity_id = (function "known-ident" -> Some 42L | _ -> None)
    ; unresolved_lookup_ref_message = (fun attr _ -> "missing lookup ref: " ^ attr)
    ; value_equal = Internal.Util.value_equal
    ; coerce_tuple_lookup_value =
        (fun attr value ->
           match attr, value with
           | "tuple", DT.Vector [ left; right ] -> DT.Tuple [ Some left; Some right ]
           | _ -> value)
    }
  in
  let base_binding = [ "existing", DT.Result_value (DT.String "kept") ] in
  assert_equal_query_option
    "match_query_term keeps bindings for wildcards"
    (Some base_binding)
    (Internal.Query.match_query_term match_context DT.QWildcard (DT.Result_entity 1L) base_binding);
  assert_equal_query_option
    "match_query_term matches entityterms through result equivalence"
    (Some [])
    (Internal.Query.match_query_term match_context (DT.QEntity (42L)) (DT.Result_value (DT.Keyword "known-ident")) []);
  assert_equal_query_option
    "match_query_term resolves ident terms through the context"
    (Some [])
    (Internal.Query.match_query_term match_context (DT.QIdent "known-ident") (DT.Result_entity 42L) []);
  assert_equal_query_option
    "match_query_term resolves lookup ref terms through the context"
    (Some [])
    (Internal.Query.match_query_term match_context (DT.QLookupRef ("name", DT.String "Ivan")) (DT.Result_entity (101L)) []);
  assert_raises_invalid_arg_message
    "match_query_term reports missing lookup refs"
    "missing lookup ref: name"
    (fun () ->
       ignore
         (Internal.Query.match_query_term
            match_context
            (DT.QLookupRef ("name", DT.String "Missing"))
            (DT.Result_entity (101L))
            []));
  assert_equal_query_option
    "match_query_term matches attrs"
    (Some [])
    (Internal.Query.match_query_term match_context (DT.QAttr "name") (DT.Result_attr "name") []);
  assert_equal_query_option
    "match_query_term matches literal values"
    (Some [])
    (Internal.Query.match_query_term match_context (DT.QValue (DT.String "Ivan")) (DT.Result_value (DT.String "Ivan")) []);
  assert_equal_query_option
    "match_query_term matches ref values against entityresults"
    (Some [])
    (Internal.Query.match_query_term match_context (DT.QValue (DT.Ref (42L))) (DT.Result_entity 42L) []);
  assert_equal_query_option
    "match_query_term matches keyword idents against entityresults"
    (Some [])
    (Internal.Query.match_query_term match_context (DT.QValue (DT.Keyword "known-ident")) (DT.Result_entity 42L) []);
  assert_equal_query_option
    "match_query_term binds vars after normalizing refs"
    (Some [ "e", DT.Result_entity 42L ])
    (Internal.Query.match_query_term match_context (DT.QVar "e") (DT.Result_value (DT.Ref (42L))) []);
  let name_datom = Internal.datom ~tx:7L ~added:true ~e:1L ~a:"name" ~v:(DT.String "Ivan") () in
  assert_equal_query_option
    "match_pattern_clause matches datoms"
    (Some [ "v", DT.Result_value (DT.String "Ivan"); "e", DT.Result_entity (1L) ])
    (Internal.Query.match_pattern_clause match_context [] (DT.QVar "e") (DT.QAttr "name") (DT.QVar "v") name_datom);
  let dynamic_attr_datom = Internal.datom ~e:2L ~a:"user.property/foo" ~v:(DT.String "bar") () in
  assert_equal_query_option
    "match_pattern_clause matches attr vars bound to keyword values"
    (Some [ "prop", DT.Result_value (DT.Keyword "user.property/foo") ])
    (Internal.Query.match_pattern_clause
       match_context
       [ "prop", DT.Result_value (DT.Keyword "user.property/foo") ]
       (DT.QEntity (2L))
       (DT.QVar "prop")
       (DT.QValue (DT.String "bar"))
       dynamic_attr_datom);
  assert_equal_query_option
    "match_pattern_tx_clause matches tx terms"
    (Some [ "tx", DT.Result_entity (7L); "v", DT.Result_value (DT.String "Ivan"); "e", DT.Result_entity (1L) ])
    (Internal.Query.match_pattern_tx_clause
       match_context
       []
       (DT.QVar "e")
       (DT.QAttr "name")
       (DT.QVar "v")
       (DT.QVar "tx")
       name_datom);
  let tuple_datom =
    Internal.datom ~e:2L ~a:"tuple" ~v:(DT.Tuple [ Some (DT.String "a"); Some (DT.String "b") ]) ~tx:8L ~added:true ()
  in
  assert_equal_query_option
    "match_value_term_for_datom_attr coerces tuple lookup values"
    (Some [])
    (Internal.Query.match_value_term_for_datom_attr
       match_context
       []
       (DT.QValue (DT.Vector [ DT.String "a"; DT.String "b" ]))
       tuple_datom);
  let reverse_datom = Internal.datom ~tx:9L ~added:true ~e:1L ~a:"parent" ~v:(DT.Ref 2L) () in
  assert_equal_query_option
    "match_reverse_pattern_clause matches reverse refs"
    (Some [ "parent", DT.Result_entity (1L) ])
    (Internal.Query.match_reverse_pattern_clause match_context [] (DT.QEntity (2L)) "_parent" (DT.QVar "parent") reverse_datom);
  assert_equal_query_option
    "eval_query_term reads bound vars"
    (Some (DT.Result_value (DT.String "kept")))
    (Internal.Query.eval_query_term match_context base_binding (DT.QVar "existing"));
  assert_equal_query_option
    "eval_query_term resolves entityterms"
    (Some (DT.Result_entity 42L))
    (Internal.Query.eval_query_term match_context [] (DT.QEntity (42L)));
  assert_equal_query_option
    "eval_query_term resolves ident terms"
    (Some (DT.Result_entity 42L))
    (Internal.Query.eval_query_term match_context [] (DT.QIdent "known-ident"));
  assert_equal_query_option
    "eval_query_term resolves lookup ref terms"
    (Some (DT.Result_entity (101L)))
    (Internal.Query.eval_query_term match_context [] (DT.QLookupRef ("name", DT.String "Ivan")));
  assert_raises_invalid_arg_message
    "eval_query_term reports missing lookup refs"
    "missing lookup ref: name"
    (fun () -> ignore (Internal.Query.eval_query_term match_context [] (DT.QLookupRef ("name", DT.String "Missing"))));
  assert_equal_query_option
    "eval_query_term resolves literal values"
    (Some (DT.Result_value (DT.Ref (42L))))
    (Internal.Query.eval_query_term match_context [] (DT.QValue (DT.Keyword "known-ident")));
  assert_equal_query_result_option
    "eval_query_term returns default source db"
    (Some (DT.Result_db match_context.source_db))
    (Internal.Query.eval_query_term match_context [] (DT.QSource "$"));
  assert_raises_invalid_arg_message
    "eval_query_term rejects named sources without source context"
    "source term requires query source context: other"
    (fun () -> ignore (Internal.Query.eval_query_term match_context [] (DT.QSource "other")));
  assert_equal_query_option
    "eval_query_term drops wildcards"
    None
    (Internal.Query.eval_query_term match_context [] DT.QWildcard);
  assert_equal_query_option
    "collect_query_terms evaluates all terms"
    (Some [ DT.Result_value (DT.String "kept"); DT.Result_entity (42L) ])
    (Internal.Query.collect_query_terms match_context base_binding [ (DT.QVar "existing"); (DT.QEntity (42L)) ]);
  assert_equal_query_option
    "collect_query_terms drops collections with wildcards"
    None
    (Internal.Query.collect_query_terms match_context base_binding [ (DT.QVar "existing"); DT.QWildcard ]);
  assert_equal_query_row
    "collect_query_terms_exn returns evaluated terms"
    [ DT.Result_value (DT.String "kept"); DT.Result_entity (42L) ]
    (Internal.Query.collect_query_terms_exn match_context base_binding [ (DT.QVar "existing"); (DT.QEntity (42L)) ]);
  assert_raises_invalid_arg_message
    "collect_query_terms_exn rejects insufficient bindings"
    "insufficient bindings"
    (fun () -> ignore (Internal.Query.collect_query_terms_exn match_context base_binding [ DT.QWildcard ]));
  assert_equal_int_option
    "query_term_entity_id returns entityids for evaluated terms"
    (Some 42)
    (Option.map
       Int64.to_int
       (Internal.Query.query_term_entity_id match_context [] (DT.QValue (DT.Keyword "known-ident"))))

let test_query_namespace__test_source_matching_helpers () =
  let result_resolution_context =
    { Internal.Query.validate_entity_id = (fun entity_id -> entity_id)
    ; resolve_query_value = (fun value -> Some value)
    ; lookup_ref_entity_id = (fun _ _ -> None)
    }
  in
  let match_context =
    { Internal.Query.result_resolution_context
    ; source_db = Internal.empty_db ()
    ; ident_entity_id = (fun _ -> None)
    ; unresolved_lookup_ref_message = (fun attr _ -> "missing lookup ref: " ^ attr)
    ; value_equal = Internal.Util.value_equal
    ; coerce_tuple_lookup_value = (fun _ value -> value)
    }
  in
  let name_datom = Internal.datom ~tx:7L ~added:true ~e:1L ~a:"name" ~v:(DT.String "Ivan") () in
  let source_context =
    { Internal.Query.match_context
    ; pattern_datoms = (fun _ _ _ _ _ -> List.to_seq [ name_datom ])
    ; fold_pattern_datoms = (fun _ _ _ _ _ ~init ~f -> f init name_datom)
    ; pattern_comparison_datoms = (fun _ _ _ _ -> None)
    ; match_data_pattern =
        (fun _ bindings e_term a_term v_term datom ->
           Internal.Query.match_pattern_clause match_context bindings e_term a_term v_term datom)
    ; match_data_pattern_tx =
        (fun _ bindings e_term a_term v_term tx_term datom ->
           Internal.Query.match_pattern_tx_clause match_context bindings e_term a_term v_term tx_term datom)
    ; match_data_pattern_tx_op =
        (fun _ bindings e_term a_term v_term tx_term op_term datom ->
           let ( let* ) = Option.bind in
           let* bindings =
             Internal.Query.match_pattern_tx_clause match_context bindings e_term a_term v_term tx_term datom
           in
           Internal.Query.match_query_term match_context op_term (Internal.Query.result_of_datom_op datom) bindings)
    }
  in
  let root_db = Internal.empty_db () in
  let other_db = Internal.empty_db () in
  (match Internal.Query.source root_db [ "other", DT.Db_source other_db ] "$" with
   | DT.Db_source db when db == root_db -> ()
   | _ -> failwith "source should default $ to the root db");
  (match Internal.Query.source_db root_db [ "other", DT.Db_source other_db ] "other" with
   | db when db == other_db -> ()
   | _ -> failwith "source_db should return named database sources");
  assert_raises_invalid_arg_message
    "source rejects unknown names"
    "unknown query source: missing"
    (fun () -> ignore (Internal.Query.source root_db [] "missing"));
  assert_raises_invalid_arg_message
    "source_db rejects relation sources"
    "query source is not a database: rows"
    (fun () -> ignore (Internal.Query.source_db root_db [ "rows", DT.Relation_source [] ] "rows"));
  assert_equal_query_option
    "match_relation_row binds each relation column"
    (Some [ "age", DT.Result_value (DT.Int64 42L); "name", DT.Result_value (DT.String "Ivan") ])
    (Internal.Query.match_relation_row
       source_context
       []
       [ (DT.QVar "name"); (DT.QVar "age") ]
       [ DT.Result_value (DT.String "Ivan"); DT.Result_value (DT.Int64 42L) ]);
  assert_raises_invalid_arg_message
    "match_relation_row rejects short rows"
    "source relation row arity mismatch"
    (fun () ->
       ignore
         (Internal.Query.match_relation_row
            source_context
            []
            [ (DT.QVar "name"); (DT.QVar "age") ]
            [ DT.Result_value (DT.String "Ivan") ]));
  assert_equal_query_rows
    "match_query_source_pattern matches database source triples"
    [ [ "name", DT.Result_value (DT.String "Ivan"); "e", DT.Result_entity (1L) ] ]
    (Internal.Query.match_query_source_pattern
       source_context
       root_db
       (DT.Db_source root_db)
       []
       [ (DT.QVar "e"); (DT.QAttr "name"); (DT.QVar "name") ]);
  assert_equal_query_rows
    "match_query_source_pattern matches relation source rows"
    [ [ "name", DT.Result_value (DT.String "Ivan") ] ]
    (Internal.Query.match_query_source_pattern
       source_context
       root_db
       (DT.Relation_source [ [ DT.Result_value (DT.String "Ivan") ] ])
       []
       [ (DT.QVar "name") ]);
  assert_raises_invalid_arg_message
    "match_query_source_pattern rejects database arity mismatch"
    "database source patterns expect 3, 4, or 5 terms"
    (fun () ->
       ignore
         (Internal.Query.match_query_source_pattern
            source_context
            root_db
            (DT.Db_source root_db)
            []
            [ (DT.QVar "e"); (DT.QAttr "name") ]));
  assert_equal_query_rows
    "match_relation_source_pattern expands short database entitypatterns"
    [ [ "e", DT.Result_entity (1L) ] ]
    (Internal.Query.match_relation_source_pattern source_context root_db [] "$" [] [ (DT.QVar "e") ]);
  assert_equal_query_rows
    "match_relation_source_pattern coerces short database attr values"
    [ [ "e", DT.Result_entity (1L) ] ]
    (Internal.Query.match_relation_source_pattern
       source_context
       root_db
       []
       "$"
       []
       [ (DT.QVar "e"); (DT.QValue (DT.Keyword "name")) ]);
  assert_equal_query_rows
    "match_source_pattern uses named relation sources"
    [ [ "name", DT.Result_value (DT.String "Ivan") ] ]
    (Internal.Query.match_source_pattern
       source_context
       root_db
       [ "rows", DT.Relation_source [ [ DT.Result_value (DT.String "Ivan") ] ] ]
       "rows"
       []
       [ (DT.QVar "name") ])

let test_query_namespace__test_aggregate_helpers () =
  if not (Internal.Query.has_aggregates [ DT.Find_aggregate (DT.Sum, [ (DT.QVar "amount") ]) ]) then
    failwith "Query.has_aggregates should detect aggregate find specs";
  if Internal.Query.has_aggregates [ DT.Find_var "amount" ] then
    failwith "Query.has_aggregates should ignore non-aggregate find specs";
  assert_equal_aggregate
    "dynamic min amount resolves from the first group binding"
    (DT.MinN 2)
    (Internal.Query.resolve_dynamic_aggregate
       (DT.MinNVar "n")
       [ [ "n", DT.Result_value (DT.Int64 2L); "amount", DT.Result_value (DT.Int64 10L) ] ]);
  assert_raises_invalid_arg "dynamic aggregate amount must be non-negative" (fun () ->
    ignore (Internal.Query.resolve_dynamic_aggregate (DT.SampleVar "n") [ [ "n", DT.Result_value (DT.Int64 (-1L)) ] ]));
  assert_raises_invalid_arg "dynamic aggregate amount must be bound" (fun () ->
    ignore (Internal.Query.resolve_dynamic_aggregate (DT.MaxNVar "n") []));
  assert_equal_string_list
    "aggregate_param_vars reports amount variables"
    [ "n" ]
    (Internal.Query.aggregate_param_vars (DT.RandNVar "n"));
  assert_equal_string_list
    "aggregate_callable_vars reports custom aggregate variables"
    [ "agg" ]
    (Internal.Query.aggregate_callable_vars (DT.CustomVar "agg"));
  assert_equal_terms
    "split_aggregate_terms returns extra args and the value term"
    ([ (DT.QVar "n"); (DT.QValue (DT.String "tag")) ], (DT.QVar "amount"))
    (Internal.Query.split_aggregate_terms [ (DT.QVar "n"); (DT.QValue (DT.String "tag")); (DT.QVar "amount") ]);
  assert_raises_invalid_arg "split_aggregate_terms rejects empty terms" (fun () ->
    ignore (Internal.Query.split_aggregate_terms []));
  assert_equal_query_row
    "custom aggregate receives extra args before values"
    [ DT.Result_value (DT.Int64 9L); DT.Result_value (DT.Int64 1L); DT.Result_value (DT.Int64 2L) ]
    (Internal.Query.aggregate_input_values
       (DT.Custom (fun values -> DT.Result_value (DT.Int64 (Int64.of_int (List.length values)))))
       [ DT.Result_value (DT.Int64 9L) ]
       [ DT.Result_value (DT.Int64 1L); DT.Result_value (DT.Int64 2L) ]);
  assert_equal_query_row
    "built-in aggregate ignores extra args after parse-time resolution"
    [ DT.Result_value (DT.Int64 1L); DT.Result_value (DT.Int64 2L) ]
    (Internal.Query.aggregate_input_values
       DT.Sum
       [ DT.Result_value (DT.Int64 9L) ]
       [ DT.Result_value (DT.Int64 1L); DT.Result_value (DT.Int64 2L) ]);
  let match_context =
    { Internal.Query.result_resolution_context =
        { validate_entity_id = (fun entity_id -> entity_id)
        ; resolve_query_value = (fun value -> Some value)
        ; lookup_ref_entity_id = (fun _ _ -> None)
        }
    ; source_db = Internal.empty_db ()
    ; ident_entity_id = (fun _ -> None)
    ; unresolved_lookup_ref_message = (fun attr _ -> "missing lookup ref: " ^ attr)
    ; value_equal = Internal.Util.value_equal
    ; coerce_tuple_lookup_value = (fun _ value -> value)
    }
  in
  let default_db = Internal.empty_db () in
  let other_db = Internal.empty_db () in
  let sources = [ "other", DT.Db_source other_db ] in
  (match Internal.Query.eval_query_term_with_sources match_context default_db sources [] (DT.QSource "$") with
   | Some (DT.Result_db db) when db == default_db -> ()
   | _ -> failwith "eval_query_term_with_sources should resolve the default source db");
  (match Internal.Query.eval_query_term_with_sources match_context default_db sources [] (DT.QSource "other") with
   | Some (DT.Result_db db) when db == other_db -> ()
   | _ -> failwith "eval_query_term_with_sources should resolve named source dbs");
  assert_equal_query_option
    "eval_query_term_with_sources delegates non-source terms"
    (Some (DT.Result_value (DT.String "Ivan")))
    (Internal.Query.eval_query_term_with_sources
       match_context
       default_db
       sources
       [ "name", DT.Result_value (DT.String "Ivan") ]
       (DT.QVar "name"));
  assert_raises_invalid_arg_message
    "eval_query_term_with_sources rejects unknown sources"
    "unknown query source: missing"
    (fun () -> ignore (Internal.Query.eval_query_term_with_sources match_context default_db sources [] (DT.QSource "missing")));
  assert_equal_query_row
    "collect_dynamic_query_terms_exn evaluates vars and sources"
    [ DT.Result_value (DT.String "Ivan"); DT.Result_db other_db ]
    (Internal.Query.collect_dynamic_query_terms_exn
       match_context
       default_db
       sources
       [ "name", DT.Result_value (DT.String "Ivan") ]
       [ (DT.QVar "name"); (DT.QSource "other") ]);
  assert_raises_invalid_arg_message
    "collect_dynamic_query_terms_exn reports unbound terms"
    "unbound query variable"
    (fun () ->
       ignore
         (Internal.Query.collect_dynamic_query_terms_exn match_context default_db sources [] [ (DT.QVar "missing") ]));
  (match
     Internal.Query.aggregate_extra_args
       match_context
       default_db
       sources
       [ [ "n", DT.Result_value (DT.Int64 2L); "amount", DT.Result_value (DT.Int64 10L) ]
       ; [ "n", DT.Result_value (DT.Int64 3L); "amount", DT.Result_value (DT.Int64 20L) ]
       ]
       [ (DT.QVar "n"); (DT.QSource "other"); (DT.QVar "amount") ]
   with
   | [ DT.Result_value (DT.Int64 2L); DT.Result_db db ] when db == other_db -> ()
   | _ -> failwith "aggregate_extra_args should use first group binding and resolve source args");
  assert_equal_query_row
    "aggregate_values evaluates the aggregate value term for every group binding"
    [ DT.Result_value (DT.Int64 10L); DT.Result_value (DT.Int64 20L) ]
    (Internal.Query.aggregate_values
       match_context
       default_db
       sources
       [ [ "amount", DT.Result_value (DT.Int64 10L) ]; [ "amount", DT.Result_value (DT.Int64 20L) ] ]
       [ (DT.QVar "amount") ]);
  assert_equal_query_row
    "aggregate_values drops bindings where the value term is unbound"
    [ DT.Result_value (DT.Int64 10L) ]
    (Internal.Query.aggregate_values
       match_context
       default_db
       sources
       [ [ "amount", DT.Result_value (DT.Int64 10L) ]; [ "name", DT.Result_value (DT.String "missing") ] ]
       [ (DT.QVar "amount") ]);
  assert_raises_invalid_arg_message
    "aggregate_extra_args rejects unbound extra args"
    "insufficient aggregate argument bindings"
    (fun () ->
       ignore
         (Internal.Query.aggregate_extra_args
            match_context
            default_db
            sources
            [ [ "amount", DT.Result_value (DT.Int64 10L) ] ]
            [ (DT.QVar "missing"); (DT.QVar "amount") ]))

let test_query_namespace__test_find_grouping_helpers () =
  let binding =
    [ "name", DT.Result_value (DT.String "Ivan")
    ; "age", DT.Result_value (DT.Int64 30L)
    ; "city", DT.Result_value (DT.String "Berlin")
    ]
  in
  assert_equal_query_option
    "collect_find_vars preserves requested order"
    (Some [ DT.Result_value (DT.Int64 30L); DT.Result_value (DT.String "Ivan") ])
    (Internal.Query.collect_find_vars binding [ "age"; "name" ]);
  assert_equal_query_option
    "collect_find_vars returns None when a requested var is missing"
    None
    (Internal.Query.collect_find_vars binding [ "age"; "missing" ]);
  assert_equal_grouped_bindings
    "group_by_key prepends later rows in the same group"
    [ ( [ DT.Result_value (DT.String "Ivan") ]
      , [ [ "age", DT.Result_value (DT.Int64 31L) ]; [ "age", DT.Result_value (DT.Int64 30L) ] ] )
    ; ( [ DT.Result_value (DT.String "Oleg") ]
      , [ [ "age", DT.Result_value (DT.Int64 40L) ] ] )
    ]
    (Internal.Query.group_by_key
       [ [ DT.Result_value (DT.String "Ivan") ], [ "age", DT.Result_value (DT.Int64 30L) ]
       ; [ DT.Result_value (DT.String "Oleg") ], [ "age", DT.Result_value (DT.Int64 40L) ]
       ; [ DT.Result_value (DT.String "Ivan") ], [ "age", DT.Result_value (DT.Int64 31L) ]
       ]);
  assert_equal_string_list
    "grouping_vars_of_find includes non-aggregate find vars"
    [ "city"; "entity"; "pattern" ]
    (Internal.Query.grouping_vars_of_find
       [ DT.Find_var "city"
       ; DT.Find_pull ("entity", [ DT.Pull_id ])
       ; Find_pull_var ("entity", "pattern")
       ; DT.Find_aggregate (DT.Count, [ (DT.QVar "age") ])
       ])

let test_query_namespace__test_input_label_helpers () =
  assert_equal_string "query_input_var_label adds a question mark" "?name" (Internal.Query.query_input_var_label "name");
  assert_equal_string
    "query_input_var_label preserves existing query prefixes"
    "$source"
    (Internal.Query.query_input_var_label "$source");
  assert_equal_string
    "query_input_binding_string formats nested tuple bindings"
    "[?name [_ ...] [?city ?country]]"
    (Internal.Query.query_input_binding_string
       (Bind_tuple
          [  Bind_scalar "name"
          ; Bind_collection Bind_ignore
          ; Bind_tuple [ Bind_scalar "city"; Bind_scalar "?country" ]
          ]));
  assert_equal_string
    "query_input_decl_binding_string formats relation declarations"
    "[[?name ?age]]"
    (Internal.Query.query_input_decl_binding_string (DT.Input_relation_decl [ "name"; "age" ]));
  assert_equal_string
    "query_input_binding_label formats rules declarations"
    "%"
    (Internal.Query.query_input_binding_label DT.Input_rules_decl);
  assert_equal_string
    "query_input_binding_label formats bound scalar inputs"
    "?name"
    (Internal.Query.query_input_binding_label (DT.Input_scalar ("name", DT.Result_value (DT.String "Ivan"))));
  if not (Internal.Query.query_input_consumes_argument ~consume_rules:true DT.Input_rules_decl) then
    failwith "rules input should consume an argument when requested";
  if Internal.Query.query_input_consumes_argument ~consume_rules:false DT.Input_rules_decl then
    failwith "rules input should not consume an argument when rules are implicit";
  if not (Internal.Query.query_input_consumes_argument ~consume_rules:false (DT.Input_tuple_decl [ "name" ])) then
    failwith "tuple declarations should consume query input arguments";
  if Internal.Query.query_input_consumes_argument ~consume_rules:true (DT.Input_source_decl "$other") then
    failwith "source declarations should not consume query input arguments"

let test_query_namespace__test_input_shape_helpers () =
  assert_equal_query_option
    "values_of_collection_result unwraps vectors"
    (Some [ DT.Result_value (DT.String "a"); DT.Result_value (DT.String "b") ])
    (Internal.Query.values_of_collection_result (DT.Result_value (DT.Vector [ DT.String "a"; DT.String "b" ])));
  assert_equal_query_option
    "values_of_collection_result drops tuple nil slots"
    (Some [ DT.Result_value (DT.Int64 1L); DT.Result_value (DT.Int64 3L) ])
    (Internal.Query.values_of_collection_result (DT.Result_value (DT.Tuple [ Some (DT.Int64 1L); None; Some (DT.Int64 3L) ])));
  assert_equal_query_option
    "values_of_collection_result rejects scalar values"
    None
    (Internal.Query.values_of_collection_result (DT.Result_value (DT.String "not-a-collection")));
  assert_equal_query_row
    "row_of_collection_result preserves tuple nil slots"
    [ DT.Result_value (DT.Int64 1L); DT.Result_value DT.Nil; DT.Result_value (DT.Int64 3L) ]
    (Internal.Query.row_of_collection_result (DT.Result_value (DT.Tuple [ Some (DT.Int64 1L); None; Some (DT.Int64 3L) ])));
  assert_equal_query_row
    "row_of_collection_result wraps scalar values"
    [ DT.Result_value (DT.String "scalar") ]
    (Internal.Query.row_of_collection_result (DT.Result_value (DT.String "scalar")));
  assert_equal_query_row
    "row_of_scalar_sequence unwraps scalar sequence values"
    [ DT.Result_value (DT.Keyword "left"); DT.Result_value (DT.Keyword "right") ]
    (Internal.Query.row_of_scalar_sequence (DT.Result_value (DT.List [ DT.Keyword "left"; DT.Keyword "right" ])));
  assert_raises_invalid_arg "row_of_scalar_sequence rejects non-sequence scalars" (fun () ->
    ignore (Internal.Query.row_of_scalar_sequence (DT.Result_value (DT.Keyword "value"))));
  assert_equal_query_rows
    "rows_of_map_entries converts map entries to relation rows"
    [ [ DT.Result_value (DT.Keyword "a"); DT.Result_value (DT.Int64 1L) ]
    ; [ DT.Result_value (DT.Keyword "b"); DT.Result_value (DT.Vector [ DT.Int64 2L; DT.Int64 3L ]) ]
    ]
    (Internal.Query.rows_of_map_entries
       [ DT.Keyword "a", DT.Int64 1L; DT.Keyword "b", DT.Vector [ DT.Int64 2L; DT.Int64 3L ] ])

let test_query_namespace__test_input_binding_helpers () =
  let input_context =
    { Internal.Query.resolve_query_input_result =
        (function
          | DT.Result_value (DT.Keyword "drop") -> None
          | result -> Some result)
    ; bind_var =
        (fun var value bindings ->
           Internal.Query.bind_var
             { validate_entity_id = (fun entity_id -> entity_id)
             ; resolve_query_value = (fun value -> Some value)
             ; lookup_ref_entity_id = (fun _ _ -> None)
             }
             var
             value
             bindings)
    ; entity_id_of_ref =
        (function
          | Ident "known" -> Some 42L
          | _ -> None)
    }
  in
  assert_equal_query_option
    "bind_relation_row binds relation columns"
    (Some [ "age", DT.Result_value (DT.Int64 30L); "name", DT.Result_value (DT.String "Ivan") ])
    (Internal.Query.bind_relation_row
       input_context
       []
       [ "name"; "age" ]
       [ DT.Result_value (DT.String "Ivan"); DT.Result_value (DT.Int64 30L) ]);
  assert_raises_invalid_arg_message
    "bind_relation_row rejects row arity mismatch"
    "relation input row arity mismatch"
    (fun () -> ignore (Internal.Query.bind_relation_row input_context [] [ "name" ] []));
  assert_equal_query_rows
    "apply_query_input binds scalar inputs"
    [ [ "name", DT.Result_value (DT.String "Ivan") ] ]
    (Internal.Query.apply_query_input input_context [ [] ] (DT.Input_scalar ("name", DT.Result_value (DT.String "Ivan"))));
  assert_equal_query_rows
    "apply_query_input binds entityref inputs"
    [ [ "e", DT.Result_entity 42L ] ]
    (Internal.Query.apply_query_input input_context [ [] ] (DT.Input_entity_ref ("e", Ident "known")));
  assert_equal_query_rows
    "apply_query_input expands relation inputs"
    [ [ "age", DT.Result_value (DT.Int64 30L); "name", DT.Result_value (DT.String "Ivan") ]
    ; [ "age", DT.Result_value (DT.Int64 40L); "name", DT.Result_value (DT.String "Oleg") ]
    ]
    (Internal.Query.apply_query_input
       input_context
       [ [] ]
       (DT.Input_relation
          ( [ "name"; "age" ]
          , [ [ DT.Result_value (DT.String "Ivan"); DT.Result_value (DT.Int64 30L) ]
            ; [ DT.Result_value (DT.String "Oleg"); DT.Result_value (DT.Int64 40L) ]
            ] )));
  let query_input_of_arg decl arg =
    match decl, arg with
    | DT.Input_ignore_decl, _ -> DT.Input_ignore
    | DT.Input_scalar_decl var, DT.Arg_scalar value -> DT.Input_scalar (var, value)
    | DT.Input_tuple_decl vars, DT.Arg_tuple row -> DT.Input_tuple (vars, row)
    | DT.Input_rules_decl, DT.Arg_rules rules -> DT.Input_rules rules
    | _ -> invalid_arg "test conversion does not support this binding"
  in
  assert_equal_inputs
    "bind_query_inputs skips source declarations and binds scalar args"
    [ DT.Input_source_decl "$other"; DT.Input_scalar ("name", DT.Result_value (DT.String "Ivan")) ]
    (Internal.Query.bind_query_inputs
       ~query_input_of_arg
       ~consume_rules:false
       [ DT.Input_source_decl "$other"; DT.Input_scalar_decl "name" ]
       [ DT.Arg_scalar (DT.Result_value (DT.String "Ivan")) ]);
  assert_equal_inputs
    "bind_query_inputs preserves already bound inputs"
    [ DT.Input_collection_ignore [ DT.Result_value (DT.Int64 1L) ]; DT.Input_ignore ]
    (Internal.Query.bind_query_inputs
       ~query_input_of_arg
       ~consume_rules:false
       [ DT.Input_collection_ignore [ DT.Result_value (DT.Int64 1L) ]; DT.Input_ignore_decl ]
       [ DT.Arg_scalar (DT.Result_value (DT.String "ignored")) ]);
  assert_equal_inputs
    "bind_query_inputs skips rules declarations when rules are implicit"
    [ DT.Input_rules_decl; DT.Input_scalar ("name", DT.Result_value (DT.String "Oleg")) ]
    (Internal.Query.bind_query_inputs
       ~query_input_of_arg
       ~consume_rules:false
       [ DT.Input_rules_decl; DT.Input_scalar_decl "name" ]
       [ DT.Arg_scalar (DT.Result_value (DT.String "Oleg")) ]);
  assert_equal_inputs
    "bind_query_inputs consumes rules declarations when requested"
    [ DT.Input_rules []; DT.Input_scalar ("name", DT.Result_value (DT.String "Oleg")) ]
    (Internal.Query.bind_query_inputs
       ~query_input_of_arg
       ~consume_rules:true
       [ DT.Input_rules_decl; DT.Input_scalar_decl "name" ]
       [ DT.Arg_rules []; DT.Arg_scalar (DT.Result_value (DT.String "Oleg")) ]);
  assert_raises_invalid_arg "bind_query_inputs rejects too few args" (fun () ->
    ignore
      (Internal.Query.bind_query_inputs
         ~query_input_of_arg
         ~consume_rules:false
         [ DT.Input_scalar_decl "name" ]
         []));
  assert_raises_invalid_arg "bind_query_inputs rejects too many args" (fun () ->
    ignore
      (Internal.Query.bind_query_inputs
         ~query_input_of_arg
         ~consume_rules:false
         [ DT.Input_scalar_decl "name" ]
         [ DT.Arg_scalar (DT.Result_value (DT.String "Ivan")); DT.Arg_scalar (DT.Result_value (DT.String "extra")) ]))

let test_query_namespace__test_callable_helpers () =
  let predicate = function
    | [ DT.Result_value (DT.Int64 value) ] -> value > 10L
    | _ -> false
  in
  let query_function = function
    | [ DT.Result_value (DT.Int64 value) ] -> Some [ DT.Result_value (DT.Int64 (Int64.add value 1L)) ]
    | _ -> None
  in
  let aggregate = function
    | values -> DT.Result_value (DT.Int64 (Int64.of_int (List.length values)))
  in
  let callables =
    Internal.Query.empty_query_callables
    |> fun callables -> { callables with Internal.Query.callable_predicates = [ "large?", predicate ] }
    |> fun callables -> { callables with Internal.Query.callable_functions = [ "inc", query_function ] }
    |> fun callables -> { callables with Internal.Query.callable_aggregates = [ "count-values", aggregate ] }
    |> fun callables -> Internal.Query.alias_callable callables "bigger?" "large?"
  in
  (match Internal.Query.callable_predicate callables "bigger?" with
   | Some f ->
     if not (f [ DT.Result_value (DT.Int64 11L) ]) then failwith "callable_predicate should resolve aliases"
   | None -> failwith "callable_predicate should find aliased predicates");
  (match Internal.Query.callable_function callables "inc" with
   | Some f ->
     if f [ DT.Result_value (DT.Int64 1L) ] <> Some [ DT.Result_value (DT.Int64 2L) ] then
       failwith "callable_function should return stored functions"
   | None -> failwith "callable_function should find stored functions");
  (match Internal.Query.resolve_callable_aggregate callables (DT.CustomVar "count-values") with
   | DT.Custom f ->
     if f [ DT.Result_value (DT.Int64 1L); DT.Result_value (DT.Int64 2L) ] <> DT.Result_value (DT.Int64 2L) then
       failwith "resolve_callable_aggregate should return stored aggregate functions"
   | _ -> failwith "resolve_callable_aggregate should resolve custom aggregate variables");
  if not (Internal.Query.has_callable callables "bigger?") then failwith "has_callable should resolve aliases";
  assert_raises_invalid_arg "resolve_callable_aggregate rejects unknown custom aggregates" (fun () ->
    ignore (Internal.Query.resolve_callable_aggregate callables (DT.CustomVar "missing")));
  let rule = { DT.rule_name = "parent"; rule_params = [ "e" ]; rule_body = [] } in
  assert_equal_rules
    "query_rules_of_inputs extracts supplied rules"
    [ rule ]
    (Internal.Query.query_rules_of_inputs [ DT.Input_rules [ rule ]; DT.Input_ignore ]);
  let callables =
    Internal.Query.query_callables_of_inputs
      [ DT.Input_predicate ("large?", predicate)
      ; DT.Input_function ("inc", query_function)
      ; DT.Input_aggregate ("count-values", aggregate)
      ; DT.Input_ignore
      ]
  in
  if not (Internal.Query.has_callable callables "large?") then
    failwith "query_callables_of_inputs should collect predicate inputs";
  if not (Internal.Query.has_callable callables "inc") then
    failwith "query_callables_of_inputs should collect function inputs";
  if not (Internal.Query.has_callable callables "count-values") then
    failwith "query_callables_of_inputs should collect aggregate inputs"

let test_query_namespace__test_rule_helpers () =
  let parent_1 = { DT.rule_name = "parent"; rule_params = [ "e" ]; rule_body = [] } in
  let parent_2 = { DT.rule_name = "parent"; rule_params = [ "e"; "child" ]; rule_body = [] } in
  let ancestor = { DT.rule_name = "ancestor"; rule_params = [ "e"; "child" ]; rule_body = [] } in
  assert_equal_rules
    "matching_rules filters by name and arity"
    [ parent_2 ]
    (Internal.Query.matching_rules [ parent_1; parent_2; ancestor ] "parent" 2);
  assert_equal_rules
    "matching_rules_exn returns matching rules"
    [ ancestor ]
    (Internal.Query.matching_rules_exn [ parent_1; parent_2; ancestor ] "ancestor" 2);
  assert_raises_invalid_arg "matching_rules_exn rejects missing rules" (fun () ->
    ignore (Internal.Query.matching_rules_exn [ parent_1 ] "missing" 1));
  assert_equal_grouped_bindings
    "project_binding keeps only requested vars"
    [ [ "name", DT.Result_value (DT.String "Ivan"); "age", DT.Result_value (DT.Int64 30L) ] ]
    [ Internal.Query.project_binding
        [ "name"; "age" ]
        [ "name", DT.Result_value (DT.String "Ivan")
        ; "city", DT.Result_value (DT.String "Berlin")
        ; "age", DT.Result_value (DT.Int64 30L)
        ]
    ];
  let predicate = function
    | [ DT.Result_value (DT.Int64 value) ] -> value > 10L
    | _ -> false
  in
  let callables =
    Internal.Query.empty_query_callables
    |> fun callables -> { callables with Internal.Query.callable_predicates = [ "large?", predicate ] }
  in
  let aliased =
    Internal.Query.rule_invocation_callables
      callables
      []
      { DT.rule_name = "large-rule"; rule_params = [ "p" ]; rule_body = [] }
      [ (DT.QVar "large?") ]
  in
  if not (Internal.Query.has_callable aliased "p") then
    failwith "rule_invocation_callables should alias unbound callable args to rule params";
  let unchanged =
    Internal.Query.rule_invocation_callables
      callables
      [ "large?", DT.Result_value (DT.Bool true) ]
      { DT.rule_name = "large-rule"; rule_params = [ "p" ]; rule_body = [] }
      [ (DT.QVar "large?") ]
  in
  if Internal.Query.has_callable unchanged "p" then
    failwith "rule_invocation_callables should not alias already-bound vars";
  if not (Internal.Query.clause_calls_rule "parent" (DT.Rule ("parent", [ (DT.QVar "e") ]))) then
    failwith "clause_calls_rule should detect direct rule calls";
  if not (Internal.Query.clause_calls_rule "parent" (DT.SourceRule ("other", "parent", [ (DT.QVar "e") ]))) then
    failwith "clause_calls_rule should detect sourced rule calls";
  if
    not
      (Internal.Query.clause_calls_rule
         "parent"
         (DT.Not [ DT.SourceClause ("other", DT.Rule ("parent", [ (DT.QVar "e") ])) ]))
  then
    failwith "clause_calls_rule should recurse through not/source clauses";
  if
    not
      (Internal.Query.clause_calls_rule
         "parent"
         (DT.OrJoinRequired
            ( [ "e" ]
            , [ "name" ]
            , [ [ (DT.Pattern ((DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "name"))) ]
              ; [ DT.Rule ("parent", [ (DT.QVar "e") ]) ]
              ] )))
  then
    failwith "clause_calls_rule should recurse through or-join branches";
  if Internal.Query.clause_calls_rule "parent" (DT.DynamicPredicate ("parent", [ (DT.QVar "e") ])) then
    failwith "clause_calls_rule should ignore predicate names";
  let recursive_parent =
    { DT.rule_name = "parent"
    ; rule_params = [ "e"; "child" ]
    ; rule_body =
        [ DT.Rule ("parent", [ (DT.QVar "e"); (DT.QVar "middle") ])
        ; DT.Rule ("parent", [ (DT.QVar "middle"); (DT.QVar "child") ])
        ]
    }
  in
  let terminal_parent =
    { DT.rule_name = "parent"
    ; rule_params = [ "e"; "child" ]
    ; rule_body = [ (DT.Pattern ((DT.QVar "e"), (DT.QAttr "parent"), (DT.QVar "child"))) ]
    }
  in
  let rule_call_key = "", "parent", [ Some (DT.Result_entity (1L)); Some (DT.Result_entity (2L)) ] in
  assert_equal_rules
    "matching_rules_for_call returns all candidates when call is not active"
    [ recursive_parent; terminal_parent ]
    (Internal.Query.matching_rules_for_call
       []
       rule_call_key
       [ recursive_parent; terminal_parent ]
       "parent"
       2);
  assert_equal_rules
    "matching_rules_for_call filters recursive candidates when call is active"
    [ terminal_parent ]
    (Internal.Query.matching_rules_for_call
       [ rule_call_key ]
       rule_call_key
       [ recursive_parent; terminal_parent ]
       "parent"
       2)

let test_query_namespace__test_variable_discovery_helpers () =
  assert_equal_string_list
    "vars_of_query_term returns vars only"
    [ "e" ]
    (Internal.Query.vars_of_query_term (DT.QVar "e"));
  assert_equal_string_list
    "vars_of_query_term ignores literals"
    []
    (Internal.Query.vars_of_query_term (DT.QValue (DT.String "Ivan")));
  assert_equal_string_list
    "vars_of_query_terms sorts and deduplicates vars"
    [ "a"; "e"; "v" ]
    (Internal.Query.vars_of_query_terms [ (DT.QVar "v"); (DT.QVar "e"); (DT.QVar "v"); (DT.QVar "a"); DT.QWildcard ]);
  assert_equal_string_list
    "vars_of_clause includes data pattern vars"
    [ "a"; "e"; "v" ]
    (Internal.Query.vars_of_clause (DT.Pattern ((DT.QVar "e"), (DT.QVar "a"), (DT.QVar "v"))));
  assert_equal_string_list
    "vars_of_clause includes function outputs and inputs"
    [ "out"; "x"; "y" ]
    (Internal.Query.vars_of_clause (DT.Function ("f", [ (DT.QVar "x"); (DT.QVar "y") ], [ "out" ], fun _ -> None)));
  assert_equal_string_list
    "vars_of_clause drops ignored ground outputs"
    [ "value"; "source" ]
    (Internal.Query.vars_of_clause (GroundTermTuple (DT.QVar "source", [ "_"; "value" ])));
  assert_equal_string_list
    "vars_of_clause includes not-join vars and body vars"
    [ "e"; "name" ]
    (Internal.Query.vars_of_clause
       (DT.NotJoin ([ "e" ], [ DT.Pattern (DT.QVar "e", (DT.QAttr "name"), (DT.QVar "name")) ])));
  assert_equal_string_list
    "vars_of_clause includes required or-join vars and branch vars"
    [ "e"; "name"; "other" ]
    (Internal.Query.vars_of_clause
       (DT.OrJoinRequired
          ( [ "e" ]
          , [ "other" ]
          , [ [ (DT.Pattern ((DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "name"))) ]
            ; [ (DT.Pattern ((DT.QVar "other"), (DT.QAttr "name"), (DT.QVar "name"))) ]
            ] )));
  assert_equal_string_list
    "vars_of_clause delegates through source clauses"
    [ "e"; "v" ]
    (Internal.Query.vars_of_clause (DT.SourceClause ("$", (DT.Pattern ((DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "v"))))))

let test_query_namespace__test_source_discovery_helpers () =
  assert_equal_string_list
    "named_source returns a singleton source list"
    [ "other" ]
    (Internal.Query.named_source "other");
  assert_equal_string_list
    "sources_of_query_term returns only source terms"
    [ "other" ]
    (Internal.Query.sources_of_query_term (DT.QSource "other"));
  assert_equal_string_list
    "sources_of_query_terms preserves repeated source references"
    [ "a"; "b"; "a" ]
    (Internal.Query.sources_of_query_terms [ (DT.QSource "a"); (DT.QVar "ignored"); (DT.QSource "b"); (DT.QSource "a") ]);
  assert_equal_string_list
    "sources_of_optional_query_term handles optional terms"
    [ "other" ]
    (Internal.Query.sources_of_optional_query_term (Some (DT.QSource "other")));
  assert_equal_string_list
    "sources_of_optional_query_term returns empty sources for None"
    []
    (Internal.Query.sources_of_optional_query_term None);
  assert_equal_string_list
    "sources_of_clause includes explicit sources and nested term sources"
    [ "people"; "needle" ]
    (Internal.Query.sources_of_clause
       (DT.SourcePattern ("people", (DT.QVar "e"), (DT.QAttr "name"), (DT.QSource "needle"))));
  assert_equal_string_list
    "sources_of_clause recurses through branch clauses"
    [ "outer"; "inner"; "dynamic" ]
    (Internal.Query.sources_of_clause
       (DT.SourceOr
          ( "outer"
          , [ [ (DT.Pattern ((DT.QSource "inner"), (DT.QAttr "name"), (DT.QVar "name"))) ]
            ; [ DT.DynamicPredicate ("pred", [ (DT.QSource "dynamic") ]) ]
            ] )));
  assert_equal_string_list
    "sources_of_find_spec includes pull sources"
    [ "pull-db" ]
    (Internal.Query.sources_of_find_spec (DT.Find_pull_source ("pull-db", "e", [ DT.Pull_id ])));
  assert_equal_string_list
    "sources_of_find_spec includes aggregate term sources"
    [ "amounts" ]
    (Internal.Query.sources_of_find_spec (DT.Find_aggregate (DT.Sum, [ (DT.QSource "amounts"); (DT.QVar "amount") ])))

let test_query_namespace__test_rule_source_analysis_helpers () =
  if not (Internal.Query.has_rule_clause (DT.Rule ("parent", [ (DT.QVar "e") ]))) then
    failwith "has_rule_clause should detect direct rule clauses";
  if
    not
      (Internal.Query.has_rule_clause
         (DT.SourceOr
            ( "other"
            , [ [ (DT.Pattern ((DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "name"))) ]
              ; [ DT.SourceRule ("other", "parent", [ (DT.QVar "e") ]) ]
              ] )))
  then
    failwith "has_rule_clause should recurse through source/or branches";
  if Internal.Query.has_rule_clause (DT.DynamicPredicate ("parent", [ (DT.QVar "e") ])) then
    failwith "has_rule_clause should ignore dynamic predicates before rule resolution";
  assert_equal_string_list
    "rule_names sorts and deduplicates rule names"
    [ "ancestor"; "parent" ]
    (Internal.Query.rule_names
       [ { DT.rule_name = "parent"; rule_params = [ "e" ]; rule_body = [] }
       ; { DT.rule_name = "ancestor"; rule_params = [ "e" ]; rule_body = [] }
       ; { DT.rule_name = "parent"; rule_params = [ "e"; "child" ]; rule_body = [] }
       ]);
  if
    Internal.Query.resolve_dynamic_rule_clause [ "parent" ] (DT.DynamicPredicate ("parent", [ (DT.QVar "e") ]))
    <> DT.Rule ("parent", [ (DT.QVar "e") ])
  then
    failwith "resolve_dynamic_rule_clause should resolve matching dynamic predicates";
  if
    Internal.Query.resolve_dynamic_rule_clause
      [ "parent" ]
      (DT.SourceClause ("other", DT.DynamicPredicate ("parent", [ (DT.QVar "e") ])))
    <> DT.SourceRule ("other", "parent", [ (DT.QVar "e") ])
  then
    failwith "resolve_dynamic_rule_clause should preserve sources for resolved rules";
  if
    Internal.Query.resolve_dynamic_rule_clause
      [ "parent" ]
      (DT.Not [ DT.Or [ [ DT.DynamicPredicate ("parent", [ (DT.QVar "e") ]) ] ] ])
    <> DT.Not [ DT.Or [ [ DT.Rule ("parent", [ (DT.QVar "e") ]) ] ] ]
  then
    failwith "resolve_dynamic_rule_clause should recurse through nested branches";
  if
    Internal.Query.resolve_dynamic_rule_clause [ "parent" ] (DT.DynamicPredicate ("large?", [ (DT.QVar "e") ]))
    <> DT.DynamicPredicate ("large?", [ (DT.QVar "e") ])
  then
    failwith "resolve_dynamic_rule_clause should leave non-rule predicates unchanged";
  let rule =
    { DT.rule_name = "ancestor"
    ; rule_params = [ "e"; "child" ]
    ; rule_body =
        [ DT.DynamicPredicate ("parent", [ (DT.QVar "e"); (DT.QVar "child") ])
        ; DT.DynamicPredicate ("large?", [ (DT.QVar "child") ])
        ]
    }
  in
  assert_equal_rules
    "resolve_dynamic_rule resolves every clause in a rule body"
    [ { rule with
        rule_body =
          [ DT.Rule ("parent", [ (DT.QVar "e"); (DT.QVar "child") ])
          ; DT.DynamicPredicate ("large?", [ (DT.QVar "child") ])
          ]
      }
    ]
    [ Internal.Query.resolve_dynamic_rule [ "parent" ] rule ];
  if not (Internal.Query.find_spec_uses_default_source (DT.Find_pull_source ("$", "e", [ DT.Pull_id ]))) then
    failwith "find_spec_uses_default_source should detect explicit default pull sources";
  if Internal.Query.find_spec_uses_default_source (DT.Find_pull_source ("other", "e", [ DT.Pull_id ])) then
    failwith "find_spec_uses_default_source should ignore named pull sources";
  if Internal.Query.find_spec_uses_default_source (DT.Find_pull ("e", [ DT.Pull_id ])) then
    failwith "find_spec_uses_default_source should ignore implicit pull specs";
  if not (Internal.Query.clause_uses_default_source (DT.Pattern ((DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "name")))) then
    failwith "clause_uses_default_source should treat bare patterns as default-source clauses";
  if
    Internal.Query.clause_uses_default_source
      (DT.SourceClause ("other", DT.SourcePattern ("other", (DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "name"))))
  then
    failwith "clause_uses_default_source should ignore named-only source clauses";
  if
    not
      (Internal.Query.clause_uses_default_source
         (DT.SourceNot
            ( "other"
            , [ DT.SourcePattern ("other", (DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "name")) ; (DT.Pattern ((DT.QVar "e"), (DT.QAttr "age"), (DT.QVar "age")))
              ] )))
  then
    failwith "clause_uses_default_source should recurse through nested clauses";
  assert_equal_inputs
    "infer_default_inputs adds default source for queries without explicit :in"
    [ DT.Input_source_decl "$"; DT.Input_scalar_decl "name" ]
    (Internal.Query.infer_default_inputs
       None
       [ DT.Find_pull_source ("$", "e", [ DT.Pull_id ]) ]
       []
       [ DT.Input_scalar_decl "name" ]);
  assert_equal_inputs
    "infer_default_inputs does not add default source when :in is explicit"
    [ DT.Input_scalar_decl "name" ]
    (Internal.Query.infer_default_inputs
       (Some (QueryFormVector []))
       [ DT.Find_pull_source ("$", "e", [ DT.Pull_id ]) ]
       []
       [ DT.Input_scalar_decl "name" ]);
  assert_equal_inputs
    "infer_default_inputs leaves named-source-only queries unchanged"
    [ DT.Input_source_decl "$other" ]
    (Internal.Query.infer_default_inputs
       None
       []
       [ DT.SourcePattern ("other", (DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "name")) ]
       [ DT.Input_source_decl "$other" ])

let test_query_namespace__test_query_validation_helpers () =
  assert_equal_string_list
    "query_term_vars preserves query var order and duplicates"
    [ "e"; "e"; "name" ]
    (Internal.Query.query_term_vars [ (DT.QVar "e"); (DT.QSource "other"); (DT.QVar "e"); (DT.QVar "name"); DT.QWildcard ]);
  assert_equal_string_list
    "vars_of_find_spec includes aggregate input vars and dynamic aggregate vars"
    [ "amount"; "n" ]
    (Internal.Query.vars_of_find_spec (DT.Find_aggregate (DT.MinNVar "n", [ (DT.QVar "amount") ])));
  assert_equal_string_list
    "vars_of_input_binding walks nested bindings"
    [ "name"; "city"; "country" ]
    (Internal.Query.vars_of_input_binding
       (Bind_tuple
          [  Bind_scalar "name"
          ; Bind_ignore
          ; Bind_collection (Bind_tuple [  Bind_scalar "city"; Bind_scalar "country" ])
          ]));
  assert_equal_string_list
    "vars_of_input drops ignored tuple columns"
    [ "name"; "age" ]
    (Internal.Query.vars_of_input (DT.Input_relation_decl [ "name"; "_"; "age" ]));
  assert_equal_string_list
    "vars_of_input extracts nested tuple bindings"
    [ "name"; "city" ]
    (Internal.Query.vars_of_input
       (DT.Input_nested_tuple_decl [ Bind_scalar "name"; Bind_collection (Bind_scalar "city") ]));
  (match Internal.Query.source_of_input (DT.Input_source_decl "$other") with
   | Some "$other" -> ()
   | _ -> failwith "source_of_input should return declared source names");
  (match Internal.Query.source_of_input (DT.Input_scalar_decl "name") with
   | None -> ()
   | Some _ -> failwith "source_of_input should ignore non-source inputs");
  Internal.Query.ensure_distinct_input_vars [ DT.Input_scalar_decl "name"; DT.Input_relation_decl [ "age" ] ];
  assert_raises_invalid_arg_message
    "ensure_distinct_input_vars rejects repeated vars"
    "Vars used in :in should be distinct"
    (fun () -> Internal.Query.ensure_distinct_input_vars [ DT.Input_scalar_decl "name"; DT.Input_tuple_decl [ "name" ] ]);
  Internal.Query.ensure_distinct_input_sources [ DT.Input_source_decl "$"; DT.Input_source_decl "$other" ];
  assert_raises_invalid_arg_message
    "ensure_distinct_input_sources rejects repeated sources"
    "Vars used in :in should be distinct"
    (fun () -> Internal.Query.ensure_distinct_input_sources [ DT.Input_source_decl "$"; DT.Input_source_decl "$" ]);
  assert_equal_string "format_query_vars prints query vars" "[?age ?name]" (Internal.Query.format_query_vars [ "age"; "name" ]);
  assert_equal_string "format_source_vars prints source vars" "[$ $other]" (Internal.Query.format_source_vars [ "$"; "other" ]);
  let valid_query : DT.query =
    { DT.find = [ DT.Find_var "e" ]
    ; inputs = [ DT.Input_source_decl "$" ]
    ; with_vars = [ "name" ]
    ; rules = []
    ; where = [ DT.Pattern ((DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "name")) ]
    }
  in
  if Internal.Query.validate_query valid_query <> valid_query then
    failwith "validate_query should return valid queries unchanged";
  assert_raises_invalid_arg_message
    "validate_query rejects unknown find vars"
    "Query for unknown vars: [?missing]"
    (fun () -> ignore (Internal.Query.validate_query { valid_query with find = [ DT.Find_var "missing" ] }));
  assert_raises_invalid_arg_message
    "validate_query rejects unknown with vars"
    "Query for unknown vars: [?missing]"
    (fun () -> ignore (Internal.Query.validate_query { valid_query with with_vars = [ "missing" ] }));
  assert_raises_invalid_arg_message
    "validate_query rejects shared find and with vars"
    ":find and :with should not use same variables: [?e]"
    (fun () -> ignore (Internal.Query.validate_query { valid_query with with_vars = [ "e" ] }));
  assert_raises_invalid_arg_message
    "validate_query rejects undeclared sources"
    "Where uses unknown source vars: [$other]"
    (fun () ->
       ignore
         (Internal.Query.validate_query
            { valid_query with
              inputs = [ DT.Input_source_decl "$" ]
            ; where = [ DT.SourcePattern ("other", (DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "name")) ]
            }))

let test_query_namespace__test_return_map_validation_helpers () =
  assert_equal_string "return_map_name formats :keys" "keys" (Internal.Query.return_map_name (DT.Return_keys [ "name" ]));
  if Internal.Query.return_map_label_count (DT.Return_syms [ "name"; "age" ]) <> 2 then
    failwith "return_map_label_count should count labels";
  let query =
    { DT.find = [ DT.Find_var "name"; DT.Find_var "age" ]
    ; inputs = [ DT.Input_source_decl "$" ]
    ; with_vars = []
    ; rules = []
    ; where =
        [ (DT.Pattern ((DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "name"))) ; (DT.Pattern ((DT.QVar "e"), (DT.QAttr "age"), (DT.QVar "age")))
        ]
    }
  in
  if Internal.Query.validate_query_return_map Return_relation None query <> None then
    failwith "validate_query_return_map should preserve absent return maps";
  if
    Internal.Query.validate_query_return_map Return_tuple (Some (Return_strs [ "name"; "age" ])) query
    <> Some (Return_strs [ "name"; "age" ])
  then
    failwith "validate_query_return_map should return valid return maps";
  assert_raises_invalid_arg_message
    "validate_query_return_map rejects collection returns"
    ":keys does not work with collection :find"
    (fun () ->
       ignore (Internal.Query.validate_query_return_map Return_collection (Some (Return_keys [ "name"; "age" ])) query));
  assert_raises_invalid_arg_message
    "validate_query_return_map rejects scalar returns"
    ":syms does not work with single-scalar :find"
    (fun () ->
       ignore (Internal.Query.validate_query_return_map Return_scalar (Some (Return_syms [ "name"; "age" ])) query));
  assert_raises_invalid_arg_message
    "validate_query_return_map rejects label count mismatch"
    "Count of :strs must match count of :find"
    (fun () ->
       ignore (Internal.Query.validate_query_return_map Return_relation (Some (Return_strs [ "name" ])) query))

let test_query_namespace__test_query_string_helpers () =
  let value_to_string = function
    | DT.String value -> "\"" ^ value ^ "\""
    | DT.Keyword value -> ":" ^ value
    | DT.Bool value -> if value then "true" else "false"
    | DT.Int64 value -> Int64.to_string value
    | value -> failf "unexpected value in test printer: %s" (string_of_int (Hashtbl.hash value))
  in
  assert_equal_string
    "query_term_string formats vars"
    "?name"
    (Internal.Query.query_term_string ~value_to_string (DT.QVar "name"));
  assert_equal_string
    "query_term_string formats lookup refs through the value printer"
    "[:user/email \"a@example.com\"]"
    (Internal.Query.query_term_string
       ~value_to_string
       (DT.QLookupRef ("user/email", DT.String "a@example.com")));
  assert_equal_string
    "query_term_string formats named sources"
    "$other"
    (Internal.Query.query_term_string ~value_to_string (DT.QSource "other"));
  assert_equal_string
    "query_output_var_string preserves wildcards"
    "_"
    (Internal.Query.query_output_var_string "_");
  assert_equal_string
    "query_output_binding_string formats tuple outputs"
    "[?name _]"
    (Internal.Query.query_output_binding_string [ "name"; "_" ]);
  assert_equal_string
    "query_call_string formats callable invocations"
    "(get ?profile :prefs)"
    (Internal.Query.query_call_string
       ~value_to_string
       "get"
       [ (DT.QVar "profile"); (DT.QValue (DT.Keyword "prefs")) ]);
  assert_equal_string
    "numeric_predicate_symbol formats odd?"
    "odd?"
    (Internal.Query.numeric_predicate_symbol OddInteger);
  assert_equal_string
    "arithmetic_op_symbol formats modulo"
    "mod"
    (Internal.Query.arithmetic_op_symbol ModuloNumbers)

let test_query_namespace__test_query_clause_string_helpers () =
  let value_to_string = function
    | DT.String value -> "\"" ^ value ^ "\""
    | DT.Keyword value -> ":" ^ value
    | DT.Bool value -> if value then "true" else "false"
    | DT.Int64 value -> Int64.to_string value
    | value -> failf "unexpected value in test printer: %s" (string_of_int (Hashtbl.hash value))
  in
  assert_equal_string
    "query_clause_string formats data patterns"
    "[?e :name \"Ivan\"]"
    (Internal.Query.query_clause_string
       ~value_to_string
       (DT.Pattern (DT.QVar "e", DT.QAttr "name", DT.QValue (DT.String "Ivan"))));
  assert_equal_string
    "query_clause_string formats source relation patterns"
    "[$other ?name :active]"
    (Internal.Query.query_clause_string
       ~value_to_string
       (DT.SourceRelationPattern ("other", [ DT.QVar "name"; DT.QValue (DT.Keyword "active") ])));
  assert_equal_string
    "query_clause_string formats dynamic collection functions"
    "[(children ?e) [?child ...]]"
    (Internal.Query.query_clause_string
       ~value_to_string
       (DT.DynamicFunctionCollection ("children", [ DT.QVar "e" ], "child")));
  assert_equal_string
    "query_clause_string formats dynamic relation functions"
    "[(pairs ?e) [[?left _]]]"
    (Internal.Query.query_clause_string
       ~value_to_string
       (DynamicFunctionRelation ("pairs", [ (DT.QVar "e") ], [ "left"; "_" ])));
  assert_equal_string
    "query_clause_string formats not clauses"
    "(not [?e :hidden true])"
    (Internal.Query.query_clause_string
       ~value_to_string
       (DT.Not [ (DT.Pattern ((DT.QVar "e"), (DT.QAttr "hidden"), (DT.QValue (DT.Bool true)))) ]));
  assert_equal_string
    "query_clause_string formats required or-join vars"
    "(or-join [[?e] ?name] [?e :name ?name] (and [?other :name ?name] [?other :kind :person]))"
    (Internal.Query.query_clause_string
       ~value_to_string
       (DT.OrJoinRequired
          ( [ "e" ]
          , [ "name" ]
          , [ [ (DT.Pattern ((DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "name"))) ]
            ; [ (DT.Pattern ((DT.QVar "other"), (DT.QAttr "name"), (DT.QVar "name"))) ; (DT.Pattern ((DT.QVar "other"), (DT.QAttr "kind"), (DT.QValue (DT.Keyword "person"))))
              ]
            ] )));
  assert_equal_string
    "query_clause_string formats unknown clauses by var count"
    "<2-var clause>"
    (Internal.Query.query_clause_string
       ~value_to_string
       (DT.NotJoin ([ "e" ], [ DT.Pattern (DT.QVar "e", (DT.QAttr "name"), (DT.QVar "name")) ])));
  assert_equal_string
    "query_var_set_string formats query variables"
    "#{?a ?b}"
    (Internal.Query.query_var_set_string [ "a"; "b" ]);
  assert_equal_string
    "query_var_sets_string formats query var set collections"
    "[#{?a} #{}]"
    (Internal.Query.query_var_sets_string [ [ "a" ]; [] ])

let test_query_namespace__test_binding_validation_helpers () =
  let value_to_string = function
    | DT.String value -> "\"" ^ value ^ "\""
    | DT.Keyword value -> ":" ^ value
    | DT.Bool value -> if value then "true" else "false"
    | DT.Int64 value -> Int64.to_string value
    | value -> failf "unexpected value in test printer: %s" (string_of_int (Hashtbl.hash value))
  in
  let binding = [ "e", DT.Result_entity (1L) ] in
  assert_equal_string_list
    "unbound_vars_of_terms returns sorted missing vars"
    [ "a"; "v" ]
    (Internal.Query.unbound_vars_of_terms binding [ (DT.QVar "v"); (DT.QVar "e"); (DT.QVar "a"); (DT.QVar "v") ]);
  Internal.Query.ensure_query_terms_bound binding [ (DT.QVar "e"); (DT.QValue (DT.String "Ivan")) ] "[?e :name \"Ivan\"]";
  assert_raises_invalid_arg_message
    "ensure_query_terms_bound reports missing vars"
    "Insufficient bindings: #{?a ?v} not bound in [?e ?a ?v]"
    (fun () ->
       Internal.Query.ensure_query_terms_bound binding [ (DT.QVar "e"); (DT.QVar "a"); (DT.QVar "v") ] "[?e ?a ?v]");
  Internal.Query.ensure_not_has_outer_binding
    ~value_to_string
    binding
    [ (DT.Pattern ((DT.QVar "e"), (DT.QAttr "name"), (DT.QValue (DT.String "Ivan")))) ];
  assert_raises_invalid_arg_message
    "ensure_not_has_outer_binding reports not clauses with no outer vars"
    "Insufficient bindings: none of #{?e ?name} is bound in (not [?e :name ?name])"
    (fun () ->
       Internal.Query.ensure_not_has_outer_binding
         ~value_to_string
         []
         [ (DT.Pattern ((DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "name"))) ]);
  let branch_a = [ (DT.Pattern ((DT.QVar "e"), (DT.QAttr "name"), (DT.QVar "name"))) ] in
  let branch_b = [ (DT.Pattern ((DT.QVar "e"), (DT.QAttr "age"), (DT.QVar "age"))) ] in
  assert_equal_string_list
    "vars_of_branch collects branch vars"
    [ "e"; "name" ]
    (Internal.Query.vars_of_branch branch_a);
  assert_equal_string_list
    "free_vars_of_branch subtracts bound vars"
    [ "name" ]
    (Internal.Query.free_vars_of_branch [ "e" ] branch_a);
  assert_raises_invalid_arg_message
    "ensure_or_branch_vars_match reports mismatched free vars"
    "All clauses in 'or' must use same set of free vars, had [#{?name} #{?age}] in (or [?e :name ?name] [?e :age ?age])"
    (fun () -> Internal.Query.ensure_or_branch_vars_match ~value_to_string binding [ branch_a; branch_b ]);
  Internal.Query.ensure_join_vars_bound binding [ "e" ];
  assert_raises_invalid_arg_message
    "ensure_join_vars_bound keeps its legacy message"
    "insufficient bindings"
    (fun () -> Internal.Query.ensure_join_vars_bound binding [ "missing" ]);
  Internal.Query.ensure_join_vars_bound_in_clause binding [ "e" ] "(or-join [?e] ...)";
  assert_raises_invalid_arg_message
    "ensure_join_vars_bound_in_clause reports missing vars"
    "Insufficient bindings: #{?missing} not bound in (or-join [?missing] ...)"
    (fun () ->
       Internal.Query.ensure_join_vars_bound_in_clause binding [ "missing" ] "(or-join [?missing] ...)");
  Internal.Query.ensure_or_join_branches_cover_listed_vars binding [ "name" ] [ branch_a ];
  assert_raises_invalid_arg_message
    "ensure_or_join_branches_cover_listed_vars requires listed vars in every branch"
    "or branches must use same free vars"
    (fun () ->
       Internal.Query.ensure_or_join_branches_cover_listed_vars binding [ "name" ] [ branch_a; branch_b ])

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
