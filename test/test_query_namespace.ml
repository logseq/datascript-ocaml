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
       [ Result_value (Int 1); Result_value (Int 2) ])

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
  test_query_namespace__test_query_string_helpers ();
  test_query_namespace__test_query_clause_string_helpers ();
  test_query_namespace__test_binding_validation_helpers ()
