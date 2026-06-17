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

let () =
  test_query_namespace__test_public_query_api ();
  test_query_namespace__test_aggregate_helpers ();
  test_query_namespace__test_find_grouping_helpers ();
  test_query_namespace__test_input_label_helpers ();
  test_query_namespace__test_input_shape_helpers ();
  test_query_namespace__test_input_binding_helpers ();
  test_query_namespace__test_callable_helpers ()
