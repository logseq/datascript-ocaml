open Datascript_types

module Make (Context : sig
  val parse_pull_pattern : db -> query_form -> pull_selector list
  val empty_db : unit -> db
  val query_form_of_value : value -> query_form
  val source_db : db -> (string * query_source) list -> string -> db
  val query_result_entity_id : db -> query_result -> entity_id option
  val pull : ?visitor:(pull_visit -> unit) -> db -> pull_selector list -> entity_ref -> pulled_entity option
  val query_match_context : db -> Query.match_context
  val eval_query_term : db -> (string * query_result) list -> query_term -> query_result option
  val bind_var : db -> string -> query_result -> (string * query_result) list -> (string * query_result) list option
  val resolve_query_value : db -> value -> value option
  val entity_id_of_ref : db -> entity_ref -> entity_id option
  val edn_string_of_value : value -> string
end) = struct
  open Context

  let pull_pattern_of_result = function
    | Result_value value -> parse_pull_pattern (empty_db ()) (query_form_of_value value)
    | Result_entity _ | Result_attr _ | Result_db _ | Result_pull _ -> invalid_arg "pull pattern input must be a value"
  
  let collect_find_specs db sources bindings find =
    let rec collect acc = function
      | [] -> Some (List.rev acc)
      | Find_var var :: rest ->
        (match List.assoc_opt var bindings with
         | Some value -> collect (value :: acc) rest
         | None -> None)
      | Find_pull (var, selector) :: rest ->
        let pull_db = source_db db sources "$" in
        (match Option.bind (List.assoc_opt var bindings) (query_result_entity_id pull_db) with
         | Some entity_id ->
           (match pull pull_db selector (Entity_id entity_id) with
            | Some entity -> collect (Result_pull entity :: acc) rest
            | None -> None)
         | None -> None)
      | Find_pull_var (var, pattern_var) :: rest ->
        let pull_db = source_db db sources "$" in
        (match
           Option.bind (List.assoc_opt var bindings) (query_result_entity_id pull_db),
           List.assoc_opt pattern_var bindings
         with
         | Some entity_id, Some pattern ->
           (match pull pull_db (pull_pattern_of_result pattern) (Entity_id entity_id) with
            | Some entity -> collect (Result_pull entity :: acc) rest
            | None -> None)
         | _ -> None)
      | Find_pull_source (source, var, selector) :: rest ->
        let pull_db = source_db db sources source in
        (match Option.bind (List.assoc_opt var bindings) (query_result_entity_id pull_db) with
         | Some entity_id ->
           (match pull pull_db selector (Entity_id entity_id) with
            | Some entity -> collect (Result_pull entity :: acc) rest
            | None -> None)
         | None -> None)
      | Find_pull_source_var (source, var, pattern_var) :: rest ->
        let pull_db = source_db db sources source in
        (match
           Option.bind (List.assoc_opt var bindings) (query_result_entity_id pull_db),
           List.assoc_opt pattern_var bindings
         with
         | Some entity_id, Some pattern ->
           (match pull pull_db (pull_pattern_of_result pattern) (Entity_id entity_id) with
            | Some entity -> collect (Result_pull entity :: acc) rest
            | None -> None)
         | _ -> None)
      | Find_aggregate _ :: rest -> collect acc rest
    in
    collect [] find
  
  let has_aggregates = Query.has_aggregates
  
  let aggregate_result = Built_ins.aggregate_result
  
  let resolve_dynamic_aggregate = Query.resolve_dynamic_aggregate
  
  let aggregate_param_vars = Query.aggregate_param_vars
  
  let query_term_vars = Query.query_term_vars
  
  let aggregate_extra_args db sources group_bindings terms =
    Query.aggregate_extra_args (query_match_context db) db sources group_bindings terms
  
  let aggregate_values db sources group_bindings terms =
    Query.aggregate_values (query_match_context db) db sources group_bindings terms
  
  let aggregate_input_values = Query.aggregate_input_values
  
  let empty_query_callables = Query.empty_query_callables
  
  let callable_predicate = Query.callable_predicate
  
  let callable_function = Query.callable_function
  
  let resolve_callable_aggregate = Query.resolve_callable_aggregate
  
  let group_by_key = Query.group_by_key
  
  let grouping_vars_of_find = Query.grouping_vars_of_find
  
  let aggregate_rows ?(callables = empty_query_callables) db sources bindings find =
    let group_vars = grouping_vars_of_find find in
    bindings
    |> List.filter_map (fun binding ->
      Query.collect_find_vars binding group_vars
      |> Option.map (fun key -> key, binding))
    |> group_by_key
    |> List.filter_map (fun (key, group_bindings) ->
      let group_binding = List.combine group_vars key in
      let rec build_row acc = function
        | [] -> Some (List.rev acc)
        | Find_var var :: rest ->
          (match List.assoc_opt var group_binding with
           | Some value -> build_row (value :: acc) rest
           | None -> None)
        | Find_pull (var, selector) :: rest ->
          let pull_db = source_db db sources "$" in
          (match Option.bind (List.assoc_opt var group_binding) (query_result_entity_id pull_db) with
           | Some entity_id ->
             (match pull pull_db selector (Entity_id entity_id) with
              | Some entity -> build_row (Result_pull entity :: acc) rest
              | None -> None)
           | None -> None)
        | Find_pull_var (var, pattern_var) :: rest ->
          let pull_db = source_db db sources "$" in
          (match
             Option.bind (List.assoc_opt var group_binding) (query_result_entity_id pull_db),
             List.assoc_opt pattern_var group_binding
           with
           | Some entity_id, Some pattern ->
             (match pull pull_db (pull_pattern_of_result pattern) (Entity_id entity_id) with
              | Some entity -> build_row (Result_pull entity :: acc) rest
              | None -> None)
           | _ -> None)
        | Find_pull_source (source, var, selector) :: rest ->
          let pull_db = source_db db sources source in
          (match Option.bind (List.assoc_opt var group_binding) (query_result_entity_id pull_db) with
           | Some entity_id ->
             (match pull pull_db selector (Entity_id entity_id) with
              | Some entity -> build_row (Result_pull entity :: acc) rest
              | None -> None)
           | None -> None)
        | Find_pull_source_var (source, var, pattern_var) :: rest ->
          let pull_db = source_db db sources source in
          (match
             Option.bind (List.assoc_opt var group_binding) (query_result_entity_id pull_db),
             List.assoc_opt pattern_var group_binding
           with
           | Some entity_id, Some pattern ->
             (match pull pull_db (pull_pattern_of_result pattern) (Entity_id entity_id) with
              | Some entity -> build_row (Result_pull entity :: acc) rest
              | None -> None)
           | _ -> None)
        | Find_aggregate (aggregate, terms) :: rest ->
          let values = aggregate_values db sources group_bindings terms in
          let aggregate =
            resolve_dynamic_aggregate aggregate group_bindings
            |> resolve_callable_aggregate callables
          in
          let values = aggregate_input_values aggregate (aggregate_extra_args db sources group_bindings terms) values in
          build_row (aggregate_result aggregate values :: acc) rest
      in
      build_row [] find)
    |> List.sort_uniq compare
  
  let aggregate_rows_with ?(callables = empty_query_callables) db sources bindings find with_vars =
    let group_vars = grouping_vars_of_find find in
    let aggregate_vars =
      List.concat_map
        (function
          | Find_aggregate (aggregate, terms) ->
            query_term_vars terms @ aggregate_param_vars aggregate
          | Find_var _ | Find_pull _ | Find_pull_var _ | Find_pull_source _ | Find_pull_source_var _ -> [])
        find
    in
    let dedupe_vars = group_vars @ aggregate_vars @ with_vars |> List.sort_uniq compare in
    let bindings =
      bindings
      |> List.filter_map (fun binding ->
        Query.collect_find_vars binding dedupe_vars
        |> Option.map (fun key -> key, binding))
      |> List.sort_uniq (fun (left, _) (right, _) -> compare left right)
      |> List.map snd
    in
    aggregate_rows ~callables db sources bindings find
  
  let collect_query_row_with_vars db sources find with_vars binding =
    match collect_find_specs db sources binding find, Query.collect_find_vars binding with_vars with
    | Some row, Some with_values -> Some (row, with_values)
    | _ -> None
  
  let non_aggregate_rows_with db sources bindings find with_vars =
    bindings
    |> List.filter_map (collect_query_row_with_vars db sources find with_vars)
    |> List.sort_uniq compare
    |> List.map fst
  
  let rule_invocation_binding db outer_binding rule terms =
    if List.length rule.rule_params <> List.length terms then
      invalid_arg ("rule arity mismatch: " ^ rule.rule_name);
    List.fold_left2
      (fun rule_binding param term ->
        match rule_binding with
        | None -> None
        | Some rule_binding ->
          (match eval_query_term db outer_binding term with
           | Some value -> bind_var db param value rule_binding
           | None -> Some rule_binding))
      (Some [])
      rule.rule_params
      terms
  
  let propagate_rule_binding db outer_binding rule_binding rule terms =
    List.fold_left2
      (fun outer_binding param term ->
        match outer_binding, term with
        | None, _ -> None
        | Some outer_binding, QVar var ->
          (match List.assoc_opt param rule_binding with
           | Some value -> bind_var db var value outer_binding
           | None -> Some outer_binding)
        | Some outer_binding, QWildcard -> Some outer_binding
        | Some outer_binding, _ -> Some outer_binding)
      (Some outer_binding)
      rule.rule_params
      terms
  
  let rule_invocation_callables = Query.rule_invocation_callables
  
  let resolve_query_input_result db = function
    | Result_value value ->
      Option.map (fun _ -> Result_value value) (resolve_query_value db value)
    | result -> Some result
  
  let query_input_context db : Query.input_context =
    { resolve_query_input_result = resolve_query_input_result db
    ; bind_var = (fun var value bindings -> bind_var db var value bindings)
    ; entity_id_of_ref = entity_id_of_ref db
    }
  
  let bind_relation_row db bindings vars row =
    Query.bind_relation_row (query_input_context db) bindings vars row
  
  let collection_values_of_input db value =
    Query.collection_values_of_input (query_input_context db) value
  
  let row_values_of_input db value =
    Query.row_values_of_input (query_input_context db) value
  
  let eval_ground_term_tuple db bindings result output_vars =
    Query.eval_ground_term_tuple (query_input_context db) bindings result output_vars
  
  let eval_ground_term_relation db bindings result output_vars =
    Query.eval_ground_term_relation (query_input_context db) bindings result output_vars
  
  let apply_query_input db bindings input =
    Query.apply_query_input (query_input_context db) bindings input
  
  let query_input_decl_binding_string = Query.query_input_decl_binding_string
  
  let query_result_input_string = function
    | Result_value value -> edn_string_of_value value
    | Result_entity entity_id -> string_of_int entity_id
    | Result_attr attr -> ":" ^ attr
    | Result_db _ -> "<db>"
    | Result_pull _ -> "<pull>"
  
  let query_result_collection_string values =
    "[" ^ String.concat " " (List.map query_result_input_string values) ^ "]"
  
  let query_input_of_arg decl arg =
    let values_of_collection_result = Query.values_of_collection_result in
    let row_of_collection_value = Query.row_of_collection_result in
    let row_of_scalar_sequence = Query.row_of_scalar_sequence in
    let cannot_bind_value_to kind value =
      invalid_arg
        ( "Cannot bind value "
        ^ query_result_input_string value
        ^ " to "
        ^ kind
        ^ " "
        ^ query_input_decl_binding_string decl )
    in
    let row_for_tuple_binding vars value =
      match values_of_collection_result value with
      | None -> cannot_bind_value_to "tuple" value
      | Some row ->
        if List.length row < List.length vars then
          invalid_arg
            ( "Not enough elements in a collection "
            ^ query_result_collection_string row
            ^ " to bind tuple "
            ^ query_input_decl_binding_string decl )
        else if List.length row > List.length vars then
          invalid_arg
            ( "Too many elements in a collection "
            ^ query_result_collection_string row
            ^ " to bind tuple "
            ^ query_input_decl_binding_string decl )
        else
          row
    in
    let rows_of_map = Query.rows_of_map_entries in
    match decl, arg with
    | Input_ignore_decl, _ -> Input_ignore
    | Input_scalar_decl var, Arg_scalar value -> Input_scalar (var, value)
    | Input_scalar_decl var, Arg_entity_ref entity_ref -> Input_entity_ref (var, entity_ref)
    | Input_collection_decl var, Arg_collection values -> Input_collection (var, values)
    | Input_collection_decl var, Arg_scalar value ->
      (match values_of_collection_result value with
       | Some values -> Input_collection (var, values)
       | None -> cannot_bind_value_to "collection" value)
    | Input_collection_ignore_decl, Arg_collection values -> Input_collection_ignore values
    | Input_collection_ignore_decl, Arg_scalar value ->
      (match values_of_collection_result value with
       | Some values -> Input_collection_ignore values
       | None -> invalid_arg "query input argument does not match :in binding")
    | Input_nested_collection_decl binding, Arg_collection values ->
      Input_nested_collection (binding, values)
    | Input_nested_collection_decl binding, Arg_scalar value ->
      (match values_of_collection_result value with
       | Some values -> Input_nested_collection (binding, values)
       | None -> invalid_arg "query input argument does not match :in binding")
    | Input_tuple_decl vars, Arg_tuple row -> Input_tuple (vars, row)
    | Input_tuple_decl vars, Arg_scalar value -> Input_tuple (vars, row_for_tuple_binding vars value)
    | Input_relation_decl vars, Arg_relation rows -> Input_relation (vars, rows)
    | Input_relation_decl vars, Arg_collection rows ->
      Input_relation (vars, List.map row_of_collection_value rows)
    | Input_relation_decl vars, Arg_scalar (Result_value (Map entries)) ->
      Input_relation (vars, rows_of_map entries)
    | Input_relation_decl vars, Arg_scalar value ->
      (match values_of_collection_result value with
       | Some rows -> Input_relation (vars, List.map row_of_collection_value rows)
       | None -> invalid_arg "query input argument does not match :in binding")
    | Input_nested_tuple_decl bindings, Arg_tuple row -> Input_nested_tuple (bindings, row)
    | Input_nested_tuple_decl bindings, Arg_scalar value ->
      Input_nested_tuple (bindings, row_of_scalar_sequence value)
    | Input_nested_relation_decl bindings, Arg_relation rows -> Input_nested_relation (bindings, rows)
    | Input_nested_relation_decl bindings, Arg_collection rows ->
      Input_nested_relation (bindings, List.map row_of_collection_value rows)
    | Input_nested_relation_decl bindings, Arg_scalar (Result_value (Map entries)) ->
      Input_nested_relation (bindings, rows_of_map entries)
    | Input_nested_relation_decl bindings, Arg_scalar value ->
      (match values_of_collection_result value with
       | Some rows -> Input_nested_relation (bindings, List.map row_of_collection_value rows)
       | None -> invalid_arg "query input argument does not match :in binding")
    | Input_scalar_decl var, Arg_predicate predicate -> Input_predicate (var, predicate)
    | Input_scalar_decl var, Arg_function f -> Input_function (var, f)
    | Input_scalar_decl var, Arg_aggregate f -> Input_aggregate (var, f)
    | Input_rules_decl, Arg_rules rules -> Input_rules rules
    | Input_scalar_decl _, _
    | Input_collection_decl _, _
    | Input_collection_ignore_decl, _
    | Input_nested_collection_decl _, _
    | Input_tuple_decl _, _
    | Input_relation_decl _, _
    | Input_nested_tuple_decl _, _
    | Input_nested_relation_decl _, _ ->
      invalid_arg "query input argument does not match :in binding"
    | (Input_scalar _
      | Input_entity_ref _
      | Input_collection _
      | Input_collection_ignore _
      | Input_nested_collection _
      | Input_tuple _
      | Input_nested_tuple _
      | Input_nested_relation _
      | Input_predicate _
      | Input_function _
      | Input_aggregate _
      | Input_rules _
      | Input_relation _
      | Input_ignore
      | Input_source_decl _
      | Input_rules_decl), _ ->
      invalid_arg "bound query inputs do not consume supplied arguments"
  
  let bind_query_inputs ~consume_rules declarations args =
    Query.bind_query_inputs ~query_input_of_arg ~consume_rules declarations args
  
  let query_callables_of_inputs = Query.query_callables_of_inputs
  
  let query_rules_of_inputs = Query.query_rules_of_inputs
  
  let initial_query_context db query input_args =
    let inputs = bind_query_inputs ~consume_rules:(query.rules = []) query.inputs input_args in
    ( query_callables_of_inputs inputs
    , List.fold_left (apply_query_input db) [ [] ] inputs
    , query_rules_of_inputs inputs )
  
end
