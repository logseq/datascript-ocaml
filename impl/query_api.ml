open Datascript_types

type bindings = (string * query_result) list
type rule_call_key = string * string * query_result option list

module Make (Context : sig
  val empty_db : unit -> db
  val validate_rule_arities : query_rule list -> query_rule list
  val initial_query_context : db -> query -> query_arg list -> Query.query_callables * bindings list * query_rule list
  val eval_clauses :
    ?active_rules:rule_call_key list ->
    ?callables:Query.query_callables ->
    ?default_source:query_source ->
    db ->
    (string * query_source) list ->
    query_rule list ->
    bindings list ->
    query_clause list ->
    bindings list
  val eval_relation_rows :
    db ->
    (string * query_source) list ->
    query_rule list ->
    bindings list ->
    query_clause list ->
    (string list * query_result list list * bool) option
  val has_aggregates : find_spec list -> bool
  val aggregate_rows : ?callables:Query.query_callables -> db -> (string * query_source) list -> bindings list -> find_spec list -> query_result list list
  val aggregate_rows_with : ?callables:Query.query_callables -> db -> (string * query_source) list -> bindings list -> find_spec list -> string list -> query_result list list
  val non_aggregate_rows_with : db -> (string * query_source) list -> bindings list -> find_spec list -> string list -> query_result list list
  val collect_find_specs : db -> (string * query_source) list -> bindings -> find_spec list -> query_result list option
  val parse_query_string_with_pull_context : ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> string -> query
  val parse_query_return_string_with_pull_context : ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> string -> query_return * query
  val parse_query_return_map_string_with_pull_context : ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> string -> query_return * query_return_map option * query
  val compare_value : value -> value -> int
end) = struct
  open Context

  let ( let* ) = Option.bind

  let rule_names = Query.rule_names
  let resolve_dynamic_rule_clause = Query.resolve_dynamic_rule_clause
  let resolve_dynamic_rule = Query.resolve_dynamic_rule

  let query_rules_and_where query input_rules =
    let rules = validate_rule_arities (query.rules @ input_rules) in
    let names = rule_names rules in
    List.map (resolve_dynamic_rule names) rules, List.map (resolve_dynamic_rule_clause names) query.where

  let query_callables_empty (callables : Query.query_callables) =
    callables.callable_predicates = []
    && callables.callable_functions = []
    && callables.callable_aggregates = []
    && callables.callable_aliases = []

  let find_var_names = function
    | [] -> Some []
    | find ->
      let rec collect acc = function
        | [] -> Some (List.rev acc)
        | Find_var var :: rest -> collect (var :: acc) rest
        | (Find_pull _ | Find_pull_var _ | Find_pull_source _ | Find_pull_source_var _ | Find_aggregate _) :: _ ->
          None
      in
      collect [] find

  let relation_rows_for_plain_find attrs rows unique_rows find =
    let* find_vars = find_var_names find in
    if find_vars = attrs then
      Some (if unique_rows then rows else List.sort_uniq compare rows)
    else
      let* indexes =
        find_vars
        |> List.fold_left
             (fun indexes var ->
               match indexes with
               | None -> None
               | Some indexes ->
                 (match List.find_index (( = ) var) attrs with
                  | Some index -> Some (index :: indexes)
                  | None -> None))
             (Some [])
        |> Option.map List.rev
      in
      rows
      |> List.map (fun row -> indexes |> List.map (fun index -> List.nth row index))
      |> List.sort_uniq compare
      |> fun rows -> Some rows

  let find_spec_vars = function
    | Find_var var
    | Find_pull (var, _)
    | Find_pull_source (_, var, _) ->
      [ var ]
    | Find_pull_var (var, pattern_var)
    | Find_pull_source_var (_, var, pattern_var) ->
      [ var; pattern_var ]
    | Find_aggregate _ -> []

  let relation_rows_for_find db sources attrs rows unique_rows find =
    match relation_rows_for_plain_find attrs rows unique_rows find with
    | Some rows -> Some rows
    | None ->
      let required_vars = find |> List.concat_map find_spec_vars |> List.sort_uniq compare in
      if required_vars <> [] && List.for_all (fun var -> List.mem var attrs) required_vars then
        rows
        |> List.filter_map (fun row -> collect_find_specs db sources (List.combine attrs row) find)
        |> List.sort_uniq compare
        |> fun rows -> Some rows
      else
        None

  let dedupe_bindings_for_find bindings find =
    let vars = Query.grouping_vars_of_find find in
    match vars with
    | [] -> bindings
    | vars ->
      bindings
      |> List.filter_map (fun binding ->
        Query.collect_find_vars binding vars
        |> Option.map (fun key -> key, binding))
      |> List.sort_uniq (fun (left, _) (right, _) -> compare left right)
      |> List.map snd
  
  let q_sources_raw ?(inputs = []) db sources query =
    let callables, input_bindings, input_rules = initial_query_context db query inputs in
    let rules, where =
      match query.rules, input_rules with
      | [], [] -> [], query.where
      | _ -> query_rules_and_where query input_rules
    in
    let has_aggregates = has_aggregates query.find in
    if
      (not has_aggregates)
      && query.with_vars = []
      && query_callables_empty callables
    then
      match eval_relation_rows db sources rules input_bindings where with
      | Some (attrs, rows, unique_rows) ->
        (match relation_rows_for_find db sources attrs rows unique_rows query.find with
         | Some rows -> rows
         | None ->
           let bindings = eval_clauses ~callables db sources rules input_bindings where in
           bindings
           |> fun bindings -> dedupe_bindings_for_find bindings query.find
           |> List.filter_map (fun binding -> collect_find_specs db sources binding query.find)
           |> List.sort_uniq compare)
      | None ->
        let bindings = eval_clauses ~callables db sources rules input_bindings where in
        bindings
        |> fun bindings -> dedupe_bindings_for_find bindings query.find
        |> List.filter_map (fun binding -> collect_find_specs db sources binding query.find)
        |> List.sort_uniq compare
    else (
      let bindings = eval_clauses ~callables db sources rules input_bindings where in
      if has_aggregates then
      if query.with_vars = [] then
        aggregate_rows ~callables db sources bindings query.find
      else
        aggregate_rows_with ~callables db sources bindings query.find query.with_vars
      else if query.with_vars <> [] then
      non_aggregate_rows_with db sources bindings query.find query.with_vars
      else
      bindings
      |> fun bindings -> dedupe_bindings_for_find bindings query.find
      |> List.filter_map (fun binding -> collect_find_specs db sources binding query.find)
      |> List.sort_uniq compare)
  
  let q_with_raw ?(inputs = []) db with_vars query =
    let callables, input_bindings, input_rules = initial_query_context db query inputs in
    let rules, where = query_rules_and_where query input_rules in
    let bindings = eval_clauses ~callables db [] rules input_bindings where in
    let with_vars = query.with_vars @ with_vars |> List.sort_uniq compare in
    if has_aggregates query.find then
      aggregate_rows_with ~callables db [] bindings query.find with_vars
    else
      non_aggregate_rows_with db [] bindings query.find with_vars

  let query_context : Query.context =
    { empty_db
    ; q_sources = q_sources_raw
    ; q_with = q_with_raw
    ; parse_query_string_with_pull_context
    ; parse_query_return_string_with_pull_context
    ; parse_query_return_map_string_with_pull_context
    ; compare_value
    }
  
end
