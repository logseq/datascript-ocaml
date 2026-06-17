open Datascript_types

type context =
  { empty_db : unit -> db
  ; q_sources :
      ?inputs:query_arg list ->
      db ->
      (string * query_source) list ->
      query ->
      query_result list list
  ; q_with :
      ?inputs:query_arg list ->
      db ->
      string list ->
      query ->
      query_result list list
  ; parse_query_string_with_pull_context :
      ?default_pull_db:db ->
      ?pull_db_for_source:(string -> db) ->
      string ->
      query
  ; parse_query_return_string_with_pull_context :
      ?default_pull_db:db ->
      ?pull_db_for_source:(string -> db) ->
      string ->
      query_return * query
  ; parse_query_return_map_string_with_pull_context :
      ?default_pull_db:db ->
      ?pull_db_for_source:(string -> db) ->
      string ->
      query_return * query_return_map option * query
  ; compare_value : value -> value -> int
  }

let q_sources context ?inputs db sources query =
  context.q_sources ?inputs db sources query

let q context ?inputs db query =
  q_sources context ?inputs db [] query

let q_string context ?inputs db input =
  q context ?inputs db (context.parse_query_string_with_pull_context ~default_pull_db:db input)

let q_with context ?inputs db with_vars query =
  context.q_with ?inputs db with_vars query

let q_with_string context ?inputs db with_vars input =
  q_with context ?inputs db with_vars (context.parse_query_string_with_pull_context ~default_pull_db:db input)

let q_sources_string context ?inputs db sources input =
  let pull_db_for_source source =
    match List.assoc_opt source sources with
    | Some (Db_source source_db) -> source_db
    | Some (Relation_source _) -> context.empty_db ()
    | None when source = "$" -> db
    | None -> context.empty_db ()
  in
  let default_pull_db = pull_db_for_source "$" in
  q_sources
    context
    ?inputs
    db
    sources
    (context.parse_query_string_with_pull_context ~default_pull_db ~pull_db_for_source input)

let q_return context ?inputs db return query =
  let rows = q context ?inputs db query in
  match return with
  | Return_relation -> Query_relation rows
  | Return_collection ->
    rows
    |> List.filter_map (function
      | value :: _ -> Some value
      | [] -> None)
    |> List.sort_uniq compare
    |> fun values -> Query_collection values
  | Return_tuple -> Query_tuple (List.nth_opt rows 0)
  | Return_scalar ->
    let value =
      Option.bind
        (List.nth_opt rows 0)
        (function
          | value :: _ -> Some value
          | [] -> None)
    in
    Query_scalar value

let q_return_string context ?inputs db input =
  let return, query = context.parse_query_return_string_with_pull_context ~default_pull_db:db input in
  q_return context ?inputs db return query

let labels_of_return_map = function
  | Return_keys labels -> List.map (fun label -> Keyword label) labels
  | Return_syms labels -> List.map (fun label -> Symbol label) labels
  | Return_strs labels -> List.map (fun label -> String label) labels

let map_query_row context labels row =
  if List.length labels <> List.length row then
    invalid_arg "return map labels must match find count";
  List.combine labels row |> List.sort (fun (left, _) (right, _) -> context.compare_value left right)

let q_return_map context ?inputs db return return_map query =
  let labels = labels_of_return_map return_map in
  let rows = q context ?inputs db query in
  match return with
  | Return_relation ->
    rows
    |> List.map (map_query_row context labels)
    |> fun rows -> Query_relation_maps rows
  | Return_tuple ->
    List.nth_opt rows 0
    |> Option.map (map_query_row context labels)
    |> fun row -> Query_tuple_map row
  | Return_collection | Return_scalar ->
    invalid_arg "return maps require relation or tuple query returns"

let q_return_map_string context ?inputs db input =
  let return, return_map, query =
    context.parse_query_return_map_string_with_pull_context ~default_pull_db:db input
  in
  match return_map with
  | Some return_map -> q_return_map context ?inputs db return return_map query
  | None -> q_return context ?inputs db return query

let has_aggregates find =
  List.exists
    (function
      | Find_aggregate _ -> true
      | Find_var _ | Find_pull _ | Find_pull_var _ | Find_pull_source _ | Find_pull_source_var _ -> false)
    find

let collect_find_vars bindings find =
  let rec collect acc = function
    | [] -> Some (List.rev acc)
    | var :: rest ->
      (match List.assoc_opt var bindings with
       | Some value -> collect (value :: acc) rest
       | None -> None)
  in
  collect [] find

let group_by_key rows =
  List.fold_left
    (fun groups (key, binding) ->
      match List.assoc_opt key groups with
      | Some bindings -> (key, binding :: bindings) :: List.remove_assoc key groups
      | None -> (key, [ binding ]) :: groups)
    []
    rows

let grouping_vars_of_find find =
  find
  |> List.concat_map (function
    | Find_var var | Find_pull (var, _) | Find_pull_source (_, var, _) -> [ var ]
    | Find_pull_var (var, pattern_var) | Find_pull_source_var (_, var, pattern_var) ->
      [ var; pattern_var ]
    | Find_aggregate _ -> [])
  |> List.sort_uniq compare

let aggregate_amount_value var binding =
  match List.assoc_opt var binding with
  | Some (Result_value (Int amount)) when amount >= 0 -> amount
  | Some (Result_value (Int _)) -> invalid_arg "aggregate amount must be non-negative"
  | Some _ -> invalid_arg "aggregate amount must be an integer"
  | None -> invalid_arg ("aggregate amount variable is unbound: " ^ var)

let resolve_dynamic_aggregate aggregate group_bindings =
  let binding =
    match group_bindings with
    | first :: _ -> first
    | [] -> []
  in
  match aggregate with
  | MinNVar var -> MinN (aggregate_amount_value var binding)
  | MaxNVar var -> MaxN (aggregate_amount_value var binding)
  | RandNVar var -> RandN (aggregate_amount_value var binding)
  | SampleVar var -> Sample (aggregate_amount_value var binding)
  | aggregate -> aggregate

let aggregate_param_vars = function
  | MinNVar var | MaxNVar var | RandNVar var | SampleVar var -> [ var ]
  | Count
  | CountDistinct
  | Distinct
  | Sum
  | Avg
  | Median
  | Variance
  | Stddev
  | Min
  | Max
  | MinN _
  | MaxN _
  | Rand
  | RandN _
  | Sample _
  | CustomVar _
  | Custom _ -> []

let aggregate_callable_vars = function
  | CustomVar var -> [ var ]
  | Count
  | CountDistinct
  | Distinct
  | Sum
  | Avg
  | Median
  | Variance
  | Stddev
  | Min
  | Max
  | MinN _
  | MaxN _
  | Rand
  | RandN _
  | Sample _
  | MinNVar _
  | MaxNVar _
  | RandNVar _
  | SampleVar _
  | Custom _ -> []

let split_aggregate_terms terms =
  match List.rev terms with
  | [] -> invalid_arg "aggregate requires at least one argument"
  | value_term :: reversed_extra_terms -> List.rev reversed_extra_terms, value_term

let aggregate_input_values aggregate extra_args values =
  match aggregate with
  | Custom _ -> extra_args @ values
  | Count
  | CountDistinct
  | Distinct
  | Sum
  | Avg
  | Median
  | Variance
  | Stddev
  | Min
  | Max
  | MinN _
  | MaxN _
  | Rand
  | RandN _
  | Sample _
  | MinNVar _
  | MaxNVar _
  | RandNVar _
  | SampleVar _
  | CustomVar _ ->
    values

let query_input_var_label var =
  if String.length var > 0 && (var.[0] = '?' || var.[0] = '$') then var else "?" ^ var

let rec query_input_binding_string = function
  | Bind_scalar var -> query_input_var_label var
  | Bind_ignore -> "_"
  | Bind_collection binding -> "[" ^ query_input_binding_string binding ^ " ...]"
  | Bind_tuple bindings -> "[" ^ String.concat " " (List.map query_input_binding_string bindings) ^ "]"

let query_input_decl_binding_string = function
  | Input_collection_decl var -> "[" ^ query_input_var_label var ^ " ...]"
  | Input_tuple_decl vars -> "[" ^ String.concat " " (List.map query_input_var_label vars) ^ "]"
  | Input_relation_decl vars -> "[[" ^ String.concat " " (List.map query_input_var_label vars) ^ "]]"
  | Input_nested_collection_decl binding -> "[" ^ query_input_binding_string binding ^ " ...]"
  | Input_nested_tuple_decl bindings -> "[" ^ String.concat " " (List.map query_input_binding_string bindings) ^ "]"
  | Input_nested_relation_decl bindings ->
    "[[" ^ String.concat " " (List.map query_input_binding_string bindings) ^ "]]"
  | Input_scalar_decl var -> query_input_var_label var
  | Input_collection_ignore_decl -> "[_ ...]"
  | Input_ignore_decl -> "_"
  | Input_rules_decl -> "%"
  | Input_source_decl source -> source
  | _ -> "[...]"

let query_input_binding_label = function
  | Input_scalar_decl var
  | Input_collection_decl var -> query_input_var_label var
  | Input_collection_ignore_decl
  | Input_ignore_decl -> "_"
  | Input_rules_decl -> "%"
  | Input_source_decl source -> source
  | Input_nested_collection_decl _
  | Input_tuple_decl _
  | Input_relation_decl _
  | Input_nested_tuple_decl _
  | Input_nested_relation_decl _ -> "[...]"
  | Input_scalar (var, _)
  | Input_entity_ref (var, _)
  | Input_collection (var, _)
  | Input_predicate (var, _)
  | Input_function (var, _)
  | Input_aggregate (var, _) -> query_input_var_label var
  | Input_rules _ -> "%"
  | Input_collection_ignore _
  | Input_ignore -> "_"
  | Input_nested_collection _
  | Input_tuple _
  | Input_relation _
  | Input_nested_tuple _
  | Input_nested_relation _ -> "[...]"

let query_input_consumes_argument ~consume_rules = function
  | Input_rules_decl -> consume_rules
  | Input_scalar_decl _
  | Input_collection_decl _
  | Input_collection_ignore_decl
  | Input_ignore_decl
  | Input_nested_collection_decl _
  | Input_tuple_decl _
  | Input_relation_decl _
  | Input_nested_tuple_decl _
  | Input_nested_relation_decl _ -> true
  | Input_source_decl _
  | Input_scalar _
  | Input_entity_ref _
  | Input_collection _
  | Input_collection_ignore _
  | Input_nested_collection _
  | Input_tuple _
  | Input_relation _
  | Input_nested_tuple _
  | Input_nested_relation _
  | Input_predicate _
  | Input_function _
  | Input_aggregate _
  | Input_rules _
  | Input_ignore -> false
