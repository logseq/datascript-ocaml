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

type query_callables =
  { callable_predicates : (string * (query_result list -> bool)) list
  ; callable_functions : (string * (query_result list -> query_result list option)) list
  ; callable_aggregates : (string * (query_result list -> query_result)) list
  ; callable_aliases : (string * string) list
  }

type result_resolution_context =
  { validate_entity_id : int -> entity_id
  ; resolve_query_value : value -> value option
  ; lookup_ref_entity_id : attr -> value -> entity_id option
  }

type match_context =
  { result_resolution_context : result_resolution_context
  ; source_db : db
  ; ident_entity_id : string -> entity_id option
  ; unresolved_lookup_ref_message : attr -> value -> string
  ; value_equal : value -> value -> bool
  ; coerce_tuple_lookup_value : attr -> value -> value
  }

type source_context =
  { match_context : match_context
  ; pattern_datoms : db -> query_term -> query_term -> query_term -> query_term option -> datom Seq.t
  ; match_data_pattern :
      db ->
      (string * query_result) list ->
      query_term ->
      query_term ->
      query_term ->
      datom ->
      (string * query_result) list option
  ; match_data_pattern_tx :
      db ->
      (string * query_result) list ->
      query_term ->
      query_term ->
      query_term ->
      query_term ->
      datom ->
      (string * query_result) list option
  ; match_data_pattern_tx_op :
      db ->
      (string * query_result) list ->
      query_term ->
      query_term ->
      query_term ->
      query_term ->
      query_term ->
      datom ->
      (string * query_result) list option
  }

type input_context =
  { resolve_query_input_result : query_result -> query_result option
  ; bind_var :
      string ->
      query_result ->
      (string * query_result) list ->
      (string * query_result) list option
  ; entity_id_of_ref : entity_ref -> entity_id option
  }

let empty_query_callables =
  { callable_predicates = []
  ; callable_functions = []
  ; callable_aggregates = []
  ; callable_aliases = []
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

let return_map_label_count = function
  | Return_keys labels | Return_syms labels | Return_strs labels -> List.length labels

let return_map_name = function
  | Return_keys _ -> "keys"
  | Return_syms _ -> "syms"
  | Return_strs _ -> "strs"

let validate_query_return_map return return_map query =
  match return_map with
  | None -> None
  | Some return_map ->
    (match return with
     | Return_collection ->
       invalid_arg (":" ^ return_map_name return_map ^ " does not work with collection :find")
     | Return_scalar ->
       invalid_arg (":" ^ return_map_name return_map ^ " does not work with single-scalar :find")
     | Return_relation | Return_tuple ->
       if return_map_label_count return_map <> List.length query.find then
         invalid_arg ("Count of :" ^ return_map_name return_map ^ " must match count of :find");
       Some return_map)

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

let rec resolve_callable_name callables name =
  match List.assoc_opt name callables.callable_aliases with
  | Some target when target <> name -> resolve_callable_name callables target
  | Some _ | None -> name

let callable_predicate callables name =
  List.assoc_opt (resolve_callable_name callables name) callables.callable_predicates

let callable_function callables name =
  List.assoc_opt (resolve_callable_name callables name) callables.callable_functions

let callable_aggregate callables name =
  List.assoc_opt (resolve_callable_name callables name) callables.callable_aggregates

let has_callable callables name =
  Option.is_some (callable_predicate callables name)
  || Option.is_some (callable_function callables name)
  || Option.is_some (callable_aggregate callables name)

let alias_callable callables alias target =
  let target = resolve_callable_name callables target in
  { callables with callable_aliases = (alias, target) :: List.remove_assoc alias callables.callable_aliases }

let resolve_callable_aggregate callables aggregate =
  match aggregate with
  | CustomVar var ->
    (match callable_aggregate callables var with
     | Some f -> Custom f
     | None -> invalid_arg ("unknown aggregate input: " ^ var))
  | aggregate -> aggregate

let result_of_datom_e d = Result_entity d.e

let result_of_datom_a d = Result_attr d.a

let result_of_datom_v d = Result_value d.v

let result_of_datom_tx d = Result_entity d.tx

let result_of_datom_op d =
  Result_value (Keyword (if d.added then "db/add" else "db/retract"))

let result_of_ref = function
  | Result_value (Ref eid) -> Result_entity eid
  | result -> result

let entity_id_of_resolved_query_result ~validate_entity_id = function
  | Some (Result_entity entity_id) -> Some entity_id
  | Some (Result_value (Int entity_id)) -> Some (validate_entity_id entity_id)
  | Some (Result_value (Ref entity_id)) -> Some entity_id
  | _ -> None

let resolved_query_result context = function
  | Result_value value -> Option.map (fun value -> result_of_ref (Result_value value)) (context.resolve_query_value value)
  | Result_db _ -> None
  | result -> Some result

let malformed_lookup_ref_message value =
  "Lookup ref should contain 2 elements: " ^ Built_ins.print_query_value ~readably:true value

let lookup_ref_entity_id_of_value context = function
  | List [ Keyword attr; value ] | List [ String attr; value ]
  | Vector [ Keyword attr; value ] | Vector [ String attr; value ] ->
    context.lookup_ref_entity_id attr value
  | _ -> None

let lookup_ref_entity_id_of_entity_value context = function
  | List [ Keyword attr; value ] | List [ String attr; value ]
  | Vector [ Keyword attr; value ] | Vector [ String attr; value ] ->
    context.lookup_ref_entity_id attr value
  | (List ((Keyword _ | String _) :: _) | Vector ((Keyword _ | String _) :: _)) as value ->
    invalid_arg (malformed_lookup_ref_message value)
  | _ -> None

let query_result_entity_id context result =
  match result with
  | Result_value value ->
    (match lookup_ref_entity_id_of_entity_value context value with
     | Some entity_id -> Some entity_id
     | None ->
       entity_id_of_resolved_query_result
         ~validate_entity_id:context.validate_entity_id
         (resolved_query_result context result))
  | _ ->
    entity_id_of_resolved_query_result
      ~validate_entity_id:context.validate_entity_id
      (resolved_query_result context result)

let query_results_equivalent context left right =
  match left, right with
  | Result_db left_db, Result_db right_db -> left_db == right_db
  | Result_db _, _ | _, Result_db _ -> false
  | Result_attr left, Result_value (Keyword right)
  | Result_value (Keyword right), Result_attr left ->
    left = right
  | _ ->
    left = right
    ||
    let left_resolved = resolved_query_result context left in
    let right_resolved = resolved_query_result context right in
    let is_entity_candidate = function
      | Some (Result_entity _) | Some (Result_value (Int _ | Ref _)) -> true
      | _ -> false
    in
    if is_entity_candidate left_resolved || is_entity_candidate right_resolved then
      match query_result_entity_id context left, query_result_entity_id context right with
      | Some left_id, Some right_id -> left_id = right_id
      | _ -> false
    else
      match left_resolved, right_resolved with
      | Some left, Some right -> left = right
      | _ -> false

let bind_var context name value bindings =
  match List.assoc_opt name bindings with
  | Some bound when query_results_equivalent context bound value -> Some bindings
  | Some _ -> None
  | None -> Some ((name, value) :: bindings)

let result_matches_entity context entity_id result =
  match query_result_entity_id context result with
  | Some actual -> actual = entity_id
  | None -> false

let match_query_term context term value bindings =
  let result_context = context.result_resolution_context in
  match term with
  | QWildcard -> Some bindings
  | QEntity eid when result_matches_entity result_context eid value -> Some bindings
  | QIdent ident ->
    (match context.ident_entity_id ident with
     | Some entity_id when result_matches_entity result_context entity_id value -> Some bindings
     | _ -> None)
  | QLookupRef (attr, lookup_value) ->
    (match result_context.lookup_ref_entity_id attr lookup_value with
     | Some entity_id when result_matches_entity result_context entity_id value -> Some bindings
     | Some _ -> None
     | None -> invalid_arg (context.unresolved_lookup_ref_message attr lookup_value))
  | QAttr attr when value = Result_attr attr -> Some bindings
  | QValue expected ->
    (match result_context.resolve_query_value expected, value with
     | Some expected, Result_value actual when context.value_equal actual expected -> Some bindings
     | Some (Ref expected), Result_entity actual when actual = expected -> Some bindings
     | Some (Keyword ident), _ ->
       (match context.ident_entity_id ident with
        | Some entity_id when result_matches_entity result_context entity_id value -> Some bindings
        | _ -> None)
     | _ -> None)
  | QVar name -> bind_var result_context name (result_of_ref value) bindings
  | _ -> None

let match_value_term_for_datom_attr context bindings v_term datom =
  match v_term with
  | QValue value ->
    let value = context.coerce_tuple_lookup_value datom.a value in
    match_query_term context (QValue value) (result_of_datom_v datom) bindings
  | _ -> match_query_term context v_term (result_of_datom_v datom) bindings

let match_pattern_clause context bindings e_term a_term v_term datom =
  let ( let* ) = Option.bind in
  let* bindings = match_query_term context e_term (result_of_datom_e datom) bindings in
  let* bindings = match_query_term context a_term (result_of_datom_a datom) bindings in
  match_value_term_for_datom_attr context bindings v_term datom

let match_pattern_tx_clause context bindings e_term a_term v_term tx_term datom =
  let ( let* ) = Option.bind in
  let* bindings = match_pattern_clause context bindings e_term a_term v_term datom in
  match_query_term context tx_term (result_of_datom_tx datom) bindings

let match_reverse_pattern_clause context bindings e_term reverse_attr v_term datom =
  match datom.v with
  | Ref target ->
    let ( let* ) = Option.bind in
    let* bindings = match_query_term context e_term (Result_entity target) bindings in
    let* bindings = match_query_term context (QAttr reverse_attr) (Result_attr reverse_attr) bindings in
    match_query_term context v_term (Result_entity datom.e) bindings
  | _ -> None

let eval_query_term context bindings = function
  | QVar name -> List.assoc_opt name bindings
  | QEntity eid -> Some (Result_entity eid)
  | QIdent ident -> Option.map (fun entity_id -> Result_entity entity_id) (context.ident_entity_id ident)
  | QLookupRef (attr, value) ->
    (match context.result_resolution_context.lookup_ref_entity_id attr value with
     | Some entity_id -> Some (Result_entity entity_id)
     | None -> invalid_arg (context.unresolved_lookup_ref_message attr value))
  | QAttr attr -> Some (Result_attr attr)
  | QValue value ->
    Option.map (fun value -> Result_value value) (context.result_resolution_context.resolve_query_value value)
  | QSource "$" -> Some (Result_db context.source_db)
  | QSource source -> invalid_arg ("source term requires query source context: " ^ source)
  | QWildcard -> None

let collect_query_terms context bindings terms =
  let rec collect acc = function
    | [] -> Some (List.rev acc)
    | term :: rest ->
      (match eval_query_term context bindings term with
       | Some value -> collect (value :: acc) rest
       | None -> None)
  in
  collect [] terms

let collect_query_terms_exn context bindings terms =
  match collect_query_terms context bindings terms with
  | Some values -> values
  | None -> invalid_arg "insufficient bindings"

let query_term_entity_id context bindings term =
  Option.bind (eval_query_term context bindings term) (query_result_entity_id context.result_resolution_context)

let source default_db sources name =
  match List.assoc_opt name sources with
  | Some source -> source
  | None ->
    if name = "$" then Db_source default_db else invalid_arg ("unknown query source: " ^ name)

let sources_with_root_default db sources =
  if List.mem_assoc "$" sources then sources else ("$", Db_source db) :: sources

let source_db default_db sources name =
  match source default_db sources name with
  | Db_source db -> db
  | Relation_source _ -> invalid_arg ("query source is not a database: " ^ name)

let query_source_db = function
  | Db_source db -> db
  | Relation_source _ -> invalid_arg "query source is not a database"

let match_relation_row context bindings terms row =
  let rec match_terms binding terms row =
    match binding, terms, row with
    | None, _, _ -> None
    | Some binding, [], _ -> Some binding
    | Some _, _ :: _, [] -> invalid_arg "source relation row arity mismatch"
    | Some binding, term :: terms, value :: row ->
      match_terms (match_query_term context.match_context term value binding) terms row
  in
  match_terms (Some bindings) terms row

let bound_pattern_term bindings = function
  | QVar name as term ->
    (match List.assoc_opt name bindings with
     | Some (Result_entity entity_id) -> QEntity entity_id
     | Some (Result_value value) -> QValue value
     | Some (Result_attr attr) -> QAttr attr
     | Some (Result_db _ | Result_pull _) | None -> term)
  | term -> term

let bound_attr_pattern_term bindings term =
  match bound_pattern_term bindings term with
  | QValue (Keyword attr | String attr | Symbol attr) -> QAttr attr
  | term -> term

let match_query_source_pattern context _default_db source bindings terms =
  match source with
  | Db_source source_db ->
    (match terms with
     | [ e_term; a_term; v_term ] ->
       let index_e_term = bound_pattern_term bindings e_term in
       let index_a_term = bound_attr_pattern_term bindings a_term in
       let index_v_term = bound_pattern_term bindings v_term in
       context.pattern_datoms source_db index_e_term index_a_term index_v_term None
       |> Seq.filter_map (fun datom -> context.match_data_pattern source_db bindings e_term a_term v_term datom)
       |> List.of_seq
     | [ e_term; a_term; v_term; tx_term ] ->
       let index_e_term = bound_pattern_term bindings e_term in
       let index_a_term = bound_attr_pattern_term bindings a_term in
       let index_v_term = bound_pattern_term bindings v_term in
       let index_tx_term = bound_pattern_term bindings tx_term in
       context.pattern_datoms source_db index_e_term index_a_term index_v_term (Some index_tx_term)
       |> Seq.filter_map (fun datom ->
         context.match_data_pattern_tx source_db bindings e_term a_term v_term tx_term datom)
       |> List.of_seq
     | [ e_term; a_term; v_term; tx_term; op_term ] ->
       let index_e_term = bound_pattern_term bindings e_term in
       let index_a_term = bound_attr_pattern_term bindings a_term in
       let index_v_term = bound_pattern_term bindings v_term in
       let index_tx_term = bound_pattern_term bindings tx_term in
       context.pattern_datoms source_db index_e_term index_a_term index_v_term (Some index_tx_term)
       |> Seq.filter_map (fun datom ->
         context.match_data_pattern_tx_op source_db bindings e_term a_term v_term tx_term op_term datom)
       |> List.of_seq
     | _ -> invalid_arg "database source patterns expect 3, 4, or 5 terms")
  | Relation_source rows ->
    rows
    |> List.filter_map (fun row -> match_relation_row context bindings terms row)

let match_source_pattern context default_db sources source_name bindings terms =
  match_query_source_pattern context default_db (source default_db sources source_name) bindings terms

let match_relation_source_pattern context default_db sources source_name bindings terms =
  let attr_term_of_short_pattern = function
    | QValue (Keyword attr | String attr | Symbol attr) -> QAttr attr
    | term -> term
  in
  match source default_db sources source_name with
  | Relation_source rows ->
    rows
    |> List.filter_map (fun row -> match_relation_row context bindings terms row)
  | Db_source _ ->
    (match terms with
     | [ e_term ] ->
       match_source_pattern context default_db sources source_name bindings [ e_term; QWildcard; QWildcard ]
     | [ e_term; a_term ] ->
       match_source_pattern
         context
         default_db
         sources
         source_name
         bindings
         [ e_term; attr_term_of_short_pattern a_term; QWildcard ]
     | _ -> invalid_arg ("query source is not a relation: " ^ source_name))

let eval_query_term_with_sources context default_db sources bindings = function
  | QSource source -> Some (Result_db (source_db default_db sources source))
  | term -> eval_query_term context bindings term

let collect_dynamic_query_terms_exn context default_db sources bindings terms =
  let rec collect acc = function
    | [] -> List.rev acc
    | term :: rest ->
      (match eval_query_term_with_sources context default_db sources bindings term with
       | Some value -> collect (value :: acc) rest
       | None -> invalid_arg "unbound query variable")
  in
  collect [] terms

let aggregate_extra_args context default_db sources group_bindings terms =
  let extra_terms, _ = split_aggregate_terms terms in
  let binding =
    match group_bindings with
    | first :: _ -> first
    | [] -> []
  in
  let rec collect acc = function
    | [] -> List.rev acc
    | term :: rest ->
      (match eval_query_term_with_sources context default_db sources binding term with
       | Some value -> collect (value :: acc) rest
       | None -> invalid_arg "insufficient aggregate argument bindings")
  in
  collect [] extra_terms

let aggregate_values context default_db sources group_bindings terms =
  let _, value_term = split_aggregate_terms terms in
  List.filter_map
    (fun binding -> eval_query_term_with_sources context default_db sources binding value_term)
    group_bindings

let query_callables_of_inputs inputs =
  inputs
  |> List.fold_left
       (fun callables -> function
         | Input_predicate (var, predicate) ->
           { callables with callable_predicates = (var, predicate) :: callables.callable_predicates }
         | Input_function (var, f) ->
           { callables with callable_functions = (var, f) :: callables.callable_functions }
         | Input_aggregate (var, f) ->
           { callables with callable_aggregates = (var, f) :: callables.callable_aggregates }
         | _ -> callables)
       empty_query_callables

let query_rules_of_inputs inputs =
  inputs
  |> List.concat_map (function
    | Input_rules rules -> rules
    | _ -> [])

let matching_rules rules name arity =
  List.filter (fun rule -> rule.rule_name = name && List.length rule.rule_params = arity) rules

let matching_rules_exn rules name arity =
  match matching_rules rules name arity with
  | [] -> invalid_arg ("unknown rule: " ^ name)
  | rules -> rules

let project_binding vars binding =
  List.filter (fun (var, _) -> List.mem var vars) binding

let rule_invocation_callables callables outer_binding rule terms =
  List.fold_left2
    (fun callables param term ->
      match term with
      | QVar var when List.assoc_opt var outer_binding = None && has_callable callables var ->
        alias_callable callables param var
      | _ -> callables)
    callables
    rule.rule_params
    terms

let rec vars_of_query_term = function
  | QVar name -> [ name ]
  | QEntity _ | QIdent _ | QLookupRef _ | QAttr _ | QValue _ | QSource _ | QWildcard -> []

and vars_of_query_terms terms =
  terms |> List.concat_map vars_of_query_term |> List.sort_uniq compare

and vars_of_clause = function
  | Pattern (e, a, v) -> vars_of_query_terms [ e; a; v ]
  | PatternTx (e, a, v, tx) -> vars_of_query_terms [ e; a; v; tx ]
  | PatternTxOp (e, a, v, tx, op) -> vars_of_query_terms [ e; a; v; tx; op ]
  | SourcePattern (_, e, a, v) -> vars_of_query_terms [ e; a; v ]
  | SourcePatternTx (_, e, a, v, tx) -> vars_of_query_terms [ e; a; v; tx ]
  | SourcePatternTxOp (_, e, a, v, tx, op) -> vars_of_query_terms [ e; a; v; tx; op ]
  | SourceRelationPattern (_, terms) -> vars_of_query_terms terms
  | Missing (e, _) | SourceMissing (_, e, _) -> vars_of_query_term e
  | GetElse (e, _, _, output) | SourceGetElse (_, e, _, _, output) -> output :: vars_of_query_term e
  | GetSome (e, _, attr, value) | SourceGetSome (_, e, _, attr, value) -> attr :: value :: vars_of_query_term e
  | GetValue (m, key, output) -> output :: vars_of_query_terms [ m; key ]
  | GetDefaultValue (m, key, default, output) -> output :: vars_of_query_terms [ m; key; default ]
  | CountValue (term, output) -> output :: vars_of_query_term term
  | EmptyValue term | NotEmptyValue term -> vars_of_query_term term
  | ContainsValue (collection, key) -> vars_of_query_terms [ collection; key ]
  | ValuePredicate (_, term) -> vars_of_query_term term
  | NumericPredicate (_, term) -> vars_of_query_term term
  | ComparisonPredicate (_, left, right) -> vars_of_query_terms [ left; right ]
  | ComparisonPredicateN (_, terms) -> vars_of_query_terms terms
  | EqualityPredicate (_, terms) -> vars_of_query_terms terms
  | ArithmeticValue (_, terms, output) -> output :: vars_of_query_terms terms
  | CompareValue (left, right, output) -> output :: vars_of_query_terms [ left; right ]
  | ExtremumValue (_, terms, output) -> output :: vars_of_query_terms terms
  | BooleanPredicate (_, term) -> vars_of_query_term term
  | BooleanNotPredicate term -> vars_of_query_term term
  | BooleanNotValue (term, output) -> output :: vars_of_query_term term
  | IdentityValue (term, output) -> output :: vars_of_query_term term
  | BooleanAndPredicate terms | BooleanOrPredicate terms -> vars_of_query_terms terms
  | BooleanAndValue (terms, output) | BooleanOrValue (terms, output) -> output :: vars_of_query_terms terms
  | RandomValue output -> [ output ]
  | RandomIntValue (bound, output) -> output :: vars_of_query_term bound
  | DifferPredicate terms -> vars_of_query_terms terms
  | IdenticalPredicate (left, right) -> vars_of_query_terms [ left; right ]
  | TypeValue (term, output) -> output :: vars_of_query_term term
  | MetaValue (term, output) -> output :: vars_of_query_term term
  | NameValue (term, output) | NamespaceValue (term, output) | KeywordFromName (term, output) ->
    output :: vars_of_query_term term
  | KeywordFromNamespaceName (namespace_term, name_term, output) ->
    output :: vars_of_query_terms [ namespace_term; name_term ]
  | StringIncludesValue (left, right)
  | StringStartsWithValue (left, right)
  | StringEndsWithValue (left, right) ->
    vars_of_query_terms [ left; right ]
  | StringLowerCaseValue (term, output)
  | StringUpperCaseValue (term, output)
  | StringCapitalizeValue (term, output)
  | StringReverseValue (term, output) ->
    output :: vars_of_query_term term
  | StringTrimValue (term, output)
  | StringTrimLeftValue (term, output)
  | StringTrimRightValue (term, output)
  | StringTrimNewlineValue (term, output) ->
    output :: vars_of_query_term term
  | StringIndexOfValue (value, needle, output) | StringLastIndexOfValue (value, needle, output) ->
    output :: vars_of_query_terms [ value; needle ]
  | StringSubstringValue (value, start, end_, output) ->
    output :: vars_of_query_terms (value :: start :: Option.to_list end_)
  | StringBuildValue (terms, output) -> output :: vars_of_query_terms terms
  | PrintStringValue (terms, output)
  | PrintLineStringValue (terms, output)
  | PrStringValue (terms, output)
  | PrnStringValue (terms, output) ->
    output :: vars_of_query_terms terms
  | StringJoinPlainValue (collection, output) -> output :: vars_of_query_term collection
  | StringJoinValue (separator, collection, output) -> output :: vars_of_query_terms [ separator; collection ]
  | StringReplaceValue (value, pattern, replacement, output)
  | StringReplaceFirstValue (value, pattern, replacement, output) ->
    output :: vars_of_query_terms [ value; pattern; replacement ]
  | StringEscapeValue (value, replacements, output) -> output :: vars_of_query_terms [ value; replacements ]
  | RePatternValue (pattern, output) -> output :: vars_of_query_term pattern
  | ReFindValue (pattern, value, output)
  | ReMatchesValue (pattern, value, output)
  | ReSeqValue (pattern, value, output) ->
    output :: vars_of_query_terms [ pattern; value ]
  | ReFindPredicate (pattern, value)
  | ReMatchesPredicate (pattern, value) ->
    vars_of_query_terms [ pattern; value ]
  | StringBlankValue term -> vars_of_query_term term
  | StringSplitValue (value, separator, output) -> output :: vars_of_query_terms [ value; separator ]
  | StringSplitLimitValue (value, separator, limit, output) ->
    output :: vars_of_query_terms [ value; separator; limit ]
  | StringSplitLinesValue (value, output) -> output :: vars_of_query_term value
  | Ground (_, output) | GroundCollection (_, output) -> List.filter (( <> ) "_") [ output ]
  | GroundTuple (_, outputs) | GroundRelation (_, outputs) -> List.filter (( <> ) "_") outputs
  | GroundTerm (term, output) | GroundTermCollection (term, output) ->
    List.filter (( <> ) "_") [ output ] @ vars_of_query_term term
  | GroundTermTuple (term, outputs) | GroundTermRelation (term, outputs) ->
    List.filter (( <> ) "_") outputs @ vars_of_query_term term
  | VectorValue (terms, output) -> output :: vars_of_query_terms terms
  | ListValue (terms, output) -> output :: vars_of_query_terms terms
  | SetValue (terms, output) -> output :: vars_of_query_terms terms
  | HashMapValue (terms, output) | ArrayMapValue (terms, output) -> output :: vars_of_query_terms terms
  | RangeEndValue (end_term, output) -> output :: vars_of_query_term end_term
  | RangeValue (start_term, end_term, output) -> output :: vars_of_query_terms [ start_term; end_term ]
  | RangeStepValue (start_term, end_term, step_term, output) ->
    output :: vars_of_query_terms [ start_term; end_term; step_term ]
  | TupleFunction (terms, output) -> output :: vars_of_query_terms terms
  | UntupleFunction (term, outputs) -> vars_of_query_term term @ List.filter (( <> ) "_") outputs
  | Predicate (_, terms, _) -> vars_of_query_terms terms
  | Function (_, terms, outputs, _) -> outputs @ vars_of_query_terms terms
  | DynamicPredicate (_, terms) -> vars_of_query_terms terms
  | DynamicFunction (_, terms, outputs) -> outputs @ vars_of_query_terms terms
  | DynamicFunctionCollection (_, terms, output) -> output :: vars_of_query_terms terms
  | DynamicFunctionRelation (_, terms, outputs) -> List.filter (( <> ) "_") outputs @ vars_of_query_terms terms
  | SourceClause (_, clause) -> vars_of_clause clause
  | Not clauses | SourceNot (_, clauses) ->
    clauses |> List.concat_map vars_of_clause |> List.sort_uniq compare
  | NotJoin (vars, clauses) | SourceNotJoin (_, vars, clauses) ->
    vars @ (clauses |> List.concat_map vars_of_clause) |> List.sort_uniq compare
  | Or branches
  | SourceOr (_, branches)
  | OrJoin (_, branches)
  | SourceOrJoin (_, _, branches) ->
    branches |> List.concat_map (List.concat_map vars_of_clause) |> List.sort_uniq compare
  | OrJoinRequired (required_vars, vars, branches) | SourceOrJoinRequired (_, required_vars, vars, branches) ->
    required_vars @ vars @ (branches |> List.concat_map (List.concat_map vars_of_clause))
    |> List.sort_uniq compare
  | Rule (_, terms) | SourceRule (_, _, terms) -> vars_of_query_terms terms

let named_source source = [ source ]

let sources_of_query_term = function
  | QSource source -> [ source ]
  | QEntity _ | QIdent _ | QLookupRef _ | QVar _ | QAttr _ | QValue _ | QWildcard -> []

let sources_of_query_terms terms =
  List.concat_map sources_of_query_term terms

let sources_of_optional_query_term = function
  | Some term -> sources_of_query_term term
  | None -> []

let rec sources_of_clause = function
  | Pattern (e, a, v) -> sources_of_query_terms [ e; a; v ]
  | PatternTx (e, a, v, tx) -> sources_of_query_terms [ e; a; v; tx ]
  | PatternTxOp (e, a, v, tx, op) -> sources_of_query_terms [ e; a; v; tx; op ]
  | SourcePattern (source, e, a, v) -> named_source source @ sources_of_query_terms [ e; a; v ]
  | SourcePatternTx (source, e, a, v, tx) ->
    named_source source @ sources_of_query_terms [ e; a; v; tx ]
  | SourcePatternTxOp (source, e, a, v, tx, op) ->
    named_source source @ sources_of_query_terms [ e; a; v; tx; op ]
  | SourceRelationPattern (source, terms) -> named_source source @ sources_of_query_terms terms
  | Missing (entity, _) -> sources_of_query_term entity
  | SourceMissing (source, entity, _) -> named_source source @ sources_of_query_term entity
  | GetElse (entity, _, _, _) -> sources_of_query_term entity
  | SourceGetElse (source, entity, _, _, _) -> named_source source @ sources_of_query_term entity
  | GetSome (entity, _, _, _) -> sources_of_query_term entity
  | SourceGetSome (source, entity, _, _, _) -> named_source source @ sources_of_query_term entity
  | GetValue (map, key, _) -> sources_of_query_terms [ map; key ]
  | GetDefaultValue (map, key, default, _) -> sources_of_query_terms [ map; key; default ]
  | CountValue (term, _)
  | EmptyValue term
  | NotEmptyValue term
  | ValuePredicate (_, term)
  | NumericPredicate (_, term)
  | BooleanPredicate (_, term)
  | BooleanNotPredicate term
  | BooleanNotValue (term, _)
  | IdentityValue (term, _)
  | RandomIntValue (term, _)
  | TypeValue (term, _)
  | MetaValue (term, _)
  | NameValue (term, _)
  | NamespaceValue (term, _)
  | KeywordFromName (term, _)
  | StringLowerCaseValue (term, _)
  | StringUpperCaseValue (term, _)
  | StringCapitalizeValue (term, _)
  | StringReverseValue (term, _)
  | StringTrimValue (term, _)
  | StringTrimLeftValue (term, _)
  | StringTrimRightValue (term, _)
  | StringTrimNewlineValue (term, _)
  | StringJoinPlainValue (term, _)
  | RePatternValue (term, _)
  | StringBlankValue term
  | StringSplitLinesValue (term, _)
  | GroundTerm (term, _)
  | GroundTermCollection (term, _)
  | GroundTermTuple (term, _)
  | GroundTermRelation (term, _)
  | RangeEndValue (term, _)
  | UntupleFunction (term, _) ->
    sources_of_query_term term
  | ContainsValue (collection, key)
  | ComparisonPredicate (_, collection, key)
  | CompareValue (collection, key, _)
  | IdenticalPredicate (collection, key)
  | KeywordFromNamespaceName (collection, key, _)
  | StringIncludesValue (collection, key)
  | StringStartsWithValue (collection, key)
  | StringEndsWithValue (collection, key)
  | StringIndexOfValue (collection, key, _)
  | StringLastIndexOfValue (collection, key, _)
  | StringJoinValue (collection, key, _)
  | StringEscapeValue (collection, key, _)
  | ReFindValue (collection, key, _)
  | ReMatchesValue (collection, key, _)
  | ReFindPredicate (collection, key)
  | ReMatchesPredicate (collection, key)
  | ReSeqValue (collection, key, _)
  | StringSplitValue (collection, key, _)
  | RangeValue (collection, key, _) ->
    sources_of_query_terms [ collection; key ]
  | StringSubstringValue (value, start, end_, _) ->
    sources_of_query_terms [ value; start ] @ sources_of_optional_query_term end_
  | StringReplaceValue (value, pattern, replacement, _)
  | StringReplaceFirstValue (value, pattern, replacement, _)
  | StringSplitLimitValue (value, pattern, replacement, _)
  | RangeStepValue (value, pattern, replacement, _) ->
    sources_of_query_terms [ value; pattern; replacement ]
  | ComparisonPredicateN (_, terms)
  | EqualityPredicate (_, terms)
  | ArithmeticValue (_, terms, _)
  | ExtremumValue (_, terms, _)
  | BooleanAndPredicate terms
  | BooleanAndValue (terms, _)
  | BooleanOrPredicate terms
  | BooleanOrValue (terms, _)
  | DifferPredicate terms
  | StringBuildValue (terms, _)
  | PrintStringValue (terms, _)
  | PrintLineStringValue (terms, _)
  | PrStringValue (terms, _)
  | PrnStringValue (terms, _)
  | VectorValue (terms, _)
  | ListValue (terms, _)
  | SetValue (terms, _)
  | HashMapValue (terms, _)
  | ArrayMapValue (terms, _)
  | TupleFunction (terms, _)
  | Predicate (_, terms, _)
  | Function (_, terms, _, _)
  | DynamicPredicate (_, terms)
  | DynamicFunction (_, terms, _)
  | DynamicFunctionCollection (_, terms, _)
  | DynamicFunctionRelation (_, terms, _)
  | Rule (_, terms)
  | SourceRule (_, _, terms) ->
    sources_of_query_terms terms
  | SourceClause (source, clause) -> named_source source @ sources_of_clause clause
  | SourceNot (source, clauses) | SourceNotJoin (source, _, clauses) ->
    named_source source @ List.concat_map sources_of_clause clauses
  | SourceOr (source, branches)
  | SourceOrJoin (source, _, branches)
  | SourceOrJoinRequired (source, _, _, branches) ->
    named_source source @ List.concat_map (List.concat_map sources_of_clause) branches
  | Not clauses | NotJoin (_, clauses) -> List.concat_map sources_of_clause clauses
  | Or branches | OrJoin (_, branches) | OrJoinRequired (_, _, branches) ->
    List.concat_map (List.concat_map sources_of_clause) branches
  | RandomValue _
  | Ground _
  | GroundCollection _
  | GroundTuple _
  | GroundRelation _ ->
    []

let sources_of_find_spec = function
  | Find_pull_source (source, _, _) | Find_pull_source_var (source, _, _) -> named_source source
  | Find_aggregate (_, terms) -> sources_of_query_terms terms
  | Find_var _ | Find_pull _ | Find_pull_var _ -> []

let rec has_rule_clause = function
  | Rule _ | SourceRule _ -> true
  | SourceClause (_, clause) -> has_rule_clause clause
  | Not clauses | SourceNot (_, clauses) | NotJoin (_, clauses) | SourceNotJoin (_, _, clauses) ->
    List.exists has_rule_clause clauses
  | Or branches | SourceOr (_, branches) | OrJoin (_, branches) | SourceOrJoin (_, _, branches) ->
    List.exists (List.exists has_rule_clause) branches
  | OrJoinRequired (_, _, branches) | SourceOrJoinRequired (_, _, _, branches) ->
    List.exists (List.exists has_rule_clause) branches
  | Pattern _
  | PatternTx _
  | PatternTxOp _
  | SourcePattern _
  | SourcePatternTx _
  | SourcePatternTxOp _
  | SourceRelationPattern _
  | Missing _
  | SourceMissing _
  | GetElse _
  | SourceGetElse _
  | GetSome _
  | SourceGetSome _
  | GetValue _
  | GetDefaultValue _
  | CountValue _
  | EmptyValue _
  | NotEmptyValue _
  | ContainsValue _
  | ValuePredicate _
  | NumericPredicate _
  | ComparisonPredicate _
  | ComparisonPredicateN _
  | EqualityPredicate _
  | ArithmeticValue _
  | CompareValue _
  | ExtremumValue _
  | BooleanPredicate _
  | BooleanNotPredicate _
  | BooleanNotValue _
  | IdentityValue _
  | BooleanAndPredicate _
  | BooleanAndValue _
  | BooleanOrPredicate _
  | BooleanOrValue _
  | RandomValue _
  | RandomIntValue _
  | DifferPredicate _
  | IdenticalPredicate _
  | TypeValue _
  | MetaValue _
  | NameValue _
  | NamespaceValue _
  | KeywordFromName _
  | KeywordFromNamespaceName _
  | StringIncludesValue _
  | StringStartsWithValue _
  | StringEndsWithValue _
  | StringLowerCaseValue _
  | StringUpperCaseValue _
  | StringCapitalizeValue _
  | StringReverseValue _
  | StringTrimValue _
  | StringTrimLeftValue _
  | StringTrimRightValue _
  | StringTrimNewlineValue _
  | StringIndexOfValue _
  | StringLastIndexOfValue _
  | StringSubstringValue _
  | StringBuildValue _
  | PrintStringValue _
  | PrintLineStringValue _
  | PrStringValue _
  | PrnStringValue _
  | StringJoinPlainValue _
  | StringJoinValue _
  | StringReplaceValue _
  | StringReplaceFirstValue _
  | StringEscapeValue _
  | RePatternValue _
  | ReFindValue _
  | ReMatchesValue _
  | ReSeqValue _
  | ReFindPredicate _
  | ReMatchesPredicate _
  | StringBlankValue _
  | StringSplitValue _
  | StringSplitLimitValue _
  | StringSplitLinesValue _
  | Ground _
  | GroundCollection _
  | GroundTuple _
  | GroundRelation _
  | GroundTerm _
  | GroundTermCollection _
  | GroundTermTuple _
  | GroundTermRelation _
  | VectorValue _
  | ListValue _
  | SetValue _
  | HashMapValue _
  | ArrayMapValue _
  | RangeEndValue _
  | RangeValue _
  | RangeStepValue _
  | TupleFunction _
  | UntupleFunction _
  | Predicate _
  | Function _
  | DynamicPredicate _
  | DynamicFunction _
  | DynamicFunctionCollection _
  | DynamicFunctionRelation _ ->
    false

let rule_names rules =
  rules |> List.map (fun rule -> rule.rule_name) |> List.sort_uniq compare

let rec resolve_dynamic_rule_clause names = function
  | DynamicPredicate (name, terms) when List.mem name names -> Rule (name, terms)
  | SourceClause (source, DynamicPredicate (name, terms)) when List.mem name names ->
    SourceRule (source, name, terms)
  | SourceClause (source, clause) -> SourceClause (source, resolve_dynamic_rule_clause names clause)
  | Not clauses -> Not (List.map (resolve_dynamic_rule_clause names) clauses)
  | SourceNot (source, clauses) -> SourceNot (source, List.map (resolve_dynamic_rule_clause names) clauses)
  | NotJoin (vars, clauses) -> NotJoin (vars, List.map (resolve_dynamic_rule_clause names) clauses)
  | SourceNotJoin (source, vars, clauses) ->
    SourceNotJoin (source, vars, List.map (resolve_dynamic_rule_clause names) clauses)
  | Or branches -> Or (List.map (List.map (resolve_dynamic_rule_clause names)) branches)
  | SourceOr (source, branches) -> SourceOr (source, List.map (List.map (resolve_dynamic_rule_clause names)) branches)
  | OrJoin (vars, branches) -> OrJoin (vars, List.map (List.map (resolve_dynamic_rule_clause names)) branches)
  | SourceOrJoin (source, vars, branches) ->
    SourceOrJoin (source, vars, List.map (List.map (resolve_dynamic_rule_clause names)) branches)
  | OrJoinRequired (required_vars, vars, branches) ->
    OrJoinRequired (required_vars, vars, List.map (List.map (resolve_dynamic_rule_clause names)) branches)
  | SourceOrJoinRequired (source, required_vars, vars, branches) ->
    SourceOrJoinRequired
      (source, required_vars, vars, List.map (List.map (resolve_dynamic_rule_clause names)) branches)
  | clause -> clause

let resolve_dynamic_rule names rule =
  { rule with rule_body = List.map (resolve_dynamic_rule_clause names) rule.rule_body }

let find_spec_uses_default_source = function
  | Find_pull_source (source, _, _) | Find_pull_source_var (source, _, _) -> source = "$"
  | Find_var _ | Find_pull _ | Find_pull_var _ | Find_aggregate _ -> false

let rec clause_uses_default_source = function
  | SourceClause (source, clause) -> source = "$" || clause_uses_default_source clause
  | SourcePattern (source, _, _, _)
  | SourcePatternTx (source, _, _, _, _)
  | SourcePatternTxOp (source, _, _, _, _, _)
  | SourceRelationPattern (source, _)
  | SourceMissing (source, _, _)
  | SourceGetElse (source, _, _, _, _)
  | SourceGetSome (source, _, _, _, _)
  | SourceRule (source, _, _) ->
    source = "$"
  | SourceNot (source, clauses)
  | SourceNotJoin (source, _, clauses) ->
    source = "$" || List.exists clause_uses_default_source clauses
  | SourceOr (source, branches)
  | SourceOrJoin (source, _, branches)
  | SourceOrJoinRequired (source, _, _, branches) ->
    source = "$" || List.exists (List.exists clause_uses_default_source) branches
  | Not clauses | NotJoin (_, clauses) -> List.exists clause_uses_default_source clauses
  | Or branches | OrJoin (_, branches) | OrJoinRequired (_, _, branches) ->
    List.exists (List.exists clause_uses_default_source) branches
  | Pattern _
  | PatternTx _
  | PatternTxOp _ ->
    true
  | Missing _
  | GetElse _
  | GetSome _
  | GetValue _
  | GetDefaultValue _
  | CountValue _
  | EmptyValue _
  | NotEmptyValue _
  | ContainsValue _
  | ValuePredicate _
  | NumericPredicate _
  | ComparisonPredicate _
  | ComparisonPredicateN _
  | EqualityPredicate _
  | ArithmeticValue _
  | CompareValue _
  | ExtremumValue _
  | BooleanPredicate _
  | BooleanNotPredicate _
  | BooleanNotValue _
  | IdentityValue _
  | BooleanAndPredicate _
  | BooleanAndValue _
  | BooleanOrPredicate _
  | BooleanOrValue _
  | RandomValue _
  | RandomIntValue _
  | DifferPredicate _
  | IdenticalPredicate _
  | TypeValue _
  | MetaValue _
  | NameValue _
  | NamespaceValue _
  | KeywordFromName _
  | KeywordFromNamespaceName _
  | StringIncludesValue _
  | StringStartsWithValue _
  | StringEndsWithValue _
  | StringLowerCaseValue _
  | StringUpperCaseValue _
  | StringCapitalizeValue _
  | StringReverseValue _
  | StringTrimValue _
  | StringTrimLeftValue _
  | StringTrimRightValue _
  | StringTrimNewlineValue _
  | StringIndexOfValue _
  | StringLastIndexOfValue _
  | StringSubstringValue _
  | StringBuildValue _
  | PrintStringValue _
  | PrintLineStringValue _
  | PrStringValue _
  | PrnStringValue _
  | StringJoinPlainValue _
  | StringJoinValue _
  | StringReplaceValue _
  | StringReplaceFirstValue _
  | StringEscapeValue _
  | RePatternValue _
  | ReFindValue _
  | ReMatchesValue _
  | ReSeqValue _
  | ReFindPredicate _
  | ReMatchesPredicate _
  | StringBlankValue _
  | StringSplitValue _
  | StringSplitLimitValue _
  | StringSplitLinesValue _
  | Ground _
  | GroundCollection _
  | GroundTuple _
  | GroundRelation _
  | GroundTerm _
  | GroundTermCollection _
  | GroundTermTuple _
  | GroundTermRelation _
  | VectorValue _
  | ListValue _
  | SetValue _
  | HashMapValue _
  | ArrayMapValue _
  | RangeEndValue _
  | RangeValue _
  | RangeStepValue _
  | TupleFunction _
  | UntupleFunction _
  | Predicate _
  | Function _
  | DynamicPredicate _
  | DynamicFunction _
  | DynamicFunctionCollection _
  | DynamicFunctionRelation _
  | Rule _ ->
    false

let infer_default_inputs in_form find where inputs =
  match in_form with
  | Some _ -> inputs
  | None ->
    if List.exists find_spec_uses_default_source find || List.exists clause_uses_default_source where
    then Input_source_decl "$" :: inputs
    else inputs

let query_term_vars terms =
  terms
  |> List.filter_map (function
    | QVar var -> Some var
    | QEntity _ | QIdent _ | QLookupRef _ | QAttr _ | QValue _ | QSource _ | QWildcard -> None)

let vars_of_find_spec = function
  | Find_var var | Find_pull (var, _) | Find_pull_source (_, var, _) ->
    [ var ]
  | Find_aggregate (aggregate, terms) ->
    query_term_vars terms @ aggregate_param_vars aggregate @ aggregate_callable_vars aggregate
  | Find_pull_var (var, pattern_var) | Find_pull_source_var (_, var, pattern_var) ->
    [ var; pattern_var ]

let rec vars_of_input_binding = function
  | Bind_scalar var -> [ var ]
  | Bind_ignore -> []
  | Bind_collection binding -> vars_of_input_binding binding
  | Bind_tuple bindings -> bindings |> List.concat_map vars_of_input_binding

let vars_of_input = function
  | Input_scalar (var, _)
  | Input_entity_ref (var, _)
  | Input_collection (var, _)
  | Input_predicate (var, _)
  | Input_function (var, _)
  | Input_aggregate (var, _)
  | Input_scalar_decl var
  | Input_collection_decl var ->
    [ var ]
  | Input_collection_ignore _
  | Input_rules _
  | Input_ignore
  | Input_collection_ignore_decl
  | Input_ignore_decl
  | Input_rules_decl ->
    []
  | Input_nested_collection (binding, _)
  | Input_nested_collection_decl binding ->
    vars_of_input_binding binding
  | Input_tuple (vars, _)
  | Input_relation (vars, _)
  | Input_tuple_decl vars
  | Input_relation_decl vars ->
    List.filter (( <> ) "_") vars
  | Input_nested_tuple (bindings, _)
  | Input_nested_relation (bindings, _)
  | Input_nested_tuple_decl bindings ->
    bindings |> List.concat_map vars_of_input_binding
  | Input_nested_relation_decl bindings -> bindings |> List.concat_map vars_of_input_binding
  | Input_source_decl _ -> []

let source_of_input = function
  | Input_source_decl source -> Some source
  | Input_scalar _
  | Input_entity_ref _
  | Input_collection _
  | Input_collection_ignore _
  | Input_ignore
  | Input_nested_collection _
  | Input_tuple _
  | Input_relation _
  | Input_nested_tuple _
  | Input_nested_relation _
  | Input_predicate _
  | Input_function _
  | Input_aggregate _
  | Input_rules _
  | Input_scalar_decl _
  | Input_collection_decl _
  | Input_collection_ignore_decl
  | Input_ignore_decl
  | Input_rules_decl
  | Input_nested_collection_decl _
  | Input_tuple_decl _
  | Input_relation_decl _
  | Input_nested_tuple_decl _
  | Input_nested_relation_decl _ ->
    None

let ensure_distinct_input_vars inputs =
  let vars = List.concat_map vars_of_input inputs in
  if List.length vars <> List.length (List.sort_uniq compare vars) then
    invalid_arg "Vars used in :in should be distinct"

let ensure_distinct_input_sources inputs =
  let sources = List.filter_map source_of_input inputs in
  if List.length sources <> List.length (List.sort_uniq compare sources) then
    invalid_arg "Vars used in :in should be distinct"

let format_query_vars vars =
  vars
  |> List.map (fun var -> "?" ^ var)
  |> String.concat " "
  |> Printf.sprintf "[%s]"

let format_source_vars sources =
  sources
  |> List.map (fun source -> if source = "$" then "$" else "$" ^ source)
  |> String.concat " "
  |> Printf.sprintf "[%s]"

let validate_query query =
  ensure_distinct_input_vars query.inputs;
  ensure_distinct_input_sources query.inputs;
  if List.length query.with_vars <> List.length (List.sort_uniq compare query.with_vars) then
    invalid_arg "Vars used in :with should be distinct";
  let declared_sources = List.filter_map source_of_input query.inputs |> List.sort_uniq compare in
  let used_sources =
    List.concat_map sources_of_find_spec query.find @ List.concat_map sources_of_clause query.where
    |> List.sort_uniq compare
  in
  let unknown_sources = List.filter (fun source -> not (List.mem source declared_sources)) used_sources in
  let available_vars =
    List.concat_map vars_of_input query.inputs
    @ List.concat_map vars_of_clause query.where
    |> List.sort_uniq compare
  in
  let unknown_find_vars =
    query.find
    |> List.concat_map vars_of_find_spec
    |> List.sort_uniq compare
    |> List.filter (fun var -> not (List.mem var available_vars))
  in
  (match unknown_find_vars with
   | [] -> query
   | _ :: _ -> invalid_arg ("Query for unknown vars: " ^ format_query_vars unknown_find_vars))
  |> fun query ->
  let unknown_with_vars =
    query.with_vars |> List.filter (fun var -> not (List.mem var available_vars))
  in
  (match unknown_with_vars with
   | [] -> query
   | _ :: _ -> invalid_arg ("Query for unknown vars: " ^ format_query_vars unknown_with_vars))
  |> fun query ->
  let find_vars = List.concat_map vars_of_find_spec query.find |> List.sort_uniq compare in
  let shared_vars = List.filter (fun var -> List.mem var find_vars) query.with_vars in
  match shared_vars with
  | [] ->
    (match unknown_sources with
     | [] -> query
     | _ :: _ -> invalid_arg ("Where uses unknown source vars: " ^ format_source_vars unknown_sources))
  | _ :: _ -> invalid_arg (":find and :with should not use same variables: " ^ format_query_vars shared_vars)

let query_input_var_label var =
  if String.length var > 0 && (var.[0] = '?' || var.[0] = '$') then var else "?" ^ var

let query_term_string ~value_to_string = function
  | QVar var -> query_input_var_label var
  | QEntity entity_id -> string_of_int entity_id
  | QIdent ident -> ":" ^ ident
  | QLookupRef (attr, value) -> "[:" ^ attr ^ " " ^ value_to_string value ^ "]"
  | QAttr attr -> ":" ^ attr
  | QValue value -> value_to_string value
  | QSource "$" -> "$"
  | QSource source -> "$" ^ source
  | QWildcard -> "_"

let query_output_var_string var =
  if var = "_" then "_" else query_input_var_label var

let query_output_binding_string = function
  | [ var ] -> query_output_var_string var
  | vars -> "[" ^ String.concat " " (List.map query_output_var_string vars) ^ "]"

let query_call_string ~value_to_string symbol terms =
  "("
  ^ String.concat " " (symbol :: List.map (query_term_string ~value_to_string) terms)
  ^ ")"

let numeric_predicate_symbol = function
  | ZeroNumber -> "zero?"
  | PositiveNumber -> "pos?"
  | NegativeNumber -> "neg?"
  | EvenInteger -> "even?"
  | OddInteger -> "odd?"

let arithmetic_op_symbol = function
  | AddNumbers -> "+"
  | SubtractNumbers -> "-"
  | MultiplyNumbers -> "*"
  | DivideNumbers -> "/"
  | IncrementNumber -> "inc"
  | DecrementNumber -> "dec"
  | QuotientNumbers -> "quot"
  | RemainderNumbers -> "rem"
  | ModuloNumbers -> "mod"

let rec query_clause_string ~value_to_string = function
  | Pattern (e, a, v) ->
    "["
    ^ String.concat " " (List.map (query_term_string ~value_to_string) [ e; a; v ])
    ^ "]"
  | PatternTx (e, a, v, tx) ->
    "["
    ^ String.concat " " (List.map (query_term_string ~value_to_string) [ e; a; v; tx ])
    ^ "]"
  | PatternTxOp (e, a, v, tx, op) ->
    "["
    ^ String.concat " " (List.map (query_term_string ~value_to_string) [ e; a; v; tx; op ])
    ^ "]"
  | SourcePattern (source, e, a, v) ->
    "["
    ^ String.concat " " (("$" ^ source) :: List.map (query_term_string ~value_to_string) [ e; a; v ])
    ^ "]"
  | SourcePatternTx (source, e, a, v, tx) ->
    "["
    ^ String.concat " " (("$" ^ source) :: List.map (query_term_string ~value_to_string) [ e; a; v; tx ])
    ^ "]"
  | SourcePatternTxOp (source, e, a, v, tx, op) ->
    "["
    ^ String.concat " " (("$" ^ source) :: List.map (query_term_string ~value_to_string) [ e; a; v; tx; op ])
    ^ "]"
  | SourceRelationPattern (source, terms) ->
    "["
    ^ String.concat " " (("$" ^ source) :: List.map (query_term_string ~value_to_string) terms)
    ^ "]"
  | NumericPredicate (predicate, term) ->
    "[" ^ query_call_string ~value_to_string (numeric_predicate_symbol predicate) [ term ] ^ "]"
  | ReFindPredicate (pattern, value) ->
    "[" ^ query_call_string ~value_to_string "re-find" [ pattern; value ] ^ "]"
  | ReMatchesPredicate (pattern, value) ->
    "[" ^ query_call_string ~value_to_string "re-matches" [ pattern; value ] ^ "]"
  | ArithmeticValue (op, terms, output_var) ->
    "["
    ^ query_call_string ~value_to_string (arithmetic_op_symbol op) terms
    ^ " "
    ^ query_output_var_string output_var
    ^ "]"
  | DynamicPredicate (name, terms) ->
    "[" ^ query_call_string ~value_to_string name terms ^ "]"
  | DynamicFunction (name, terms, output_vars) ->
    "["
    ^ query_call_string ~value_to_string name terms
    ^ " "
    ^ query_output_binding_string output_vars
    ^ "]"
  | DynamicFunctionCollection (name, terms, output_var) ->
    "["
    ^ query_call_string ~value_to_string name terms
    ^ " ["
    ^ query_output_var_string output_var
    ^ " ...]]"
  | DynamicFunctionRelation (name, terms, output_vars) ->
    "["
    ^ query_call_string ~value_to_string name terms
    ^ " [["
    ^ String.concat " " (List.map query_output_var_string output_vars)
    ^ "]]]"
  | Not clauses | SourceNot (_, clauses) -> query_not_clause_string ~value_to_string clauses
  | Or branches | SourceOr (_, branches) -> query_or_clause_string ~value_to_string branches
  | OrJoin (vars, branches) | SourceOrJoin (_, vars, branches) ->
    query_or_join_clause_string ~value_to_string [] vars branches
  | OrJoinRequired (required_vars, vars, branches) | SourceOrJoinRequired (_, required_vars, vars, branches) ->
    query_or_join_clause_string ~value_to_string required_vars vars branches
  | clause -> "<" ^ string_of_int (List.length (vars_of_clause clause)) ^ "-var clause>"

and query_not_clause_string ~value_to_string clauses =
  "(not "
  ^ String.concat " " (List.map (query_clause_string ~value_to_string) clauses)
  ^ ")"

and query_branch_string ~value_to_string = function
  | [ clause ] -> query_clause_string ~value_to_string clause
  | clauses ->
    "(and "
    ^ String.concat " " (List.map (query_clause_string ~value_to_string) clauses)
    ^ ")"

and query_or_clause_string ~value_to_string branches =
  "(or "
  ^ String.concat " " (List.map (query_branch_string ~value_to_string) branches)
  ^ ")"

and query_or_join_vars_string required_vars vars =
  let free = List.map query_input_var_label vars in
  match required_vars with
  | [] -> "[" ^ String.concat " " free ^ "]"
  | required_vars ->
    let required = "[" ^ String.concat " " (List.map query_input_var_label required_vars) ^ "]" in
    "[" ^ String.concat " " (required :: free) ^ "]"

and query_or_join_clause_string ~value_to_string required_vars vars branches =
  "(or-join "
  ^ query_or_join_vars_string required_vars vars
  ^ " "
  ^ String.concat " " (List.map (query_branch_string ~value_to_string) branches)
  ^ ")"

let query_var_set_string vars =
  "#{" ^ String.concat " " (List.map query_input_var_label vars) ^ "}"

let query_var_sets_string var_sets =
  "[" ^ String.concat " " (List.map query_var_set_string var_sets) ^ "]"

let unbound_vars_of_terms bindings terms =
  let bound_vars = List.map fst bindings in
  terms
  |> vars_of_query_terms
  |> List.filter (fun var -> not (List.mem var bound_vars))
  |> List.sort_uniq compare

let ensure_query_terms_bound bindings terms clause_string =
  match unbound_vars_of_terms bindings terms with
  | [] -> ()
  | unbound_vars ->
    invalid_arg
      ( "Insufficient bindings: "
      ^ query_var_set_string unbound_vars
      ^ " not bound in "
      ^ clause_string )

let ensure_not_has_outer_binding ~value_to_string bindings clauses =
  let clause_vars = clauses |> List.concat_map vars_of_clause |> List.sort_uniq compare in
  let bound_vars = List.map fst bindings in
  if clause_vars <> [] && not (List.exists (fun var -> List.mem var bound_vars) clause_vars) then
    let unbound_vars = List.filter (fun var -> not (List.mem var bound_vars)) clause_vars in
    invalid_arg
      ( "Insufficient bindings: none of "
      ^ query_var_set_string unbound_vars
      ^ " is bound in "
      ^ query_not_clause_string ~value_to_string clauses )

let vars_of_branch clauses =
  clauses |> List.concat_map vars_of_clause |> List.sort_uniq compare

let free_vars_of_branch bound_vars clauses =
  vars_of_branch clauses |> List.filter (fun var -> not (List.mem var bound_vars))

let ensure_or_branch_vars_match ~value_to_string bindings branches =
  let bound_vars = List.map fst bindings in
  match List.map (free_vars_of_branch bound_vars) branches with
  | [] | [ _ ] -> ()
  | expected :: rest ->
    let branch_vars = expected :: rest in
    if List.exists (( <> ) expected) rest then
      invalid_arg
        ( "All clauses in 'or' must use same set of free vars, had "
        ^ query_var_sets_string branch_vars
        ^ " in "
        ^ query_or_clause_string ~value_to_string branches )

let ensure_join_vars_bound bindings vars =
  let bound_vars = List.map fst bindings in
  if List.exists (fun var -> not (List.mem var bound_vars)) vars then
    invalid_arg "insufficient bindings"

let ensure_join_vars_bound_in_clause bindings vars clause_string =
  let bound_vars = List.map fst bindings in
  let unbound_vars = List.filter (fun var -> not (List.mem var bound_vars)) vars in
  if unbound_vars <> [] then
    invalid_arg
      ( "Insufficient bindings: "
      ^ query_var_set_string unbound_vars
      ^ " not bound in "
      ^ clause_string )

let ensure_or_join_branches_cover_listed_vars bindings vars branches =
  let bound_vars = List.map fst bindings in
  let required_vars = List.filter (fun var -> not (List.mem var bound_vars)) vars in
  branches
  |> List.iter (fun branch ->
    let branch_vars = vars_of_branch branch in
    if List.exists (fun var -> not (List.mem var branch_vars)) required_vars then
      invalid_arg "or branches must use same free vars")

let rec clause_calls_rule name = function
  | Rule (rule_name, _) | SourceRule (_, rule_name, _) -> rule_name = name
  | SourceClause (_, clause) -> clause_calls_rule name clause
  | Not clauses | SourceNot (_, clauses) | NotJoin (_, clauses) | SourceNotJoin (_, _, clauses) ->
    List.exists (clause_calls_rule name) clauses
  | Or branches
  | SourceOr (_, branches)
  | OrJoin (_, branches)
  | SourceOrJoin (_, _, branches)
  | OrJoinRequired (_, _, branches)
  | SourceOrJoinRequired (_, _, _, branches) ->
    List.exists (List.exists (clause_calls_rule name)) branches
  | Pattern _ | PatternTx _ | PatternTxOp _ | SourcePattern _ | SourcePatternTx _ | SourcePatternTxOp _
  | SourceRelationPattern _ | Missing _ | SourceMissing _ | GetElse _ | SourceGetElse _ | GetSome _
  | SourceGetSome _ | GetValue _ | GetDefaultValue _ | CountValue _ | EmptyValue _ | NotEmptyValue _ | ContainsValue _
  | ValuePredicate _ | NumericPredicate _ | ComparisonPredicate _ | ComparisonPredicateN _ | EqualityPredicate _
  | ArithmeticValue _ | CompareValue _ | ExtremumValue _ | BooleanPredicate _ | BooleanNotPredicate _ | BooleanNotValue _
  | IdentityValue _ | BooleanAndPredicate _ | BooleanAndValue _ | BooleanOrPredicate _ | BooleanOrValue _
  | RandomValue _ | RandomIntValue _ | DifferPredicate _
  | IdenticalPredicate _ | TypeValue _ | MetaValue _ | NameValue _ | NamespaceValue _ | KeywordFromName _
  | KeywordFromNamespaceName _ | Ground _
  | GroundCollection _ | StringIncludesValue _ | StringStartsWithValue _ | StringEndsWithValue _
  | GroundTuple _ | GroundRelation _ | GroundTerm _ | GroundTermCollection _ | GroundTermTuple _
  | GroundTermRelation _ | StringLowerCaseValue _ | StringUpperCaseValue _
  | StringCapitalizeValue _ | StringReverseValue _ | StringTrimValue _ | StringTrimLeftValue _
  | StringTrimRightValue _ | StringTrimNewlineValue _ | StringIndexOfValue _ | StringLastIndexOfValue _
  | VectorValue _ | ListValue _ | SetValue _ | StringSubstringValue _ | StringBuildValue _
  | PrintStringValue _ | PrintLineStringValue _ | PrStringValue _ | PrnStringValue _ | StringJoinPlainValue _
  | StringJoinValue _
  | HashMapValue _ | ArrayMapValue _ | TupleFunction _ | StringReplaceValue _ | StringReplaceFirstValue _
  | StringEscapeValue _ | StringBlankValue _ | StringSplitValue _ | StringSplitLimitValue _
  | StringSplitLinesValue _
  | RePatternValue _ | ReFindValue _ | ReMatchesValue _ | ReSeqValue _ | ReFindPredicate _
  | ReMatchesPredicate _ | RangeEndValue _ | RangeValue _
  | RangeStepValue _ | UntupleFunction _ | Predicate _ | Function _ | DynamicPredicate _ | DynamicFunction _
  | DynamicFunctionCollection _ | DynamicFunctionRelation _ ->
    false

let matching_rules_for_call active_rules key rules name arity =
  let candidates = matching_rules_exn rules name arity in
  if List.mem key active_rules then
    List.filter (fun rule -> not (List.exists (clause_calls_rule name) rule.rule_body)) candidates
  else
    candidates

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

let values_of_collection_result = function
  | Result_value (List values | Vector values | Set values) -> Some (List.map (fun value -> Result_value value) values)
  | Result_value (Tuple values) ->
    Some (values |> List.filter_map (Option.map (fun value -> Result_value value)))
  | _ -> None

let row_of_collection_result = function
  | Result_value (List values | Vector values | Set values) -> List.map (fun value -> Result_value value) values
  | Result_value (Tuple values) ->
    values |> List.map (function Some value -> Result_value value | None -> Result_value Nil)
  | value -> [ value ]

let row_of_scalar_sequence value =
  match values_of_collection_result value with
  | Some row -> row
  | None -> invalid_arg "query input argument does not match :in binding"

let rows_of_map_entries entries =
  entries
  |> List.map (fun (key, value) -> [ Result_value key; Result_value value ])

let bind_relation_row context bindings vars row =
  if List.length vars <> List.length row then
    invalid_arg "relation input row arity mismatch";
  List.fold_left2
    (fun binding var value ->
      match binding, var with
      | None, _ -> None
      | Some binding, "_" -> Some binding
      | Some binding, _ -> context.bind_var var value binding)
    (Some bindings)
    vars
    row

let resolve_query_input_row context row =
  let rec resolve acc = function
    | [] -> Some (List.rev acc)
    | value :: rest ->
      (match context.resolve_query_input_result value with
       | Some value -> resolve (value :: acc) rest
       | None -> None)
  in
  resolve [] row

let collection_values_of_input context value =
  match context.resolve_query_input_result value with
  | Some (Result_value (List values | Vector values | Set values)) ->
    Some (List.map (fun value -> Result_value value) values)
  | Some (Result_value (Tuple values)) ->
    Some (values |> List.filter_map (Option.map (fun value -> Result_value value)))
  | Some _ | None -> None

let row_values_of_input context value =
  match context.resolve_query_input_result value with
  | Some (Result_value (List values | Vector values | Set values)) ->
    Some (List.map (fun value -> Result_value value) values)
  | Some (Result_value (Tuple values)) ->
    Some (values |> List.map (function Some value -> Result_value value | None -> Result_value Nil))
  | Some _ | None -> None

let eval_ground_term_tuple context bindings result output_vars =
  match row_values_of_input context result with
  | Some row ->
    (match bind_relation_row context bindings output_vars row with
     | Some bindings -> [ bindings ]
     | None -> [])
  | None -> []

let eval_ground_term_relation context bindings result output_vars =
  match collection_values_of_input context result with
  | Some rows ->
    rows
    |> List.filter_map (fun row ->
      match row_values_of_input context row with
      | Some row -> bind_relation_row context bindings output_vars row
      | None -> None)
  | None -> []

let rec bind_input_binding context input_binding value bindings =
  match input_binding with
  | Bind_scalar var ->
    (match context.resolve_query_input_result value with
     | Some value -> List.filter_map (fun binding -> context.bind_var var value binding) bindings
     | None -> [])
  | Bind_ignore ->
    (match context.resolve_query_input_result value with
     | Some _ -> bindings
     | None -> [])
  | Bind_collection binding ->
    (match collection_values_of_input context value with
     | Some values ->
       List.concat_map (fun value -> bind_input_binding context binding value bindings) values
     | None -> [])
  | Bind_tuple bindings_ ->
    (match row_values_of_input context value with
     | Some row -> bind_nested_input_tuple context bindings_ row bindings
     | None -> [])

and bind_nested_input_tuple context input_bindings row bindings =
  if List.length input_bindings <> List.length row then
    invalid_arg "relation input row arity mismatch";
  List.fold_left2
    (fun bindings input_binding value -> bind_input_binding context input_binding value bindings)
    bindings
    input_bindings
    row

let apply_query_input context bindings = function
  | Input_scalar (var, value) ->
    (match context.resolve_query_input_result value with
     | Some value -> List.filter_map (fun binding -> context.bind_var var value binding) bindings
     | None -> [])
  | Input_entity_ref (var, entity_ref) ->
    (match context.entity_id_of_ref entity_ref with
     | Some entity_id ->
       List.filter_map (fun binding -> context.bind_var var (Result_entity entity_id) binding) bindings
     | None -> [])
  | Input_collection (var, values) ->
    let values = List.filter_map context.resolve_query_input_result values in
    List.concat_map
      (fun binding -> List.filter_map (fun value -> context.bind_var var value binding) values)
      bindings
  | Input_collection_ignore values ->
    let _ = List.filter_map context.resolve_query_input_result values in
    bindings
  | Input_nested_collection (input_binding, values) ->
    List.concat_map (fun value -> bind_input_binding context input_binding value bindings) values
  | Input_tuple (vars, row) ->
    (match resolve_query_input_row context row with
     | Some row -> List.filter_map (fun binding -> bind_relation_row context binding vars row) bindings
     | None -> [])
  | Input_relation (vars, rows) ->
    let rows = List.filter_map (resolve_query_input_row context) rows in
    List.concat_map
      (fun binding -> List.filter_map (bind_relation_row context binding vars) rows)
      bindings
  | Input_nested_tuple (input_bindings, row) -> bind_nested_input_tuple context input_bindings row bindings
  | Input_nested_relation (input_bindings, rows) ->
    List.concat_map (fun row -> bind_nested_input_tuple context input_bindings row bindings) rows
  | Input_predicate _
  | Input_function _ ->
    bindings
  | Input_aggregate _ -> bindings
  | Input_rules _ -> bindings
  | Input_ignore -> bindings
  | Input_scalar_decl _
  | Input_collection_decl _
  | Input_collection_ignore_decl
  | Input_ignore_decl
  | Input_nested_collection_decl _
  | Input_tuple_decl _
  | Input_relation_decl _
  | Input_nested_tuple_decl _
  | Input_nested_relation_decl _ ->
    invalid_arg "query input declarations require supplied input arguments"
  | Input_source_decl _
  | Input_rules_decl ->
    bindings

let query_input_arity_error ~consume_rules declarations provided =
  let labels =
    declarations
    |> List.map query_input_binding_label
    |> String.concat " "
  in
  let required =
    declarations
    |> List.filter (query_input_consumes_argument ~consume_rules)
    |> List.length
    |> ( + ) 1
  in
  invalid_arg
    (Printf.sprintf
       "Wrong number of arguments for bindings [%s], %d required, %d provided"
       labels
       required
       provided)

let bind_query_inputs ~query_input_of_arg ~consume_rules declarations args =
  let provided = List.length args + 1 in
  let arity_error () = query_input_arity_error ~consume_rules declarations provided in
  let rec bind acc declarations args =
    match declarations with
    | [] ->
      (match args with
       | [] -> List.rev acc
       | _ :: _ -> arity_error ())
    | (Input_scalar _ | Input_entity_ref _ | Input_collection _ | Input_tuple _ | Input_relation _
      | Input_nested_collection _ | Input_nested_tuple _ | Input_nested_relation _ | Input_predicate _
      | Input_function _ | Input_aggregate _ | Input_ignore as input)
      :: rest ->
      bind (input :: acc) rest args
    | Input_collection_ignore _ as input :: rest -> bind (input :: acc) rest args
    | Input_source_decl _ as input :: rest -> bind (input :: acc) rest args
    | Input_rules_decl as input :: rest when not consume_rules -> bind (input :: acc) rest args
    | decl :: rest ->
      (match args with
       | [] -> arity_error ()
       | arg :: args -> bind (query_input_of_arg decl arg :: acc) rest args)
  in
  bind [] declarations args
