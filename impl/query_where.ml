open Datascript_types

type bindings = (string * query_result) list

module Make (Context : sig
  val query_evaluator_context : Query_eval.evaluator_context
  val edn_string_of_value : value -> string
  val query_source_context : db -> Query.source_context
  val query_match_context : db -> Query.match_context
  val eval_query_term : db -> bindings -> query_term -> query_result option
  val collect_query_terms_exn : db -> bindings -> query_term list -> query_result list
  val bind_var : db -> string -> query_result -> bindings -> bindings option
  val match_query_source_pattern : db -> query_source -> bindings -> query_term list -> bindings list
  val match_source_pattern : db -> (string * query_source) list -> string -> bindings -> query_term list -> bindings list
  val match_relation_source_pattern : db -> (string * query_source) list -> string -> bindings -> query_term list -> bindings list
  val bind_relation_row : db -> bindings -> string list -> query_result list -> bindings option
  val collection_values_of_input : db -> query_result -> query_result list option
  val row_values_of_input : db -> query_result -> query_result list option
  val eval_ground_term_tuple : db -> bindings -> query_result -> string list -> bindings list
  val eval_ground_term_relation : db -> bindings -> query_result -> string list -> bindings list
  val rule_invocation_binding : db -> bindings -> query_rule -> query_term list -> bindings option
  val rule_invocation_callables : Query.query_callables -> bindings -> query_rule -> query_term list -> Query.query_callables
  val propagate_rule_binding : db -> bindings -> bindings -> query_rule -> query_term list -> bindings option
  val normalize_value : value -> value
end) = struct
  open Context

  let empty_query_callables = Query.empty_query_callables
  let callable_predicate = Query.callable_predicate
  let callable_function = Query.callable_function
  let source = Query.source
  let sources_with_root_default = Query.sources_with_root_default
  let source_db = Query.source_db
  let query_source_db = Query.query_source_db

  let eval_missing_clause = Query_eval.eval_missing_clause query_evaluator_context
  let eval_get_else_clause = Query_eval.eval_get_else_clause query_evaluator_context
  let eval_get_some_clause = Query_eval.eval_get_some_clause query_evaluator_context
  let eval_ground_tuple = Query_eval.eval_ground_tuple query_evaluator_context
  let eval_ground_result = Query_eval.eval_ground_result query_evaluator_context
  let eval_get_value_clause = Query_eval.eval_get_value_clause query_evaluator_context
  let eval_get_default_value_clause = Query_eval.eval_get_default_value_clause query_evaluator_context
  let eval_count_value_clause = Query_eval.eval_count_value_clause query_evaluator_context
  let value_has_count = Query_eval.value_has_count
  let value_is_not_empty = Query_eval.value_is_not_empty
  let eval_value_predicate_clause = Query_eval.eval_value_predicate_clause query_evaluator_context
  let eval_type_predicate_clause = Query_eval.eval_type_predicate_clause query_evaluator_context
  let eval_numeric_predicate_clause = Query_eval.eval_numeric_predicate_clause query_evaluator_context
  let eval_comparison_predicate_clause = Query_eval.eval_comparison_predicate_clause query_evaluator_context
  let eval_comparison_predicate_n_clause = Query_eval.eval_comparison_predicate_n_clause query_evaluator_context
  let eval_equality_predicate_clause = Query_eval.eval_equality_predicate_clause query_evaluator_context
  let eval_arithmetic_clause = Query_eval.eval_arithmetic_clause query_evaluator_context
  let eval_compare_value_clause = Query_eval.eval_compare_value_clause query_evaluator_context
  let eval_extremum_value_clause = Query_eval.eval_extremum_value_clause query_evaluator_context
  let eval_boolean_predicate_clause = Query_eval.eval_boolean_predicate_clause query_evaluator_context
  let eval_boolean_not_predicate_clause = Query_eval.eval_boolean_not_predicate_clause query_evaluator_context
  let eval_boolean_not_clause = Query_eval.eval_boolean_not_clause query_evaluator_context
  let eval_identity_value_clause = Query_eval.eval_identity_value_clause query_evaluator_context
  let eval_boolean_and_predicate_clause = Query_eval.eval_boolean_and_predicate_clause query_evaluator_context
  let eval_boolean_and_clause = Query_eval.eval_boolean_and_clause query_evaluator_context
  let eval_boolean_or_predicate_clause = Query_eval.eval_boolean_or_predicate_clause query_evaluator_context
  let eval_boolean_or_clause = Query_eval.eval_boolean_or_clause query_evaluator_context
  let eval_random_value_clause = Query_eval.eval_random_value_clause query_evaluator_context
  let eval_random_int_value_clause = Query_eval.eval_random_int_value_clause query_evaluator_context
  let eval_differ_predicate_clause = Query_eval.eval_differ_predicate_clause query_evaluator_context
  let eval_identical_predicate_clause = Query_eval.eval_identical_predicate_clause query_evaluator_context
  let eval_type_value_clause = Query_eval.eval_type_value_clause query_evaluator_context
  let eval_meta_value_clause = Query_eval.eval_meta_value_clause query_evaluator_context
  let eval_name_value_clause = Query_eval.eval_name_value_clause query_evaluator_context
  let eval_namespace_value_clause = Query_eval.eval_namespace_value_clause query_evaluator_context
  let eval_keyword_from_name_clause = Query_eval.eval_keyword_from_name_clause query_evaluator_context
  let eval_keyword_from_namespace_name_clause = Query_eval.eval_keyword_from_namespace_name_clause query_evaluator_context
  let string_starts_with = Query_eval.string_starts_with
  let string_ends_with = Query_eval.string_ends_with
  let string_index_of = Query_eval.string_index_of
  let string_includes = Query_eval.string_includes
  let string_last_index_of = Query_eval.string_last_index_of
  let eval_string_predicate_clause = Query_eval.eval_string_predicate_clause query_evaluator_context
  let eval_string_index_clause = Query_eval.eval_string_index_clause query_evaluator_context
  let eval_string_substring_clause = Query_eval.eval_string_substring_clause query_evaluator_context
  let eval_print_string_clause = Query_eval.eval_print_string_clause query_evaluator_context
  let eval_string_build_clause = Query_eval.eval_string_build_clause query_evaluator_context
  let eval_string_join_clause = Query_eval.eval_string_join_clause query_evaluator_context
  let eval_string_join_plain_clause = Query_eval.eval_string_join_plain_clause query_evaluator_context
  let eval_string_replace_clause = Query_eval.eval_string_replace_clause query_evaluator_context
  let eval_string_escape_clause = Query_eval.eval_string_escape_clause query_evaluator_context
  let regex_find = Query_eval.regex_find
  let regex_matches = Query_eval.regex_matches
  let eval_re_pattern_value_clause = Query_eval.eval_re_pattern_value_clause query_evaluator_context
  let eval_regex_string_clause = Query_eval.eval_regex_string_clause query_evaluator_context
  let eval_regex_predicate_clause = Query_eval.eval_regex_predicate_clause query_evaluator_context
  let eval_re_seq_value_clause = Query_eval.eval_re_seq_value_clause query_evaluator_context
  let eval_string_blank_clause = Query_eval.eval_string_blank_clause query_evaluator_context
  let is_ascii_whitespace = Query_eval.is_ascii_whitespace
  let eval_string_split_clause = Query_eval.eval_string_split_clause query_evaluator_context
  let eval_string_split_limit_clause = Query_eval.eval_string_split_limit_clause query_evaluator_context
  let eval_string_split_lines_clause = Query_eval.eval_string_split_lines_clause query_evaluator_context
  let reverse_string = Query_eval.reverse_string
  let capitalize_string = Query_eval.capitalize_string
  let trim_left_with = Query_eval.trim_left_with
  let trim_right_with = Query_eval.trim_right_with
  let trim_with = Query_eval.trim_with
  let is_newline = Query_eval.is_newline
  let eval_string_transform_clause = Query_eval.eval_string_transform_clause query_evaluator_context
  let eval_contains_value_clause = Query_eval.eval_contains_value_clause query_evaluator_context
  let eval_tuple_function = Query_eval.eval_tuple_function query_evaluator_context
  let eval_collection_value_clause = Query_eval.eval_collection_value_clause query_evaluator_context
  let eval_hash_map_value_clause = Query_eval.eval_hash_map_value_clause query_evaluator_context
  let eval_range_end_value_clause = Query_eval.eval_range_end_value_clause query_evaluator_context
  let eval_range_value_clause = Query_eval.eval_range_value_clause query_evaluator_context
  let eval_range_step_value_clause = Query_eval.eval_range_step_value_clause query_evaluator_context
  let eval_untuple_function = Query_eval.eval_untuple_function query_evaluator_context

  let project_binding = Query.project_binding
  let ( let* ) = Option.bind

  let query_callables_empty (callables : Query.query_callables) =
    callables.callable_predicates = []
    && callables.callable_functions = []
    && callables.callable_aggregates = []
    && callables.callable_aliases = []

  type relation =
    { attrs : string list
    ; rows : query_result list list
    }

  let unique_vars terms =
    terms
    |> List.filter_map (function
      | QVar name -> Some name
      | _ -> None)
    |> List.fold_left
         (fun vars var -> if List.mem var vars then vars else var :: vars)
         []
    |> List.rev

  let binding_row attrs binding =
    let rec collect acc = function
      | [] -> Some (List.rev acc)
      | attr :: rest ->
        (match List.assoc_opt attr binding with
         | Some value -> collect (value :: acc) rest
         | None -> None)
    in
    collect [] attrs

  let row_binding attrs row =
    List.combine attrs row

  let direct_pattern_term = function
    | QVar _ | QEntity _ | QAttr _ | QValue _ | QWildcard -> true
    | QIdent _ | QLookupRef _ | QSource _ -> false

  let direct_pattern_terms terms =
    List.for_all direct_pattern_term terms
    &&
    (match terms with
     | [ _; _; QValue _ ] | [ _; _; QValue _; _ ] | [ _; _; QValue _; _; _ ] -> false
     | _ -> true)
    &&
    let vars = unique_vars terms in
    List.length vars = List.length (List.filter_map (function QVar var -> Some var | _ -> None) terms)

  let result_of_pattern_position datom position =
    match position with
    | 0 -> Query.result_of_datom_e datom
    | 1 -> Query.result_of_datom_a datom
    | 2 -> Query.result_of_ref (Query.result_of_datom_v datom)
    | 3 -> Query.result_of_datom_tx datom
    | 4 -> Query.result_of_datom_op datom
    | _ -> invalid_arg "invalid datom pattern position"

  let direct_pattern_row attrs terms datom =
    attrs
    |> List.map (fun attr ->
      let rec find index = function
        | [] -> invalid_arg "pattern variable is missing from row"
        | QVar var :: _ when var = attr -> result_of_pattern_position datom index
        | _ :: rest -> find (index + 1) rest
      in
      find 0 terms)

  let direct_entity_value_binding e_var entity_id value_var value =
    [ value_var, Query.result_of_ref (Result_value value); e_var, Result_entity entity_id ]

  let fast_entity_value_join db source_db e_var match_attr match_value value_attr value_var =
    let source_context = query_source_context db in
    source_context.pattern_datoms source_db (QVar e_var) (QAttr match_attr) (QValue match_value) None
    |> Seq.filter (fun datom -> query_evaluator_context.compare_value datom.v match_value = 0)
    |> Seq.flat_map (fun match_datom ->
      source_context.pattern_datoms source_db (QEntity match_datom.e) (QAttr value_attr) (QVar value_var) None)
    |> Seq.map (fun datom -> direct_entity_value_binding e_var datom.e value_var datom.v)
    |> List.of_seq

  let fast_comparison_scan db source_db e_var attr value_var predicate threshold =
    let source_context = query_source_context db in
    source_context.pattern_datoms source_db (QVar e_var) (QAttr attr) (QVar value_var) None
    |> Seq.filter_map (fun datom ->
      if
        Built_ins.matches_comparison_predicate
          predicate
          (query_evaluator_context.compare_value datom.v threshold)
      then Some (direct_entity_value_binding e_var datom.e value_var datom.v)
      else None)
    |> List.of_seq

  let eval_fast_index_clauses db source clauses =
    match source, clauses with
    | Db_source source_db,
      [ Pattern (QVar e1, QAttr match_attr, QValue match_value)
      ; Pattern (QVar e2, QAttr value_attr, QVar value_var)
      ]
      when e1 = e2 && e1 <> value_var ->
      Some (fast_entity_value_join db source_db e1 match_attr match_value value_attr value_var)
    | Db_source source_db,
      [ Pattern (QVar e2, QAttr value_attr, QVar value_var)
      ; Pattern (QVar e1, QAttr match_attr, QValue match_value)
      ]
      when e1 = e2 && e1 <> value_var ->
      Some (fast_entity_value_join db source_db e1 match_attr match_value value_attr value_var)
    | Db_source source_db,
      [ Pattern (QVar e, QAttr attr, QVar value_var)
      ; ComparisonPredicate (predicate, QVar compared_var, QValue threshold)
      ]
      when compared_var = value_var && e <> value_var ->
      Some (fast_comparison_scan db source_db e attr value_var predicate threshold)
    | Db_source source_db,
      [ ComparisonPredicate (predicate, QVar compared_var, QValue threshold)
      ; Pattern (QVar e, QAttr attr, QVar value_var)
      ]
      when compared_var = value_var && e <> value_var ->
      Some (fast_comparison_scan db source_db e attr value_var predicate threshold)
    | _ -> None

  let relation_of_pattern db source terms =
    match source with
    | Relation_source _ -> None
    | Db_source source_db ->
      let source_context = query_source_context db in
      let attrs = unique_vars terms in
      let can_direct =
        direct_pattern_terms terms
        &&
        match terms with
        | [ _; QAttr attr; _ ] | [ _; QAttr attr; _; _ ] ->
          not (query_evaluator_context.is_reverse_ref attr)
        | [ _; _; _; _; _ ] -> false
        | _ -> false
      in
      let rows =
        if can_direct then
          match terms with
          | [ e_term; a_term; v_term ] ->
            source_context.pattern_datoms source_db e_term a_term v_term None
            |> Seq.map (direct_pattern_row attrs terms)
            |> List.of_seq
          | [ e_term; a_term; v_term; tx_term ] ->
            source_context.pattern_datoms source_db e_term a_term v_term (Some tx_term)
            |> Seq.map (direct_pattern_row attrs terms)
            |> List.of_seq
          | [ e_term; a_term; v_term; tx_term; _op_term ] ->
            source_context.pattern_datoms source_db e_term a_term v_term (Some tx_term)
            |> Seq.map (direct_pattern_row attrs terms)
            |> List.of_seq
          | _ -> invalid_arg "database source patterns expect 3, 4, or 5 terms"
        else
          match terms with
          | [ e_term; a_term; v_term ] ->
            source_context.pattern_datoms source_db e_term a_term v_term None
            |> Seq.filter_map (fun datom ->
              let* binding = source_context.match_data_pattern source_db [] e_term a_term v_term datom in
              binding_row attrs binding)
            |> List.of_seq
          | [ e_term; a_term; v_term; tx_term ] ->
            source_context.pattern_datoms source_db e_term a_term v_term (Some tx_term)
            |> Seq.filter_map (fun datom ->
              let* binding =
                source_context.match_data_pattern_tx source_db [] e_term a_term v_term tx_term datom
              in
              binding_row attrs binding)
            |> List.of_seq
          | [ e_term; a_term; v_term; tx_term; op_term ] ->
            source_context.pattern_datoms source_db e_term a_term v_term (Some tx_term)
            |> Seq.filter_map (fun datom ->
              let* binding =
                source_context.match_data_pattern_tx_op source_db [] e_term a_term v_term tx_term op_term datom
              in
              binding_row attrs binding)
            |> List.of_seq
          | _ -> invalid_arg "database source patterns expect 3, 4, or 5 terms"
      in
      Some { attrs; rows }

  let relation_key attrs common row =
    let binding = row_binding attrs row in
    List.map (fun attr -> List.assoc attr binding) common

  let append_relation_rows left_attrs right_attrs left_row right_row =
    let right_binding = row_binding right_attrs right_row in
    let extra_right_values =
      right_attrs
      |> List.filter_map (fun attr ->
        if List.mem attr left_attrs then None else Some (List.assoc attr right_binding))
    in
    left_row @ extra_right_values

  let hash_join left right =
    let common = List.filter (fun attr -> List.mem attr right.attrs) left.attrs in
    let right_only = List.filter (fun attr -> not (List.mem attr left.attrs)) right.attrs in
    let attrs = left.attrs @ right_only in
    if common = [] then
      { attrs
      ; rows =
          List.concat_map
            (fun left_row ->
              List.map
                (fun right_row -> append_relation_rows left.attrs right.attrs left_row right_row)
                right.rows)
            left.rows
      }
    else
      let grouped = Hashtbl.create (List.length left.rows) in
      List.iter
        (fun row ->
          let key = relation_key left.attrs common row in
          let rows = Option.value (Hashtbl.find_opt grouped key) ~default:[] in
          Hashtbl.replace grouped key (row :: rows))
        left.rows;
      let rows =
        right.rows
        |> List.concat_map (fun right_row ->
          let key = relation_key right.attrs common right_row in
          match Hashtbl.find_opt grouped key with
          | None -> []
          | Some left_rows ->
            List.map
              (fun left_row -> append_relation_rows left.attrs right.attrs left_row right_row)
              left_rows)
      in
      { attrs; rows }

  let value_of_relation_term db relation row term =
    let binding = row_binding relation.attrs row in
    match eval_query_term db binding term with
    | Some result -> Query_eval.value_of_query_result result
    | None -> None

  let relation_comparison_matches db relation row predicate left_term right_term =
    match
      ( value_of_relation_term db relation row left_term
      , value_of_relation_term db relation row right_term )
    with
    | Some left, Some right ->
      Built_ins.matches_comparison_predicate
        predicate
        (query_evaluator_context.compare_value left right)
    | _ -> false

  let filter_relation_comparison db relation predicate left_term right_term =
    { relation with
      rows =
        List.filter
          (fun row -> relation_comparison_matches db relation row predicate left_term right_term)
          relation.rows
    }

  let relation_bindings relation =
    List.map (row_binding relation.attrs) relation.rows

  let eval_relation_clauses db sources default_source bindings clauses =
    match bindings with
    | [ [] ] ->
      let rec apply relation = function
        | [] -> Some (relation_bindings relation)
        | Pattern (e_term, a_term, v_term) :: rest ->
          let* next = relation_of_pattern db default_source [ e_term; a_term; v_term ] in
          apply (hash_join relation next) rest
        | PatternTx (e_term, a_term, v_term, tx_term) :: rest ->
          let* next = relation_of_pattern db default_source [ e_term; a_term; v_term; tx_term ] in
          apply (hash_join relation next) rest
        | PatternTxOp (e_term, a_term, v_term, tx_term, op_term) :: rest ->
          let* next = relation_of_pattern db default_source [ e_term; a_term; v_term; tx_term; op_term ] in
          apply (hash_join relation next) rest
        | SourcePattern (source_name, e_term, a_term, v_term) :: rest ->
          let* next = relation_of_pattern db (source db sources source_name) [ e_term; a_term; v_term ] in
          apply (hash_join relation next) rest
        | SourcePatternTx (source_name, e_term, a_term, v_term, tx_term) :: rest ->
          let* next =
            relation_of_pattern db (source db sources source_name) [ e_term; a_term; v_term; tx_term ]
          in
          apply (hash_join relation next) rest
        | SourcePatternTxOp (source_name, e_term, a_term, v_term, tx_term, op_term) :: rest ->
          let* next =
            relation_of_pattern
              db
              (source db sources source_name)
              [ e_term; a_term; v_term; tx_term; op_term ]
          in
          apply (hash_join relation next) rest
        | ComparisonPredicate (predicate, left_term, right_term) :: rest ->
          apply (filter_relation_comparison db relation predicate left_term right_term) rest
        | _ -> None
      in
      (match eval_fast_index_clauses db default_source clauses with
       | Some bindings -> Some bindings
       | None -> apply { attrs = []; rows = [ [] ] } clauses)
    | _ -> None
  
  let merge_projected_binding db vars outer_binding inner_binding =
    vars
    |> List.fold_left
         (fun binding var ->
           match binding with
           | None -> None
           | Some binding ->
             (match List.assoc_opt var inner_binding with
              | Some value -> bind_var db var value binding
              | None -> Some binding))
         (Some outer_binding)
  
  let rec eval_clauses
      ?(active_rules = [])
      ?(callables = empty_query_callables)
      ?default_source
      db
      sources
      rules
      bindings
      clauses =
    let default_source = Option.value default_source ~default:(source db sources "$") in
    match active_rules, query_callables_empty callables, rules, eval_relation_clauses db sources default_source bindings clauses with
    | [], true, [], Some bindings -> bindings
    | _ ->
      List.fold_left
        (fun bindings clause ->
          List.concat_map
            (fun binding ->
               eval_clause ~active_rules ~callables ~default_source db sources rules binding clause)
            bindings)
        bindings
        clauses
  
  and query_clause_string clause =
    Query.query_clause_string ~value_to_string:edn_string_of_value clause
  
  and query_or_join_clause_string required_vars vars branches =
    Query.query_or_join_clause_string ~value_to_string:edn_string_of_value required_vars vars branches
  
  and ensure_query_terms_bound bindings terms clause_string =
    Query.ensure_query_terms_bound bindings terms clause_string
  
  and ensure_not_has_outer_binding bindings clauses =
    Query.ensure_not_has_outer_binding ~value_to_string:edn_string_of_value bindings clauses
  
  and ensure_or_branch_vars_match bindings branches =
    Query.ensure_or_branch_vars_match ~value_to_string:edn_string_of_value bindings branches
  
  and ensure_join_vars_bound bindings vars =
    Query.ensure_join_vars_bound bindings vars
  
  and ensure_join_vars_bound_in_clause bindings vars clause_string =
    Query.ensure_join_vars_bound_in_clause bindings vars clause_string
  
  and ensure_or_join_branches_cover_listed_vars bindings vars branches =
    Query.ensure_or_join_branches_cover_listed_vars bindings vars branches
  
  and rule_call_key db source name bindings terms =
    source, name, List.map (eval_query_term db bindings) terms
  
  and matching_rules_for_call active_rules key rules name arity =
    Query.matching_rules_for_call active_rules key rules name arity
  
  and collect_dynamic_query_terms_exn db sources bindings terms =
    Query.collect_dynamic_query_terms_exn (query_match_context db) db sources bindings terms
  
  and eval_dynamic_predicate_clause callables db sources bindings name terms =
    match callable_predicate callables name with
    | Some predicate ->
      if predicate (collect_dynamic_query_terms_exn db sources bindings terms) then [ bindings ] else []
    | None ->
      invalid_arg
        ("Unknown predicate '" ^ name ^ " in " ^ query_clause_string (DynamicPredicate (name, terms)))
  
  and eval_dynamic_function_clause callables db sources bindings name terms output_vars =
    match callable_function callables name with
    | Some f ->
      (match f (collect_dynamic_query_terms_exn db sources bindings terms) with
       | Some outputs ->
         (match bind_relation_row db bindings output_vars outputs with
          | Some bindings -> [ bindings ]
          | None -> [])
       | None -> [])
    | None ->
      invalid_arg
        ("Unknown function '" ^ name ^ " in " ^ query_clause_string (DynamicFunction (name, terms, output_vars)))
  
  and eval_dynamic_function_collection_clause callables db sources bindings name terms output_var =
    match callable_function callables name with
    | Some f ->
      (match f (collect_dynamic_query_terms_exn db sources bindings terms) with
       | Some [ result ] ->
         (match collection_values_of_input db result with
          | Some values ->
            values
            |> List.filter_map (fun value ->
              match bind_var db output_var value bindings with
              | Some bindings -> Some bindings
              | None -> None)
          | None -> [])
       | Some _ -> invalid_arg "dynamic collection function output must return one collection"
       | None -> [])
    | None ->
      invalid_arg
        ( "Unknown function '"
        ^ name
        ^ " in "
        ^ query_clause_string (DynamicFunctionCollection (name, terms, output_var)) )
  
  and eval_dynamic_function_relation_clause callables db sources bindings name terms output_vars =
    match callable_function callables name with
    | Some f ->
      (match f (collect_dynamic_query_terms_exn db sources bindings terms) with
       | Some [ result ] ->
         (match collection_values_of_input db result with
          | Some values ->
            values
            |> List.filter_map (fun value ->
              match row_values_of_input db value with
              | Some row -> bind_relation_row db bindings output_vars row
              | None -> None)
          | None -> [])
       | Some _ -> invalid_arg "dynamic relation function output must return one collection"
       | None -> [])
    | None ->
      invalid_arg
        ( "Unknown function '"
        ^ name
        ^ " in "
        ^ query_clause_string (DynamicFunctionRelation (name, terms, output_vars)) )
  
  and eval_clause
      ?(active_rules = [])
      ?(callables = empty_query_callables)
      ?default_source
      db
      sources
      rules
      bindings =
    let default_source = Option.value default_source ~default:(source db sources "$") in
    function
    | Pattern (e_term, a_term, v_term) ->
      match_query_source_pattern db default_source bindings [ e_term; a_term; v_term ]
    | PatternTx (e_term, a_term, v_term, tx_term) ->
      match_query_source_pattern db default_source bindings [ e_term; a_term; v_term; tx_term ]
    | PatternTxOp (e_term, a_term, v_term, tx_term, op_term) ->
      match_query_source_pattern db default_source bindings [ e_term; a_term; v_term; tx_term; op_term ]
    | SourcePattern (source, e_term, a_term, v_term) ->
      match_source_pattern db sources source bindings [ e_term; a_term; v_term ]
    | SourcePatternTx (source, e_term, a_term, v_term, tx_term) ->
      match_source_pattern db sources source bindings [ e_term; a_term; v_term; tx_term ]
    | SourcePatternTxOp (source, e_term, a_term, v_term, tx_term, op_term) ->
      match_source_pattern db sources source bindings [ e_term; a_term; v_term; tx_term; op_term ]
    | SourceRelationPattern (source, terms) ->
      match_relation_source_pattern db sources source bindings terms
    | Missing (entity_term, attr) ->
      eval_missing_clause (query_source_db default_source) bindings entity_term attr
    | SourceMissing (source, entity_term, attr) ->
      eval_missing_clause (source_db db sources source) bindings entity_term attr
    | GetElse (entity_term, attr, default, output_var) ->
      eval_get_else_clause (query_source_db default_source) bindings entity_term attr default output_var
    | SourceGetElse (source, entity_term, attr, default, output_var) ->
      eval_get_else_clause (source_db db sources source) bindings entity_term attr default output_var
    | GetSome (entity_term, attrs, attr_var, value_var) ->
      eval_get_some_clause (query_source_db default_source) bindings entity_term attrs attr_var value_var
    | SourceGetSome (source, entity_term, attrs, attr_var, value_var) ->
      eval_get_some_clause (source_db db sources source) bindings entity_term attrs attr_var value_var
    | GetValue (map_term, key_term, output_var) ->
      eval_get_value_clause db bindings map_term key_term output_var
    | GetDefaultValue (map_term, key_term, default_term, output_var) ->
      eval_get_default_value_clause db bindings map_term key_term default_term output_var
    | CountValue (term, output_var) ->
      eval_count_value_clause db bindings term output_var
    | EmptyValue term ->
      eval_value_predicate_clause db bindings term (value_has_count 0)
    | NotEmptyValue term ->
      eval_value_predicate_clause db bindings term value_is_not_empty
    | ContainsValue (collection_term, key_term) ->
      eval_contains_value_clause db bindings collection_term key_term
    | ValuePredicate (predicate, term) ->
      eval_type_predicate_clause db bindings predicate term
    | NumericPredicate (predicate, term) ->
      ensure_query_terms_bound bindings [ term ] (query_clause_string (NumericPredicate (predicate, term)));
      eval_numeric_predicate_clause db bindings predicate term
    | ComparisonPredicate (predicate, left_term, right_term) ->
      eval_comparison_predicate_clause db bindings predicate left_term right_term
    | ComparisonPredicateN (predicate, terms) ->
      eval_comparison_predicate_n_clause db bindings predicate terms
    | EqualityPredicate (predicate, terms) ->
      eval_equality_predicate_clause db bindings predicate terms
    | ArithmeticValue (op, terms, output_var) ->
      ensure_query_terms_bound bindings terms (query_clause_string (ArithmeticValue (op, terms, output_var)));
      eval_arithmetic_clause db bindings op terms output_var
    | CompareValue (left_term, right_term, output_var) ->
      eval_compare_value_clause db bindings left_term right_term output_var
    | ExtremumValue (op, terms, output_var) ->
      eval_extremum_value_clause db bindings op terms output_var
    | BooleanPredicate (predicate, term) ->
      eval_boolean_predicate_clause db bindings predicate term
    | BooleanNotPredicate term ->
      eval_boolean_not_predicate_clause db bindings term
    | BooleanNotValue (term, output_var) ->
      eval_boolean_not_clause db bindings term output_var
    | IdentityValue (term, output_var) ->
      eval_identity_value_clause db bindings term output_var
    | BooleanAndPredicate terms ->
      eval_boolean_and_predicate_clause db bindings terms
    | BooleanAndValue (terms, output_var) ->
      eval_boolean_and_clause db bindings terms output_var
    | BooleanOrPredicate terms ->
      eval_boolean_or_predicate_clause db bindings terms
    | BooleanOrValue (terms, output_var) ->
      eval_boolean_or_clause db bindings terms output_var
    | RandomValue output_var ->
      eval_random_value_clause db bindings output_var
    | RandomIntValue (bound_term, output_var) ->
      eval_random_int_value_clause db bindings bound_term output_var
    | DifferPredicate terms ->
      eval_differ_predicate_clause db bindings terms
    | IdenticalPredicate (left_term, right_term) ->
      eval_identical_predicate_clause db bindings left_term right_term
    | TypeValue (term, output_var) ->
      eval_type_value_clause db bindings term output_var
    | MetaValue (term, output_var) ->
      eval_meta_value_clause db bindings term output_var
    | NameValue (term, output_var) ->
      eval_name_value_clause db bindings term output_var
    | NamespaceValue (term, output_var) ->
      eval_namespace_value_clause db bindings term output_var
    | KeywordFromName (term, output_var) ->
      eval_keyword_from_name_clause db bindings term output_var
    | KeywordFromNamespaceName (namespace_term, name_term, output_var) ->
      eval_keyword_from_namespace_name_clause db bindings namespace_term name_term output_var
    | StringIncludesValue (left_term, right_term) ->
      eval_string_predicate_clause db bindings left_term right_term string_includes
    | StringStartsWithValue (left_term, right_term) ->
      eval_string_predicate_clause db bindings left_term right_term string_starts_with
    | StringEndsWithValue (left_term, right_term) ->
      eval_string_predicate_clause db bindings left_term right_term string_ends_with
    | StringLowerCaseValue (term, output_var) ->
      eval_string_transform_clause db bindings term output_var String.lowercase_ascii
    | StringUpperCaseValue (term, output_var) ->
      eval_string_transform_clause db bindings term output_var String.uppercase_ascii
    | StringCapitalizeValue (term, output_var) ->
      eval_string_transform_clause db bindings term output_var capitalize_string
    | StringReverseValue (term, output_var) ->
      eval_string_transform_clause db bindings term output_var reverse_string
    | StringTrimValue (term, output_var) ->
      eval_string_transform_clause db bindings term output_var (trim_with is_ascii_whitespace)
    | StringTrimLeftValue (term, output_var) ->
      eval_string_transform_clause db bindings term output_var (trim_left_with is_ascii_whitespace)
    | StringTrimRightValue (term, output_var) ->
      eval_string_transform_clause db bindings term output_var (trim_right_with is_ascii_whitespace)
    | StringTrimNewlineValue (term, output_var) ->
      eval_string_transform_clause db bindings term output_var (trim_right_with is_newline)
    | StringIndexOfValue (value_term, needle_term, output_var) ->
      eval_string_index_clause db bindings value_term needle_term output_var string_index_of
    | StringLastIndexOfValue (value_term, needle_term, output_var) ->
      eval_string_index_clause db bindings value_term needle_term output_var string_last_index_of
    | StringSubstringValue (value_term, start_term, end_term, output_var) ->
      eval_string_substring_clause db bindings value_term start_term end_term output_var
    | StringBuildValue (terms, output_var) ->
      eval_string_build_clause db bindings terms output_var
    | PrintStringValue (terms, output_var) ->
      eval_print_string_clause db bindings terms output_var ~readably:false ~newline:false
    | PrintLineStringValue (terms, output_var) ->
      eval_print_string_clause db bindings terms output_var ~readably:false ~newline:true
    | PrStringValue (terms, output_var) ->
      eval_print_string_clause db bindings terms output_var ~readably:true ~newline:false
    | PrnStringValue (terms, output_var) ->
      eval_print_string_clause db bindings terms output_var ~readably:true ~newline:true
    | StringJoinPlainValue (collection_term, output_var) ->
      eval_string_join_plain_clause db bindings collection_term output_var
    | StringJoinValue (separator_term, collection_term, output_var) ->
      eval_string_join_clause db bindings separator_term collection_term output_var
    | StringReplaceValue (value_term, pattern_term, replacement_term, output_var) ->
      eval_string_replace_clause db bindings value_term pattern_term replacement_term output_var false
    | StringReplaceFirstValue (value_term, pattern_term, replacement_term, output_var) ->
      eval_string_replace_clause db bindings value_term pattern_term replacement_term output_var true
    | StringEscapeValue (value_term, replacement_term, output_var) ->
      eval_string_escape_clause db bindings value_term replacement_term output_var
    | RePatternValue (pattern_term, output_var) ->
      eval_re_pattern_value_clause db bindings pattern_term output_var
    | ReFindValue (pattern_term, value_term, output_var) ->
      eval_regex_string_clause db bindings pattern_term value_term output_var regex_find
    | ReMatchesValue (pattern_term, value_term, output_var) ->
      eval_regex_string_clause db bindings pattern_term value_term output_var regex_matches
    | ReSeqValue (pattern_term, value_term, output_var) ->
      eval_re_seq_value_clause db bindings pattern_term value_term output_var
    | ReFindPredicate (pattern_term, value_term) ->
      eval_regex_predicate_clause db bindings pattern_term value_term regex_find
    | ReMatchesPredicate (pattern_term, value_term) ->
      eval_regex_predicate_clause db bindings pattern_term value_term regex_matches
    | StringBlankValue term ->
      eval_string_blank_clause db bindings term
    | StringSplitValue (value_term, separator_term, output_var) ->
      eval_string_split_clause db bindings value_term separator_term output_var
    | StringSplitLimitValue (value_term, separator_term, limit_term, output_var) ->
      eval_string_split_limit_clause db bindings value_term separator_term limit_term output_var
    | StringSplitLinesValue (value_term, output_var) ->
      eval_string_split_lines_clause db bindings value_term output_var
    | Ground (value, output_var) ->
      eval_ground_result db bindings (Result_value value) output_var
    | GroundCollection (values, output_var) ->
      values
      |> List.concat_map (fun value -> eval_ground_result db bindings (Result_value value) output_var)
    | GroundTuple (values, output_vars) ->
      eval_ground_tuple db bindings values output_vars
    | GroundRelation (rows, output_vars) ->
      rows |> List.concat_map (fun values -> eval_ground_tuple db bindings values output_vars)
    | GroundTerm (term, output_var) ->
      (match eval_query_term db bindings term with
       | Some result -> eval_ground_result db bindings result output_var
       | None -> [])
    | GroundTermCollection (term, output_var) ->
      (match eval_query_term db bindings term with
       | Some result ->
         (match collection_values_of_input db result with
          | Some values -> values |> List.concat_map (fun value -> eval_ground_result db bindings value output_var)
          | None -> [])
       | None -> [])
    | GroundTermTuple (term, output_vars) ->
      (match eval_query_term db bindings term with
       | Some result -> eval_ground_term_tuple db bindings result output_vars
       | None -> [])
    | GroundTermRelation (term, output_vars) ->
      (match eval_query_term db bindings term with
       | Some result -> eval_ground_term_relation db bindings result output_vars
       | None -> [])
    | VectorValue (terms, output_var) ->
      eval_collection_value_clause db bindings terms output_var (fun values -> Vector values)
    | ListValue (terms, output_var) ->
      eval_collection_value_clause db bindings terms output_var (fun values -> List values)
    | SetValue (terms, output_var) ->
      eval_collection_value_clause db bindings terms output_var (fun values -> normalize_value (Set values))
    | HashMapValue (terms, output_var) ->
      eval_hash_map_value_clause db bindings terms output_var
    | ArrayMapValue (terms, output_var) ->
      eval_hash_map_value_clause db bindings terms output_var
    | RangeEndValue (end_term, output_var) ->
      eval_range_end_value_clause db bindings end_term output_var
    | RangeValue (start_term, end_term, output_var) ->
      eval_range_value_clause db bindings start_term end_term output_var
    | RangeStepValue (start_term, end_term, step_term, output_var) ->
      eval_range_step_value_clause db bindings start_term end_term step_term output_var
    | TupleFunction (terms, output_var) ->
      eval_tuple_function db bindings terms output_var
    | UntupleFunction (tuple_term, output_vars) ->
      eval_untuple_function db bindings tuple_term output_vars
    | Predicate (_name, terms, predicate) ->
      if predicate (collect_query_terms_exn db bindings terms) then [ bindings ] else []
    | Function (_name, terms, output_vars, f) ->
      (match f (collect_query_terms_exn db bindings terms) with
       | Some outputs ->
         (match bind_relation_row db bindings output_vars outputs with
          | Some bindings -> [ bindings ]
          | None -> [])
       | None -> [])
    | DynamicPredicate (name, terms) ->
      eval_dynamic_predicate_clause callables db sources bindings name terms
    | DynamicFunction (name, terms, output_vars) ->
      eval_dynamic_function_clause callables db sources bindings name terms output_vars
    | DynamicFunctionCollection (name, terms, output_var) ->
      eval_dynamic_function_collection_clause callables db sources bindings name terms output_var
    | DynamicFunctionRelation (name, terms, output_binding) ->
      eval_dynamic_function_relation_clause callables db sources bindings name terms output_binding
    | SourceClause (source_name, clause) ->
      let clause_db = source_db db sources source_name in
      eval_clause
        ~active_rules
        ~callables
        ~default_source:(Db_source clause_db)
        clause_db
        sources
        rules
        bindings
        clause
    | Not clauses ->
      ensure_not_has_outer_binding bindings clauses;
      (match eval_clauses ~active_rules ~callables ~default_source db sources rules [ bindings ] clauses with
       | [] -> [ bindings ]
       | _ -> [])
    | SourceNot (source, clauses) ->
      let clause_db = source_db db sources source in
      let sources = sources_with_root_default db sources in
      ensure_not_has_outer_binding bindings clauses;
      (match
         eval_clauses
           ~active_rules
           ~callables
           ~default_source:(Db_source clause_db)
           clause_db
           sources
           rules
           [ bindings ]
           clauses
       with
       | [] -> [ bindings ]
       | _ -> [])
    | NotJoin (vars, clauses) ->
      ensure_join_vars_bound bindings vars;
      let projected_binding = project_binding vars bindings in
      (match eval_clauses ~active_rules ~callables ~default_source db sources rules [ projected_binding ] clauses with
       | [] -> [ bindings ]
       | _ -> [])
    | SourceNotJoin (source, vars, clauses) ->
      let clause_db = source_db db sources source in
      let sources = sources_with_root_default db sources in
      ensure_join_vars_bound bindings vars;
      let projected_binding = project_binding vars bindings in
      (match
         eval_clauses
           ~active_rules
           ~callables
           ~default_source:(Db_source clause_db)
           clause_db
           sources
           rules
           [ projected_binding ]
           clauses
       with
       | [] -> [ bindings ]
       | _ -> [])
    | Or branches ->
      ensure_or_branch_vars_match bindings branches;
      List.concat_map
        (fun clauses -> eval_clauses ~active_rules ~callables ~default_source db sources rules [ bindings ] clauses)
        branches
    | SourceOr (source, branches) ->
      let clause_db = source_db db sources source in
      let sources = sources_with_root_default db sources in
      ensure_or_branch_vars_match bindings branches;
      List.concat_map
        (fun clauses ->
           eval_clauses
             ~active_rules
             ~callables
             ~default_source:(Db_source clause_db)
             clause_db
             sources
             rules
             [ bindings ]
             clauses)
        branches
    | OrJoin (vars, branches) ->
      ensure_or_join_branches_cover_listed_vars bindings vars branches;
      let projected_binding = project_binding vars bindings in
      branches
      |> List.concat_map
           (fun clauses ->
              eval_clauses ~active_rules ~callables ~default_source db sources rules [ projected_binding ] clauses)
      |> List.filter_map (merge_projected_binding db vars bindings)
    | SourceOrJoin (source, vars, branches) ->
      let clause_db = source_db db sources source in
      let sources = sources_with_root_default db sources in
      ensure_or_join_branches_cover_listed_vars bindings vars branches;
      let projected_binding = project_binding vars bindings in
      branches
      |> List.concat_map
           (fun clauses ->
              eval_clauses
                ~active_rules
                ~callables
                ~default_source:(Db_source clause_db)
                clause_db
                sources
                rules
                [ projected_binding ]
                clauses)
      |> List.filter_map (merge_projected_binding clause_db vars bindings)
    | OrJoinRequired (required_vars, vars, branches) ->
      ensure_join_vars_bound_in_clause
        bindings
        required_vars
        (query_or_join_clause_string required_vars vars branches);
      ensure_or_join_branches_cover_listed_vars bindings vars branches;
      let projected_binding = project_binding (required_vars @ vars |> List.sort_uniq compare) bindings in
      branches
      |> List.concat_map
           (fun clauses ->
              eval_clauses ~active_rules ~callables ~default_source db sources rules [ projected_binding ] clauses)
      |> List.filter_map (merge_projected_binding db vars bindings)
    | SourceOrJoinRequired (source, required_vars, vars, branches) ->
      let clause_db = source_db db sources source in
      let sources = sources_with_root_default db sources in
      ensure_join_vars_bound_in_clause
        bindings
        required_vars
        (query_or_join_clause_string required_vars vars branches);
      ensure_or_join_branches_cover_listed_vars bindings vars branches;
      let projected_binding = project_binding (required_vars @ vars |> List.sort_uniq compare) bindings in
      branches
      |> List.concat_map
           (fun clauses ->
              eval_clauses
                ~active_rules
                ~callables
                ~default_source:(Db_source clause_db)
                clause_db
                sources
                rules
                [ projected_binding ]
                clauses)
      |> List.filter_map (merge_projected_binding clause_db vars bindings)
    | Rule (name, terms) ->
      let key = rule_call_key db "" name bindings terms in
      matching_rules_for_call active_rules key rules name (List.length terms)
      |> List.concat_map (fun rule ->
        match rule_invocation_binding db bindings rule terms with
        | None -> []
        | Some rule_binding ->
          let rule_callables = rule_invocation_callables callables bindings rule terms in
          eval_clauses
            ~active_rules:(key :: active_rules)
            ~callables:rule_callables
            ~default_source
            db
            sources
            rules
            [ rule_binding ]
            rule.rule_body
          |> List.filter_map (fun rule_binding -> propagate_rule_binding db bindings rule_binding rule terms))
    | SourceRule (source, name, terms) ->
      let rule_db = source_db db sources source in
      let key = rule_call_key rule_db source name bindings terms in
      matching_rules_for_call active_rules key rules name (List.length terms)
      |> List.concat_map (fun rule ->
        match rule_invocation_binding rule_db bindings rule terms with
        | None -> []
        | Some rule_binding ->
          let rule_callables = rule_invocation_callables callables bindings rule terms in
          eval_clauses
            ~active_rules:(key :: active_rules)
            ~callables:rule_callables
            ~default_source:(Db_source rule_db)
            rule_db
            sources
            rules
            [ rule_binding ]
            rule.rule_body
          |> List.filter_map (fun rule_binding -> propagate_rule_binding rule_db bindings rule_binding rule terms))
  
end
