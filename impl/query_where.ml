open Datascript_types

type bindings = (string * query_result) list

module Make (Context : sig
  val query_evaluator_context : Query_eval.evaluator_context
  val edn_string_of_value : value -> string
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
