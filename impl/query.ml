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
