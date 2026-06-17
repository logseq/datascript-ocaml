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

val empty_query_callables : query_callables

val q : context -> ?inputs:query_arg list -> db -> query -> query_result list list
val q_string : context -> ?inputs:query_arg list -> db -> string -> query_result list list
val q_with :
  context -> ?inputs:query_arg list -> db -> string list -> query -> query_result list list
val q_with_string :
  context -> ?inputs:query_arg list -> db -> string list -> string -> query_result list list
val q_sources :
  context ->
  ?inputs:query_arg list ->
  db ->
  (string * query_source) list ->
  query ->
  query_result list list
val q_sources_string :
  context ->
  ?inputs:query_arg list ->
  db ->
  (string * query_source) list ->
  string ->
  query_result list list
val q_return : context -> ?inputs:query_arg list -> db -> query_return -> query -> query_output
val q_return_string : context -> ?inputs:query_arg list -> db -> string -> query_output
val q_return_map :
  context ->
  ?inputs:query_arg list ->
  db ->
  query_return ->
  query_return_map ->
  query ->
  query_output
val q_return_map_string : context -> ?inputs:query_arg list -> db -> string -> query_output
val has_aggregates : find_spec list -> bool
val collect_find_vars : (string * query_result) list -> string list -> query_result list option
val group_by_key :
  (query_result list * (string * query_result) list) list ->
  (query_result list * (string * query_result) list list) list
val grouping_vars_of_find : find_spec list -> string list
val aggregate_amount_value : string -> (string * query_result) list -> int
val resolve_dynamic_aggregate : aggregate -> (string * query_result) list list -> aggregate
val aggregate_param_vars : aggregate -> string list
val aggregate_callable_vars : aggregate -> string list
val split_aggregate_terms : query_term list -> query_term list * query_term
val aggregate_input_values : aggregate -> query_result list -> query_result list -> query_result list
val resolve_callable_name : query_callables -> string -> string
val callable_predicate : query_callables -> string -> (query_result list -> bool) option
val callable_function : query_callables -> string -> (query_result list -> query_result list option) option
val callable_aggregate : query_callables -> string -> (query_result list -> query_result) option
val has_callable : query_callables -> string -> bool
val alias_callable : query_callables -> string -> string -> query_callables
val resolve_callable_aggregate : query_callables -> aggregate -> aggregate
val query_callables_of_inputs : query_input list -> query_callables
val query_rules_of_inputs : query_input list -> query_rule list
val matching_rules : query_rule list -> string -> int -> query_rule list
val matching_rules_exn : query_rule list -> string -> int -> query_rule list
val project_binding : string list -> (string * query_result) list -> (string * query_result) list
val rule_invocation_callables :
  query_callables -> (string * query_result) list -> query_rule -> query_term list -> query_callables
val vars_of_query_term : query_term -> string list
val vars_of_query_terms : query_term list -> string list
val vars_of_clause : query_clause -> string list
val query_input_var_label : string -> string
val query_term_string : value_to_string:(value -> string) -> query_term -> string
val query_output_var_string : string -> string
val query_output_binding_string : string list -> string
val query_call_string : value_to_string:(value -> string) -> string -> query_term list -> string
val numeric_predicate_symbol : numeric_predicate -> string
val arithmetic_op_symbol : arithmetic_op -> string
val query_clause_string : value_to_string:(value -> string) -> query_clause -> string
val query_not_clause_string : value_to_string:(value -> string) -> query_clause list -> string
val query_or_clause_string : value_to_string:(value -> string) -> query_clause list list -> string
val query_or_join_vars_string : string list -> string list -> string
val query_or_join_clause_string :
  value_to_string:(value -> string) -> string list -> string list -> query_clause list list -> string
val query_var_set_string : string list -> string
val query_var_sets_string : string list list -> string
val unbound_vars_of_terms : (string * query_result) list -> query_term list -> string list
val ensure_query_terms_bound : (string * query_result) list -> query_term list -> string -> unit
val ensure_not_has_outer_binding :
  value_to_string:(value -> string) -> (string * query_result) list -> query_clause list -> unit
val vars_of_branch : query_clause list -> string list
val free_vars_of_branch : string list -> query_clause list -> string list
val ensure_or_branch_vars_match :
  value_to_string:(value -> string) ->
  (string * query_result) list ->
  query_clause list list ->
  unit
val ensure_join_vars_bound : (string * query_result) list -> string list -> unit
val ensure_join_vars_bound_in_clause : (string * query_result) list -> string list -> string -> unit
val ensure_or_join_branches_cover_listed_vars :
  (string * query_result) list -> string list -> query_clause list list -> unit
val clause_calls_rule : string -> query_clause -> bool
val matching_rules_for_call :
  (string * string * query_result option list) list ->
  string * string * query_result option list ->
  query_rule list ->
  string ->
  int ->
  query_rule list
val query_input_binding_string : input_binding -> string
val query_input_decl_binding_string : query_input -> string
val query_input_binding_label : query_input -> string
val query_input_consumes_argument : consume_rules:bool -> query_input -> bool
val values_of_collection_result : query_result -> query_result list option
val row_of_collection_result : query_result -> query_result list
val row_of_scalar_sequence : query_result -> query_result list
val rows_of_map_entries : (value * value) list -> query_result list list
val bind_query_inputs :
  query_input_of_arg:(query_input -> query_arg -> query_input) ->
  consume_rules:bool ->
  query_input list ->
  query_arg list ->
  query_input list
