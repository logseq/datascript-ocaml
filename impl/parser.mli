open Datascript_types

val read_edn : string -> query_form
val query_value_of_form : query_form -> value
val query_form_of_value : value -> query_form
val section_forms : query_form -> query_form list
val query_form_section : string -> (query_form * query_form) list -> query_form option
val query_form_sections : query_form list -> (query_form * query_form) list
val query_form_map : query_form -> (query_form * query_form) list
val query_form_sequence : query_form -> query_form list option
val query_symbol_name : string -> string
val query_callable_name : string -> string
val is_plain_input_symbol : string -> bool
val is_query_input_symbol : string -> bool
val query_input_name : string -> string
val query_source_name : string -> string
val is_query_source_symbol : string -> bool
val is_plain_rule_symbol : string -> bool
val aggregate_of_symbol : string -> aggregate option
val amount_aggregate_of_symbol : string -> int -> aggregate option
val dynamic_amount_aggregate_of_symbol : string -> string -> aggregate option
val parse_find_arg : query_form -> query_term
val parse_find_args : query_form list -> query_term list
val parse_output_var : query_form -> string
val parse_output_vars : query_form -> string list
val parse_flat_output_vars : query_form -> string list option
val parse_collection_output_var : query_form -> string option
val parse_relation_output_vars : query_form -> string list option
val nonempty_input_vars : string -> string list -> string list
val input_relation_vars : query_form -> query_form list option
val input_var_of_form : query_form -> string option
val flat_input_vars : query_form list -> string list option
val parse_nested_input_binding : query_form -> input_binding
val nested_relation_binding : query_form -> input_binding list option
val parse_input_binding : query_form -> query_input option
val parse_inputs : query_form option -> query_input list
val parse_binding : query_form -> input_binding
val parse_in : query_form -> query_input list
val input_declares_rules_var : query_form option -> bool
val ensure_distinct_input_rules_var : query_form option -> unit
val parse_with_var : query_form -> string
val parse_with_section : query_form option -> string list
val parse_with : query_form -> string list
val parse_return_map_labels : string -> query_form -> string list
val parse_return_map_section : (query_form * query_form) list -> query_return_map option
val lookup_ref_of_form : query_form -> (attr * value) option
val parse_pattern_term :
  ?entity_position:bool ->
  ?attr_position:bool ->
  ?lookup_ref_position:bool ->
  ?source_position:bool ->
  query_form ->
  query_term
val comparison_predicate_of_symbol : string -> comparison_predicate option
val value_predicate_of_symbol : string -> value_predicate option
val numeric_predicate_of_symbol : string -> numeric_predicate option
val boolean_predicate_of_symbol : string -> boolean_predicate option
val unary_string_predicate_clause_of_symbol : string -> (query_term -> query_clause) option
val binary_string_predicate_clause_of_symbol : string -> (query_term -> query_term -> query_clause) option
val equality_predicate_of_symbol : string -> equality_predicate option
val arithmetic_op_of_symbol : string -> arithmetic_op option
val query_attr_name : query_form -> attr
val parse_data_pattern_clause : query_form list -> query_clause
val parse_rule_expr : string -> query_form list -> string * query_term list
val parse_source_pattern_clause : string -> query_form list -> query_clause
val parse_missing_clause : query_form list -> query_clause
val parse_get_else_clause : query_form list -> string -> query_clause
val parse_two_output_vars : query_form -> string * string
val parse_get_some_clause : query_form list -> query_form -> query_clause
val parse_get_clause : query_form list -> string -> query_clause
val parse_core_value_function : string -> query_form list -> string -> query_clause
val parse_collection_function : string -> query_form list -> string -> query_clause
val parse_flat_value_function : string -> query_form list -> string list -> query_clause
val ground_values_of_form : query_form -> value list
val ground_relation_rows_of_form : query_form -> value list list
val dynamic_ground_term : query_form -> query_term option
val parse_ground_function : query_form list -> query_form -> query_clause
val parse_value_metadata_function : string -> query_form list -> string -> query_clause
val parse_string_transform_function : string -> query_form list -> string -> query_clause

type query_context =
  { empty_db : unit -> db
  ; parse_pull_pattern : db -> query_form -> pull_selector list
  ; value_of_query_result : query_result -> value option
  ; string_is_blank : string -> bool
  ; string_includes : string -> string -> bool
  ; string_starts_with : string -> string -> bool
  ; string_ends_with : string -> string -> bool
  ; matches_value_predicate : value_predicate -> value -> bool
  ; matches_numeric_predicate : numeric_predicate -> value -> bool
  ; matches_boolean_predicate : boolean_predicate -> query_result -> bool
  ; comparison_chain_matches : comparison_predicate -> value list -> bool
  ; all_values_equal : value list -> bool
  ; value_has_count : int -> value -> bool
  ; value_is_not_empty : value -> bool
  ; value_contains : value -> value -> bool
  ; split_at : int -> value list -> value list * value list
  ; values_equal : value -> value -> bool
  }

val parse_find_form : query_context -> ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> query_form -> find_spec
val parse_find_relation : query_context -> ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> query_form option -> find_spec list
val is_find_form : query_context -> ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> query_form -> bool
val parse_find_return : query_context -> ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> query_form option -> query_return * find_spec list
val parse_find : query_context -> query_form -> query_return * find_spec list
val parse_complement_predicate_clause : query_context -> string -> query_form list -> query_clause
val parse_join_vars : string -> query_form -> string list
val parse_rule_var : query_form -> string
val ensure_distinct_rule_vars : string -> string list -> string list -> string list * string list
val parse_rule_vars : string -> query_form -> string list * string list
val or_join_clause : string list -> string list -> query_clause list list -> query_clause
val source_or_join_clause : string -> string list -> string list -> query_clause list list -> query_clause
val ensure_inferred_join_vars : string list -> unit
val parse_pattern_clause : query_context -> query_form -> query_clause
val parse_or_branch : query_context -> query_form -> query_clause list
val parse_rule_head : query_form -> string * string list
val parse_rule : query_context -> query_form -> query_rule
val validate_rule_arities : query_rule list -> query_rule list
val is_rule_head : query_form -> bool
val is_rule_form : query_form -> bool
val unwrap_extra_rules_nesting : query_form list -> query_form list
val parse_rules : query_context -> query_form option -> query_rule list
val parse_where : query_context -> query_form option -> query_clause list
val parse_query_return_with_pull_context : query_context -> ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> query_form -> query_return * query
val parse_query_return : query_context -> query_form -> query_return * query
val parse_query_return_map_with_pull_context : query_context -> ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> query_form -> query_return * query_return_map option * query
val parse_query_return_map : query_context -> query_form -> query_return * query_return_map option * query
val parse_query : query_context -> query_form -> query
val parse_query_with_pull_context : query_context -> ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> query_form -> query
val parse_query_string : query_context -> string -> query
val parse_query_string_with_pull_context : query_context -> ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> string -> query
val parse_query_return_string : query_context -> string -> query_return * query
val parse_query_return_string_with_pull_context : query_context -> ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> string -> query_return * query
val parse_query_return_map_string : query_context -> string -> query_return * query_return_map option * query
val parse_query_return_map_string_with_pull_context : query_context -> ?default_pull_db:db -> ?pull_db_for_source:(string -> db) -> string -> query_return * query_return_map option * query
