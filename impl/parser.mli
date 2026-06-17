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
