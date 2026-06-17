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
