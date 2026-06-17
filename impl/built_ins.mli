open Datascript_types

val map_get_value : (value * value) list -> value -> value option
val value_get : value -> value -> value option
val value_count : value -> int option
val value_has_count : int -> value -> bool
val value_is_not_empty : value -> bool
val matches_value_predicate : value_predicate -> value -> bool
val matches_numeric_predicate : numeric_predicate -> value -> bool
val matches_comparison_predicate : comparison_predicate -> int -> bool
val comparison_chain_matches : comparison_predicate -> value list -> bool
val all_values_equal : value list -> bool
val eval_arithmetic : arithmetic_op -> value list -> value option
val normalized_comparison : int -> int
val extremum_value : extremum_op -> value -> value list -> value
val string_starts_with : string -> string -> bool
val string_ends_with : string -> string -> bool
val string_index_of : string -> string -> int option
val string_includes : string -> string -> bool
val string_last_index_of : string -> string -> int option
val is_ascii_whitespace : char -> bool
val string_is_blank : string -> bool
val split_string : string -> string -> string list
val split_string_limited : string -> string -> int -> string list
val split_lines : string -> string list
val string_of_query_value : value -> string
val escaped_string_literal : string -> string
val print_query_value : readably:bool -> value -> string
val print_query_values : readably:bool -> value list -> string
val collection_string_values : value -> string list option
val replace_string : ?first_only:bool -> string -> string -> string -> string
val compile_regex : string -> Str.regexp
val replace_regex : ?first_only:bool -> string -> string -> string -> string
val string_escape_replacement : (value * value) list -> char -> string option
val escape_string : string -> (value * value) list -> string
val regex_pattern_of_result : query_result -> string option
val regex_find : string -> string -> string option
val regex_matches : string -> string -> string option
val regex_seq : string -> string -> string list
val split_regex : string -> string -> string list
val split_regex_limited : string -> string -> int -> string list
val reverse_string : string -> string
val capitalize_string : string -> string
val trim_left_with : (char -> bool) -> string -> string
val trim_right_with : (char -> bool) -> string -> string
val trim_with : (char -> bool) -> string -> string
val is_newline : char -> bool
val aggregate_result : aggregate -> query_result list -> query_result
val value_is_truthy : value -> bool
val boolean_and_value : value list -> value
val boolean_or_value : value list -> value
val split_at : int -> 'a list -> 'a list * 'a list
val values_equal : value -> value -> bool
val type_keyword_of_value : value -> string
val value_contains : value -> value -> bool
val range_values : int -> int -> int -> int list
