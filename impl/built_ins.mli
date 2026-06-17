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
val aggregate_result : aggregate -> query_result list -> query_result
val value_is_truthy : value -> bool
val boolean_and_value : value list -> value
val boolean_or_value : value list -> value
val split_at : int -> 'a list -> 'a list * 'a list
val values_equal : value -> value -> bool
val type_keyword_of_value : value -> string
val value_contains : value -> value -> bool
val range_values : int -> int -> int -> int list
