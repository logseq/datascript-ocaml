open Datascript_types

val list_equal_by : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
val entity_ref_equal : entity_ref -> entity_ref -> bool
val value_equal : value -> value -> bool
val split_keyword : string -> string * string
val compare_list_with : ('a -> 'a -> int) -> 'a list -> 'a list -> int
val compare_option_with : ('a -> 'a -> int) -> 'a option -> 'a option -> int
val compare_value : value -> value -> int
val first_nonzero : int list -> int
val compare_datom : index -> datom -> datom -> int
val normalize_value : value -> value
val normalize_datom_value : datom -> datom
