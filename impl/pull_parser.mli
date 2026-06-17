open Datascript_types

type context =
  { cardinality : db -> attr -> cardinality
  ; is_ref_attr : db -> attr -> bool
  ; is_reverse_ref : attr -> bool
  ; reverse_ref : attr -> attr
  ; query_value_of_form : query_form -> value
  ; read_edn : string -> query_form
  ; split_keyword : string -> string * string
  }

val parse_pattern : context -> db -> query_form -> pull_selector list
val parse_pattern_string : context -> db -> string -> pull_selector list
