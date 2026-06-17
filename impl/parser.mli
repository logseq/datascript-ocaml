open Datascript_types

val read_edn : string -> query_form
val query_value_of_form : query_form -> value
val query_form_of_value : value -> query_form
