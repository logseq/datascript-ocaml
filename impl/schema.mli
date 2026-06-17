open Datascript_types

val validate_schema : schema -> schema
val schema_attr_by_name : schema -> attr -> schema_attr option
val schema_attr_is_ref : schema -> attr -> bool
val schema_attr_is_tuple : schema_attr option -> bool
val schema_attr_is_avet_accessible : schema -> attr -> bool
val schema_has_no_history : schema -> attr -> bool
val schema_fields : attr list
val schema_from_transaction_datoms :
  ?strict:bool ->
  ?removed_attrs:attr list ->
  ?removed_fields:(attr * attr) list ->
  ?ignored_schema_entities:entity_id list ->
  schema ->
  datom list ->
  schema
val split_namespaced_attr : attr -> string option * string
val join_namespaced_attr : string option -> string -> attr
val is_reverse_ref : attr -> bool
val reverse_ref : attr -> attr
