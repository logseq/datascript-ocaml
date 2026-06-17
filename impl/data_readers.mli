open Datascript_types

type context =
  { tx0 : tx
  ; read_edn : string -> query_form
  ; query_value_of_form : query_form -> value
  ; datom : ?tx:tx -> ?added:bool -> e:entity_id -> a:attr -> v:value -> unit -> datom
  ; validate_schema : schema -> schema
  ; empty_db : ?schema:schema -> unit -> db
  ; max_eid_in_value : int -> value -> int
  ; resolve_value_for_attr :
      db ->
      attr ->
      datom list ->
      tx ->
      int ->
      (string * entity_id) list ->
      value ->
      value * int * (string * entity_id) list
  ; init_db : ?schema:schema -> datom list -> db
  }

val attr_of_edn_key : query_form -> attr
val tx_attr_of_edn_key : query_form -> attr
val tx_op_name_of_edn_form : query_form -> string
val is_edn_attr_key : query_form -> bool
val keyword_name_of_form : query_form -> string
val entity_ref_of_edn_form : context -> query_form -> entity_ref
val tx_data_of_edn_form : context -> query_form -> tx_op list
val parse_tx_data_string : context -> string -> tx_op list
val schema_of_edn_form : context -> query_form -> schema
val schema_of_edn_string : context -> string -> schema
val db_from_reader_form : context -> query_form -> db
val db_from_reader_string : context -> string -> db
