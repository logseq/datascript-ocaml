open Datascript_types

val encode_datom_key : index -> datom -> string
val encode_index_attr_value_prefix : index -> string -> value -> string
val encode_tave_tx_prefix : ?attr:string -> tx -> string
val decode_datom_key : index -> string -> datom
val encode_datom_value : datom -> string
val decode_datom_value : string -> datom
val decode_index_entry : index -> string -> string -> datom
val encode_index_value : index -> datom -> string

val compare_encoded_keys : index -> string -> string -> int
val avet_key_attr : string -> string
val avet_key_value : string -> value
val decode_avet_key_at : attr -> string -> datom
val tave_key_tx : string -> tx

val encode_schema : schema -> string
val decode_schema : string -> schema
val encode_datoms : datom list -> string
val decode_datoms : string -> datom list
