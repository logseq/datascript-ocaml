open Datascript_types

type context =
  { datoms_by_entity : db -> entity_id -> datom Seq.t
  ; datoms_by_avet_ref : db -> attr -> entity_id -> datom Seq.t
  ; all_datoms : db -> datom Seq.t
  ; compare_value : value -> value -> int
  ; cardinality : db -> attr -> cardinality
  ; is_ref_attr : db -> attr -> bool
  ; is_component : db -> attr -> bool
  ; reverse_ref : attr -> attr
  ; is_reverse_ref : attr -> bool
  ; entity_id_of_ref : db -> entity_ref -> entity_id option
  }

val entity : context -> db -> entity_ref -> entity option
val entity_attr_raw : entity -> attr -> tx_value option
val entity_attr : context -> entity -> attr -> tx_value option
val entity_attrs : entity -> (attr * tx_value) list
val entity_db : entity -> db
val is_entity : entity -> bool
val entity_equal : entity -> entity -> bool
val entity_hash : entity -> int
val touch : context -> entity -> entity
