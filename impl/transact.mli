open Datascript_types

type context =
  { validate_entity_id : int -> entity_id
  ; entid_in_datoms : db -> datom list -> attr -> value -> entity_id option
  ; ident_attr : attr
  ; allocate_entity_id : entity_id -> entity_id
  ; lookup_ref_entity_id_in_datoms : strict_missing:bool -> db -> datom list -> attr -> value -> entity_id option
  ; unresolved_lookup_ref_message : attr -> value -> string
  ; normalize_value : value -> value
  ; is_ref_attr : db -> attr -> bool
  ; is_reverse_ref : attr -> bool
  ; reverse_ref : attr -> attr
  ; cardinality : db -> attr -> cardinality
  ; max_eid_in_value : int -> value -> int
  }

val remember_tempid : (string * entity_id) list -> string -> entity_id -> (string * entity_id) list
val remember_current_tx : (string * entity_id) list -> tx -> (string * entity_id) list
val ensure_current_tx_tempid : (string * entity_id) list -> tx -> (string * entity_id) list
val is_current_tx_alias : string -> bool
val remember_current_tx_alias : (string * entity_id) list -> tx -> string -> (string * entity_id) list
val resolve_entity_ref : context -> db -> datom list -> tx -> entity_id -> (string * entity_id) list -> entity_ref -> entity_id * entity_id * (string * entity_id) list
val resolve_value : context -> db -> datom list -> tx -> entity_id -> (string * entity_id) list -> value -> value * entity_id * (string * entity_id) list
val attr_name_of_value : value -> attr option
val entity_ref_of_ref_attr_value : value -> entity_ref option
val ref_attr_for_value_resolution : context -> db -> attr -> attr option
val resolve_value_for_attr : context -> db -> attr -> datom list -> tx -> entity_id -> (string * entity_id) list -> value -> value * entity_id * (string * entity_id) list
val attr_expands_collection : context -> db -> attr -> bool
val ref_lookup_collection_value : value -> bool
val resolve_existing_entity_ref : context -> db -> datom list -> tx -> entity_id -> (string * entity_id) list -> entity_ref -> entity_id * entity_id * (string * entity_id) list
val resolve_optional_existing_entity_ref : context -> db -> datom list -> tx -> entity_id -> (string * entity_id) list -> entity_ref -> entity_id option * entity_id * (string * entity_id) list
val resolve_tx_value_for_attr : context -> db -> attr -> datom list -> tx -> entity_id -> (string * entity_id) list -> tx_value -> tx_value * entity_id * (string * entity_id) list
val resolve_optional_value_for_attr : context -> db -> attr -> datom list -> tx -> entity_id -> (string * entity_id) list -> value option -> value option * entity_id * (string * entity_id) list
val resolve_entity_attrs : context -> db -> datom list -> tx -> entity_id -> (string * entity_id) list -> (attr * tx_value) list -> (attr * tx_value) list * entity_id * (string * entity_id) list
val remap_value_ref : context -> entity_id -> entity_id -> value -> value
val remap_datom_entity : context -> entity_id -> entity_id -> datom -> datom
val remap_resolved_tx_value : context -> entity_id -> entity_id -> tx_value -> tx_value
val remap_tempid_entity : entity_id -> entity_id -> (string * entity_id) list -> (string * entity_id) list
