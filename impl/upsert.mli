open Datascript_types

type resolution = attr * value * entity_id

type context =
  { is_unique_identity : db -> attr -> bool
  ; entid_in_datoms : db -> datom list -> attr -> value -> entity_id option
  ; value_to_string : value -> string
  }

val lookup_ref_string : context -> attr -> value -> string
val conflicting_upserts_message : context -> resolution -> resolution -> string
val explicit_conflict_message : context -> attr -> value -> entity_id -> entity_id -> string
val identity_resolutions : context -> db -> datom list -> (attr * tx_value) list -> resolution list
val conflicting_identity_resolution : resolution list -> (resolution * resolution) option
val validate_explicit_target : context -> db -> datom list -> entity_id -> (attr * tx_value) list -> unit
val entity_unique_identity : context -> db -> datom list -> (attr * tx_value) list -> entity_id option
