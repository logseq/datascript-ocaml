open Datascript_types

type context =
  { is_unique : db -> attr -> bool
  ; entid_in_datoms : db -> datom list -> attr -> value -> entity_id option
  ; visible_datoms : db -> datom list
  ; value_to_string : value -> string
  }

val unresolved_message : context -> attr -> value -> string
val non_unique_message : context -> attr -> value -> string
val entity_id_in_datoms : ?strict_missing:bool -> context -> db -> datom list -> attr -> value -> entity_id option
val entity_id : ?strict_missing:bool -> context -> db -> attr -> value -> entity_id option
