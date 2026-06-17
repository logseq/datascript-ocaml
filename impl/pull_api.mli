open Datascript_types

type context =
  { compare_value : value -> value -> int
  ; entity : db -> entity_ref -> entity option
  ; entity_attr_raw : entity -> attr -> tx_value option
  ; datoms_by_avet_ref : db -> attr -> entity_id -> datom list
  ; is_component : db -> attr -> bool
  ; is_reverse_ref : attr -> bool
  ; reverse_ref : attr -> attr
  ; entity_id_of_ref : db -> entity_ref -> entity_id option
  }

val pull : ?visitor:(pull_visit -> unit) -> context -> db -> pull_selector list -> entity_ref -> pulled_entity option
val pull_many : ?visitor:(pull_visit -> unit) -> context -> db -> pull_selector list -> entity_ref list -> pulled_entity option list
