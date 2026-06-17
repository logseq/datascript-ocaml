open Datascript_types

val tx0 : tx
val datom : ?tx:tx -> ?added:bool -> e:entity_id -> a:attr -> v:value -> unit -> datom
val is_datom : datom -> bool
val value_equal : value -> value -> bool
val same_fact : datom -> datom -> bool

type index_context =
  { is_avet_accessible : db -> attr -> bool
  ; resolve_entity_ref : db -> entity_ref -> entity_id
  ; resolve_value_for_optional_attr : db -> attr option -> value -> value
  ; resolve_value_for_attr : db -> attr -> value -> value
  ; compare_value : value -> value -> int
  ; first_nonzero : int list -> int
  }

val indexed_attr_required_message : attr -> string
val validate_index_access : index_context -> db -> index -> attr option -> unit
val datoms : index_context -> db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val datoms_ref : index_context -> db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val find_datom : index_context -> db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom option
val find_datom_ref : index_context -> db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom option
val seek_datoms : index_context -> db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val seek_datoms_ref : index_context -> db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val rseek_datoms : index_context -> db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val rseek_datoms_ref : index_context -> db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val index_range : index_context -> db -> attr -> ?start:value -> ?stop:value -> unit -> datom list

val hash : db -> int
val hash_cache_size : unit -> int
val diff : db -> db -> datom list * datom list * datom list
val squuid : ?msec:int -> unit -> value
val squuid_time_millis : value -> int
