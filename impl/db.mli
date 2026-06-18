open Datascript_types

val tx0 : tx
val datom : ?tx:tx -> ?added:bool -> e:entity_id -> a:attr -> v:value -> unit -> datom
val is_datom : datom -> bool

type core_context =
  { next_db_uid : unit -> int
  }

val max_entity_id : int
val max_allocatable_entity_id : int
val validate_entity_id : int -> entity_id
val max_eid_with_entity_id : int -> entity_id -> entity_id
val refresh_identity : core_context -> db -> db
val max_eid_in_value : int -> value -> int
val normalize_datom_for_schema : schema -> datom -> datom
val refresh_indexes : db -> db
val refresh_indexes_with_added_datoms : db -> datom list -> db
val with_datoms : db -> datom list -> db
val empty_db : core_context -> ?schema:schema -> ?storage:storage -> unit -> db
val empty : core_context -> db -> db
val history_datoms_for_schema : schema -> datom list -> datom list
val init_db : core_context -> ?schema:schema -> ?storage:storage -> datom list -> db
val history : core_context -> db -> db
val is_history : db -> bool
val visible_active_datoms : db -> datom list
val is_filtered : db -> bool
val unfiltered : core_context -> db -> db
val filter : core_context -> db -> (db -> datom -> bool) -> db

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
val datoms : index_context -> db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom Seq.t
val datoms_ref : index_context -> db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom Seq.t
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
