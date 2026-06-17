include module type of Datascript_types

module Conn : sig
  type t

  type creation_context =
    { empty_db : ?schema:schema -> ?storage:storage -> unit -> db
    ; init_db : ?schema:schema -> ?storage:storage -> datom list -> db
    ; store : ?storage:storage -> db -> unit
    }

  type schema_context =
    { store : ?storage:storage -> db -> unit
    ; with_schema : db -> schema -> db
    }

  type restore_context =
    { restore : storage -> db option
    ; restore_tail_groups : storage -> datom list list
    }

  type transact_context =
    { store : ?storage:storage -> db -> unit
    ; store_tail : storage -> datom list list -> unit
    ; storage_tail_datom_count : datom list list -> int
    ; storage_tail_compaction_threshold : int
    ; transact : tx_meta:tx_meta -> db -> tx_op list -> tx_report
    }

  type reset_context =
    { store : ?storage:storage -> db -> unit
    ; datoms : db -> datom list
    }

  val create : creation_context -> ?schema:schema -> ?storage:storage -> unit -> t
  val from_db : creation_context -> db -> t
  val from_datoms : creation_context -> ?schema:schema -> ?storage:storage -> datom list -> t
  val db : t -> db
  val is_conn : t -> bool
  val listen : t -> string -> (tx_report -> unit) -> string
  val listen_auto : t -> (tx_report -> unit) -> string
  val unlisten : t -> string -> unit
  val reset_schema : schema_context -> t -> schema -> db
  val restore : restore_context -> storage -> t option
  val transact : transact_context -> ?tx_meta:tx_meta -> t -> tx_op list -> tx_report
  val reset : reset_context -> ?tx_meta:tx_meta -> t -> db -> db
end

type conn = Conn.t

module Db : sig
  val tx0 : tx
  val datom : ?tx:tx -> ?added:bool -> e:entity_id -> a:attr -> v:value -> unit -> datom
  val is_datom : datom -> bool
  val value_equal : value -> value -> bool
  val same_fact : datom -> datom -> bool
  val diff : db -> db -> datom list * datom list * datom list
  val squuid : ?msec:int -> unit -> value
  val squuid_time_millis : value -> int
end

module Entity : sig
  type context =
    { datoms_by_entity : db -> entity_id -> datom list
    ; all_datoms : db -> datom list
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
  val entity_db : entity -> db
  val is_entity : entity -> bool
  val entity_equal : entity -> entity -> bool
  val entity_hash : entity -> int
  val touch : context -> entity -> entity
end

module Lru : sig
  type ('key, 'value) t
  type ('key, 'value) cache

  val create : int -> ('key, 'value) t
  val assoc : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t
  val find : 'key -> ('key, 'value) t -> 'value option
  val cache : int -> ('key, 'value) cache
  val cache_get : ('key, 'value) cache -> 'key -> (unit -> 'value) -> 'value
end

module Schema : sig
  val validate_schema : schema -> schema
  val schema_attr_by_name : schema -> attr -> schema_attr option
  val schema_attr_is_ref : schema -> attr -> bool
  val schema_attr_is_tuple : schema_attr option -> bool
  val schema_attr_is_avet_accessible : schema -> attr -> bool
  val schema_has_no_history : schema -> attr -> bool
  val split_namespaced_attr : attr -> string option * string
  val join_namespaced_attr : string option -> string -> attr
  val is_reverse_ref : attr -> bool
  val reverse_ref : attr -> attr
end

val tx0 : tx
val datom : ?tx:tx -> ?added:bool -> e:entity_id -> a:attr -> v:value -> unit -> datom
val is_datom : datom -> bool
val empty_db : ?schema:schema -> ?storage:storage -> unit -> db
val is_db : db -> bool
val init_db : ?schema:schema -> ?storage:storage -> datom list -> db
val history : db -> db
val is_history : db -> bool
val filter : db -> (db -> datom -> bool) -> db
val is_filtered : db -> bool
val unfiltered_db : db -> db
val serializable : db -> serializable_db
val from_serializable : serializable_db -> db
val db_from_reader_string : string -> db
val memory_storage : unit -> storage
val file_storage : string -> storage
val store : ?storage:storage -> db -> unit
val store_tail : storage -> datom list list -> unit
val restore : storage -> db option
val db_with_tail : db -> datom list list -> db
val storage : db -> storage option
val addresses : db list -> storage_address list
val settings : db -> (attr * value) list
val storage_addresses : storage -> storage_address list
val collect_garbage : storage -> unit
val diff : db -> db -> datom list * datom list * datom list
val squuid : ?msec:int -> unit -> value
val squuid_time_millis : value -> int
val create_conn : ?schema:schema -> ?storage:storage -> unit -> conn
val conn_from_db : db -> conn
val conn_from_datoms : ?schema:schema -> ?storage:storage -> datom list -> conn
val restore_conn : storage -> conn option
val conn_db : conn -> db
val db : conn -> db
val is_conn : conn -> bool
val listen : conn -> string -> (tx_report -> unit) -> string
val listen_bang : conn -> string -> (tx_report -> unit) -> string
val listen_auto : conn -> (tx_report -> unit) -> string
val listen_bang_auto : conn -> (tx_report -> unit) -> string
val unlisten : conn -> string -> unit
val unlisten_bang : conn -> string -> unit
val reset_conn : ?tx_meta:tx_meta -> conn -> db -> db
val reset_conn_bang : ?tx_meta:tx_meta -> conn -> db -> db
val reset_schema : conn -> schema -> db
val reset_schema_bang : conn -> schema -> db
val schema : db -> schema
val with_schema : db -> schema -> db
val schema_of_edn_string : string -> schema
val is_reverse_ref : attr -> bool
val reverse_ref : attr -> attr
val parse_tx_data_string : string -> tx_op list
val db_with : tx_op list -> db -> db
val db_with_string : string -> db -> db
val transact : ?tx_meta:tx_meta -> db -> tx_op list -> tx_report
val transact_string : ?tx_meta:tx_meta -> db -> string -> tx_report
val with_tx : ?tx_meta:tx_meta -> db -> tx_op list -> tx_report
val with_tx_string : ?tx_meta:tx_meta -> db -> string -> tx_report
val transact_conn : ?tx_meta:tx_meta -> conn -> tx_op list -> tx_report
val transact_conn_string : ?tx_meta:tx_meta -> conn -> string -> tx_report
val transact_bang : ?tx_meta:tx_meta -> conn -> tx_op list -> tx_report
val transact_bang_string : ?tx_meta:tx_meta -> conn -> string -> tx_report
val transact_async : ?tx_meta:tx_meta -> conn -> tx_op list -> tx_report
val transact_async_string : ?tx_meta:tx_meta -> conn -> string -> tx_report
val tempid : ?part:string -> ?value:int -> unit -> entity_ref
val resolve_tempid : ?db:db -> (string * entity_id) list -> string -> entity_id option
val entity : db -> entity_ref -> entity option
val entity_attr : entity -> attr -> tx_value option
val entity_db : entity -> db
val is_entity : entity -> bool
val entity_equal : entity -> entity -> bool
val entity_hash : entity -> int
val touch : entity -> entity
val entid : db -> attr -> value -> entity_id option
val entid_ref : db -> entity_ref -> entity_id option
val read_edn : string -> query_form
val parse_binding : query_form -> input_binding
val parse_in : query_form -> query_input list
val parse_with : query_form -> string list
val parse_find : query_form -> query_return * find_spec list
val parse_pull_pattern : db -> query_form -> pull_selector list
val parse_pull_pattern_string : db -> string -> pull_selector list
val pull : ?visitor:(pull_visit -> unit) -> db -> pull_selector list -> entity_ref -> pulled_entity option
val pull_string : ?visitor:(pull_visit -> unit) -> db -> string -> entity_ref -> pulled_entity option
val pull_many : ?visitor:(pull_visit -> unit) -> db -> pull_selector list -> entity_ref list -> pulled_entity option list
val pull_many_string : ?visitor:(pull_visit -> unit) -> db -> string -> entity_ref list -> pulled_entity option list
val parse_query : query_form -> query
val parse_query_string : string -> query
val parse_query_return : query_form -> query_return * query
val parse_query_return_string : string -> query_return * query
val parse_query_return_map : query_form -> query_return * query_return_map option * query
val parse_query_return_map_string : string -> query_return * query_return_map option * query
val q : ?inputs:query_arg list -> db -> query -> query_result list list
val q_string : ?inputs:query_arg list -> db -> string -> query_result list list
val q_with : ?inputs:query_arg list -> db -> string list -> query -> query_result list list
val q_with_string :
  ?inputs:query_arg list -> db -> string list -> string -> query_result list list
val q_sources :
  ?inputs:query_arg list -> db -> (string * query_source) list -> query -> query_result list list
val q_sources_string :
  ?inputs:query_arg list ->
  db ->
  (string * query_source) list ->
  string ->
  query_result list list
val q_return : ?inputs:query_arg list -> db -> query_return -> query -> query_output
val q_return_string : ?inputs:query_arg list -> db -> string -> query_output
val q_return_map :
  ?inputs:query_arg list -> db -> query_return -> query_return_map -> query -> query_output
val q_return_map_string : ?inputs:query_arg list -> db -> string -> query_output
val datoms : db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val datoms_ref : db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val find_datom : db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom option
val find_datom_ref : db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom option
val seek_datoms : db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val seek_datoms_ref : db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val rseek_datoms : db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val rseek_datoms_ref : db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
val index_range : db -> attr -> ?start:value -> ?stop:value -> unit -> datom list
