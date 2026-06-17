include module type of Datascript_types

module Built_ins : sig
  val map_get_value : (value * value) list -> value -> value option
  val value_get : value -> value -> value option
  val value_count : value -> int option
  val value_has_count : int -> value -> bool
  val value_is_not_empty : value -> bool
  val matches_value_predicate : value_predicate -> value -> bool
  val matches_numeric_predicate : numeric_predicate -> value -> bool
  val matches_comparison_predicate : comparison_predicate -> int -> bool
  val comparison_chain_matches : comparison_predicate -> value list -> bool
  val all_values_equal : value list -> bool
  val eval_arithmetic : arithmetic_op -> value list -> value option
  val normalized_comparison : int -> int
  val extremum_value : extremum_op -> value -> value list -> value
  val string_starts_with : string -> string -> bool
  val string_ends_with : string -> string -> bool
  val string_index_of : string -> string -> int option
  val string_includes : string -> string -> bool
  val string_last_index_of : string -> string -> int option
  val is_ascii_whitespace : char -> bool
  val string_is_blank : string -> bool
  val split_string : string -> string -> string list
  val split_string_limited : string -> string -> int -> string list
  val split_lines : string -> string list
  val string_of_query_value : value -> string
  val escaped_string_literal : string -> string
  val print_query_value : readably:bool -> value -> string
  val print_query_values : readably:bool -> value list -> string
  val collection_string_values : value -> string list option
  val replace_string : ?first_only:bool -> string -> string -> string -> string
  val compile_regex : string -> Str.regexp
  val replace_regex : ?first_only:bool -> string -> string -> string -> string
  val string_escape_replacement : (value * value) list -> char -> string option
  val escape_string : string -> (value * value) list -> string
  val regex_pattern_of_result : query_result -> string option
  val regex_find : string -> string -> string option
  val regex_matches : string -> string -> string option
  val regex_seq : string -> string -> string list
  val split_regex : string -> string -> string list
  val split_regex_limited : string -> string -> int -> string list
  val reverse_string : string -> string
  val capitalize_string : string -> string
  val trim_left_with : (char -> bool) -> string -> string
  val trim_right_with : (char -> bool) -> string -> string
  val trim_with : (char -> bool) -> string -> string
  val is_newline : char -> bool
  val aggregate_result : aggregate -> query_result list -> query_result
  val value_is_truthy : value -> bool
  val boolean_and_value : value list -> value
  val boolean_or_value : value list -> value
  val split_at : int -> 'a list -> 'a list * 'a list
  val values_equal : value -> value -> bool
  val type_keyword_of_value : value -> string
  val value_contains : value -> value -> bool
  val range_values : int -> int -> int -> int list
end

module Data_readers : sig
  val attr_of_edn_key : query_form -> attr
  val tx_attr_of_edn_key : query_form -> attr
  val tx_op_name_of_edn_form : query_form -> string
  val is_edn_attr_key : query_form -> bool
  val keyword_name_of_form : query_form -> string
  val entity_ref_of_edn_form : query_form -> entity_ref
  val tx_data_of_edn_form : query_form -> tx_op list
  val parse_tx_data_string : string -> tx_op list
  val schema_of_edn_form : query_form -> schema
  val schema_of_edn_string : string -> schema
  val db_from_reader_form : query_form -> db
  val db_from_reader_string : string -> db
end

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
  val datoms : db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
  val datoms_ref : db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
  val find_datom : db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom option
  val find_datom_ref : db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom option
  val seek_datoms : db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
  val seek_datoms_ref : db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
  val rseek_datoms : db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
  val rseek_datoms_ref : db -> index -> ?e:entity_ref -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom list
  val index_range : db -> attr -> ?start:value -> ?stop:value -> unit -> datom list
  val hash : db -> int
  val hash_cache_size : unit -> int
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

module Lookup_refs : sig
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

module Serialize : sig
  type context =
    { next_db_uid : unit -> int
    ; validate_schema : schema -> schema
    ; normalize_datom_for_schema : schema -> datom -> datom
    ; refresh_db_indexes : db -> db
    }

  val serializable : db -> serializable_db
  val from_serializable : context -> serializable_db -> db
end

module Storage : sig
  type store_context =
    { serializable : db -> serializable_db
    }

  type tail_context =
    { apply_group : db -> datom list -> db
    }

  type restore_context =
    { from_serializable : serializable_db -> db
    ; db_with_tail : db -> datom list list -> db
    }

  val root_address : storage_address
  val tail_address : storage_address
  val memory_storage : unit -> storage
  val file_storage : string -> storage
  val store : store_context -> ?storage:storage -> db -> unit
  val store_tail : storage -> datom list list -> unit
  val tail_compaction_threshold : int
  val tail_datom_count : datom list list -> int
  val restore_root_snapshot : storage -> serializable_db option
  val restore_tail_groups : storage -> datom list list
  val db_with_tail : tail_context -> db -> datom list list -> db
  val restore : restore_context -> storage -> db option
  val storage_addresses : storage -> storage_address list
  val storage : db -> storage option
  val addresses : db list -> storage_address list
  val settings : db -> (attr * value) list
  val collect_garbage : storage -> unit
end

module Util : sig
  val list_equal_by : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
  val entity_ref_equal : entity_ref -> entity_ref -> bool
  val value_equal : value -> value -> bool
  val split_keyword : string -> string * string
  val compare_list_with : ('a -> 'a -> int) -> 'a list -> 'a list -> int
  val compare_option_with : ('a -> 'a -> int) -> 'a option -> 'a option -> int
  val compare_value : value -> value -> int
  val first_nonzero : int list -> int
  val compare_datom : index -> datom -> datom -> int
  val normalize_value : value -> value
  val normalize_datom_value : datom -> datom
end

module Parser : sig
  val read_edn : string -> query_form
  val section_forms : query_form -> query_form list
  val query_form_section : string -> (query_form * query_form) list -> query_form option
  val query_form_sections : query_form list -> (query_form * query_form) list
  val query_form_map : query_form -> (query_form * query_form) list
  val query_form_sequence : query_form -> query_form list option
  val query_symbol_name : string -> string
  val query_callable_name : string -> string
  val is_plain_input_symbol : string -> bool
  val is_query_input_symbol : string -> bool
  val query_input_name : string -> string
  val query_source_name : string -> string
  val is_query_source_symbol : string -> bool
  val is_plain_rule_symbol : string -> bool
  val aggregate_of_symbol : string -> aggregate option
  val amount_aggregate_of_symbol : string -> int -> aggregate option
  val dynamic_amount_aggregate_of_symbol : string -> string -> aggregate option
  val parse_find_arg : query_form -> query_term
  val parse_find_args : query_form list -> query_term list
  val parse_output_var : query_form -> string
  val parse_output_vars : query_form -> string list
  val parse_flat_output_vars : query_form -> string list option
  val parse_collection_output_var : query_form -> string option
  val parse_relation_output_vars : query_form -> string list option
  val nonempty_input_vars : string -> string list -> string list
  val input_relation_vars : query_form -> query_form list option
  val input_var_of_form : query_form -> string option
  val flat_input_vars : query_form list -> string list option
  val parse_nested_input_binding : query_form -> input_binding
  val nested_relation_binding : query_form -> input_binding list option
  val parse_input_binding : query_form -> query_input option
  val parse_inputs : query_form option -> query_input list
  val input_declares_rules_var : query_form option -> bool
  val ensure_distinct_input_rules_var : query_form option -> unit
  val parse_with_var : query_form -> string
  val parse_with_section : query_form option -> string list
  val parse_return_map_labels : string -> query_form -> string list
  val parse_return_map_section : (query_form * query_form) list -> query_return_map option
  val lookup_ref_of_form : query_form -> (attr * value) option
  val parse_pattern_term :
    ?entity_position:bool ->
    ?attr_position:bool ->
    ?lookup_ref_position:bool ->
    ?source_position:bool ->
    query_form ->
    query_term
  val comparison_predicate_of_symbol : string -> comparison_predicate option
  val value_predicate_of_symbol : string -> value_predicate option
  val numeric_predicate_of_symbol : string -> numeric_predicate option
  val boolean_predicate_of_symbol : string -> boolean_predicate option
  val unary_string_predicate_clause_of_symbol : string -> (query_term -> query_clause) option
  val binary_string_predicate_clause_of_symbol : string -> (query_term -> query_term -> query_clause) option
  val equality_predicate_of_symbol : string -> equality_predicate option
  val arithmetic_op_of_symbol : string -> arithmetic_op option
  val query_attr_name : query_form -> attr
  val parse_data_pattern_clause : query_form list -> query_clause
  val parse_rule_expr : string -> query_form list -> string * query_term list
  val parse_source_pattern_clause : string -> query_form list -> query_clause
  val parse_missing_clause : query_form list -> query_clause
  val parse_get_else_clause : query_form list -> string -> query_clause
  val parse_two_output_vars : query_form -> string * string
  val parse_get_some_clause : query_form list -> query_form -> query_clause
  val parse_get_clause : query_form list -> string -> query_clause
  val parse_core_value_function : string -> query_form list -> string -> query_clause
  val parse_collection_function : string -> query_form list -> string -> query_clause
  val parse_flat_value_function : string -> query_form list -> string list -> query_clause
  val ground_values_of_form : query_form -> value list
  val ground_relation_rows_of_form : query_form -> value list list
  val dynamic_ground_term : query_form -> query_term option
  val parse_ground_function : query_form list -> query_form -> query_clause
  val parse_value_metadata_function : string -> query_form list -> string -> query_clause
  val parse_string_transform_function : string -> query_form list -> string -> query_clause
  val parse_binding : query_form -> input_binding
  val parse_in : query_form -> query_input list
  val parse_with : query_form -> string list
  val parse_find : query_form -> query_return * find_spec list
  val parse_clause : query_form -> query_clause
  val parse_rules : query_form -> query_rule list
  val parse_query : query_form -> query
  val parse_query_string : string -> query
  val parse_query_return : query_form -> query_return * query
  val parse_query_return_string : string -> query_return * query
  val parse_query_return_map : query_form -> query_return * query_return_map option * query
  val parse_query_return_map_string : string -> query_return * query_return_map option * query
end

module Pull_parser : sig
  val parse_pattern : db -> query_form -> pull_selector list
  val parse_pattern_string : db -> string -> pull_selector list
end

module Pull_api : sig
  val pull : ?visitor:(pull_visit -> unit) -> db -> pull_selector list -> entity_ref -> pulled_entity option
  val pull_string : ?visitor:(pull_visit -> unit) -> db -> string -> entity_ref -> pulled_entity option
  val pull_many : ?visitor:(pull_visit -> unit) -> db -> pull_selector list -> entity_ref list -> pulled_entity option list
  val pull_many_string : ?visitor:(pull_visit -> unit) -> db -> string -> entity_ref list -> pulled_entity option list
end

module Upsert : sig
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
end

val tx0 : tx
val datom : ?tx:tx -> ?added:bool -> e:entity_id -> a:attr -> v:value -> unit -> datom
val is_datom : datom -> bool
val empty_db : ?schema:schema -> ?storage:storage -> unit -> db
val empty : db -> db
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
val db_hash : db -> int
val db_hash_cache_size : unit -> int
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

module Query : sig
  type query_callables =
    { callable_predicates : (string * (query_result list -> bool)) list
    ; callable_functions : (string * (query_result list -> query_result list option)) list
    ; callable_aggregates : (string * (query_result list -> query_result)) list
    ; callable_aliases : (string * string) list
    }

  type result_resolution_context =
    { validate_entity_id : int -> entity_id
    ; resolve_query_value : value -> value option
    ; lookup_ref_entity_id : attr -> value -> entity_id option
    }

  type match_context =
    { result_resolution_context : result_resolution_context
    ; ident_entity_id : string -> entity_id option
    ; unresolved_lookup_ref_message : attr -> value -> string
    ; value_equal : value -> value -> bool
    ; coerce_tuple_lookup_value : attr -> value -> value
    }

  val empty_query_callables : query_callables
  val q : ?inputs:query_arg list -> db -> query -> query_result list list
  val q_string : ?inputs:query_arg list -> db -> string -> query_result list list
  val q_with : ?inputs:query_arg list -> db -> string list -> query -> query_result list list
  val q_with_string :
    ?inputs:query_arg list -> db -> string list -> string -> query_result list list
  val q_sources :
    ?inputs:query_arg list ->
    db ->
    (string * query_source) list ->
    query ->
    query_result list list
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
  val return_map_label_count : query_return_map -> int
  val return_map_name : query_return_map -> string
  val validate_query_return_map :
    query_return -> query_return_map option -> query -> query_return_map option
  val has_aggregates : find_spec list -> bool
  val collect_find_vars : (string * query_result) list -> string list -> query_result list option
  val group_by_key :
    (query_result list * (string * query_result) list) list ->
    (query_result list * (string * query_result) list list) list
  val grouping_vars_of_find : find_spec list -> string list
  val aggregate_amount_value : string -> (string * query_result) list -> int
  val resolve_dynamic_aggregate : aggregate -> (string * query_result) list list -> aggregate
  val aggregate_param_vars : aggregate -> string list
  val aggregate_callable_vars : aggregate -> string list
  val split_aggregate_terms : query_term list -> query_term list * query_term
  val aggregate_input_values : aggregate -> query_result list -> query_result list -> query_result list
  val resolve_callable_name : query_callables -> string -> string
  val callable_predicate : query_callables -> string -> (query_result list -> bool) option
  val callable_function : query_callables -> string -> (query_result list -> query_result list option) option
  val callable_aggregate : query_callables -> string -> (query_result list -> query_result) option
  val has_callable : query_callables -> string -> bool
  val alias_callable : query_callables -> string -> string -> query_callables
  val resolve_callable_aggregate : query_callables -> aggregate -> aggregate
  val result_of_datom_e : datom -> query_result
  val result_of_datom_a : datom -> query_result
  val result_of_datom_v : datom -> query_result
  val result_of_datom_tx : datom -> query_result
  val result_of_datom_op : datom -> query_result
  val result_of_ref : query_result -> query_result
  val entity_id_of_resolved_query_result :
    validate_entity_id:(int -> entity_id) -> query_result option -> entity_id option
  val resolved_query_result : result_resolution_context -> query_result -> query_result option
  val lookup_ref_entity_id_of_value : result_resolution_context -> value -> entity_id option
  val query_result_entity_id : result_resolution_context -> query_result -> entity_id option
  val query_results_equivalent : result_resolution_context -> query_result -> query_result -> bool
  val bind_var :
    result_resolution_context ->
    string ->
    query_result ->
    (string * query_result) list ->
    (string * query_result) list option
  val result_matches_entity : result_resolution_context -> entity_id -> query_result -> bool
  val match_query_term :
    match_context ->
    query_term ->
    query_result ->
    (string * query_result) list ->
    (string * query_result) list option
  val match_value_term_for_datom_attr :
    match_context ->
    (string * query_result) list ->
    query_term ->
    datom ->
    (string * query_result) list option
  val match_pattern_clause :
    match_context ->
    (string * query_result) list ->
    query_term ->
    query_term ->
    query_term ->
    datom ->
    (string * query_result) list option
  val match_pattern_tx_clause :
    match_context ->
    (string * query_result) list ->
    query_term ->
    query_term ->
    query_term ->
    query_term ->
    datom ->
    (string * query_result) list option
  val match_reverse_pattern_clause :
    match_context ->
    (string * query_result) list ->
    query_term ->
    attr ->
    query_term ->
    datom ->
    (string * query_result) list option
  val query_callables_of_inputs : query_input list -> query_callables
  val query_rules_of_inputs : query_input list -> query_rule list
  val matching_rules : query_rule list -> string -> int -> query_rule list
  val matching_rules_exn : query_rule list -> string -> int -> query_rule list
  val project_binding : string list -> (string * query_result) list -> (string * query_result) list
  val rule_invocation_callables :
    query_callables -> (string * query_result) list -> query_rule -> query_term list -> query_callables
  val vars_of_query_term : query_term -> string list
  val vars_of_query_terms : query_term list -> string list
  val vars_of_clause : query_clause -> string list
  val named_source : string -> string list
  val sources_of_query_term : query_term -> string list
  val sources_of_query_terms : query_term list -> string list
  val sources_of_optional_query_term : query_term option -> string list
  val sources_of_clause : query_clause -> string list
  val sources_of_find_spec : find_spec -> string list
  val has_rule_clause : query_clause -> bool
  val rule_names : query_rule list -> string list
  val resolve_dynamic_rule_clause : string list -> query_clause -> query_clause
  val resolve_dynamic_rule : string list -> query_rule -> query_rule
  val find_spec_uses_default_source : find_spec -> bool
  val clause_uses_default_source : query_clause -> bool
  val infer_default_inputs :
    query_form option -> find_spec list -> query_clause list -> query_input list -> query_input list
  val query_term_vars : query_term list -> string list
  val vars_of_find_spec : find_spec -> string list
  val vars_of_input_binding : input_binding -> string list
  val vars_of_input : query_input -> string list
  val source_of_input : query_input -> string option
  val ensure_distinct_input_vars : query_input list -> unit
  val ensure_distinct_input_sources : query_input list -> unit
  val format_query_vars : string list -> string
  val format_source_vars : string list -> string
  val validate_query : query -> query
  val query_input_var_label : string -> string
  val query_term_string : value_to_string:(value -> string) -> query_term -> string
  val query_output_var_string : string -> string
  val query_output_binding_string : string list -> string
  val query_call_string : value_to_string:(value -> string) -> string -> query_term list -> string
  val numeric_predicate_symbol : numeric_predicate -> string
  val arithmetic_op_symbol : arithmetic_op -> string
  val query_clause_string : value_to_string:(value -> string) -> query_clause -> string
  val query_not_clause_string : value_to_string:(value -> string) -> query_clause list -> string
  val query_or_clause_string : value_to_string:(value -> string) -> query_clause list list -> string
  val query_or_join_vars_string : string list -> string list -> string
  val query_or_join_clause_string :
    value_to_string:(value -> string) -> string list -> string list -> query_clause list list -> string
  val query_var_set_string : string list -> string
  val query_var_sets_string : string list list -> string
  val unbound_vars_of_terms : (string * query_result) list -> query_term list -> string list
  val ensure_query_terms_bound : (string * query_result) list -> query_term list -> string -> unit
  val ensure_not_has_outer_binding :
    value_to_string:(value -> string) -> (string * query_result) list -> query_clause list -> unit
  val vars_of_branch : query_clause list -> string list
  val free_vars_of_branch : string list -> query_clause list -> string list
  val ensure_or_branch_vars_match :
    value_to_string:(value -> string) ->
    (string * query_result) list ->
    query_clause list list ->
    unit
  val ensure_join_vars_bound : (string * query_result) list -> string list -> unit
  val ensure_join_vars_bound_in_clause : (string * query_result) list -> string list -> string -> unit
  val ensure_or_join_branches_cover_listed_vars :
    (string * query_result) list -> string list -> query_clause list list -> unit
  val clause_calls_rule : string -> query_clause -> bool
  val matching_rules_for_call :
    (string * string * query_result option list) list ->
    string * string * query_result option list ->
    query_rule list ->
    string ->
    int ->
    query_rule list
  val query_input_binding_string : input_binding -> string
  val query_input_decl_binding_string : query_input -> string
  val query_input_binding_label : query_input -> string
  val query_input_consumes_argument : consume_rules:bool -> query_input -> bool
  val values_of_collection_result : query_result -> query_result list option
  val row_of_collection_result : query_result -> query_result list
  val row_of_scalar_sequence : query_result -> query_result list
  val rows_of_map_entries : (value * value) list -> query_result list list
  val bind_query_inputs :
    query_input_of_arg:(query_input -> query_arg -> query_input) ->
    consume_rules:bool ->
    query_input list ->
    query_arg list ->
    query_input list
end

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
