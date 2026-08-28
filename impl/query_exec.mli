(** Datahike-aligned query execute layer: run compiled physical ops.

    Returns [None] when a shape is not executable here; callers use the
    relational interpreter in [Query_where] as permanent fallback. *)

open Datascript_types

[@@@ocaml.warning "-67"]

type bindings = (string * query_result) list

type relation =
  { attrs : string list
  ; rows : query_result list list
  ; unique_rows : bool
  }

module Make (Context : sig
  val query_evaluator_context : Query_eval.evaluator_context
  val query_source_context : db -> Query.source_context
  val cardinality_one : db -> attr -> bool
  val datoms_by_attr_value : db -> attr -> value -> datom list
  val entity_ids_by_attr_value : db -> attr -> value -> entity_id list option
  val entity_ids_array_by_attr_value : db -> attr -> value -> entity_id array option
  val query_attr_uses_avet : db -> attr -> bool
  val query_value_uses_avet : value -> bool
  val aevt_attr_array : db -> attr -> datom array option
  val aevt_duplicate_datoms : db -> attr -> datom list
  val find_entity_in_aevt_array : datom array -> entity_id -> datom option
end) : sig
  val run :
    db ->
    (string * query_source) list ->
    query_rule list ->
    bindings list ->
    Query_plan.physical_plan ->
    relation option
end
