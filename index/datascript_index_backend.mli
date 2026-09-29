open Datascript_types

(** Signature implemented by every native index backend (LMDB, SQLite,
    PostgreSQL, ...). A backend is a regular module; storage backends pack one
    as a first-class module value together with its store handle (see
    [Datascript_storage_protocol.index_db]) so the query layer dispatches
    uniformly without knowing the concrete backend type.

    Operations are side-effecting on the underlying store; functions returning
    [t] return the handle for interface compatibility with the persistent set
    API. *)
module type S = sig
  type db
  (** Store handle shared by the four index tables (EAVT/AEVT/AVET/TAVE). *)

  type t
  (** Per-index handle over [db]. *)

  type seq
  (** Lazy scan over one index (always over [datom]s). *)

  val kind : storage_kind
  val sync : db -> unit
  val meta_get : db -> string -> string option
  val meta_set : db -> string -> string -> unit

  val empty : index -> db -> t
  val db_of : t -> db
  val of_sorted_list : index -> datom list -> db -> t
  val of_sorted_lists : (index * datom list) list -> db -> unit
  val of_eavt_datoms : avet:(attr -> bool) -> datom list -> db -> unit
  val of_bulk : index -> datom list -> db -> t
  val append_datoms : datom list -> t -> t
  val append_tx_data : avet:(attr -> bool) -> datom list -> db -> unit
  val add : datom -> t -> t
  val remove : datom -> t -> t
  val remove_datoms : datom list -> t -> t
  val lookup : t -> datom -> datom option
  val to_list : t -> datom list
  val fold : ('acc -> datom -> 'acc) -> 'acc -> t -> 'acc
  val fold_slice :
    ('acc -> datom -> 'acc) -> 'acc -> ?from_:datom -> ?to_:datom -> ?cmp:(datom -> datom -> int) -> t -> 'acc
  val find_first_slice :
    ?from_:datom -> ?to_:datom -> ?cmp:(datom -> datom -> int) -> t -> datom option
  val fold_attr_prefix : ('acc -> datom -> 'acc) -> 'acc -> t -> attr -> 'acc
  val slice : ?from_:datom -> ?to_:datom -> ?cmp:(datom -> datom -> int) -> t -> datom list
  val slice_seq : ?from_:datom -> ?to_:datom -> ?cmp:(datom -> datom -> int) -> t -> seq
  val rslice_seq : ?from_:datom -> ?to_:datom -> ?cmp:(datom -> datom -> int) -> t -> seq
  val seq : t -> seq
  val seq_to_list : seq -> datom list
  val fold_seq : ('acc -> datom -> 'acc) -> 'acc -> seq -> 'acc
  val to_seq : seq -> datom Seq.t
  val seek : datom -> seq -> seq
  val flush : t -> t
  val copy : t -> t
  val fold_tave_range :
    ('acc -> datom -> 'acc) -> 'acc -> db -> from_tx:tx -> ?to_tx:tx -> ?attr:attr -> unit -> 'acc
  val prune_tave_before : db -> before_tx:tx -> unit
end
