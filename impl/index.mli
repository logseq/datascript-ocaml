open Datascript_types

type t = index_set
type 'a seq
type index_db
type lmdb = index_db

val same_storage_db : storage -> index_db -> bool
val create_index_db : storage option -> index_db * storage option
val create_lmdb : storage option -> index_db * storage option
val index_db_of : index_db -> index_db
val lmdb_of : index_db -> index_db
val db_of : t -> index_db
val index_db_for_storage : storage -> index_db
val lmdb_for_storage : storage -> index_db
val sync_indexes_to_storage : since_tx:tx -> t -> t -> t -> storage -> unit
val sync_removals_to_storage : datom list -> t -> t -> t -> storage -> unit
val load_indexes_from_storage : storage -> index_db -> unit

val empty : index -> index_db -> t
val of_sorted_list : index -> datom list -> index_db -> t
val of_sorted_lists : (index * datom list) list -> index_db -> unit
val of_eavt_datoms : avet:(string -> bool) -> datom list -> index_db -> unit
val of_bulk : index -> datom list -> index_db -> t
val append_datoms : datom list -> t -> t
val append_tx_data : avet:(attr -> bool) -> datom list -> t -> t -> t -> t * t * t
val add : datom -> t -> t
val remove : datom -> t -> t
val lookup : t -> datom -> datom option
val to_list : t -> datom list
val fold : ('acc -> datom -> 'acc) -> 'acc -> t -> 'acc
val fold_slice :
  ('acc -> datom -> 'acc) -> 'acc -> ?from_:datom -> ?to_:datom -> ?cmp:(datom -> datom -> int) -> t -> 'acc
val find_first_slice :
  ?from_:datom -> ?to_:datom -> ?cmp:(datom -> datom -> int) -> t -> datom option
val fold_attr_prefix : ('acc -> datom -> 'acc) -> 'acc -> t -> string -> 'acc
val slice : ?from_:datom -> ?to_:datom -> ?cmp:(datom -> datom -> int) -> t -> datom list
val slice_seq : ?from_:datom -> ?to_:datom -> ?cmp:(datom -> datom -> int) -> t -> datom seq
val rslice_seq : ?from_:datom -> ?to_:datom -> ?cmp:(datom -> datom -> int) -> t -> datom seq
val seq : t -> datom seq
val seq_to_list : datom seq -> datom list
val fold_seq : ('acc -> datom -> 'acc) -> 'acc -> datom seq -> 'acc
val to_seq : datom seq -> datom Seq.t
val seek : datom -> datom seq -> datom seq
val flush : t -> t
val copy : t -> t
val fold_tave_range :
  ('acc -> datom -> 'acc) -> 'acc -> t -> from_tx:tx -> ?to_tx:tx -> ?attr:string -> unit -> 'acc
val prune_tave_before : t -> before_tx:tx -> unit
