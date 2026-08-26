open Datascript_types

type t = index_set
type 'a seq
type lmdb

val create_lmdb : storage option -> lmdb * storage option
val lmdb_of : lmdb -> lmdb
val db_of : t -> lmdb
val lmdb_for_storage : storage -> lmdb
val sync_indexes_to_storage : t -> t -> t -> storage -> unit
val load_indexes_from_storage : storage -> lmdb -> unit

val empty : index -> lmdb -> t
val of_sorted_list : index -> datom list -> lmdb -> t
val add : datom -> t -> t
val remove : datom -> t -> t
val to_list : t -> datom list
val fold : ('acc -> datom -> 'acc) -> 'acc -> t -> 'acc
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
