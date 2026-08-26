open Datascript_types

type t
type 'a seq

val db_of : t -> Datascript_lmdb_db.t
val empty : index -> Datascript_lmdb_db.t -> t
val of_sorted_list : index -> datom list -> Datascript_lmdb_db.t -> t
val add : datom -> t -> t
val remove : datom -> t -> t
val flush : t -> t
val copy : t -> t
val sync_merged_to_lmdb : t -> Datascript_lmdb_db.t -> unit
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
