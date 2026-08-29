open Datascript_types

type t
type 'a seq

val db_of : t -> Datascript_sqlite_db.t
val empty : index -> Datascript_sqlite_db.t -> t
val of_sorted_list : index -> datom list -> Datascript_sqlite_db.t -> t
val of_sorted_lists : (index * datom list) list -> Datascript_sqlite_db.t -> unit
val of_eavt_datoms : avet:(string -> bool) -> datom list -> Datascript_sqlite_db.t -> unit
val of_bulk : index -> datom list -> Datascript_sqlite_db.t -> t
val append_datoms : datom list -> t -> t
val append_tx_data : avet:(string -> bool) -> datom list -> t -> t -> t -> t * t * t
val add : datom -> t -> t
val remove : datom -> t -> t
val remove_datoms : datom list -> t -> t
val flush : t -> t
val copy : t -> t
val sync_append_since_tx : since_tx:tx -> t -> Datascript_sqlite_db.t -> unit
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
val fold_tave_range :
  ('acc -> datom -> 'acc) -> 'acc -> Datascript_sqlite_db.t -> from_tx:tx -> ?to_tx:tx -> ?attr:string
  -> unit -> 'acc
val prune_tave_before : Datascript_sqlite_db.t -> before_tx:tx -> unit
