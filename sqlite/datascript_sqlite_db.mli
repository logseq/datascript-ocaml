open Datascript_types

type t

val create_temp : unit -> t
val open_path : string -> t
val close : t -> unit
val sync : t -> unit

val meta_get : t -> string -> string option
val meta_set : t -> string -> string -> unit

val with_write_txn : t -> (unit -> unit) -> unit
val with_bulk_write_txn : t -> (unit -> unit) -> unit
val put_index_txn : index -> t -> string -> string -> unit
val put_index_entries_txn : index -> t -> (string * string) list -> unit
val remove_index_txn : index -> t -> string -> unit
val put_index : index -> t -> string -> string -> unit
val remove_index : index -> t -> string -> unit
val get_index : index -> t -> string -> string option

val fold_index : index -> t -> (string -> string -> unit) -> unit
val fold_index_prefix : index -> t -> string -> (string -> string -> unit) -> unit
val fold_index_range_until :
  index ->
  t ->
  ?from_key:string ->
  ?stop:(string -> string -> bool) ->
  (string -> string -> unit) ->
  unit
val fold_index_range_desc_until :
  index ->
  t ->
  ?hi_key:string ->
  ?stop:(string -> string -> bool) ->
  (string -> string -> unit) ->
  unit

(** Lazy key/value streams (upstream-style). Consumers can stop early; the
    statement is finalized when the sequence ends or is GC'd. *)
val seq_index_range_until :
  index ->
  t ->
  ?from_key:string ->
  ?stop:(string -> string -> bool) ->
  unit ->
  (string * string) Seq.t

val seq_index_prefix :
  index -> t -> string -> unit -> (string * string) Seq.t

val seq_index_range_desc_until :
  index ->
  t ->
  ?hi_key:string ->
  ?stop:(string -> string -> bool) ->
  unit ->
  (string * string) Seq.t

val copy_index : index -> t -> t -> unit
