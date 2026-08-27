open Datascript_types

type t

val create_temp : unit -> t
val open_path : string -> t
val close : t -> unit
val sync : t -> unit
val remove_path : string -> unit

val meta_get : t -> string -> string option
val meta_set : t -> string -> string -> unit

val with_write_txn : t -> ([ `Read | `Write ] Lmdb.Txn.t -> unit) -> unit
val put_index_txn : index -> [ `Read | `Write ] Lmdb.Txn.t -> t -> string -> string -> unit
val remove_index_txn : index -> [ `Read | `Write ] Lmdb.Txn.t -> t -> string -> unit
val copy_index_txn : index -> [ `Read | `Write ] Lmdb.Txn.t -> t -> t -> unit

val fold_index : index -> t -> (string -> string -> unit) -> unit
val fold_index_range :
  index -> t -> ?from_key:string -> ?to_key:string -> (string -> string -> unit) -> unit
val put_index : index -> t -> string -> string -> unit
val remove_index : index -> t -> string -> unit
