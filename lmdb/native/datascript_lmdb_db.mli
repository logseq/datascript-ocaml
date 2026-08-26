open Datascript_types

type t

val create_temp : unit -> t
val open_path : string -> t
val close : t -> unit
val sync : t -> unit
val remove_path : string -> unit

val meta_get : t -> string -> string option
val meta_set : t -> string -> string -> unit

val fold_index : index -> t -> (string -> string -> unit) -> unit
val put_index : index -> t -> string -> string -> unit
val remove_index : index -> t -> string -> unit
