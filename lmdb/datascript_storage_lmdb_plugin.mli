open Datascript_types

val wrap_lmdb : ?check_live:(unit -> unit) -> Datascript_lmdb_db.t -> storage
