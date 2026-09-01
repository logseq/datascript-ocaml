open Datascript_types

val wrap_sqlite : ?check_live:(unit -> unit) -> Datascript_sqlite_db.t -> storage
