(** Host-installed sync SQLite driver for Melange.

    Call [set_driver] with a JavaScript object implementing the driver methods
    documented in [datascript_sqlite_driver.js]. The library does not detect
    worker vs Node; the host chooses sqlite-wasm, [node:sqlite], or any other
    synchronous SQLite and injects it here. *)

val set_driver : 'a -> unit
val has_driver : unit -> bool
