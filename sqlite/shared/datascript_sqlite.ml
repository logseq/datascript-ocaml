module Ds = Datascript

type session =
  { sqlite : Datascript_sqlite_db.t
  ; mutable closed : bool
  }

let ensure_open session =
  if session.closed then invalid_arg "SQLite session is closed"

let open_session path =
  let sqlite = Datascript_sqlite_db.open_path path in
  { sqlite; closed = false }

let open_memory () =
  let sqlite = Datascript_sqlite_db.create_temp () in
  { sqlite; closed = false }

let close session =
  if not session.closed then (
    session.closed <- true;
    Datascript_sqlite_db.close session.sqlite)

let storage session =
  ensure_open session;
  Datascript_storage_sqlite_plugin.wrap_sqlite ~check_live:(fun () -> ensure_open session) session.sqlite

let sync_count session =
  ensure_open session;
  Datascript_sqlite_db.sync_count session.sqlite
