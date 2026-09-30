module Ds = Datascript

type session =
  { lmdb : Datascript_lmdb_db.t
  ; mutable closed : bool
  }

let ensure_open session =
  if session.closed then invalid_arg "LMDB session is closed"

let open_session db_path =
  let lmdb = Datascript_lmdb_db.open_path db_path in
  { lmdb; closed = false }

let close session =
  if not session.closed then (
    session.closed <- true;
    Datascript_lmdb_db.close session.lmdb)

let storage session =
  ensure_open session;
  Datascript_storage_lmdb_plugin.wrap_lmdb ~check_live:(fun () -> ensure_open session) session.lmdb
