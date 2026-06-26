module Ds = Datascript

type session =
  { path : string
  ; mutable closed : bool
  }

external sqlite_open : string -> unit = "datascript_sqlite_open"
external sqlite_close : string -> unit = "datascript_sqlite_close"
external sqlite_store : string -> (string * string) list -> unit = "datascript_sqlite_store"
external sqlite_restore : string -> string -> string option = "datascript_sqlite_restore"
external sqlite_list_addresses : string -> string list = "datascript_sqlite_list_addresses"
external sqlite_delete : string -> string list -> unit = "datascript_sqlite_delete"

let ensure_open session =
  if session.closed then invalid_arg "SQLite session is closed"

let open_session path =
  sqlite_open path;
  { path; closed = false }

let close session =
  if not session.closed then (
    sqlite_close session.path;
    session.closed <- true)

let storage session : Ds.storage =
  { storage_store =
      (fun entries ->
        ensure_open session;
        sqlite_store session.path
          (List.map
             (fun (address, payload) ->
               (address, Datascript_sqlite_codec.encode payload))
             entries))
  ; storage_restore =
      (fun address ->
        ensure_open session;
        sqlite_restore session.path address
        |> Option.map Datascript_sqlite_codec.decode)
  ; storage_list_addresses =
      (fun () ->
        ensure_open session;
        sqlite_list_addresses session.path)
  ; storage_delete =
      (fun addresses ->
        ensure_open session;
        sqlite_delete session.path addresses)
  }
