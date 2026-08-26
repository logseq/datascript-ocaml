module Ds = Datascript
open Lmdb

type session =
  { path : string
  ; env : Env.t
  ; map : (string, string, [ `Uni ]) Map.t
  ; mutable closed : bool
  }

let kvs_map_name = "kvs"
let default_map_size = 1024 * 1024 * 1024

let lock_path path = path ^ "-lock"

let remove_files path =
  if Sys.file_exists path then Sys.remove path;
  let lock = lock_path path in
  if Sys.file_exists lock then Sys.remove lock

let ensure_open session =
  if session.closed then invalid_arg "LMDB session is closed"

let open_env db_path =
  Env.(create Rw ~flags:Flags.no_subdir ~map_size:default_map_size ~max_maps:8 db_path)

let open_map env =
  try Map.open_existing Nodup ~key:Conv.string ~value:Conv.string ~name:kvs_map_name env
  with Not_found ->
    Map.create Nodup ~key:Conv.string ~value:Conv.string ~name:kvs_map_name env

let open_session db_path =
  remove_files db_path;
  let env = open_env db_path in
  let map = open_map env in
  { path = db_path; env; map; closed = false }

let close session =
  if not session.closed then (
    Map.close session.map;
    Env.sync session.env;
    Env.close session.env;
    session.closed <- true)

let encode_payload payload = Datascript_sqlite_codec.encode payload

let decode_payload content = Datascript_sqlite_codec.decode content

let storage session : Ds.storage =
  { storage_store =
      (fun entries ->
        ensure_open session;
        ignore
          (Txn.go Rw session.env (fun txn ->
             List.iter
               (fun (address, payload) ->
                 Map.set ~txn session.map address (encode_payload payload))
               entries;
             None)))
  ; storage_restore =
      (fun address ->
        ensure_open session;
        (try Some (Map.get session.map address |> decode_payload)
         with Not_found -> None))
  ; storage_list_addresses =
      (fun () ->
        ensure_open session;
        let addresses = ref [] in
        let next = Map.to_dispenser session.map in
        let rec loop () =
          match next () with
          | None -> ()
          | Some (address, _) ->
              addresses := address :: !addresses;
              loop ()
        in
        loop ();
        List.rev !addresses)
  ; storage_delete =
      (fun addresses ->
        ensure_open session;
        ignore
          (Txn.go Rw session.env (fun txn ->
             List.iter
               (fun address ->
                 try Map.remove ~txn session.map address with Not_found -> ())
               addresses;
             None)))
  }
