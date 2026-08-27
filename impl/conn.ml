open Datascript_types

type t =
  { mutable db : db
  ; mutable listeners : (string * (tx_report -> unit)) list
  ; mutable next_listener_id : int
  ; storage : storage option
  }

type creation_context =
  { empty_db : ?schema:schema -> ?storage:storage -> unit -> db
  ; init_db : ?schema:schema -> ?storage:storage -> datom list -> db
  ; store : ?storage:storage -> db -> unit
  }

type schema_context =
  { store : ?storage:storage -> db -> unit
  ; with_schema : db -> schema -> db
  }

type restore_context = { restore : storage -> db option }

type transact_context =
  { store : ?storage:storage -> db -> unit
  ; transact : tx_meta:tx_meta -> db -> tx_op list -> tx_report
  }

type reset_context =
  { store : ?storage:storage -> db -> unit
  ; datoms : db -> datom list
  ; snapshot_db : db -> db
  }

type context =
  { empty_db : ?schema:schema -> ?storage:storage -> unit -> db
  ; init_db : ?schema:schema -> ?storage:storage -> datom list -> db
  ; store : ?storage:storage -> db -> unit
  ; restore : storage -> db option
  ; transact : tx_meta:tx_meta -> db -> tx_op list -> tx_report
  ; datoms : db -> datom list
  ; with_schema : db -> schema -> db
  }

let tx_meta_skips_store tx_meta =
  List.exists (function
    | "skip-store?", Bool true -> true
    | _ -> false)
    tx_meta

let tx_meta_without_store_control tx_meta =
  List.filter
    (function
      | "skip-store?", _ -> false
      | _ -> true)
    tx_meta

let make ?storage db =
  let db =
    match storage with
    | None -> db
    | Some _ -> { db with storage_ref = storage }
  in
  { db; listeners = []; next_listener_id = 0; storage }

let create (context : creation_context) ?schema ?storage () =
  let db = context.empty_db ?schema ?storage () in
  let db =
    match storage with
    | None -> db
    | Some storage ->
      context.store ~storage db;
      { db with storage_ref = Some storage }
  in
  make ?storage db

let from_db (context : creation_context) db =
  match db.storage_ref with
  | None -> make db
  | Some storage ->
    context.store ~storage db;
    make ~storage db

let from_datoms (context : creation_context) ?schema ?storage datoms =
  from_db context (context.init_db ?schema ?storage datoms)

let db conn = conn.db

let is_conn (_ : t) = true

let listen conn key callback =
  conn.listeners <- (key, callback) :: List.remove_assoc key conn.listeners;
  key

let listen_auto conn callback =
  let rec next_key () =
    conn.next_listener_id <- conn.next_listener_id + 1;
    let key = "listener-" ^ string_of_int conn.next_listener_id in
    if List.mem_assoc key conn.listeners then next_key () else key
  in
  listen conn (next_key ()) callback

let unlisten conn key =
  conn.listeners <- List.remove_assoc key conn.listeners

let notify_listeners conn report =
  conn.listeners
  |> List.rev
  |> List.iter (fun (_, callback) -> callback report)

let reset_schema (context : schema_context) conn schema =
  let db = context.with_schema conn.db schema in
  conn.db <- db;
  (match conn.storage with
   | None -> ()
   | Some storage -> context.store ~storage db);
  db

let restore (context : restore_context) storage =
  match context.restore storage with
  | None -> None
  | Some db -> Some (make ~storage db)

let transact (context : transact_context) ?(tx_meta = []) conn tx_data =
  let skip_store = tx_meta_skips_store tx_meta in
  let report = context.transact ~tx_meta:(tx_meta_without_store_control tx_meta) conn.db tx_data in
  conn.db <- report.db_after;
  if not skip_store then
    (match conn.storage with
     | None -> ()
     | Some storage -> context.store ~storage report.db_after);
  notify_listeners conn report;
  report

let reset (context : reset_context) ?(tx_meta = []) conn db =
  let db_before = context.snapshot_db conn.db in
  let db =
    match conn.storage with
    | None -> db
    | Some _ -> { db with storage_ref = conn.storage }
  in
  let tx_data =
    List.map (fun datom -> { datom with added = false }) (context.datoms conn.db)
    @ context.datoms db
  in
  let report = { db_before; db_after = db; tx_data; tempids = []; tx_meta; purged_datoms = [] } in
  conn.db <- db;
  (match conn.storage with
   | None -> ()
   | Some storage -> context.store ~storage db);
  notify_listeners conn report;
  db
