open Datascript_types

type t =
  { mutable db : db
  ; mutable listeners : (string * (tx_report -> unit)) list
  ; mutable next_listener_id : int
  ; storage : storage option
  ; (* per-transaction datom batches, newest first: consing each report's
       tx_data is O(1); reversed back to on-disk tx order only when the
       tail is written or read out *)
    mutable storage_tail : datom list list
  }

type creation_context =
  { empty_db : ?schema:schema -> ?storage:storage -> unit -> db
  ; init_db : ?schema:schema -> ?storage:storage -> datom list -> db
  ; store : ?storage:storage -> db -> db
  }

type schema_context =
  { store : ?storage:storage -> db -> db
  ; with_schema : db -> schema -> db
  }

type restore_context =
  { restore : storage -> db option
  ; restore_tail_groups : storage -> datom list list
  }

type transact_context =
  { store : ?storage:storage -> db -> db
  ; store_tail : storage -> datom list list -> unit
  ; storage_tail_datom_count : datom list list -> int
  ; storage_tail_compaction_threshold : db -> int
  ; transact : tx_meta:tx_meta -> db -> tx_op list -> tx_report
  }

type reset_context =
  { store : ?storage:storage -> db -> db
  ; datoms : db -> datom list
  }

type context =
  { empty_db : ?schema:schema -> ?storage:storage -> unit -> db
  ; init_db : ?schema:schema -> ?storage:storage -> datom list -> db
  ; store : ?storage:storage -> db -> db
  ; store_tail : storage -> datom list list -> unit
  ; restore : storage -> db option
  ; restore_tail_groups : storage -> datom list list
  ; storage_tail_datom_count : datom list list -> int
  ; storage_tail_compaction_threshold : db -> int
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

let make ?storage ?(storage_tail = []) db =
  let db =
    match storage with
    | None -> db
    | Some _ -> { db with storage_ref = storage }
  in
  { db; listeners = []; next_listener_id = 0; storage; storage_tail = List.rev storage_tail }

let create (context : creation_context) ?schema ?storage () =
  let db = context.empty_db ?schema ?storage () in
  let db =
    match storage with
    | None -> db
    | Some storage ->
      { (context.store ~storage db) with storage_ref = Some storage }
  in
  make ?storage db

let from_db (context : creation_context) db =
  match db.storage_ref with
  | None -> make db
  | Some storage ->
    make ~storage (context.store ~storage db)

let from_datoms (context : creation_context) ?schema ?storage datoms =
  from_db context (context.init_db ?schema ?storage datoms)

let db conn = conn.db

(* cljs (swap! conn assoc <key> <v>) — in-place db update with no
   store/notify side effects, for bookkeeping fields like :max-tx. *)
let update_db conn f = conn.db <- f conn.db

let storage_tail conn = List.rev conn.storage_tail

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
   | Some storage ->
     conn.db <- context.store ~storage db;
     conn.storage_tail <- []);
  db

let restore (context : restore_context) storage =
  match context.restore storage with
  | None -> None
  | Some db -> Some (make ~storage ~storage_tail:(context.restore_tail_groups storage) db)

let transact (context : transact_context) ?(tx_meta = []) conn tx_data =
  let skip_store = tx_meta_skips_store tx_meta in
  let report = context.transact ~tx_meta:(tx_meta_without_store_control tx_meta) conn.db tx_data in
  conn.db <- report.db_after;
  if not skip_store then
    (match conn.storage with
     | None -> ()
     | Some storage ->
       if report.tx_data <> [] then begin
         let tail = report.tx_data :: conn.storage_tail in
         if context.storage_tail_datom_count tail > context.storage_tail_compaction_threshold report.db_after then begin
           conn.db <- context.store ~storage report.db_after;
           conn.storage_tail <- []
         end else begin
           conn.storage_tail <- tail;
           context.store_tail storage (List.rev conn.storage_tail)
         end
       end);
  notify_listeners conn report;
  report

(* cljs compare-and-set! + dc/store-after-transact! + dc/run-callbacks:
   install a report computed off-conn (e.g. by a post-process pipeline
   built with transact/with-tx) as the conn's authoritative db, persist
   its tx-data through the storage tail, and notify listeners with that
   exact report rather than a re-transaction. *)
let apply_report (context : transact_context) conn (report : tx_report) =
  let db_after =
    match conn.storage with
    | None -> report.db_after
    | Some _ -> { report.db_after with storage_ref = conn.storage }
  in
  conn.db <- db_after;
  (match conn.storage with
   | None -> ()
   | Some storage ->
     if report.tx_data <> [] then begin
       let tail = report.tx_data :: conn.storage_tail in
       if context.storage_tail_datom_count tail > context.storage_tail_compaction_threshold db_after then begin
         conn.db <- context.store ~storage db_after;
         conn.storage_tail <- []
       end else begin
         conn.storage_tail <- tail;
         context.store_tail storage (List.rev conn.storage_tail)
       end
     end);
  let report = { report with db_after } in
  notify_listeners conn report;
  report

let reset (context : reset_context) ?(tx_meta = []) conn db =
  let db =
    match conn.storage with
    | None -> db
    | Some _ -> { db with storage_ref = conn.storage }
  in
  let tx_data =
    List.map (fun datom -> { datom with added = false }) (context.datoms conn.db)
    @ context.datoms db
  in
  let report = { db_before = conn.db; db_after = db; tx_data; tempids = []; tx_meta } in
  conn.db <- db;
  (match conn.storage with
   | None -> ()
   | Some storage ->
     conn.db <- context.store ~storage db;
     conn.storage_tail <- []);
  notify_listeners conn report;
  db
