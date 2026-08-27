open Datascript_types

(** How a storage backend relates to the in-memory LMDB index layer. *)
type storage_index_db =
  | Share_index_db of Datascript_lmdb_db.t
  | Separate_index_db

(** Callback bundle for a pluggable storage backend (LMDB file, SQLite, PostgreSQL, ...). *)
type storage_backend = {
  kind : storage_kind
  ; restore_meta : unit -> schema * entity_id * tx * datom list
  ; store_meta : db -> unit
  ; sync_indexes_to_storage :
      since_tx:tx ->
      Datascript_lmdb_index.t ->
      Datascript_lmdb_index.t ->
      Datascript_lmdb_index.t ->
      unit
  ; sync_removals_to_storage : datom list -> unit
  ; load_indexes_from_storage : Datascript_lmdb_db.t -> unit
  ; index_db : storage_index_db
}

type backend_state = {
  check_live : (unit -> unit) option
  ; backend : storage_backend
}

let registry : (int, backend_state) Hashtbl.t = Hashtbl.create 16
let next_id = ref 0

let register_backend backend ?check_live () =
  incr next_id;
  let id = !next_id in
  let state = { backend; check_live } in
  Hashtbl.replace registry id state;
  Storage_handle id

let id_of = function
  | Storage_handle id -> id

let state_of storage =
  match Hashtbl.find_opt registry (id_of storage) with
  | Some state -> state
  | None -> invalid_arg "unknown storage handle"

let backend_of storage = (state_of storage).backend

let ensure_live storage =
  let state = state_of storage in
  Option.iter (fun check -> check ()) state.check_live

let kind_of storage = (backend_of storage).kind

let memory_backend lmdb =
  let restore_meta () = Datascript_storage_lmdb.restore_meta lmdb in
  let store_meta db = Datascript_storage_lmdb.store_meta lmdb db in
  let sync_indexes_to_storage ~since_tx eavt aevt avet =
    Datascript_lmdb_index.sync_append_since_tx ~since_tx eavt lmdb;
    Datascript_lmdb_index.sync_append_since_tx ~since_tx aevt lmdb;
    Datascript_lmdb_index.sync_append_since_tx ~since_tx avet lmdb
  in
  let sync_removals_to_storage removed_datoms =
    let remove index =
      let t = Datascript_lmdb_index.empty index lmdb in
      ignore (Datascript_lmdb_index.remove_datoms removed_datoms t)
    in
    remove Eavt;
    remove Aevt;
    remove Avet
  in
  let load_indexes_from_storage target_lmdb =
    if lmdb != target_lmdb then Datascript_storage_lmdb.sync_indexes lmdb target_lmdb
  in
  {
    kind = storage_kind_memory
  ; restore_meta
  ; store_meta
  ; sync_indexes_to_storage
  ; sync_removals_to_storage
  ; load_indexes_from_storage
  ; index_db = Share_index_db lmdb
  }

let memory_storage () =
  register_backend (memory_backend (Datascript_lmdb_db.create_temp ())) ()

let benchmark_memory_storage () =
  register_backend (memory_backend (Datascript_lmdb_db.create_benchmark_temp ())) ()

let restore_meta storage =
  ensure_live storage;
  (backend_of storage).restore_meta ()

let store_db storage db =
  ensure_live storage;
  (backend_of storage).store_meta db

let sync_indexes_to_storage ~since_tx eavt aevt avet storage =
  ensure_live storage;
  (backend_of storage).sync_indexes_to_storage ~since_tx eavt aevt avet

let sync_removals_to_storage removed_datoms storage =
  ensure_live storage;
  (backend_of storage).sync_removals_to_storage removed_datoms

let load_indexes_from_storage storage target_lmdb =
  ensure_live storage;
  (backend_of storage).load_indexes_from_storage target_lmdb

let db_for_storage storage =
  ensure_live storage;
  match (backend_of storage).index_db with
  | Share_index_db db -> db
  | Separate_index_db ->
      invalid_arg "storage backend uses a separate index db, expected shared LMDB index db"

let same_storage_db storage index_lmdb =
  ensure_live storage;
  match (backend_of storage).index_db with
  | Share_index_db db -> db == index_lmdb
  | Separate_index_db -> false

let create_index_db storage =
  match storage with
  | None -> (Datascript_lmdb_db.create_temp (), None)
  | Some storage ->
      ensure_live storage;
      (match (backend_of storage).index_db with
       | Share_index_db db -> (db, Some storage)
       | Separate_index_db -> (Datascript_lmdb_db.create_temp (), Some storage))

(** Backwards-compatible alias. *)
let register_plugin = register_backend

(** Backwards-compatible alias. *)
type plugin = storage_backend

(** Backwards-compatible alias. *)
type index_db_mode = storage_index_db
