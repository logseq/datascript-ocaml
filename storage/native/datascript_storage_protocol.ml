open Datascript_types

(** Shared index database handle for a storage backend: a backend module
 (packed as a first-class module) plus its store handle and a unique identity
 token minted by [pack_index_db]. [same_storage_db] compares tokens, so two
 handles backed by the same store (e.g. a live index db and its registered
 storage) compare equal regardless of backend. *)
type index_db =
  | Index_db : 'db 'idx 'sq.
      int
      * 'db
      * (module Datascript_index_backend.S
           with type db = 'db
            and type t = 'idx
            and type seq = 'sq)
      -> index_db

let pack_index_db (type db idx sq)
    (module B : Datascript_index_backend.S
      with type db = db
       and type t = idx
       and type seq = sq)
    (db : db) : index_db =
  Index_db (Oo.id (object end), db, (module B))

(** How a storage backend relates to the live index layer. *)
type storage_index_db =
  | Share_index_db of index_db
  | Separate_index_db

(** Callback bundle for a pluggable storage backend (LMDB file, SQLite, PostgreSQL, ...). *)
type storage_backend = {
  kind : storage_kind
  ; restore_meta : unit -> schema * entity_id * tx * datom list * bool
  ; store_meta : db -> unit
  ; sync_indexes_to_storage : since_tx:tx -> unit
  ; sync_removals_to_storage : datom list -> unit
  ; load_indexes_from_storage : index_db -> unit
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
  (* Share path: live indexes use this LMDB env, so delta sync is unnecessary. *)
  let sync_indexes_to_storage ~since_tx = ignore since_tx in
  let sync_removals_to_storage removed_datoms =
    let remove index =
      let t = Datascript_lmdb_index.empty index lmdb in
      ignore (Datascript_lmdb_index.remove_datoms removed_datoms t)
    in
    remove Eavt;
    remove Aevt;
    remove Avet;
    remove Tave
  in
  (* The callback target is always this backend's own index db, so indexes are
     already loaded. *)
  let load_indexes_from_storage _target = () in
  {
    kind = storage_kind_memory
  ; restore_meta
  ; store_meta
  ; sync_indexes_to_storage
  ; sync_removals_to_storage
  ; load_indexes_from_storage
  ; index_db = Share_index_db (pack_index_db (module Datascript_lmdb_backend) lmdb)
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

let sync_indexes_to_storage ~since_tx storage =
  ensure_live storage;
  (backend_of storage).sync_indexes_to_storage ~since_tx

let sync_removals_to_storage removed_datoms storage =
  ensure_live storage;
  (backend_of storage).sync_removals_to_storage removed_datoms

let load_indexes_from_storage storage target =
  ensure_live storage;
  (backend_of storage).load_indexes_from_storage target

let db_for_storage storage =
  ensure_live storage;
  match (backend_of storage).index_db with
  | Share_index_db db -> db
  | Separate_index_db ->
      invalid_arg "storage backend uses a separate index db, expected shared index db"

let same_storage_db storage index_db =
  ensure_live storage;
  match (backend_of storage).index_db, index_db with
  | Share_index_db (Index_db (a, _, _)), Index_db (b, _, _) -> a = b
  | Separate_index_db, _ -> false

let temp_lmdb_index_db () =
  pack_index_db (module Datascript_lmdb_backend) (Datascript_lmdb_db.create_temp ())

let create_index_db storage =
  match storage with
  | None -> (temp_lmdb_index_db (), None)
  | Some storage ->
      ensure_live storage;
      (match (backend_of storage).index_db with
       | Share_index_db db -> (db, Some storage)
       | Separate_index_db -> (temp_lmdb_index_db (), Some storage))

(** Backwards-compatible alias. *)
let register_plugin = register_backend

(** Backwards-compatible alias. *)
type plugin = storage_backend

(** Backwards-compatible alias. *)
type index_db_mode = storage_index_db
