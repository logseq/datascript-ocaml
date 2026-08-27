open Datascript_types

(** How a storage backend relates to the in-memory LMDB index layer.

    - [Share_index_db lmdb]: index datoms live in the same LMDB env as storage
      (memory and file LMDB backends).
    - [Separate_index_db]: storage keeps its own index tables and copies into a
      temp LMDB index on restore (SQLite and similar backends). *)
type storage_index_db =
  | Share_index_db of Datascript_lmdb_db.t
  | Separate_index_db

(** Callback bundle for a pluggable storage backend.

    Third-party packages (LMDB file, SQLite, PostgreSQL, ...) register an
    implementation via {!register_backend}. Use any unique {!storage_kind} string,
    for example ["pg"]. *)
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

val kind_of : storage -> storage_kind
val ensure_live : storage -> unit
val memory_storage : unit -> storage
val benchmark_memory_storage : unit -> storage

val register_backend : storage_backend -> ?check_live:(unit -> unit) -> unit -> storage
val restore_meta : storage -> schema * entity_id * tx * datom list
val store_db : storage -> db -> unit
val sync_indexes_to_storage :
  since_tx:tx -> Datascript_lmdb_index.t -> Datascript_lmdb_index.t -> Datascript_lmdb_index.t -> storage -> unit
val sync_removals_to_storage : datom list -> storage -> unit
val load_indexes_from_storage : storage -> Datascript_lmdb_db.t -> unit
val db_for_storage : storage -> Datascript_lmdb_db.t
val same_storage_db : storage -> Datascript_lmdb_db.t -> bool
val create_index_db : storage option -> Datascript_lmdb_db.t * storage option

(** Backwards-compatible aliases. *)
type plugin = storage_backend
type index_db_mode = storage_index_db
val register_plugin : storage_backend -> ?check_live:(unit -> unit) -> unit -> storage
