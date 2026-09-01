open Datascript_types

(** Shared index database handle for a storage backend. *)
type index_db =
  | Lmdb of Datascript_lmdb_db.t
  | Sqlite of Datascript_sqlite_db.t

(** How a storage backend relates to the live index layer.

    - [Share_index_db handle]: index datoms live in the same store as storage
      (memory LMDB and SQLite backends).
    - [Separate_index_db]: storage keeps its own tables and copies into a
      temporary index on restore (legacy mirror backends). *)
type storage_index_db =
  | Share_index_db of index_db
  | Separate_index_db

type storage_backend = {
  kind : storage_kind
  ; restore_meta : unit -> schema * entity_id * tx * datom list
  ; store_meta : db -> unit
  ; sync_indexes_to_storage : since_tx:tx -> unit
  ; sync_removals_to_storage : datom list -> unit
  ; load_indexes_from_storage : index_db -> unit
  ; index_db : storage_index_db
}

val kind_of : storage -> storage_kind
val ensure_live : storage -> unit
val memory_storage : unit -> storage
val benchmark_memory_storage : unit -> storage
val register_backend : storage_backend -> ?check_live:(unit -> unit) -> unit -> storage
val restore_meta : storage -> schema * entity_id * tx * datom list
val store_db : storage -> db -> unit
val sync_indexes_to_storage : since_tx:tx -> storage -> unit
val sync_removals_to_storage : datom list -> storage -> unit
val load_indexes_from_storage : storage -> index_db -> unit
val db_for_storage : storage -> index_db
val same_storage_db : storage -> index_db -> bool
val create_index_db : storage option -> index_db * storage option

type plugin = storage_backend
type index_db_mode = storage_index_db
val register_plugin : storage_backend -> ?check_live:(unit -> unit) -> unit -> storage

val wrap_lmdb : ?check_live:(unit -> unit) -> Datascript_lmdb_db.t -> storage
