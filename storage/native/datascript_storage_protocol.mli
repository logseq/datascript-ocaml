open Datascript_types

(** Shared index database handle for a storage backend: a backend module
    packed as a first-class module together with its store handle and a unique
    identity token (see {!same_storage_db}). *)
type index_db =
  | Index_db : 'db 'idx 'sq.
      int
      * 'db
      * (module Datascript_index_backend.S
           with type db = 'db
            and type t = 'idx
            and type seq = 'sq)
      -> index_db

(** Pack a backend store handle into an {!index_db} value. The minted identity
    token is what {!same_storage_db} compares. *)
val pack_index_db :
  (module Datascript_index_backend.S
     with type db = 'db
      and type t = 'idx
      and type seq = 'sq)
  -> 'db
  -> index_db

(** How a storage backend relates to the live index layer.

    - [Share_index_db handle]: index datoms live in the same store as storage
      (memory/file LMDB, SQLite and PostgreSQL backends).
    - [Separate_index_db]: storage keeps its own tables and copies into a
      temporary index on restore (legacy mirror backends). *)
type storage_index_db =
  | Share_index_db of index_db
  | Separate_index_db

(** Callback bundle for a pluggable storage backend.

    Third-party packages (LMDB file, SQLite, PostgreSQL, ...) register an
    implementation via {!register_backend}. Use any unique {!storage_kind} string,
    for example ["pg"]. *)
type storage_backend = {
  kind : storage_kind
  ; restore_meta : unit -> schema * entity_id * tx * datom list * bool
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
val restore_meta : storage -> schema * entity_id * tx * datom list * bool
val store_db : storage -> db -> unit
val sync_indexes_to_storage : since_tx:tx -> storage -> unit
val sync_removals_to_storage : datom list -> storage -> unit
val load_indexes_from_storage : storage -> index_db -> unit
val db_for_storage : storage -> index_db
val same_storage_db : storage -> index_db -> bool
val create_index_db : storage option -> index_db * storage option

(** Backwards-compatible aliases. *)
type plugin = storage_backend
type index_db_mode = storage_index_db
val register_plugin : storage_backend -> ?check_live:(unit -> unit) -> unit -> storage
