open Datascript_types

type restore_context = { next_db_uid : unit -> int }

val memory_storage : unit -> storage
val ensure_live : storage -> unit
val kind_of : storage -> storage_kind
val store : ?storage:storage -> db -> unit
val restore_root_snapshot : storage -> serializable_db option
val restore : restore_context -> storage -> db option
val storage : db -> storage option
val settings : db -> (attr * value) list
val collect_garbage : storage -> unit
