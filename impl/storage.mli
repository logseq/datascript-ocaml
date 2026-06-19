open Datascript_types

type tail_context =
  { apply_group : db -> datom list -> db
  }

type restore_context =
  { next_db_uid : unit -> int
  ; db_with_tail : db -> datom list list -> db
  }

val root_address : storage_address
val tail_address : storage_address
val memory_storage : unit -> storage
val file_storage : string -> storage
val store : ?storage:storage -> db -> unit
val store_tail : storage -> datom list list -> unit
val tail_compaction_threshold : int
val tail_datom_count : datom list list -> int
val restore_root_snapshot : storage -> serializable_db option
val restore_tail_groups : storage -> datom list list
val db_with_tail : tail_context -> db -> datom list list -> db
val restore : restore_context -> storage -> db option
val storage_addresses : storage -> storage_address list
val storage : db -> storage option
val addresses : db list -> storage_address list
val settings : db -> (attr * value) list
val collect_garbage : storage -> unit
