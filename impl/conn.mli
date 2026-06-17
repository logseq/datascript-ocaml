open Datascript_types

type t

type creation_context =
  { empty_db : ?schema:schema -> ?storage:storage -> unit -> db
  ; init_db : ?schema:schema -> ?storage:storage -> datom list -> db
  ; store : ?storage:storage -> db -> unit
  }

type schema_context =
  { store : ?storage:storage -> db -> unit
  ; with_schema : db -> schema -> db
  }

type restore_context =
  { restore : storage -> db option
  ; restore_tail_groups : storage -> datom list list
  }

type transact_context =
  { store : ?storage:storage -> db -> unit
  ; store_tail : storage -> datom list list -> unit
  ; storage_tail_datom_count : datom list list -> int
  ; storage_tail_compaction_threshold : int
  ; transact : tx_meta:tx_meta -> db -> tx_op list -> tx_report
  }

type reset_context =
  { store : ?storage:storage -> db -> unit
  ; datoms : db -> datom list
  }

type context =
  { empty_db : ?schema:schema -> ?storage:storage -> unit -> db
  ; init_db : ?schema:schema -> ?storage:storage -> datom list -> db
  ; store : ?storage:storage -> db -> unit
  ; store_tail : storage -> datom list list -> unit
  ; restore : storage -> db option
  ; restore_tail_groups : storage -> datom list list
  ; storage_tail_datom_count : datom list list -> int
  ; storage_tail_compaction_threshold : int
  ; transact : tx_meta:tx_meta -> db -> tx_op list -> tx_report
  ; datoms : db -> datom list
  ; with_schema : db -> schema -> db
  }

val create : creation_context -> ?schema:schema -> ?storage:storage -> unit -> t
val from_db : creation_context -> db -> t
val from_datoms : creation_context -> ?schema:schema -> ?storage:storage -> datom list -> t
val db : t -> db
val is_conn : t -> bool
val listen : t -> string -> (tx_report -> unit) -> string
val listen_auto : t -> (tx_report -> unit) -> string
val unlisten : t -> string -> unit
val reset_schema : schema_context -> t -> schema -> db
val restore : restore_context -> storage -> t option
val transact : transact_context -> ?tx_meta:tx_meta -> t -> tx_op list -> tx_report
val reset : reset_context -> ?tx_meta:tx_meta -> t -> db -> db
