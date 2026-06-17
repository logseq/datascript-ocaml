open Datascript_types

type context =
  { next_db_uid : unit -> int
  ; validate_schema : schema -> schema
  ; normalize_datom_for_schema : schema -> datom -> datom
  ; refresh_db_indexes : db -> db
  }

val serializable : db -> serializable_db
val from_serializable : context -> serializable_db -> db
