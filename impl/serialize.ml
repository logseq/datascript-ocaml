open Datascript_types

module PSet = Persistent_sorted_set

type context =
  { next_db_uid : unit -> int
  ; validate_schema : schema -> schema
  ; normalize_datom_for_schema : schema -> datom -> datom
  ; refresh_db_indexes : db -> db
  }

let serializable db =
  { serializable_schema = db.schema
  ; serializable_datoms = db.datoms
  ; serializable_history_datoms = db.history_datoms
  ; serializable_historical = db.historical
  ; serializable_max_eid = db.max_eid
  ; serializable_max_tx = db.max_tx
  }

let empty_index index =
  PSet.empty_by (Util.compare_datom index)

let from_serializable context snapshot =
  let schema = context.validate_schema snapshot.serializable_schema in
  let datoms = List.map (context.normalize_datom_for_schema schema) snapshot.serializable_datoms in
  let history_datoms = List.map (context.normalize_datom_for_schema schema) snapshot.serializable_history_datoms in
  context.refresh_db_indexes
    { db_uid = context.next_db_uid ()
    ; schema
    ; datoms
    ; eavt_index = empty_index Eavt
    ; aevt_index = empty_index Aevt
    ; avet_index = empty_index Avet
    ; vaet_index = empty_index Vaet
    ; history_datoms
    ; historical = snapshot.serializable_historical
    ; max_eid = snapshot.serializable_max_eid
    ; max_datom_e = 0
    ; max_tx = snapshot.serializable_max_tx
    ; unique_index = []
    ; filter_pred = None
    ; storage_ref = None
    ; tx_fns = []
    }
