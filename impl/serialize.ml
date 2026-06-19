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
  ; serializable_datoms = PSet.to_list db.eavt_index
  ; serializable_max_eid = db.max_eid
  ; serializable_max_tx = db.max_tx
  }

let empty_index index =
  PSet.empty_by ~cmp:(Util.compare_datom index) ()

let index_from_datoms index datoms =
  let cmp = Util.compare_datom index in
  let items = Array.of_list datoms in
  Array.sort cmp items;
  PSet.of_sorted_array_by ~cmp items

let from_serializable context snapshot =
  let schema = context.validate_schema snapshot.serializable_schema in
  let datoms = List.map (context.normalize_datom_for_schema schema) snapshot.serializable_datoms in
  { db_uid = context.next_db_uid ()
  ; schema
  ; eavt_index = index_from_datoms Eavt datoms
  ; aevt_index = empty_index Aevt
  ; avet_index = empty_index Avet
  ; max_eid = snapshot.serializable_max_eid
  ; max_datom_e = 0
  ; max_tx = snapshot.serializable_max_tx
  ; filter_pred = None
  ; storage_ref = None
  ; tx_fns = []
  }
  |> context.refresh_db_indexes
