open Datascript_types

module Index = Index
module Schema = Schema

type context =
  { next_db_uid : unit -> int
  ; validate_schema : schema -> schema
  ; normalize_datom_for_schema : schema -> datom -> datom
  ; with_datoms : db -> datom list -> db
  }

let serializable db =
  { serializable_schema = db.schema
  ; serializable_datoms =
      Index.to_list db.eavt_index @ db.duplicate_datoms |> List.sort (Datascript_types.Compare.compare_datom Eavt)
  ; serializable_max_eid = db.max_eid
  ; serializable_max_tx = db.max_tx
  }

let from_serializable context snapshot =
  let schema = context.validate_schema snapshot.serializable_schema in
  let datoms = List.map (context.normalize_datom_for_schema schema) snapshot.serializable_datoms in
  let lmdb, storage_ref = Index.create_lmdb None in
  { db_uid = context.next_db_uid ()
  ; schema
  ; eavt_index = Index.empty Eavt lmdb
  ; aevt_index = Index.empty Aevt lmdb
  ; avet_index = Index.empty Avet lmdb
  ; aevt_by_attr = Hashtbl.create 0
  ; avet_by_attr = Hashtbl.create 0
  ; avet_entities_by_attr_value = Hashtbl.create 0
  ; duplicate_datoms = []
  ; duplicate_aevt_datoms = []
  ; duplicate_avet_datoms = []
  ; duplicate_eavt_by_entity = Hashtbl.create 0
  ; duplicate_aevt_by_attr = Hashtbl.create 0
  ; duplicate_avet_by_attr = Hashtbl.create 0
  ; max_eid = snapshot.serializable_max_eid
  ; max_datom_e = 0
  ; max_tx = snapshot.serializable_max_tx
  ; store_max_tx = snapshot.serializable_max_tx
  ; as_of_tx = None
  ; since_tx = None
  ; history = false
  ; filter_pred = None
  ; storage_ref
  ; tx_fns = []
  }
  |> fun db -> context.with_datoms db datoms
