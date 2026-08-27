open Datascript_types

module Index = Index

type restore_context = { next_db_uid : unit -> int }

let memory_storage = Datascript_storage_protocol.memory_storage
let ensure_live = Datascript_storage_protocol.ensure_live
let kind_of = Datascript_storage_protocol.kind_of

let store ?storage db =
  match storage, db.storage_ref with
  | Some target_storage, _ | None, Some target_storage ->
      if not (Index.same_storage_db target_storage (Index.db_of db.eavt_index)) then (
        let _, _, stored_max_tx, _ = Datascript_storage_protocol.restore_meta target_storage in
        Index.sync_indexes_to_storage ~since_tx:stored_max_tx db.eavt_index db.aevt_index db.avet_index
          target_storage);
      Datascript_storage_protocol.store_db target_storage db
  | None, None -> invalid_arg "db has no attached storage"

let restore_root_snapshot storage =
  let schema, max_eid, max_tx, duplicate_datoms = Datascript_storage_protocol.restore_meta storage in
  let lmdb, _ = Index.create_lmdb None in
  Index.load_indexes_from_storage storage lmdb;
  Some
    { serializable_schema = schema
    ; serializable_datoms = Index.to_list (Index.empty Eavt lmdb) @ duplicate_datoms
    ; serializable_max_eid = max_eid
    ; serializable_max_tx = max_tx
    }

let restore context storage =
  let schema, max_eid, max_tx, duplicate_datoms = Datascript_storage_protocol.restore_meta storage in
  let schema = Schema.validate_schema schema in
  let lmdb, _ = Index.create_lmdb None in
  Index.load_indexes_from_storage storage lmdb;
  let duplicate_eavt_by_entity =
    let table = Hashtbl.create 1024 in
    List.iter
      (fun datom ->
        let existing = Option.value (Hashtbl.find_opt table datom.e) ~default:[] in
        Hashtbl.replace table datom.e (datom :: existing))
      duplicate_datoms;
    Hashtbl.iter (fun entity_id datoms -> Hashtbl.replace table entity_id (List.rev datoms)) table;
    table
  in
  let duplicate_datoms_by_attr duplicate_datoms =
    let table = Hashtbl.create 1024 in
    List.iter
      (fun datom ->
        let existing = Option.value (Hashtbl.find_opt table datom.a) ~default:[] in
        Hashtbl.replace table datom.a (datom :: existing))
      duplicate_datoms;
    Hashtbl.iter (fun attr datoms -> Hashtbl.replace table attr (List.rev datoms)) table;
    table
  in
  let duplicate_aevt_datoms = List.sort (Util.compare_datom Aevt) duplicate_datoms in
  let duplicate_avet_datoms =
    duplicate_datoms
    |> List.filter (fun datom -> Schema.schema_attr_is_avet_accessible schema datom.a)
    |> List.sort (Util.compare_datom Avet)
  in
  Some
    { db_uid = context.next_db_uid ()
    ; schema
    ; eavt_index = Index.empty Eavt lmdb
    ; aevt_index = Index.empty Aevt lmdb
    ; avet_index = Index.empty Avet lmdb
    ; aevt_by_attr = Hashtbl.create 0
    ; avet_by_attr = Hashtbl.create 0
    ; avet_entities_by_attr_value = Hashtbl.create 0
    ; duplicate_datoms
    ; duplicate_aevt_datoms
    ; duplicate_avet_datoms
    ; duplicate_eavt_by_entity
    ; duplicate_aevt_by_attr = duplicate_datoms_by_attr duplicate_aevt_datoms
    ; duplicate_avet_by_attr = duplicate_datoms_by_attr duplicate_avet_datoms
    ; max_eid
    ; max_datom_e = max_eid
    ; max_tx
    ; store_max_tx = max_tx
    ; as_of_tx = None
    ; since_tx = None
    ; history = false
    ; filter_pred = None
    ; pending_datoms = []
    ; storage_ref = Some storage
    ; tx_fns = []
    }

let storage (db : db) = db.storage_ref

let settings (_db : db) =
  [ "branching-factor", Int 32
  ; "ref-type", Keyword "weak"
  ; "storage", Bool (Option.is_some _db.storage_ref)
  ]

let collect_garbage _storage = ()
