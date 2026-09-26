open Datascript_types

module Index = Index

type tail_context =
  { apply_group : db -> datom list -> db
  }

type restore_context = { next_db_uid : unit -> int }

let memory_storage = Datascript_storage_lmdb.memory_storage
let file_storage = Datascript_storage_lmdb.file_storage

let store ?storage db =
  match storage, db.storage_ref with
  | Some storage, _ | None, Some storage ->
      let lmdb = Datascript_storage_lmdb.lmdb storage in
      Datascript_storage_lmdb.store_meta lmdb db
  | None, None -> invalid_arg "db has no attached storage"

let store_tail _storage _tail = ()

let tail_compaction_threshold = 0
let tail_datom_count _tail = 0

let restore_tail_groups _storage = []

let restore_root_snapshot storage =
  let lmdb = Datascript_storage_lmdb.lmdb storage in
  let schema, max_eid, max_tx, duplicate_datoms =
    Datascript_storage_lmdb.restore_meta lmdb
  in
  Some
    { serializable_schema = schema
    ; serializable_datoms = Index.to_list (Index.empty Eavt lmdb) @ duplicate_datoms
    ; serializable_max_eid = max_eid
    ; serializable_max_tx = max_tx
    }

let db_with_tail _context db tail =
  List.fold_left
    (fun db group ->
      match group with
      | [] -> db
      | _ -> Db.refresh_indexes_with_tx_data db group)
    db
    tail

let restore context storage =
  let lmdb = Datascript_storage_lmdb.lmdb storage in
  let schema, max_eid, max_tx, duplicate_datoms =
    Datascript_storage_lmdb.restore_meta lmdb
  in
  let schema = Schema.validate_schema schema in
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
    ; tave_index = Index.empty Tave lmdb
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

let storage_addresses storage = storage.storage_list_addresses ()
let storage (db : db) = db.storage_ref

let storage_root_addresses storage = storage.storage_list_addresses ()

let addresses dbs =
  dbs
  |> List.concat_map (fun db ->
    match db.storage_ref with
    | None -> []
    | Some storage -> storage_root_addresses storage)
  |> List.sort_uniq compare

let settings (db : db) =
  [ "branching-factor", Int 32
  ; "ref-type", Keyword "weak"
  ; "storage", Bool (Option.is_some db.storage_ref)
  ]

let collect_garbage storage =
  let lmdb = Datascript_storage_lmdb.lmdb storage in
  Datascript_storage_lmdb.sync lmdb
