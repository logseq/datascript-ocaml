open Datascript_types

module Index = Index
module Schema = Schema

type context =
  { next_db_uid : unit -> int
  ; validate_schema : schema -> schema
  ; normalize_datom_for_schema : schema -> datom -> datom
  ; refresh_db_indexes : db -> db
  }

let serializable db =
  { serializable_schema = db.schema
  ; serializable_datoms =
      Index.to_list db.eavt_index @ db.duplicate_datoms |> List.sort (Datascript_types.Compare.compare_datom Eavt)
  ; serializable_max_eid = db.max_eid
  ; serializable_max_tx = db.max_tx
  }

let duplicate_datoms datoms =
  let datoms = List.sort (Datascript_types.Compare.compare_datom Eavt) datoms in
  let rec loop previous duplicates = function
    | [] -> List.rev duplicates
    | datom :: rest ->
      (match previous with
       | Some previous when Datascript_types.Compare.compare_datom Eavt previous datom = 0 ->
         loop (Some datom) (datom :: duplicates) rest
       | _ -> loop (Some datom) duplicates rest)
  in
  loop None [] datoms

let duplicate_aevt_datoms duplicate_datoms =
  List.sort (Datascript_types.Compare.compare_datom Aevt) duplicate_datoms

let duplicate_avet_datoms schema duplicate_datoms =
  duplicate_datoms
  |> List.filter (fun datom -> Schema.schema_attr_is_avet_accessible schema datom.a)
  |> List.sort (Datascript_types.Compare.compare_datom Avet)

let duplicate_eavt_by_entity duplicate_datoms =
  let table = Hashtbl.create 1024 in
  List.iter
    (fun datom ->
      let existing = Option.value (Hashtbl.find_opt table datom.e) ~default:[] in
      Hashtbl.replace table datom.e (datom :: existing))
    duplicate_datoms;
  Hashtbl.iter (fun entity_id datoms -> Hashtbl.replace table entity_id (List.rev datoms)) table;
  table

let duplicate_datoms_by_attr duplicate_datoms =
  let table = Hashtbl.create 1024 in
  List.iter
    (fun datom ->
      let existing = Option.value (Hashtbl.find_opt table datom.a) ~default:[] in
      Hashtbl.replace table datom.a (datom :: existing))
    duplicate_datoms;
  Hashtbl.iter (fun attr datoms -> Hashtbl.replace table attr (List.rev datoms)) table;
  table

let from_serializable context snapshot =
  let schema = context.validate_schema snapshot.serializable_schema in
  let datoms = List.map (context.normalize_datom_for_schema schema) snapshot.serializable_datoms in
  let duplicate_datoms = duplicate_datoms datoms in
  let duplicate_aevt_datoms = duplicate_aevt_datoms duplicate_datoms in
  let duplicate_avet_datoms = duplicate_avet_datoms schema duplicate_datoms in
  let lmdb, storage_ref = Index.create_lmdb None in
  { db_uid = context.next_db_uid ()
  ; schema
  ; eavt_index = Index.empty Eavt lmdb
  ; aevt_index = Index.empty Aevt lmdb
  ; avet_index = Index.empty Avet lmdb
  ; aevt_by_attr = Hashtbl.create 0
  ; avet_by_attr = Hashtbl.create 0
  ; duplicate_datoms
  ; duplicate_aevt_datoms
  ; duplicate_avet_datoms
  ; duplicate_eavt_by_entity = duplicate_eavt_by_entity duplicate_datoms
  ; duplicate_aevt_by_attr = duplicate_datoms_by_attr duplicate_aevt_datoms
  ; duplicate_avet_by_attr = duplicate_datoms_by_attr duplicate_avet_datoms
  ; max_eid = snapshot.serializable_max_eid
  ; max_datom_e = 0
  ; max_tx = snapshot.serializable_max_tx
  ; filter_pred = None
  ; storage_ref
  ; tx_fns = []
  }
  |> context.refresh_db_indexes
