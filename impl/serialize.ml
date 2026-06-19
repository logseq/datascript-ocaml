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
  ; serializable_datoms =
      PSet.to_list db.eavt_index @ db.duplicate_datoms |> List.sort (Util.compare_datom Eavt)
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

let duplicate_datoms datoms =
  let datoms = List.sort (Util.compare_datom Eavt) datoms in
  let rec loop previous duplicates = function
    | [] -> List.rev duplicates
    | datom :: rest ->
      (match previous with
       | Some previous when Util.compare_datom Eavt previous datom = 0 ->
         loop (Some datom) (datom :: duplicates) rest
       | _ -> loop (Some datom) duplicates rest)
  in
  loop None [] datoms

let duplicate_aevt_datoms duplicate_datoms =
  List.sort (Util.compare_datom Aevt) duplicate_datoms

let duplicate_avet_datoms schema duplicate_datoms =
  duplicate_datoms
  |> List.filter (fun datom -> Schema.schema_attr_is_avet_accessible schema datom.a)
  |> List.sort (Util.compare_datom Avet)

let duplicate_eavt_by_entity duplicate_datoms =
  let table = Hashtbl.create 1024 in
  List.iter
    (fun datom ->
      let existing = Option.value (Hashtbl.find_opt table datom.e) ~default:[] in
      Hashtbl.replace table datom.e (datom :: existing))
    duplicate_datoms;
  Hashtbl.iter (fun entity_id datoms -> Hashtbl.replace table entity_id (List.rev datoms)) table;
  table

let from_serializable context snapshot =
  let schema = context.validate_schema snapshot.serializable_schema in
  let datoms = List.map (context.normalize_datom_for_schema schema) snapshot.serializable_datoms in
  let duplicate_datoms = duplicate_datoms datoms in
  { db_uid = context.next_db_uid ()
  ; schema
  ; eavt_index = index_from_datoms Eavt datoms
  ; aevt_index = empty_index Aevt
  ; avet_index = empty_index Avet
  ; duplicate_datoms
  ; duplicate_aevt_datoms = duplicate_aevt_datoms duplicate_datoms
  ; duplicate_avet_datoms = duplicate_avet_datoms schema duplicate_datoms
  ; duplicate_eavt_by_entity = duplicate_eavt_by_entity duplicate_datoms
  ; max_eid = snapshot.serializable_max_eid
  ; max_datom_e = 0
  ; max_tx = snapshot.serializable_max_tx
  ; filter_pred = None
  ; storage_ref = None
  ; tx_fns = []
  }
  |> context.refresh_db_indexes
