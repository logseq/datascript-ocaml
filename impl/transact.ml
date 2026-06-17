open Datascript_types

type context =
  { validate_entity_id : int -> entity_id
  ; entid_in_datoms : db -> datom list -> attr -> value -> entity_id option
  ; ident_attr : attr
  ; allocate_entity_id : entity_id -> entity_id
  ; lookup_ref_entity_id_in_datoms : strict_missing:bool -> db -> datom list -> attr -> value -> entity_id option
  ; unresolved_lookup_ref_message : attr -> value -> string
  ; normalize_value : value -> value
  ; is_ref_attr : db -> attr -> bool
  ; is_reverse_ref : attr -> bool
  ; reverse_ref : attr -> attr
  ; cardinality : db -> attr -> cardinality
  ; max_eid_in_value : int -> value -> int
  }

let remember_tempid tempids tempid eid =
  match List.assoc_opt tempid tempids with
  | Some existing when existing = eid -> tempids
  | Some _ -> invalid_arg ("conflicting tempid: " ^ tempid)
  | None -> tempids @ [ tempid, eid ]

let remember_current_tx tempids tx =
  remember_tempid tempids "db/current-tx" tx

let ensure_current_tx_tempid tempids tx =
  ("db/current-tx", tx) :: List.remove_assoc "db/current-tx" tempids

let is_current_tx_alias = function
  | ":db/current-tx" | "datomic.tx" | "datascript.tx" -> true
  | _ -> false

let remember_current_tx_alias tempids tx alias =
  let tempids = ensure_current_tx_tempid tempids tx in
  remember_tempid tempids alias tx

let rec resolve_entity_ref context db datoms tx max_eid tempids = function
  | Entity_id e ->
    let e = context.validate_entity_id e in
    e, max max_eid e, tempids
  | CurrentTx -> tx, max_eid, remember_current_tx tempids tx
  | Ident ident ->
    (match context.entid_in_datoms db datoms context.ident_attr (Keyword ident) with
     | Some e -> e, max max_eid e, tempids
     | None -> invalid_arg "ident did not resolve")
  | Temp_id tempid ->
    if is_current_tx_alias tempid then
      tx, max_eid, remember_current_tx_alias tempids tx tempid
    else
      (match List.assoc_opt tempid tempids with
       | Some e -> e, max_eid, tempids
       | None ->
         let e = context.allocate_entity_id max_eid in
         e, e, remember_tempid tempids tempid e)
  | Lookup_ref (attr, value) ->
    let value, max_eid, tempids = resolve_value context db datoms tx max_eid tempids value in
    (match context.lookup_ref_entity_id_in_datoms ~strict_missing:true db datoms attr value with
     | Some e -> e, max max_eid e, tempids
     | None -> invalid_arg (context.unresolved_lookup_ref_message attr value))

and resolve_value context db datoms tx max_eid tempids = function
  | TxRef -> Ref tx, max_eid, remember_current_tx tempids tx
  | Ref e ->
    let e = context.validate_entity_id e in
    Ref e, max max_eid e, tempids
  | Ref_to entity_ref ->
    let e, max_eid, tempids = resolve_entity_ref context db datoms tx max_eid tempids entity_ref in
    Ref e, max_eid, tempids
  | List values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value context db datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    context.normalize_value (List (List.rev values)), max_eid, tempids
  | Vector values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value context db datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    context.normalize_value (Vector (List.rev values)), max_eid, tempids
  | Map entries ->
    let entries, max_eid, tempids =
      List.fold_left
        (fun (entries, max_eid, tempids) (key, value) ->
          let key, max_eid, tempids = resolve_value context db datoms tx max_eid tempids key in
          let value, max_eid, tempids = resolve_value context db datoms tx max_eid tempids value in
          (key, value) :: entries, max_eid, tempids)
        ([], max_eid, tempids)
        entries
    in
    context.normalize_value (Map (List.rev entries)), max_eid, tempids
  | Set values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value context db datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    context.normalize_value (Set (List.rev values)), max_eid, tempids
  | Tuple values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          match value with
          | None -> None :: values, max_eid, tempids
          | Some value ->
            let value, max_eid, tempids = resolve_value context db datoms tx max_eid tempids value in
            Some value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    context.normalize_value (Tuple (List.rev values)), max_eid, tempids
  | value -> value, max_eid, tempids

let attr_name_of_value = function
  | Keyword attr | String attr | Symbol attr -> Some attr
  | _ -> None

let entity_ref_of_ref_attr_value = function
  | TxRef -> Some CurrentTx
  | Ref entity_id -> Some (Entity_id entity_id)
  | Ref_to entity_ref -> Some entity_ref
  | Int entity_id when entity_id < 0 -> Some (Temp_id (string_of_int entity_id))
  | Int entity_id -> Some (Entity_id entity_id)
  | String tempid -> Some (Temp_id tempid)
  | Keyword "db/current-tx" -> Some CurrentTx
  | Keyword ident -> Some (Ident ident)
  | Symbol "db/current-tx" -> Some CurrentTx
  | Symbol ("datomic.tx" | "datascript.tx" as tempid) -> Some (Temp_id tempid)
  | List [ attr; value ] | Vector [ attr; value ] ->
    attr_name_of_value attr |> Option.map (fun attr -> Lookup_ref (attr, value))
  | _ -> None

let ref_attr_for_value_resolution context db attr =
  if context.is_ref_attr db attr then
    Some attr
  else if context.is_reverse_ref attr && context.is_ref_attr db (context.reverse_ref attr) then
    Some (context.reverse_ref attr)
  else
    None

let resolve_value_for_attr context db attr datoms tx max_eid tempids value =
  match ref_attr_for_value_resolution context db attr, entity_ref_of_ref_attr_value value with
  | Some _, Some entity_ref ->
    let entity_id, max_eid, tempids = resolve_entity_ref context db datoms tx max_eid tempids entity_ref in
    Ref entity_id, max_eid, tempids
  | Some _, None -> invalid_arg "Expected number or lookup ref for entity id"
  | _ ->
    resolve_value context db datoms tx max_eid tempids value

let attr_expands_collection context db attr =
  context.cardinality db attr = Many || context.is_reverse_ref attr

let ref_lookup_collection_value = function
  | (List _ | Vector _) as value ->
    (match entity_ref_of_ref_attr_value value with
     | Some _ -> true
     | None -> false)
  | _ -> false

let resolve_existing_entity_ref context db datoms tx max_eid tempids = function
  | Temp_id _ -> invalid_arg "Tempids are allowed in :db/add only"
  | entity_ref -> resolve_entity_ref context db datoms tx max_eid tempids entity_ref

let resolve_optional_existing_entity_ref context db datoms tx max_eid tempids = function
  | Temp_id _ -> invalid_arg "Tempids are allowed in :db/add only"
  | Lookup_ref (attr, value) ->
    let value, max_eid, tempids = resolve_value context db datoms tx max_eid tempids value in
    (match context.lookup_ref_entity_id_in_datoms ~strict_missing:false db datoms attr value with
     | Some e -> Some e, max max_eid e, tempids
     | None -> None, max_eid, tempids)
  | entity_ref ->
    let e, max_eid, tempids = resolve_entity_ref context db datoms tx max_eid tempids entity_ref in
    Some e, max_eid, tempids

let resolve_tx_value_for_attr context db attr datoms tx max_eid tempids = function
  | One_value ((List values | Vector values) as value) when attr_expands_collection context db attr && not (ref_lookup_collection_value value) ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value_for_attr context db attr datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    Many_values (List.rev values), max_eid, tempids
  | One_value (Set values) when attr_expands_collection context db attr ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value_for_attr context db attr datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    Many_values (List.rev values), max_eid, tempids
  | One_value value ->
    let value, max_eid, tempids = resolve_value_for_attr context db attr datoms tx max_eid tempids value in
    One_value value, max_eid, tempids
  | Many_values values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value_for_attr context db attr datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    Many_values (List.rev values), max_eid, tempids
  | One_entity entity -> One_entity entity, max_eid, tempids
  | Many_entities entities -> Many_entities entities, max_eid, tempids

let resolve_optional_value_for_attr context db attr datoms tx max_eid tempids = function
  | Some value ->
    let value, max_eid, tempids = resolve_value_for_attr context db attr datoms tx max_eid tempids value in
    Some value, max_eid, tempids
  | None -> None, max_eid, tempids

let resolve_entity_attrs context db datoms tx max_eid tempids attrs =
  let attrs, max_eid, tempids =
    List.fold_left
      (fun (attrs, max_eid, tempids) (attr, tx_value) ->
        let tx_value, max_eid, tempids = resolve_tx_value_for_attr context db attr datoms tx max_eid tempids tx_value in
        (attr, tx_value) :: attrs, max_eid, tempids)
      ([], max_eid, tempids)
      attrs
  in
  List.rev attrs, max_eid, tempids

let rec remap_value_ref context old_e new_e = function
  | Ref entity_id when entity_id = old_e -> Ref new_e
  | List values ->
    List (List.map (remap_value_ref context old_e new_e) values)
  | Vector values ->
    Vector (List.map (remap_value_ref context old_e new_e) values)
  | Map entries ->
    Map
      (List.map
         (fun (key, value) ->
           remap_value_ref context old_e new_e key, remap_value_ref context old_e new_e value)
         entries)
  | Set values ->
    context.normalize_value (Set (List.map (remap_value_ref context old_e new_e) values))
  | Tuple values ->
    Tuple
      (List.map
         (function
           | None -> None
           | Some value -> Some (remap_value_ref context old_e new_e value))
         values)
  | value -> value

let remap_datom_entity context old_e new_e d =
  { d with
    e = if d.e = old_e then new_e else d.e
  ; v = remap_value_ref context old_e new_e d.v
  }

let remap_resolved_tx_value context old_e new_e = function
  | One_value value -> One_value (remap_value_ref context old_e new_e value)
  | Many_values values -> Many_values (List.map (remap_value_ref context old_e new_e) values)
  | nested -> nested

let remap_tempid_entity old_e new_e tempids =
  List.map
    (fun (tempid, entity_id) ->
      if entity_id = old_e then
        tempid, new_e
      else
        tempid, entity_id)
    tempids
