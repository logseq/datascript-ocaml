open Datascript_types

type context =
  { datoms_by_entity : db -> entity_id -> datom list
  ; all_datoms : db -> datom list
  ; compare_value : value -> value -> int
  ; cardinality : db -> attr -> cardinality
  ; is_ref_attr : db -> attr -> bool
  ; is_component : db -> attr -> bool
  ; reverse_ref : attr -> attr
  ; is_reverse_ref : attr -> bool
  ; entity_id_of_ref : db -> entity_ref -> entity_id option
  }

let tx_value_of_attr_values context db attr values =
  let values = List.sort context.compare_value values in
  match context.cardinality db attr, values with
  | Many, values -> Many_values values
  | One, value :: _ -> One_value value
  | One, [] -> Many_values []

let entity_has_forward_attrs context db entity_id =
  context.datoms_by_entity db entity_id <> []

let entity_visible_attr_values context db attr values =
  if context.is_ref_attr db attr then
    values
    |> List.filter (function
      | Ref entity_id -> entity_has_forward_attrs context db entity_id
      | _ -> true)
  else
    values

let group_forward_entity_attrs context db entity_id =
  let add_attr groups d =
    match List.assoc_opt d.a groups with
    | None -> (d.a, [ d.v ]) :: groups
    | Some values -> (d.a, d.v :: values) :: List.remove_assoc d.a groups
  in
  context.datoms_by_entity db entity_id
  |> List.fold_left add_attr []
  |> List.filter_map (fun (attr, values) ->
    match entity_visible_attr_values context db attr values with
    | [] -> None
    | values -> Some (attr, tx_value_of_attr_values context db attr values))

let group_reverse_entity_attrs context db entity_id =
  context.all_datoms db
  |> List.filter_map (fun d ->
    match d.v with
    | Ref ref_id when ref_id = entity_id -> Some (context.reverse_ref d.a, d.a, Ref d.e)
    | _ -> None)
  |> List.fold_left
       (fun groups (reverse_attr, forward_attr, value) ->
         match List.assoc_opt reverse_attr groups with
         | None -> (reverse_attr, (forward_attr, [ value ])) :: groups
         | Some (_, values) ->
           (reverse_attr, (forward_attr, value :: values)) :: List.remove_assoc reverse_attr groups)
       []
  |> List.map (fun (attr, (forward_attr, values)) ->
    let values = List.sort context.compare_value values in
    if context.is_component db forward_attr then
      match values with
      | value :: _ -> attr, One_value value
      | [] -> attr, Many_values []
    else
      attr, Many_values values)

let group_entity_attrs context db entity_id =
  match group_forward_entity_attrs context db entity_id with
  | [] -> []
  | forward_attrs ->
    forward_attrs
    @ group_reverse_entity_attrs context db entity_id
    |> List.sort (fun (left, _) (right, _) -> compare left right)

let entity context db entity_ref =
  match context.entity_id_of_ref db entity_ref with
  | None -> None
  | Some entity_id ->
    (match group_entity_attrs context db entity_id with
     | [] -> None
     | attrs -> Some { id = entity_id; db; attrs })

let entity_attr_raw (entity : entity) = function
  | "db/id" -> Some (One_value (Int entity.id))
  | attr -> List.assoc_opt attr entity.attrs

let rec materialized_tx_entity context db visited entity_id =
  if List.mem entity_id visited then
    Some { db_id = Some (Entity_id entity_id); attrs = [] }
  else
    match entity context db (Entity_id entity_id) with
    | None -> None
    | Some entity ->
      let attrs = List.filter (fun (attr, _) -> not (context.is_reverse_ref attr)) entity.attrs in
      Some { db_id = Some (Entity_id entity_id); attrs }

and materialize_ref_values context db visited = function
  | One_value (Ref entity_id) ->
    (match materialized_tx_entity context db visited entity_id with
     | Some entity -> One_entity entity
     | None -> One_value (Ref entity_id))
  | Many_values values
    when List.for_all (function Ref _ -> true | _ -> false) values ->
    let entities =
      values
      |> List.filter_map (function
        | Ref entity_id -> materialized_tx_entity context db visited entity_id
        | _ -> None)
      |> List.sort (fun left right -> compare left.db_id right.db_id)
    in
    if entities = [] && values <> [] then Many_values values else Many_entities entities
  | value -> value

let entity_attr context (entity : entity) attr =
  entity_attr_raw entity attr
  |> Option.map (materialize_ref_values context entity.db [ entity.id ])

let entity_db (entity : entity) = entity.db

let is_entity (_ : entity) = true

let entity_equal (left : entity) (right : entity) =
  left.id = right.id && left.db.db_uid = right.db.db_uid

let entity_hash (entity : entity) =
  Hashtbl.hash (entity.db.db_uid, entity.id)

let touch context ent =
  let rec touch_entity visited (entity : entity) =
    let attrs =
      entity.attrs
      |> List.map (fun (attr, tx_value) -> attr, touch_attr_value entity.db visited attr tx_value)
    in
    { entity with attrs }
  and touch_attr_value db visited attr tx_value =
    let component_attr = if context.is_reverse_ref attr then context.reverse_ref attr else attr in
    if not (context.is_component db component_attr) then
      tx_value
    else
      match tx_value with
      | One_value (Ref entity_id) ->
        (match touched_tx_entity db visited entity_id with
         | Some entity -> One_entity entity
         | None -> tx_value)
      | Many_values values ->
        let entities =
          values
          |> List.filter_map (function
            | Ref entity_id -> touched_tx_entity db visited entity_id
            | _ -> None)
          |> List.sort (fun left right -> compare left.db_id right.db_id)
        in
        if entities = [] && values <> [] then tx_value else Many_entities entities
      | One_value _ | One_entity _ | Many_entities _ -> tx_value
  and touched_tx_entity db visited entity_id =
    if List.mem entity_id visited then
      Some { db_id = Some (Entity_id entity_id); attrs = [] }
    else
      match entity context db (Entity_id entity_id) with
      | None -> None
      | Some entity ->
        let touched = touch_entity (entity_id :: visited) entity in
        let attrs = List.filter (fun (attr, _) -> not (context.is_reverse_ref attr)) touched.attrs in
        Some { db_id = Some (Entity_id touched.id); attrs }
  in
  touch_entity [ ent.id ] ent
