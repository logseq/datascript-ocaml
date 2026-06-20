open Datascript_types

type context =
  { datoms_by_entity : db -> entity_id -> datom list
  ; datoms_by_avet_ref : db -> attr -> entity_id -> datom list
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

let sorted_forward_entity_attrs context db entity_id =
  group_forward_entity_attrs context db entity_id
  |> List.sort (fun (left, _) (right, _) -> compare left right)

let forward_entity_attr context db entity_id attr =
  context.datoms_by_entity db entity_id
  |> List.filter_map (fun d -> if d.a = attr then Some d.v else None)
  |> entity_visible_attr_values context db attr
  |> function
  | [] -> None
  | values -> Some (tx_value_of_attr_values context db attr values)

let reverse_entity_attr context db entity_id attr =
  let forward_attr = context.reverse_ref attr in
  let values =
    context.datoms_by_avet_ref db forward_attr entity_id
    |> List.map (fun d -> Ref d.e)
    |> List.sort context.compare_value
  in
  match values with
  | [] -> None
  | value :: _ when context.is_component db forward_attr -> Some (One_value value)
  | values -> Some (Many_values values)

let lazy_entity context db entity_id =
  let materialized = lazy (group_entity_attrs context db entity_id) in
  { id = entity_id
  ; db
  ; attrs = []
  ; lookup_attr =
      (fun attr ->
        if context.is_reverse_ref attr then
          reverse_entity_attr context db entity_id attr
        else
          forward_entity_attr context db entity_id attr)
  ; materialize_attrs = (fun () -> Lazy.force materialized)
  }

let materialized_entity context db entity_id attrs =
  { id = entity_id
  ; db
  ; attrs
  ; lookup_attr =
      (fun attr ->
        match List.assoc_opt attr attrs with
        | Some value -> Some value
        | None ->
          if context.is_reverse_ref attr then
            reverse_entity_attr context db entity_id attr
          else
            None)
  ; materialize_attrs = (fun () -> attrs)
  }

let entity context db entity_ref =
  match context.entity_id_of_ref db entity_ref with
  | None -> None
  | Some entity_id ->
    if entity_has_forward_attrs context db entity_id then
      Some (lazy_entity context db entity_id)
    else
      None

let entity_attr_raw (entity : entity) = function
  | "db/id" -> Some (One_value (Int entity.id))
  | attr -> entity.lookup_attr attr

let rec materialized_tx_entity context db visited entity_id =
  if List.mem entity_id visited then
    Some { db_id = Some (Entity_id entity_id); attrs = [] }
  else
    match entity context db (Entity_id entity_id) with
    | None -> None
    | Some entity ->
      let attrs = sorted_forward_entity_attrs context db entity.id in
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

let entity_attrs (entity : entity) = entity.materialize_attrs ()

let is_entity (_ : entity) = true

let entity_equal (left : entity) (right : entity) =
  left.id = right.id && left.db.db_uid = right.db.db_uid

let entity_hash (entity : entity) =
  Hashtbl.hash (entity.db.db_uid, entity.id)

let touch context ent =
  let rec touch_entity visited (entity : entity) =
    let attrs =
      entity.materialize_attrs ()
      |> List.map (fun (attr, tx_value) -> attr, touch_attr_value entity.db visited attr tx_value)
    in
    materialized_entity context entity.db entity.id attrs
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
        let attrs =
          sorted_forward_entity_attrs context db entity.id
          |> List.map (fun (attr, tx_value) -> attr, touch_attr_value entity.db (entity_id :: visited) attr tx_value)
        in
        Some { db_id = Some (Entity_id entity.id); attrs }
  in
  touch_entity [ ent.id ] ent
