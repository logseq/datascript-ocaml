open Datascript_types

type resolution = attr * value * entity_id

type context =
  { is_unique_identity : db -> attr -> bool
  ; entid_in_datoms : db -> datom list -> attr -> value -> entity_id option
  ; value_to_string : value -> string
  }

let lookup_ref_string context attr value =
  "[:" ^ attr ^ " " ^ context.value_to_string value ^ "]"

let conflicting_upserts_message context (left_attr, left_value, left_e) (right_attr, right_value, right_e) =
  "Conflicting upserts: "
  ^ lookup_ref_string context left_attr left_value
  ^ " resolves to "
  ^ string_of_int left_e
  ^ ", but "
  ^ lookup_ref_string context right_attr right_value
  ^ " resolves to "
  ^ string_of_int right_e

let explicit_conflict_message context attr value target_e entity_id =
  "Conflicting upsert: "
  ^ lookup_ref_string context attr value
  ^ " resolves to "
  ^ string_of_int target_e
  ^ ", but entity already has :db/id "
  ^ string_of_int entity_id

let identity_resolutions context db datoms attrs =
  attrs
  |> List.concat_map (function
    | attr, One_value value when context.is_unique_identity db attr ->
      (match context.entid_in_datoms db datoms attr value with
       | Some entity_id -> [ attr, value, entity_id ]
       | None -> [])
    | attr, Many_values values when context.is_unique_identity db attr ->
      values
      |> List.filter_map (fun value ->
        match context.entid_in_datoms db datoms attr value with
        | Some entity_id -> Some (attr, value, entity_id)
        | None -> None)
    | _ -> [])

let conflicting_identity_resolution = function
  | [] | [ _ ] -> None
  | first :: rest ->
    rest
    |> List.find_opt (fun (_, _, entity_id) ->
      let _, _, first_entity_id = first in
      entity_id <> first_entity_id)
    |> Option.map (fun conflict -> first, conflict)

let validate_explicit_target context db datoms entity_id attrs =
  let resolutions = identity_resolutions context db datoms attrs in
  match conflicting_identity_resolution resolutions with
  | Some (left, right) -> invalid_arg (conflicting_upserts_message context left right)
  | None ->
    resolutions
    |> List.iter (fun (attr, value, target_e) ->
      if target_e <> entity_id then
        invalid_arg (explicit_conflict_message context attr value target_e entity_id))

let entity_unique_identity context db datoms attrs =
  let attr_value attr =
    match List.assoc_opt attr attrs with
    | Some (One_value value) -> Some value
    | Some (Many_values (value :: _)) -> Some value
    | _ -> None
  in
  let direct_resolutions = identity_resolutions context db datoms attrs in
  let direct_identity =
    match conflicting_identity_resolution direct_resolutions with
    | Some (left, right) -> invalid_arg (conflicting_upserts_message context left right)
    | None ->
      (match direct_resolutions with
       | [] -> None
       | (_, _, target_e) :: _ -> Some target_e)
  in
  match direct_identity with
  | Some _ as identity -> identity
  | None ->
    db.schema
    |> List.find_map (fun (attr, schema_attr) ->
      match schema_attr.unique, schema_attr.tuple_attrs with
      | Some Identity, Some source_attrs ->
        let values = List.map attr_value source_attrs in
        if List.for_all Option.is_some values then
          context.entid_in_datoms db datoms attr (Tuple values)
        else
          None
      | _ -> None)
