open Datascript_types

module Make (Context : sig
  val lookup_ref_entity_id : db -> attr -> value -> entity_id option
  val entid : db -> attr -> value -> entity_id option
  val ident_attr : attr
  val normalize_value : value -> value
end) = struct
  open Context

  let rec entity_id_of_ref db = function
    | Entity_id entity_id -> Some entity_id
    | Lookup_ref (attr, value) ->
      (match resolve_ref_value db value with
       | Some value -> lookup_ref_entity_id db attr value
       | None -> None)
    | Ident ident -> entid db ident_attr (Keyword ident)
    | CurrentTx -> None
    | Temp_id _ -> None
  
  and resolve_ref_value ?(preserve_vector = false) db = function
    | Ref_to entity_ref -> Option.map (fun entity_id -> Ref entity_id) (entity_id_of_ref db entity_ref)
    | List values ->
      let rec resolve_values acc = function
        | [] -> Some (normalize_value (List (List.rev acc)))
        | value :: rest ->
          (match resolve_ref_value ~preserve_vector:true db value with
           | Some value -> resolve_values (value :: acc) rest
           | None -> None)
      in
      resolve_values [] values
    | Vector values ->
      let rec resolve_values acc = function
        | [] ->
          let values = List.rev acc in
          Some (normalize_value (if preserve_vector then Vector values else List values))
        | value :: rest ->
          (match resolve_ref_value ~preserve_vector:true db value with
           | Some value -> resolve_values (value :: acc) rest
           | None -> None)
      in
      resolve_values [] values
    | Map entries ->
      let rec resolve_entries acc = function
        | [] -> Some (normalize_value (Map (List.rev acc)))
        | (key, value) :: rest ->
          (match
             resolve_ref_value ~preserve_vector:true db key,
             resolve_ref_value ~preserve_vector:true db value
           with
           | Some key, Some value -> resolve_entries ((key, value) :: acc) rest
           | _ -> None)
      in
      resolve_entries [] entries
    | Set values ->
      let rec resolve_values acc = function
        | [] -> Some (normalize_value (Set (List.rev acc)))
        | value :: rest ->
          (match resolve_ref_value ~preserve_vector:true db value with
           | Some value -> resolve_values (value :: acc) rest
           | None -> None)
      in
      resolve_values [] values
    | Tuple values ->
      let rec resolve_values acc = function
        | [] -> Some (normalize_value (Tuple (List.rev acc)))
        | None :: rest -> resolve_values (None :: acc) rest
        | Some value :: rest ->
          (match resolve_ref_value ~preserve_vector:true db value with
           | Some value -> resolve_values (Some value :: acc) rest
           | None -> None)
      in
      resolve_values [] values
    | value -> Some (normalize_value value)
  
end
