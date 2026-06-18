open Datascript_types

module Make (Context : sig
  val schema_attr : db -> attr -> schema_attr option
  val cardinality : db -> attr -> cardinality
  val is_unique : db -> attr -> bool
  val tuple_attrs : db -> attr -> attr list option
  val is_tuple_attr : db -> attr -> bool
  val is_component : db -> attr -> bool
  val is_ref_attr : db -> attr -> bool
  val tuple_attrs_for_source : db -> attr -> (attr * attr list) list
  val is_reverse_ref : attr -> bool
  val reverse_ref : attr -> attr
  val value_equal : value -> value -> bool
  val same_fact : datom -> datom -> bool
  val datom : ?tx:tx -> ?added:bool -> e:entity_id -> a:attr -> v:value -> unit -> datom
  val normalize_value : value -> value
  val validate_entity_id : int -> entity_id
  val max_allocatable_entity_id : int
  val visible_datoms : db -> datom list
end) = struct
  open Context

  let without_entity_attr e a datoms =
    List.filter (fun d -> d.e <> e || d.a <> a) datoms
  
  let without_fact e a value datoms =
    List.filter (fun d -> d.e <> e || d.a <> a || not (value_equal d.v value)) datoms
  
  let has_unique_conflict db datoms d =
    is_unique db d.a
    && List.exists (fun existing -> existing.e <> d.e && existing.a = d.a && value_equal existing.v d.v) datoms
  
  let entity_attr_datoms datoms e a =
    List.filter (fun d -> d.e = e && d.a = a) datoms
  
  let current_attr_value datoms e a =
    match entity_attr_datoms datoms e a with
    | [] -> None
    | d :: _ -> Some d.v
  
  let retraction_datom tx d = { d with tx; added = false }
  
  let compare_eavt_datom left right =
    compare
      (left.e, left.a, left.v, left.tx)
      (right.e, right.a, right.v, right.tx)
  
  let sorted_retractions tx datoms =
    datoms
    |> List.sort compare_eavt_datom
    |> List.map (retraction_datom tx)
  
  let validate_datom_value db d =
    if d.v = Nil then invalid_arg "Cannot store nil as a value";
    let value_matches_type value value_type =
      match value_type, value with
      | RefType, Ref _ -> true
      | TupleType, Tuple _ -> true
      | StringType, String _ -> true
      | KeywordType, Keyword _ -> true
      | NumberType, (Int _ | Float _) -> true
      | UuidType, Uuid _ -> true
      | InstantType, Instant _ -> true
      | _ -> false
    in
    let validate_tuple_types attr values types =
      if List.length values <> List.length types then
        invalid_arg ("tuple attribute value arity mismatch: " ^ attr);
      List.iter2
        (fun value value_type ->
          match value with
          | None -> ()
          | Some value ->
            if not (value_matches_type value value_type) then
              invalid_arg ("tuple attribute element type mismatch: " ^ attr))
        values
        types
    in
    match schema_attr db d.a with
    | Some { value_type = Some RefType; _ } ->
      (match d.v with
       | Ref _ -> ()
       | _ -> invalid_arg "Expected number or lookup ref for entity id")
    | Some { value_type = Some TupleType; tuple_types; _ } ->
      (match d.v with
       | Tuple values ->
         (match tuple_types with
          | Some types -> validate_tuple_types d.a values types
          | None -> ())
       | _ -> invalid_arg ("tuple attribute requires tuple value: " ^ d.a))
    | Some { value_type = Some StringType; _ } ->
      (match d.v with
       | String _ -> ()
       | _ -> invalid_arg ("string attribute requires string value: " ^ d.a))
    | Some { value_type = Some KeywordType; _ } ->
      (match d.v with
       | Keyword _ -> ()
       | _ -> invalid_arg ("keyword attribute requires keyword value: " ^ d.a))
    | Some { value_type = Some NumberType; _ } ->
      (match d.v with
       | Int _ | Float _ -> ()
       | _ -> invalid_arg ("number attribute requires numeric value: " ^ d.a))
    | Some { value_type = Some UuidType; _ } ->
      (match d.v with
       | Uuid _ -> ()
       | _ -> invalid_arg ("uuid attribute requires uuid value: " ^ d.a))
    | Some { value_type = Some InstantType; _ } ->
      (match d.v with
       | Instant _ -> ()
       | _ -> invalid_arg ("instant attribute requires instant value: " ^ d.a))
    | _ -> ()
  
  let value_option_equal left right =
    match left, right with
    | None, None -> true
    | Some left, Some right -> value_equal left right
    | None, Some _ | Some _, None -> false
  
  let tuple_direct_write_matches_sources db datoms d =
    match tuple_attrs db d.a, d.v with
    | Some source_attrs, Tuple values ->
      List.length source_attrs = List.length values
      && List.for_all Option.is_some values
      && List.for_all2
           (fun source_attr value -> value_option_equal (current_attr_value datoms d.e source_attr) value)
           source_attrs
           values
    | _ -> false
  
  let add_active_datom_with_report ?(allow_tuple = false) db tx datoms d =
    let d = { d with v = normalize_value d.v } in
    if is_tuple_attr db d.a && not allow_tuple then
      if tuple_direct_write_matches_sources db datoms d then datoms, []
      else invalid_arg "cannot modify tuple attributes directly"
    else begin
      validate_datom_value db d;
      if has_unique_conflict db datoms d then invalid_arg "unique constraint";
      if List.exists (same_fact d) datoms then datoms, []
      else
        match cardinality db d.a with
        | Many -> d :: datoms, [ d ]
        | One ->
          let removed = entity_attr_datoms datoms d.e d.a in
          let datoms = without_entity_attr d.e d.a datoms in
          d :: datoms, List.map (retraction_datom tx) removed @ [ d ]
    end
  
  let retract_active_datom datoms e a value =
    let value = Option.map normalize_value value in
    match value with
    | Some value -> without_fact e a value datoms
    | None -> without_entity_attr e a datoms
  
  let retract_active_datom_with_report tx datoms e a value =
    let value = Option.map normalize_value value in
    let removed =
      match value with
      | Some value -> List.filter (fun d -> d.e = e && d.a = a && value_equal d.v value) datoms
      | None -> entity_attr_datoms datoms e a
    in
    retract_active_datom datoms e a value, sorted_retractions tx removed
  
  let ref_value_id = function
    | Ref entity_id -> Some entity_id
    | _ -> None
  
  let rec component_entity_closure db datoms seen e =
    if List.mem e seen then seen
    else
      let seen = e :: seen in
      datoms
      |> List.filter (fun d -> d.e = e && is_component db d.a)
      |> List.fold_left
           (fun seen d ->
             match ref_value_id d.v with
             | Some child -> component_entity_closure db datoms seen child
             | None -> seen)
           seen
  
  let retracts_entity ids d =
    List.mem d.e ids
    ||
    match ref_value_id d.v with
    | Some entity_id -> List.mem entity_id ids
    | None -> false
  
  let retract_entities_with_report tx datoms ids =
    let removed = List.filter (retracts_entity ids) datoms in
    List.filter (fun d -> not (retracts_entity ids d)) datoms, sorted_retractions tx removed
  
  let retract_entity_with_report db tx datoms e =
    let ids = component_entity_closure db datoms [] e in
    retract_entities_with_report tx datoms ids
  
  let component_child_closure db datoms component_datoms =
    List.fold_left
      (fun ids d ->
        match ref_value_id d.v with
        | Some child -> component_entity_closure db datoms ids child
        | None -> ids)
      []
      component_datoms
  
  let retract_attr_with_report db tx datoms e a =
    if is_component db a then
      let attr_datoms = entity_attr_datoms datoms e a in
      let child_ids = component_child_closure db datoms attr_datoms in
      let removes d = (d.e = e && d.a = a) || retracts_entity child_ids d in
      let removed = List.filter removes datoms in
      List.filter (fun d -> not (removes d)) datoms, sorted_retractions tx removed
    else
      retract_active_datom_with_report tx datoms e a None
  
  let compare_and_set_matches db datoms e a expected =
    match cardinality db a, expected with
    | Many, Some expected ->
      entity_attr_datoms datoms e a
      |> List.exists (fun d -> value_equal d.v expected)
    | Many, None -> entity_attr_datoms datoms e a = []
    | One, Some expected ->
      (match current_attr_value datoms e a with
       | Some actual -> value_equal actual expected
       | None -> false)
    | One, None -> current_attr_value datoms e a = None
  
  let tuple_value datoms e source_attrs =
    Tuple (List.map (current_attr_value datoms e) source_attrs)
  
  let refresh_tuple_attrs_for_source db tx datoms e source_attr tx_data =
    tuple_attrs_for_source db source_attr
    |> List.fold_left
         (fun (datoms, tx_data) (tuple_attr, source_attrs) ->
           let datom = datom ~tx ~e ~a:tuple_attr ~v:(tuple_value datoms e source_attrs) () in
           let datoms, tuple_tx_data = add_active_datom_with_report ~allow_tuple:true db tx datoms datom in
           datoms, tx_data @ tuple_tx_data)
         (datoms, tx_data)
  
  let add_user_datom_with_report db tx datoms d =
    let datoms, tx_data = add_active_datom_with_report db tx datoms d in
    refresh_tuple_attrs_for_source db tx datoms d.e d.a tx_data
  
  let retract_user_attr_with_report db tx datoms e a value =
    if is_tuple_attr db a then invalid_arg "cannot modify tuple attributes directly";
    let datoms, tx_data =
      match value with
      | Some value -> retract_active_datom_with_report tx datoms e a (Some value)
      | None -> retract_attr_with_report db tx datoms e a
    in
    refresh_tuple_attrs_for_source db tx datoms e a tx_data
  
  let normalize_entity_attr_value db e attr value =
    if is_reverse_ref attr then
      let straight_attr = reverse_ref attr in
      if not (is_ref_attr db straight_attr) then
        invalid_arg "reverse entity attribute requires ref schema";
      match value with
      | Ref target -> target, straight_attr, Ref e
      | _ -> invalid_arg "reverse entity attribute value must be a ref"
    else
      e, attr, value
  
  let add_entity_attr_value db tx datoms e attr value =
    let e, attr, value = normalize_entity_attr_value db e attr value in
    add_user_datom_with_report db tx datoms (datom ~tx ~e ~a:attr ~v:value ())
  
  let allocate_entity_id max_eid =
    if max_eid >= max_allocatable_entity_id then
      invalid_arg ("next entity id would enter the transaction id range: " ^ string_of_int (max_eid + 1));
    validate_entity_id (max_eid + 1)
  
  let rec coerce_tuple_lookup_value db datoms attr value =
    match schema_attr db attr, value with
    | Some { tuple_attrs = Some source_attrs; _ }, (List values | Vector values)
      when List.length source_attrs = List.length values ->
      let lookup_attr_name = function
        | Keyword attr | String attr | Symbol attr -> Some attr
        | _ -> None
      in
      let coerce_component source_attr value =
        match value with
        | Nil -> None
        | Int entity_id when is_ref_attr db source_attr -> Some (Ref (validate_entity_id entity_id))
        | (List [ lookup_attr; lookup_value ] | Vector [ lookup_attr; lookup_value ]) when is_ref_attr db source_attr ->
          (match Option.bind (lookup_attr_name lookup_attr) (fun attr -> entid_in_datoms db datoms attr lookup_value) with
           | Some entity_id -> Some (Ref entity_id)
           | None -> Some (normalize_value value))
        | value -> Some (normalize_value value)
      in
      Tuple (List.map2 coerce_component source_attrs values)
    | Some { tuple_attrs = Some source_attrs; _ }, Tuple values
      when List.length source_attrs = List.length values ->
      let lookup_attr_name = function
        | Keyword attr | String attr | Symbol attr -> Some attr
        | _ -> None
      in
      let coerce_component source_attr = function
        | None -> None
        | Some Nil -> None
        | Some (Int entity_id) when is_ref_attr db source_attr -> Some (Ref (validate_entity_id entity_id))
        | Some ((List [ lookup_attr; lookup_value ] | Vector [ lookup_attr; lookup_value ]) as lookup_ref) when is_ref_attr db source_attr ->
          (match Option.bind (lookup_attr_name lookup_attr) (fun attr -> entid_in_datoms db datoms attr lookup_value) with
           | Some entity_id -> Some (Ref entity_id)
           | None -> Some (normalize_value lookup_ref))
        | Some value -> Some (normalize_value value)
      in
      Tuple (List.map2 coerce_component source_attrs values)
    | _ -> normalize_value value
  
  and entid_in_datoms db datoms attr value =
    let value = coerce_tuple_lookup_value db datoms attr value in
    if is_unique db attr then
      datoms
      |> List.find_opt (fun d -> d.a = attr && value_equal d.v value)
      |> Option.map (fun d -> d.e)
    else
      None
  
  let entid db attr value = entid_in_datoms db (visible_datoms db) attr value
  
end
