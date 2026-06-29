open Datascript_types

type context =
  { compare_value : value -> value -> int
  ; entity : db -> entity_ref -> entity option
  ; entity_attr_raw : entity -> attr -> tx_value option
  ; entity_attrs : entity -> (attr * tx_value) list
  ; datoms_by_entity : db -> entity_id -> datom Seq.t
  ; all_datoms : db -> datom Seq.t
  ; datoms_by_avet_ref : db -> attr -> entity_id -> datom Seq.t
  ; cardinality : db -> attr -> cardinality
  ; is_ref_attr : db -> attr -> bool
  ; is_component : db -> attr -> bool
  ; is_reverse_ref : attr -> bool
  ; reverse_ref : attr -> attr
  ; entity_id_of_ref : db -> entity_ref -> entity_id option
  }

let pull_key_of_attr attr = Keyword attr

let compare_pull_key context left right =
  match left, right with
  | Keyword left, Keyword right -> compare left right
  | String left, String right -> compare left right
  | Keyword left, String right -> compare left right
  | String left, Keyword right -> compare left right
  | _ -> context.compare_value left right

let pulled_id_stub entity_id =
  Pulled_entity { pulled_id = entity_id; pulled_attrs = [ pull_key_of_attr "db/id", Pulled_scalar (Int entity_id) ] }

let ref_entity_id_of_value context db attr = function
  | Ref entity_id -> Some entity_id
  | Int entity_id when context.is_ref_attr db attr -> context.entity_id_of_ref db (Entity_id entity_id)
  | _ -> None

let shallow_pulled_value context db attr value =
  match ref_entity_id_of_value context db attr value with
  | Some entity_id -> pulled_id_stub entity_id
  | None -> Pulled_scalar value

let scalar_or_many context db attr = function
  | One_value value -> shallow_pulled_value context db attr value
  | Many_values values -> Pulled_many (List.map (shallow_pulled_value context db attr) values)
  | One_entity _ | Many_entities _ -> invalid_arg "nested entity values are not stored"

let default_pull_limit = 1000

let take n values =
  if n < 0 then invalid_arg "pull limit must be non-negative";
  let rec take acc remaining = function
    | _ when remaining = 0 -> List.rev acc
    | [] -> List.rev acc
    | value :: rest -> take (value :: acc) (remaining - 1) rest
  in
  take [] n values

let limit_tx_value limit = function
  | One_value value -> One_value value
  | Many_values values -> Many_values (take limit values)
  | One_entity _ | Many_entities _ -> invalid_arg "nested entity values are not stored"

let default_limit_tx_value = limit_tx_value default_pull_limit

let rec pull_selector_forward_attr context = function
  | Pull_attr attr
  | Pull_attr_default (attr, _)
  | Pull_attr_limit (attr, _)
  | Pull_attr_unlimited attr
  | Pull_attr_xform (attr, _)
  | Pull_attr_default_xform (attr, _, _)
  | Pull_ref (attr, _)
  | Pull_ref_default (attr, _, _)
  | Pull_ref_limit (attr, _, _)
  | Pull_ref_unlimited (attr, _)
  | Pull_ref_xform (attr, _, _) ->
    if context.is_reverse_ref attr then None else Some attr
  | Pull_recursive_ref (attr, _, _) ->
    if context.is_reverse_ref attr then None else Some attr
  | Pull_as (selector, _) -> pull_selector_forward_attr context selector
  | Pull_id
  | Pull_wildcard
  | Pull_reverse_ref _
  | Pull_reverse_ref_default _
  | Pull_reverse_ref_limit _
  | Pull_reverse_ref_unlimited _
  | Pull_reverse_ref_xform _ ->
    None

let wildcard_shadowed_attrs context selectors =
  selectors
  |> List.filter_map (pull_selector_forward_attr context)
  |> List.sort_uniq String.compare

let rec pull_selector_needs_full_entity context = function
  | Pull_attr attr
  | Pull_attr_default (attr, _)
  | Pull_attr_limit (attr, _)
  | Pull_attr_unlimited attr
  | Pull_attr_xform (attr, _)
  | Pull_attr_default_xform (attr, _, _)
  | Pull_ref (attr, _)
  | Pull_ref_default (attr, _, _)
  | Pull_ref_limit (attr, _, _)
  | Pull_ref_unlimited (attr, _)
  | Pull_ref_xform (attr, _, _)
  | Pull_recursive_ref (attr, _, _) ->
    context.is_reverse_ref attr
  | Pull_as (selector, _) -> pull_selector_needs_full_entity context selector
  | Pull_id
  | Pull_wildcard
  | Pull_reverse_ref _
  | Pull_reverse_ref_default _
  | Pull_reverse_ref_limit _
  | Pull_reverse_ref_unlimited _
  | Pull_reverse_ref_xform _ ->
    false

let selector_needs_full_entity context selectors =
  List.exists (pull_selector_needs_full_entity context) selectors

let rec pull_selector_is_wildcard = function
  | Pull_wildcard -> true
  | Pull_as (selector, _) -> pull_selector_is_wildcard selector
  | Pull_id
  | Pull_attr _
  | Pull_attr_default _
  | Pull_attr_limit _
  | Pull_attr_unlimited _
  | Pull_attr_xform _
  | Pull_attr_default_xform _
  | Pull_ref _
  | Pull_ref_default _
  | Pull_ref_limit _
  | Pull_ref_unlimited _
  | Pull_ref_xform _
  | Pull_recursive_ref _
  | Pull_reverse_ref _
  | Pull_reverse_ref_default _
  | Pull_reverse_ref_limit _
  | Pull_reverse_ref_unlimited _
  | Pull_reverse_ref_xform _ ->
    false

let forward_attrs_for_selectors context selectors =
  if List.exists pull_selector_is_wildcard selectors then
    []
  else
    selectors
    |> List.filter_map (pull_selector_forward_attr context)
    |> List.sort_uniq String.compare

let tx_value_of_attr_values context db attr values =
  match context.cardinality db attr, values with
  | Many, [] -> Many_values []
  | Many, [ _ ] -> Many_values values
  | Many, values -> Many_values (List.sort context.compare_value values)
  | One, [] -> Many_values []
  | One, [ value ] -> One_value value
  | One, first :: rest ->
    One_value
      (List.fold_left
         (fun max_value value ->
           if context.compare_value max_value value < 0 then value else max_value)
         first
         rest)

let forward_entity context db entity_id attrs =
  let datoms = context.datoms_by_entity db entity_id in
  match Seq.uncons datoms with
  | None -> None
  | Some (first, rest) ->
    let wanted attr = attrs = [] || List.mem attr attrs in
    let add_attr attr values acc =
      match tx_value_of_attr_values context db attr values with
      | Many_values [] -> acc
      | value -> (attr, value) :: acc
    in
    let attrs =
      let rec collect current_attr current_values acc seq =
        match seq () with
        | Seq.Nil ->
          (match current_attr with
           | None -> List.rev acc
           | Some attr -> List.rev (add_attr attr current_values acc))
        | Seq.Cons (datom, rest) ->
          if not (wanted datom.a) then
            collect current_attr current_values acc rest
          else
            match current_attr with
            | Some attr when attr = datom.a ->
              collect current_attr (datom.v :: current_values) acc rest
            | Some attr ->
              collect (Some datom.a) [ datom.v ] (add_attr attr current_values acc) rest
            | None -> collect (Some datom.a) [ datom.v ] acc rest
      in
      collect None [] [] (Seq.cons first rest)
    in
    Some
      { id = entity_id
      ; db
      ; attrs
      ; lookup_attr = (fun attr -> List.assoc_opt attr attrs)
      ; materialize_attrs = (fun () -> attrs)
      }

let dedupe_pulled_attrs attrs =
  attrs
  |> List.fold_left
       (fun deduped (attr, value) -> (attr, value) :: List.remove_assoc attr deduped)
       []
  |> List.rev

let visit_pull visitor event =
  match visitor with
  | None -> ()
  | Some visitor -> visitor event

let visit_pull_attr context visitor entity_id attr =
  if attr <> "db/id" then
    if context.is_reverse_ref attr then
      visit_pull visitor (PullVisitReverse (context.reverse_ref attr, entity_id))
    else
      visit_pull visitor (PullVisitAttr (entity_id, attr))

let rec pull_entity_by_id ?visitor context db selector entity_id =
  pull_entity_by_id_visited ?visitor ~root_id:entity_id ~root_reexpanded:false context db [] selector selector entity_id

and pull_entity_by_id_visited ?visitor ~root_id ~root_reexpanded context db visited context_selector selector entity_id =
  let entity =
    if selector_needs_full_entity context selector then
      context.entity db (Entity_id entity_id)
    else
      forward_entity context db entity_id (forward_attrs_for_selectors context selector)
  in
  match entity with
  | None -> None
  | Some entity ->
    let attrs =
      selector
      |> List.concat_map
           (pull_selector_attrs ?visitor ~root_id ~root_reexpanded context db visited context_selector entity)
      |> dedupe_pulled_attrs
      |> List.sort (fun (left, _) (right, _) -> compare_pull_key context left right)
    in
    (match attrs with
     | [] -> None
     | attrs -> Some { pulled_id = entity.id; pulled_attrs = attrs })

and pull_selector_attrs ?visitor ~root_id ~root_reexpanded context db visited context_selector entity = function
  | Pull_id -> [ pull_key_of_attr "db/id", Pulled_scalar (Int entity.id) ]
  | Pull_wildcard ->
    visit_pull visitor (PullVisitWildcard entity.id);
    let shadowed_attrs = wildcard_shadowed_attrs context context_selector in
    (pull_key_of_attr "db/id", Pulled_scalar (Int entity.id))
    :: (context.entity_attrs entity
        |> List.filter (fun (attr, _) ->
          (not (context.is_reverse_ref attr)) && not (List.mem attr shadowed_attrs))
        |> List.map (fun (attr, value) ->
          visit_pull_attr context visitor entity.id attr;
          pull_key_of_attr attr, pulled_attr_value ?visitor ~root_id ~root_reexpanded context db visited entity attr (default_limit_tx_value value)))
  | Pull_attr attr ->
    visit_pull_attr context visitor entity.id attr;
    context.entity_attr_raw entity attr
    |> Option.map (fun value ->
      [ pull_key_of_attr attr, pulled_attr_value ?visitor ~root_id ~root_reexpanded context db visited entity attr (default_limit_tx_value value) ])
    |> Option.value ~default:[]
  | Pull_attr_default (attr, default) ->
    visit_pull_attr context visitor entity.id attr;
    context.entity_attr_raw entity attr
    |> Option.map (fun value ->
      [ pull_key_of_attr attr, pulled_attr_value ?visitor ~root_id ~root_reexpanded context db visited entity attr (default_limit_tx_value value) ])
    |> Option.value ~default:[ pull_key_of_attr attr, Pulled_scalar default ]
  | Pull_attr_limit (attr, limit) ->
    visit_pull_attr context visitor entity.id attr;
    context.entity_attr_raw entity attr
    |> Option.map (fun value ->
      [ pull_key_of_attr attr, pulled_attr_value ?visitor ~root_id ~root_reexpanded context db visited entity attr (limit_tx_value limit value) ])
    |> Option.value ~default:[]
  | Pull_attr_unlimited attr ->
    visit_pull_attr context visitor entity.id attr;
    context.entity_attr_raw entity attr
    |> Option.map (fun value ->
      [ pull_key_of_attr attr, pulled_attr_value ?visitor ~root_id ~root_reexpanded context db visited entity attr value ])
    |> Option.value ~default:[]
  | Pull_attr_xform (attr, f) ->
    visit_pull_attr context visitor entity.id attr;
    let pulled =
      context.entity_attr_raw entity attr
      |> Option.map (fun value -> pulled_attr_value ?visitor ~root_id ~root_reexpanded context db visited entity attr (default_limit_tx_value value))
      |> Option.value ~default:(Pulled_scalar Nil)
      |> f
    in
    (match pulled with
     | Pulled_many [] -> []
     | value -> [ pull_key_of_attr attr, value ])
  | Pull_attr_default_xform (attr, default, f) ->
    visit_pull_attr context visitor entity.id attr;
    (match context.entity_attr_raw entity attr with
     | None -> [ pull_key_of_attr attr, Pulled_scalar default ]
     | Some value ->
       let pulled =
         pulled_attr_value ?visitor ~root_id ~root_reexpanded context db visited entity attr (default_limit_tx_value value)
         |> f
       in
       (match pulled with
        | Pulled_many [] -> []
        | value -> [ pull_key_of_attr attr, value ]))
  | Pull_ref (attr, selector) ->
    visit_pull_attr context visitor entity.id attr;
    (match context.entity_attr_raw entity attr with
     | None -> []
     | Some value ->
       let pulled =
         pull_ref_value ?visitor ~root_id ~root_reexpanded context db visited attr selector default_pull_limit value
       in
       (match pulled with
        | Pulled_many [] -> []
        | value -> [ pull_key_of_attr attr, value ]))
  | Pull_ref_default (attr, selector, default) ->
    visit_pull_attr context visitor entity.id attr;
    (match context.entity_attr_raw entity attr with
     | None -> [ pull_key_of_attr attr, Pulled_scalar default ]
     | Some value ->
       let pulled =
         pull_ref_value ?visitor ~root_id ~root_reexpanded context db visited attr selector default_pull_limit value
       in
       (match pulled with
        | Pulled_many [] -> []
        | value -> [ pull_key_of_attr attr, value ]))
  | Pull_ref_limit (attr, selector, limit) ->
    visit_pull_attr context visitor entity.id attr;
    (match context.entity_attr_raw entity attr with
     | None -> []
     | Some value ->
       let pulled = pull_ref_value ?visitor ~root_id ~root_reexpanded context db visited attr selector limit value in
       (match pulled with
        | Pulled_many [] -> []
        | value -> [ pull_key_of_attr attr, value ]))
  | Pull_ref_unlimited (attr, selector) ->
    visit_pull_attr context visitor entity.id attr;
    (match context.entity_attr_raw entity attr with
     | None -> []
     | Some value ->
       let pulled = pull_ref_value_unlimited ?visitor ~root_id ~root_reexpanded context db visited attr selector value in
       (match pulled with
        | Pulled_many [] -> []
        | value -> [ pull_key_of_attr attr, value ]))
  | Pull_ref_xform (attr, selector, f) ->
    visit_pull_attr context visitor entity.id attr;
    let pulled =
      context.entity_attr_raw entity attr
      |> Option.map (pull_ref_value ?visitor ~root_id ~root_reexpanded context db visited attr selector default_pull_limit)
      |> Option.value ~default:(Pulled_scalar Nil)
      |> f
    in
    (match pulled with
     | Pulled_many [] -> []
     | value -> [ pull_key_of_attr attr, value ])
  | Pull_recursive_ref (attr, selector, depth) ->
    visit_pull_attr context visitor entity.id attr;
    (match if context.is_reverse_ref attr then Some (Many_values []) else context.entity_attr_raw entity attr with
     | None -> []
     | Some value ->
       let pulled =
         pull_recursive_ref_value
           ?visitor
           ~root_id
           ~root_reexpanded
           context
           db
           visited
           context_selector
           attr
           selector
           depth
           entity.id
           value
       in
       (match pulled with
        | Pulled_many [] -> []
        | value -> [ pull_key_of_attr attr, value ]))
  | Pull_reverse_ref (attr, selector) ->
    visit_pull visitor (PullVisitReverse (attr, entity.id));
    let pulled =
      context.datoms_by_avet_ref db attr entity.id
      |> Seq.filter_map
           (fun d ->
             pull_entity_by_id_visited ?visitor ~root_id ~root_reexpanded context db visited selector selector d.e)
      |> Seq.map (fun entity -> Pulled_entity entity)
      |> List.of_seq
    in
    (if context.is_component db attr then
       match pulled with
       | [] -> []
       | value :: _ -> [ pull_key_of_attr attr, value ]
     else
       match pulled with
       | [] -> []
       | values -> [ pull_key_of_attr attr, Pulled_many (take default_pull_limit values) ])
  | Pull_reverse_ref_default (attr, selector, default) ->
    visit_pull visitor (PullVisitReverse (attr, entity.id));
    let pulled =
      context.datoms_by_avet_ref db attr entity.id
      |> Seq.filter_map
           (fun d ->
             pull_entity_by_id_visited ?visitor ~root_id ~root_reexpanded context db visited selector selector d.e)
      |> Seq.map (fun entity -> Pulled_entity entity)
      |> List.of_seq
    in
    (if context.is_component db attr then
       match pulled with
       | [] -> [ pull_key_of_attr attr, Pulled_scalar default ]
       | value :: _ -> [ pull_key_of_attr attr, value ]
     else
       match pulled with
       | [] -> [ pull_key_of_attr attr, Pulled_scalar default ]
       | values -> [ pull_key_of_attr attr, Pulled_many (take default_pull_limit values) ])
  | Pull_reverse_ref_limit (attr, selector, limit) ->
    visit_pull visitor (PullVisitReverse (attr, entity.id));
    let pulled =
      context.datoms_by_avet_ref db attr entity.id
      |> Seq.filter_map
           (fun d ->
             pull_entity_by_id_visited ?visitor ~root_id ~root_reexpanded context db visited selector selector d.e)
      |> Seq.map (fun entity -> Pulled_entity entity)
      |> List.of_seq
    in
    (if context.is_component db attr then
       match pulled with
       | [] -> []
       | value :: _ -> [ pull_key_of_attr attr, value ]
     else
       match pulled with
       | [] -> []
       | values -> [ pull_key_of_attr attr, Pulled_many (take limit values) ])
  | Pull_reverse_ref_unlimited (attr, selector) ->
    visit_pull visitor (PullVisitReverse (attr, entity.id));
    let pulled =
      context.datoms_by_avet_ref db attr entity.id
      |> Seq.filter_map
           (fun d ->
             pull_entity_by_id_visited ?visitor ~root_id ~root_reexpanded context db visited selector selector d.e)
      |> Seq.map (fun entity -> Pulled_entity entity)
      |> List.of_seq
    in
    (if context.is_component db attr then
       match pulled with
       | [] -> []
       | value :: _ -> [ pull_key_of_attr attr, value ]
     else
       match pulled with
       | [] -> []
       | values -> [ pull_key_of_attr attr, Pulled_many values ])
  | Pull_reverse_ref_xform (attr, selector, f) ->
    visit_pull visitor (PullVisitReverse (attr, entity.id));
    let pulled =
      context.datoms_by_avet_ref db attr entity.id
      |> Seq.filter_map
           (fun d ->
             pull_entity_by_id_visited ?visitor ~root_id ~root_reexpanded context db visited selector selector d.e)
      |> Seq.map (fun entity -> Pulled_entity entity)
      |> List.of_seq
    in
    let pulled =
      if context.is_component db attr then
        match pulled with
        | [] -> Pulled_scalar Nil
        | value :: _ -> value
      else
        match pulled with
        | [] -> Pulled_scalar Nil
        | values -> Pulled_many (take default_pull_limit values)
    in
    (match f pulled with
     | Pulled_many [] -> []
     | value -> [ pull_key_of_attr attr, value ])
  | Pull_as (selector, alias) ->
    pull_selector_attrs ?visitor ~root_id ~root_reexpanded context db visited context_selector entity selector
    |> List.map (fun (_, value) -> alias, value)

and pulled_attr_value ?visitor ~root_id ~root_reexpanded context db visited entity attr value =
  if context.is_component db attr then
    pull_component_value ?visitor ~root_id ~root_reexpanded context db visited entity.id attr value
  else
    scalar_or_many context db attr value

and pull_component_value ?visitor ~root_id ~root_reexpanded context db visited current_id attr = function
  | One_value value when Option.is_some (ref_entity_id_of_value context db attr value) ->
    let entity_id = Option.get (ref_entity_id_of_value context db attr value) in
    if List.mem entity_id (current_id :: visited) then pulled_id_stub entity_id
    else
      (match
         pull_entity_by_id_visited
           ?visitor
           ~root_id
           ~root_reexpanded
           context
           db
           (current_id :: visited)
           [ Pull_wildcard ]
           [ Pull_wildcard ]
           entity_id
       with
       | Some entity -> Pulled_entity entity
       | None -> Pulled_scalar value)
  | Many_values values ->
    values
    |> List.filter_map (function
      | value when Option.is_some (ref_entity_id_of_value context db attr value) ->
        let entity_id = Option.get (ref_entity_id_of_value context db attr value) in
        if List.mem entity_id (current_id :: visited) then
          Some (pulled_id_stub entity_id)
        else
          pull_entity_by_id_visited
            ?visitor
            ~root_id
            ~root_reexpanded
            context
            db
            (current_id :: visited)
            [ Pull_wildcard ]
            [ Pull_wildcard ]
            entity_id
          |> Option.map (fun entity -> Pulled_entity entity)
      | _ -> None)
    |> fun values -> Pulled_many values
  | value -> scalar_or_many context db attr value

and pull_ref_value_with_limit ?visitor ~root_id ~root_reexpanded context db visited attr selector limit = function
  | One_value value when Option.is_some (ref_entity_id_of_value context db attr value) ->
    let entity_id = Option.get (ref_entity_id_of_value context db attr value) in
    (match pull_entity_by_id_visited ?visitor ~root_id ~root_reexpanded context db visited selector selector entity_id with
     | Some entity -> Pulled_entity entity
     | None -> Pulled_many [])
  | Many_values values ->
    values
    |> List.filter_map (function
      | value when Option.is_some (ref_entity_id_of_value context db attr value) ->
        let entity_id = Option.get (ref_entity_id_of_value context db attr value) in
        pull_entity_by_id_visited ?visitor ~root_id ~root_reexpanded context db visited selector selector entity_id
        |> Option.map (fun entity -> Pulled_entity entity)
      | _ -> None)
    |> (fun values ->
      match limit with
      | Some limit -> take limit values
      | None -> values)
    |> fun values -> Pulled_many values
  | value -> scalar_or_many context db attr value

and pull_ref_value ?visitor ~root_id ~root_reexpanded context db visited attr selector limit value =
  pull_ref_value_with_limit ?visitor ~root_id ~root_reexpanded context db visited attr selector (Some limit) value

and pull_ref_value_unlimited ?visitor ~root_id ~root_reexpanded context db visited attr selector value =
  pull_ref_value_with_limit ?visitor ~root_id ~root_reexpanded context db visited attr selector None value

and pull_recursive_ref_value
  ?visitor
  ~root_id
  ~root_reexpanded
  context
  db
  visited
  context_selector
  attr
  selector
  depth
  current_id
  value
  =
  let seen = current_id :: visited in
  let next_recursive_depth = function
    | Some depth when depth <= 1 -> None
    | Some depth -> Some (Some (depth - 1))
    | None -> Some None
  in
  let recursive_context next_current_depth =
    let found_current = ref false in
    let selectors =
      context_selector
      |> List.filter_map (function
        | Pull_recursive_ref (context_attr, context_selector, context_depth) ->
          if context_attr = attr then begin
            found_current := true;
            Option.map
              (fun next_depth -> Pull_recursive_ref (context_attr, context_selector, next_depth))
              next_current_depth
          end
          else
            Some (Pull_recursive_ref (context_attr, context_selector, context_depth))
        | _ -> None)
    in
    match !found_current, next_current_depth with
    | true, _ | false, None -> selectors
    | false, Some next_depth -> Pull_recursive_ref (attr, selector, next_depth) :: selectors
  in
  let selector_for_depth () =
    match recursive_context (next_recursive_depth depth) with
    | [] -> selector
    | recursive_selectors -> selector @ recursive_selectors
  in
  let pull_child entity_id =
    if List.mem entity_id seen then
      if entity_id = root_id && not root_reexpanded then
        let selector = selector_for_depth () in
        pull_entity_by_id_visited
          ?visitor
          ~root_id
          ~root_reexpanded:true
          context
          db
          (current_id :: visited)
          selector
          selector
          entity_id
        |> Option.map (fun entity -> Pulled_entity entity)
      else
        Some (pulled_id_stub entity_id)
    else
      let selector = selector_for_depth () in
      pull_entity_by_id_visited
        ?visitor
        ~root_id
        ~root_reexpanded
        context
        db
        (current_id :: visited)
        selector
        selector
        entity_id
      |> Option.map (fun entity -> Pulled_entity entity)
  in
  let pull_reverse_children forward_attr =
    let pulled =
      context.datoms_by_avet_ref db forward_attr current_id
      |> Seq.filter_map (fun d -> pull_child d.e)
      |> List.of_seq
      |> take default_pull_limit
    in
    if context.is_component db forward_attr then
      match pulled with
      | [] -> Pulled_many []
      | value :: _ -> value
    else
      Pulled_many pulled
  in
  if context.is_reverse_ref attr then
    pull_reverse_children (context.reverse_ref attr)
  else
    match value with
    | One_value value when Option.is_some (ref_entity_id_of_value context db attr value) ->
      let entity_id = Option.get (ref_entity_id_of_value context db attr value) in
      (match pull_child entity_id with
       | Some value -> value
       | None -> Pulled_many [])
    | Many_values values ->
      values
      |> List.filter_map (function
        | value when Option.is_some (ref_entity_id_of_value context db attr value) ->
          let entity_id = Option.get (ref_entity_id_of_value context db attr value) in
          pull_child entity_id
        | _ -> None)
      |> take default_pull_limit
      |> fun values -> Pulled_many values
    | value -> scalar_or_many context db attr value

let pull ?visitor context db selector entity_ref =
  match context.entity_id_of_ref db entity_ref with
  | None -> None
  | Some entity_id -> pull_entity_by_id ?visitor context db selector entity_id

let pull_many ?visitor context db selector entity_refs =
  List.map (pull ?visitor context db selector) entity_refs

let pull_wildcard_entity ?visitor context db entity_id attrs =
  let entity =
    { id = entity_id
    ; db
    ; attrs
    ; lookup_attr = (fun attr -> List.assoc_opt attr attrs)
    ; materialize_attrs = (fun () -> attrs)
    }
  in
  visit_pull visitor (PullVisitWildcard entity_id);
  (pull_key_of_attr "db/id", Pulled_scalar (Int entity_id))
  :: (attrs
      |> List.filter (fun (attr, _) -> not (context.is_reverse_ref attr))
      |> List.map (fun (attr, value) ->
        visit_pull_attr context visitor entity_id attr;
        pull_key_of_attr attr, pulled_attr_value ?visitor ~root_id:entity_id ~root_reexpanded:false context db [] entity attr (default_limit_tx_value value)))
  |> List.sort (fun (left, _) (right, _) -> compare_pull_key context left right)
  |> fun pulled_attrs -> { pulled_id = entity_id; pulled_attrs }

let pull_wildcard_many_by_ids ?visitor context db entity_ids =
  match entity_ids with
  | [] -> []
  | _ ->
    let cached lookup =
      let table = Hashtbl.create 128 in
      fun db attr ->
        match Hashtbl.find_opt table attr with
        | Some value -> value
        | None ->
          let value = lookup db attr in
          Hashtbl.replace table attr value;
          value
    in
    let context =
      { context with
        cardinality = cached context.cardinality
      ; is_ref_attr = cached context.is_ref_attr
      ; is_component = cached context.is_component
      }
    in
    let wanted = Bytes.make (db.max_datom_e + 1) '\000' in
    List.iter
      (fun entity_id ->
        if entity_id >= 0 && entity_id < Bytes.length wanted then
          Bytes.set wanted entity_id '\001')
      entity_ids;
    let wanted_entity entity_id =
      entity_id >= 0 && entity_id < Bytes.length wanted && Bytes.get wanted entity_id = '\001'
    in
    let add_attr attr values acc =
      match tx_value_of_attr_values context db attr values with
      | Many_values [] -> acc
      | value -> (attr, value) :: acc
    in
    let finish_entity current_entity current_attr current_values attrs acc =
      match current_entity with
      | None -> acc
      | Some entity_id ->
        let attrs =
          match current_attr with
          | None -> attrs
          | Some attr -> add_attr attr current_values attrs
        in
        pull_wildcard_entity ?visitor context db entity_id (List.rev attrs) :: acc
    in
    let rec collect current_entity current_attr current_values attrs acc seq =
      match seq () with
      | Seq.Nil -> List.rev (finish_entity current_entity current_attr current_values attrs acc)
      | Seq.Cons (datom, rest) ->
        if not (wanted_entity datom.e) then
          collect current_entity current_attr current_values attrs acc rest
        else
          match current_entity, current_attr with
          | Some entity_id, Some attr when entity_id = datom.e && attr = datom.a ->
            collect current_entity current_attr (datom.v :: current_values) attrs acc rest
          | Some entity_id, Some attr when entity_id = datom.e ->
            collect
              current_entity
              (Some datom.a)
              [ datom.v ]
              (add_attr attr current_values attrs)
              acc
              rest
          | Some _, _ ->
            let acc = finish_entity current_entity current_attr current_values attrs acc in
            collect (Some datom.e) (Some datom.a) [ datom.v ] [] acc rest
          | None, _ ->
            collect (Some datom.e) (Some datom.a) [ datom.v ] [] acc rest
    in
    collect None None [] [] [] (context.all_datoms db)
