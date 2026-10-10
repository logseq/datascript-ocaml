open Datascript_types

type context =
  { datoms_by_entity : db -> entity_id -> datom Seq.t
  ; datoms_by_entity_attr : db -> entity_id -> attr -> datom Seq.t
  ; datoms_by_avet_ref : db -> attr -> entity_id -> datom Seq.t
  ; all_datoms : db -> datom Seq.t
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
  | One, [] -> Many_values []
  | One, values -> One_value (List.hd (List.rev values))

let entity_has_forward_attrs context db entity_id =
  Option.is_some (Seq.uncons (context.datoms_by_entity db entity_id))

let entity_visible_attr_values context db attr values =
  if context.is_ref_attr db attr then
    (* upstream entity-attr wraps (:v datom) into an entity whenever the
       schema marks attr a ref, so a plain number stored before the schema
       gained :db.type/ref (e.g. a :db/add applied while the attr was not
       yet ref-typed) is read back as an entity id. *)
    values
    |> List.map (function
      | Int64 entity_id ->
        (match Util.int64_to_int entity_id with
         | Some entity_id -> Ref entity_id
         | None -> invalid_arg ("entity id out of range: " ^ Int64.to_string entity_id))
      | v -> v)
    |> List.filter (function
      | Ref entity_id -> entity_has_forward_attrs context db entity_id
      | _ -> true)
  else
    values

(* Raw forward groups without ref wrapping / visibility filtering — a
   single bounded index scan. The EAVT entity slice is ordered by attr, so
   groups are consecutive runs (upstream's `partition-by :a` in
   datoms->cache) instead of a per-datom assoc rebuild. Conversion happens
   per attr so that reading one attribute never pays (or raises on) another
   attribute's values. *)
let raw_forward_entity_attrs context db entity_id =
  context.datoms_by_entity db entity_id
  |> Seq.fold_left
       (fun groups datom ->
         match groups with
         | (attr, values) :: rest when Util.compare_attr attr datom.a = 0 ->
           (attr, datom.v :: values) :: rest
         | groups -> (datom.a, [ datom.v ]) :: groups)
       []
  |> List.rev_map (fun (attr, values) -> attr, List.rev values)

let tx_value_of_raw_attr context db attr values =
  match entity_visible_attr_values context db attr values with
  | [] -> None
  | values -> Some (tx_value_of_attr_values context db attr values)

let sorted_forward_entity_attrs context db entity_id =
  raw_forward_entity_attrs context db entity_id
  |> List.filter_map (fun (attr, values) ->
    Option.map (fun v -> attr, v) (tx_value_of_raw_attr context db attr values))
  (* the eavt slice emits datoms in (e, a, v) index order, so the attr
     groups above are already sorted — no final sort needed *)

let reverse_entity_attr context db entity_id attr =
  let forward_attr = context.reverse_ref attr in
  let values =
    context.datoms_by_avet_ref db forward_attr entity_id
    |> Seq.map (fun d -> Ref d.e)
    |> List.of_seq
    |> List.sort context.compare_value
  in
  match values with
  | [] -> None
  | value :: _ when context.is_component db forward_attr -> Some (One_value value)
  | values -> Some (Many_values values)

let lazy_entity context db entity_id =
  (* upstream Entity holds only db + eid and resolves each attr on demand:
     forward attrs through a bounded (eid, attr) index seek, reverse attrs
     through a bounded seek on the ref attribute. Resolved attrs are cached
     on the entity; once the entity is materialized (upstream `touched`),
     lookups of absent forward attrs answer from the cache alone. *)
  let lookup_cache : (attr, tx_value option) Hashtbl.t = Hashtbl.create 8 in
  let materialized = ref None in
  let lookup attr =
    match Hashtbl.find_opt lookup_cache attr with
    | Some cached -> cached
    | None ->
        let result =
          if context.is_reverse_ref attr then
            reverse_entity_attr context db entity_id attr
          else
            match !materialized with
            | Some attrs -> List.assoc_opt attr attrs
            | None ->
              context.datoms_by_entity_attr db entity_id attr
              |> Seq.map (fun datom -> datom.v)
              |> List.of_seq
              |> tx_value_of_raw_attr context db attr
        in
        Hashtbl.replace lookup_cache attr result;
        result
  in
  let materialize_attrs () =
    match !materialized with
    | Some attrs -> attrs
    | None ->
      let attrs = sorted_forward_entity_attrs context db entity_id in
      materialized := Some attrs;
      attrs
  in
  { id = entity_id
  ; db
  ; attrs = []
  ; lookup_attr = lookup
  ; materialize_attrs
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
  | "db/id" -> Some (One_value (Int64 (Int64.of_int entity.id)))
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
