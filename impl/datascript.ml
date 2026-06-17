include Datascript_types

module Built_ins = Built_ins
module Conn = Conn
module Db_impl = Db

type conn = Conn.t

let tx0 = Db_impl.tx0

let next_db_uid =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter

let refresh_db_identity db =
  { db with db_uid = next_db_uid () }

module Entity = Entity
module Lru = Lru
module Lookup_refs = Lookup_refs
module Schema = Schema
module Serialize = Serialize
module Storage = Storage
module Util = Util
module Upsert = Upsert

let max_entity_id = 0x7fffffff

let validate_entity_id entity_id =
  if entity_id < 0 then
    invalid_arg ("entity id must not be negative: " ^ string_of_int entity_id);
  if entity_id > max_entity_id then
    invalid_arg ("highest supported entity id exceeded: " ^ string_of_int entity_id);
  entity_id

let datom = Db_impl.datom

let is_datom = Db_impl.is_datom

let validate_schema = Schema.validate_schema

let is_db (_ : db) = true

let rec max_eid_in_value max_eid = function
  | Ref entity_id -> max max_eid (validate_entity_id entity_id)
  | List values | Vector values ->
    List.fold_left max_eid_in_value max_eid values
  | Map entries ->
    List.fold_left
      (fun max_eid (key, value) ->
        max_eid_in_value (max_eid_in_value max_eid key) value)
      max_eid
      entries
  | Set values ->
    List.fold_left max_eid_in_value max_eid values
  | Tuple values ->
    List.fold_left
      (fun max_eid -> function
        | None -> max_eid
        | Some value -> max_eid_in_value max_eid value)
      max_eid
      values
  | Nil | Int _ | Float _ | String _ | Symbol _ | Bool _ | Keyword _ | Uuid _ | Instant _ | Regex _ | TxRef | Ref_to _ -> max_eid

let split_keyword = Util.split_keyword
let compare_value = Util.compare_value
let first_nonzero = Util.first_nonzero
let compare_datom = Util.compare_datom
let normalize_value = Util.normalize_value
let normalize_datom_value = Util.normalize_datom_value

let schema_attr_is_ref = Schema.schema_attr_is_ref

let normalize_datom_for_schema schema d =
  let d = normalize_datom_value d in
  if schema_attr_is_ref schema d.a then
    match d.v with
    | Int entity_id -> { d with v = Ref (validate_entity_id entity_id) }
    | _ -> d
  else
    d

let schema_attr_is_avet_accessible = Schema.schema_attr_is_avet_accessible

let datom_has_ref_value = function
  | { v = Ref _; _ } -> true
  | _ -> false

let build_index index datoms =
  datoms |> List.sort (compare_datom index)

let build_avet_index schema datoms =
  datoms
  |> List.filter (fun d -> schema_attr_is_avet_accessible schema d.a)
  |> build_index Avet

let build_vaet_index datoms =
  datoms
  |> List.filter datom_has_ref_value
  |> build_index Vaet

let refresh_db_indexes db =
  { db with
    eavt_index = build_index Eavt db.datoms
  ; aevt_index = build_index Aevt db.datoms
  ; avet_index = build_avet_index db.schema db.datoms
  ; vaet_index = build_vaet_index db.datoms
  }

let with_db_datoms db datoms =
  refresh_db_indexes { db with datoms }

let empty_db ?(schema = []) ?storage () =
  let schema = validate_schema schema in
  refresh_db_indexes
    { db_uid = next_db_uid ()
    ; schema
    ; datoms = []
    ; eavt_index = []
    ; aevt_index = []
    ; avet_index = []
    ; vaet_index = []
    ; history_datoms = []
    ; historical = false
    ; max_eid = 0
    ; max_tx = tx0
    ; filter_pred = None
    ; storage_ref = storage
    ; tx_fns = []
    }

let empty db = empty_db ~schema:db.schema ?storage:db.storage_ref ()

let schema_has_no_history = Schema.schema_has_no_history

let history_datoms_for_schema schema tx_data =
  List.filter (fun d -> not (schema_has_no_history schema d.a)) tx_data

let init_db ?(schema = []) ?storage datoms =
  let schema = validate_schema schema in
  let datoms = List.map (normalize_datom_for_schema schema) datoms in
  let history_datoms = history_datoms_for_schema schema datoms in
  let max_eid =
    List.fold_left (fun max_eid d -> max_eid_in_value (max max_eid (validate_entity_id d.e)) d.v) 0 datoms
  in
  let max_tx = List.fold_left (fun max_tx d -> max max_tx d.tx) tx0 datoms in
  refresh_db_indexes
    { db_uid = next_db_uid ()
    ; schema
    ; datoms
    ; eavt_index = []
    ; aevt_index = []
    ; avet_index = []
    ; vaet_index = []
    ; history_datoms
    ; historical = false
    ; max_eid
    ; max_tx
    ; filter_pred = None
    ; storage_ref = storage
    ; tx_fns = []
    }

let history db = with_db_datoms (refresh_db_identity { db with historical = true }) db.history_datoms

let is_history db = db.historical

let visible_datoms db =
  match db.filter_pred with
  | None -> db.datoms
  | Some pred -> List.filter pred db.datoms

let is_filtered db = Option.is_some db.filter_pred

let unfiltered_db db = refresh_db_identity { db with filter_pred = None }

let filter db pred =
  let unfiltered = unfiltered_db db in
  let filter_pred =
    match db.filter_pred with
    | None -> fun datom -> pred unfiltered datom
    | Some existing -> fun datom -> existing datom && pred unfiltered datom
  in
  refresh_db_identity { db with filter_pred = Some filter_pred }

let serializable = Serialize.serializable

let serialize_context : Serialize.context =
  { next_db_uid
  ; validate_schema
  ; normalize_datom_for_schema
  ; refresh_db_indexes
  }

let from_serializable snapshot =
  Serialize.from_serializable serialize_context snapshot

let storage_store_context : Storage.store_context =
  { serializable }

let store ?storage db =
  Storage.store storage_store_context ?storage db

let memory_storage = Storage.memory_storage
let file_storage = Storage.file_storage
let store_tail = Storage.store_tail
let storage_tail_compaction_threshold = Storage.tail_compaction_threshold
let storage_tail_datom_count = Storage.tail_datom_count
let restore_tail_groups = Storage.restore_tail_groups
let storage_addresses = Storage.storage_addresses
let storage = Storage.storage
let addresses = Storage.addresses
let settings = Storage.settings
let collect_garbage = Storage.collect_garbage

let conn_creation_context : Conn.creation_context =
  { empty_db; init_db; store }

let create_conn ?schema ?storage () =
  Conn.create conn_creation_context ?schema ?storage ()

let conn_from_db db =
  Conn.from_db conn_creation_context db

let conn_from_datoms ?schema ?storage datoms =
  Conn.from_datoms conn_creation_context ?schema ?storage datoms

let conn_db = Conn.db

let db = conn_db

let is_conn = Conn.is_conn

let listen = Conn.listen

let listen_bang = listen

let listen_auto = Conn.listen_auto

let listen_bang_auto = listen_auto

let unlisten = Conn.unlisten

let unlisten_bang = unlisten

let schema db = db.schema

let with_schema db schema = refresh_db_indexes (refresh_db_identity { db with schema = validate_schema schema })

let reset_schema conn schema =
  let context : Conn.schema_context = { store; with_schema } in
  Conn.reset_schema context conn schema

let reset_schema_bang = reset_schema

let schema_attr db attr = List.assoc_opt attr db.schema

let ident_attr = "db/ident"

let cardinality db attr =
  if attr = "db/tupleAttrs" || attr = "db/tupleTypes" then Many
  else
    match schema_attr db attr with
    | Some schema_attr -> schema_attr.cardinality
    | None -> One

let is_unique_identity db attr =
  attr = ident_attr
  ||
  match schema_attr db attr with
  | Some { unique = Some Identity; _ } -> true
  | _ -> false

let is_unique db attr =
  attr = ident_attr
  ||
  match schema_attr db attr with
  | Some { unique = Some _; _ } -> true
  | _ -> false

let tuple_attrs db attr =
  match schema_attr db attr with
  | Some { tuple_attrs = Some attrs; _ } -> Some attrs
  | _ -> None

let is_tuple_attr db attr = Option.is_some (tuple_attrs db attr)

let is_indexed db attr =
  attr = ident_attr
  ||
  is_tuple_attr db attr
  ||
  match schema_attr db attr with
  | Some { indexed = true; _ } -> true
  | _ -> false

let is_component db attr =
  match schema_attr db attr with
  | Some { is_component = true; _ } -> true
  | _ -> false

let is_ref_attr db attr =
  match schema_attr db attr with
  | Some { value_type = Some RefType; _ } -> true
  | _ -> false

let tuple_attrs_for_source db source_attr =
  db.schema
  |> List.filter_map (fun (attr, schema_attr) ->
    match schema_attr.tuple_attrs with
    | Some source_attrs when List.mem source_attr source_attrs -> Some (attr, source_attrs)
    | _ -> None)

let is_reverse_ref = Schema.is_reverse_ref

let reverse_ref = Schema.reverse_ref

let value_equal = Db_impl.value_equal

let same_fact = Db_impl.same_fact

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

let allocate_entity_id max_eid = validate_entity_id (max_eid + 1)

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

let rec edn_string_of_value = function
  | Nil -> "nil"
  | Bool value -> if value then "true" else "false"
  | Int value -> string_of_int value
  | Float value -> string_of_float value
  | String value -> "\"" ^ String.escaped value ^ "\""
  | Keyword value -> ":" ^ value
  | Symbol value -> value
  | Uuid value -> "#uuid \"" ^ value ^ "\""
  | Instant millis -> string_of_int millis
  | Regex value -> "#\"" ^ String.escaped value ^ "\""
  | Ref entity_id -> string_of_int entity_id
  | TxRef -> ":db/current-tx"
  | Ref_to entity_ref -> edn_string_of_entity_ref entity_ref
  | List values -> "(" ^ String.concat " " (List.map edn_string_of_value values) ^ ")"
  | Vector values -> "[" ^ String.concat " " (List.map edn_string_of_value values) ^ "]"
  | Set values -> "#{" ^ String.concat " " (List.map edn_string_of_value values) ^ "}"
  | Tuple values ->
    "[" ^ String.concat " " (List.map (function None -> "nil" | Some value -> edn_string_of_value value) values) ^ "]"
  | Map entries ->
    "{"
    ^ String.concat
        " "
        (List.map
           (fun (key, value) -> edn_string_of_value key ^ " " ^ edn_string_of_value value)
           entries)
    ^ "}"

and edn_string_of_entity_ref = function
  | Entity_id entity_id -> string_of_int entity_id
  | Temp_id tempid -> tempid
  | CurrentTx -> ":db/current-tx"
  | Ident ident -> ":" ^ ident
  | Lookup_ref (attr, value) -> "[:" ^ attr ^ " " ^ edn_string_of_value value ^ "]"

let cas_current_value_string db datoms e a =
  match cardinality db a with
  | Many ->
    let values =
      entity_attr_datoms datoms e a
      |> List.map (fun d -> d.v)
      |> List.sort compare_value
      |> List.map edn_string_of_value
    in
    "(" ^ String.concat " " values ^ ")"
  | One ->
    current_attr_value datoms e a
    |> Option.map edn_string_of_value
    |> Option.value ~default:"nil"

let cas_expected_value_string = function
  | None -> "nil"
  | Some value -> edn_string_of_value value

let compare_and_set_failure_message db datoms e a expected =
  ":db.fn/cas failed on datom ["
  ^ string_of_int e
  ^ " :"
  ^ a
  ^ " "
  ^ cas_current_value_string db datoms e a
  ^ "], expected "
  ^ cas_expected_value_string expected

let lookup_refs_context : Lookup_refs.context =
  { is_unique
  ; entid_in_datoms
  ; visible_datoms
  ; value_to_string = edn_string_of_value
  }

let lookup_ref_entity_id_in_datoms ?strict_missing db datoms attr value =
  Lookup_refs.entity_id_in_datoms ?strict_missing lookup_refs_context db datoms attr value

let lookup_ref_entity_id ?strict_missing db attr value =
  Lookup_refs.entity_id ?strict_missing lookup_refs_context db attr value

let unresolved_lookup_ref_message attr value =
  Lookup_refs.unresolved_message lookup_refs_context attr value

let unresolved_entity_ref_message = function
  | Lookup_ref (attr, value) -> unresolved_lookup_ref_message attr value
  | _ -> "lookup ref did not resolve"

let upsert_context : Upsert.context =
  { is_unique_identity
  ; entid_in_datoms
  ; value_to_string = edn_string_of_value
  }

let validate_explicit_upsert_target =
  Upsert.validate_explicit_target upsert_context

let entity_unique_identity =
  Upsert.entity_unique_identity upsert_context

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

let rec resolve_entity_ref db datoms tx max_eid tempids = function
  | Entity_id e ->
    let e = validate_entity_id e in
    e, max max_eid e, tempids
  | CurrentTx -> tx, max_eid, remember_current_tx tempids tx
  | Ident ident ->
    (match entid_in_datoms db datoms ident_attr (Keyword ident) with
     | Some e -> e, max max_eid e, tempids
     | None -> invalid_arg "ident did not resolve")
  | Temp_id tempid ->
    if is_current_tx_alias tempid then
      tx, max_eid, remember_current_tx_alias tempids tx tempid
    else
      (match List.assoc_opt tempid tempids with
       | Some e -> e, max_eid, tempids
       | None ->
         let e = allocate_entity_id max_eid in
         e, e, remember_tempid tempids tempid e)
  | Lookup_ref (attr, value) ->
    let value, max_eid, tempids = resolve_value db datoms tx max_eid tempids value in
    (match lookup_ref_entity_id_in_datoms ~strict_missing:true db datoms attr value with
     | Some e -> e, max max_eid e, tempids
     | None -> invalid_arg (unresolved_lookup_ref_message attr value))

and resolve_value db datoms tx max_eid tempids = function
  | TxRef -> Ref tx, max_eid, remember_current_tx tempids tx
  | Ref e ->
    let e = validate_entity_id e in
    Ref e, max max_eid e, tempids
  | Ref_to entity_ref ->
    let e, max_eid, tempids = resolve_entity_ref db datoms tx max_eid tempids entity_ref in
    Ref e, max_eid, tempids
  | List values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value db datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    normalize_value (List (List.rev values)), max_eid, tempids
  | Vector values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value db datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    normalize_value (Vector (List.rev values)), max_eid, tempids
  | Map entries ->
    let entries, max_eid, tempids =
      List.fold_left
        (fun (entries, max_eid, tempids) (key, value) ->
          let key, max_eid, tempids = resolve_value db datoms tx max_eid tempids key in
          let value, max_eid, tempids = resolve_value db datoms tx max_eid tempids value in
          (key, value) :: entries, max_eid, tempids)
        ([], max_eid, tempids)
        entries
    in
    normalize_value (Map (List.rev entries)), max_eid, tempids
  | Set values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value db datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    normalize_value (Set (List.rev values)), max_eid, tempids
  | Tuple values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          match value with
          | None -> None :: values, max_eid, tempids
          | Some value ->
            let value, max_eid, tempids = resolve_value db datoms tx max_eid tempids value in
            Some value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    normalize_value (Tuple (List.rev values)), max_eid, tempids
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

let ref_attr_for_value_resolution db attr =
  if is_ref_attr db attr then
    Some attr
  else if is_reverse_ref attr && is_ref_attr db (reverse_ref attr) then
    Some (reverse_ref attr)
  else
    None

let resolve_value_for_attr db attr datoms tx max_eid tempids value =
  match ref_attr_for_value_resolution db attr, entity_ref_of_ref_attr_value value with
  | Some _, Some entity_ref ->
    let entity_id, max_eid, tempids = resolve_entity_ref db datoms tx max_eid tempids entity_ref in
    Ref entity_id, max_eid, tempids
  | Some _, None -> invalid_arg "Expected number or lookup ref for entity id"
  | _ ->
    resolve_value db datoms tx max_eid tempids value

let attr_expands_collection db attr =
  cardinality db attr = Many || is_reverse_ref attr

let ref_lookup_collection_value = function
  | (List _ | Vector _) as value ->
    (match entity_ref_of_ref_attr_value value with
     | Some _ -> true
     | None -> false)
  | _ -> false

let resolve_existing_entity_ref db datoms tx max_eid tempids = function
  | Temp_id _ -> invalid_arg "Tempids are allowed in :db/add only"
  | entity_ref -> resolve_entity_ref db datoms tx max_eid tempids entity_ref

let resolve_optional_existing_entity_ref db datoms tx max_eid tempids = function
  | Temp_id _ -> invalid_arg "Tempids are allowed in :db/add only"
  | Lookup_ref (attr, value) ->
    let value, max_eid, tempids = resolve_value db datoms tx max_eid tempids value in
    (match lookup_ref_entity_id_in_datoms db datoms attr value with
     | Some e -> Some e, max max_eid e, tempids
     | None -> None, max_eid, tempids)
  | entity_ref ->
    let e, max_eid, tempids = resolve_entity_ref db datoms tx max_eid tempids entity_ref in
    Some e, max_eid, tempids

let resolve_tx_value_for_attr db attr datoms tx max_eid tempids = function
  | One_value ((List values | Vector values) as value) when attr_expands_collection db attr && not (ref_lookup_collection_value value) ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value_for_attr db attr datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    Many_values (List.rev values), max_eid, tempids
  | One_value (Set values) when attr_expands_collection db attr ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value_for_attr db attr datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    Many_values (List.rev values), max_eid, tempids
  | One_value value ->
    let value, max_eid, tempids = resolve_value_for_attr db attr datoms tx max_eid tempids value in
    One_value value, max_eid, tempids
  | Many_values values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value_for_attr db attr datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    Many_values (List.rev values), max_eid, tempids
  | One_entity entity -> One_entity entity, max_eid, tempids
  | Many_entities entities -> Many_entities entities, max_eid, tempids

let resolve_optional_value_for_attr db attr datoms tx max_eid tempids = function
  | Some value ->
    let value, max_eid, tempids = resolve_value_for_attr db attr datoms tx max_eid tempids value in
    Some value, max_eid, tempids
  | None -> None, max_eid, tempids

let resolve_entity_attrs db datoms tx max_eid tempids attrs =
  let attrs, max_eid, tempids =
    List.fold_left
      (fun (attrs, max_eid, tempids) (attr, tx_value) ->
        let tx_value, max_eid, tempids = resolve_tx_value_for_attr db attr datoms tx max_eid tempids tx_value in
        (attr, tx_value) :: attrs, max_eid, tempids)
      ([], max_eid, tempids)
      attrs
  in
  List.rev attrs, max_eid, tempids

let rec remap_value_ref old_e new_e = function
  | Ref entity_id when entity_id = old_e -> Ref new_e
  | List values ->
    List (List.map (remap_value_ref old_e new_e) values)
  | Vector values ->
    Vector (List.map (remap_value_ref old_e new_e) values)
  | Map entries ->
    Map
      (List.map
         (fun (key, value) ->
           remap_value_ref old_e new_e key, remap_value_ref old_e new_e value)
         entries)
  | Set values ->
    normalize_value (Set (List.map (remap_value_ref old_e new_e) values))
  | Tuple values ->
    Tuple
      (List.map
         (function
           | None -> None
           | Some value -> Some (remap_value_ref old_e new_e value))
         values)
  | value -> value

let remap_datom_entity old_e new_e d =
  { d with
    e = if d.e = old_e then new_e else d.e
  ; v = remap_value_ref old_e new_e d.v
  }

let remap_resolved_tx_value old_e new_e = function
  | One_value value -> One_value (remap_value_ref old_e new_e value)
  | Many_values values -> Many_values (List.map (remap_value_ref old_e new_e) values)
  | nested -> nested

let remap_tempid_entity old_e new_e tempids =
  List.map
    (fun (tempid, entity_id) ->
      if entity_id = old_e then
        tempid, new_e
      else
        tempid, entity_id)
    tempids

let default_schema_attr =
  { cardinality = One
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let schema_keyword_values datoms e attr =
  datoms
  |> List.filter_map (fun d ->
    match d.e = e, d.a = attr, d.v with
    | true, true, Keyword value -> Some value
    | _ -> None)
  |> List.rev

let schema_bool_value datoms e attr =
  datoms
  |> List.find_map (fun d ->
    match d.e = e, d.a = attr, d.v with
    | true, true, Bool value -> Some value
    | _ -> None)

let schema_string_value datoms e attr =
  datoms
  |> List.find_map (fun d ->
    match d.e = e, d.a = attr, d.v with
    | true, true, String value -> Some value
    | _ -> None)

let schema_keyword_value datoms e attr =
  match schema_keyword_values datoms e attr with
  | value :: _ -> Some value
  | [] -> None

let is_db_namespace_ident ident =
  String.length ident >= 3 && String.sub ident 0 3 = "db/"

let schema_fields =
  [ "db/cardinality"
  ; "db/valueType"
  ; "db/type"
  ; "db/unique"
  ; "db/index"
  ; "db/isComponent"
  ; "db/noHistory"
  ; "db/doc"
  ; "db/tupleAttrs"
  ; "db/tupleTypes"
  ]

let schema_field_removed removed_fields attr field =
  List.mem (attr, field) removed_fields

let schema_value_type_removed removed_fields attr =
  schema_field_removed removed_fields attr "db/valueType"
  || schema_field_removed removed_fields attr "db/type"

let value_type_of_schema_keyword = function
  | "db.type/ref" -> RefType
  | "db.type/tuple" -> TupleType
  | "db.type/string" -> StringType
  | "db.type/keyword" -> KeywordType
  | "db.type/number" -> NumberType
  | "db.type/uuid" -> UuidType
  | "db.type/instant" -> InstantType
  | value -> invalid_arg ("unknown schema value type: " ^ value)

let schema_attr_from_datoms
      ?(strict = true)
      ?(ignored_schema_entities = [])
      ?(removed_fields = [])
      current
      datoms
      e
  =
  let ident = schema_keyword_value datoms e "db/ident" in
  let has_schema_fields =
    List.exists
      (fun d ->
        d.e = e && List.mem d.a schema_fields)
      datoms
  in
  if has_schema_fields then begin
    (match ident with
     | Some ident when is_db_namespace_ident ident ->
       if strict then invalid_arg "schema transaction cannot install db namespace attrs"
     | _ -> ());
    let has_attr attr = List.exists (fun d -> d.e = e && d.a = attr) datoms in
    if has_attr "db/cardinality" || has_attr "db/valueType" || has_attr "db/type" then
      match ident, schema_keyword_value datoms e "db/cardinality" with
      | Some _, Some _ -> ()
      | None, _ when List.mem e ignored_schema_entities -> ()
      | _ ->
        if strict then invalid_arg "incomplete schema transaction attributes"
  end;
  match ident, has_schema_fields with
  | Some attr, true ->
    let base =
      match List.assoc_opt attr current with
      | Some spec -> spec
      | None -> default_schema_attr
    in
    let unique_removed = schema_field_removed removed_fields attr "db/unique" in
    let base =
      { cardinality =
          (if schema_field_removed removed_fields attr "db/cardinality" then
             default_schema_attr.cardinality
           else
             base.cardinality)
      ; value_type =
          (if schema_value_type_removed removed_fields attr then
             default_schema_attr.value_type
           else
             base.value_type)
      ; unique =
          (if unique_removed then
             default_schema_attr.unique
           else
             base.unique)
      ; indexed =
          (if
             schema_field_removed removed_fields attr "db/index"
             || (unique_removed && schema_bool_value datoms e "db/index" = None)
           then
             default_schema_attr.indexed
           else
             base.indexed)
      ; is_component =
          (if schema_field_removed removed_fields attr "db/isComponent" then
             default_schema_attr.is_component
           else
             base.is_component)
      ; no_history =
          (if schema_field_removed removed_fields attr "db/noHistory" then
             default_schema_attr.no_history
           else
             base.no_history)
      ; doc =
          (if schema_field_removed removed_fields attr "db/doc" then
             default_schema_attr.doc
           else
             base.doc)
      ; tuple_attrs =
          (if schema_field_removed removed_fields attr "db/tupleAttrs" then
             default_schema_attr.tuple_attrs
           else
             base.tuple_attrs)
      ; tuple_types =
          (if schema_field_removed removed_fields attr "db/tupleTypes" then
             default_schema_attr.tuple_types
           else
             base.tuple_types)
      }
    in
    let unique =
      match schema_keyword_value datoms e "db/unique" with
      | Some "db.unique/identity" -> Some Identity
      | Some "db.unique/value" -> Some Value
      | _ -> base.unique
    in
    let spec =
      { cardinality =
          (match schema_keyword_value datoms e "db/cardinality" with
           | Some "db.cardinality/many" -> Many
           | Some "db.cardinality/one" -> One
           | _ -> base.cardinality)
      ; value_type =
          (match
             match schema_keyword_value datoms e "db/valueType" with
             | Some _ as value_type -> value_type
             | None -> schema_keyword_value datoms e "db/type"
           with
           | Some value -> Some (value_type_of_schema_keyword value)
           | _ -> base.value_type)
      ; unique
      ; indexed =
          (match schema_bool_value datoms e "db/index" with
           | Some value -> value
           | None -> base.indexed || Option.is_some unique)
      ; is_component =
          (match schema_bool_value datoms e "db/isComponent" with
           | Some value -> value
           | None -> base.is_component)
      ; no_history =
          (match schema_bool_value datoms e "db/noHistory" with
           | Some value -> value
           | None -> base.no_history)
      ; doc =
          (match schema_string_value datoms e "db/doc" with
           | Some value -> Some value
           | None -> base.doc)
      ; tuple_attrs =
          (match schema_keyword_values datoms e "db/tupleAttrs" with
           | [] -> base.tuple_attrs
           | attrs -> Some attrs)
      ; tuple_types =
          (match schema_keyword_values datoms e "db/tupleTypes" with
           | [] -> base.tuple_types
           | types -> Some (List.map value_type_of_schema_keyword types))
      }
    in
    Some (attr, spec)
  | _ -> None

let schema_idents_from_datoms datoms =
  datoms
  |> List.filter_map (fun d ->
    match d.a, d.v with
    | "db/ident", Keyword ident -> Some ident
    | _ -> None)
  |> List.sort_uniq compare

let replace_schema_attr schema (attr, spec) =
  let schema = List.remove_assoc attr schema in
  schema @ [ attr, spec ]

let schema_from_transaction_datoms
      ?(strict = true)
      ?(removed_attrs = [])
      ?(removed_fields = [])
      ?(ignored_schema_entities = [])
      current
      datoms
  =
  let schema =
    let described_attrs = schema_idents_from_datoms datoms @ removed_attrs |> List.sort_uniq compare in
    List.filter (fun (attr, _) -> not (List.mem attr described_attrs)) current
  in
  datoms
  |> List.fold_left
       (fun schema d ->
         match schema_attr_from_datoms ~strict ~ignored_schema_entities ~removed_fields current datoms d.e with
         | Some entry -> replace_schema_attr schema entry
         | None -> schema)
       schema
  |> validate_schema

let apply_tx tx_ops db =
  if is_filtered db then invalid_arg "filtered db is read-only";
  let tx = db.max_tx + 1 in
  let current_schema = ref db.schema in
  let current_tx_fns = ref db.tx_fns in
  let removed_schema_attrs = ref [] in
  let removed_schema_fields = ref [] in
  let ignored_schema_entities = ref [] in
  let current_db () = { db with schema = !current_schema; tx_fns = !current_tx_fns } in
  let refresh_schema datoms =
    current_schema
    := schema_from_transaction_datoms
         ~strict:false
         ~removed_attrs:!removed_schema_attrs
         ~removed_fields:!removed_schema_fields
         ~ignored_schema_entities:!ignored_schema_entities
         db.schema
         datoms
  in
  let rec max_explicit_entity_ref max_eid = function
    | Entity_id e -> max max_eid (validate_entity_id e)
    | Lookup_ref (_, value) -> max_explicit_value max_eid value
    | _ -> max_eid
  and max_explicit_value max_eid = function
    | Ref entity_id -> max max_eid (validate_entity_id entity_id)
    | Ref_to entity_ref -> max_explicit_entity_ref max_eid entity_ref
    | List values | Vector values ->
      List.fold_left max_explicit_value max_eid values
    | Map entries ->
      List.fold_left
        (fun max_eid (key, value) ->
          max_explicit_value (max_explicit_value max_eid key) value)
        max_eid
        entries
    | Set values ->
      List.fold_left max_explicit_value max_eid values
    | Tuple values ->
      List.fold_left
        (fun max_eid -> function
          | None -> max_eid
          | Some value -> max_explicit_value max_eid value)
        max_eid
        values
    | _ -> max_eid
  and max_explicit_tx_value max_eid = function
    | One_value value -> max_explicit_value max_eid value
    | Many_values values -> List.fold_left max_explicit_value max_eid values
    | One_entity entity -> max_explicit_tx_entity max_eid entity
    | Many_entities entities -> List.fold_left max_explicit_tx_entity max_eid entities
  and max_explicit_tx_entity max_eid entity =
    let max_eid =
      match entity.db_id with
      | Some entity_ref -> max_explicit_entity_ref max_eid entity_ref
      | None -> max_eid
    in
    entity.attrs
    |> List.fold_left (fun max_eid (_, tx_value) -> max_explicit_tx_value max_eid tx_value) max_eid
  and max_explicit_tx_op max_eid = function
    | Add (entity_ref, _, value) ->
      let max_eid = max_explicit_entity_ref max_eid entity_ref in
      max_explicit_value max_eid value
    | Retract (entity_ref, _, value) ->
      let max_eid = max_explicit_entity_ref max_eid entity_ref in
      (match value with
       | Some value -> max_explicit_value max_eid value
       | None -> max_eid)
    | RetractEntity entity_ref | RetractAttr (entity_ref, _) -> max_explicit_entity_ref max_eid entity_ref
    | CompareAndSet (entity_ref, _, expected, new_value) ->
      let max_eid = max_explicit_entity_ref max_eid entity_ref in
      let max_eid =
        match expected with
        | Some expected -> max_explicit_value max_eid expected
        | None -> max_eid
      in
      max_explicit_value max_eid new_value
    | Entity entity -> max_explicit_tx_entity max_eid entity
    | Raw_datom d -> max_eid_in_value (max max_eid (validate_entity_id d.e)) d.v
    | InstallTxFn (entity_ref, _) -> max_explicit_entity_ref max_eid entity_ref
    | CallIdent (entity_ref, args) ->
      let max_eid = max_explicit_entity_ref max_eid entity_ref in
      List.fold_left max_explicit_value max_eid args
    | Call _ -> max_eid
  in
  let initial_max_eid = List.fold_left max_explicit_tx_op db.max_eid tx_ops in
  let max_tx_seen = ref tx in
  let mark_entity_tempid entity_tempids = function
    | Temp_id tempid when not (List.mem tempid entity_tempids) -> tempid :: entity_tempids
    | _ -> entity_tempids
  in
  let validate_tempid_usage tempids entity_tempids =
    let value_only =
      tempids
      |> List.filter_map (fun (tempid, _) ->
        if tempid <> "db/current-tx" && not (is_current_tx_alias tempid) && not (List.mem tempid entity_tempids) then
          Some tempid
        else
          None)
    in
    match value_only with
    | [] -> ()
    | tempids ->
      invalid_arg
        ("Tempids used only as value in transaction: ("
         ^ String.concat " " tempids
         ^ ")")
  in
  let rec tx_value_has_assertions attr = function
    | One_value (List []) | One_value (Vector []) | One_value (Set []) when attr_expands_collection db attr -> false
    | Many_values [] | Many_entities [] -> false
    | One_entity _ | Many_entities _ -> true
    | One_value _ | Many_values _ -> true
  and tx_entity_has_assertions (entity : tx_entity) =
    List.exists (fun (attr, tx_value) -> tx_value_has_assertions attr tx_value) entity.attrs
  in
  let remember_removed_schema_ident entity_id ident =
    if not (List.mem ident !removed_schema_attrs) then
      removed_schema_attrs := ident :: !removed_schema_attrs;
    if not (List.mem entity_id !ignored_schema_entities) then
      ignored_schema_entities := entity_id :: !ignored_schema_entities
  in
  let note_schema_ident_retraction datoms entity_id = function
    | Some (Keyword ident) -> remember_removed_schema_ident entity_id ident
    | None ->
      (match current_attr_value datoms entity_id "db/ident" with
       | Some (Keyword ident) -> remember_removed_schema_ident entity_id ident
       | _ -> ())
    | Some _ -> ()
  in
  let note_schema_field_retraction datoms entity_id field =
    if List.mem field schema_fields then
      match current_attr_value datoms entity_id "db/ident" with
      | Some (Keyword ident) ->
        let removed = ident, field in
        if not (List.mem removed !removed_schema_fields) then
          removed_schema_fields := removed :: !removed_schema_fields
      | _ -> ()
  in
  let add_resolved_attr_value e attr value (datoms, max_eid, tempids, entity_tempids, tx_data) =
    let db = current_db () in
    let datoms, datom_tx_data = add_entity_attr_value db tx datoms e attr value in
    datoms, max_eid, tempids, entity_tempids, tx_data @ datom_tx_data
  in
  let merge_tempid_entity tempid old_e target_e datoms tempids tx_data =
    let db = current_db () in
    if old_e <= db.max_eid then
      invalid_arg
        ("Conflicting upsert: "
         ^ tempid
         ^ " resolves both to "
         ^ string_of_int old_e
         ^ " and "
         ^ string_of_int target_e);
    let old_datoms, kept_datoms = List.partition (fun d -> d.e = old_e) datoms in
    let dedupe_facts datoms =
      datoms
      |> List.fold_left
           (fun deduped d ->
             if List.exists (same_fact d) deduped then deduped else d :: deduped)
           []
    in
    let kept_datoms =
      kept_datoms
      |> List.map (remap_datom_entity old_e target_e)
      |> dedupe_facts
    in
    let datoms, moved_tx_data =
      old_datoms
      |> List.fold_left
           (fun (datoms, moved_tx_data) d ->
             if is_tuple_attr db d.a then
               datoms, moved_tx_data
             else
               let d = remap_datom_entity old_e target_e d in
               let datoms, datom_tx_data = add_user_datom_with_report db tx datoms d in
               datoms, moved_tx_data @ datom_tx_data)
           (kept_datoms, [])
    in
    let tx_data =
      tx_data
      |> List.filter_map (fun d ->
        if d.e = old_e then None else Some (remap_datom_entity old_e target_e d))
    in
    let tx_data = tx_data @ moved_tx_data in
    let tempids = remap_tempid_entity old_e target_e tempids in
    datoms, tempids, tx_data
  in
  let tuple_identity_target_for_add datoms e attr value =
    let db = current_db () in
    tuple_attrs_for_source db attr
    |> List.find_map (fun (tuple_attr, source_attrs) ->
      if is_unique_identity db tuple_attr then
        let values =
          List.map
            (fun source_attr ->
              if source_attr = attr then Some value
              else current_attr_value datoms e source_attr)
            source_attrs
        in
        if List.for_all Option.is_some values then
          match entid_in_datoms db datoms tuple_attr (Tuple values) with
          | Some target_e when target_e <> e -> Some target_e
          | _ -> None
        else
          None
      else
        None)
  in
  let resolve_add_tempid datoms max_eid tempids tx_data tempid attr value =
    let db = current_db () in
    if is_unique_identity db attr then
      match entid_in_datoms db datoms attr value, List.assoc_opt tempid tempids with
      | Some target_e, Some old_e when old_e <> target_e ->
        let datoms, tempids, tx_data = merge_tempid_entity tempid old_e target_e datoms tempids tx_data in
        target_e, datoms, max max_eid target_e, remember_tempid tempids tempid target_e, tx_data
      | Some target_e, _ ->
        target_e, datoms, max max_eid target_e, remember_tempid tempids tempid target_e, tx_data
      | None, _ ->
        let e, max_eid, tempids = resolve_entity_ref db datoms tx max_eid tempids (Temp_id tempid) in
        e, datoms, max_eid, tempids, tx_data
    else
      let e, max_eid, tempids = resolve_entity_ref db datoms tx max_eid tempids (Temp_id tempid) in
      match tuple_identity_target_for_add datoms e attr value with
      | Some target_e ->
        let datoms, tempids, tx_data = merge_tempid_entity tempid e target_e datoms tempids tx_data in
        target_e, datoms, max max_eid target_e, remember_tempid tempids tempid target_e, tx_data
      | None -> e, datoms, max_eid, tempids, tx_data
  in
  let is_forward_nested_attr = function
    | attr, (One_entity _ | Many_entities _) -> not (is_reverse_ref attr)
    | _ -> false
  in
  let has_only_forward_nested_attrs (entity : tx_entity) =
    entity.attrs <> [] && List.for_all is_forward_nested_attr entity.attrs
  in
  let rec tx_value_has_schema_fields = function
    | One_value _ | Many_values _ -> false
    | One_entity entity -> tx_entity_has_schema_fields entity
    | Many_entities entities -> List.exists tx_entity_has_schema_fields entities
  and tx_entity_has_schema_fields entity =
    entity.attrs
    |> List.exists (fun (attr, value) -> attr = "db/ident" || List.mem attr schema_fields || tx_value_has_schema_fields value)
  in
  let tx_op_affects_schema = function
    | Add (_, attr, _) | Raw_datom { a = attr; _ } ->
      attr = "db/ident" || List.mem attr schema_fields
    | Entity entity -> tx_entity_has_schema_fields entity
    | Retract _ | RetractEntity _ | RetractAttr _ -> true
    | CompareAndSet (_, attr, _, _) -> attr = "db/ident" || List.mem attr schema_fields
    | InstallTxFn _ | CallIdent _ | Call _ -> false
  in
  let resolve_transaction_function_ref datoms max_eid tempids entity_ref =
    match entity_ref with
    | Ident ident ->
      (match entid_in_datoms (current_db ()) datoms ident_attr (Keyword ident) with
       | Some e -> e, max max_eid e, tempids
       | None -> invalid_arg ("Cannot find entity for transaction fn: " ^ ident))
    | _ ->
      resolve_entity_ref (current_db ()) datoms tx max_eid tempids entity_ref
  in
  let resolve_call_args datoms max_eid tempids args =
    args
    |> List.fold_left
         (fun (args, max_eid, tempids) arg ->
           let arg, max_eid, tempids = resolve_value (current_db ()) datoms tx max_eid tempids arg in
           arg :: args, max_eid, tempids)
         ([], max_eid, tempids)
    |> fun (args, max_eid, tempids) -> List.rev args, max_eid, tempids
  in
  let rec apply_op (datoms, max_eid, tempids, entity_tempids, tx_data) tx_op =
    let db = current_db () in
    match tx_op with
    | Add (e, a, v) ->
      let entity_ref = e in
      let e, v, datoms, max_eid, tempids, tx_data =
        match e with
        | Temp_id tempid ->
          let v, max_eid, tempids = resolve_value_for_attr db a datoms tx max_eid tempids v in
          let e, datoms, max_eid, tempids, tx_data =
            resolve_add_tempid datoms max_eid tempids tx_data tempid a v
          in
          e, v, datoms, max_eid, tempids, tx_data
        | _ ->
          let e, max_eid, tempids = resolve_entity_ref db datoms tx max_eid tempids e in
          let v, max_eid, tempids = resolve_value_for_attr db a datoms tx max_eid tempids v in
          e, v, datoms, max_eid, tempids, tx_data
      in
      let entity_tempids = mark_entity_tempid entity_tempids entity_ref in
      let d = datom ~tx ~e ~a ~v () in
      let datoms, datom_tx_data = add_user_datom_with_report db tx datoms d in
      datoms, max_eid, tempids, entity_tempids, tx_data @ datom_tx_data
    | Retract (e, a, value) ->
      let e, max_eid, tempids = resolve_optional_existing_entity_ref db datoms tx max_eid tempids e in
      (match e with
       | None -> datoms, max_eid, tempids, entity_tempids, tx_data
       | Some e ->
         let value, max_eid, tempids = resolve_optional_value_for_attr db a datoms tx max_eid tempids value in
         if a = "db/ident" then note_schema_ident_retraction datoms e value;
         note_schema_field_retraction datoms e a;
         let datoms, datom_tx_data = retract_user_attr_with_report db tx datoms e a value in
         datoms, max_eid, tempids, entity_tempids, tx_data @ datom_tx_data)
    | RetractEntity e ->
      let e, max_eid, tempids = resolve_optional_existing_entity_ref db datoms tx max_eid tempids e in
      (match e with
       | None -> datoms, max_eid, tempids, entity_tempids, tx_data
       | Some e ->
         note_schema_ident_retraction datoms e None;
         let datoms, datom_tx_data = retract_entity_with_report db tx datoms e in
         datoms, max_eid, tempids, entity_tempids, tx_data @ datom_tx_data)
    | RetractAttr (e, a) ->
      let e, max_eid, tempids = resolve_optional_existing_entity_ref db datoms tx max_eid tempids e in
      (match e with
       | None -> datoms, max_eid, tempids, entity_tempids, tx_data
       | Some e ->
         if a = "db/ident" then note_schema_ident_retraction datoms e None;
         note_schema_field_retraction datoms e a;
         let datoms, datom_tx_data = retract_user_attr_with_report db tx datoms e a None in
         datoms, max_eid, tempids, entity_tempids, tx_data @ datom_tx_data)
    | CompareAndSet (e, a, expected, new_value) ->
      let e, max_eid, tempids = resolve_existing_entity_ref db datoms tx max_eid tempids e in
      let expected, max_eid, tempids = resolve_optional_value_for_attr db a datoms tx max_eid tempids expected in
      let new_value, max_eid, tempids = resolve_value_for_attr db a datoms tx max_eid tempids new_value in
      if not (compare_and_set_matches db datoms e a expected) then
        invalid_arg (compare_and_set_failure_message db datoms e a expected);
      let d = datom ~tx ~e ~a ~v:new_value () in
      let datoms, datom_tx_data = add_user_datom_with_report db tx datoms d in
      datoms, max_eid, tempids, entity_tempids, tx_data @ datom_tx_data
    | Raw_datom d ->
      let d = normalize_datom_for_schema db.schema d in
      max_tx_seen := max !max_tx_seen d.tx;
      if d.added then
        let datoms, datom_tx_data = add_active_datom_with_report ~allow_tuple:true db d.tx datoms d in
        datoms, max_eid_in_value (max max_eid d.e) d.v, tempids, entity_tempids, tx_data @ datom_tx_data
      else
        begin
        if d.a = "db/ident" then note_schema_ident_retraction datoms d.e (Some d.v);
        note_schema_field_retraction datoms d.e d.a;
        let datoms, datom_tx_data = retract_active_datom_with_report d.tx datoms d.e d.a (Some d.v) in
        datoms, max_eid_in_value (max max_eid d.e) d.v, tempids, entity_tempids, tx_data @ datom_tx_data
        end
    | Call f ->
      let db_for_call = with_db_datoms { db with max_eid } datoms in
      apply_ops (datoms, max_eid, tempids, entity_tempids, tx_data) (f db_for_call)
    | InstallTxFn (entity_ref, f) ->
      let e, max_eid, tempids = resolve_transaction_function_ref datoms max_eid tempids entity_ref in
      current_tx_fns := (e, f) :: List.remove_assoc e !current_tx_fns;
      datoms, max_eid, tempids, mark_entity_tempid entity_tempids entity_ref, tx_data
    | CallIdent (entity_ref, args) ->
      let e, max_eid, tempids = resolve_transaction_function_ref datoms max_eid tempids entity_ref in
      let args, max_eid, tempids = resolve_call_args datoms max_eid tempids args in
      (match List.assoc_opt e !current_tx_fns with
       | Some f ->
         let db_for_call = with_db_datoms { db with max_eid } datoms in
         apply_ops (datoms, max_eid, tempids, entity_tempids, tx_data) (f db_for_call args)
       | None -> invalid_arg "Entity expected to have transaction function metadata")
    | Entity entity when not (tx_entity_has_assertions entity) ->
      datoms, max_eid, tempids, entity_tempids, tx_data
    | Entity entity ->
      let datoms, max_eid, tempids, entity_tempids, tx_data, _ =
        apply_entity_map (datoms, max_eid, tempids, entity_tempids, tx_data) entity
      in
      datoms, max_eid, tempids, entity_tempids, tx_data
  and apply_entity_map (datoms, max_eid, tempids, entity_tempids, tx_data) entity =
    let db = current_db () in
    let entity =
      { entity with attrs = List.filter (fun (attr, _) -> attr <> "db/id") entity.attrs }
    in
    if entity.db_id = None && has_only_forward_nested_attrs entity then
      apply_nested_first_entity_map (datoms, max_eid, tempids, entity_tempids, tx_data) entity
    else
      let e, attrs, datoms, max_eid, tempids, tx_data =
        match entity.db_id with
        | Some (Temp_id tempid) ->
          let attrs, max_eid, tempids = resolve_entity_attrs db datoms tx max_eid tempids entity.attrs in
          (match entity_unique_identity db datoms attrs with
           | Some target_e ->
             let datoms, tempids, tx_data, attrs =
               match List.assoc_opt tempid tempids with
               | Some old_e when old_e <> target_e ->
                 let datoms, tempids, tx_data = merge_tempid_entity tempid old_e target_e datoms tempids tx_data in
                 let attrs =
                   List.map
                     (fun (attr, tx_value) -> attr, remap_resolved_tx_value old_e target_e tx_value)
                     attrs
                 in
                 datoms, tempids, tx_data, attrs
               | _ -> datoms, tempids, tx_data, attrs
             in
             target_e, attrs, datoms, max max_eid target_e, remember_tempid tempids tempid target_e, tx_data
           | None ->
             let e, max_eid, tempids = resolve_entity_ref db datoms tx max_eid tempids (Temp_id tempid) in
             e, attrs, datoms, max_eid, tempids, tx_data)
        | Some entity_ref ->
          let e, max_eid, tempids = resolve_entity_ref db datoms tx max_eid tempids entity_ref in
          let attrs, max_eid, tempids = resolve_entity_attrs db datoms tx max_eid tempids entity.attrs in
          validate_explicit_upsert_target db datoms e attrs;
          e, attrs, datoms, max_eid, tempids, tx_data
        | None ->
          let e = allocate_entity_id max_eid in
          let attrs, max_eid, tempids = resolve_entity_attrs db datoms tx e tempids entity.attrs in
          (match entity_unique_identity db datoms attrs with
           | Some e -> e, attrs, datoms, max max_eid e, tempids, tx_data
           | None -> e, attrs, datoms, max_eid, tempids, tx_data)
      in
      let entity_tempids =
        match entity.db_id with
        | Some entity_ref -> mark_entity_tempid entity_tempids entity_ref
        | None -> entity_tempids
      in
      let tuple_identity_lookup_writes =
        attrs
        |> List.filter_map (function
          | attr, One_value value when is_tuple_attr db attr && is_unique_identity db attr ->
            (match entid_in_datoms db datoms attr value with
             | Some target_e when target_e = e -> Some (attr, value)
             | _ -> None)
          | _ -> None)
      in
      let tuple_identity_write_was_lookup attr value =
        List.exists
          (fun (lookup_attr, lookup_value) ->
            lookup_attr = attr && value_equal lookup_value value)
          tuple_identity_lookup_writes
      in
      let add_entity_map_attr_value
            parent_e
            attr
            value
            (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
        =
        let actual_e, actual_attr, actual_value = normalize_entity_attr_value db parent_e attr value in
        if is_tuple_attr db actual_attr then
          if tuple_identity_write_was_lookup actual_attr actual_value then
            datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes
          else
            ( datoms
            , max_eid
            , tempids
            , entity_tempids
            , tx_data
            , tuple_sources
            , (actual_e, actual_attr, actual_value) :: direct_tuple_writes )
        else if tuple_attrs_for_source db actual_attr <> [] then
          let datoms, datom_tx_data =
            add_active_datom_with_report db tx datoms (datom ~tx ~e:actual_e ~a:actual_attr ~v:actual_value ())
          in
          ( datoms
          , max_eid
          , tempids
          , entity_tempids
          , tx_data @ datom_tx_data
          , (actual_e, actual_attr) :: tuple_sources
          , direct_tuple_writes )
        else
          let datoms, max_eid, tempids, entity_tempids, tx_data =
            add_resolved_attr_value parent_e attr value (datoms, max_eid, tempids, entity_tempids, tx_data)
          in
          datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes
      in
      let apply_nested_entity
            parent_e
            attr
            (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
            (nested : tx_entity)
        =
        if is_reverse_ref attr then
          begin
          if not (is_ref_attr db (reverse_ref attr)) then
            invalid_arg "reverse nested entity attribute requires ref schema";
          let nested = { nested with attrs = nested.attrs @ [ reverse_ref attr, One_value (Ref parent_e) ] } in
          let datoms, max_eid, tempids, entity_tempids, tx_data, _ =
            apply_entity_map (datoms, max_eid, tempids, entity_tempids, tx_data) nested
          in
          datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes
          end
        else
          begin
          if not (is_ref_attr db attr) then
            invalid_arg "nested entity attribute requires ref schema";
          let datoms, max_eid, tempids, entity_tempids, tx_data, nested_e =
            apply_entity_map (datoms, max_eid, tempids, entity_tempids, tx_data) nested
          in
          add_entity_map_attr_value
            parent_e
            attr
            (Ref nested_e)
            (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
          end
      in
      let apply_attr (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes) (attr, tx_value) =
        match tx_value with
        | One_value (List values | Vector values) when attr_expands_collection db attr ->
          List.fold_left
            (fun state value -> add_entity_map_attr_value e attr value state)
            (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
            values
        | One_value (Set values) when attr_expands_collection db attr ->
          List.fold_left
            (fun state value -> add_entity_map_attr_value e attr value state)
            (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
            values
        | One_value value ->
          add_entity_map_attr_value e attr value (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
        | Many_values values ->
          List.fold_left
            (fun state value -> add_entity_map_attr_value e attr value state)
            (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
            values
        | One_entity nested ->
          apply_nested_entity e attr (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes) nested
        | Many_entities nested_entities ->
          List.fold_left
            (apply_nested_entity e attr)
            (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
            nested_entities
      in
      let datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes =
        List.fold_left apply_attr (datoms, max_eid, tempids, entity_tempids, tx_data, [], []) attrs
      in
      let tuple_sources = List.sort_uniq compare tuple_sources in
      let datoms, tx_data =
        List.fold_left
          (fun (datoms, tx_data) (entity_id, source_attr) ->
            refresh_tuple_attrs_for_source db tx datoms entity_id source_attr tx_data)
          (datoms, tx_data)
          tuple_sources
      in
      List.iter
        (fun (e, a, v) ->
          if not (tuple_direct_write_matches_sources db datoms (datom ~tx ~e ~a ~v ())) then
            invalid_arg "cannot modify tuple attributes directly")
        direct_tuple_writes;
      datoms, max_eid, tempids, entity_tempids, tx_data, e
  and apply_nested_first_entity_map state entity =
      let db = current_db () in
      let transact_nested state nested =
        let datoms, max_eid, tempids, entity_tempids, tx_data, nested_e =
          apply_entity_map state nested
        in
        (datoms, max_eid, tempids, entity_tempids, tx_data), Ref nested_e
      in
      let transact_nested_attr (state, attrs) (attr, tx_value) =
        if not (is_ref_attr db attr) then
          invalid_arg "nested entity attribute requires ref schema";
        match tx_value with
        | One_entity nested ->
          let state, ref_value = transact_nested state nested in
          state, (attr, One_value ref_value) :: attrs
        | Many_entities nested_entities ->
          let state, values =
            List.fold_left
              (fun (state, values) nested ->
                let state, ref_value = transact_nested state nested in
                state, ref_value :: values)
              (state, [])
              nested_entities
          in
          state, (attr, Many_values (List.rev values)) :: attrs
        | One_value _ | Many_values _ -> state, attrs
      in
      let (datoms, max_eid, tempids, entity_tempids, tx_data), attrs =
        List.fold_left transact_nested_attr (state, []) entity.attrs
      in
      let attrs = List.rev attrs in
      let e, max_eid =
        match entity_unique_identity db datoms attrs with
        | Some e -> e, max max_eid e
        | None ->
          let e = allocate_entity_id max_eid in
          e, e
      in
      let apply_attr (datoms, max_eid, tempids, entity_tempids, tx_data) (attr, tx_value) =
        match tx_value with
        | One_value value -> add_resolved_attr_value e attr value (datoms, max_eid, tempids, entity_tempids, tx_data)
        | Many_values values ->
          List.fold_left
            (fun state value -> add_resolved_attr_value e attr value state)
            (datoms, max_eid, tempids, entity_tempids, tx_data)
            values
        | One_entity _ | Many_entities _ -> datoms, max_eid, tempids, entity_tempids, tx_data
      in
      let datoms, max_eid, tempids, entity_tempids, tx_data =
        List.fold_left apply_attr (datoms, max_eid, tempids, entity_tempids, tx_data) attrs
      in
      datoms, max_eid, tempids, entity_tempids, tx_data, e
  and apply_ops state tx_ops =
    List.fold_left
      (fun state tx_op ->
        let state = apply_op state tx_op in
        let datoms, _, _, _, _ = state in
        if tx_op_affects_schema tx_op then refresh_schema datoms;
        state)
      state
      tx_ops
  in
  let datoms, max_eid, tempids, entity_tempids, tx_data =
    apply_ops (db.datoms, initial_max_eid, [], [], []) tx_ops
  in
  let tempids = ensure_current_tx_tempid tempids tx in
  validate_tempid_usage tempids entity_tempids;
  let schema =
    schema_from_transaction_datoms
      ~strict:true
      ~removed_attrs:!removed_schema_attrs
      ~removed_fields:!removed_schema_fields
      ~ignored_schema_entities:!ignored_schema_entities
      db.schema
      datoms
  in
  let history_tx_data = history_datoms_for_schema schema tx_data in
  ( refresh_db_indexes
      (refresh_db_identity
         { db with
           schema
         ; datoms
         ; history_datoms = db.history_datoms @ history_tx_data
         ; max_eid
         ; max_tx = !max_tx_seen
         ; tx_fns = !current_tx_fns
         })
  , tempids
  , tx_data
  )

let db_with tx_ops db =
  let db_after, _, _ = apply_tx tx_ops db in
  db_after

let storage_tail_context : Storage.tail_context =
  { apply_group = (fun db group -> db_with (List.map (fun datom -> Raw_datom datom) group) db)
  }

let db_with_tail db tail =
  Storage.db_with_tail storage_tail_context db tail

let storage_restore_context : Storage.restore_context =
  { from_serializable; db_with_tail }

let restore storage =
  Storage.restore storage_restore_context storage

let restore_conn storage =
  let context : Conn.restore_context = { restore; restore_tail_groups } in
  Conn.restore context storage

let transact ?(tx_meta = []) db tx_ops =
  let db_after, tempids, tx_data = apply_tx tx_ops db in
  { db_before = db; db_after; tx_data; tempids; tx_meta }

let with_tx ?tx_meta db tx_ops = transact ?tx_meta db tx_ops

let transact_conn ?(tx_meta = []) conn tx_data =
  let context : Conn.transact_context =
    { store
    ; store_tail
    ; storage_tail_datom_count
    ; storage_tail_compaction_threshold
    ; transact = (fun ~tx_meta db tx_data -> transact ~tx_meta db tx_data)
    }
  in
  Conn.transact context ~tx_meta conn tx_data

let transact_bang ?tx_meta conn tx_data = transact_conn ?tx_meta conn tx_data

let transact_async ?tx_meta conn tx_data = transact_conn ?tx_meta conn tx_data

let last_tempid = ref 0

let tempid ?part ?value () =
  match part, value with
  | Some "db.part/tx", _ | Some ":db.part/tx", _ -> CurrentTx
  | _, Some value when value > 0 -> Entity_id (validate_entity_id value)
  | _, Some value -> Temp_id (string_of_int value)
  | _ ->
    decr last_tempid;
    Temp_id (string_of_int !last_tempid)

let resolve_tempid ?db:_ tempids tempid = List.assoc_opt tempid tempids

let is_avet_accessible db attr =
  is_ref_attr db attr
  || is_unique db attr
  || is_indexed db attr

let rec resolve_index_entity_ref db = function
  | Entity_id entity_id -> Some entity_id
  | Ident ident -> entid db ident_attr (Keyword ident)
  | Lookup_ref (attr, value) ->
    let value = resolve_index_value db value in
    lookup_ref_entity_id ~strict_missing:true db attr value
  | CurrentTx | Temp_id _ -> None

and resolve_index_value db = function
  | Ref_to entity_ref ->
    (match resolve_index_entity_ref db entity_ref with
     | Some entity_id -> Ref entity_id
     | None -> invalid_arg (unresolved_entity_ref_message entity_ref))
  | List values ->
    normalize_value (List (List.map (resolve_index_value db) values))
  | Vector values ->
    normalize_value (Vector (List.map (resolve_index_value db) values))
  | Map entries ->
    normalize_value
      (Map
         (List.map
            (fun (key, value) ->
              resolve_index_value db key, resolve_index_value db value)
            entries))
  | Set values ->
    normalize_value (Set (List.map (resolve_index_value db) values))
  | Tuple values ->
    normalize_value
      (Tuple
         (List.map
            (function
              | None -> None
              | Some value -> Some (resolve_index_value db value))
            values))
  | value -> normalize_value value

let entid_ref db = function
  | Entity_id entity_id -> Some (validate_entity_id entity_id)
  | Ident ident -> entid db ident_attr (Keyword ident)
  | Lookup_ref (attr, value) -> lookup_ref_entity_id db attr (resolve_index_value db value)
  | CurrentTx | Temp_id _ -> invalid_arg "transaction-local entity refs cannot be resolved from a db"

let resolve_index_value_option db = Option.map (resolve_index_value db)

let resolve_index_value_for_attr db attr value =
  match ref_attr_for_value_resolution db attr, entity_ref_of_ref_attr_value value with
  | Some _, Some entity_ref ->
    (match resolve_index_entity_ref db entity_ref with
     | Some entity_id -> Ref entity_id
     | None -> invalid_arg (unresolved_entity_ref_message entity_ref))
  | _ -> resolve_index_value db value

let resolve_index_value_option_for_attr db attr = Option.map (resolve_index_value_for_attr db attr)

let resolve_index_value_option_for_optional_attr db attr value =
  match attr with
  | Some attr -> resolve_index_value_option_for_attr db attr value
  | None -> resolve_index_value_option db value

let resolve_index_entity_ref_exn db entity_ref =
  match resolve_index_entity_ref db entity_ref with
  | Some entity_id -> entity_id
  | None -> invalid_arg (unresolved_entity_ref_message entity_ref)

let db_index_context : Db_impl.index_context =
  { is_avet_accessible
  ; resolve_entity_ref = resolve_index_entity_ref_exn
  ; resolve_value_for_optional_attr =
      (fun db attr value -> resolve_index_value_option_for_optional_attr db attr (Some value) |> Option.get)
  ; resolve_value_for_attr = resolve_index_value_for_attr
  ; compare_value
  ; first_nonzero
  }

let datoms db index ?e ?a ?v ?tx () =
  Db_impl.datoms db_index_context db index ?e ?a ?v ?tx ()

let datoms_ref db index ?e ?a ?v ?tx () =
  Db_impl.datoms_ref db_index_context db index ?e ?a ?v ?tx ()

let diff = Db_impl.diff

let db_hash = Db_impl.hash

let db_hash_cache_size = Db_impl.hash_cache_size

let squuid = Db_impl.squuid

let squuid_time_millis = Db_impl.squuid_time_millis

let reset_conn ?(tx_meta = []) conn db =
  let context : Conn.reset_context =
    { store; datoms = (fun db -> datoms db Eavt ()) }
  in
  Conn.reset context ~tx_meta conn db

let reset_conn_bang ?tx_meta conn db = reset_conn ?tx_meta conn db

let find_datom db index ?e ?a ?v ?tx () =
  Db_impl.find_datom db_index_context db index ?e ?a ?v ?tx ()

let find_datom_ref db index ?e ?a ?v ?tx () =
  Db_impl.find_datom_ref db_index_context db index ?e ?a ?v ?tx ()

let seek_datoms db index ?e ?a ?v ?tx () =
  Db_impl.seek_datoms db_index_context db index ?e ?a ?v ?tx ()

let seek_datoms_ref db index ?e ?a ?v ?tx () =
  Db_impl.seek_datoms_ref db_index_context db index ?e ?a ?v ?tx ()

let rseek_datoms db index ?e ?a ?v ?tx () =
  Db_impl.rseek_datoms db_index_context db index ?e ?a ?v ?tx ()

let rseek_datoms_ref db index ?e ?a ?v ?tx () =
  Db_impl.rseek_datoms_ref db_index_context db index ?e ?a ?v ?tx ()

let index_range db attr ?start ?stop () =
  Db_impl.index_range db_index_context db attr ?start ?stop ()

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

let entity_context =
  { Entity.datoms_by_entity = (fun db entity_id -> datoms db Eavt ~e:entity_id ())
  ; all_datoms = (fun db -> datoms db Eavt ())
  ; compare_value
  ; cardinality
  ; is_ref_attr
  ; is_component
  ; reverse_ref
  ; is_reverse_ref
  ; entity_id_of_ref
  }

let entity db entity_ref =
  Entity.entity entity_context db entity_ref

let entity_attr_raw = Entity.entity_attr_raw

let entity_attr entity attr =
  Entity.entity_attr entity_context entity attr

let entity_db = Entity.entity_db

let is_entity = Entity.is_entity

let entity_equal = Entity.entity_equal

let entity_hash = Entity.entity_hash

let touch entity =
  Entity.touch entity_context entity

module Pull_api_impl = Pull_api

let pull_api_context : Pull_api_impl.context =
  { compare_value
  ; entity
  ; entity_attr_raw
  ; datoms_by_avet_ref = (fun db attr entity_id -> datoms db Avet ~a:attr ~v:(Ref entity_id) ())
  ; is_component
  ; is_reverse_ref
  ; reverse_ref
  ; entity_id_of_ref
  }

let pull ?visitor db selector entity_ref =
  Pull_api_impl.pull ?visitor pull_api_context db selector entity_ref

let pull_many ?visitor db selector entity_refs =
  Pull_api_impl.pull_many ?visitor pull_api_context db selector entity_refs

module Parser_impl = Parser

let read_edn = Parser_impl.read_edn

let query_value_of_form form = normalize_value (Parser_impl.query_value_of_form form)

let query_form_of_value = Parser_impl.query_form_of_value

module Data_readers_impl = Data_readers

let data_readers_context : Data_readers_impl.context =
  { tx0
  ; read_edn
  ; query_value_of_form
  ; datom
  ; validate_schema
  ; empty_db = (fun ?(schema = []) () -> empty_db ~schema ())
  ; max_eid_in_value
  ; resolve_value_for_attr
  ; init_db = (fun ?(schema = []) datoms -> init_db ~schema datoms)
  }

module Data_readers = struct
  let attr_of_edn_key = Data_readers_impl.attr_of_edn_key
  let tx_attr_of_edn_key = Data_readers_impl.tx_attr_of_edn_key
  let tx_op_name_of_edn_form = Data_readers_impl.tx_op_name_of_edn_form
  let is_edn_attr_key = Data_readers_impl.is_edn_attr_key
  let keyword_name_of_form = Data_readers_impl.keyword_name_of_form
  let entity_ref_of_edn_form form = Data_readers_impl.entity_ref_of_edn_form data_readers_context form
  let tx_data_of_edn_form form = Data_readers_impl.tx_data_of_edn_form data_readers_context form
  let parse_tx_data_string input = Data_readers_impl.parse_tx_data_string data_readers_context input
  let schema_of_edn_form form = Data_readers_impl.schema_of_edn_form data_readers_context form
  let schema_of_edn_string input = Data_readers_impl.schema_of_edn_string data_readers_context input
  let db_from_reader_form form = Data_readers_impl.db_from_reader_form data_readers_context form
  let db_from_reader_string input = Data_readers_impl.db_from_reader_string data_readers_context input
end

let parse_tx_data_string = Data_readers.parse_tx_data_string
let schema_of_edn_string = Data_readers.schema_of_edn_string
let db_from_reader_string = Data_readers.db_from_reader_string

let db_with_string input db =
  db_with (parse_tx_data_string input) db

let transact_string ?tx_meta db input =
  transact ?tx_meta db (parse_tx_data_string input)

let with_tx_string ?tx_meta db input =
  transact_string ?tx_meta db input

let transact_conn_string ?tx_meta conn input =
  transact_conn ?tx_meta conn (parse_tx_data_string input)

let transact_bang_string ?tx_meta conn input =
  transact_conn_string ?tx_meta conn input

let transact_async_string ?tx_meta conn input =
  transact_conn_string ?tx_meta conn input

module Pull_parser_impl = Pull_parser

let pull_parser_context : Pull_parser_impl.context =
  { cardinality
  ; is_ref_attr
  ; is_reverse_ref
  ; reverse_ref
  ; query_value_of_form
  ; read_edn
  ; split_keyword
  }

let parse_pull_pattern db form =
  Pull_parser_impl.parse_pattern pull_parser_context db form

let parse_pull_pattern_string db input =
  Pull_parser_impl.parse_pattern_string pull_parser_context db input

let result_of_datom_tx = Query.result_of_datom_tx
let result_of_datom_op = Query.result_of_datom_op
let result_of_ref = Query.result_of_ref

let resolve_query_value db value = resolve_ref_value ~preserve_vector:true db value

let query_result_context db : Query.result_resolution_context =
  { validate_entity_id
  ; resolve_query_value = resolve_query_value db
  ; lookup_ref_entity_id = (fun attr value -> entity_id_of_ref db (Lookup_ref (attr, value)))
  }

let query_match_context db : Query.match_context =
  { result_resolution_context = query_result_context db
  ; source_db = db
  ; ident_entity_id = (fun ident -> entid db ident_attr (Keyword ident))
  ; unresolved_lookup_ref_message
  ; value_equal
  ; coerce_tuple_lookup_value = (fun attr value -> coerce_tuple_lookup_value db (visible_datoms db) attr value)
  }

let query_result_entity_id db result =
  Query.query_result_entity_id (query_result_context db) result

let bind_var db name value bindings =
  Query.bind_var (query_result_context db) name value bindings

let match_query_term db term value bindings =
  Query.match_query_term (query_match_context db) term value bindings

let match_pattern_clause db bindings e_term a_term v_term datom =
  Query.match_pattern_clause (query_match_context db) bindings e_term a_term v_term datom

let match_pattern_tx_clause db bindings e_term a_term v_term tx_term datom =
  Query.match_pattern_tx_clause (query_match_context db) bindings e_term a_term v_term tx_term datom

let match_reverse_pattern_clause db bindings e_term reverse_attr v_term datom =
  Query.match_reverse_pattern_clause (query_match_context db) bindings e_term reverse_attr v_term datom

let pattern_datoms db a_term =
  match a_term with
  | QAttr attr when is_reverse_ref attr ->
    datoms db Eavt ~a:(reverse_ref attr) ()
  | _ -> datoms db Eavt ()

let match_data_pattern db bindings e_term a_term v_term datom =
  match a_term with
  | QAttr attr when is_reverse_ref attr ->
    match_reverse_pattern_clause db bindings e_term attr v_term datom
  | _ -> match_pattern_clause db bindings e_term a_term v_term datom

let match_data_pattern_tx db bindings e_term a_term v_term tx_term datom =
  match a_term with
  | QAttr attr when is_reverse_ref attr ->
    let ( let* ) = Option.bind in
    let* bindings = match_reverse_pattern_clause db bindings e_term attr v_term datom in
    match_query_term db tx_term (result_of_datom_tx datom) bindings
  | _ -> match_pattern_tx_clause db bindings e_term a_term v_term tx_term datom

let match_data_pattern_tx_op db bindings e_term a_term v_term tx_term op_term datom =
  let ( let* ) = Option.bind in
  let* bindings = match_data_pattern_tx db bindings e_term a_term v_term tx_term datom in
  match_query_term db op_term (result_of_datom_op datom) bindings

let query_source_context db : Query.source_context =
  { match_context = query_match_context db
  ; pattern_datoms
  ; match_data_pattern
  ; match_data_pattern_tx
  ; match_data_pattern_tx_op
  }

let eval_query_term db bindings term =
  Query.eval_query_term (query_match_context db) bindings term

let collect_query_terms db bindings terms =
  Query.collect_query_terms (query_match_context db) bindings terms

let collect_query_terms_exn db bindings terms =
  Query.collect_query_terms_exn (query_match_context db) bindings terms

let collect_find_vars = Query.collect_find_vars

let query_term_entity_id db bindings term =
  Query.query_term_entity_id (query_match_context db) bindings term

let attr_value_for_query db entity_id attr =
  if is_reverse_ref attr then
    let forward_attr = reverse_ref attr in
    datoms db Eavt ()
    |> List.find_opt (fun d -> d.a = forward_attr && d.v = Ref entity_id)
    |> Option.map (fun d -> Ref d.e)
  else
    datoms db Eavt ~e:entity_id ~a:attr ()
    |> List.find_opt (fun _ -> true)
    |> Option.map (fun d -> d.v)

let attr_present_for_query db entity_id attr =
  Option.is_some (attr_value_for_query db entity_id attr)

let eval_missing_clause clause_db bindings entity_term attr =
  match query_term_entity_id clause_db bindings entity_term with
  | Some entity_id when not (attr_present_for_query clause_db entity_id attr) -> [ bindings ]
  | Some _ | None -> []

let eval_get_else_clause clause_db bindings entity_term attr default output_var =
  if default = Nil then invalid_arg "get-else: nil default value is not supported";
  match query_term_entity_id clause_db bindings entity_term with
  | None -> []
  | Some entity_id ->
    let value = Option.value (attr_value_for_query clause_db entity_id attr) ~default in
    (match bind_var clause_db output_var (Result_value value) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let eval_get_some_clause clause_db bindings entity_term attrs attr_var value_var =
  match query_term_entity_id clause_db bindings entity_term with
  | None -> []
  | Some entity_id ->
    attrs
    |> List.find_map (fun attr ->
      Option.map (fun value -> attr, value) (attr_value_for_query clause_db entity_id attr))
    |> (function
      | None -> []
      | Some (attr, value) ->
        (match bind_var clause_db attr_var (Result_attr attr) bindings with
         | None -> []
         | Some bindings ->
           (match bind_var clause_db value_var (Result_value value) bindings with
            | Some bindings -> [ bindings ]
            | None -> [])))

let eval_ground_tuple db bindings values output_vars =
  if List.length values <> List.length output_vars then
    invalid_arg "ground tuple arity mismatch";
  List.fold_left2
    (fun binding value output_var ->
      match binding, output_var with
      | None, _ -> None
      | Some binding, "_" -> Some binding
      | Some binding, output_var -> bind_var db output_var (Result_value value) binding)
    (Some bindings)
    values
    output_vars
  |> (function
    | Some bindings -> [ bindings ]
    | None -> [])

let eval_ground_result db bindings result output_var =
  match output_var with
  | "_" -> [ bindings ]
  | _ ->
    (match bind_var db output_var result bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let value_of_query_result = function
  | Result_value value -> Some value
  | Result_entity entity_id -> Some (Ref entity_id)
  | Result_attr attr -> Some (Keyword attr)
  | Result_db _ | Result_pull _ -> None

let collect_query_values db bindings terms =
  let ( let* ) = Option.bind in
  let* results = collect_query_terms db bindings terms in
  let rec collect acc = function
    | [] -> Some (List.rev acc)
    | result :: rest ->
      let* value = value_of_query_result result in
      collect (value :: acc) rest
  in
  collect [] results

let value_get = Built_ins.value_get

let bind_get_value db bindings output_var value =
  match bind_var db output_var (result_of_ref (Result_value value)) bindings with
  | Some bindings -> [ bindings ]
  | None -> []

let eval_get_value_clause db bindings map_term key_term output_var =
  match collect_query_terms_exn db bindings [ map_term; key_term ] with
  | [ Result_value collection; key_result ] ->
    (match Option.bind (value_of_query_result key_result) (value_get collection) with
     | None -> []
     | Some value -> bind_get_value db bindings output_var value)
  | _ -> []

let eval_get_default_value_clause db bindings map_term key_term default_term output_var =
  match collect_query_values db bindings [ map_term; key_term; default_term ] with
  | Some [ collection; key; default ] ->
    let value =
      match value_get collection key with
      | Some value -> value
      | None -> default
    in
    bind_get_value db bindings output_var value
  | Some _ | None -> []

let value_count = Built_ins.value_count

let eval_count_value_clause db bindings term output_var =
  match eval_query_term db bindings term with
  | Some (Result_value value) ->
    (match value_count value with
     | None -> []
     | Some count ->
       (match bind_var db output_var (Result_value (Int count)) bindings with
        | Some bindings -> [ bindings ]
        | None -> []))
  | Some (Result_entity _) | Some (Result_attr _) | Some (Result_db _) | Some (Result_pull _) | None -> []

let value_has_count = Built_ins.value_has_count

let value_is_not_empty = Built_ins.value_is_not_empty

let eval_value_predicate_clause db bindings term predicate =
  match eval_query_term db bindings term with
  | Some (Result_value value) when predicate value -> [ bindings ]
  | Some _ | None -> []

let matches_value_predicate = Built_ins.matches_value_predicate

let eval_type_predicate_clause db bindings predicate term =
  eval_value_predicate_clause db bindings term (matches_value_predicate predicate)

let matches_numeric_predicate = Built_ins.matches_numeric_predicate

let eval_numeric_predicate_clause db bindings predicate term =
  eval_value_predicate_clause db bindings term (matches_numeric_predicate predicate)

let matches_comparison_predicate = Built_ins.matches_comparison_predicate

let eval_comparison_predicate_clause db bindings predicate left_term right_term =
  match collect_query_values db bindings [ left_term; right_term ] with
  | Some [ left; right ] when matches_comparison_predicate predicate (compare_value left right) -> [ bindings ]
  | Some _ | None -> []

let comparison_chain_matches = Built_ins.comparison_chain_matches

let eval_comparison_predicate_n_clause db bindings predicate terms =
  match collect_query_values db bindings terms with
  | Some values when comparison_chain_matches predicate values -> [ bindings ]
  | Some _ | None -> []

let all_values_equal = Built_ins.all_values_equal

let eval_equality_predicate_clause db bindings predicate terms =
  match collect_query_values db bindings terms with
  | None -> []
  | Some values ->
    let equal = all_values_equal values in
    let matches =
      match predicate with
      | EqualValues -> equal
      | NotEqualValues -> not equal
    in
    if matches then [ bindings ] else []

let eval_arithmetic = Built_ins.eval_arithmetic

let eval_arithmetic_clause db bindings op terms output_var =
  match collect_query_values db bindings terms with
  | None -> []
  | Some values ->
    (match eval_arithmetic op values with
     | None -> []
     | Some value ->
       (match bind_var db output_var (Result_value value) bindings with
        | Some bindings -> [ bindings ]
        | None -> []))

let normalized_comparison = Built_ins.normalized_comparison

let eval_compare_value_clause db bindings left_term right_term output_var =
  match collect_query_values db bindings [ left_term; right_term ] with
  | Some [ left; right ] ->
    (match bind_var db output_var (Result_value (Int (normalized_comparison (compare_value left right)))) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let extremum_value = Built_ins.extremum_value

let eval_extremum_value_clause db bindings op terms output_var =
  match collect_query_values db bindings terms with
  | None -> []
  | Some [] -> invalid_arg "min/max expects at least one value"
  | Some (first :: rest) ->
    (match bind_var db output_var (Result_value (extremum_value op first rest)) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let matches_boolean_predicate predicate result =
  match predicate, result with
  | TrueValue, Result_value (Bool true) -> true
  | FalseValue, Result_value (Bool false) -> true
  | NilValue, Result_value Nil -> true
  | SomeValue, Result_value Nil -> false
  | SomeValue, (Result_value _ | Result_entity _ | Result_attr _ | Result_db _) -> true
  | _ -> false

let eval_boolean_predicate_clause db bindings predicate term =
  match eval_query_term db bindings term with
  | Some result when matches_boolean_predicate predicate result -> [ bindings ]
  | Some _ | None -> []

let value_is_truthy = Built_ins.value_is_truthy

let query_result_is_truthy = function
  | Result_value value -> value_is_truthy value
  | Result_entity _ | Result_attr _ | Result_db _ | Result_pull _ -> true

let eval_boolean_not_predicate_clause db bindings term =
  match eval_query_term db bindings term with
  | Some result when not (query_result_is_truthy result) -> [ bindings ]
  | Some _ | None -> []

let eval_boolean_not_clause db bindings term output_var =
  match eval_query_term db bindings term with
  | Some result ->
    (match bind_var db output_var (Result_value (Bool (not (query_result_is_truthy result)))) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | None -> []

let eval_identity_value_clause db bindings term output_var =
  match eval_query_term db bindings term with
  | Some result ->
    (match bind_var db output_var result bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | None -> []

let eval_boolean_and_predicate_clause db bindings terms =
  match collect_query_terms db bindings terms with
  | Some results when List.for_all query_result_is_truthy results -> [ bindings ]
  | Some _ | None -> []

let boolean_and_value = Built_ins.boolean_and_value

let eval_boolean_and_clause db bindings terms output_var =
  match collect_query_values db bindings terms with
  | None -> []
  | Some values ->
    (match bind_var db output_var (Result_value (boolean_and_value values)) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let eval_boolean_or_predicate_clause db bindings terms =
  match collect_query_terms db bindings terms with
  | Some results when List.exists query_result_is_truthy results -> [ bindings ]
  | Some _ | None -> []

let boolean_or_value = Built_ins.boolean_or_value

let eval_boolean_or_clause db bindings terms output_var =
  match collect_query_values db bindings terms with
  | None -> []
  | Some values ->
    (match bind_var db output_var (Result_value (boolean_or_value values)) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let eval_random_value_clause db bindings output_var =
  match bind_var db output_var (Result_value (Float (Random.float 1.0))) bindings with
  | Some bindings -> [ bindings ]
  | None -> []

let eval_random_int_value_clause db bindings bound_term output_var =
  match eval_query_term db bindings bound_term with
  | Some (Result_value (Int bound)) when bound > 0 ->
    (match bind_var db output_var (Result_value (Int (Random.int bound))) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_value (Int _)) -> invalid_arg "rand-int bound must be positive"
  | Some _ | None -> []

let split_at = Built_ins.split_at

let values_equal = Built_ins.values_equal

let eval_differ_predicate_clause db bindings terms =
  match collect_query_values db bindings terms with
  | None -> []
  | Some values ->
    let left, right = split_at (List.length values / 2) values in
    if not (List.length left = List.length right && List.for_all2 values_equal left right) then
      [ bindings ]
    else
      []

let eval_identical_predicate_clause db bindings left_term right_term =
  match collect_query_values db bindings [ left_term; right_term ] with
  | Some [ left; right ] when values_equal left right -> [ bindings ]
  | Some _ | None -> []

let type_keyword_of_value = Built_ins.type_keyword_of_value

let eval_type_value_clause db bindings term output_var =
  match eval_query_term db bindings term with
  | Some (Result_value value) ->
    (match bind_var db output_var (Result_value (Keyword (type_keyword_of_value value))) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_entity _) ->
    (match bind_var db output_var (Result_value (Keyword "type/entity")) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_attr _) ->
    (match bind_var db output_var (Result_value (Keyword "type/attr")) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_db _) | Some (Result_pull _) | None -> []

let eval_meta_value_clause db bindings term output_var =
  match eval_query_term db bindings term with
  | Some _ ->
    (match bind_var db output_var (Result_value Nil) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | None -> []

let bind_string_value db output_var value bindings =
  bind_var db output_var (Result_value (String value)) bindings

let bind_keyword_value db output_var value bindings =
  bind_var db output_var (Result_value (Keyword value)) bindings

let eval_name_value_clause db bindings term output_var =
  match eval_query_term db bindings term with
  | Some (Result_value (Keyword keyword)) ->
    let _, name = split_keyword keyword in
    (match bind_string_value db output_var name bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_attr attr) ->
    let _, name = split_keyword attr in
    (match bind_string_value db output_var name bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_value (String value)) ->
    (match bind_string_value db output_var value bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let eval_namespace_value_clause db bindings term output_var =
  match eval_query_term db bindings term with
  | Some (Result_value (Keyword keyword)) ->
    let namespace, _ = split_keyword keyword in
    if namespace = "" then
      []
    else
      (match bind_string_value db output_var namespace bindings with
       | Some bindings -> [ bindings ]
       | None -> [])
  | Some (Result_attr attr) ->
    let namespace, _ = split_keyword attr in
    if namespace = "" then
      []
    else
      (match bind_string_value db output_var namespace bindings with
       | Some bindings -> [ bindings ]
       | None -> [])
  | Some _ | None -> []

let eval_keyword_from_name_clause db bindings term output_var =
  match eval_query_term db bindings term with
  | Some (Result_value (String value)) ->
    (match bind_keyword_value db output_var value bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_value (Keyword keyword)) | Some (Result_attr keyword) ->
    (match bind_keyword_value db output_var keyword bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let eval_keyword_from_namespace_name_clause db bindings namespace_term name_term output_var =
  match collect_query_terms db bindings [ namespace_term; name_term ] with
  | Some [ Result_value (String namespace); Result_value (String name) ] ->
    (match bind_keyword_value db output_var (namespace ^ "/" ^ name) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let string_starts_with = Built_ins.string_starts_with
let string_ends_with = Built_ins.string_ends_with
let string_index_of = Built_ins.string_index_of
let string_includes = Built_ins.string_includes
let string_last_index_of = Built_ins.string_last_index_of

let eval_string_predicate_clause db bindings left_term right_term predicate =
  match collect_query_terms db bindings [ left_term; right_term ] with
  | Some [ Result_value (String left); Result_value (String right) ] when predicate left right -> [ bindings ]
  | Some _ | None -> []

let eval_string_index_clause db bindings value_term needle_term output_var index_of =
  match collect_query_terms db bindings [ value_term; needle_term ] with
  | Some [ Result_value (String value); Result_value (String needle) ] ->
    (match index_of value needle with
     | None -> []
     | Some index ->
       (match bind_var db output_var (Result_value (Int index)) bindings with
        | Some bindings -> [ bindings ]
        | None -> []))
  | Some _ | None -> []

let query_result_int = function
  | Result_value (Int value) -> Some value
  | Result_value _ | Result_entity _ | Result_attr _ | Result_db _ | Result_pull _ -> None

let eval_string_substring_clause db bindings value_term start_term end_term output_var =
  let terms = value_term :: start_term :: Option.to_list end_term in
  match collect_query_terms db bindings terms with
  | Some (Result_value (String value) :: start_result :: rest) ->
    (match query_result_int start_result, rest with
     | Some start_index, [] ->
       if start_index < 0 || start_index > String.length value then
         invalid_arg "substring index out of bounds";
       (match bind_string_value db output_var (String.sub value start_index (String.length value - start_index)) bindings with
        | Some bindings -> [ bindings ]
        | None -> [])
     | Some start_index, [ end_result ] ->
       (match query_result_int end_result with
        | None -> invalid_arg "substring indexes must be integers"
        | Some end_index ->
          if start_index < 0 || end_index < start_index || end_index > String.length value then
            invalid_arg "substring index out of bounds";
          (match bind_string_value db output_var (String.sub value start_index (end_index - start_index)) bindings with
           | Some bindings -> [ bindings ]
           | None -> []))
     | _ -> invalid_arg "substring indexes must be integers")
  | Some _ | None -> []

let string_of_query_value = Built_ins.string_of_query_value
let print_query_values = Built_ins.print_query_values

let eval_print_string_clause db bindings terms output_var ~readably ~newline =
  match collect_query_values db bindings terms with
  | None -> []
  | Some values ->
    let printed = print_query_values ~readably values ^ (if newline then "\n" else "") in
    (match bind_string_value db output_var printed bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let eval_string_build_clause db bindings terms output_var =
  match collect_query_values db bindings terms with
  | None -> []
  | Some values ->
    (match bind_string_value db output_var (values |> List.map string_of_query_value |> String.concat "") bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let collection_string_values = Built_ins.collection_string_values

let eval_string_join_clause db bindings separator_term collection_term output_var =
  match collect_query_terms db bindings [ separator_term; collection_term ] with
  | Some [ Result_value (String separator); Result_value collection ] ->
    (match collection_string_values collection with
     | None -> []
     | Some values ->
       (match bind_string_value db output_var (String.concat separator values) bindings with
        | Some bindings -> [ bindings ]
        | None -> []))
  | Some _ | None -> []

let eval_string_join_plain_clause db bindings collection_term output_var =
  match eval_query_term db bindings collection_term with
  | Some (Result_value collection) ->
    (match collection_string_values collection with
     | None -> []
     | Some values ->
       (match bind_string_value db output_var (String.concat "" values) bindings with
        | Some bindings -> [ bindings ]
        | None -> []))
  | Some _ | None -> []

let replace_string = Built_ins.replace_string
let replace_regex = Built_ins.replace_regex

let eval_string_replace_clause db bindings value_term pattern_term replacement_term output_var first_only =
  match collect_query_terms db bindings [ value_term; pattern_term; replacement_term ] with
  | Some [ Result_value (String value); Result_value (String pattern); Result_value (String replacement) ] ->
    (match bind_string_value db output_var (replace_string ~first_only value pattern replacement) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some [ Result_value (String value); Result_value (Regex pattern); Result_value (String replacement) ] ->
    (match bind_string_value db output_var (replace_regex ~first_only value pattern replacement) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let escape_string = Built_ins.escape_string

let eval_string_escape_clause db bindings value_term replacement_term output_var =
  match collect_query_terms db bindings [ value_term; replacement_term ] with
  | Some [ Result_value (String value); Result_value (Map replacements) ] ->
    (match bind_string_value db output_var (escape_string value replacements) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let regex_pattern_of_result = Built_ins.regex_pattern_of_result
let regex_find = Built_ins.regex_find
let regex_matches = Built_ins.regex_matches
let regex_seq = Built_ins.regex_seq

let eval_re_pattern_value_clause db bindings pattern_term output_var =
  match eval_query_term db bindings pattern_term with
  | Some (Result_value (String pattern)) | Some (Result_value (Regex pattern)) ->
    (match bind_var db output_var (Result_value (Regex pattern)) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let eval_regex_string_clause db bindings pattern_term value_term output_var f =
  match collect_query_terms db bindings [ pattern_term; value_term ] with
  | Some [ pattern_result; Result_value (String value) ] ->
    (match Option.bind (regex_pattern_of_result pattern_result) (fun pattern -> f pattern value) with
     | None -> []
     | Some matched ->
       (match bind_string_value db output_var matched bindings with
        | Some bindings -> [ bindings ]
        | None -> []))
  | Some _ | None -> []

let eval_regex_predicate_clause db bindings pattern_term value_term f =
  match collect_query_terms db bindings [ pattern_term; value_term ] with
  | Some [ pattern_result; Result_value (String value) ] ->
    (match Option.bind (regex_pattern_of_result pattern_result) (fun pattern -> f pattern value) with
     | Some _ -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let eval_re_seq_value_clause db bindings pattern_term value_term output_var =
  match collect_query_terms db bindings [ pattern_term; value_term ] with
  | Some [ pattern_result; Result_value (String value) ] ->
    (match regex_pattern_of_result pattern_result with
     | None -> []
     | Some pattern ->
       (match regex_seq pattern value with
        | [] -> []
        | matches ->
          let values = List.map (fun value -> String value) matches in
          (match bind_var db output_var (Result_value (List values)) bindings with
           | Some bindings -> [ bindings ]
           | None -> [])))
  | Some _ | None -> []

let string_is_blank = Built_ins.string_is_blank

let eval_string_blank_clause db bindings term =
  match eval_query_term db bindings term with
  | Some (Result_value (String value)) when string_is_blank value -> [ bindings ]
  | Some _ | None -> []

let split_string = Built_ins.split_string
let split_string_limited = Built_ins.split_string_limited

let split_regex = Built_ins.split_regex
let split_regex_limited = Built_ins.split_regex_limited

let is_ascii_whitespace = Built_ins.is_ascii_whitespace
let split_lines = Built_ins.split_lines

let bind_string_list db output_var values bindings =
  bind_var db output_var (Result_value (List (List.map (fun value -> String value) values))) bindings

let eval_string_split_clause db bindings value_term separator_term output_var =
  match collect_query_terms db bindings [ value_term; separator_term ] with
  | Some [ Result_value (String value); Result_value (String separator) ] ->
    (match bind_string_list db output_var (split_string value separator) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some [ Result_value (String value); Result_value (Regex pattern) ] ->
    (match bind_string_list db output_var (split_regex value pattern) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let eval_string_split_limit_clause db bindings value_term separator_term limit_term output_var =
  match collect_query_terms db bindings [ value_term; separator_term; limit_term ] with
  | Some [ Result_value (String value); Result_value (String separator); Result_value (Int limit) ] ->
    (match bind_string_list db output_var (split_string_limited value separator limit) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some [ Result_value (String value); Result_value (Regex pattern); Result_value (Int limit) ] ->
    (match bind_string_list db output_var (split_regex_limited value pattern limit) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let eval_string_split_lines_clause db bindings value_term output_var =
  match eval_query_term db bindings value_term with
  | Some (Result_value (String value)) ->
    (match bind_string_list db output_var (split_lines value) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let reverse_string = Built_ins.reverse_string
let capitalize_string = Built_ins.capitalize_string
let trim_left_with = Built_ins.trim_left_with
let trim_right_with = Built_ins.trim_right_with
let trim_with = Built_ins.trim_with
let is_newline = Built_ins.is_newline

let eval_string_transform_clause db bindings term output_var transform =
  match eval_query_term db bindings term with
  | Some (Result_value (String value)) ->
    (match bind_string_value db output_var (transform value) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let value_contains = Built_ins.value_contains

let eval_contains_value_clause db bindings collection_term key_term =
  match collect_query_terms db bindings [ collection_term; key_term ] with
  | Some [ Result_value collection; key_result ] ->
    (match value_of_query_result key_result with
     | Some key when value_contains collection key -> [ bindings ]
     | Some _ | None -> [])
  | Some _ | None -> []

let eval_tuple_function db bindings terms output_var =
  match collect_query_values db bindings terms with
  | None -> []
  | Some values ->
    let tuple = Tuple (List.map (fun value -> Some value) values) in
    (match bind_var db output_var (Result_value tuple) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let eval_collection_value_clause db bindings terms output_var make_value =
  match collect_query_values db bindings terms with
  | None -> []
  | Some values ->
    (match bind_var db output_var (Result_value (make_value values)) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let eval_hash_map_value_clause db bindings terms output_var =
  if List.length terms mod 2 <> 0 then
    invalid_arg "hash-map arity mismatch";
  match collect_query_values db bindings terms with
  | None -> []
  | Some values ->
    let rec pairs acc = function
      | [] -> List.rev acc
      | key :: value :: rest -> pairs ((key, value) :: acc) rest
      | [ _ ] -> invalid_arg "hash-map arity mismatch"
    in
    let map = normalize_value (Map (pairs [] values)) in
    (match bind_var db output_var (Result_value map) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let range_values = Built_ins.range_values

let eval_range_values db bindings output_var start_value end_value step =
  range_values start_value end_value step
  |> List.filter_map (fun value -> bind_var db output_var (Result_value (Int value)) bindings)

let eval_range_end_value_clause db bindings end_term output_var =
  match collect_query_terms db bindings [ end_term ] with
  | None -> []
  | Some [ Result_value (Int end_value) ] -> eval_range_values db bindings output_var 0 end_value 1
  | Some _ -> invalid_arg "range requires integer bounds"

let eval_range_value_clause db bindings start_term end_term output_var =
  match collect_query_terms db bindings [ start_term; end_term ] with
  | None -> []
  | Some [ Result_value (Int start_value); Result_value (Int end_value) ] ->
    eval_range_values db bindings output_var start_value end_value 1
  | Some _ -> invalid_arg "range requires integer bounds"

let eval_range_step_value_clause db bindings start_term end_term step_term output_var =
  match collect_query_terms db bindings [ start_term; end_term; step_term ] with
  | None -> []
  | Some [ Result_value (Int start_value); Result_value (Int end_value); Result_value (Int step) ] ->
    eval_range_values db bindings output_var start_value end_value step
  | Some _ -> invalid_arg "range requires integer bounds"

let eval_untuple_values db bindings output_vars values =
  if List.length values <> List.length output_vars then
    invalid_arg "untuple arity mismatch";
  List.fold_left2
    (fun binding output_var value ->
      match binding, output_var, value with
      | None, _, _ | _, _, None -> None
      | Some binding, "_", Some _ -> Some binding
      | Some binding, output_var, Some value -> bind_var db output_var (result_of_ref (Result_value value)) binding)
    (Some bindings)
    output_vars
    values
  |> (function
    | Some bindings -> [ bindings ]
    | None -> [])

let eval_untuple_function db bindings tuple_term output_vars =
  match eval_query_term db bindings tuple_term with
  | Some (Result_value (Tuple values)) -> eval_untuple_values db bindings output_vars values
  | Some (Result_value (List values | Vector values)) ->
    eval_untuple_values db bindings output_vars (List.map (fun value -> Some value) values)
  | Some _ | None -> []

let source default_db sources name =
  Query.source default_db sources name

let sources_with_root_default db sources =
  Query.sources_with_root_default db sources

let source_db default_db sources name =
  Query.source_db default_db sources name

let query_source_db = Query.query_source_db

let match_query_source_pattern default_db source bindings terms =
  Query.match_query_source_pattern (query_source_context default_db) default_db source bindings terms

let match_source_pattern default_db sources source_name bindings terms =
  Query.match_source_pattern (query_source_context default_db) default_db sources source_name bindings terms

let match_relation_source_pattern default_db sources source_name bindings terms =
  Query.match_relation_source_pattern (query_source_context default_db) default_db sources source_name bindings terms

let pull_pattern_of_result = function
  | Result_value value -> parse_pull_pattern (empty_db ()) (query_form_of_value value)
  | Result_entity _ | Result_attr _ | Result_db _ | Result_pull _ -> invalid_arg "pull pattern input must be a value"

let collect_find_specs db sources bindings find =
  let rec collect acc = function
    | [] -> Some (List.rev acc)
    | Find_var var :: rest ->
      (match List.assoc_opt var bindings with
       | Some value -> collect (value :: acc) rest
       | None -> None)
    | Find_pull (var, selector) :: rest ->
      let pull_db = source_db db sources "$" in
      (match Option.bind (List.assoc_opt var bindings) (query_result_entity_id pull_db) with
       | Some entity_id ->
         (match pull pull_db selector (Entity_id entity_id) with
          | Some entity -> collect (Result_pull entity :: acc) rest
          | None -> None)
       | None -> None)
    | Find_pull_var (var, pattern_var) :: rest ->
      let pull_db = source_db db sources "$" in
      (match
         Option.bind (List.assoc_opt var bindings) (query_result_entity_id pull_db),
         List.assoc_opt pattern_var bindings
       with
       | Some entity_id, Some pattern ->
         (match pull pull_db (pull_pattern_of_result pattern) (Entity_id entity_id) with
          | Some entity -> collect (Result_pull entity :: acc) rest
          | None -> None)
       | _ -> None)
    | Find_pull_source (source, var, selector) :: rest ->
      let pull_db = source_db db sources source in
      (match Option.bind (List.assoc_opt var bindings) (query_result_entity_id pull_db) with
       | Some entity_id ->
         (match pull pull_db selector (Entity_id entity_id) with
          | Some entity -> collect (Result_pull entity :: acc) rest
          | None -> None)
       | None -> None)
    | Find_pull_source_var (source, var, pattern_var) :: rest ->
      let pull_db = source_db db sources source in
      (match
         Option.bind (List.assoc_opt var bindings) (query_result_entity_id pull_db),
         List.assoc_opt pattern_var bindings
       with
       | Some entity_id, Some pattern ->
         (match pull pull_db (pull_pattern_of_result pattern) (Entity_id entity_id) with
          | Some entity -> collect (Result_pull entity :: acc) rest
          | None -> None)
       | _ -> None)
    | Find_aggregate _ :: rest -> collect acc rest
  in
  collect [] find

let has_aggregates = Query.has_aggregates

let aggregate_result = Built_ins.aggregate_result

let resolve_dynamic_aggregate = Query.resolve_dynamic_aggregate

let aggregate_param_vars = Query.aggregate_param_vars

let query_term_vars = Query.query_term_vars

let aggregate_extra_args db sources group_bindings terms =
  Query.aggregate_extra_args (query_match_context db) db sources group_bindings terms

let aggregate_values db sources group_bindings terms =
  Query.aggregate_values (query_match_context db) db sources group_bindings terms

let aggregate_input_values = Query.aggregate_input_values

let empty_query_callables = Query.empty_query_callables

let callable_predicate = Query.callable_predicate

let callable_function = Query.callable_function

let resolve_callable_aggregate = Query.resolve_callable_aggregate

let group_by_key = Query.group_by_key

let grouping_vars_of_find = Query.grouping_vars_of_find

let aggregate_rows ?(callables = empty_query_callables) db sources bindings find =
  let group_vars = grouping_vars_of_find find in
  bindings
  |> List.filter_map (fun binding ->
    collect_find_vars binding group_vars
    |> Option.map (fun key -> key, binding))
  |> group_by_key
  |> List.filter_map (fun (key, group_bindings) ->
    let group_binding = List.combine group_vars key in
    let rec build_row acc = function
      | [] -> Some (List.rev acc)
      | Find_var var :: rest ->
        (match List.assoc_opt var group_binding with
         | Some value -> build_row (value :: acc) rest
         | None -> None)
      | Find_pull (var, selector) :: rest ->
        let pull_db = source_db db sources "$" in
        (match Option.bind (List.assoc_opt var group_binding) (query_result_entity_id pull_db) with
         | Some entity_id ->
           (match pull pull_db selector (Entity_id entity_id) with
            | Some entity -> build_row (Result_pull entity :: acc) rest
            | None -> None)
         | None -> None)
      | Find_pull_var (var, pattern_var) :: rest ->
        let pull_db = source_db db sources "$" in
        (match
           Option.bind (List.assoc_opt var group_binding) (query_result_entity_id pull_db),
           List.assoc_opt pattern_var group_binding
         with
         | Some entity_id, Some pattern ->
           (match pull pull_db (pull_pattern_of_result pattern) (Entity_id entity_id) with
            | Some entity -> build_row (Result_pull entity :: acc) rest
            | None -> None)
         | _ -> None)
      | Find_pull_source (source, var, selector) :: rest ->
        let pull_db = source_db db sources source in
        (match Option.bind (List.assoc_opt var group_binding) (query_result_entity_id pull_db) with
         | Some entity_id ->
           (match pull pull_db selector (Entity_id entity_id) with
            | Some entity -> build_row (Result_pull entity :: acc) rest
            | None -> None)
         | None -> None)
      | Find_pull_source_var (source, var, pattern_var) :: rest ->
        let pull_db = source_db db sources source in
        (match
           Option.bind (List.assoc_opt var group_binding) (query_result_entity_id pull_db),
           List.assoc_opt pattern_var group_binding
         with
         | Some entity_id, Some pattern ->
           (match pull pull_db (pull_pattern_of_result pattern) (Entity_id entity_id) with
            | Some entity -> build_row (Result_pull entity :: acc) rest
            | None -> None)
         | _ -> None)
      | Find_aggregate (aggregate, terms) :: rest ->
        let values = aggregate_values db sources group_bindings terms in
        let aggregate =
          resolve_dynamic_aggregate aggregate group_bindings
          |> resolve_callable_aggregate callables
        in
        let values = aggregate_input_values aggregate (aggregate_extra_args db sources group_bindings terms) values in
        build_row (aggregate_result aggregate values :: acc) rest
    in
    build_row [] find)
  |> List.sort_uniq compare

let aggregate_rows_with ?(callables = empty_query_callables) db sources bindings find with_vars =
  let group_vars = grouping_vars_of_find find in
  let aggregate_vars =
    List.concat_map
      (function
        | Find_aggregate (aggregate, terms) ->
          query_term_vars terms @ aggregate_param_vars aggregate
        | Find_var _ | Find_pull _ | Find_pull_var _ | Find_pull_source _ | Find_pull_source_var _ -> [])
      find
  in
  let dedupe_vars = group_vars @ aggregate_vars @ with_vars |> List.sort_uniq compare in
  let bindings =
    bindings
    |> List.filter_map (fun binding ->
      collect_find_vars binding dedupe_vars
      |> Option.map (fun key -> key, binding))
    |> List.sort_uniq (fun (left, _) (right, _) -> compare left right)
    |> List.map snd
  in
  aggregate_rows ~callables db sources bindings find

let collect_query_row_with_vars db sources find with_vars binding =
  match collect_find_specs db sources binding find, collect_find_vars binding with_vars with
  | Some row, Some with_values -> Some (row, with_values)
  | _ -> None

let non_aggregate_rows_with db sources bindings find with_vars =
  bindings
  |> List.filter_map (collect_query_row_with_vars db sources find with_vars)
  |> List.sort_uniq compare
  |> List.map fst

let rule_invocation_binding db outer_binding rule terms =
  if List.length rule.rule_params <> List.length terms then
    invalid_arg ("rule arity mismatch: " ^ rule.rule_name);
  List.fold_left2
    (fun rule_binding param term ->
      match rule_binding with
      | None -> None
      | Some rule_binding ->
        (match eval_query_term db outer_binding term with
         | Some value -> bind_var db param value rule_binding
         | None -> Some rule_binding))
    (Some [])
    rule.rule_params
    terms

let propagate_rule_binding db outer_binding rule_binding rule terms =
  List.fold_left2
    (fun outer_binding param term ->
      match outer_binding, term with
      | None, _ -> None
      | Some outer_binding, QVar var ->
        (match List.assoc_opt param rule_binding with
         | Some value -> bind_var db var value outer_binding
         | None -> Some outer_binding)
      | Some outer_binding, QWildcard -> Some outer_binding
      | Some outer_binding, _ -> Some outer_binding)
    (Some outer_binding)
    rule.rule_params
    terms

let rule_invocation_callables = Query.rule_invocation_callables

let resolve_query_input_result db = function
  | Result_value value ->
    Option.map (fun _ -> Result_value value) (resolve_query_value db value)
  | result -> Some result

let query_input_context db : Query.input_context =
  { resolve_query_input_result = resolve_query_input_result db
  ; bind_var = (fun var value bindings -> bind_var db var value bindings)
  ; entity_id_of_ref = entity_id_of_ref db
  }

let bind_relation_row db bindings vars row =
  Query.bind_relation_row (query_input_context db) bindings vars row

let collection_values_of_input db value =
  Query.collection_values_of_input (query_input_context db) value

let row_values_of_input db value =
  Query.row_values_of_input (query_input_context db) value

let eval_ground_term_tuple db bindings result output_vars =
  Query.eval_ground_term_tuple (query_input_context db) bindings result output_vars

let eval_ground_term_relation db bindings result output_vars =
  Query.eval_ground_term_relation (query_input_context db) bindings result output_vars

let apply_query_input db bindings input =
  Query.apply_query_input (query_input_context db) bindings input

let query_input_decl_binding_string = Query.query_input_decl_binding_string

let query_result_input_string = function
  | Result_value value -> edn_string_of_value value
  | Result_entity entity_id -> string_of_int entity_id
  | Result_attr attr -> ":" ^ attr
  | Result_db _ -> "<db>"
  | Result_pull _ -> "<pull>"

let query_result_collection_string values =
  "[" ^ String.concat " " (List.map query_result_input_string values) ^ "]"

let query_input_of_arg decl arg =
  let values_of_collection_result = Query.values_of_collection_result in
  let row_of_collection_value = Query.row_of_collection_result in
  let row_of_scalar_sequence = Query.row_of_scalar_sequence in
  let cannot_bind_value_to kind value =
    invalid_arg
      ( "Cannot bind value "
      ^ query_result_input_string value
      ^ " to "
      ^ kind
      ^ " "
      ^ query_input_decl_binding_string decl )
  in
  let row_for_tuple_binding vars value =
    match values_of_collection_result value with
    | None -> cannot_bind_value_to "tuple" value
    | Some row ->
      if List.length row < List.length vars then
        invalid_arg
          ( "Not enough elements in a collection "
          ^ query_result_collection_string row
          ^ " to bind tuple "
          ^ query_input_decl_binding_string decl )
      else if List.length row > List.length vars then
        invalid_arg
          ( "Too many elements in a collection "
          ^ query_result_collection_string row
          ^ " to bind tuple "
          ^ query_input_decl_binding_string decl )
      else
        row
  in
  let rows_of_map = Query.rows_of_map_entries in
  match decl, arg with
  | Input_ignore_decl, _ -> Input_ignore
  | Input_scalar_decl var, Arg_scalar value -> Input_scalar (var, value)
  | Input_scalar_decl var, Arg_entity_ref entity_ref -> Input_entity_ref (var, entity_ref)
  | Input_collection_decl var, Arg_collection values -> Input_collection (var, values)
  | Input_collection_decl var, Arg_scalar value ->
    (match values_of_collection_result value with
     | Some values -> Input_collection (var, values)
     | None -> cannot_bind_value_to "collection" value)
  | Input_collection_ignore_decl, Arg_collection values -> Input_collection_ignore values
  | Input_collection_ignore_decl, Arg_scalar value ->
    (match values_of_collection_result value with
     | Some values -> Input_collection_ignore values
     | None -> invalid_arg "query input argument does not match :in binding")
  | Input_nested_collection_decl binding, Arg_collection values ->
    Input_nested_collection (binding, values)
  | Input_nested_collection_decl binding, Arg_scalar value ->
    (match values_of_collection_result value with
     | Some values -> Input_nested_collection (binding, values)
     | None -> invalid_arg "query input argument does not match :in binding")
  | Input_tuple_decl vars, Arg_tuple row -> Input_tuple (vars, row)
  | Input_tuple_decl vars, Arg_scalar value -> Input_tuple (vars, row_for_tuple_binding vars value)
  | Input_relation_decl vars, Arg_relation rows -> Input_relation (vars, rows)
  | Input_relation_decl vars, Arg_collection rows ->
    Input_relation (vars, List.map row_of_collection_value rows)
  | Input_relation_decl vars, Arg_scalar (Result_value (Map entries)) ->
    Input_relation (vars, rows_of_map entries)
  | Input_relation_decl vars, Arg_scalar value ->
    (match values_of_collection_result value with
     | Some rows -> Input_relation (vars, List.map row_of_collection_value rows)
     | None -> invalid_arg "query input argument does not match :in binding")
  | Input_nested_tuple_decl bindings, Arg_tuple row -> Input_nested_tuple (bindings, row)
  | Input_nested_tuple_decl bindings, Arg_scalar value ->
    Input_nested_tuple (bindings, row_of_scalar_sequence value)
  | Input_nested_relation_decl bindings, Arg_relation rows -> Input_nested_relation (bindings, rows)
  | Input_nested_relation_decl bindings, Arg_collection rows ->
    Input_nested_relation (bindings, List.map row_of_collection_value rows)
  | Input_nested_relation_decl bindings, Arg_scalar (Result_value (Map entries)) ->
    Input_nested_relation (bindings, rows_of_map entries)
  | Input_nested_relation_decl bindings, Arg_scalar value ->
    (match values_of_collection_result value with
     | Some rows -> Input_nested_relation (bindings, List.map row_of_collection_value rows)
     | None -> invalid_arg "query input argument does not match :in binding")
  | Input_scalar_decl var, Arg_predicate predicate -> Input_predicate (var, predicate)
  | Input_scalar_decl var, Arg_function f -> Input_function (var, f)
  | Input_scalar_decl var, Arg_aggregate f -> Input_aggregate (var, f)
  | Input_rules_decl, Arg_rules rules -> Input_rules rules
  | Input_scalar_decl _, _
  | Input_collection_decl _, _
  | Input_collection_ignore_decl, _
  | Input_nested_collection_decl _, _
  | Input_tuple_decl _, _
  | Input_relation_decl _, _
  | Input_nested_tuple_decl _, _
  | Input_nested_relation_decl _, _ ->
    invalid_arg "query input argument does not match :in binding"
  | (Input_scalar _
    | Input_entity_ref _
    | Input_collection _
    | Input_collection_ignore _
    | Input_nested_collection _
    | Input_tuple _
    | Input_nested_tuple _
    | Input_nested_relation _
    | Input_predicate _
    | Input_function _
    | Input_aggregate _
    | Input_rules _
    | Input_relation _
    | Input_ignore
    | Input_source_decl _
    | Input_rules_decl), _ ->
    invalid_arg "bound query inputs do not consume supplied arguments"

let bind_query_inputs ~consume_rules declarations args =
  Query.bind_query_inputs ~query_input_of_arg ~consume_rules declarations args

let query_callables_of_inputs = Query.query_callables_of_inputs

let query_rules_of_inputs = Query.query_rules_of_inputs

let initial_query_context db query input_args =
  let inputs = bind_query_inputs ~consume_rules:(query.rules = []) query.inputs input_args in
  ( query_callables_of_inputs inputs
  , List.fold_left (apply_query_input db) [ [] ] inputs
  , query_rules_of_inputs inputs )

let project_binding = Query.project_binding

let merge_projected_binding db vars outer_binding inner_binding =
  vars
  |> List.fold_left
       (fun binding var ->
         match binding with
         | None -> None
         | Some binding ->
           (match List.assoc_opt var inner_binding with
            | Some value -> bind_var db var value binding
            | None -> Some binding))
       (Some outer_binding)

let rec eval_clauses
    ?(active_rules = [])
    ?(callables = empty_query_callables)
    ?default_source
    db
    sources
    rules
    bindings
    clauses =
  let default_source = Option.value default_source ~default:(source db sources "$") in
  List.fold_left
    (fun bindings clause ->
      List.concat_map
        (fun binding ->
           eval_clause ~active_rules ~callables ~default_source db sources rules binding clause)
        bindings)
    bindings
    clauses

and query_clause_string clause =
  Query.query_clause_string ~value_to_string:edn_string_of_value clause

and query_or_join_clause_string required_vars vars branches =
  Query.query_or_join_clause_string ~value_to_string:edn_string_of_value required_vars vars branches

and ensure_query_terms_bound bindings terms clause_string =
  Query.ensure_query_terms_bound bindings terms clause_string

and ensure_not_has_outer_binding bindings clauses =
  Query.ensure_not_has_outer_binding ~value_to_string:edn_string_of_value bindings clauses

and ensure_or_branch_vars_match bindings branches =
  Query.ensure_or_branch_vars_match ~value_to_string:edn_string_of_value bindings branches

and ensure_join_vars_bound bindings vars =
  Query.ensure_join_vars_bound bindings vars

and ensure_join_vars_bound_in_clause bindings vars clause_string =
  Query.ensure_join_vars_bound_in_clause bindings vars clause_string

and ensure_or_join_branches_cover_listed_vars bindings vars branches =
  Query.ensure_or_join_branches_cover_listed_vars bindings vars branches

and rule_call_key db source name bindings terms =
  source, name, List.map (eval_query_term db bindings) terms

and matching_rules_for_call active_rules key rules name arity =
  Query.matching_rules_for_call active_rules key rules name arity

and collect_dynamic_query_terms_exn db sources bindings terms =
  Query.collect_dynamic_query_terms_exn (query_match_context db) db sources bindings terms

and eval_dynamic_predicate_clause callables db sources bindings name terms =
  match callable_predicate callables name with
  | Some predicate ->
    if predicate (collect_dynamic_query_terms_exn db sources bindings terms) then [ bindings ] else []
  | None ->
    invalid_arg
      ("Unknown predicate '" ^ name ^ " in " ^ query_clause_string (DynamicPredicate (name, terms)))

and eval_dynamic_function_clause callables db sources bindings name terms output_vars =
  match callable_function callables name with
  | Some f ->
    (match f (collect_dynamic_query_terms_exn db sources bindings terms) with
     | Some outputs ->
       (match bind_relation_row db bindings output_vars outputs with
        | Some bindings -> [ bindings ]
        | None -> [])
     | None -> [])
  | None ->
    invalid_arg
      ("Unknown function '" ^ name ^ " in " ^ query_clause_string (DynamicFunction (name, terms, output_vars)))

and eval_dynamic_function_collection_clause callables db sources bindings name terms output_var =
  match callable_function callables name with
  | Some f ->
    (match f (collect_dynamic_query_terms_exn db sources bindings terms) with
     | Some [ result ] ->
       (match collection_values_of_input db result with
        | Some values ->
          values
          |> List.filter_map (fun value ->
            match bind_var db output_var value bindings with
            | Some bindings -> Some bindings
            | None -> None)
        | None -> [])
     | Some _ -> invalid_arg "dynamic collection function output must return one collection"
     | None -> [])
  | None ->
    invalid_arg
      ( "Unknown function '"
      ^ name
      ^ " in "
      ^ query_clause_string (DynamicFunctionCollection (name, terms, output_var)) )

and eval_dynamic_function_relation_clause callables db sources bindings name terms output_vars =
  match callable_function callables name with
  | Some f ->
    (match f (collect_dynamic_query_terms_exn db sources bindings terms) with
     | Some [ result ] ->
       (match collection_values_of_input db result with
        | Some values ->
          values
          |> List.filter_map (fun value ->
            match row_values_of_input db value with
            | Some row -> bind_relation_row db bindings output_vars row
            | None -> None)
        | None -> [])
     | Some _ -> invalid_arg "dynamic relation function output must return one collection"
     | None -> [])
  | None ->
    invalid_arg
      ( "Unknown function '"
      ^ name
      ^ " in "
      ^ query_clause_string (DynamicFunctionRelation (name, terms, output_vars)) )

and eval_clause
    ?(active_rules = [])
    ?(callables = empty_query_callables)
    ?default_source
    db
    sources
    rules
    bindings =
  let default_source = Option.value default_source ~default:(source db sources "$") in
  function
  | Pattern (e_term, a_term, v_term) ->
    match_query_source_pattern db default_source bindings [ e_term; a_term; v_term ]
  | PatternTx (e_term, a_term, v_term, tx_term) ->
    match_query_source_pattern db default_source bindings [ e_term; a_term; v_term; tx_term ]
  | PatternTxOp (e_term, a_term, v_term, tx_term, op_term) ->
    match_query_source_pattern db default_source bindings [ e_term; a_term; v_term; tx_term; op_term ]
  | SourcePattern (source, e_term, a_term, v_term) ->
    match_source_pattern db sources source bindings [ e_term; a_term; v_term ]
  | SourcePatternTx (source, e_term, a_term, v_term, tx_term) ->
    match_source_pattern db sources source bindings [ e_term; a_term; v_term; tx_term ]
  | SourcePatternTxOp (source, e_term, a_term, v_term, tx_term, op_term) ->
    match_source_pattern db sources source bindings [ e_term; a_term; v_term; tx_term; op_term ]
  | SourceRelationPattern (source, terms) ->
    match_relation_source_pattern db sources source bindings terms
  | Missing (entity_term, attr) ->
    eval_missing_clause (query_source_db default_source) bindings entity_term attr
  | SourceMissing (source, entity_term, attr) ->
    eval_missing_clause (source_db db sources source) bindings entity_term attr
  | GetElse (entity_term, attr, default, output_var) ->
    eval_get_else_clause (query_source_db default_source) bindings entity_term attr default output_var
  | SourceGetElse (source, entity_term, attr, default, output_var) ->
    eval_get_else_clause (source_db db sources source) bindings entity_term attr default output_var
  | GetSome (entity_term, attrs, attr_var, value_var) ->
    eval_get_some_clause (query_source_db default_source) bindings entity_term attrs attr_var value_var
  | SourceGetSome (source, entity_term, attrs, attr_var, value_var) ->
    eval_get_some_clause (source_db db sources source) bindings entity_term attrs attr_var value_var
  | GetValue (map_term, key_term, output_var) ->
    eval_get_value_clause db bindings map_term key_term output_var
  | GetDefaultValue (map_term, key_term, default_term, output_var) ->
    eval_get_default_value_clause db bindings map_term key_term default_term output_var
  | CountValue (term, output_var) ->
    eval_count_value_clause db bindings term output_var
  | EmptyValue term ->
    eval_value_predicate_clause db bindings term (value_has_count 0)
  | NotEmptyValue term ->
    eval_value_predicate_clause db bindings term value_is_not_empty
  | ContainsValue (collection_term, key_term) ->
    eval_contains_value_clause db bindings collection_term key_term
  | ValuePredicate (predicate, term) ->
    eval_type_predicate_clause db bindings predicate term
  | NumericPredicate (predicate, term) ->
    ensure_query_terms_bound bindings [ term ] (query_clause_string (NumericPredicate (predicate, term)));
    eval_numeric_predicate_clause db bindings predicate term
  | ComparisonPredicate (predicate, left_term, right_term) ->
    eval_comparison_predicate_clause db bindings predicate left_term right_term
  | ComparisonPredicateN (predicate, terms) ->
    eval_comparison_predicate_n_clause db bindings predicate terms
  | EqualityPredicate (predicate, terms) ->
    eval_equality_predicate_clause db bindings predicate terms
  | ArithmeticValue (op, terms, output_var) ->
    ensure_query_terms_bound bindings terms (query_clause_string (ArithmeticValue (op, terms, output_var)));
    eval_arithmetic_clause db bindings op terms output_var
  | CompareValue (left_term, right_term, output_var) ->
    eval_compare_value_clause db bindings left_term right_term output_var
  | ExtremumValue (op, terms, output_var) ->
    eval_extremum_value_clause db bindings op terms output_var
  | BooleanPredicate (predicate, term) ->
    eval_boolean_predicate_clause db bindings predicate term
  | BooleanNotPredicate term ->
    eval_boolean_not_predicate_clause db bindings term
  | BooleanNotValue (term, output_var) ->
    eval_boolean_not_clause db bindings term output_var
  | IdentityValue (term, output_var) ->
    eval_identity_value_clause db bindings term output_var
  | BooleanAndPredicate terms ->
    eval_boolean_and_predicate_clause db bindings terms
  | BooleanAndValue (terms, output_var) ->
    eval_boolean_and_clause db bindings terms output_var
  | BooleanOrPredicate terms ->
    eval_boolean_or_predicate_clause db bindings terms
  | BooleanOrValue (terms, output_var) ->
    eval_boolean_or_clause db bindings terms output_var
  | RandomValue output_var ->
    eval_random_value_clause db bindings output_var
  | RandomIntValue (bound_term, output_var) ->
    eval_random_int_value_clause db bindings bound_term output_var
  | DifferPredicate terms ->
    eval_differ_predicate_clause db bindings terms
  | IdenticalPredicate (left_term, right_term) ->
    eval_identical_predicate_clause db bindings left_term right_term
  | TypeValue (term, output_var) ->
    eval_type_value_clause db bindings term output_var
  | MetaValue (term, output_var) ->
    eval_meta_value_clause db bindings term output_var
  | NameValue (term, output_var) ->
    eval_name_value_clause db bindings term output_var
  | NamespaceValue (term, output_var) ->
    eval_namespace_value_clause db bindings term output_var
  | KeywordFromName (term, output_var) ->
    eval_keyword_from_name_clause db bindings term output_var
  | KeywordFromNamespaceName (namespace_term, name_term, output_var) ->
    eval_keyword_from_namespace_name_clause db bindings namespace_term name_term output_var
  | StringIncludesValue (left_term, right_term) ->
    eval_string_predicate_clause db bindings left_term right_term string_includes
  | StringStartsWithValue (left_term, right_term) ->
    eval_string_predicate_clause db bindings left_term right_term string_starts_with
  | StringEndsWithValue (left_term, right_term) ->
    eval_string_predicate_clause db bindings left_term right_term string_ends_with
  | StringLowerCaseValue (term, output_var) ->
    eval_string_transform_clause db bindings term output_var String.lowercase_ascii
  | StringUpperCaseValue (term, output_var) ->
    eval_string_transform_clause db bindings term output_var String.uppercase_ascii
  | StringCapitalizeValue (term, output_var) ->
    eval_string_transform_clause db bindings term output_var capitalize_string
  | StringReverseValue (term, output_var) ->
    eval_string_transform_clause db bindings term output_var reverse_string
  | StringTrimValue (term, output_var) ->
    eval_string_transform_clause db bindings term output_var (trim_with is_ascii_whitespace)
  | StringTrimLeftValue (term, output_var) ->
    eval_string_transform_clause db bindings term output_var (trim_left_with is_ascii_whitespace)
  | StringTrimRightValue (term, output_var) ->
    eval_string_transform_clause db bindings term output_var (trim_right_with is_ascii_whitespace)
  | StringTrimNewlineValue (term, output_var) ->
    eval_string_transform_clause db bindings term output_var (trim_right_with is_newline)
  | StringIndexOfValue (value_term, needle_term, output_var) ->
    eval_string_index_clause db bindings value_term needle_term output_var string_index_of
  | StringLastIndexOfValue (value_term, needle_term, output_var) ->
    eval_string_index_clause db bindings value_term needle_term output_var string_last_index_of
  | StringSubstringValue (value_term, start_term, end_term, output_var) ->
    eval_string_substring_clause db bindings value_term start_term end_term output_var
  | StringBuildValue (terms, output_var) ->
    eval_string_build_clause db bindings terms output_var
  | PrintStringValue (terms, output_var) ->
    eval_print_string_clause db bindings terms output_var ~readably:false ~newline:false
  | PrintLineStringValue (terms, output_var) ->
    eval_print_string_clause db bindings terms output_var ~readably:false ~newline:true
  | PrStringValue (terms, output_var) ->
    eval_print_string_clause db bindings terms output_var ~readably:true ~newline:false
  | PrnStringValue (terms, output_var) ->
    eval_print_string_clause db bindings terms output_var ~readably:true ~newline:true
  | StringJoinPlainValue (collection_term, output_var) ->
    eval_string_join_plain_clause db bindings collection_term output_var
  | StringJoinValue (separator_term, collection_term, output_var) ->
    eval_string_join_clause db bindings separator_term collection_term output_var
  | StringReplaceValue (value_term, pattern_term, replacement_term, output_var) ->
    eval_string_replace_clause db bindings value_term pattern_term replacement_term output_var false
  | StringReplaceFirstValue (value_term, pattern_term, replacement_term, output_var) ->
    eval_string_replace_clause db bindings value_term pattern_term replacement_term output_var true
  | StringEscapeValue (value_term, replacement_term, output_var) ->
    eval_string_escape_clause db bindings value_term replacement_term output_var
  | RePatternValue (pattern_term, output_var) ->
    eval_re_pattern_value_clause db bindings pattern_term output_var
  | ReFindValue (pattern_term, value_term, output_var) ->
    eval_regex_string_clause db bindings pattern_term value_term output_var regex_find
  | ReMatchesValue (pattern_term, value_term, output_var) ->
    eval_regex_string_clause db bindings pattern_term value_term output_var regex_matches
  | ReSeqValue (pattern_term, value_term, output_var) ->
    eval_re_seq_value_clause db bindings pattern_term value_term output_var
  | ReFindPredicate (pattern_term, value_term) ->
    eval_regex_predicate_clause db bindings pattern_term value_term regex_find
  | ReMatchesPredicate (pattern_term, value_term) ->
    eval_regex_predicate_clause db bindings pattern_term value_term regex_matches
  | StringBlankValue term ->
    eval_string_blank_clause db bindings term
  | StringSplitValue (value_term, separator_term, output_var) ->
    eval_string_split_clause db bindings value_term separator_term output_var
  | StringSplitLimitValue (value_term, separator_term, limit_term, output_var) ->
    eval_string_split_limit_clause db bindings value_term separator_term limit_term output_var
  | StringSplitLinesValue (value_term, output_var) ->
    eval_string_split_lines_clause db bindings value_term output_var
  | Ground (value, output_var) ->
    eval_ground_result db bindings (Result_value value) output_var
  | GroundCollection (values, output_var) ->
    values
    |> List.concat_map (fun value -> eval_ground_result db bindings (Result_value value) output_var)
  | GroundTuple (values, output_vars) ->
    eval_ground_tuple db bindings values output_vars
  | GroundRelation (rows, output_vars) ->
    rows |> List.concat_map (fun values -> eval_ground_tuple db bindings values output_vars)
  | GroundTerm (term, output_var) ->
    (match eval_query_term db bindings term with
     | Some result -> eval_ground_result db bindings result output_var
     | None -> [])
  | GroundTermCollection (term, output_var) ->
    (match eval_query_term db bindings term with
     | Some result ->
       (match collection_values_of_input db result with
        | Some values -> values |> List.concat_map (fun value -> eval_ground_result db bindings value output_var)
        | None -> [])
     | None -> [])
  | GroundTermTuple (term, output_vars) ->
    (match eval_query_term db bindings term with
     | Some result -> eval_ground_term_tuple db bindings result output_vars
     | None -> [])
  | GroundTermRelation (term, output_vars) ->
    (match eval_query_term db bindings term with
     | Some result -> eval_ground_term_relation db bindings result output_vars
     | None -> [])
  | VectorValue (terms, output_var) ->
    eval_collection_value_clause db bindings terms output_var (fun values -> Vector values)
  | ListValue (terms, output_var) ->
    eval_collection_value_clause db bindings terms output_var (fun values -> List values)
  | SetValue (terms, output_var) ->
    eval_collection_value_clause db bindings terms output_var (fun values -> normalize_value (Set values))
  | HashMapValue (terms, output_var) ->
    eval_hash_map_value_clause db bindings terms output_var
  | ArrayMapValue (terms, output_var) ->
    eval_hash_map_value_clause db bindings terms output_var
  | RangeEndValue (end_term, output_var) ->
    eval_range_end_value_clause db bindings end_term output_var
  | RangeValue (start_term, end_term, output_var) ->
    eval_range_value_clause db bindings start_term end_term output_var
  | RangeStepValue (start_term, end_term, step_term, output_var) ->
    eval_range_step_value_clause db bindings start_term end_term step_term output_var
  | TupleFunction (terms, output_var) ->
    eval_tuple_function db bindings terms output_var
  | UntupleFunction (tuple_term, output_vars) ->
    eval_untuple_function db bindings tuple_term output_vars
  | Predicate (_name, terms, predicate) ->
    if predicate (collect_query_terms_exn db bindings terms) then [ bindings ] else []
  | Function (_name, terms, output_vars, f) ->
    (match f (collect_query_terms_exn db bindings terms) with
     | Some outputs ->
       (match bind_relation_row db bindings output_vars outputs with
        | Some bindings -> [ bindings ]
        | None -> [])
     | None -> [])
  | DynamicPredicate (name, terms) ->
    eval_dynamic_predicate_clause callables db sources bindings name terms
  | DynamicFunction (name, terms, output_vars) ->
    eval_dynamic_function_clause callables db sources bindings name terms output_vars
  | DynamicFunctionCollection (name, terms, output_var) ->
    eval_dynamic_function_collection_clause callables db sources bindings name terms output_var
  | DynamicFunctionRelation (name, terms, output_binding) ->
    eval_dynamic_function_relation_clause callables db sources bindings name terms output_binding
  | SourceClause (source_name, clause) ->
    let clause_db = source_db db sources source_name in
    eval_clause
      ~active_rules
      ~callables
      ~default_source:(Db_source clause_db)
      clause_db
      sources
      rules
      bindings
      clause
  | Not clauses ->
    ensure_not_has_outer_binding bindings clauses;
    (match eval_clauses ~active_rules ~callables ~default_source db sources rules [ bindings ] clauses with
     | [] -> [ bindings ]
     | _ -> [])
  | SourceNot (source, clauses) ->
    let clause_db = source_db db sources source in
    let sources = sources_with_root_default db sources in
    ensure_not_has_outer_binding bindings clauses;
    (match
       eval_clauses
         ~active_rules
         ~callables
         ~default_source:(Db_source clause_db)
         clause_db
         sources
         rules
         [ bindings ]
         clauses
     with
     | [] -> [ bindings ]
     | _ -> [])
  | NotJoin (vars, clauses) ->
    ensure_join_vars_bound bindings vars;
    let projected_binding = project_binding vars bindings in
    (match eval_clauses ~active_rules ~callables ~default_source db sources rules [ projected_binding ] clauses with
     | [] -> [ bindings ]
     | _ -> [])
  | SourceNotJoin (source, vars, clauses) ->
    let clause_db = source_db db sources source in
    let sources = sources_with_root_default db sources in
    ensure_join_vars_bound bindings vars;
    let projected_binding = project_binding vars bindings in
    (match
       eval_clauses
         ~active_rules
         ~callables
         ~default_source:(Db_source clause_db)
         clause_db
         sources
         rules
         [ projected_binding ]
         clauses
     with
     | [] -> [ bindings ]
     | _ -> [])
  | Or branches ->
    ensure_or_branch_vars_match bindings branches;
    List.concat_map
      (fun clauses -> eval_clauses ~active_rules ~callables ~default_source db sources rules [ bindings ] clauses)
      branches
  | SourceOr (source, branches) ->
    let clause_db = source_db db sources source in
    let sources = sources_with_root_default db sources in
    ensure_or_branch_vars_match bindings branches;
    List.concat_map
      (fun clauses ->
         eval_clauses
           ~active_rules
           ~callables
           ~default_source:(Db_source clause_db)
           clause_db
           sources
           rules
           [ bindings ]
           clauses)
      branches
  | OrJoin (vars, branches) ->
    ensure_or_join_branches_cover_listed_vars bindings vars branches;
    let projected_binding = project_binding vars bindings in
    branches
    |> List.concat_map
         (fun clauses ->
            eval_clauses ~active_rules ~callables ~default_source db sources rules [ projected_binding ] clauses)
    |> List.filter_map (merge_projected_binding db vars bindings)
  | SourceOrJoin (source, vars, branches) ->
    let clause_db = source_db db sources source in
    let sources = sources_with_root_default db sources in
    ensure_or_join_branches_cover_listed_vars bindings vars branches;
    let projected_binding = project_binding vars bindings in
    branches
    |> List.concat_map
         (fun clauses ->
            eval_clauses
              ~active_rules
              ~callables
              ~default_source:(Db_source clause_db)
              clause_db
              sources
              rules
              [ projected_binding ]
              clauses)
    |> List.filter_map (merge_projected_binding clause_db vars bindings)
  | OrJoinRequired (required_vars, vars, branches) ->
    ensure_join_vars_bound_in_clause
      bindings
      required_vars
      (query_or_join_clause_string required_vars vars branches);
    ensure_or_join_branches_cover_listed_vars bindings vars branches;
    let projected_binding = project_binding (required_vars @ vars |> List.sort_uniq compare) bindings in
    branches
    |> List.concat_map
         (fun clauses ->
            eval_clauses ~active_rules ~callables ~default_source db sources rules [ projected_binding ] clauses)
    |> List.filter_map (merge_projected_binding db vars bindings)
  | SourceOrJoinRequired (source, required_vars, vars, branches) ->
    let clause_db = source_db db sources source in
    let sources = sources_with_root_default db sources in
    ensure_join_vars_bound_in_clause
      bindings
      required_vars
      (query_or_join_clause_string required_vars vars branches);
    ensure_or_join_branches_cover_listed_vars bindings vars branches;
    let projected_binding = project_binding (required_vars @ vars |> List.sort_uniq compare) bindings in
    branches
    |> List.concat_map
         (fun clauses ->
            eval_clauses
              ~active_rules
              ~callables
              ~default_source:(Db_source clause_db)
              clause_db
              sources
              rules
              [ projected_binding ]
              clauses)
    |> List.filter_map (merge_projected_binding clause_db vars bindings)
  | Rule (name, terms) ->
    let key = rule_call_key db "" name bindings terms in
    matching_rules_for_call active_rules key rules name (List.length terms)
    |> List.concat_map (fun rule ->
      match rule_invocation_binding db bindings rule terms with
      | None -> []
      | Some rule_binding ->
        let rule_callables = rule_invocation_callables callables bindings rule terms in
        eval_clauses
          ~active_rules:(key :: active_rules)
          ~callables:rule_callables
          ~default_source
          db
          sources
          rules
          [ rule_binding ]
          rule.rule_body
        |> List.filter_map (fun rule_binding -> propagate_rule_binding db bindings rule_binding rule terms))
  | SourceRule (source, name, terms) ->
    let rule_db = source_db db sources source in
    let key = rule_call_key rule_db source name bindings terms in
    matching_rules_for_call active_rules key rules name (List.length terms)
    |> List.concat_map (fun rule ->
      match rule_invocation_binding rule_db bindings rule terms with
      | None -> []
      | Some rule_binding ->
        let rule_callables = rule_invocation_callables callables bindings rule terms in
        eval_clauses
          ~active_rules:(key :: active_rules)
          ~callables:rule_callables
          ~default_source:(Db_source rule_db)
          rule_db
          sources
          rules
          [ rule_binding ]
          rule.rule_body
        |> List.filter_map (fun rule_binding -> propagate_rule_binding rule_db bindings rule_binding rule terms))

let parser_query_context : Parser_impl.query_context =
  { empty_db = (fun () -> empty_db ())
  ; parse_pull_pattern
  ; value_of_query_result
  ; string_is_blank
  ; string_includes
  ; string_starts_with
  ; string_ends_with
  ; matches_value_predicate
  ; matches_numeric_predicate
  ; matches_boolean_predicate
  ; comparison_chain_matches
  ; all_values_equal
  ; value_has_count
  ; value_is_not_empty
  ; value_contains
  ; split_at
  ; values_equal
  }

let parse_find form = Parser_impl.parse_find parser_query_context form

let validate_rule_arities = Parser_impl.validate_rule_arities

let parse_binding = Parser_impl.parse_binding
let parse_in = Parser_impl.parse_in
let parse_with = Parser_impl.parse_with

let rule_names = Query.rule_names
let resolve_dynamic_rule_clause = Query.resolve_dynamic_rule_clause
let resolve_dynamic_rule = Query.resolve_dynamic_rule

let parse_query_return form = Parser_impl.parse_query_return parser_query_context form
let parse_query_return_map form = Parser_impl.parse_query_return_map parser_query_context form
let parse_query form = Parser_impl.parse_query parser_query_context form
let parse_query_string input = Parser_impl.parse_query_string parser_query_context input
let parse_query_string_with_pull_context ?default_pull_db ?pull_db_for_source input =
  Parser_impl.parse_query_string_with_pull_context parser_query_context ?default_pull_db ?pull_db_for_source input
let parse_query_return_string input = Parser_impl.parse_query_return_string parser_query_context input
let parse_query_return_string_with_pull_context ?default_pull_db ?pull_db_for_source input =
  Parser_impl.parse_query_return_string_with_pull_context parser_query_context ?default_pull_db ?pull_db_for_source input
let parse_query_return_map_string input = Parser_impl.parse_query_return_map_string parser_query_context input
let parse_query_return_map_string_with_pull_context ?default_pull_db ?pull_db_for_source input =
  Parser_impl.parse_query_return_map_string_with_pull_context parser_query_context ?default_pull_db ?pull_db_for_source input

module Pull_parser = struct
  let parse_pattern = parse_pull_pattern
  let parse_pattern_string = parse_pull_pattern_string
end

module Parser = struct
  let read_edn = read_edn
  let section_forms = Parser_impl.section_forms
  let query_form_section = Parser_impl.query_form_section
  let query_form_sections = Parser_impl.query_form_sections
  let query_form_map = Parser_impl.query_form_map
  let query_form_sequence = Parser_impl.query_form_sequence
  let query_symbol_name = Parser_impl.query_symbol_name
  let query_callable_name = Parser_impl.query_callable_name
  let is_plain_input_symbol = Parser_impl.is_plain_input_symbol
  let is_query_input_symbol = Parser_impl.is_query_input_symbol
  let query_input_name = Parser_impl.query_input_name
  let query_source_name = Parser_impl.query_source_name
  let is_query_source_symbol = Parser_impl.is_query_source_symbol
  let is_plain_rule_symbol = Parser_impl.is_plain_rule_symbol
  let aggregate_of_symbol = Parser_impl.aggregate_of_symbol
  let amount_aggregate_of_symbol = Parser_impl.amount_aggregate_of_symbol
  let dynamic_amount_aggregate_of_symbol = Parser_impl.dynamic_amount_aggregate_of_symbol
  let parse_find_arg = Parser_impl.parse_find_arg
  let parse_find_args = Parser_impl.parse_find_args
  let parse_output_var = Parser_impl.parse_output_var
  let parse_output_vars = Parser_impl.parse_output_vars
  let parse_flat_output_vars = Parser_impl.parse_flat_output_vars
  let parse_collection_output_var = Parser_impl.parse_collection_output_var
  let parse_relation_output_vars = Parser_impl.parse_relation_output_vars
  let nonempty_input_vars = Parser_impl.nonempty_input_vars
  let input_relation_vars = Parser_impl.input_relation_vars
  let input_var_of_form = Parser_impl.input_var_of_form
  let flat_input_vars = Parser_impl.flat_input_vars
  let parse_nested_input_binding = Parser_impl.parse_nested_input_binding
  let nested_relation_binding = Parser_impl.nested_relation_binding
  let parse_input_binding = Parser_impl.parse_input_binding
  let parse_inputs = Parser_impl.parse_inputs
  let input_declares_rules_var = Parser_impl.input_declares_rules_var
  let ensure_distinct_input_rules_var = Parser_impl.ensure_distinct_input_rules_var
  let parse_with_var = Parser_impl.parse_with_var
  let parse_with_section = Parser_impl.parse_with_section
  let parse_return_map_labels = Parser_impl.parse_return_map_labels
  let parse_return_map_section = Parser_impl.parse_return_map_section
  let lookup_ref_of_form = Parser_impl.lookup_ref_of_form
  let parse_pattern_term = Parser_impl.parse_pattern_term
  let comparison_predicate_of_symbol = Parser_impl.comparison_predicate_of_symbol
  let value_predicate_of_symbol = Parser_impl.value_predicate_of_symbol
  let numeric_predicate_of_symbol = Parser_impl.numeric_predicate_of_symbol
  let boolean_predicate_of_symbol = Parser_impl.boolean_predicate_of_symbol
  let unary_string_predicate_clause_of_symbol = Parser_impl.unary_string_predicate_clause_of_symbol
  let binary_string_predicate_clause_of_symbol = Parser_impl.binary_string_predicate_clause_of_symbol
  let equality_predicate_of_symbol = Parser_impl.equality_predicate_of_symbol
  let arithmetic_op_of_symbol = Parser_impl.arithmetic_op_of_symbol
  let query_attr_name = Parser_impl.query_attr_name
  let parse_data_pattern_clause = Parser_impl.parse_data_pattern_clause
  let parse_rule_expr = Parser_impl.parse_rule_expr
  let parse_source_pattern_clause = Parser_impl.parse_source_pattern_clause
  let parse_missing_clause = Parser_impl.parse_missing_clause
  let parse_get_else_clause = Parser_impl.parse_get_else_clause
  let parse_two_output_vars = Parser_impl.parse_two_output_vars
  let parse_get_some_clause = Parser_impl.parse_get_some_clause
  let parse_get_clause = Parser_impl.parse_get_clause
  let parse_core_value_function = Parser_impl.parse_core_value_function
  let parse_collection_function = Parser_impl.parse_collection_function
  let parse_flat_value_function = Parser_impl.parse_flat_value_function
  let ground_values_of_form = Parser_impl.ground_values_of_form
  let ground_relation_rows_of_form = Parser_impl.ground_relation_rows_of_form
  let dynamic_ground_term = Parser_impl.dynamic_ground_term
  let parse_ground_function = Parser_impl.parse_ground_function
  let parse_value_metadata_function = Parser_impl.parse_value_metadata_function
  let parse_string_transform_function = Parser_impl.parse_string_transform_function
  let parse_binding = parse_binding
  let parse_in = parse_in
  let parse_with = parse_with
  let parse_find = parse_find
  let parse_clause form = Parser_impl.parse_pattern_clause parser_query_context form
  let parse_rules form = Parser_impl.parse_rules parser_query_context (Some form)
  let parse_query = parse_query
  let parse_query_string = parse_query_string
  let parse_query_return = parse_query_return
  let parse_query_return_string = parse_query_return_string
  let parse_query_return_map = parse_query_return_map
  let parse_query_return_map_string = parse_query_return_map_string
end

let pull_string ?visitor db input entity_ref =
  pull ?visitor db (parse_pull_pattern_string db input) entity_ref

let pull_many_string ?visitor db input entity_refs =
  pull_many ?visitor db (parse_pull_pattern_string db input) entity_refs

module Pull_api = struct
  let pull = pull
  let pull_string = pull_string
  let pull_many = pull_many
  let pull_many_string = pull_many_string
end

let query_rules_and_where query input_rules =
  let rules = validate_rule_arities (query.rules @ input_rules) in
  let names = rule_names rules in
  List.map (resolve_dynamic_rule names) rules, List.map (resolve_dynamic_rule_clause names) query.where

let q_sources_raw ?(inputs = []) db sources query =
  let callables, input_bindings, input_rules = initial_query_context db query inputs in
  let rules, where = query_rules_and_where query input_rules in
  let bindings = eval_clauses ~callables db sources rules input_bindings where in
  if has_aggregates query.find then
    if query.with_vars = [] then
      aggregate_rows ~callables db sources bindings query.find
    else
      aggregate_rows_with ~callables db sources bindings query.find query.with_vars
  else if query.with_vars <> [] then
    non_aggregate_rows_with db sources bindings query.find query.with_vars
  else
    bindings
    |> List.filter_map (fun binding -> collect_find_specs db sources binding query.find)
    |> List.sort_uniq compare

let q_with_raw ?(inputs = []) db with_vars query =
  let callables, input_bindings, input_rules = initial_query_context db query inputs in
  let rules, where = query_rules_and_where query input_rules in
  let bindings = eval_clauses ~callables db [] rules input_bindings where in
  let with_vars = query.with_vars @ with_vars |> List.sort_uniq compare in
  if has_aggregates query.find then
    aggregate_rows_with ~callables db [] bindings query.find with_vars
  else
    non_aggregate_rows_with db [] bindings query.find with_vars

module Query_impl = Query

let query_context : Query_impl.context =
  { empty_db = (fun () -> empty_db ())
  ; q_sources = q_sources_raw
  ; q_with = q_with_raw
  ; parse_query_string_with_pull_context
  ; parse_query_return_string_with_pull_context
  ; parse_query_return_map_string_with_pull_context
  ; compare_value
  }

module Query = struct
  type query_callables = Query_impl.query_callables =
    { callable_predicates : (string * (query_result list -> bool)) list
    ; callable_functions : (string * (query_result list -> query_result list option)) list
    ; callable_aggregates : (string * (query_result list -> query_result)) list
    ; callable_aliases : (string * string) list
    }

  type result_resolution_context = Query_impl.result_resolution_context =
    { validate_entity_id : int -> entity_id
    ; resolve_query_value : value -> value option
    ; lookup_ref_entity_id : attr -> value -> entity_id option
    }

  type match_context = Query_impl.match_context =
    { result_resolution_context : result_resolution_context
    ; source_db : db
    ; ident_entity_id : string -> entity_id option
    ; unresolved_lookup_ref_message : attr -> value -> string
    ; value_equal : value -> value -> bool
    ; coerce_tuple_lookup_value : attr -> value -> value
    }

  type source_context = Query_impl.source_context =
    { match_context : match_context
    ; pattern_datoms : db -> query_term -> datom list
    ; match_data_pattern :
        db ->
        (string * query_result) list ->
        query_term ->
        query_term ->
        query_term ->
        datom ->
        (string * query_result) list option
    ; match_data_pattern_tx :
        db ->
        (string * query_result) list ->
        query_term ->
        query_term ->
        query_term ->
        query_term ->
        datom ->
        (string * query_result) list option
    ; match_data_pattern_tx_op :
        db ->
        (string * query_result) list ->
        query_term ->
        query_term ->
        query_term ->
        query_term ->
        query_term ->
        datom ->
        (string * query_result) list option
    }

  type input_context = Query_impl.input_context =
    { resolve_query_input_result : query_result -> query_result option
    ; bind_var :
        string ->
        query_result ->
        (string * query_result) list ->
        (string * query_result) list option
    ; entity_id_of_ref : entity_ref -> entity_id option
    }

  let empty_query_callables = Query_impl.empty_query_callables
  let q = Query_impl.q query_context
  let q_string = Query_impl.q_string query_context
  let q_with = Query_impl.q_with query_context
  let q_with_string = Query_impl.q_with_string query_context
  let q_sources = Query_impl.q_sources query_context
  let q_sources_string = Query_impl.q_sources_string query_context
  let q_return = Query_impl.q_return query_context
  let q_return_string = Query_impl.q_return_string query_context
  let q_return_map = Query_impl.q_return_map query_context
  let q_return_map_string = Query_impl.q_return_map_string query_context
  let return_map_label_count = Query_impl.return_map_label_count
  let return_map_name = Query_impl.return_map_name
  let validate_query_return_map = Query_impl.validate_query_return_map
  let has_aggregates = Query_impl.has_aggregates
  let collect_find_vars = Query_impl.collect_find_vars
  let group_by_key = Query_impl.group_by_key
  let grouping_vars_of_find = Query_impl.grouping_vars_of_find
  let aggregate_amount_value = Query_impl.aggregate_amount_value
  let resolve_dynamic_aggregate = Query_impl.resolve_dynamic_aggregate
  let aggregate_param_vars = Query_impl.aggregate_param_vars
  let aggregate_callable_vars = Query_impl.aggregate_callable_vars
  let split_aggregate_terms = Query_impl.split_aggregate_terms
  let aggregate_input_values = Query_impl.aggregate_input_values
  let resolve_callable_name = Query_impl.resolve_callable_name
  let callable_predicate = Query_impl.callable_predicate
  let callable_function = Query_impl.callable_function
  let callable_aggregate = Query_impl.callable_aggregate
  let has_callable = Query_impl.has_callable
  let alias_callable = Query_impl.alias_callable
  let resolve_callable_aggregate = Query_impl.resolve_callable_aggregate
  let result_of_datom_e = Query_impl.result_of_datom_e
  let result_of_datom_a = Query_impl.result_of_datom_a
  let result_of_datom_v = Query_impl.result_of_datom_v
  let result_of_datom_tx = Query_impl.result_of_datom_tx
  let result_of_datom_op = Query_impl.result_of_datom_op
  let result_of_ref = Query_impl.result_of_ref
  let entity_id_of_resolved_query_result = Query_impl.entity_id_of_resolved_query_result
  let resolved_query_result = Query_impl.resolved_query_result
  let lookup_ref_entity_id_of_value = Query_impl.lookup_ref_entity_id_of_value
  let query_result_entity_id = Query_impl.query_result_entity_id
  let query_results_equivalent = Query_impl.query_results_equivalent
  let bind_var = Query_impl.bind_var
  let result_matches_entity = Query_impl.result_matches_entity
  let match_query_term = Query_impl.match_query_term
  let match_value_term_for_datom_attr = Query_impl.match_value_term_for_datom_attr
  let match_pattern_clause = Query_impl.match_pattern_clause
  let match_pattern_tx_clause = Query_impl.match_pattern_tx_clause
  let match_reverse_pattern_clause = Query_impl.match_reverse_pattern_clause
  let eval_query_term = Query_impl.eval_query_term
  let collect_query_terms = Query_impl.collect_query_terms
  let collect_query_terms_exn = Query_impl.collect_query_terms_exn
  let query_term_entity_id = Query_impl.query_term_entity_id
  let source = Query_impl.source
  let sources_with_root_default = Query_impl.sources_with_root_default
  let source_db = Query_impl.source_db
  let query_source_db = Query_impl.query_source_db
  let match_relation_row = Query_impl.match_relation_row
  let match_query_source_pattern = Query_impl.match_query_source_pattern
  let match_source_pattern = Query_impl.match_source_pattern
  let match_relation_source_pattern = Query_impl.match_relation_source_pattern
  let eval_query_term_with_sources = Query_impl.eval_query_term_with_sources
  let collect_dynamic_query_terms_exn = Query_impl.collect_dynamic_query_terms_exn
  let aggregate_extra_args = Query_impl.aggregate_extra_args
  let aggregate_values = Query_impl.aggregate_values
  let query_callables_of_inputs = Query_impl.query_callables_of_inputs
  let query_rules_of_inputs = Query_impl.query_rules_of_inputs
  let matching_rules = Query_impl.matching_rules
  let matching_rules_exn = Query_impl.matching_rules_exn
  let project_binding = Query_impl.project_binding
  let rule_invocation_callables = Query_impl.rule_invocation_callables
  let vars_of_query_term = Query_impl.vars_of_query_term
  let vars_of_query_terms = Query_impl.vars_of_query_terms
  let vars_of_clause = Query_impl.vars_of_clause
  let named_source = Query_impl.named_source
  let sources_of_query_term = Query_impl.sources_of_query_term
  let sources_of_query_terms = Query_impl.sources_of_query_terms
  let sources_of_optional_query_term = Query_impl.sources_of_optional_query_term
  let sources_of_clause = Query_impl.sources_of_clause
  let sources_of_find_spec = Query_impl.sources_of_find_spec
  let has_rule_clause = Query_impl.has_rule_clause
  let rule_names = Query_impl.rule_names
  let resolve_dynamic_rule_clause = Query_impl.resolve_dynamic_rule_clause
  let resolve_dynamic_rule = Query_impl.resolve_dynamic_rule
  let find_spec_uses_default_source = Query_impl.find_spec_uses_default_source
  let clause_uses_default_source = Query_impl.clause_uses_default_source
  let infer_default_inputs = Query_impl.infer_default_inputs
  let query_term_vars = Query_impl.query_term_vars
  let vars_of_find_spec = Query_impl.vars_of_find_spec
  let vars_of_input_binding = Query_impl.vars_of_input_binding
  let vars_of_input = Query_impl.vars_of_input
  let source_of_input = Query_impl.source_of_input
  let ensure_distinct_input_vars = Query_impl.ensure_distinct_input_vars
  let ensure_distinct_input_sources = Query_impl.ensure_distinct_input_sources
  let format_query_vars = Query_impl.format_query_vars
  let format_source_vars = Query_impl.format_source_vars
  let validate_query = Query_impl.validate_query
  let query_input_var_label = Query_impl.query_input_var_label
  let query_term_string = Query_impl.query_term_string
  let query_output_var_string = Query_impl.query_output_var_string
  let query_output_binding_string = Query_impl.query_output_binding_string
  let query_call_string = Query_impl.query_call_string
  let numeric_predicate_symbol = Query_impl.numeric_predicate_symbol
  let arithmetic_op_symbol = Query_impl.arithmetic_op_symbol
  let query_clause_string = Query_impl.query_clause_string
  let query_not_clause_string = Query_impl.query_not_clause_string
  let query_or_clause_string = Query_impl.query_or_clause_string
  let query_or_join_vars_string = Query_impl.query_or_join_vars_string
  let query_or_join_clause_string = Query_impl.query_or_join_clause_string
  let query_var_set_string = Query_impl.query_var_set_string
  let query_var_sets_string = Query_impl.query_var_sets_string
  let unbound_vars_of_terms = Query_impl.unbound_vars_of_terms
  let ensure_query_terms_bound = Query_impl.ensure_query_terms_bound
  let ensure_not_has_outer_binding = Query_impl.ensure_not_has_outer_binding
  let vars_of_branch = Query_impl.vars_of_branch
  let free_vars_of_branch = Query_impl.free_vars_of_branch
  let ensure_or_branch_vars_match = Query_impl.ensure_or_branch_vars_match
  let ensure_join_vars_bound = Query_impl.ensure_join_vars_bound
  let ensure_join_vars_bound_in_clause = Query_impl.ensure_join_vars_bound_in_clause
  let ensure_or_join_branches_cover_listed_vars = Query_impl.ensure_or_join_branches_cover_listed_vars
  let clause_calls_rule = Query_impl.clause_calls_rule
  let matching_rules_for_call = Query_impl.matching_rules_for_call
  let query_input_binding_string = Query_impl.query_input_binding_string
  let query_input_decl_binding_string = Query_impl.query_input_decl_binding_string
  let query_input_binding_label = Query_impl.query_input_binding_label
  let query_input_consumes_argument = Query_impl.query_input_consumes_argument
  let values_of_collection_result = Query_impl.values_of_collection_result
  let row_of_collection_result = Query_impl.row_of_collection_result
  let row_of_scalar_sequence = Query_impl.row_of_scalar_sequence
  let rows_of_map_entries = Query_impl.rows_of_map_entries
  let bind_relation_row = Query_impl.bind_relation_row
  let resolve_query_input_row = Query_impl.resolve_query_input_row
  let collection_values_of_input = Query_impl.collection_values_of_input
  let row_values_of_input = Query_impl.row_values_of_input
  let eval_ground_term_tuple = Query_impl.eval_ground_term_tuple
  let eval_ground_term_relation = Query_impl.eval_ground_term_relation
  let bind_input_binding = Query_impl.bind_input_binding
  let bind_nested_input_tuple = Query_impl.bind_nested_input_tuple
  let apply_query_input = Query_impl.apply_query_input
  let bind_query_inputs = Query_impl.bind_query_inputs
end

let q = Query.q
let q_string = Query.q_string
let q_with = Query.q_with
let q_with_string = Query.q_with_string
let q_sources = Query.q_sources
let q_sources_string = Query.q_sources_string
let q_return = Query.q_return
let q_return_string = Query.q_return_string
let q_return_map = Query.q_return_map
let q_return_map_string = Query.q_return_map_string

let db_datoms = datoms
let db_datoms_ref = datoms_ref
let db_find_datom = find_datom
let db_find_datom_ref = find_datom_ref
let db_seek_datoms = seek_datoms
let db_seek_datoms_ref = seek_datoms_ref
let db_rseek_datoms = rseek_datoms
let db_rseek_datoms_ref = rseek_datoms_ref
let db_index_range = index_range

module Db = struct
  include Db_impl

  let datoms = db_datoms
  let datoms_ref = db_datoms_ref
  let find_datom = db_find_datom
  let find_datom_ref = db_find_datom_ref
  let seek_datoms = db_seek_datoms
  let seek_datoms_ref = db_seek_datoms_ref
  let rseek_datoms = db_rseek_datoms
  let rseek_datoms_ref = db_rseek_datoms_ref
  let index_range = db_index_range
end
