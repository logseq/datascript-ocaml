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

let result_of_datom_e d = Result_entity d.e

let result_of_datom_a d = Result_attr d.a

let result_of_datom_v d = Result_value d.v

let result_of_datom_tx d = Result_entity d.tx

let result_of_datom_op d =
  Result_value (Keyword (if d.added then "db/add" else "db/retract"))

let result_of_ref = function
  | Result_value (Ref eid) -> Result_entity eid
  | result -> result

let resolve_query_value db value = resolve_ref_value ~preserve_vector:true db value

let resolved_query_result db = function
  | Result_value value -> Option.map (fun value -> result_of_ref (Result_value value)) (resolve_query_value db value)
  | Result_db _ -> None
  | result -> Some result

let lookup_ref_entity_id_of_value db = function
  | List [ Keyword attr; value ] | List [ String attr; value ]
  | Vector [ Keyword attr; value ] | Vector [ String attr; value ] ->
    entity_id_of_ref db (Lookup_ref (attr, value))
  | _ -> None

let entity_id_of_resolved_query_result = function
  | Some (Result_entity entity_id) -> Some entity_id
  | Some (Result_value (Int entity_id)) -> Some (validate_entity_id entity_id)
  | Some (Result_value (Ref entity_id)) -> Some entity_id
  | _ -> None

let query_result_entity_id db result =
  match result with
  | Result_value value ->
    (match lookup_ref_entity_id_of_value db value with
     | Some entity_id -> Some entity_id
     | None -> entity_id_of_resolved_query_result (resolved_query_result db result))
  | _ -> entity_id_of_resolved_query_result (resolved_query_result db result)

let query_results_equivalent db left right =
  match left, right with
  | Result_db left_db, Result_db right_db -> left_db == right_db
  | Result_db _, _ | _, Result_db _ -> false
  | _ ->
    left = right
    ||
    match query_result_entity_id db left, query_result_entity_id db right with
    | Some left_id, Some right_id -> left_id = right_id
    | _ ->
      (match resolved_query_result db left, resolved_query_result db right with
       | Some left, Some right -> left = right
       | _ -> false)

let bind_var db name value bindings =
  match List.assoc_opt name bindings with
  | Some bound when query_results_equivalent db bound value -> Some bindings
  | Some _ -> None
  | None -> Some ((name, value) :: bindings)

let result_matches_entity db entity_id result =
  match query_result_entity_id db result with
  | Some actual -> actual = entity_id
  | None -> false

let match_query_term db term value bindings =
  match term with
  | QWildcard -> Some bindings
  | QEntity eid when result_matches_entity db eid value -> Some bindings
  | QIdent ident ->
    (match entid db ident_attr (Keyword ident) with
     | Some entity_id when result_matches_entity db entity_id value -> Some bindings
     | _ -> None)
  | QLookupRef (attr, lookup_value) ->
    (match entity_id_of_ref db (Lookup_ref (attr, lookup_value)) with
     | Some entity_id when result_matches_entity db entity_id value -> Some bindings
     | Some _ -> None
     | None -> invalid_arg (unresolved_lookup_ref_message attr lookup_value))
  | QAttr attr when value = Result_attr attr -> Some bindings
  | QValue expected ->
    (match resolve_query_value db expected, value with
     | Some expected, Result_value actual when value_equal actual expected -> Some bindings
     | Some (Ref expected), Result_entity actual when actual = expected -> Some bindings
     | Some (Keyword ident), _ ->
       (match entid db ident_attr (Keyword ident) with
        | Some entity_id when result_matches_entity db entity_id value -> Some bindings
        | _ -> None)
     | _ -> None)
  | QVar name -> bind_var db name (result_of_ref value) bindings
  | _ -> None

let match_value_term_for_datom_attr db bindings v_term datom =
  match v_term with
  | QValue value ->
    let value = coerce_tuple_lookup_value db (visible_datoms db) datom.a value in
    match_query_term db (QValue value) (result_of_datom_v datom) bindings
  | _ -> match_query_term db v_term (result_of_datom_v datom) bindings

let match_pattern_clause db bindings e_term a_term v_term datom =
  let ( let* ) = Option.bind in
  let* bindings = match_query_term db e_term (result_of_datom_e datom) bindings in
  let* bindings = match_query_term db a_term (result_of_datom_a datom) bindings in
  match_value_term_for_datom_attr db bindings v_term datom

let match_pattern_tx_clause db bindings e_term a_term v_term tx_term datom =
  let ( let* ) = Option.bind in
  let* bindings = match_pattern_clause db bindings e_term a_term v_term datom in
  match_query_term db tx_term (result_of_datom_tx datom) bindings

let match_reverse_pattern_clause db bindings e_term reverse_attr v_term datom =
  match datom.v with
  | Ref target ->
    let ( let* ) = Option.bind in
    let* bindings = match_query_term db e_term (Result_entity target) bindings in
    let* bindings = match_query_term db (QAttr reverse_attr) (Result_attr reverse_attr) bindings in
    match_query_term db v_term (Result_entity datom.e) bindings
  | _ -> None

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

let eval_query_term db bindings = function
  | QVar name -> List.assoc_opt name bindings
  | QEntity eid -> Some (Result_entity eid)
  | QIdent ident -> Option.map (fun entity_id -> Result_entity entity_id) (entid db ident_attr (Keyword ident))
  | QLookupRef (attr, value) ->
    (match entity_id_of_ref db (Lookup_ref (attr, value)) with
     | Some entity_id -> Some (Result_entity entity_id)
     | None -> invalid_arg (unresolved_lookup_ref_message attr value))
  | QAttr attr -> Some (Result_attr attr)
  | QValue value -> Option.map (fun value -> Result_value value) (resolve_query_value db value)
  | QSource "$" -> Some (Result_db db)
  | QSource source -> invalid_arg ("source term requires query source context: " ^ source)
  | QWildcard -> None

let collect_query_terms db bindings terms =
  let rec collect acc = function
    | [] -> Some (List.rev acc)
    | term :: rest ->
      (match eval_query_term db bindings term with
       | Some value -> collect (value :: acc) rest
       | None -> None)
  in
  collect [] terms

let collect_query_terms_exn db bindings terms =
  match collect_query_terms db bindings terms with
  | Some values -> values
  | None -> invalid_arg "insufficient bindings"

let collect_find_vars = Query.collect_find_vars

let query_term_entity_id db bindings term =
  Option.bind (eval_query_term db bindings term) (query_result_entity_id db)

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

let string_starts_with value prefix =
  let prefix_length = String.length prefix in
  String.length value >= prefix_length && String.sub value 0 prefix_length = prefix

let string_ends_with value suffix =
  let value_length = String.length value in
  let suffix_length = String.length suffix in
  value_length >= suffix_length && String.sub value (value_length - suffix_length) suffix_length = suffix

let string_index_of value needle =
  let value_length = String.length value in
  let needle_length = String.length needle in
  let rec scan index =
    if index + needle_length > value_length then
      None
    else if String.sub value index needle_length = needle then
      Some index
    else
      scan (index + 1)
  in
  scan 0

let string_includes value needle =
  Option.is_some (string_index_of value needle)

let string_last_index_of value needle =
  let needle_length = String.length needle in
  let rec scan index =
    if index < 0 then
      None
    else if String.sub value index needle_length = needle then
      Some index
    else
      scan (index - 1)
  in
  scan (String.length value - needle_length)

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

let string_of_query_value = function
  | String value -> value
  | Symbol value -> value
  | Nil -> ""
  | Int value -> string_of_int value
  | Float value -> string_of_float value
  | Bool true -> "true"
  | Bool false -> "false"
  | Keyword value -> ":" ^ value
  | Uuid value -> value
  | Instant value -> string_of_int value
  | Regex value -> value
  | Ref entity_id -> string_of_int entity_id
  | List _ | Vector _ | Map _ | Set _ | Tuple _ | TxRef | Ref_to _ -> invalid_arg "cannot stringify composite query value"

let escaped_string_literal value =
  let buffer = Buffer.create (String.length value + 2) in
  Buffer.add_char buffer '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | ch -> Buffer.add_char buffer ch)
    value;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let rec print_query_value ~readably = function
  | String value -> if readably then escaped_string_literal value else value
  | Symbol value -> value
  | Nil -> "nil"
  | Int value -> string_of_int value
  | Float value -> string_of_float value
  | Bool true -> "true"
  | Bool false -> "false"
  | Keyword value -> ":" ^ value
  | Uuid value -> value
  | Instant value -> string_of_int value
  | Regex value -> "#\"" ^ value ^ "\""
  | Ref entity_id -> string_of_int entity_id
  | List values -> "(" ^ print_query_values ~readably values ^ ")"
  | Vector values -> "[" ^ print_query_values ~readably values ^ "]"
  | Set values -> "#{" ^ print_query_values ~readably values ^ "}"
  | Map entries ->
    entries
    |> List.map (fun (key, value) -> print_query_value ~readably key ^ " " ^ print_query_value ~readably value)
    |> String.concat ", "
    |> fun body -> "{" ^ body ^ "}"
  | Tuple values ->
    values
    |> List.map (function
      | None -> "nil"
      | Some value -> print_query_value ~readably value)
    |> String.concat " "
    |> fun body -> "[" ^ body ^ "]"
  | TxRef -> "#datascript/tx"
  | Ref_to _ -> "#datascript/ref"

and print_query_values ~readably values =
  values |> List.map (print_query_value ~readably) |> String.concat " "

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

let collection_string_values = function
  | List values | Vector values | Set values -> Some (List.map string_of_query_value values)
  | Tuple values ->
    values
    |> List.map (function
      | Some value -> string_of_query_value value
      | None -> "")
    |> fun values -> Some values
  | _ -> None

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

let replace_string ?(first_only = false) value pattern replacement =
  if pattern = "" then
    value
  else
    let pattern_length = String.length pattern in
    let buffer = Buffer.create (String.length value) in
    let rec loop index =
      match string_index_of (String.sub value index (String.length value - index)) pattern with
      | None -> Buffer.add_substring buffer value index (String.length value - index)
      | Some relative_index ->
        let match_index = index + relative_index in
        Buffer.add_substring buffer value index (match_index - index);
        Buffer.add_string buffer replacement;
        let next_index = match_index + pattern_length in
        if first_only then
          Buffer.add_substring buffer value next_index (String.length value - next_index)
        else
          loop next_index
    in
    loop 0;
    Buffer.contents buffer

let compile_regex pattern =
  try Str.regexp pattern with
  | Failure message -> invalid_arg ("invalid regex pattern: " ^ message)

let replace_regex ?(first_only = false) value pattern replacement =
  let regex = compile_regex pattern in
  if first_only then
    Str.replace_first regex replacement value
  else
    Str.global_replace regex replacement value

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

let string_escape_replacement replacements ch =
  let key = String (String.make 1 ch) in
  List.find_map
    (fun (replacement_key, replacement_value) ->
      if compare_value replacement_key key = 0 then Some (string_of_query_value replacement_value) else None)
    replacements

let escape_string value replacements =
  let buffer = Buffer.create (String.length value) in
  String.iter
    (fun ch ->
      match string_escape_replacement replacements ch with
      | Some replacement -> Buffer.add_string buffer replacement
      | None -> Buffer.add_char buffer ch)
    value;
  Buffer.contents buffer

let eval_string_escape_clause db bindings value_term replacement_term output_var =
  match collect_query_terms db bindings [ value_term; replacement_term ] with
  | Some [ Result_value (String value); Result_value (Map replacements) ] ->
    (match bind_string_value db output_var (escape_string value replacements) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let regex_pattern_of_result = function
  | Result_value (Regex pattern) | Result_value (String pattern) -> Some pattern
  | _ -> None

let regex_find pattern value =
  let regex = compile_regex pattern in
  try
    ignore (Str.search_forward regex value 0);
    Some (Str.matched_string value)
  with
  | Not_found -> None

let regex_matches pattern value =
  let regex = compile_regex pattern in
  if Str.string_match regex value 0 && Str.match_end () = String.length value then
    Some (Str.matched_string value)
  else
    None

let regex_seq pattern value =
  let regex = compile_regex pattern in
  let length = String.length value in
  let rec collect index matches =
    if index > length then
      List.rev matches
    else
      try
        let match_start = Str.search_forward regex value index in
        let match_end = Str.match_end () in
        let matched = Str.matched_string value in
        let next_index = if match_end <= match_start then match_start + 1 else match_end in
        collect next_index (matched :: matches)
      with
      | Not_found -> List.rev matches
  in
  collect 0 []

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

let is_ascii_whitespace = function
  | ' ' | '\n' | '\r' | '\t' | '\012' -> true
  | _ -> false

let string_is_blank value =
  String.for_all is_ascii_whitespace value

let eval_string_blank_clause db bindings term =
  match eval_query_term db bindings term with
  | Some (Result_value (String value)) when string_is_blank value -> [ bindings ]
  | Some _ | None -> []

let split_string value separator =
  if separator = "" then
    invalid_arg "split separator cannot be empty";
  let separator_length = String.length separator in
  let rec collect start acc =
    match string_index_of (String.sub value start (String.length value - start)) separator with
    | None -> List.rev (String.sub value start (String.length value - start) :: acc)
    | Some relative_index ->
      let index = start + relative_index in
      collect (index + separator_length) (String.sub value start (index - start) :: acc)
  in
  collect 0 []

let split_string_limited value separator limit =
  if limit <= 0 then
    split_string value separator
  else if limit = 1 then
    [ value ]
  else begin
    if separator = "" then
      invalid_arg "split separator cannot be empty";
    let separator_length = String.length separator in
    let rec collect start remaining acc =
      if remaining = 1 then
        List.rev (String.sub value start (String.length value - start) :: acc)
      else
        match string_index_of (String.sub value start (String.length value - start)) separator with
        | None -> List.rev (String.sub value start (String.length value - start) :: acc)
        | Some relative_index ->
          let index = start + relative_index in
          collect
            (index + separator_length)
            (remaining - 1)
            (String.sub value start (index - start) :: acc)
    in
    collect 0 limit []
  end

let split_regex value pattern =
  Str.split (compile_regex pattern) value

let split_regex_limited value pattern limit =
  if limit <= 0 then
    split_regex value pattern
  else if limit = 1 then
    [ value ]
  else begin
    let regex = compile_regex pattern in
    let length = String.length value in
    let rec collect start remaining acc =
      if remaining = 1 || start > length then
        List.rev (String.sub value start (length - start) :: acc)
      else
        try
          let match_start = Str.search_forward regex value start in
          let match_end = Str.match_end () in
          let next_start = if match_end <= match_start then match_start + 1 else match_end in
          collect next_start (remaining - 1) (String.sub value start (match_start - start) :: acc)
        with
        | Not_found -> List.rev (String.sub value start (length - start) :: acc)
    in
    collect 0 limit []
  end

let split_lines value =
  let length = String.length value in
  let rec collect start index acc =
    if index >= length then
      List.rev (String.sub value start (length - start) :: acc)
    else
      match value.[index] with
      | '\n' ->
        collect (index + 1) (index + 1) (String.sub value start (index - start) :: acc)
      | '\r' ->
        let next_index = if index + 1 < length && value.[index + 1] = '\n' then index + 2 else index + 1 in
        collect next_index next_index (String.sub value start (index - start) :: acc)
      | _ -> collect start (index + 1) acc
  in
  collect 0 0 []

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

let reverse_string value =
  String.init (String.length value) (fun index -> value.[String.length value - index - 1])

let capitalize_string value =
  match String.length value with
  | 0 -> value
  | length ->
    String.make 1 (Char.uppercase_ascii value.[0])
    ^ String.lowercase_ascii (String.sub value 1 (length - 1))

let trim_left_with pred value =
  let length = String.length value in
  let rec first_non_matching index =
    if index >= length then length
    else if pred value.[index] then first_non_matching (index + 1)
    else index
  in
  let start = first_non_matching 0 in
  String.sub value start (length - start)

let trim_right_with pred value =
  let rec last_non_matching index =
    if index < 0 then -1
    else if pred value.[index] then last_non_matching (index - 1)
    else index
  in
  String.sub value 0 (last_non_matching (String.length value - 1) + 1)

let trim_with pred value =
  value |> trim_left_with pred |> trim_right_with pred

let is_newline = function
  | '\n' | '\r' -> true
  | _ -> false

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
  match List.assoc_opt name sources with
  | Some source -> source
  | None ->
    if name = "$" then Db_source default_db else invalid_arg ("unknown query source: " ^ name)

let sources_with_root_default db sources =
  if List.mem_assoc "$" sources then sources else ("$", Db_source db) :: sources

let source_db default_db sources name =
  match source default_db sources name with
  | Db_source db -> db
  | Relation_source _ -> invalid_arg ("query source is not a database: " ^ name)

let query_source_db = function
  | Db_source db -> db
  | Relation_source _ -> invalid_arg "query source is not a database"

let match_relation_row db bindings terms row =
  let rec match_terms binding terms row =
    match binding, terms, row with
    | None, _, _ -> None
    | Some binding, [], _ -> Some binding
    | Some _, _ :: _, [] -> invalid_arg "source relation row arity mismatch"
    | Some binding, term :: terms, value :: row ->
      match_terms (match_query_term db term value binding) terms row
  in
  match_terms (Some bindings) terms row

let match_query_source_pattern default_db source bindings terms =
  match source with
  | Db_source source_db ->
    (match terms with
     | [ e_term; a_term; v_term ] ->
       pattern_datoms source_db a_term
       |> List.filter_map (fun datom -> match_data_pattern source_db bindings e_term a_term v_term datom)
     | [ e_term; a_term; v_term; tx_term ] ->
       pattern_datoms source_db a_term
       |> List.filter_map (fun datom -> match_data_pattern_tx source_db bindings e_term a_term v_term tx_term datom)
     | [ e_term; a_term; v_term; tx_term; op_term ] ->
       pattern_datoms source_db a_term
       |> List.filter_map (fun datom -> match_data_pattern_tx_op source_db bindings e_term a_term v_term tx_term op_term datom)
     | _ -> invalid_arg "database source patterns expect 3, 4, or 5 terms")
  | Relation_source rows ->
    rows
    |> List.filter_map (fun row -> match_relation_row default_db bindings terms row)

let match_source_pattern default_db sources source_name bindings terms =
  match_query_source_pattern default_db (source default_db sources source_name) bindings terms

let match_relation_source_pattern default_db sources source_name bindings terms =
  let attr_term_of_short_pattern = function
    | QValue (Keyword attr | String attr | Symbol attr) -> QAttr attr
    | term -> term
  in
  match source default_db sources source_name with
  | Relation_source rows ->
    rows
    |> List.filter_map (fun row -> match_relation_row default_db bindings terms row)
  | Db_source _ ->
    (match terms with
     | [ e_term ] ->
       match_source_pattern default_db sources source_name bindings [ e_term; QWildcard; QWildcard ]
     | [ e_term; a_term ] ->
       match_source_pattern
         default_db
         sources
         source_name
         bindings
         [ e_term; attr_term_of_short_pattern a_term; QWildcard ]
     | _ -> invalid_arg ("query source is not a relation: " ^ source_name))

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

let aggregate_callable_vars = Query.aggregate_callable_vars

let query_term_vars terms =
  terms
  |> List.filter_map (function
    | QVar var -> Some var
    | QEntity _ | QIdent _ | QLookupRef _ | QAttr _ | QValue _ | QSource _ | QWildcard -> None)

let eval_query_term_with_sources db sources bindings = function
  | QSource source -> Some (Result_db (source_db db sources source))
  | term -> eval_query_term db bindings term

let split_aggregate_terms = Query.split_aggregate_terms

let aggregate_extra_args db sources group_bindings terms =
  let extra_terms, _ = split_aggregate_terms terms in
  let binding =
    match group_bindings with
    | first :: _ -> first
    | [] -> []
  in
  let rec collect acc = function
    | [] -> List.rev acc
    | term :: rest ->
      (match eval_query_term_with_sources db sources binding term with
       | Some value -> collect (value :: acc) rest
       | None -> invalid_arg "insufficient aggregate argument bindings")
  in
  collect [] extra_terms

let aggregate_values db sources group_bindings terms =
  let _, value_term = split_aggregate_terms terms in
  List.filter_map
    (fun binding -> eval_query_term_with_sources db sources binding value_term)
    group_bindings

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

let bind_relation_row db bindings vars row =
  if List.length vars <> List.length row then
    invalid_arg "relation input row arity mismatch";
  List.fold_left2
    (fun binding var value ->
      match binding, var with
      | None, _ -> None
      | Some binding, "_" -> Some binding
      | Some binding, _ -> bind_var db var value binding)
    (Some bindings)
    vars
    row

let resolve_query_input_result db = function
  | Result_value value ->
    Option.map (fun _ -> Result_value value) (resolve_query_value db value)
  | result -> Some result

let resolve_query_input_row db row =
  let rec resolve acc = function
    | [] -> Some (List.rev acc)
    | value :: rest ->
      (match resolve_query_input_result db value with
       | Some value -> resolve (value :: acc) rest
       | None -> None)
  in
  resolve [] row

let collection_values_of_input db value =
  match resolve_query_input_result db value with
  | Some (Result_value (List values | Vector values | Set values)) -> Some (List.map (fun value -> Result_value value) values)
  | Some (Result_value (Tuple values)) ->
    Some (values |> List.filter_map (Option.map (fun value -> Result_value value)))
  | Some _ | None -> None

let row_values_of_input db value =
  match resolve_query_input_result db value with
  | Some (Result_value (List values | Vector values | Set values)) -> Some (List.map (fun value -> Result_value value) values)
  | Some (Result_value (Tuple values)) ->
    Some (values |> List.map (function Some value -> Result_value value | None -> Result_value Nil))
  | Some _ | None -> None

let eval_ground_term_tuple db bindings result output_vars =
  match row_values_of_input db result with
  | Some row ->
    (match bind_relation_row db bindings output_vars row with
     | Some bindings -> [ bindings ]
     | None -> [])
  | None -> []

let eval_ground_term_relation db bindings result output_vars =
  match collection_values_of_input db result with
  | Some rows ->
    rows
    |> List.filter_map (fun row ->
      match row_values_of_input db row with
      | Some row -> bind_relation_row db bindings output_vars row
      | None -> None)
  | None -> []

let rec bind_input_binding db input_binding value bindings =
  match input_binding with
  | Bind_scalar var ->
    (match resolve_query_input_result db value with
     | Some value -> List.filter_map (fun binding -> bind_var db var value binding) bindings
     | None -> [])
  | Bind_ignore ->
    (match resolve_query_input_result db value with
     | Some _ -> bindings
     | None -> [])
  | Bind_collection binding ->
    (match collection_values_of_input db value with
     | Some values ->
       List.concat_map (fun value -> bind_input_binding db binding value bindings) values
     | None -> [])
  | Bind_tuple bindings_ ->
    (match row_values_of_input db value with
     | Some row -> bind_nested_input_tuple db bindings_ row bindings
     | None -> [])

and bind_nested_input_tuple db input_bindings row bindings =
  if List.length input_bindings <> List.length row then
    invalid_arg "relation input row arity mismatch";
  List.fold_left2
    (fun bindings input_binding value -> bind_input_binding db input_binding value bindings)
    bindings
    input_bindings
    row

let apply_query_input db bindings = function
  | Input_scalar (var, value) ->
    (match resolve_query_input_result db value with
     | Some value -> List.filter_map (fun binding -> bind_var db var value binding) bindings
     | None -> [])
  | Input_entity_ref (var, entity_ref) ->
    (match entity_id_of_ref db entity_ref with
     | Some entity_id ->
       List.filter_map (fun binding -> bind_var db var (Result_entity entity_id) binding) bindings
     | None -> [])
  | Input_collection (var, values) ->
    let values = List.filter_map (resolve_query_input_result db) values in
    List.concat_map
      (fun binding -> List.filter_map (fun value -> bind_var db var value binding) values)
      bindings
  | Input_collection_ignore values ->
    let _ = List.filter_map (resolve_query_input_result db) values in
    bindings
  | Input_nested_collection (input_binding, values) ->
    List.concat_map (fun value -> bind_input_binding db input_binding value bindings) values
  | Input_tuple (vars, row) ->
    (match resolve_query_input_row db row with
     | Some row -> List.filter_map (fun binding -> bind_relation_row db binding vars row) bindings
     | None -> [])
  | Input_relation (vars, rows) ->
    let rows = List.filter_map (resolve_query_input_row db) rows in
    List.concat_map
      (fun binding -> List.filter_map (bind_relation_row db binding vars) rows)
      bindings
  | Input_nested_tuple (input_bindings, row) -> bind_nested_input_tuple db input_bindings row bindings
  | Input_nested_relation (input_bindings, rows) ->
    List.concat_map (fun row -> bind_nested_input_tuple db input_bindings row bindings) rows
  | Input_predicate _
  | Input_function _ ->
    bindings
  | Input_aggregate _ -> bindings
  | Input_rules _ -> bindings
  | Input_ignore -> bindings
  | Input_scalar_decl _
  | Input_collection_decl _
  | Input_collection_ignore_decl
  | Input_ignore_decl
  | Input_nested_collection_decl _
  | Input_tuple_decl _
  | Input_relation_decl _
  | Input_nested_tuple_decl _
  | Input_nested_relation_decl _ ->
    invalid_arg "query input declarations require supplied input arguments"
  | Input_source_decl _
  | Input_rules_decl ->
    bindings

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

let matching_rules_exn = Query.matching_rules_exn

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

and vars_of_clause clause = Query.vars_of_clause clause

and query_clause_string clause =
  Query.query_clause_string ~value_to_string:edn_string_of_value clause

and query_or_join_clause_string required_vars vars branches =
  Query.query_or_join_clause_string ~value_to_string:edn_string_of_value required_vars vars branches

and ensure_query_terms_bound bindings terms clause_string =
  Query.ensure_query_terms_bound bindings terms clause_string

and ensure_not_has_outer_binding bindings clauses =
  Query.ensure_not_has_outer_binding ~value_to_string:edn_string_of_value bindings clauses

and vars_of_branch clauses =
  Query.vars_of_branch clauses

and ensure_or_branch_vars_match bindings branches =
  Query.ensure_or_branch_vars_match ~value_to_string:edn_string_of_value bindings branches

and ensure_join_vars_bound bindings vars =
  Query.ensure_join_vars_bound bindings vars

and ensure_join_vars_bound_in_clause bindings vars clause_string =
  Query.ensure_join_vars_bound_in_clause bindings vars clause_string

and ensure_or_join_branches_cover_listed_vars bindings vars branches =
  Query.ensure_or_join_branches_cover_listed_vars bindings vars branches

and clause_calls_rule name clause =
  Query.clause_calls_rule name clause

and rule_call_key db source name bindings terms =
  source, name, List.map (eval_query_term db bindings) terms

and matching_rules_for_call active_rules key rules name arity =
  let candidates = matching_rules_exn rules name arity in
  if List.mem key active_rules then
    List.filter (fun rule -> not (List.exists (clause_calls_rule name) rule.rule_body)) candidates
  else
    candidates

and eval_dynamic_query_term db sources bindings = function
  | QSource source -> Some (Result_db (source_db db sources source))
  | term -> eval_query_term db bindings term

and collect_dynamic_query_terms_exn db sources bindings terms =
  let rec collect acc = function
    | [] -> List.rev acc
    | term :: rest ->
      (match eval_dynamic_query_term db sources bindings term with
       | Some value -> collect (value :: acc) rest
       | None -> invalid_arg "unbound query variable")
  in
  collect [] terms

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

let section_forms = function
  | QueryFormVector forms | QueryFormList forms -> forms
  | form -> [ form ]

let query_form_section key entries =
  let values =
    entries
    |> List.filter_map (fun (entry_key, value) ->
      if entry_key = QueryFormKeyword key then Some (section_forms value) else None)
  in
  match values with
  | [] -> None
  | [ forms ] -> Some (QueryFormVector forms)
  | _ -> Some (QueryFormVector (List.concat values))

let query_form_sections forms =
  let finish key values sections =
    match key with
    | None -> sections
    | Some key -> (QueryFormKeyword key, QueryFormVector (List.rev values)) :: sections
  in
  let rec collect key values sections = function
    | [] -> List.rev (finish key values sections)
    | QueryFormKeyword key' :: rest ->
      collect (Some key') [] (finish key values sections) rest
    | form :: rest ->
      (match key with
       | None -> invalid_arg "query vector must start with a keyword section"
       | Some _ -> collect key (form :: values) sections rest)
  in
  collect None [] [] forms

let query_form_map = function
  | QueryFormMap entries -> entries
  | QueryFormVector forms -> query_form_sections forms
  | _ -> invalid_arg "query should be a vector or a map"

let query_form_sequence = function
  | QueryFormVector forms | QueryFormList forms -> Some forms
  | _ -> None

let query_symbol_name symbol =
  if String.length symbol > 1 && symbol.[0] = '?' then
    String.sub symbol 1 (String.length symbol - 1)
  else
    invalid_arg ("expected query variable symbol: " ^ symbol)

let query_callable_name symbol =
  if String.length symbol > 1 && symbol.[0] = '?' then query_symbol_name symbol else symbol

let is_plain_input_symbol symbol =
  String.length symbol > 0
  && symbol <> "_"
  && symbol <> "%"
  && symbol.[0] <> '?'
  && symbol.[0] <> '$'

let is_query_input_symbol symbol =
  (String.length symbol > 1 && symbol.[0] = '?') || is_plain_input_symbol symbol

let query_input_name symbol =
  if String.length symbol > 1 && symbol.[0] = '?' then
    String.sub symbol 1 (String.length symbol - 1)
  else if is_plain_input_symbol symbol then symbol
  else
    invalid_arg ("expected query input symbol: " ^ symbol)

let query_source_name symbol =
  if symbol = "$" then "$"
  else if String.length symbol > 1 && symbol.[0] = '$' then
    String.sub symbol 1 (String.length symbol - 1)
  else
    invalid_arg ("expected query source symbol: " ^ symbol)

let is_query_source_symbol symbol =
  String.length symbol > 0 && symbol.[0] = '$'

let is_plain_rule_symbol symbol =
  String.length symbol > 0
  && symbol <> "_"
  && symbol <> "%"
  && symbol.[0] <> '?'
  && symbol.[0] <> '$'

let aggregate_of_symbol = function
  | "count" -> Some Count
  | "count-distinct" -> Some CountDistinct
  | "distinct" -> Some Distinct
  | "sum" -> Some Sum
  | "avg" -> Some Avg
  | "median" -> Some Median
  | "variance" -> Some Variance
  | "stddev" -> Some Stddev
  | "min" -> Some Min
  | "max" -> Some Max
  | "rand" -> Some Rand
  | _ -> None

let amount_aggregate_of_symbol symbol amount =
  if amount < 0 then invalid_arg (symbol ^ " aggregate amount must be non-negative");
  match symbol with
  | "min" -> Some (MinN amount)
  | "max" -> Some (MaxN amount)
  | "rand" -> Some (RandN amount)
  | "sample" -> Some (Sample amount)
  | _ -> None

let dynamic_amount_aggregate_of_symbol symbol amount_var =
  match symbol with
  | "min" -> Some (MinNVar amount_var)
  | "max" -> Some (MaxNVar amount_var)
  | "rand" -> Some (RandNVar amount_var)
  | "sample" -> Some (SampleVar amount_var)
  | _ -> None

let parse_find_arg = function
  | QueryFormSymbol symbol when is_query_source_symbol symbol -> QSource (query_source_name symbol)
  | QueryFormSymbol symbol -> QVar (query_symbol_name symbol)
  | form -> QValue (query_value_of_form form)

let parse_find_args forms = List.map parse_find_arg forms

let parse_find_form ?(default_pull_db = empty_db ()) ?(pull_db_for_source = fun _ -> empty_db ()) = function
  | QueryFormSymbol symbol -> Find_var (query_symbol_name symbol)
  | form ->
    (match query_form_sequence form with
     | Some [ QueryFormSymbol "pull"; QueryFormSymbol var; QueryFormSymbol pattern_var ]
       when is_query_input_symbol pattern_var && pattern_var <> "*" ->
       Find_pull_var (query_symbol_name var, query_input_name pattern_var)
     | Some [ QueryFormSymbol "pull"; QueryFormSymbol var; pattern ] ->
       Find_pull (query_symbol_name var, parse_pull_pattern default_pull_db pattern)
     | Some [ QueryFormSymbol "pull"; QueryFormSymbol source; QueryFormSymbol var; QueryFormSymbol pattern_var ]
       when is_query_source_symbol source && is_query_input_symbol pattern_var && pattern_var <> "*" ->
       Find_pull_source_var (query_source_name source, query_symbol_name var, query_input_name pattern_var)
     | Some [ QueryFormSymbol "pull"; QueryFormSymbol source; QueryFormSymbol var; pattern ]
       when is_query_source_symbol source ->
       let source_name = query_source_name source in
       Find_pull_source
         (source_name, query_symbol_name var, parse_pull_pattern (pull_db_for_source source_name) pattern)
     | Some [ QueryFormSymbol aggregate; QueryFormSymbol var ] ->
       (match aggregate_of_symbol aggregate with
        | Some aggregate -> Find_aggregate (aggregate, [ QVar (query_symbol_name var) ])
        | None -> invalid_arg "find elements must be variable symbols")
     | Some (QueryFormSymbol "aggregate" :: QueryFormSymbol aggregate_var :: args)
       when String.length aggregate_var > 0 && aggregate_var.[0] = '?' ->
       (match args with
        | [] -> invalid_arg "aggregate custom aggregate requires at least one argument"
        | args -> Find_aggregate (CustomVar (query_symbol_name aggregate_var), parse_find_args args))
     | Some [ QueryFormSymbol aggregate; QueryFormInt amount; QueryFormSymbol var ] ->
       (match amount_aggregate_of_symbol aggregate amount with
        | Some aggregate -> Find_aggregate (aggregate, [ QVar (query_symbol_name var) ])
        | None -> invalid_arg "find elements must be variable symbols")
     | Some [ QueryFormSymbol aggregate; QueryFormSymbol amount_var; QueryFormSymbol var ]
       when String.length amount_var > 0 && amount_var.[0] = '?' ->
       (match dynamic_amount_aggregate_of_symbol aggregate (query_symbol_name amount_var) with
        | Some aggregate -> Find_aggregate (aggregate, [ QVar (query_symbol_name var) ])
        | None -> invalid_arg "find elements must be variable symbols")
     | Some [ QueryFormSymbol aggregate; _; QueryFormSymbol _ ] ->
       (match amount_aggregate_of_symbol aggregate 0 with
        | Some _ -> invalid_arg (aggregate ^ " aggregate amount must be an integer literal")
        | None -> invalid_arg "find elements must be variable symbols")
     | Some (QueryFormSymbol aggregate_name :: args) ->
       (match aggregate_of_symbol aggregate_name with
        | Some aggregate ->
          (match args with
           | [] -> invalid_arg (aggregate_name ^ " aggregate requires at least one argument")
           | args -> Find_aggregate (aggregate, parse_find_args args))
        | None -> invalid_arg "find elements must be variable symbols")
     | Some _ | None -> invalid_arg "find elements must be variable symbols")

let parse_find_relation ?default_pull_db ?pull_db_for_source = function
  | Some (QueryFormVector forms | QueryFormList forms) ->
    List.map (parse_find_form ?default_pull_db ?pull_db_for_source) forms
  | Some _ -> invalid_arg "query :find must be a vector"
  | None -> invalid_arg "query requires :find"

let is_find_form ?default_pull_db ?pull_db_for_source form =
  match parse_find_form ?default_pull_db ?pull_db_for_source form with
  | _ -> true
  | exception Invalid_argument _ -> false

let parse_find_return ?default_pull_db ?pull_db_for_source = function
  | Some (QueryFormVector [ (QueryFormVector [ form; QueryFormSymbol "..." ]
                           | QueryFormList [ form; QueryFormSymbol "..." ]) ])
  | Some (QueryFormList [ (QueryFormVector [ form; QueryFormSymbol "..." ]
                         | QueryFormList [ form; QueryFormSymbol "..." ]) ]) ->
    Return_collection, [ parse_find_form ?default_pull_db ?pull_db_for_source form ]
  | Some (QueryFormVector [ form; QueryFormSymbol "." ])
  | Some (QueryFormList [ form; QueryFormSymbol "." ]) ->
    Return_scalar, [ parse_find_form ?default_pull_db ?pull_db_for_source form ]
  | Some (QueryFormVector [ ((QueryFormVector _ | QueryFormList _) as form) ])
  | Some (QueryFormList [ ((QueryFormVector _ | QueryFormList _) as form) ])
    when not (is_find_form ?default_pull_db ?pull_db_for_source form) ->
    (match form with
     | QueryFormVector forms
     | QueryFormList forms ->
       Return_tuple, List.map (parse_find_form ?default_pull_db ?pull_db_for_source) forms
     | _ -> assert false)
  | form -> Return_relation, parse_find_relation ?default_pull_db ?pull_db_for_source form

let parse_find form = parse_find_return (Some form)

let parse_query_value_form = query_value_of_form

let lookup_ref_of_form = function
  | QueryFormVector [ QueryFormKeyword attr; value ] ->
    Some (attr, query_value_of_form value)
  | _ -> None

let parse_pattern_term
      ?(entity_position = false)
      ?(attr_position = false)
      ?(lookup_ref_position = false)
      ?(source_position = true)
      form =
  match lookup_ref_of_form form with
  | Some (attr, value) when entity_position -> QLookupRef (attr, value)
  | Some (attr, value) when lookup_ref_position -> QValue (Ref_to (Lookup_ref (attr, value)))
  | _ ->
    (match form with
     | QueryFormSymbol "_" -> QWildcard
     | QueryFormSymbol symbol when String.length symbol > 0 && symbol.[0] = '?' ->
       QVar (query_symbol_name symbol)
     | QueryFormSymbol symbol when source_position && is_query_source_symbol symbol ->
       QSource (query_source_name symbol)
     | QueryFormSymbol symbol -> QValue (Symbol symbol)
     | QueryFormInt entity_id when entity_position -> QEntity entity_id
     | QueryFormKeyword attr when attr_position -> QAttr attr
     | QueryFormKeyword value -> QValue (Keyword value)
     | QueryFormInt value -> QValue (Int value)
     | QueryFormFloat value -> QValue (Float value)
     | QueryFormString value -> QValue (String value)
     | QueryFormBool value -> QValue (Bool value)
     | QueryFormNil -> QValue Nil
     | QueryFormVector _ | QueryFormList _ | QueryFormSet _ | QueryFormTagged _ | QueryFormMap _ as form ->
       QValue (parse_query_value_form form))

let comparison_predicate_of_symbol = function
  | "<" -> Some LessThan
  | ">" -> Some GreaterThan
  | "<=" -> Some LessOrEqual
  | ">=" -> Some GreaterOrEqual
  | _ -> None

let value_predicate_of_symbol = function
  | "number?" -> Some NumberValue
  | "integer?" -> Some IntegerValue
  | "string?" -> Some StringValue
  | "boolean?" -> Some BooleanValue
  | "keyword?" -> Some KeywordValue
  | _ -> None

let numeric_predicate_of_symbol = function
  | "zero?" -> Some ZeroNumber
  | "pos?" -> Some PositiveNumber
  | "neg?" -> Some NegativeNumber
  | "even?" -> Some EvenInteger
  | "odd?" -> Some OddInteger
  | _ -> None

let boolean_predicate_of_symbol = function
  | "true?" -> Some TrueValue
  | "false?" -> Some FalseValue
  | "nil?" -> Some NilValue
  | "some?" -> Some SomeValue
  | _ -> None

let unary_string_predicate_clause_of_symbol = function
  | "clojure.string/blank?" -> Some (fun term -> StringBlankValue term)
  | _ -> None

let unary_string_predicate_of_symbol = function
  | "clojure.string/blank?" -> Some string_is_blank
  | _ -> None

let binary_string_predicate_clause_of_symbol = function
  | "clojure.string/includes?" -> Some (fun left right -> StringIncludesValue (left, right))
  | "clojure.string/starts-with?" -> Some (fun left right -> StringStartsWithValue (left, right))
  | "clojure.string/ends-with?" -> Some (fun left right -> StringEndsWithValue (left, right))
  | _ -> None

let binary_string_predicate_of_symbol = function
  | "clojure.string/includes?" -> Some string_includes
  | "clojure.string/starts-with?" -> Some string_starts_with
  | "clojure.string/ends-with?" -> Some string_ends_with
  | _ -> None

let equality_predicate_of_symbol = function
  | "=" | "==" -> Some EqualValues
  | "!=" | "not=" -> Some NotEqualValues
  | _ -> None

let arithmetic_op_of_symbol = function
  | "+" -> Some AddNumbers
  | "-" -> Some SubtractNumbers
  | "*" -> Some MultiplyNumbers
  | "/" -> Some DivideNumbers
  | "quot" -> Some QuotientNumbers
  | "rem" -> Some RemainderNumbers
  | "mod" -> Some ModuloNumbers
  | "inc" -> Some IncrementNumber
  | "dec" -> Some DecrementNumber
  | _ -> None

let query_attr_name = function
  | QueryFormKeyword attr | QueryFormString attr -> attr
  | _ -> invalid_arg "expected query attribute"

let query_results_as_values results =
  let ( let* ) = Option.bind in
  let rec collect acc = function
    | [] -> Some (List.rev acc)
    | result :: rest ->
      let* value = value_of_query_result result in
      collect (value :: acc) rest
  in
  collect [] results

let one_arg_message symbol = "complement " ^ symbol ^ " requires one argument"

let two_arg_message symbol = "complement " ^ symbol ^ " requires two arguments"

let parse_complement_predicate_clause symbol args =
  let terms = List.map parse_pattern_term args in
  let clause predicate = Predicate ("complement " ^ symbol, terms, fun results -> not (predicate results)) in
  let unary_result_predicate predicate =
    match args with
    | [ _ ] -> clause (function [ result ] -> predicate result | _ -> false)
    | _ -> invalid_arg (one_arg_message symbol)
  in
  let unary_value_predicate predicate =
    unary_result_predicate (function Result_value value -> predicate value | _ -> false)
  in
  let binary_string_predicate predicate =
    match args with
    | [ _; _ ] ->
      clause (function
        | [ Result_value (String left); Result_value (String right) ] -> predicate left right
        | _ -> false)
    | _ -> invalid_arg (two_arg_message symbol)
  in
  match
    value_predicate_of_symbol symbol,
    numeric_predicate_of_symbol symbol,
    boolean_predicate_of_symbol symbol,
    unary_string_predicate_of_symbol symbol,
    binary_string_predicate_of_symbol symbol,
    comparison_predicate_of_symbol symbol,
    equality_predicate_of_symbol symbol
  with
  | Some predicate, _, _, _, _, _, _ ->
    unary_value_predicate (matches_value_predicate predicate)
  | _, Some predicate, _, _, _, _, _ ->
    unary_value_predicate (matches_numeric_predicate predicate)
  | _, _, Some predicate, _, _, _, _ ->
    unary_result_predicate (matches_boolean_predicate predicate)
  | _, _, _, Some predicate, _, _, _ ->
    unary_value_predicate (function String value -> predicate value | _ -> false)
  | _, _, _, _, Some predicate, _, _ ->
    binary_string_predicate predicate
  | _, _, _, _, _, Some predicate, _ ->
    (match args with
     | [] -> invalid_arg ("comparison predicate requires at least one argument: " ^ symbol)
     | _ :: _ ->
       clause (fun results ->
         match query_results_as_values results with
         | Some values -> comparison_chain_matches predicate values
         | None -> false))
  | _, _, _, _, _, _, Some predicate ->
    clause (fun results ->
      match query_results_as_values results with
      | None -> false
      | Some values ->
        let equal = all_values_equal values in
        (match predicate with
         | EqualValues -> equal
         | NotEqualValues -> not equal))
  | None, None, None, None, None, None, None ->
    (match symbol, args with
     | ("empty?" | "not-empty" | "not-empty?"), [ _ ] ->
       unary_value_predicate
         (fun value ->
            match symbol with
            | "empty?" -> value_has_count 0 value
            | _ -> value_is_not_empty value)
     | "contains?", [ _; _ ] ->
       clause (function
         | [ Result_value collection; key_result ] ->
           (match value_of_query_result key_result with
            | Some key -> value_contains collection key
            | None -> false)
         | _ -> false)
     | "-differ?", _ ->
       clause (fun results ->
         match query_results_as_values results with
         | None -> false
         | Some values ->
           let left, right = split_at (List.length values / 2) values in
           not (List.length left = List.length right && List.for_all2 values_equal left right))
     | "identical?", [ _; _ ] ->
       clause (fun results ->
         match query_results_as_values results with
         | Some [ left; right ] -> values_equal left right
         | _ -> false)
     | "identical?", _ -> invalid_arg (two_arg_message symbol)
     | ("empty?" | "not-empty" | "not-empty?"), _ -> invalid_arg (one_arg_message symbol)
     | "contains?", _ -> invalid_arg (two_arg_message symbol)
     | _ -> invalid_arg ("unsupported complement predicate: " ^ symbol))

let parse_data_pattern_clause = function
  | [ e; a; v ] ->
    Pattern
      ( parse_pattern_term ~entity_position:true ~source_position:false e
      , parse_pattern_term ~attr_position:true ~source_position:false a
      , parse_pattern_term ~lookup_ref_position:true ~source_position:false v )
  | [ e; a; v; tx ] ->
    PatternTx
      ( parse_pattern_term ~entity_position:true ~source_position:false e
      , parse_pattern_term ~attr_position:true ~source_position:false a
      , parse_pattern_term ~lookup_ref_position:true ~source_position:false v
      , parse_pattern_term ~entity_position:true ~source_position:false tx )
  | [ e; a; v; tx; op ] ->
    PatternTxOp
      ( parse_pattern_term ~entity_position:true ~source_position:false e
      , parse_pattern_term ~attr_position:true ~source_position:false a
      , parse_pattern_term ~lookup_ref_position:true ~source_position:false v
      , parse_pattern_term ~entity_position:true ~source_position:false tx
      , parse_pattern_term ~source_position:false op )
  | [] -> invalid_arg "pattern could not be empty"
  | terms -> SourceRelationPattern ("$", List.map (parse_pattern_term ~source_position:false) terms)

let parse_rule_expr rule_name args =
  match args with
  | [] -> invalid_arg "rule-expr requires at least one argument"
  | args -> rule_name, List.map (parse_pattern_term ~source_position:false) args

let parse_source_pattern_clause source_name terms =
  match terms with
  | [ (QueryFormList (QueryFormSymbol rule_name :: args)
      | QueryFormVector (QueryFormSymbol rule_name :: args))
    ] ->
    let rule_name, args = parse_rule_expr rule_name args in
    SourceRule (source_name, rule_name, args)
  | QueryFormSymbol rule_name :: args when is_plain_rule_symbol rule_name ->
    let rule_name, args = parse_rule_expr rule_name args in
    SourceRule (source_name, rule_name, args)
  | [ e; a; v ] ->
    SourcePattern
      ( source_name
      , parse_pattern_term ~entity_position:true ~source_position:false e
      , parse_pattern_term ~attr_position:true ~source_position:false a
      , parse_pattern_term ~lookup_ref_position:true ~source_position:false v )
  | [ e; a; v; tx ] ->
    SourcePatternTx
      ( source_name
      , parse_pattern_term ~entity_position:true ~source_position:false e
      , parse_pattern_term ~attr_position:true ~source_position:false a
      , parse_pattern_term ~lookup_ref_position:true ~source_position:false v
      , parse_pattern_term ~entity_position:true ~source_position:false tx )
  | [ e; a; v; tx; op ] ->
    SourcePatternTxOp
      ( source_name
      , parse_pattern_term ~entity_position:true ~source_position:false e
      , parse_pattern_term ~attr_position:true ~source_position:false a
      , parse_pattern_term ~lookup_ref_position:true ~source_position:false v
      , parse_pattern_term ~entity_position:true ~source_position:false tx
      , parse_pattern_term ~source_position:false op )
  | [] -> invalid_arg "source pattern could not be empty"
  | terms -> SourceRelationPattern (source_name, List.map (parse_pattern_term ~source_position:false) terms)

let parse_missing_clause = function
  | [ entity; attr ] ->
    Missing (parse_pattern_term ~entity_position:true entity, query_attr_name attr)
  | QueryFormSymbol source :: entity :: attr :: [] when is_query_source_symbol source ->
    SourceMissing
      (query_source_name source, parse_pattern_term ~entity_position:true entity, query_attr_name attr)
  | _ -> invalid_arg "missing? requires an entity and an attribute"

let parse_get_else_clause args output =
  let output_var = query_symbol_name output in
  match args with
  | [ entity; attr; default ] ->
    GetElse
      ( parse_pattern_term ~entity_position:true entity
      , query_attr_name attr
      , query_value_of_form default
      , output_var )
  | QueryFormSymbol source :: entity :: attr :: default :: [] when is_query_source_symbol source ->
    SourceGetElse
      ( query_source_name source
      , parse_pattern_term ~entity_position:true entity
      , query_attr_name attr
      , query_value_of_form default
      , output_var )
  | _ -> invalid_arg "get-else requires an entity, an attribute, a default, and an output"

let parse_two_output_vars = function
  | QueryFormVector [ QueryFormSymbol left; QueryFormSymbol right ]
  | QueryFormList [ QueryFormSymbol left; QueryFormSymbol right ] ->
    query_symbol_name left, query_symbol_name right
  | _ -> invalid_arg "expected two output variables"

let parse_get_some_clause args output =
  let attr_var, value_var = parse_two_output_vars output in
  let build entity attrs =
    match attrs with
    | [] -> invalid_arg "get-some requires at least one attribute"
    | attrs ->
      GetSome
        ( parse_pattern_term ~entity_position:true entity
        , List.map query_attr_name attrs
        , attr_var
        , value_var )
  in
  match args with
  | QueryFormSymbol source :: entity :: attrs when is_query_source_symbol source ->
    (match attrs with
     | [] -> invalid_arg "get-some requires at least one attribute"
     | attrs ->
       SourceGetSome
         ( query_source_name source
         , parse_pattern_term ~entity_position:true entity
         , List.map query_attr_name attrs
         , attr_var
         , value_var ))
  | entity :: attrs -> build entity attrs
  | [] -> invalid_arg "get-some requires an entity and attributes"

let parse_get_clause args output =
  match args with
  | [ map; key ] -> GetValue (parse_pattern_term map, parse_pattern_term key, query_symbol_name output)
  | [ map; key; default ] ->
    GetDefaultValue
      (parse_pattern_term map, parse_pattern_term key, parse_pattern_term default, query_symbol_name output)
  | _ -> invalid_arg "get requires a map, a key, an optional default, and an output"

let parse_core_value_function symbol args output =
  let output_var = query_symbol_name output in
  match symbol, args with
  | "identity", [ term ] -> IdentityValue (parse_pattern_term term, output_var)
  | "and", terms -> BooleanAndValue (List.map parse_pattern_term terms, output_var)
  | "or", terms -> BooleanOrValue (List.map parse_pattern_term terms, output_var)
  | "compare", [ left; right ] -> CompareValue (parse_pattern_term left, parse_pattern_term right, output_var)
  | "compare", _ -> invalid_arg "compare requires two arguments"
  | "min", [] | "max", [] -> invalid_arg (symbol ^ " requires at least one argument")
  | "min", terms -> ExtremumValue (MinimumValue, List.map parse_pattern_term terms, output_var)
  | "max", terms -> ExtremumValue (MaximumValue, List.map parse_pattern_term terms, output_var)
  | "rand", [] -> RandomValue output_var
  | "rand", _ -> invalid_arg "rand requires no arguments"
  | "rand-int", [ bound ] -> RandomIntValue (parse_pattern_term bound, output_var)
  | "rand-int", _ -> invalid_arg "rand-int requires one argument"
  | _ -> DynamicFunction (query_callable_name symbol, List.map parse_pattern_term args, [ output_var ])

let parse_collection_function symbol args output =
  let output_var = query_symbol_name output in
  match symbol, args with
  | "vector", terms -> VectorValue (List.map parse_pattern_term terms, output_var)
  | "list", terms -> ListValue (List.map parse_pattern_term terms, output_var)
  | "set", terms -> SetValue (List.map parse_pattern_term terms, output_var)
  | "hash-map", terms ->
    if List.length terms mod 2 <> 0 then invalid_arg "hash-map requires an even number of arguments";
    HashMapValue (List.map parse_pattern_term terms, output_var)
  | "array-map", terms ->
    if List.length terms mod 2 <> 0 then invalid_arg "array-map requires an even number of arguments";
    ArrayMapValue (List.map parse_pattern_term terms, output_var)
  | "range", [ end_ ] -> RangeEndValue (parse_pattern_term end_, output_var)
  | "range", [ start; end_ ] -> RangeValue (parse_pattern_term start, parse_pattern_term end_, output_var)
  | "range", [ start; end_; step ] ->
    RangeStepValue (parse_pattern_term start, parse_pattern_term end_, parse_pattern_term step, output_var)
  | "range", _ -> invalid_arg "range requires one, two, or three arguments"
  | "tuple", terms -> TupleFunction (List.map parse_pattern_term terms, output_var)
  | _ -> parse_core_value_function symbol args output

let parse_flat_value_function symbol args output_vars =
  match symbol, args with
  | "identity", [ term ] -> GroundTermTuple (parse_pattern_term term, output_vars)
  | _ -> DynamicFunction (query_callable_name symbol, List.map parse_pattern_term args, output_vars)

let parse_output_var = function
  | QueryFormSymbol "_" -> "_"
  | QueryFormSymbol symbol -> query_symbol_name symbol
  | _ -> invalid_arg "expected output variable"

let parse_output_vars = function
  | QueryFormVector forms | QueryFormList forms -> List.map parse_output_var forms
  | QueryFormSymbol symbol -> [ query_symbol_name symbol ]
  | _ -> invalid_arg "expected output variables"

let parse_flat_output_vars = function
  | QueryFormVector forms | QueryFormList forms -> Some (List.map parse_output_var forms)
  | _ -> None

let parse_collection_output_var = function
  | QueryFormVector [ QueryFormSymbol output; QueryFormSymbol "..." ]
  | QueryFormList [ QueryFormSymbol output; QueryFormSymbol "..." ] ->
    Some (query_symbol_name output)
  | _ -> None

let parse_relation_output_vars = function
  | QueryFormVector [ relation_form; QueryFormSymbol "..." ]
  | QueryFormList [ relation_form; QueryFormSymbol "..." ] ->
    (match relation_form with
     | QueryFormSymbol _ -> None
     | form -> parse_flat_output_vars form)
  | _ -> None

let ground_values_of_form = function
  | QueryFormVector values | QueryFormList values -> List.map query_value_of_form values
  | _ -> invalid_arg "ground tuple output requires a vector or list value"

let ground_relation_rows_of_form = function
  | QueryFormVector rows | QueryFormList rows -> List.map ground_values_of_form rows
  | _ -> invalid_arg "ground relation output requires a vector or list value"

let dynamic_ground_term = function
  | QueryFormSymbol symbol
    when (String.length symbol > 0 && symbol.[0] = '?') || is_query_source_symbol symbol ->
    Some (parse_pattern_term (QueryFormSymbol symbol))
  | _ -> None

let parse_ground_function args output =
  match args, output with
  | [ value_form ], QueryFormSymbol output_symbol when Option.is_some (dynamic_ground_term value_form) ->
    GroundTerm (Option.get (dynamic_ground_term value_form), parse_output_var (QueryFormSymbol output_symbol))
  | [ value_form ], QueryFormVector [ QueryFormSymbol output_symbol; QueryFormSymbol "..." ]
  | [ value_form ], QueryFormList [ QueryFormSymbol output_symbol; QueryFormSymbol "..." ]
    when Option.is_some (dynamic_ground_term value_form) ->
    GroundTermCollection (Option.get (dynamic_ground_term value_form), query_symbol_name output_symbol)
  | [ value_form ], QueryFormVector [ (QueryFormVector _ | QueryFormList _ as output_form) ]
  | [ value_form ], QueryFormList [ (QueryFormVector _ | QueryFormList _ as output_form) ]
  | [ value_form ], QueryFormVector [ (QueryFormVector _ | QueryFormList _ as output_form); QueryFormSymbol "..." ]
  | [ value_form ], QueryFormList [ (QueryFormVector _ | QueryFormList _ as output_form); QueryFormSymbol "..." ]
    when Option.is_some (dynamic_ground_term value_form) ->
    GroundTermRelation (Option.get (dynamic_ground_term value_form), parse_output_vars output_form)
  | [ value_form ], (QueryFormVector _ | QueryFormList _ as output_form)
    when Option.is_some (dynamic_ground_term value_form) ->
    GroundTermTuple (Option.get (dynamic_ground_term value_form), parse_output_vars output_form)
  | [ value_form ], QueryFormSymbol output_symbol ->
    Ground (query_value_of_form value_form, parse_output_var (QueryFormSymbol output_symbol))
  | [ value_form ], QueryFormVector [ QueryFormSymbol output_symbol; QueryFormSymbol "..." ]
  | [ value_form ], QueryFormList [ QueryFormSymbol output_symbol; QueryFormSymbol "..." ] ->
    GroundCollection (ground_values_of_form value_form, query_symbol_name output_symbol)
  | [ value_form ], QueryFormVector [ (QueryFormVector _ | QueryFormList _ as output_form) ]
  | [ value_form ], QueryFormList [ (QueryFormVector _ | QueryFormList _ as output_form) ]
  | [ value_form ], QueryFormVector [ (QueryFormVector _ | QueryFormList _ as output_form); QueryFormSymbol "..." ]
  | [ value_form ], QueryFormList [ (QueryFormVector _ | QueryFormList _ as output_form); QueryFormSymbol "..." ] ->
    GroundRelation (ground_relation_rows_of_form value_form, parse_output_vars output_form)
  | [ value_form ], (QueryFormVector _ | QueryFormList _ as output_form) ->
    GroundTuple (ground_values_of_form value_form, parse_output_vars output_form)
  | [ _ ], _ -> invalid_arg "ground output must be a variable or tuple binding"
  | _ -> invalid_arg "ground requires one argument"

let parse_value_metadata_function symbol args output =
  let output_var = query_symbol_name output in
  match symbol, args with
  | "type", [ term ] -> TypeValue (parse_pattern_term term, output_var)
  | "meta", [ term ] -> MetaValue (parse_pattern_term term, output_var)
  | "name", [ term ] -> NameValue (parse_pattern_term term, output_var)
  | "namespace", [ term ] -> NamespaceValue (parse_pattern_term term, output_var)
  | "keyword", [ term ] -> KeywordFromName (parse_pattern_term term, output_var)
  | "keyword", [ namespace; name ] ->
    KeywordFromNamespaceName (parse_pattern_term namespace, parse_pattern_term name, output_var)
  | ("type" | "meta" | "name" | "namespace"), _ -> invalid_arg (symbol ^ " requires one argument")
  | "keyword", _ -> invalid_arg "keyword requires one or two arguments"
  | _ -> parse_collection_function symbol args output

let parse_string_transform_function symbol args output =
  let output_var = query_symbol_name output in
  match symbol, args with
  | "clojure.string/lower-case", [ term ] -> StringLowerCaseValue (parse_pattern_term term, output_var)
  | "clojure.string/upper-case", [ term ] -> StringUpperCaseValue (parse_pattern_term term, output_var)
  | "clojure.string/capitalize", [ term ] -> StringCapitalizeValue (parse_pattern_term term, output_var)
  | "clojure.string/reverse", [ term ] -> StringReverseValue (parse_pattern_term term, output_var)
  | "clojure.string/trim", [ term ] -> StringTrimValue (parse_pattern_term term, output_var)
  | "clojure.string/triml", [ term ] -> StringTrimLeftValue (parse_pattern_term term, output_var)
  | "clojure.string/trimr", [ term ] -> StringTrimRightValue (parse_pattern_term term, output_var)
  | "clojure.string/trim-newline", [ term ] -> StringTrimNewlineValue (parse_pattern_term term, output_var)
  | "clojure.string/index-of", [ value; needle ] ->
    StringIndexOfValue (parse_pattern_term value, parse_pattern_term needle, output_var)
  | "clojure.string/last-index-of", [ value; needle ] ->
    StringLastIndexOfValue (parse_pattern_term value, parse_pattern_term needle, output_var)
  | "str", terms -> StringBuildValue (List.map parse_pattern_term terms, output_var)
  | "print-str", terms -> PrintStringValue (List.map parse_pattern_term terms, output_var)
  | "println-str", terms -> PrintLineStringValue (List.map parse_pattern_term terms, output_var)
  | "pr-str", terms -> PrStringValue (List.map parse_pattern_term terms, output_var)
  | "prn-str", terms -> PrnStringValue (List.map parse_pattern_term terms, output_var)
  | "clojure.string/join", [ collection ] ->
    StringJoinPlainValue (parse_pattern_term collection, output_var)
  | "clojure.string/join", [ separator; collection ] ->
    StringJoinValue (parse_pattern_term separator, parse_pattern_term collection, output_var)
  | "clojure.string/replace", [ value; pattern; replacement ] ->
    StringReplaceValue
      (parse_pattern_term value, parse_pattern_term pattern, parse_pattern_term replacement, output_var)
  | "clojure.string/replace-first", [ value; pattern; replacement ] ->
    StringReplaceFirstValue
      (parse_pattern_term value, parse_pattern_term pattern, parse_pattern_term replacement, output_var)
  | "clojure.string/escape", [ value; replacements ] ->
    StringEscapeValue (parse_pattern_term value, parse_pattern_term replacements, output_var)
  | "re-pattern", [ pattern ] -> RePatternValue (parse_pattern_term pattern, output_var)
  | "re-find", [ pattern; value ] ->
    ReFindValue (parse_pattern_term pattern, parse_pattern_term value, output_var)
  | "re-matches", [ pattern; value ] ->
    ReMatchesValue (parse_pattern_term pattern, parse_pattern_term value, output_var)
  | "re-seq", [ pattern; value ] ->
    ReSeqValue (parse_pattern_term pattern, parse_pattern_term value, output_var)
  | "clojure.string/split", [ value; separator ] ->
    StringSplitValue (parse_pattern_term value, parse_pattern_term separator, output_var)
  | "clojure.string/split", [ value; separator; limit ] ->
    StringSplitLimitValue
      (parse_pattern_term value, parse_pattern_term separator, parse_pattern_term limit, output_var)
  | "clojure.string/split-lines", [ value ] ->
    StringSplitLinesValue (parse_pattern_term value, output_var)
  | "subs", [ value; start ] ->
    StringSubstringValue (parse_pattern_term value, parse_pattern_term start, None, output_var)
  | "subs", [ value; start; end_ ] ->
    StringSubstringValue
      (parse_pattern_term value, parse_pattern_term start, Some (parse_pattern_term end_), output_var)
  | ( "clojure.string/lower-case"
    | "clojure.string/upper-case"
    | "clojure.string/capitalize"
    | "clojure.string/reverse"
    | "clojure.string/trim"
    | "clojure.string/triml"
    | "clojure.string/trimr"
    | "clojure.string/trim-newline" ), _ ->
    invalid_arg (symbol ^ " requires one argument")
  | ("clojure.string/index-of" | "clojure.string/last-index-of"), _ ->
    invalid_arg (symbol ^ " requires two arguments")
  | "clojure.string/join", _ -> invalid_arg "clojure.string/join requires one or two arguments"
  | ("clojure.string/replace" | "clojure.string/replace-first"), _ ->
    invalid_arg (symbol ^ " requires three arguments")
  | "clojure.string/escape", _ -> invalid_arg "clojure.string/escape requires two arguments"
  | "re-pattern", _ -> invalid_arg "re-pattern requires one argument"
  | ("re-find" | "re-matches" | "re-seq"), _ ->
    invalid_arg (symbol ^ " requires two arguments")
  | "clojure.string/split", _ -> invalid_arg "clojure.string/split requires two or three arguments"
  | "clojure.string/split-lines", _ -> invalid_arg "clojure.string/split-lines requires one argument"
  | "subs", _ -> invalid_arg "subs requires two or three arguments"
  | _ -> parse_value_metadata_function symbol args output

let parse_join_vars clause_name = function
  | QueryFormVector vars ->
    (match List.map parse_output_var vars with
     | [] -> invalid_arg "Join variables should not be empty"
     | vars -> vars)
  | _ -> invalid_arg (clause_name ^ " join variables must be a vector")

let parse_rule_var = function
  | QueryFormSymbol "_" -> invalid_arg "rule variables must not be placeholders"
  | QueryFormSymbol symbol ->
    if String.length symbol > 1 && symbol.[0] = '?' then
      query_symbol_name symbol
    else
      invalid_arg ("Cannot parse var, expected symbol starting with ?, got: " ^ symbol)
  | _ -> invalid_arg "expected rule variable"

let ensure_distinct_rule_vars clause_name required free =
  let vars = required @ free in
  (if List.length vars <> List.length (List.sort_uniq compare vars) then
    let message =
      match clause_name with
      | "rule" -> "Rule variables should be distinct"
      | _ -> clause_name ^ " rule variables must be distinct"
    in
    invalid_arg message);
  required, free

let parse_rule_vars clause_name = function
  | QueryFormVector ((QueryFormVector required | QueryFormList required) :: free_forms)
  | QueryFormList ((QueryFormVector required | QueryFormList required) :: free_forms) ->
    let required = List.map parse_rule_var required in
    let free = List.map parse_rule_var free_forms in
    (match required, free with
     | [], [] -> invalid_arg "Cannot parse rule-vars"
     | required, free -> ensure_distinct_rule_vars clause_name required free)
  | QueryFormVector free_forms | QueryFormList free_forms ->
    let free = List.map parse_rule_var free_forms in
    (match free with
     | [] -> invalid_arg "Cannot parse rule-vars"
     | free -> ensure_distinct_rule_vars clause_name [] free)
  | _ -> invalid_arg "Cannot parse rule-vars"

let or_join_clause required_vars vars branches =
  match required_vars with
  | [] -> OrJoin (vars, branches)
  | required_vars -> OrJoinRequired (required_vars, vars, branches)

let source_or_join_clause source required_vars vars branches =
  match required_vars with
  | [] -> SourceOrJoin (source, vars, branches)
  | required_vars -> SourceOrJoinRequired (source, required_vars, vars, branches)

let ensure_inferred_join_vars vars =
  if vars = [] then invalid_arg "Join variables should not be empty"

let rec parse_pattern_clause = function
  | QueryFormVector [ QueryFormList (QueryFormSymbol "missing?" :: args) ] ->
    parse_missing_clause args
  | (QueryFormVector (QueryFormSymbol "not" :: clause_forms)
    | QueryFormList (QueryFormSymbol "not" :: clause_forms)) ->
    (match List.map parse_pattern_clause clause_forms with
     | [] -> invalid_arg "Cannot parse 'not' clause"
     | clauses ->
       ensure_inferred_join_vars (clauses |> List.concat_map vars_of_clause |> List.sort_uniq compare);
       Not clauses)
  | (QueryFormVector (QueryFormSymbol "not-join" :: join_vars :: clause_forms)
    | QueryFormList (QueryFormSymbol "not-join" :: join_vars :: clause_forms)) ->
    let vars = parse_join_vars "not-join" join_vars in
    (match List.map parse_pattern_clause clause_forms with
     | [] -> invalid_arg "Cannot parse 'not-join' clause"
     | clauses -> NotJoin (vars, clauses))
  | (QueryFormVector (QueryFormSymbol "not-join" :: _)
    | QueryFormList (QueryFormSymbol "not-join" :: _)) ->
    invalid_arg "Cannot parse 'not-join' clause"
  | (QueryFormVector (QueryFormSymbol "or" :: branch_forms)
    | QueryFormList (QueryFormSymbol "or" :: branch_forms)) ->
    (match List.map parse_or_branch branch_forms with
     | [] -> invalid_arg "Cannot parse 'or' clause"
     | branches ->
       ensure_inferred_join_vars (branches |> List.concat_map vars_of_branch |> List.sort_uniq compare);
       Or branches)
  | (QueryFormVector (QueryFormSymbol "or-join" :: join_vars :: branch_forms)
    | QueryFormList (QueryFormSymbol "or-join" :: join_vars :: branch_forms)) ->
    let required_vars, vars = parse_rule_vars "or-join" join_vars in
    (match List.map parse_or_branch branch_forms with
     | [] -> invalid_arg "Cannot parse 'or-join' clause"
     | branches -> or_join_clause required_vars vars branches)
  | (QueryFormVector (QueryFormSymbol "or-join" :: _)
    | QueryFormList (QueryFormSymbol "or-join" :: _)) ->
    invalid_arg "Cannot parse 'or-join' clause"
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; (QueryFormVector (QueryFormSymbol "not" :: clause_forms)
        | QueryFormList (QueryFormSymbol "not" :: clause_forms))
      ]
    when is_query_source_symbol source_symbol ->
    (match List.map parse_pattern_clause clause_forms with
     | [] -> invalid_arg "source-qualified not requires at least one clause"
     | clauses -> SourceNot (query_source_name source_symbol, clauses))
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; (QueryFormVector (QueryFormSymbol "not-join" :: join_vars :: clause_forms)
        | QueryFormList (QueryFormSymbol "not-join" :: join_vars :: clause_forms))
      ]
    when is_query_source_symbol source_symbol ->
    let vars = parse_join_vars "source-qualified not-join" join_vars in
    (match List.map parse_pattern_clause clause_forms with
     | [] -> invalid_arg "source-qualified not-join requires at least one clause"
     | clauses -> SourceNotJoin (query_source_name source_symbol, vars, clauses))
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; (QueryFormVector (QueryFormSymbol "not-join" :: _)
        | QueryFormList (QueryFormSymbol "not-join" :: _))
      ]
    when is_query_source_symbol source_symbol ->
    invalid_arg "source-qualified not-join requires join variables and clauses"
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; (QueryFormVector (QueryFormSymbol "or" :: branch_forms)
        | QueryFormList (QueryFormSymbol "or" :: branch_forms))
      ]
    when is_query_source_symbol source_symbol ->
    (match List.map parse_or_branch branch_forms with
     | [] -> invalid_arg "source-qualified or requires at least one branch"
     | branches -> SourceOr (query_source_name source_symbol, branches))
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; (QueryFormVector (QueryFormSymbol "or-join" :: join_vars :: branch_forms)
        | QueryFormList (QueryFormSymbol "or-join" :: join_vars :: branch_forms))
      ]
    when is_query_source_symbol source_symbol ->
    let required_vars, vars = parse_rule_vars "source-qualified or-join" join_vars in
    (match List.map parse_or_branch branch_forms with
     | [] -> invalid_arg "source-qualified or-join requires at least one branch"
     | branches -> source_or_join_clause (query_source_name source_symbol) required_vars vars branches)
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; (QueryFormVector (QueryFormSymbol "or-join" :: _)
        | QueryFormList (QueryFormSymbol "or-join" :: _))
      ]
    when is_query_source_symbol source_symbol ->
    invalid_arg "source-qualified or-join requires join variables and branches"
  | QueryFormList (QueryFormSymbol source_symbol :: QueryFormSymbol "not" :: clause_forms)
    when is_query_source_symbol source_symbol ->
    (match List.map parse_pattern_clause clause_forms with
     | [] -> invalid_arg "source-qualified not requires at least one clause"
     | clauses -> SourceNot (query_source_name source_symbol, clauses))
  | QueryFormList (QueryFormSymbol source_symbol :: QueryFormSymbol "not-join" :: join_vars :: clause_forms)
    when is_query_source_symbol source_symbol ->
    let vars = parse_join_vars "source-qualified not-join" join_vars in
    (match List.map parse_pattern_clause clause_forms with
     | [] -> invalid_arg "source-qualified not-join requires at least one clause"
     | clauses -> SourceNotJoin (query_source_name source_symbol, vars, clauses))
  | QueryFormList (QueryFormSymbol source_symbol :: QueryFormSymbol "not-join" :: _)
    when is_query_source_symbol source_symbol ->
    invalid_arg "source-qualified not-join requires join variables and clauses"
  | QueryFormList (QueryFormSymbol source_symbol :: QueryFormSymbol "or" :: branch_forms)
    when is_query_source_symbol source_symbol ->
    (match List.map parse_or_branch branch_forms with
     | [] -> invalid_arg "source-qualified or requires at least one branch"
     | branches -> SourceOr (query_source_name source_symbol, branches))
  | QueryFormList (QueryFormSymbol source_symbol :: QueryFormSymbol "or-join" :: join_vars :: branch_forms)
    when is_query_source_symbol source_symbol ->
    let required_vars, vars = parse_rule_vars "source-qualified or-join" join_vars in
    (match List.map parse_or_branch branch_forms with
     | [] -> invalid_arg "source-qualified or-join requires at least one branch"
     | branches -> source_or_join_clause (query_source_name source_symbol) required_vars vars branches)
  | QueryFormList (QueryFormSymbol source_symbol :: QueryFormSymbol "or-join" :: _)
    when is_query_source_symbol source_symbol ->
    invalid_arg "source-qualified or-join requires join variables and branches"
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; ((QueryFormVector _ | QueryFormList _) as call)
      ]
    when is_query_source_symbol source_symbol ->
    let source_name = query_source_name source_symbol in
    (match parse_pattern_clause (QueryFormVector [ call ]) with
     | Rule (rule_name, args) -> SourceRule (source_name, rule_name, args)
     | clause -> SourceClause (source_name, clause))
  | QueryFormVector
      [ QueryFormSymbol source_symbol
      ; ((QueryFormVector _ | QueryFormList _) as call)
      ; output
      ]
    when is_query_source_symbol source_symbol ->
    let source_name = query_source_name source_symbol in
    (match parse_pattern_clause (QueryFormVector [ call; output ]) with
     | Rule (rule_name, args) -> SourceRule (source_name, rule_name, args)
     | clause -> SourceClause (source_name, clause))
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol symbol :: args)
        | QueryFormVector (QueryFormSymbol symbol :: args))
      ]
    when String.length symbol > 1 && symbol.[0] = '?' ->
    DynamicPredicate (query_callable_name symbol, List.map parse_pattern_term args)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol symbol :: args)
        | QueryFormVector (QueryFormSymbol symbol :: args))
      ; output
      ]
    when String.length symbol > 1 && symbol.[0] = '?' ->
    (match parse_collection_output_var output with
     | Some output_var ->
       DynamicFunctionCollection (query_callable_name symbol, List.map parse_pattern_term args, output_var)
     | None ->
       (match parse_relation_output_vars output with
        | Some output_vars ->
          DynamicFunctionRelation (query_callable_name symbol, List.map parse_pattern_term args, output_vars)
        | None ->
          DynamicFunction (query_callable_name symbol, List.map parse_pattern_term args, parse_output_vars output)))
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "empty?"; term ]
        | QueryFormVector [ QueryFormSymbol "empty?"; term ])
      ] ->
    EmptyValue (parse_pattern_term term)
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol ("not-empty" | "not-empty?"); term ]
        | QueryFormVector [ QueryFormSymbol ("not-empty" | "not-empty?"); term ])
      ] ->
    NotEmptyValue (parse_pattern_term term)
  | QueryFormVector
      [ (QueryFormList
           (QueryFormSymbol (("empty?" | "not-empty" | "not-empty?") as symbol) :: _)
        | QueryFormVector
            (QueryFormSymbol (("empty?" | "not-empty" | "not-empty?") as symbol) :: _))
      ] ->
    invalid_arg ("predicate requires one argument: " ^ symbol)
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "contains?"; collection; key ]
        | QueryFormVector [ QueryFormSymbol "contains?"; collection; key ])
      ] ->
    ContainsValue (parse_pattern_term collection, parse_pattern_term key)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "get-else" :: args)
        | QueryFormVector (QueryFormSymbol "get-else" :: args))
      ; QueryFormSymbol output
      ] ->
    parse_get_else_clause args output
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "get-some" :: args)
        | QueryFormVector (QueryFormSymbol "get-some" :: args))
      ; output
      ] ->
    parse_get_some_clause args output
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "get" :: args)
        | QueryFormVector (QueryFormSymbol "get" :: args))
      ; QueryFormSymbol output
      ] ->
    parse_get_clause args output
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "count"; term ]
        | QueryFormVector [ QueryFormSymbol "count"; term ])
      ; QueryFormSymbol output
      ] ->
    CountValue (parse_pattern_term term, query_symbol_name output)
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "not"; term ]
        | QueryFormVector [ QueryFormSymbol "not"; term ])
      ; QueryFormSymbol output
      ] ->
    BooleanNotValue (parse_pattern_term term, query_symbol_name output)
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "not"; term ]
        | QueryFormVector [ QueryFormSymbol "not"; term ])
      ] ->
    BooleanNotPredicate (parse_pattern_term term)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "not" :: _)
        | QueryFormVector (QueryFormSymbol "not" :: _))
      ] ->
    invalid_arg "not predicate requires one argument"
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "and" :: terms)
        | QueryFormVector (QueryFormSymbol "and" :: terms))
      ] ->
    BooleanAndPredicate (List.map parse_pattern_term terms)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "or" :: terms)
        | QueryFormVector (QueryFormSymbol "or" :: terms))
      ] ->
    BooleanOrPredicate (List.map parse_pattern_term terms)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "-differ?" :: args)
        | QueryFormVector (QueryFormSymbol "-differ?" :: args))
      ] ->
    DifferPredicate (List.map parse_pattern_term args)
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "identical?"; left; right ]
        | QueryFormVector [ QueryFormSymbol "identical?"; left; right ])
      ] ->
    IdenticalPredicate (parse_pattern_term left, parse_pattern_term right)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "identical?" :: _)
        | QueryFormVector (QueryFormSymbol "identical?" :: _))
      ] ->
    invalid_arg "identical? requires two arguments"
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "untuple"; tuple ]
        | QueryFormVector [ QueryFormSymbol "untuple"; tuple ])
      ; output
      ] ->
    UntupleFunction (parse_pattern_term tuple, parse_output_vars output)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "untuple" :: _)
        | QueryFormVector (QueryFormSymbol "untuple" :: _))
      ; _
      ] ->
    invalid_arg "untuple requires one argument"
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol "ground" :: args)
        | QueryFormVector (QueryFormSymbol "ground" :: args))
      ; output
      ] ->
    parse_ground_function args output
  | QueryFormVector
      [ (QueryFormList
           ((QueryFormList [ QueryFormSymbol "complement"; QueryFormSymbol symbol ]
            | QueryFormVector [ QueryFormSymbol "complement"; QueryFormSymbol symbol ])
            :: args)
        | QueryFormVector
            ((QueryFormList [ QueryFormSymbol "complement"; QueryFormSymbol symbol ]
             | QueryFormVector [ QueryFormSymbol "complement"; QueryFormSymbol symbol ])
             :: args))
      ] ->
    parse_complement_predicate_clause symbol args
  | QueryFormVector
      [ (QueryFormList
           ((QueryFormList (QueryFormSymbol "complement" :: _)
            | QueryFormVector (QueryFormSymbol "complement" :: _))
            :: _)
        | QueryFormVector
            ((QueryFormList (QueryFormSymbol "complement" :: _)
             | QueryFormVector (QueryFormSymbol "complement" :: _))
             :: _))
      ] ->
    invalid_arg "complement requires one predicate symbol"
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "re-find"; pattern; value ]
        | QueryFormVector [ QueryFormSymbol "re-find"; pattern; value ])
      ] ->
    ReFindPredicate (parse_pattern_term pattern, parse_pattern_term value)
  | QueryFormVector
      [ (QueryFormList [ QueryFormSymbol "re-matches"; pattern; value ]
        | QueryFormVector [ QueryFormSymbol "re-matches"; pattern; value ])
      ] ->
    ReMatchesPredicate (parse_pattern_term pattern, parse_pattern_term value)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol symbol :: args)
        | QueryFormVector (QueryFormSymbol symbol :: args))
      ] ->
    (match
       value_predicate_of_symbol symbol,
       numeric_predicate_of_symbol symbol,
       boolean_predicate_of_symbol symbol,
       unary_string_predicate_clause_of_symbol symbol,
       binary_string_predicate_clause_of_symbol symbol,
       comparison_predicate_of_symbol symbol,
       equality_predicate_of_symbol symbol,
       args
     with
     | Some predicate, _, _, _, _, _, _, [ term ] ->
       ValuePredicate (predicate, parse_pattern_term term)
     | _, Some predicate, _, _, _, _, _, [ term ] ->
       NumericPredicate (predicate, parse_pattern_term term)
     | _, _, Some predicate, _, _, _, _, [ term ] ->
       BooleanPredicate (predicate, parse_pattern_term term)
     | _, _, _, Some clause, _, _, _, [ term ] ->
       clause (parse_pattern_term term)
     | _, _, _, _, Some clause, _, _, [ left; right ] ->
       clause (parse_pattern_term left) (parse_pattern_term right)
     | _, _, _, _, _, Some predicate, _, [ left; right ] ->
       ComparisonPredicate (predicate, parse_pattern_term left, parse_pattern_term right)
     | _, _, _, _, _, Some predicate, _, _ :: _ ->
       ComparisonPredicateN (predicate, List.map parse_pattern_term args)
     | _, _, _, _, _, Some _, _, [] ->
       invalid_arg ("comparison predicate requires at least one argument: " ^ symbol)
     | _, _, _, _, _, _, Some predicate, _ ->
       EqualityPredicate (predicate, List.map parse_pattern_term args)
     | Some _, _, _, _, _, _, _, _
     | _, Some _, _, _, _, _, _, _
     | _, _, Some _, _, _, _, _, _
     | _, _, _, Some _, _, _, _, _
     | _, _, _, _, Some _, _, _, _ ->
       invalid_arg ("predicate requires one argument: " ^ symbol)
     | None, None, None, None, None, None, None, _ ->
       DynamicPredicate (query_callable_name symbol, List.map parse_pattern_term args))
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol symbol :: args)
        | QueryFormVector (QueryFormSymbol symbol :: args))
      ; QueryFormSymbol output
      ] ->
    (match arithmetic_op_of_symbol symbol with
     | Some op -> ArithmeticValue (op, List.map parse_pattern_term args, query_symbol_name output)
     | None -> parse_string_transform_function symbol args output)
  | QueryFormVector
      [ (QueryFormList (QueryFormSymbol symbol :: args)
        | QueryFormVector (QueryFormSymbol symbol :: args))
      ; (QueryFormVector _ | QueryFormList _ as output)
      ] ->
    parse_flat_value_function symbol args (parse_output_vars output)
  | (QueryFormVector (QueryFormSymbol rule_name :: args)
    | QueryFormList (QueryFormSymbol rule_name :: args))
    when is_plain_rule_symbol rule_name ->
    let rule_name, args = parse_rule_expr rule_name args in
    Rule (rule_name, args)
  | QueryFormList (QueryFormSymbol source_symbol :: terms) when is_query_source_symbol source_symbol ->
    parse_source_pattern_clause (query_source_name source_symbol) terms
  | QueryFormVector (QueryFormSymbol source_symbol :: terms) when is_query_source_symbol source_symbol ->
    parse_source_pattern_clause (query_source_name source_symbol) terms
  | QueryFormVector terms -> parse_data_pattern_clause terms
  | _ -> invalid_arg "where clauses must be vectors"

and parse_or_branch = function
  | (QueryFormVector (QueryFormSymbol "and" :: clause_forms)
    | QueryFormList (QueryFormSymbol "and" :: clause_forms)) ->
    (match List.map parse_pattern_clause clause_forms with
     | [] -> invalid_arg "or branch requires at least one clause"
     | clauses -> clauses)
  | form -> [ parse_pattern_clause form ]

let parse_rule_head = function
  | QueryFormVector (QueryFormSymbol rule_name :: params)
  | QueryFormList (QueryFormSymbol rule_name :: params) ->
    let required_vars, free_vars = parse_rule_vars "rule" (QueryFormVector params) in
    let params = required_vars @ free_vars in
    rule_name, params
  | _ -> invalid_arg "rule head must be a vector or list"

let parse_rule = function
  | (QueryFormVector (head :: body_forms) | QueryFormList (head :: body_forms)) ->
    let rule_name, rule_params = parse_rule_head head in
    (match List.map parse_pattern_clause body_forms with
     | [] -> invalid_arg "Rule branch should have clauses"
     | rule_body -> { rule_name; rule_params; rule_body })
  | _ -> invalid_arg "rules must be vectors or lists"

let validate_rule_arities rules =
  List.iter
    (fun rule ->
      rules
      |> List.iter (fun other ->
        if rule.rule_name = other.rule_name
           && List.length rule.rule_params <> List.length other.rule_params
        then
          invalid_arg "Arity mismatch"))
    rules;
  rules

let is_rule_head = function
  | QueryFormVector (QueryFormSymbol _ :: _)
  | QueryFormList (QueryFormSymbol _ :: _) -> true
  | _ -> false

let is_rule_form = function
  | QueryFormVector (head :: _)
  | QueryFormList (head :: _) -> is_rule_head head
  | _ -> false

let unwrap_extra_rules_nesting = function
  | [ (QueryFormVector inner | QueryFormList inner) ] when List.for_all is_rule_form inner -> inner
  | rules -> rules

let parse_rules = function
  | Some (QueryFormVector rules | QueryFormList rules) ->
    unwrap_extra_rules_nesting rules |> List.map parse_rule |> validate_rule_arities
  | Some _ -> invalid_arg "query :rules must be a vector or list"
  | None -> []

let nonempty_input_vars binding_name vars =
  match vars with
  | [] -> invalid_arg (binding_name ^ " :in binding requires at least one variable")
  | vars -> vars

let input_relation_vars = function
  | QueryFormVector vars | QueryFormList vars -> Some vars
  | _ -> None

let input_var_of_form = function
  | QueryFormSymbol "_" -> Some "_"
  | QueryFormSymbol symbol when String.length symbol > 1 && symbol.[0] = '?' ->
    Some (query_symbol_name symbol)
  | _ -> None

let flat_input_vars forms =
  let rec collect acc = function
    | [] -> Some (List.rev acc)
    | form :: rest ->
      (match input_var_of_form form with
       | Some var -> collect (var :: acc) rest
       | None -> None)
  in
  collect [] forms

let rec parse_nested_input_binding form =
  match form with
  | QueryFormSymbol "_" -> Bind_ignore
  | QueryFormSymbol symbol -> Bind_scalar (query_symbol_name symbol)
  | _ ->
    (match query_form_sequence form with
     | Some [ binding_form; QueryFormSymbol "..." ] ->
       Bind_collection (parse_nested_input_binding binding_form)
     | Some [ (QueryFormVector _ | QueryFormList _ as relation_form) ] ->
       Bind_collection (parse_nested_input_binding relation_form)
     | Some forms ->
       (match List.map parse_nested_tuple_binding forms with
        | [] -> invalid_arg "tuple :in binding requires at least one variable"
        | bindings -> Bind_tuple bindings)
     | None -> invalid_arg "cannot parse :in binding")

and parse_nested_tuple_binding = function
  | QueryFormSymbol "_" -> Bind_ignore
  | form -> parse_nested_input_binding form

let parse_binding = parse_nested_input_binding

let nested_relation_binding = function
  | QueryFormVector forms | QueryFormList forms ->
    (match flat_input_vars forms with
     | Some _ -> None
     | None ->
       (match parse_nested_input_binding (QueryFormVector forms) with
        | Bind_tuple bindings -> Some bindings
        | _ -> None))
  | _ -> None

let parse_input_binding = function
  | QueryFormSymbol "%" -> Some Input_rules_decl
  | QueryFormSymbol "_" -> Some Input_ignore_decl
  | QueryFormSymbol symbol when is_query_source_symbol symbol ->
    Some (Input_source_decl (query_source_name symbol))
  | QueryFormSymbol symbol -> Some (Input_scalar_decl (query_input_name symbol))
  | form ->
    (match query_form_sequence form with
     | Some [ QueryFormSymbol "_"; QueryFormSymbol "..." ] ->
       Some Input_collection_ignore_decl
     | Some [ QueryFormSymbol symbol; QueryFormSymbol "..." ] ->
       Some (Input_collection_decl (query_symbol_name symbol))
     | Some [ relation_form; QueryFormSymbol "..." ] ->
       (match input_relation_vars relation_form with
        | Some vars ->
          (match flat_input_vars vars with
           | Some vars -> Some (Input_relation_decl (vars |> nonempty_input_vars "relation"))
           | None ->
             (match nested_relation_binding relation_form with
              | Some bindings -> Some (Input_nested_relation_decl bindings)
              | None -> Some (Input_nested_collection_decl (parse_nested_input_binding relation_form))))
        | None -> Some (Input_nested_collection_decl (parse_nested_input_binding relation_form)))
     | Some [ relation_form ] ->
       (match input_relation_vars relation_form with
        | Some vars ->
          (match flat_input_vars vars with
           | Some vars -> Some (Input_relation_decl (vars |> nonempty_input_vars "relation"))
           | None ->
             (match nested_relation_binding relation_form with
              | Some bindings -> Some (Input_nested_relation_decl bindings)
              | None -> invalid_arg "cannot parse :in binding"))
        | None -> Some (Input_tuple_decl ([ relation_form ] |> List.map parse_output_var |> nonempty_input_vars "tuple")))
     | Some vars ->
       (match flat_input_vars vars with
        | Some vars -> Some (Input_tuple_decl (vars |> nonempty_input_vars "tuple"))
        | None ->
          (match parse_nested_input_binding form with
           | Bind_tuple bindings -> Some (Input_nested_tuple_decl bindings)
           | _ -> invalid_arg "cannot parse :in binding"))
     | None -> invalid_arg "cannot parse :in binding")

let parse_inputs = function
  | Some (QueryFormVector inputs | QueryFormList inputs) -> List.filter_map parse_input_binding inputs
  | Some _ -> invalid_arg "query :in must be a vector or list"
  | None -> []

let parse_in form = parse_inputs (Some form)

let input_declares_rules_var = function
  | Some (QueryFormVector inputs) | Some (QueryFormList inputs) ->
    List.exists (function QueryFormSymbol "%" -> true | _ -> false) inputs
  | Some _ | None -> false

let ensure_distinct_input_rules_var = function
  | Some (QueryFormVector inputs) | Some (QueryFormList inputs) ->
    let count =
      List.fold_left
        (fun count -> function
          | QueryFormSymbol "%" -> count + 1
          | _ -> count)
        0
        inputs
    in
    if count > 1 then invalid_arg "Vars used in :in should be distinct"
  | Some _ | None -> ()

let parse_with_var = function
  | QueryFormSymbol "_" -> invalid_arg "Cannot parse :with clause"
  | QueryFormSymbol symbol -> query_symbol_name symbol
  | _ -> invalid_arg "Cannot parse :with clause"

let parse_with_section = function
  | Some (QueryFormVector vars | QueryFormList vars) -> List.map parse_with_var vars
  | Some _ -> invalid_arg "query :with must be a vector or list"
  | None -> []

let parse_with form = parse_with_section (Some form)

let vars_of_find_spec = function
  | Find_var var | Find_pull (var, _) | Find_pull_source (_, var, _) ->
    [ var ]
  | Find_aggregate (aggregate, terms) ->
    query_term_vars terms @ aggregate_param_vars aggregate @ aggregate_callable_vars aggregate
  | Find_pull_var (var, pattern_var) | Find_pull_source_var (_, var, pattern_var) ->
    [ var; pattern_var ]

let rec vars_of_input_binding = function
  | Bind_scalar var -> [ var ]
  | Bind_ignore -> []
  | Bind_collection binding -> vars_of_input_binding binding
  | Bind_tuple bindings -> bindings |> List.concat_map vars_of_input_binding

let vars_of_input = function
  | Input_scalar (var, _)
  | Input_entity_ref (var, _)
  | Input_collection (var, _)
  | Input_predicate (var, _)
  | Input_function (var, _)
  | Input_aggregate (var, _)
  | Input_scalar_decl var
  | Input_collection_decl var ->
    [ var ]
  | Input_collection_ignore _
  | Input_rules _
  | Input_ignore
  | Input_collection_ignore_decl
  | Input_ignore_decl
  | Input_rules_decl ->
    []
  | Input_nested_collection (binding, _)
  | Input_nested_collection_decl binding ->
    vars_of_input_binding binding
  | Input_tuple (vars, _)
  | Input_relation (vars, _)
  | Input_tuple_decl vars
  | Input_relation_decl vars ->
    List.filter (( <> ) "_") vars
  | Input_nested_tuple (bindings, _)
  | Input_nested_relation (bindings, _)
  | Input_nested_tuple_decl bindings ->
    bindings |> List.concat_map vars_of_input_binding
  | Input_nested_relation_decl bindings -> bindings |> List.concat_map vars_of_input_binding
  | Input_source_decl _ -> []

let source_of_input = function
  | Input_source_decl source -> Some source
  | Input_scalar _
  | Input_entity_ref _
  | Input_collection _
  | Input_collection_ignore _
  | Input_ignore
  | Input_nested_collection _
  | Input_tuple _
  | Input_relation _
  | Input_nested_tuple _
  | Input_nested_relation _
  | Input_predicate _
  | Input_function _
  | Input_aggregate _
  | Input_rules _
  | Input_scalar_decl _
  | Input_collection_decl _
  | Input_collection_ignore_decl
  | Input_ignore_decl
  | Input_rules_decl
  | Input_nested_collection_decl _
  | Input_tuple_decl _
  | Input_relation_decl _
  | Input_nested_tuple_decl _
  | Input_nested_relation_decl _ ->
    None

let named_source source = [ source ]

let sources_of_query_term = function
  | QSource source -> [ source ]
  | QEntity _ | QIdent _ | QLookupRef _ | QVar _ | QAttr _ | QValue _ | QWildcard -> []

let sources_of_query_terms terms =
  List.concat_map sources_of_query_term terms

let sources_of_optional_query_term = function
  | Some term -> sources_of_query_term term
  | None -> []

let rec sources_of_clause = function
  | Pattern (e, a, v) -> sources_of_query_terms [ e; a; v ]
  | PatternTx (e, a, v, tx) -> sources_of_query_terms [ e; a; v; tx ]
  | PatternTxOp (e, a, v, tx, op) -> sources_of_query_terms [ e; a; v; tx; op ]
  | SourcePattern (source, e, a, v) -> named_source source @ sources_of_query_terms [ e; a; v ]
  | SourcePatternTx (source, e, a, v, tx) ->
    named_source source @ sources_of_query_terms [ e; a; v; tx ]
  | SourcePatternTxOp (source, e, a, v, tx, op) ->
    named_source source @ sources_of_query_terms [ e; a; v; tx; op ]
  | SourceRelationPattern (source, terms) -> named_source source @ sources_of_query_terms terms
  | Missing (entity, _) -> sources_of_query_term entity
  | SourceMissing (source, entity, _) -> named_source source @ sources_of_query_term entity
  | GetElse (entity, _, _, _) -> sources_of_query_term entity
  | SourceGetElse (source, entity, _, _, _) -> named_source source @ sources_of_query_term entity
  | GetSome (entity, _, _, _) -> sources_of_query_term entity
  | SourceGetSome (source, entity, _, _, _) -> named_source source @ sources_of_query_term entity
  | GetValue (map, key, _) -> sources_of_query_terms [ map; key ]
  | GetDefaultValue (map, key, default, _) -> sources_of_query_terms [ map; key; default ]
  | CountValue (term, _)
  | EmptyValue term
  | NotEmptyValue term
  | ValuePredicate (_, term)
  | NumericPredicate (_, term)
  | BooleanPredicate (_, term)
  | BooleanNotPredicate term
  | BooleanNotValue (term, _)
  | IdentityValue (term, _)
  | RandomIntValue (term, _)
  | TypeValue (term, _)
  | MetaValue (term, _)
  | NameValue (term, _)
  | NamespaceValue (term, _)
  | KeywordFromName (term, _)
  | StringLowerCaseValue (term, _)
  | StringUpperCaseValue (term, _)
  | StringCapitalizeValue (term, _)
  | StringReverseValue (term, _)
  | StringTrimValue (term, _)
  | StringTrimLeftValue (term, _)
  | StringTrimRightValue (term, _)
  | StringTrimNewlineValue (term, _)
  | StringJoinPlainValue (term, _)
  | RePatternValue (term, _)
  | StringBlankValue term
  | StringSplitLinesValue (term, _)
  | GroundTerm (term, _)
  | GroundTermCollection (term, _)
  | GroundTermTuple (term, _)
  | GroundTermRelation (term, _)
  | RangeEndValue (term, _)
  | UntupleFunction (term, _) ->
    sources_of_query_term term
  | ContainsValue (collection, key)
  | ComparisonPredicate (_, collection, key)
  | CompareValue (collection, key, _)
  | IdenticalPredicate (collection, key)
  | KeywordFromNamespaceName (collection, key, _)
  | StringIncludesValue (collection, key)
  | StringStartsWithValue (collection, key)
  | StringEndsWithValue (collection, key)
  | StringIndexOfValue (collection, key, _)
  | StringLastIndexOfValue (collection, key, _)
  | StringJoinValue (collection, key, _)
  | StringEscapeValue (collection, key, _)
  | ReFindValue (collection, key, _)
  | ReMatchesValue (collection, key, _)
  | ReFindPredicate (collection, key)
  | ReMatchesPredicate (collection, key)
  | ReSeqValue (collection, key, _)
  | StringSplitValue (collection, key, _)
  | RangeValue (collection, key, _) ->
    sources_of_query_terms [ collection; key ]
  | StringSubstringValue (value, start, end_, _) ->
    sources_of_query_terms [ value; start ] @ sources_of_optional_query_term end_
  | StringReplaceValue (value, pattern, replacement, _)
  | StringReplaceFirstValue (value, pattern, replacement, _)
  | StringSplitLimitValue (value, pattern, replacement, _)
  | RangeStepValue (value, pattern, replacement, _) ->
    sources_of_query_terms [ value; pattern; replacement ]
  | ComparisonPredicateN (_, terms)
  | EqualityPredicate (_, terms)
  | ArithmeticValue (_, terms, _)
  | ExtremumValue (_, terms, _)
  | BooleanAndPredicate terms
  | BooleanAndValue (terms, _)
  | BooleanOrPredicate terms
  | BooleanOrValue (terms, _)
  | DifferPredicate terms
  | StringBuildValue (terms, _)
  | PrintStringValue (terms, _)
  | PrintLineStringValue (terms, _)
  | PrStringValue (terms, _)
  | PrnStringValue (terms, _)
  | VectorValue (terms, _)
  | ListValue (terms, _)
  | SetValue (terms, _)
  | HashMapValue (terms, _)
  | ArrayMapValue (terms, _)
  | TupleFunction (terms, _)
  | Predicate (_, terms, _)
  | Function (_, terms, _, _)
  | DynamicPredicate (_, terms)
  | DynamicFunction (_, terms, _)
  | DynamicFunctionCollection (_, terms, _)
  | DynamicFunctionRelation (_, terms, _)
  | Rule (_, terms)
  | SourceRule (_, _, terms) ->
    sources_of_query_terms terms
  | SourceClause (source, clause) -> named_source source @ sources_of_clause clause
  | SourceNot (source, clauses) | SourceNotJoin (source, _, clauses) ->
    named_source source @ List.concat_map sources_of_clause clauses
  | SourceOr (source, branches)
  | SourceOrJoin (source, _, branches)
  | SourceOrJoinRequired (source, _, _, branches) ->
    named_source source @ List.concat_map (List.concat_map sources_of_clause) branches
  | Not clauses | NotJoin (_, clauses) -> List.concat_map sources_of_clause clauses
  | Or branches | OrJoin (_, branches) | OrJoinRequired (_, _, branches) ->
    List.concat_map (List.concat_map sources_of_clause) branches
  | RandomValue _
  | Ground _
  | GroundCollection _
  | GroundTuple _
  | GroundRelation _ ->
    []

let rec has_rule_clause = function
  | Rule _ | SourceRule _ -> true
  | SourceClause (_, clause) -> has_rule_clause clause
  | Not clauses | SourceNot (_, clauses) | NotJoin (_, clauses) | SourceNotJoin (_, _, clauses) ->
    List.exists has_rule_clause clauses
  | Or branches | SourceOr (_, branches) | OrJoin (_, branches) | SourceOrJoin (_, _, branches) ->
    List.exists (List.exists has_rule_clause) branches
  | OrJoinRequired (_, _, branches) | SourceOrJoinRequired (_, _, _, branches) ->
    List.exists (List.exists has_rule_clause) branches
  | Pattern _
  | PatternTx _
  | PatternTxOp _
  | SourcePattern _
  | SourcePatternTx _
  | SourcePatternTxOp _
  | SourceRelationPattern _
  | Missing _
  | SourceMissing _
  | GetElse _
  | SourceGetElse _
  | GetSome _
  | SourceGetSome _
  | GetValue _
  | GetDefaultValue _
  | CountValue _
  | EmptyValue _
  | NotEmptyValue _
  | ContainsValue _
  | ValuePredicate _
  | NumericPredicate _
  | ComparisonPredicate _
  | ComparisonPredicateN _
  | EqualityPredicate _
  | ArithmeticValue _
  | CompareValue _
  | ExtremumValue _
  | BooleanPredicate _
  | BooleanNotPredicate _
  | BooleanNotValue _
  | IdentityValue _
  | BooleanAndPredicate _
  | BooleanAndValue _
  | BooleanOrPredicate _
  | BooleanOrValue _
  | RandomValue _
  | RandomIntValue _
  | DifferPredicate _
  | IdenticalPredicate _
  | TypeValue _
  | MetaValue _
  | NameValue _
  | NamespaceValue _
  | KeywordFromName _
  | KeywordFromNamespaceName _
  | StringIncludesValue _
  | StringStartsWithValue _
  | StringEndsWithValue _
  | StringLowerCaseValue _
  | StringUpperCaseValue _
  | StringCapitalizeValue _
  | StringReverseValue _
  | StringTrimValue _
  | StringTrimLeftValue _
  | StringTrimRightValue _
  | StringTrimNewlineValue _
  | StringIndexOfValue _
  | StringLastIndexOfValue _
  | StringSubstringValue _
  | StringBuildValue _
  | PrintStringValue _
  | PrintLineStringValue _
  | PrStringValue _
  | PrnStringValue _
  | StringJoinPlainValue _
  | StringJoinValue _
  | StringReplaceValue _
  | StringReplaceFirstValue _
  | StringEscapeValue _
  | RePatternValue _
  | ReFindValue _
  | ReMatchesValue _
  | ReSeqValue _
  | ReFindPredicate _
  | ReMatchesPredicate _
  | StringBlankValue _
  | StringSplitValue _
  | StringSplitLimitValue _
  | StringSplitLinesValue _
  | Ground _
  | GroundCollection _
  | GroundTuple _
  | GroundRelation _
  | GroundTerm _
  | GroundTermCollection _
  | GroundTermTuple _
  | GroundTermRelation _
  | VectorValue _
  | ListValue _
  | SetValue _
  | HashMapValue _
  | ArrayMapValue _
  | RangeEndValue _
  | RangeValue _
  | RangeStepValue _
  | TupleFunction _
  | UntupleFunction _
  | Predicate _
  | Function _
  | DynamicPredicate _
  | DynamicFunction _
  | DynamicFunctionCollection _
  | DynamicFunctionRelation _ ->
    false

let rule_names rules =
  rules |> List.map (fun rule -> rule.rule_name) |> List.sort_uniq compare

let rec resolve_dynamic_rule_clause names = function
  | DynamicPredicate (name, terms) when List.mem name names -> Rule (name, terms)
  | SourceClause (source, DynamicPredicate (name, terms)) when List.mem name names ->
    SourceRule (source, name, terms)
  | SourceClause (source, clause) -> SourceClause (source, resolve_dynamic_rule_clause names clause)
  | Not clauses -> Not (List.map (resolve_dynamic_rule_clause names) clauses)
  | SourceNot (source, clauses) -> SourceNot (source, List.map (resolve_dynamic_rule_clause names) clauses)
  | NotJoin (vars, clauses) -> NotJoin (vars, List.map (resolve_dynamic_rule_clause names) clauses)
  | SourceNotJoin (source, vars, clauses) ->
    SourceNotJoin (source, vars, List.map (resolve_dynamic_rule_clause names) clauses)
  | Or branches -> Or (List.map (List.map (resolve_dynamic_rule_clause names)) branches)
  | SourceOr (source, branches) -> SourceOr (source, List.map (List.map (resolve_dynamic_rule_clause names)) branches)
  | OrJoin (vars, branches) -> OrJoin (vars, List.map (List.map (resolve_dynamic_rule_clause names)) branches)
  | SourceOrJoin (source, vars, branches) ->
    SourceOrJoin (source, vars, List.map (List.map (resolve_dynamic_rule_clause names)) branches)
  | OrJoinRequired (required_vars, vars, branches) ->
    OrJoinRequired (required_vars, vars, List.map (List.map (resolve_dynamic_rule_clause names)) branches)
  | SourceOrJoinRequired (source, required_vars, vars, branches) ->
    SourceOrJoinRequired
      (source, required_vars, vars, List.map (List.map (resolve_dynamic_rule_clause names)) branches)
  | clause -> clause

let resolve_dynamic_rule names rule =
  { rule with rule_body = List.map (resolve_dynamic_rule_clause names) rule.rule_body }

let sources_of_find_spec = function
  | Find_pull_source (source, _, _) | Find_pull_source_var (source, _, _) -> named_source source
  | Find_aggregate (_, terms) -> sources_of_query_terms terms
  | Find_var _ | Find_pull _ | Find_pull_var _ -> []

let find_spec_uses_default_source = function
  | Find_pull_source (source, _, _) | Find_pull_source_var (source, _, _) -> source = "$"
  | Find_var _ | Find_pull _ | Find_pull_var _ | Find_aggregate _ -> false

let rec clause_uses_default_source = function
  | SourceClause (source, clause) -> source = "$" || clause_uses_default_source clause
  | SourcePattern (source, _, _, _)
  | SourcePatternTx (source, _, _, _, _)
  | SourcePatternTxOp (source, _, _, _, _, _)
  | SourceRelationPattern (source, _)
  | SourceMissing (source, _, _)
  | SourceGetElse (source, _, _, _, _)
  | SourceGetSome (source, _, _, _, _)
  | SourceRule (source, _, _) ->
    source = "$"
  | SourceNot (source, clauses)
  | SourceNotJoin (source, _, clauses) ->
    source = "$" || List.exists clause_uses_default_source clauses
  | SourceOr (source, branches)
  | SourceOrJoin (source, _, branches)
  | SourceOrJoinRequired (source, _, _, branches) ->
    source = "$" || List.exists (List.exists clause_uses_default_source) branches
  | Not clauses | NotJoin (_, clauses) -> List.exists clause_uses_default_source clauses
  | Or branches | OrJoin (_, branches) | OrJoinRequired (_, _, branches) ->
    List.exists (List.exists clause_uses_default_source) branches
  | Pattern _
  | PatternTx _
  | PatternTxOp _ ->
    true
  | Missing _
  | GetElse _
  | GetSome _
  | GetValue _
  | GetDefaultValue _
  | CountValue _
  | EmptyValue _
  | NotEmptyValue _
  | ContainsValue _
  | ValuePredicate _
  | NumericPredicate _
  | ComparisonPredicate _
  | ComparisonPredicateN _
  | EqualityPredicate _
  | ArithmeticValue _
  | CompareValue _
  | ExtremumValue _
  | BooleanPredicate _
  | BooleanNotPredicate _
  | BooleanNotValue _
  | IdentityValue _
  | BooleanAndPredicate _
  | BooleanAndValue _
  | BooleanOrPredicate _
  | BooleanOrValue _
  | RandomValue _
  | RandomIntValue _
  | DifferPredicate _
  | IdenticalPredicate _
  | TypeValue _
  | MetaValue _
  | NameValue _
  | NamespaceValue _
  | KeywordFromName _
  | KeywordFromNamespaceName _
  | StringIncludesValue _
  | StringStartsWithValue _
  | StringEndsWithValue _
  | StringLowerCaseValue _
  | StringUpperCaseValue _
  | StringCapitalizeValue _
  | StringReverseValue _
  | StringTrimValue _
  | StringTrimLeftValue _
  | StringTrimRightValue _
  | StringTrimNewlineValue _
  | StringIndexOfValue _
  | StringLastIndexOfValue _
  | StringSubstringValue _
  | StringBuildValue _
  | PrintStringValue _
  | PrintLineStringValue _
  | PrStringValue _
  | PrnStringValue _
  | StringJoinPlainValue _
  | StringJoinValue _
  | StringReplaceValue _
  | StringReplaceFirstValue _
  | StringEscapeValue _
  | RePatternValue _
  | ReFindValue _
  | ReMatchesValue _
  | ReSeqValue _
  | ReFindPredicate _
  | ReMatchesPredicate _
  | StringBlankValue _
  | StringSplitValue _
  | StringSplitLimitValue _
  | StringSplitLinesValue _
  | Ground _
  | GroundCollection _
  | GroundTuple _
  | GroundRelation _
  | GroundTerm _
  | GroundTermCollection _
  | GroundTermTuple _
  | GroundTermRelation _
  | VectorValue _
  | ListValue _
  | SetValue _
  | HashMapValue _
  | ArrayMapValue _
  | RangeEndValue _
  | RangeValue _
  | RangeStepValue _
  | TupleFunction _
  | UntupleFunction _
  | Predicate _
  | Function _
  | DynamicPredicate _
  | DynamicFunction _
  | DynamicFunctionCollection _
  | DynamicFunctionRelation _
  | Rule _ ->
    false

let infer_default_inputs in_form find where inputs =
  match in_form with
  | Some _ -> inputs
  | None ->
    if List.exists find_spec_uses_default_source find || List.exists clause_uses_default_source where
    then Input_source_decl "$" :: inputs
    else inputs

let ensure_distinct_input_vars inputs =
  let vars = List.concat_map vars_of_input inputs in
  if List.length vars <> List.length (List.sort_uniq compare vars) then
    invalid_arg "Vars used in :in should be distinct"

let ensure_distinct_input_sources inputs =
  let sources = List.filter_map source_of_input inputs in
  if List.length sources <> List.length (List.sort_uniq compare sources) then
    invalid_arg "Vars used in :in should be distinct"

let format_query_vars vars =
  vars
  |> List.map (fun var -> "?" ^ var)
  |> String.concat " "
  |> Printf.sprintf "[%s]"

let format_source_vars sources =
  sources
  |> List.map (fun source -> if source = "$" then "$" else "$" ^ source)
  |> String.concat " "
  |> Printf.sprintf "[%s]"

let validate_query query =
  ensure_distinct_input_vars query.inputs;
  ensure_distinct_input_sources query.inputs;
  if List.length query.with_vars <> List.length (List.sort_uniq compare query.with_vars) then
    invalid_arg "Vars used in :with should be distinct";
  let declared_sources = List.filter_map source_of_input query.inputs |> List.sort_uniq compare in
  let used_sources =
    List.concat_map sources_of_find_spec query.find @ List.concat_map sources_of_clause query.where
    |> List.sort_uniq compare
  in
  let unknown_sources = List.filter (fun source -> not (List.mem source declared_sources)) used_sources in
  let available_vars =
    List.concat_map vars_of_input query.inputs
    @ List.concat_map vars_of_clause query.where
    |> List.sort_uniq compare
  in
  let unknown_find_vars =
    query.find
    |> List.concat_map vars_of_find_spec
    |> List.sort_uniq compare
    |> List.filter (fun var -> not (List.mem var available_vars))
  in
  (match unknown_find_vars with
   | [] -> query
   | _ :: _ -> invalid_arg ("Query for unknown vars: " ^ format_query_vars unknown_find_vars))
  |> fun query ->
  let unknown_with_vars =
    query.with_vars |> List.filter (fun var -> not (List.mem var available_vars))
  in
  (match unknown_with_vars with
   | [] -> query
   | _ :: _ -> invalid_arg ("Query for unknown vars: " ^ format_query_vars unknown_with_vars))
  |> fun query ->
  let find_vars = List.concat_map vars_of_find_spec query.find |> List.sort_uniq compare in
  let shared_vars = List.filter (fun var -> List.mem var find_vars) query.with_vars in
  match shared_vars with
  | [] ->
    (match unknown_sources with
     | [] -> query
     | _ :: _ -> invalid_arg ("Where uses unknown source vars: " ^ format_source_vars unknown_sources))
  | _ :: _ -> invalid_arg (":find and :with should not use same variables: " ^ format_query_vars shared_vars)

let parse_where = function
  | Some (QueryFormVector clauses | QueryFormList clauses) -> List.map parse_pattern_clause clauses
  | Some _ -> invalid_arg "query :where must be a vector or list"
  | None -> []

let parse_query_return_with_pull_context ?default_pull_db ?pull_db_for_source form =
  let entries = query_form_map form in
  let return, find =
    parse_find_return ?default_pull_db ?pull_db_for_source (query_form_section "find" entries)
  in
  let in_form = query_form_section "in" entries in
  ensure_distinct_input_rules_var in_form;
  let rules = parse_rules (query_form_section "rules" entries) in
  let rule_names = rule_names rules in
  let rules = List.map (resolve_dynamic_rule rule_names) rules in
  let where =
    parse_where (query_form_section "where" entries)
    |> List.map (resolve_dynamic_rule_clause rule_names)
  in
  let inputs = parse_inputs in_form |> infer_default_inputs in_form find where in
  let query =
    { find
    ; inputs
    ; with_vars = parse_with_section (query_form_section "with" entries)
    ; rules
    ; where
    }
    |> validate_query
  in
  if query.rules = []
     && List.exists has_rule_clause query.where
     && not (input_declares_rules_var in_form)
  then invalid_arg "Missing rules var '%' in :in";
  return, query

let parse_query_return form =
  parse_query_return_with_pull_context form

let parse_return_map_labels section_name = function
  | QueryFormVector labels | QueryFormList labels ->
    (match labels with
     | [] -> invalid_arg (":" ^ section_name ^ " requires at least one label")
     | labels ->
       List.map
         (function
           | QueryFormSymbol label -> label
           | _ -> invalid_arg (":" ^ section_name ^ " labels must be symbols"))
         labels)
  | _ -> invalid_arg (":" ^ section_name ^ " must be a vector or list")

let parse_return_map_section entries =
  let sections =
    [ "keys", (fun labels -> Return_keys labels)
    ; "syms", (fun labels -> Return_syms labels)
    ; "strs", (fun labels -> Return_strs labels)
    ]
    |> List.filter_map (fun (section_name, make_return_map) ->
      query_form_section section_name entries
      |> Option.map (fun section -> make_return_map (parse_return_map_labels section_name section)))
  in
  match sections with
  | [] -> None
  | [ section ] -> Some section
  | _ -> invalid_arg "Only one of :keys/:syms/:strs must be present"

let return_map_label_count = function
  | Return_keys labels | Return_syms labels | Return_strs labels -> List.length labels

let return_map_name = function
  | Return_keys _ -> "keys"
  | Return_syms _ -> "syms"
  | Return_strs _ -> "strs"

let validate_query_return_map return return_map query =
  match return_map with
  | None -> None
  | Some return_map ->
    (match return with
     | Return_collection ->
       invalid_arg (":" ^ return_map_name return_map ^ " does not work with collection :find")
     | Return_scalar ->
       invalid_arg (":" ^ return_map_name return_map ^ " does not work with single-scalar :find")
     | Return_relation | Return_tuple ->
       if return_map_label_count return_map <> List.length query.find then
         invalid_arg ("Count of :" ^ return_map_name return_map ^ " must match count of :find");
       Some return_map)

let parse_query_return_map_with_pull_context ?default_pull_db ?pull_db_for_source form =
  let entries = query_form_map form in
  let return, query =
    parse_query_return_with_pull_context ?default_pull_db ?pull_db_for_source form
  in
  let return_map = parse_return_map_section entries in
  return, validate_query_return_map return return_map query, query

let parse_query_return_map form =
  parse_query_return_map_with_pull_context form

let parse_query form =
  snd (parse_query_return form)

let parse_query_with_pull_context ?default_pull_db ?pull_db_for_source form =
  snd (parse_query_return_with_pull_context ?default_pull_db ?pull_db_for_source form)

let parse_query_string input =
  parse_query (read_edn input)

let parse_query_string_with_pull_context ?default_pull_db ?pull_db_for_source input =
  parse_query_with_pull_context ?default_pull_db ?pull_db_for_source (read_edn input)

let parse_query_return_string input =
  parse_query_return (read_edn input)

let parse_query_return_string_with_pull_context ?default_pull_db ?pull_db_for_source input =
  parse_query_return_with_pull_context ?default_pull_db ?pull_db_for_source (read_edn input)

let parse_query_return_map_string input =
  parse_query_return_map (read_edn input)

let parse_query_return_map_string_with_pull_context ?default_pull_db ?pull_db_for_source input =
  parse_query_return_map_with_pull_context ?default_pull_db ?pull_db_for_source (read_edn input)

module Pull_parser = struct
  let parse_pattern = parse_pull_pattern
  let parse_pattern_string = parse_pull_pattern_string
end

module Parser = struct
  let read_edn = read_edn
  let parse_binding = parse_binding
  let parse_in = parse_in
  let parse_with = parse_with
  let parse_find = parse_find
  let parse_clause = parse_pattern_clause
  let parse_rules form = parse_rules (Some form)
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
  let query_callables_of_inputs = Query_impl.query_callables_of_inputs
  let query_rules_of_inputs = Query_impl.query_rules_of_inputs
  let matching_rules = Query_impl.matching_rules
  let matching_rules_exn = Query_impl.matching_rules_exn
  let project_binding = Query_impl.project_binding
  let rule_invocation_callables = Query_impl.rule_invocation_callables
  let vars_of_query_term = Query_impl.vars_of_query_term
  let vars_of_query_terms = Query_impl.vars_of_query_terms
  let vars_of_clause = Query_impl.vars_of_clause
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
  let query_input_binding_string = Query_impl.query_input_binding_string
  let query_input_decl_binding_string = Query_impl.query_input_decl_binding_string
  let query_input_binding_label = Query_impl.query_input_binding_label
  let query_input_consumes_argument = Query_impl.query_input_consumes_argument
  let values_of_collection_result = Query_impl.values_of_collection_result
  let row_of_collection_result = Query_impl.row_of_collection_result
  let row_of_scalar_sequence = Query_impl.row_of_scalar_sequence
  let rows_of_map_entries = Query_impl.rows_of_map_entries
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
