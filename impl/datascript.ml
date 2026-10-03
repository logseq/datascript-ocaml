include Datascript_types

module Built_ins = Built_ins
module Db_impl = Db

type conn = Conn.t

let tx0 = Db_impl.tx0
let max_allocatable_entity_id = Db_impl.max_allocatable_entity_id

let next_db_uid =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter

let db_core_context : Db_impl.core_context =
  { next_db_uid
  }

let refresh_db_identity db = Db_impl.refresh_identity db_core_context db

module Lru = Lru
module Lookup_refs = Lookup_refs
module Serialize = Serialize
module Util = Util
module Upsert = Upsert
module PSet = Persistent_sorted_set

let validate_entity_id = Db_impl.validate_entity_id

let datom ?tx ?added e a v = Db_impl.datom ?tx ?added ~e ~a ~v ()

let is_datom = Db_impl.is_datom

let validate_schema = Schema.validate_schema

let is_db (_ : db) = true

let max_eid_in_value = Db_impl.max_eid_in_value

let split_keyword = Util.split_keyword
let compare_value = Util.compare_value
let first_nonzero = Util.first_nonzero
let normalize_value = Util.normalize_value

let normalize_datom_for_schema = Db_impl.normalize_datom_for_schema

let refresh_db_indexes = Db_impl.refresh_indexes
let refresh_db_indexes_with_added_datoms = Db_impl.refresh_indexes_with_added_datoms

(* Shared per-transaction memo (see Transact.tx_memo); populated through the
   memo-aware refresh_db_indexes_with_tx_data below and consulted by
   add_active_datom_with_report_db.  Only read while apply_tx has it active. *)
let transact_tx_memo : Transact.tx_memo =
  { active = false; base_max_eid = 0L; datoms = Hashtbl.create 8 }

let tx_memo_apply tx_data =
  if transact_tx_memo.active then
    List.iter
      (fun d ->
        if d.e > transact_tx_memo.base_max_eid
           && d.e <= Db_impl.max_allocatable_entity_id
        then
          let key = d.e, d.a in
          let bucket =
            Option.value (Hashtbl.find_opt transact_tx_memo.datoms key)
              ~default:[]
          in
          let bucket =
            if d.added then d :: bucket
            else
              List.filter (fun x -> Util.compare_value x.v d.v <> 0) bucket
          in
          Hashtbl.replace transact_tx_memo.datoms key bucket)
      tx_data

let refresh_db_indexes_with_tx_data db tx_data =
  let db = Db_impl.refresh_indexes_with_tx_data db tx_data in
  tx_memo_apply tx_data;
  db

let empty_db ?(schema = []) ?storage () =
  Db_impl.empty_db db_core_context ~schema ?storage ()

let empty db = Db_impl.empty db_core_context db

let init_db ?(schema = []) ?storage datoms =
  Db_impl.init_db db_core_context ~schema ?storage datoms

let visible_datoms = Db_impl.visible_datoms

let is_filtered = Db_impl.is_filtered

let unfiltered_db db = Db_impl.unfiltered db_core_context db

let filter db pred =
  Db_impl.filter db_core_context db pred

let serializable = Serialize.serializable

let serialize_context : Serialize.context =
  { next_db_uid
  ; validate_schema
  ; normalize_datom_for_schema
  ; refresh_db_indexes
  }

let from_serializable snapshot =
  Serialize.from_serializable serialize_context snapshot

let store ?storage db =
  Storage.store ?storage db

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

let ident_attr = Schema_access.ident_attr
let schema_attr = Schema_access.schema_attr
let cardinality = Schema_access.cardinality
let is_unique_identity = Schema_access.is_unique_identity
let is_unique = Schema_access.is_unique
let tuple_attrs = Schema_access.tuple_attrs
let is_tuple_attr = Schema_access.is_tuple_attr
let is_indexed = Schema_access.is_indexed
let is_component = Schema_access.is_component
let is_ref_attr = Schema_access.is_ref_attr
let tuple_attrs_for_source = Schema_access.tuple_attrs_for_source

let is_reverse_ref = Schema.is_reverse_ref

let reverse_ref = Schema.reverse_ref

let value_equal = Db_impl.value_equal

let same_fact = Db_impl.same_fact

module Transact_datoms_impl = Transact_datoms.Make (struct
  let schema_attr = schema_attr
  let cardinality = cardinality
  let is_unique = is_unique
  let tuple_attrs = tuple_attrs
  let is_tuple_attr = is_tuple_attr
  let is_component = is_component
  let is_ref_attr = is_ref_attr
  let tuple_attrs_for_source = tuple_attrs_for_source
  let is_reverse_ref = is_reverse_ref
  let reverse_ref = reverse_ref
  let value_equal = value_equal
  let same_fact = same_fact
  let datom = Db_impl.datom
  let normalize_value = normalize_value
  let validate_entity_id = validate_entity_id
  let max_allocatable_entity_id = max_allocatable_entity_id
  let visible_datoms = visible_datoms
end)

let normalize_entity_attr_value = Transact_datoms_impl.normalize_entity_attr_value
let allocate_entity_id = Transact_datoms_impl.allocate_entity_id
let entid_in_datoms = Transact_datoms_impl.entid_in_datoms

let rec edn_string_of_value = function
  | Nil -> "nil"
  | Bool value -> if value then "true" else "false"
  | Int64 value -> Int64.to_string value
  | Float value -> string_of_float value
  | String value -> "\"" ^ String.escaped value ^ "\""
  | Keyword value -> ":" ^ value
  | Symbol value -> value
  | Uuid value -> "#uuid \"" ^ value ^ "\""
  | Instant millis -> "#inst \"" ^ Util.string_of_instant_millis millis ^ "\""
  | Regex value -> "#\"" ^ String.escaped value ^ "\""
  | Ref entity_id -> Int64.to_string entity_id
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
  | Entity_id entity_id -> Int64.to_string entity_id
  | Temp_id tempid -> tempid
  | CurrentTx -> ":db/current-tx"
  | Ident ident -> ":" ^ ident
  | Lookup_ref (attr, value) -> "[:" ^ attr ^ " " ^ edn_string_of_value value ^ "]"

let cas_expected_value_string = function
  | None -> "nil"
  | Some value -> edn_string_of_value value

let lookup_refs_context : Lookup_refs.context =
  { is_unique
  ; entid_in_datoms
  ; visible_datoms
  ; value_to_string = edn_string_of_value
  }

let unresolved_lookup_ref_message attr value =
  Lookup_refs.unresolved_message lookup_refs_context attr value

let unresolved_entity_ref_message = function
  | Lookup_ref (attr, value) -> unresolved_lookup_ref_message attr value
  | _ -> "lookup ref did not resolve"

let find_avet_exact db attr value =
  let bound = Db_impl.datom ~e:0L ~a:attr ~v:value () in
  let compare_prefix left right =
    let cmp = Util.compare_attr left.a right.a in
    if cmp <> 0 then cmp else compare_value left.v right.v
  in
  let cmp left right =
    if right == bound then compare_prefix left right
    else if left == bound then -compare_prefix right left
    else Util.compare_datom Avet left right
  in
  match
    PSet.slice ~from_:bound ~to_:bound ~cmp db.avet_index
    @ List.filter
        (fun datom -> datom.a = attr && value_equal datom.v value)
        (Option.value (Hashtbl.find_opt db.duplicate_avet_by_attr attr) ~default:[])
    |> List.sort (Util.compare_datom Avet)
  with
  | datom :: _ -> Some datom
  | [] -> None

let find_eavt_exact db entity_id attr value =
  let bound = Db_impl.datom ~e:entity_id ~a:attr ~v:value () in
  let compare_prefix left right =
    let cmp = compare left.e right.e in
    if cmp <> 0 then cmp
    else
      let cmp = Util.compare_attr left.a right.a in
      if cmp <> 0 then cmp else compare_value left.v right.v
  in
  let cmp left right =
    if right == bound then compare_prefix left right
    else if left == bound then -compare_prefix right left
    else Util.compare_datom Eavt left right
  in
  match
    PSet.slice ~from_:bound ~to_:bound ~cmp db.eavt_index
    @ List.filter
        (fun datom -> datom.e = entity_id && datom.a = attr && value_equal datom.v value)
        (Option.value (Hashtbl.find_opt db.duplicate_eavt_by_entity entity_id) ~default:[])
    |> List.sort (Util.compare_datom Eavt)
  with
  | datom :: _ -> Some datom
  | [] -> None

let rec coerce_tuple_lookup_value_db db attr value =
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
      | Int64 entity_id when is_ref_attr db source_attr ->
        Some (Ref (validate_entity_id entity_id))
      | (List [ lookup_attr; lookup_value ] | Vector [ lookup_attr; lookup_value ]) when is_ref_attr db source_attr ->
        (match Option.bind (lookup_attr_name lookup_attr) (fun attr -> entid_db db attr lookup_value) with
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
      | Some (Int64 entity_id) when is_ref_attr db source_attr ->
        Some (Ref (validate_entity_id entity_id))
      | Some ((List [ lookup_attr; lookup_value ] | Vector [ lookup_attr; lookup_value ]) as lookup_ref) when is_ref_attr db source_attr ->
        (match Option.bind (lookup_attr_name lookup_attr) (fun attr -> entid_db db attr lookup_value) with
         | Some entity_id -> Some (Ref entity_id)
         | None -> Some (normalize_value lookup_ref))
      | Some value -> Some (normalize_value value)
    in
    Tuple (List.map2 coerce_component source_attrs values)
  | _ -> normalize_value value

and entid_db db attr value =
  let value = coerce_tuple_lookup_value_db db attr value in
  if is_unique db attr then
    find_avet_exact db attr value |> Option.map (fun datom -> datom.e)
  else
    None

let lookup_ref_entity_id_db ?(strict_missing = false) db attr value =
  if not (is_unique db attr) then
    invalid_arg (Lookup_refs.non_unique_message lookup_refs_context attr value);
  match entid_db db attr value with
  | Some entity_id -> Some entity_id
  | None ->
    if strict_missing then
      invalid_arg (unresolved_lookup_ref_message attr value)
    else
      None

let entid = entid_db

module Transact_impl = Transact

let transact_resolve_context : Transact_impl.context =
  { validate_entity_id
  ; entid = entid_db
  ; ident_attr
  ; allocate_entity_id
  ; lookup_ref_entity_id =
      (fun ~strict_missing db attr value -> lookup_ref_entity_id_db ~strict_missing db attr value)
  ; unresolved_lookup_ref_message
  ; normalize_value
  ; is_ref_attr
  ; is_reverse_ref
  ; reverse_ref
  ; cardinality
  ; is_unique_identity
  ; max_eid_with_entity_id = Db_impl.max_eid_with_entity_id
  ; max_eid_in_value
  }

let entity_ref_of_ref_attr_value = Transact_impl.entity_ref_of_ref_attr_value
let ref_attr_for_value_resolution db attr =
  Transact_impl.ref_attr_for_value_resolution transact_resolve_context db attr
let resolve_value_for_attr db attr datoms tx max_eid tempids value =
  Transact_impl.resolve_value_for_attr transact_resolve_context db attr datoms tx max_eid tempids value

module Db_access_impl = Db_access.Make (struct
  let is_ref_attr = is_ref_attr
  let is_unique = is_unique
  let is_indexed = is_indexed
  let entid = entid_db
  let ident_attr = ident_attr
  let lookup_ref_entity_id ?strict_missing db attr value = lookup_ref_entity_id_db ?strict_missing db attr value
  let normalize_value = normalize_value
  let unresolved_entity_ref_message = unresolved_entity_ref_message
  let ref_attr_for_value_resolution = ref_attr_for_value_resolution
  let entity_ref_of_ref_attr_value = entity_ref_of_ref_attr_value
  let compare_value = compare_value
  let first_nonzero = first_nonzero
  let validate_entity_id = validate_entity_id
end)

let entity_attr_datoms_db db e a =
  Db_access_impl.datoms db Eavt ~e ~a () |> List.of_seq

let current_attr_value_db db e a =
  Db_access_impl.find_datom db Eavt ~e ~a () |> Option.map (fun datom -> datom.v)

let value_option_equal left right =
  match left, right with
  | None, None -> true
  | Some left, Some right -> value_equal left right
  | None, Some _ | Some _, None -> false

let tuple_direct_write_matches_sources_db schema_db db d =
  match tuple_attrs schema_db d.a, d.v with
  | Some source_attrs, Tuple values ->
    List.length source_attrs = List.length values
    && List.for_all Option.is_some values
    && List.for_all2
         (fun source_attr value ->
           value_option_equal (current_attr_value_db db d.e source_attr) value)
         source_attrs
         values
  | _ -> false

let validate_datom_value db d =
  if d.v = Nil then invalid_arg "Cannot store nil as a value";
  let value_matches_type value value_type =
    match value_type, value with
    | RefType, Ref _ -> true
    | TupleType, Tuple _ -> true
    | StringType, String _ -> true
    | KeywordType, Keyword _ -> true
    | NumberType, (Int64 _ | Float _) -> true
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
     | Int64 _ | Float _ -> ()
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

let retraction_datom tx datom = { datom with tx; added = false }

let compare_eavt_datom = Util.compare_datom Eavt

let sorted_retractions tx datoms =
  datoms |> List.sort compare_eavt_datom |> List.map (retraction_datom tx)

let add_active_datom_with_report_db ?(allow_tuple = false) ?(validate_value = true) schema_db tx db d =
  let d = { d with v = normalize_value d.v } in
  if is_tuple_attr schema_db d.a && not allow_tuple then
    if tuple_direct_write_matches_sources_db schema_db db d then db, []
    else invalid_arg "cannot modify tuple attributes directly"
  else begin
    if validate_value then validate_datom_value schema_db d;
    (if is_unique schema_db d.a then
       match find_avet_exact db d.a d.v with
       | Some existing when existing.e <> d.e ->
         invalid_arg "unique constraint"
       | Some _ | None -> ());
    (* tx-fresh entities (e allocated inside this transaction) cannot exist
       in the pre-tx index, so their same-fact and entity/attr lookups are
       answered from the tx memo instead of descending the index trees. *)
    (* Transaction entities live above max_allocatable_entity_id and can
       already carry index datoms from earlier transactions, so they are
       never memo-fresh. *)
    let tx_fresh_bucket =
      if transact_tx_memo.active && d.e > transact_tx_memo.base_max_eid
         && d.e <= Db_impl.max_allocatable_entity_id
      then
        Some
          (Option.value
             (Hashtbl.find_opt transact_tx_memo.datoms (d.e, d.a))
             ~default:[])
      else None
    in
    let same_fact_exists =
      match tx_fresh_bucket with
      | Some bucket ->
        List.exists (fun existing -> compare_value existing.v d.v = 0) bucket
      | None -> find_eavt_exact db d.e d.a d.v |> Option.is_some
    in
    if same_fact_exists then
      db, []
    else
      let tx_data =
        match cardinality schema_db d.a with
        | Many -> [ d ]
        | One ->
          let existing =
            match tx_fresh_bucket with
            | Some bucket -> bucket
            | None -> entity_attr_datoms_db db d.e d.a
          in
          sorted_retractions tx existing @ [ d ]
      in
      (* Index decisions use schema_db (the current schema, which may have been
         updated by schema datoms earlier in this transaction); the working db
         is also synced so subsequent datoms see the same schema. *)
      refresh_db_indexes_with_tx_data { db with schema = schema_db.schema } tx_data, tx_data
  end

let retract_active_datom_with_report_db tx db e a value =
  let value = Option.map normalize_value value in
  let removed =
    match value with
    | Some value ->
      find_eavt_exact db e a value |> Option.to_list
    | None -> entity_attr_datoms_db db e a
  in
  let tx_data = sorted_retractions tx removed in
  refresh_db_indexes_with_tx_data db tx_data, tx_data

let ref_value_id = function
  | Ref entity_id -> Some entity_id
  | _ -> None

let rec component_entity_closure_db schema_db db seen e =
  if List.mem e seen then
    seen
  else
    let seen = e :: seen in
    Db_access_impl.datoms db Eavt ~e ()
    |> Seq.filter (fun datom -> is_component schema_db datom.a)
    |> Seq.fold_left
         (fun seen datom ->
           match ref_value_id datom.v with
           | Some child -> component_entity_closure_db schema_db db seen child
           | None -> seen)
         seen

let ref_attrs db =
  db.schema
  |> List.filter_map (fun (attr, spec) ->
    match spec.value_type with
    | Some RefType -> Some attr
    | _ -> None)

let incoming_ref_datoms db ids =
  ref_attrs db
  |> List.concat_map (fun attr ->
    ids
    |> List.concat_map (fun entity_id ->
      Db_access_impl.datoms db Avet ~a:attr ~v:(Ref entity_id) () |> List.of_seq))

let unique_datoms datoms =
  datoms |> List.sort_uniq (Util.compare_datom Eavt)

let retract_entities_with_report_db tx db ids =
  let entity_datoms =
    ids
    |> List.concat_map (fun entity_id ->
      Db_access_impl.datoms db Eavt ~e:entity_id () |> List.of_seq)
  in
  let removed = unique_datoms (entity_datoms @ incoming_ref_datoms db ids) in
  let tx_data = sorted_retractions tx removed in
  refresh_db_indexes_with_tx_data db tx_data, tx_data

let retract_entity_with_report_db schema_db tx db e =
  let ids = component_entity_closure_db schema_db db [] e in
  retract_entities_with_report_db tx db ids

let component_child_closure_db schema_db db component_datoms =
  List.fold_left
    (fun ids datom ->
      match ref_value_id datom.v with
      | Some child -> component_entity_closure_db schema_db db ids child
      | None -> ids)
    []
    component_datoms

let rec retract_user_attr_with_report_db schema_db tx db e a value =
  if is_tuple_attr schema_db a then invalid_arg "cannot modify tuple attributes directly";
  let db, tx_data =
    match value with
    | Some value -> retract_active_datom_with_report_db tx db e a (Some value)
    | None when is_component schema_db a ->
      let attr_datoms = entity_attr_datoms_db db e a in
      let child_ids = component_child_closure_db schema_db db attr_datoms in
      let db, child_tx_data = retract_entities_with_report_db tx db child_ids in
      let db, attr_tx_data = retract_active_datom_with_report_db tx db e a None in
      db, child_tx_data @ attr_tx_data
    | None -> retract_active_datom_with_report_db tx db e a None
  in
  refresh_tuple_attrs_for_source_db schema_db tx db e a tx_data

and tuple_value_db db e source_attrs =
  Tuple (List.map (current_attr_value_db db e) source_attrs)

and refresh_tuple_attrs_for_source_db schema_db tx db e source_attr tx_data =
  tuple_attrs_for_source schema_db source_attr
  |> List.fold_left
       (fun (db, tx_data) (tuple_attr, source_attrs) ->
         let datom = Db_impl.datom ~tx ~e ~a:tuple_attr ~v:(tuple_value_db db e source_attrs) () in
         let db, tuple_tx_data =
           add_active_datom_with_report_db ~allow_tuple:true schema_db tx db datom
         in
         db, tx_data @ tuple_tx_data)
       (db, tx_data)

let add_user_datom_with_report_db schema_db tx db d =
  let db, tx_data = add_active_datom_with_report_db schema_db tx db d in
  refresh_tuple_attrs_for_source_db schema_db tx db d.e d.a tx_data

let add_entity_attr_value_db schema_db tx db e attr value =
  let e, attr, value = normalize_entity_attr_value schema_db e attr value in
  add_user_datom_with_report_db schema_db tx db (Db_impl.datom ~tx ~e ~a:attr ~v:value ())

let compare_and_set_matches_db db e a expected =
  match cardinality db a, expected with
  | Many, Some expected ->
    entity_attr_datoms_db db e a |> List.exists (fun datom -> value_equal datom.v expected)
  | Many, None -> entity_attr_datoms_db db e a = []
  | One, Some expected ->
    (match current_attr_value_db db e a with
     | Some actual -> value_equal actual expected
     | None -> false)
  | One, None -> current_attr_value_db db e a = None

let cas_current_value_string_db db e a =
  match cardinality db a with
  | Many ->
    let values =
      entity_attr_datoms_db db e a
      |> List.map (fun d -> d.v)
      |> List.sort compare_value
      |> List.map edn_string_of_value
    in
    "(" ^ String.concat " " values ^ ")"
  | One ->
    current_attr_value_db db e a
    |> Option.map edn_string_of_value
    |> Option.value ~default:"nil"

let compare_and_set_failure_message_db db e a expected =
  ":db.fn/cas failed on datom ["
  ^ Int64.to_string e
  ^ " :"
  ^ a
  ^ " "
  ^ cas_current_value_string_db db e a
  ^ "], expected "
  ^ cas_expected_value_string expected

let upsert_context_for_db db : Upsert.context =
  { is_unique_identity
  ; entid_in_datoms = (fun _schema_db _datoms attr value -> entid_db db attr value)
  ; value_to_string = edn_string_of_value
  }

let validate_explicit_upsert_target_db schema_db db entity_id attrs =
  Upsert.validate_explicit_target (upsert_context_for_db db) schema_db [] entity_id attrs

let entity_unique_identity_db schema_db db attrs =
  Upsert.entity_unique_identity (upsert_context_for_db db) schema_db [] attrs

let schema_datoms_for_tx db tx_data =
  let schema_datom datom =
    datom.a = "db/ident" || List.mem datom.a Schema.schema_fields
  in
  let same_fact left right =
    left.e = right.e && left.a = right.a && value_equal left.v right.v
  in
  (* first-occurrence dedup on the (e,a,v) fact, ignoring tx — same
     semantics as append_unique but bucketed by (e,a) and accumulated
     reversed so a whole seed tx costs O(n) instead of O(n^2). *)
  let seen = Hashtbl.create 512 in
  let append_unique_rev datoms_rev datom =
    let key = datom.e, datom.a in
    match Hashtbl.find_opt seen key with
    | Some bucket when List.exists (same_fact datom) bucket -> datoms_rev
    | Some bucket ->
      Hashtbl.replace seen key (datom :: bucket);
      datom :: datoms_rev
    | None ->
      Hashtbl.replace seen key [ datom ];
      datom :: datoms_rev
  in
  let touched_schema_entities =
    tx_data
    |> List.filter_map (fun datom ->
      if schema_datom datom then Some datom.e else None)
    |> List.sort_uniq compare
  in
  let active_datoms =
    touched_schema_entities
    |> List.concat_map (fun entity_id ->
      Db_access_impl.datoms db Eavt ~e:entity_id () |> List.of_seq)
  in
  let asserted_schema_datoms =
    tx_data
    |> List.filter (fun datom -> datom.added && schema_datom datom)
    |> List.rev
  in
  List.fold_left append_unique_rev [] (asserted_schema_datoms @ active_datoms)
  |> List.rev

let schema_fields = Schema.schema_fields

let schema_from_transaction_datoms = Schema.schema_from_transaction_datoms

let transact_apply_context : Transact_impl.apply_context =
  { resolve_context = transact_resolve_context
  ; is_filtered
  ; schema_from_transaction_datoms =
      (fun ?(validate = true) ?removed_field_attrs ~strict ~removed_attrs ~removed_fields ~ignored_schema_entities schema datoms ->
        schema_from_transaction_datoms
          ~validate
          ~strict
          ~removed_attrs
          ~removed_fields
          ~ignored_schema_entities
          ?removed_field_attrs
          schema
          datoms)
  ; schema_datoms = schema_datoms_for_tx
  ; schema_fields
  ; current_attr_value = current_attr_value_db
  ; add_entity_attr_value = add_entity_attr_value_db
  ; same_fact
  ; add_user_datom_with_report = add_user_datom_with_report_db
  ; is_tuple_attr
  ; tuple_attrs_for_source
  ; is_unique_identity
  ; retract_user_attr_with_report = retract_user_attr_with_report_db
  ; retract_active_datom_with_report = retract_active_datom_with_report_db
  ; retract_entity_with_report = retract_entity_with_report_db
  ; compare_and_set_matches = compare_and_set_matches_db
  ; compare_and_set_failure_message = compare_and_set_failure_message_db
  ; datom = Db_impl.datom
  ; normalize_datom_for_schema
  ; add_active_datom_with_report = add_active_datom_with_report_db
  ; validate_explicit_upsert_target = validate_explicit_upsert_target_db
  ; entity_unique_identity = entity_unique_identity_db
  ; existing_unique_entity =
      (fun db attr value ->
        Db_access_impl.find_datom db Avet ~a:attr ~v:value ()
        |> Option.map (fun d -> d.e))
  ; existing_entity_datoms =
      (fun db entity_id ->
        Db_access_impl.datoms db Eavt ~e:entity_id ()
        |> List.of_seq)
  ; existing_entity_attr_datoms =
      (fun db entity_id attr ->
        Db_access_impl.datoms db Eavt ~e:entity_id ~a:attr ()
        |> List.of_seq)
  ; datoms_referencing_entity =
      (fun db entity_id -> incoming_ref_datoms db [ entity_id ])
  ; value_equal
  ; normalize_entity_attr_value
  ; tuple_direct_write_matches_sources = tuple_direct_write_matches_sources_db
  ; refresh_tuple_attrs_for_source = refresh_tuple_attrs_for_source_db
  ; refresh_db_indexes_with_added_datoms
  ; refresh_db_indexes_with_tx_data
  ; refresh_db_identity
  ; tx_memo = transact_tx_memo
  }

let apply_tx tx_ops db =
  Transact_impl.apply_tx transact_apply_context tx_ops db

let db_with tx_ops db =
  let db_after, _, _ = apply_tx tx_ops db in
  db_after

let apply_tail_group db group =
  (* upstream replays each committed tx through the transact path, so schema
     datoms carried by the tail update db.schema for later groups; refresh it
     before inserting so unique/cardinality checks and ref value tags see
     the attrs this and earlier groups installed *)
  let db =
    match schema_datoms_for_tx db group with
    | [] -> db
    | schema_datoms ->
      { db with
        schema =
          schema_from_transaction_datoms ~strict:false ~validate:false
            db.schema schema_datoms
      }
  in
  let group = List.map (Storage.normalize_stored_datom db.schema) group in
  List.iter
    (fun datom ->
      if datom.added && is_unique db datom.a then
        match Db_access_impl.find_datom db Avet ~a:datom.a ~v:datom.v () with
        | Some existing when existing.e <> datom.e ->
          invalid_arg "tail group conflicts with an existing unique value"
        | Some _ | None -> ())
    group;
  let group =
    List.fold_left
      (fun tx_data datom ->
        if datom.added && cardinality db datom.a = One then
          let existing =
            Db_access_impl.datoms db Eavt ~e:datom.e ~a:datom.a ()
            |> Seq.filter (fun existing -> not (value_equal existing.v datom.v))
            |> Seq.map (fun existing ->
              { existing with tx = datom.tx; added = false })
            |> List.of_seq
          in
          List.rev_append existing (datom :: tx_data)
        else
          datom :: tx_data)
      []
      group
    |> List.rev
  in
  let max_eid =
    List.fold_left
      (fun max_eid datom ->
        let max_eid =
          if datom.e <= max_allocatable_entity_id then max max_eid datom.e
          else max_eid
        in
        max_eid_in_value max_eid datom.v)
      db.max_eid
      group
  in
  let db = refresh_db_indexes_with_tx_data db group in
  { db with max_eid }

let storage_tail_context : Storage.tail_context =
  { apply_group = apply_tail_group }

let db_with_tail db tail =
  Storage.db_with_tail storage_tail_context db tail

let storage_restore_context : Storage.restore_context =
  { next_db_uid; db_with_tail }

let restore storage =
  Storage.restore storage_restore_context storage

let restore_conn storage =
  let context : Conn.restore_context = { restore; restore_tail_groups } in
  Conn.restore context storage

let tx_meta_skips_store tx_meta =
  List.exists
    (function
      | "skip-store?", Bool true -> true
      | _ -> false)
    tx_meta

let persist_transact_tail ~tx_meta db tx_data =
  if tx_data = [] || tx_meta_skips_store tx_meta then
    db
  else
    match db.storage_ref with
    | None -> db
    | Some storage ->
      let tail = restore_tail_groups storage @ [ tx_data ] in
      if storage_tail_datom_count tail > storage_tail_compaction_threshold db then
        store ~storage db
      else begin
        store_tail storage tail;
        db
      end

let transact_report ?(tx_meta = []) db tx_ops =
  let db_after, tempids, tx_data = apply_tx tx_ops db in
  { db_before = db; db_after; tx_data; tempids; tx_meta }

let transact ?(tx_meta = []) db tx_ops =
  let report = transact_report ~tx_meta db tx_ops in
  let db_after = persist_transact_tail ~tx_meta report.db_after report.tx_data in
  { report with db_after }

let with_tx ?tx_meta db tx_ops = transact ?tx_meta db tx_ops

let transact_conn ?(tx_meta = []) conn tx_data =
  let context : Conn.transact_context =
    { store
    ; store_tail
    ; storage_tail_datom_count
    ; storage_tail_compaction_threshold
    ; transact = (fun ~tx_meta db tx_data -> transact_report ~tx_meta db tx_data)
    }
  in
  Conn.transact context ~tx_meta conn tx_data

let apply_report (conn : conn) (report : tx_report) : tx_report =
  let context : Conn.transact_context =
    { store
    ; store_tail
    ; storage_tail_datom_count
    ; storage_tail_compaction_threshold
    ; transact = (fun ~tx_meta db tx_data -> transact_report ~tx_meta db tx_data)
    }
  in
  Conn.apply_report context conn report

let transact_bang ?tx_meta conn tx_data = transact_conn ?tx_meta conn tx_data

let transact_async ?tx_meta conn tx_data = transact_conn ?tx_meta conn tx_data

let last_tempid = ref 0

let tempid ?part ?value () =
  match part, value with
  | Some "db.part/tx", _ | Some ":db.part/tx", _ -> CurrentTx
  | _, Some value when value > 0L -> Entity_id (validate_entity_id value)
  | _, Some value -> Temp_id (Int64.to_string value)
  | _ ->
    decr last_tempid;
    Temp_id (string_of_int !last_tempid)

let resolve_tempid ?db:_ tempids tempid = List.assoc_opt tempid tempids

let entid_ref = Db_access_impl.entid_ref
let datoms ?e ?a ?v ?tx db index = Db_access_impl.datoms db index ?e ?a ?v ?tx ()

let fold_datoms ?e ?a ?v ?tx f init db index =
  Db_access_impl.fold_datoms f init db index ?e ?a ?v ?tx ()

let datoms_ref ?e ?a ?v ?tx db index = Db_access_impl.datoms_ref db index ?e ?a ?v ?tx ()

let find_datom ?e ?a ?v ?tx db index = Db_access_impl.find_datom db index ?e ?a ?v ?tx ()

let find_datom_ref ?e ?a ?v ?tx db index =
  Db_access_impl.find_datom_ref db index ?e ?a ?v ?tx ()

let seek_datoms ?e ?a ?v ?tx db index = Db_access_impl.seek_datoms db index ?e ?a ?v ?tx ()

let seek_datoms_ref ?e ?a ?v ?tx db index =
  Db_access_impl.seek_datoms_ref db index ?e ?a ?v ?tx ()

let rseek_datoms ?e ?a ?v ?tx db index =
  Db_access_impl.rseek_datoms db index ?e ?a ?v ?tx ()

let rseek_datoms_ref ?e ?a ?v ?tx db index =
  Db_access_impl.rseek_datoms_ref db index ?e ?a ?v ?tx ()

let index_range ?start ?stop db attr = Db_access_impl.index_range db attr ?start ?stop ()

let diff = Db_impl.diff

let db_hash = Db_impl.hash

let db_hash_cache_size = Db_impl.hash_cache_size

let squuid = Db_impl.squuid

let squuid_time_millis = Db_impl.squuid_time_millis

let reset_conn ?(tx_meta = []) conn db =
  let context : Conn.reset_context =
    { store; datoms = (fun db -> Db_access_impl.datoms_list db Eavt ()) }
  in
  Conn.reset context ~tx_meta conn db

let reset_conn_bang ?tx_meta conn db = reset_conn ?tx_meta conn db

module Entity_refs_impl = Entity_refs.Make (struct
  let lookup_ref_entity_id db attr value = lookup_ref_entity_id_db db attr value
  let entid = entid_db
  let ident_attr = ident_attr
  let normalize_value = normalize_value
end)

let entity_id_of_ref = Entity_refs_impl.entity_id_of_ref
let resolve_ref_value = Entity_refs_impl.resolve_ref_value

let values_compare_equal_fast left right =
  match left, right with
  | Nil, Nil -> true
  | Int64 left, Int64 right ->
    left = right
  | Ref left, Ref right ->
    left = right
  | Int64 left, Ref right ->
    left = right
  | Ref left, Int64 right ->
    left = right
  | String left, String right
  | Symbol left, Symbol right
  | Keyword left, Keyword right
  | Uuid left, Uuid right
  | Regex left, Regex right ->
    left = right
  | Bool left, Bool right -> left = right
  | Instant left, Instant right -> left = right
  | TxRef, TxRef -> true
  | _ -> compare_value left right = 0

(* upstream -search db [nil attr v]: avet slice when the attr is indexed
   (a ref, unique, or :db/index attr), else an aevt scan filtered by value *)
let search_attr_value db attr value =
  if Db_access_impl.is_avet_accessible db attr then
    Db_access_impl.datoms db Avet ~a:attr ~v:value ()
  else
    Db_access_impl.search_datoms db Aevt ~a:attr ()
    |> Seq.filter (fun datom -> values_compare_equal_fast datom.v value)

let entity_context =
  { Entity.datoms_by_entity = (fun db entity_id -> Db_access_impl.datoms db Eavt ~e:entity_id ())
  ; datoms_by_avet_ref = (fun db attr entity_id -> search_attr_value db attr (Ref entity_id))
  ; all_datoms = (fun db -> Db_access_impl.datoms db Eavt ())
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

let entity_attrs = Entity.entity_attrs

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
  ; entity_attrs
  ; datoms_by_entity = (fun db entity_id -> Db_access_impl.datoms db Eavt ~e:entity_id ())
  ; all_datoms = (fun db -> Db_access_impl.datoms db Eavt ())
  ; datoms_by_avet_ref = (fun db attr entity_id -> search_attr_value db attr (Ref entity_id))
  ; cardinality
  ; is_ref_attr
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
  ; datom = Db_impl.datom
  ; validate_schema
  ; empty_db = (fun ?(schema = []) () -> empty_db ~schema ())
  ; max_eid_with_entity_id = Db_impl.max_eid_with_entity_id
  ; max_eid_in_value
  ; resolve_value_for_attr =
      (fun db attr datoms tx max_eid tempids value ->
        let datom_db = init_db ~schema:db.schema datoms in
        let value, max_eid, tempids =
          resolve_value_for_attr db attr datom_db tx max_eid
            (Transact_impl.tempid_map_of_list tempids) value
        in
        value, max_eid, Transact_impl.tempid_map_order tempids)
  ; init_db = (fun ?(schema = []) datoms -> init_db ~schema datoms)
  }


let parse_tx_data_string input = Data_readers_impl.parse_tx_data_string data_readers_context input
let schema_of_edn_string input = Data_readers_impl.schema_of_edn_string data_readers_context input
let db_from_reader_string input = Data_readers_impl.db_from_reader_string data_readers_context input
let attr_of_edn_key = Data_readers_impl.attr_of_edn_key
let tx_attr_of_edn_key = Data_readers_impl.tx_attr_of_edn_key
let tx_op_name_of_edn_form = Data_readers_impl.tx_op_name_of_edn_form
let is_edn_attr_key = Data_readers_impl.is_edn_attr_key
let keyword_name_of_form = Data_readers_impl.keyword_name_of_form
let entity_ref_of_edn_form = Data_readers_impl.entity_ref_of_edn_form data_readers_context
let tx_data_of_edn_form = Data_readers_impl.tx_data_of_edn_form data_readers_context
let schema_of_edn_form = Data_readers_impl.schema_of_edn_form data_readers_context
let db_from_reader_form = Data_readers_impl.db_from_reader_form data_readers_context

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
  ; coerce_tuple_lookup_value = (fun attr value -> coerce_tuple_lookup_value_db db attr value)
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

(* A bound term in entity position that cannot resolve to an entity id makes
   the pattern unsatisfiable — upstream `-search` finds no datoms. A term
   carrying no binding (variable, wildcard) imposes no constraint. *)
type entity_term_resolution =
  | Resolved_eid of entity_id
  | Unresolved_eid
  | No_eid_constraint

let query_entity_id_term db = function
  | QEntity entity_id -> Resolved_eid entity_id
  | QIdent ident ->
    (match entid db ident_attr (Keyword ident) with
     | Some entity_id -> Resolved_eid entity_id
     | None -> Unresolved_eid)
  | QValue (Int64 entity_id) -> Resolved_eid entity_id
  | QValue value ->
    (match Query.query_result_entity_id (query_result_context db) (Result_value value) with
     | Some entity_id -> Resolved_eid entity_id
     | None ->
       (* match_query_term resolves keyword idents in entity position *)
       (match resolve_query_value db value with
        | Some (Keyword ident) ->
          (match entid db ident_attr (Keyword ident) with
           | Some entity_id -> Resolved_eid entity_id
           | None -> Unresolved_eid)
        | _ -> Unresolved_eid))
  | _ -> No_eid_constraint

let query_value_term = function
  | QValue value -> Some value
  (* a variable bound to an entity id substitutes to QEntity; in value position
     that entity is the ref value of the datom *)
  | QEntity entity_id -> Some (Ref entity_id)
  | _ -> None

let query_tx_term = function
  | Some (QValue (Int64 tx)) -> Some tx
  | _ -> None

let query_attr_uses_avet db attr =
  is_ref_attr db attr || is_unique db attr || is_indexed db attr

let query_value_uses_avet = function
  | Nil | List _ | Vector _ | Map _ | Set _ | Tuple _ | TxRef -> false
  | Int64 _ | Float _ | String _ | Symbol _ | Bool _ | Keyword _ | Uuid _ | Instant _ | Regex _ | Ref _ | Ref_to _ -> true

let resolve_query_value_for_attr db attr value =
  match ref_attr_for_value_resolution db attr, entity_ref_of_ref_attr_value value with
  | Some _, Some entity_ref ->
    Option.map (fun entity_id -> Ref entity_id) (entid_ref db entity_ref)
  | _ -> resolve_query_value db value

let datoms_by_attr_value db attr value =
  match resolve_query_value_for_attr db attr value with
  | None -> []
  | Some value ->
    let value =
      if is_tuple_attr db attr then
        coerce_tuple_lookup_value_db db attr value
      else
        normalize_value value
    in
    let ident_entity_value =
      match value, ref_attr_for_value_resolution db attr with
      | Keyword ident, None -> Option.map (fun entity_id -> Ref entity_id) (entid db ident_attr (Keyword ident))
      | _ -> None
    in
    let datom_value_matches datom =
      values_compare_equal_fast datom.v value
      ||
      match ident_entity_value with
      | Some entity_value -> values_compare_equal_fast datom.v entity_value
      | None -> false
    in
    if Option.is_none ident_entity_value && query_value_uses_avet value && query_attr_uses_avet db attr then
      Db_access_impl.datoms_list db Avet ~a:attr ~v:value ()
    else
      Db_access_impl.datoms_list db Aevt ~a:attr ()
      |> List.filter datom_value_matches

let pattern_value_needs_attr_resolution db attr value =
  is_tuple_attr db attr
  ||
  match ref_attr_for_value_resolution db attr, entity_ref_of_ref_attr_value value with
  | Some _, Some _ -> true
  | _ ->
    (match value with
     | Keyword ident -> Option.is_some (entid db ident_attr (Keyword ident))
     | _ -> false)

let primary_attr_datoms db index attr =
  let attr_prefix_datoms index index_set =
    let bound = Db_impl.datom ~e:0L ~a:attr ~v:Nil () in
    let compare_prefix left right = Util.compare_attr left.a right.a in
    let cmp left right =
      if right == bound then compare_prefix left right
      else if left == bound then -compare_prefix right left
      else Util.compare_datom index left right
    in
    PSet.slice ~from_:bound ~to_:bound ~cmp index_set
  in
  match index with
  | Aevt ->
    (match Hashtbl.find_opt db.aevt_by_attr attr with
     | Some datoms -> datoms
     | None ->
       let datoms = attr_prefix_datoms Aevt db.aevt_index in
       Hashtbl.replace db.aevt_by_attr attr datoms;
       datoms)
  | Avet ->
    (match Hashtbl.find_opt db.avet_by_attr attr with
     | Some datoms -> datoms
     | None ->
       let datoms = attr_prefix_datoms Avet db.avet_index in
       Hashtbl.replace db.avet_by_attr attr datoms;
       datoms)
  | Eavt -> PSet.to_list db.eavt_index

let primary_attr_datoms_seq db index ?e ~a ?v ?tx () =
  let datoms = primary_attr_datoms db index a in
  match e, v, tx with
  | None, None, None -> List.to_seq datoms
  | _ ->
    datoms
    |> List.to_seq
    |> Seq.filter (fun datom ->
      (match e with
       | Some entity_id -> datom.e = entity_id
       | None -> true)
      &&
      (match v with
       | Some value -> values_compare_equal_fast datom.v value
       | None -> true)
      &&
      match tx with
      | Some tx -> datom.tx = tx
      | None -> true)

let query_attr_datoms_seq db index ?e ~a ?v ?tx () =
  match db.duplicate_datoms with
  | [] -> Db_access_impl.datoms db index ?e ~a ?v ?tx ()
  | _ -> primary_attr_datoms_seq db index ?e ~a ?v ?tx ()

let pattern_datoms db e_term a_term v_term tx_term =
  match query_entity_id_term db e_term with
  | Unresolved_eid -> Seq.empty
  | resolution ->
    let e =
      match resolution with
      | Resolved_eid entity_id -> Some entity_id
      | Unresolved_eid | No_eid_constraint -> None
    in
  let v = query_value_term v_term in
  let tx = query_tx_term tx_term in
  let matches_optional_e_tx datom =
    (match e with
     | Some entity_id -> datom.e = entity_id
     | None -> true)
    &&
    match tx with
    | Some tx -> datom.tx = tx
    | None -> true
  in
  match a_term, v with
  | QAttr attr, _ when is_reverse_ref attr ->
    query_attr_datoms_seq db Aevt ~a:(reverse_ref attr) ?tx ()
  | QAttr attr, Some value when not (pattern_value_needs_attr_resolution db attr value) ->
    if query_value_uses_avet value && query_attr_uses_avet db attr then
      query_attr_datoms_seq db Avet ?e ~a:attr ~v:value ?tx ()
    else
      query_attr_datoms_seq db Aevt ?e ~a:attr ~v:value ?tx ()
  | QAttr attr, Some value ->
    datoms_by_attr_value db attr value
    |> List.to_seq
    |> Seq.filter matches_optional_e_tx
  | QAttr attr, _ ->
    query_attr_datoms_seq db Aevt ?e ~a:attr ?tx ()
  | _ -> Db_access_impl.datoms db Eavt ?e ?v ?tx ()

let fold_pattern_datoms db e_term a_term v_term tx_term ~init ~f =
  match query_entity_id_term db e_term with
  | Unresolved_eid -> init
  | resolution ->
    let e =
      match resolution with
      | Resolved_eid entity_id -> Some entity_id
      | Unresolved_eid | No_eid_constraint -> None
    in
  let v = query_value_term v_term in
  let tx = query_tx_term tx_term in
  let matches_optional_e_tx datom =
    (match e with
     | Some entity_id -> datom.e = entity_id
     | None -> true)
    &&
    match tx with
    | Some tx -> datom.tx = tx
    | None -> true
  in
  match a_term, v with
  | QAttr attr, _ when is_reverse_ref attr ->
    Db_access_impl.fold_datoms f init db Aevt ~a:(reverse_ref attr) ?tx ()
  | QAttr attr, Some value when not (pattern_value_needs_attr_resolution db attr value) ->
    if query_value_uses_avet value && query_attr_uses_avet db attr then
      Db_access_impl.fold_datoms f init db Avet ?e ~a:attr ~v:value ?tx ()
    else
      Db_access_impl.fold_datoms f init db Aevt ?e ~a:attr ~v:value ?tx ()
  | QAttr attr, Some value ->
    datoms_by_attr_value db attr value
    |> List.fold_left (fun acc datom -> if matches_optional_e_tx datom then f acc datom else acc) init
  | QAttr attr, _ ->
    Db_access_impl.fold_datoms f init db Aevt ?e ~a:attr ?tx ()
  | _ -> Db_access_impl.fold_datoms f init db Eavt ?e ?v ?tx ()

let pattern_comparison_datoms db terms predicate threshold =
  let attr =
    match terms with
    | [ _; QAttr attr; _ ]
    | [ _; QAttr attr; _; _ ]
    | [ _; QAttr attr; _; _; _ ]
      when (not (is_reverse_ref attr)) && query_attr_uses_avet db attr ->
      Some attr
    | _ -> None
  in
  match attr, predicate with
  | Some attr, GreaterThan | Some attr, GreaterOrEqual -> Some (Db_access_impl.index_range db attr ~start:threshold ())
  | Some attr, LessThan | Some attr, LessOrEqual -> Some (Db_access_impl.index_range db attr ~stop:threshold ())
  | _ -> None

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
  ; fold_pattern_datoms
  ; pattern_comparison_datoms
  ; match_data_pattern
  ; match_data_pattern_tx
  ; match_data_pattern_tx_op
  }

let eval_query_term db bindings term =
  Query.eval_query_term (query_match_context db) bindings term

let collect_query_terms_exn db bindings terms =
  Query.collect_query_terms_exn (query_match_context db) bindings terms


let query_evaluator_context : Query_eval.evaluator_context =
  { result_resolution_context = query_result_context
  ; match_context = query_match_context
  ; datoms = Db_access_impl.datoms
  ; is_reverse_ref
  ; reverse_ref
  ; compare_value
  ; split_keyword
  ; normalize_value
  }

let value_of_query_result = Query_eval.value_of_query_result
let value_has_count = Query_eval.value_has_count
let value_is_not_empty = Query_eval.value_is_not_empty
let matches_value_predicate = Query_eval.matches_value_predicate
let matches_numeric_predicate = Query_eval.matches_numeric_predicate
let comparison_chain_matches = Query_eval.comparison_chain_matches
let all_values_equal = Query_eval.all_values_equal
let matches_boolean_predicate = Query_eval.matches_boolean_predicate
let split_at = Query_eval.split_at
let values_equal = Query_eval.values_equal
let string_starts_with = Query_eval.string_starts_with
let string_ends_with = Query_eval.string_ends_with
let string_includes = Query_eval.string_includes

let string_includes_prefilter input query =
  query = "" || (String.contains input query.[0] && string_includes input query)
let string_is_blank = Query_eval.string_is_blank
let value_contains = Query_eval.value_contains

let source_db default_db sources name =
  Query.source_db default_db sources name


let match_query_source_pattern default_db source bindings terms =
  Query.match_query_source_pattern (query_source_context default_db) default_db source bindings terms

let match_source_pattern default_db sources source_name bindings terms =
  Query.match_source_pattern (query_source_context default_db) default_db sources source_name bindings terms

let match_relation_source_pattern default_db sources source_name bindings terms =
  Query.match_relation_source_pattern (query_source_context default_db) default_db sources source_name bindings terms

module Query_runtime_impl = Query_runtime.Make (struct
  let parse_pull_pattern = parse_pull_pattern
  let empty_db () = empty_db ()
  let query_form_of_value = query_form_of_value
  let source_db = source_db
  let query_result_entity_id = query_result_entity_id
  let pull = pull
  let query_match_context = query_match_context
  let eval_query_term = eval_query_term
  let bind_var = bind_var
  let resolve_query_value = resolve_query_value
  let entity_id_of_ref = entity_id_of_ref
  let edn_string_of_value = edn_string_of_value
end)

let collect_find_specs = Query_runtime_impl.collect_find_specs
let has_aggregates = Query_runtime_impl.has_aggregates
let aggregate_rows = Query_runtime_impl.aggregate_rows
let aggregate_rows_with = Query_runtime_impl.aggregate_rows_with
let non_aggregate_rows_with = Query_runtime_impl.non_aggregate_rows_with
let rule_invocation_binding = Query_runtime_impl.rule_invocation_binding
let propagate_rule_binding = Query_runtime_impl.propagate_rule_binding
let rule_invocation_callables = Query_runtime_impl.rule_invocation_callables
let bind_relation_row = Query_runtime_impl.bind_relation_row
let collection_values_of_input = Query_runtime_impl.collection_values_of_input
let row_values_of_input = Query_runtime_impl.row_values_of_input
let eval_ground_term_tuple = Query_runtime_impl.eval_ground_term_tuple
let eval_ground_term_relation = Query_runtime_impl.eval_ground_term_relation
let initial_query_context = Query_runtime_impl.initial_query_context

module Query_where_impl = Query_where.Make (struct
  let query_evaluator_context = query_evaluator_context
  let edn_string_of_value = edn_string_of_value
  let query_source_context = query_source_context
  let query_match_context = query_match_context
  let eval_query_term = eval_query_term
  let collect_query_terms_exn = collect_query_terms_exn
  let bind_var = bind_var
  let match_query_source_pattern = match_query_source_pattern
  let match_source_pattern = match_source_pattern
  let match_relation_source_pattern = match_relation_source_pattern
  let bind_relation_row = bind_relation_row
  let collection_values_of_input = collection_values_of_input
  let row_values_of_input = row_values_of_input
  let eval_ground_term_tuple = eval_ground_term_tuple
  let eval_ground_term_relation = eval_ground_term_relation
  let rule_invocation_binding = rule_invocation_binding
  let rule_invocation_callables = rule_invocation_callables
  let propagate_rule_binding = propagate_rule_binding
  let query_result_entity_id = query_result_entity_id
  let is_ref_attr = is_ref_attr
  let cardinality_one db attr = cardinality db attr = One
  let normalize_value = normalize_value
end)

let eval_clauses = Query_where_impl.eval_clauses
let eval_relation_rows = Query_where_impl.eval_relation_rows

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

let normalize_var (name : var) : var =
  if String.length name > 0 && name.[0] = '?' then String.sub name 1 (String.length name - 1) else name

let normalize_source (name : source_var) : source_var =
  if String.equal name "$" then
    name
  else if String.length name > 1 && name.[0] = '$' then
    String.sub name 1 (String.length name - 1)
  else
    name

(* Boundary normalization: [?e]/[e] denote the same var, [$x]/[x] the
   same source.  Public [string] domains are interned here so queries
   built either way are equivalent. *)

let norm_term = function
  | QVar name -> QVar (normalize_var name)
  | QSource name -> QSource (normalize_source name)
  | term -> term

let norm_terms = List.map norm_term

let rec norm_binding = function
  | Bind_scalar name -> Bind_scalar (normalize_var name)
  | Bind_collection binding -> Bind_collection (norm_binding binding)
  | Bind_tuple bindings -> Bind_tuple (List.map norm_binding bindings)
  | binding -> binding

let norm_vars = List.map normalize_var

let rec norm_clause = function
  | Pattern (e, a, v) -> Pattern (norm_term e, norm_term a, norm_term v)
  | PatternTx (e, a, v, tx) ->
    PatternTx (norm_term e, norm_term a, norm_term v, norm_term tx)
  | PatternTxOp (e, a, v, tx, op) ->
    PatternTxOp (norm_term e, norm_term a, norm_term v, norm_term tx, norm_term op)
  | SourcePattern (src, e, a, v) ->
    SourcePattern (normalize_source src, norm_term e, norm_term a, norm_term v)
  | SourcePatternTx (src, e, a, v, tx) ->
    SourcePatternTx (normalize_source src, norm_term e, norm_term a, norm_term v, norm_term tx)
  | SourcePatternTxOp (src, e, a, v, tx, op) ->
    SourcePatternTxOp
      (normalize_source src, norm_term e, norm_term a, norm_term v, norm_term tx, norm_term op)
  | SourceRelationPattern (src, terms) ->
    SourceRelationPattern (normalize_source src, norm_terms terms)
  | Missing (e, a) -> Missing (norm_term e, norm_term a)
  | SourceMissing (src, e, a) -> SourceMissing (normalize_source src, norm_term e, norm_term a)
  | GetElse (e, a, default, out) ->
    GetElse (norm_term e, norm_term a, norm_term default, normalize_var out)
  | SourceGetElse (src, e, a, default, out) ->
    SourceGetElse
      (normalize_source src, norm_term e, norm_term a, norm_term default, normalize_var out)
  | GetSome (e, attrs, attr_var, value_var) ->
    GetSome
      (norm_term e, norm_terms attrs, normalize_var attr_var, normalize_var value_var)
  | SourceGetSome (src, e, attrs, attr_var, value_var) ->
    SourceGetSome
      ( normalize_source src
      , norm_term e
      , norm_terms attrs
      , normalize_var attr_var
      , normalize_var value_var )
  | GetValue (e, a, out) -> GetValue (norm_term e, norm_term a, normalize_var out)
  | GetDefaultValue (e, a, default, out) ->
    GetDefaultValue (norm_term e, norm_term a, norm_term default, normalize_var out)
  | CountValue (e, out) -> CountValue (norm_term e, normalize_var out)
  | EmptyValue e -> EmptyValue (norm_term e)
  | NotEmptyValue e -> NotEmptyValue (norm_term e)
  | ContainsValue (e, needle) -> ContainsValue (norm_term e, norm_term needle)
  | ValuePredicate (pred, e) -> ValuePredicate (pred, norm_term e)
  | NumericPredicate (pred, e) -> NumericPredicate (pred, norm_term e)
  | ComparisonPredicate (pred, l, r) ->
    ComparisonPredicate (pred, norm_term l, norm_term r)
  | ComparisonPredicateN (pred, terms) -> ComparisonPredicateN (pred, norm_terms terms)
  | EqualityPredicate (pred, terms) -> EqualityPredicate (pred, norm_terms terms)
  | ArithmeticValue (op, terms, out) ->
    ArithmeticValue (op, norm_terms terms, normalize_var out)
  | CompareValue (a, b, out) -> CompareValue (norm_term a, norm_term b, normalize_var out)
  | ExtremumValue (op, terms, out) ->
    ExtremumValue (op, norm_terms terms, normalize_var out)
  | BooleanPredicate (pred, e) -> BooleanPredicate (pred, norm_term e)
  | BooleanNotPredicate e -> BooleanNotPredicate (norm_term e)
  | BooleanNotValue (e, out) -> BooleanNotValue (norm_term e, normalize_var out)
  | IdentityValue (e, out) -> IdentityValue (norm_term e, normalize_var out)
  | BooleanAndPredicate terms -> BooleanAndPredicate (norm_terms terms)
  | BooleanAndValue (terms, out) -> BooleanAndValue (norm_terms terms, normalize_var out)
  | BooleanOrPredicate terms -> BooleanOrPredicate (norm_terms terms)
  | BooleanOrValue (terms, out) -> BooleanOrValue (norm_terms terms, normalize_var out)
  | RandomValue out -> RandomValue (normalize_var out)
  | RandomIntValue (e, out) -> RandomIntValue (norm_term e, normalize_var out)
  | DifferPredicate terms -> DifferPredicate (norm_terms terms)
  | IdenticalPredicate (a, b) -> IdenticalPredicate (norm_term a, norm_term b)
  | TypeValue (e, out) -> TypeValue (norm_term e, normalize_var out)
  | MetaValue (e, out) -> MetaValue (norm_term e, normalize_var out)
  | NameValue (e, out) -> NameValue (norm_term e, normalize_var out)
  | NamespaceValue (e, out) -> NamespaceValue (norm_term e, normalize_var out)
  | KeywordFromName (e, out) -> KeywordFromName (norm_term e, normalize_var out)
  | KeywordFromNamespaceName (e, ns, out) ->
    KeywordFromNamespaceName (norm_term e, norm_term ns, normalize_var out)
  | StringIncludesValue (s, sub) -> StringIncludesValue (norm_term s, norm_term sub)
  | StringStartsWithValue (s, sub) -> StringStartsWithValue (norm_term s, norm_term sub)
  | StringEndsWithValue (s, sub) -> StringEndsWithValue (norm_term s, norm_term sub)
  | StringLowerCaseValue (s, out) -> StringLowerCaseValue (norm_term s, normalize_var out)
  | StringUpperCaseValue (s, out) -> StringUpperCaseValue (norm_term s, normalize_var out)
  | StringCapitalizeValue (s, out) ->
    StringCapitalizeValue (norm_term s, normalize_var out)
  | StringReverseValue (s, out) -> StringReverseValue (norm_term s, normalize_var out)
  | StringTrimValue (s, out) -> StringTrimValue (norm_term s, normalize_var out)
  | StringTrimLeftValue (s, out) -> StringTrimLeftValue (norm_term s, normalize_var out)
  | StringTrimRightValue (s, out) ->
    StringTrimRightValue (norm_term s, normalize_var out)
  | StringTrimNewlineValue (s, out) ->
    StringTrimNewlineValue (norm_term s, normalize_var out)
  | StringIndexOfValue (s, sub, out) ->
    StringIndexOfValue (norm_term s, norm_term sub, normalize_var out)
  | StringLastIndexOfValue (s, sub, out) ->
    StringLastIndexOfValue (norm_term s, norm_term sub, normalize_var out)
  | StringSubstringValue (s, start, stop, out) ->
    StringSubstringValue
      (norm_term s, norm_term start, Option.map norm_term stop, normalize_var out)
  | StringBuildValue (terms, out) -> StringBuildValue (norm_terms terms, normalize_var out)
  | PrintStringValue (terms, out) -> PrintStringValue (norm_terms terms, normalize_var out)
  | PrintLineStringValue (terms, out) ->
    PrintLineStringValue (norm_terms terms, normalize_var out)
  | PrStringValue (terms, out) -> PrStringValue (norm_terms terms, normalize_var out)
  | PrnStringValue (terms, out) -> PrnStringValue (norm_terms terms, normalize_var out)
  | StringJoinPlainValue (s, out) -> StringJoinPlainValue (norm_term s, normalize_var out)
  | StringJoinValue (s, sep, out) ->
    StringJoinValue (norm_term s, norm_term sep, normalize_var out)
  | StringReplaceValue (s, pat, rep, out) ->
    StringReplaceValue (norm_term s, norm_term pat, norm_term rep, normalize_var out)
  | StringReplaceFirstValue (s, pat, rep, out) ->
    StringReplaceFirstValue (norm_term s, norm_term pat, norm_term rep, normalize_var out)
  | StringEscapeValue (s, pat, out) ->
    StringEscapeValue (norm_term s, norm_term pat, normalize_var out)
  | RePatternValue (s, out) -> RePatternValue (norm_term s, normalize_var out)
  | ReFindValue (re, s, out) -> ReFindValue (norm_term re, norm_term s, normalize_var out)
  | ReMatchesValue (re, s, out) ->
    ReMatchesValue (norm_term re, norm_term s, normalize_var out)
  | ReSeqValue (re, s, out) -> ReSeqValue (norm_term re, norm_term s, normalize_var out)
  | ReFindPredicate (re, s) -> ReFindPredicate (norm_term re, norm_term s)
  | ReMatchesPredicate (re, s) -> ReMatchesPredicate (norm_term re, norm_term s)
  | StringBlankValue s -> StringBlankValue (norm_term s)
  | StringSplitValue (s, pat, out) ->
    StringSplitValue (norm_term s, norm_term pat, normalize_var out)
  | StringSplitLimitValue (s, pat, n, out) ->
    StringSplitLimitValue (norm_term s, norm_term pat, norm_term n, normalize_var out)
  | StringSplitLinesValue (s, out) ->
    StringSplitLinesValue (norm_term s, normalize_var out)
  | Ground (v, out) -> Ground (v, normalize_var out)
  | GroundCollection (v, out) -> GroundCollection (v, normalize_var out)
  | GroundTuple (v, vars) -> GroundTuple (v, norm_vars vars)
  | GroundRelation (v, vars) -> GroundRelation (v, norm_vars vars)
  | GroundTerm (t, out) -> GroundTerm (norm_term t, normalize_var out)
  | GroundTermCollection (t, out) -> GroundTermCollection (norm_term t, normalize_var out)
  | GroundTermTuple (t, vars) -> GroundTermTuple (norm_term t, norm_vars vars)
  | GroundTermRelation (t, vars) -> GroundTermRelation (norm_term t, norm_vars vars)
  | VectorValue (terms, out) -> VectorValue (norm_terms terms, normalize_var out)
  | ListValue (terms, out) -> ListValue (norm_terms terms, normalize_var out)
  | SetValue (terms, out) -> SetValue (norm_terms terms, normalize_var out)
  | HashMapValue (terms, out) -> HashMapValue (norm_terms terms, normalize_var out)
  | ArrayMapValue (terms, out) -> ArrayMapValue (norm_terms terms, normalize_var out)
  | RangeEndValue (e, out) -> RangeEndValue (norm_term e, normalize_var out)
  | RangeValue (a, b, out) -> RangeValue (norm_term a, norm_term b, normalize_var out)
  | RangeStepValue (a, b, step, out) ->
    RangeStepValue (norm_term a, norm_term b, norm_term step, normalize_var out)
  | TupleFunction (terms, out) -> TupleFunction (norm_terms terms, normalize_var out)
  | UntupleFunction (t, vars) -> UntupleFunction (norm_term t, norm_vars vars)
  | Predicate (name, terms, f) -> Predicate (name, norm_terms terms, f)
  | Function (name, terms, outs, f) -> Function (name, norm_terms terms, norm_vars outs, f)
  | DynamicPredicate (name, terms) -> DynamicPredicate (name, norm_terms terms)
  | DynamicFunction (name, terms, outs) ->
    DynamicFunction (name, norm_terms terms, norm_vars outs)
  | DynamicFunctionCollection (name, terms, out) ->
    DynamicFunctionCollection (name, norm_terms terms, normalize_var out)
  | DynamicFunctionRelation (name, terms, outs) ->
    DynamicFunctionRelation (name, norm_terms terms, norm_vars outs)
  | SourceClause (src, clause) ->
    SourceClause (normalize_source src, norm_clause clause)
  | Not clauses -> Not (List.map norm_clause clauses)
  | SourceNot (src, clauses) -> SourceNot (normalize_source src, List.map norm_clause clauses)
  | NotJoin (vars, clauses) -> NotJoin (norm_vars vars, List.map norm_clause clauses)
  | SourceNotJoin (src, vars, clauses) ->
    SourceNotJoin (normalize_source src, norm_vars vars, List.map norm_clause clauses)
  | Or branches -> Or (List.map (List.map norm_clause) branches)
  | SourceOr (src, branches) ->
    SourceOr (normalize_source src, List.map (List.map norm_clause) branches)
  | OrJoin (vars, branches) ->
    OrJoin (norm_vars vars, List.map (List.map norm_clause) branches)
  | SourceOrJoin (src, vars, branches) ->
    SourceOrJoin (normalize_source src, norm_vars vars, List.map (List.map norm_clause) branches)
  | OrJoinRequired (required, vars, branches) ->
    OrJoinRequired (norm_vars required, norm_vars vars, List.map (List.map norm_clause) branches)
  | SourceOrJoinRequired (src, required, vars, branches) ->
    SourceOrJoinRequired
      ( normalize_source src
      , norm_vars required
      , norm_vars vars
      , List.map (List.map norm_clause) branches )
  | Rule (name, args) -> Rule (name, norm_terms args)
  | SourceRule (src, name, args) -> SourceRule (normalize_source src, name, norm_terms args)

let norm_rule (rule : query_rule) : query_rule =
  { rule with
    rule_params = norm_vars rule.rule_params
  ; rule_body = List.map norm_clause rule.rule_body
  }

let norm_spec = function
  | Spec_source src -> Spec_source (normalize_source src)
  | Spec_scalar v -> Spec_scalar (normalize_var v)
  | Spec_collection v -> Spec_collection (normalize_var v)
  | Spec_nested_collection binding -> Spec_nested_collection (norm_binding binding)
  | Spec_tuple vars -> Spec_tuple (norm_vars vars)
  | Spec_relation vars -> Spec_relation (norm_vars vars)
  | Spec_nested_tuple bindings -> Spec_nested_tuple (List.map norm_binding bindings)
  | Spec_nested_relation bindings -> Spec_nested_relation (List.map norm_binding bindings)
  | spec -> spec

let norm_aggregate = function
  | MinNVar v -> MinNVar (normalize_var v)
  | MaxNVar v -> MaxNVar (normalize_var v)
  | RandNVar v -> RandNVar (normalize_var v)
  | SampleVar v -> SampleVar (normalize_var v)
  | CustomVar v -> CustomVar (normalize_var v)
  | agg -> agg

let norm_find = function
  | Find_var v -> Find_var (normalize_var v)
  | Find_pull (v, p) -> Find_pull (normalize_var v, p)
  | Find_pull_form (v, form) -> Find_pull_form (normalize_var v, form)
  | Find_pull_var (v, alias) -> Find_pull_var (normalize_var v, normalize_var alias)
  | Find_pull_source (src, v, p) ->
    Find_pull_source (normalize_source src, normalize_var v, p)
  | Find_pull_source_form (src, v, form) ->
    Find_pull_source_form (normalize_source src, normalize_var v, form)
  | Find_pull_source_var (src, v, alias) ->
    Find_pull_source_var (normalize_source src, normalize_var v, normalize_var alias)
  | Find_aggregate (agg, terms) -> Find_aggregate (norm_aggregate agg, norm_terms terms)

let validate_rule_arities = Parser_impl.validate_rule_arities

(* Declaration-only :in specs <-> the internal query_input constructors. *)

let query_input_of_spec = function
  | Spec_source name -> Input_source_decl name
  | Spec_scalar name -> Input_scalar_decl name
  | Spec_collection name -> Input_collection_decl name
  | Spec_collection_ignore -> Input_collection_ignore_decl
  | Spec_nested_collection binding -> Input_nested_collection_decl binding
  | Spec_tuple vars -> Input_tuple_decl vars
  | Spec_relation vars -> Input_relation_decl vars
  | Spec_nested_tuple bindings -> Input_nested_tuple_decl bindings
  | Spec_nested_relation bindings -> Input_nested_relation_decl bindings
  | Spec_rules -> Input_rules_decl
  | Spec_ignore -> Input_ignore_decl

let spec_of_query_input = function
  | Input_source_decl name -> Spec_source name
  | Input_scalar_decl name -> Spec_scalar name
  | Input_collection_decl name -> Spec_collection name
  | Input_collection_ignore_decl -> Spec_collection_ignore
  | Input_nested_collection_decl binding -> Spec_nested_collection binding
  | Input_tuple_decl vars -> Spec_tuple vars
  | Input_relation_decl vars -> Spec_relation vars
  | Input_nested_tuple_decl bindings -> Spec_nested_tuple bindings
  | Input_nested_relation_decl bindings -> Spec_nested_relation bindings
  | Input_rules_decl -> Spec_rules
  | Input_ignore_decl | Input_ignore -> Spec_ignore
  | Input_scalar _ | Input_entity_ref _ | Input_collection _ | Input_collection_ignore _
  | Input_nested_collection _ | Input_tuple _ | Input_relation _ | Input_nested_tuple _
  | Input_nested_relation _ | Input_predicate _ | Input_function _ | Input_aggregate _
  | Input_rules _ ->
    invalid_arg "query input specs are declaration-only"

let parse_binding = Parser_impl.parse_binding
let parse_in form = List.map spec_of_query_input (Parser_impl.parse_in form)
let parse_with = Parser_impl.parse_with


let parse_query_return form = Parser_impl.parse_query_return parser_query_context form
let parse_query_return_map form = Parser_impl.parse_query_return_map parser_query_context form
let parse_query form = Parser_impl.parse_query parser_query_context form
let parse_query_string input = Parser_impl.parse_query_string parser_query_context input
let parse_rules_string input = Parser_impl.parse_rules parser_query_context (Some (read_edn input))
let parse_query_string_with_pull_context ?default_pull_db ?pull_db_for_source input =
  Parser_impl.parse_query_string_with_pull_context parser_query_context ?default_pull_db ?pull_db_for_source input
let parse_query_return_string input = Parser_impl.parse_query_return_string parser_query_context input
let parse_query_return_string_with_pull_context ?default_pull_db ?pull_db_for_source input =
  Parser_impl.parse_query_return_string_with_pull_context parser_query_context ?default_pull_db ?pull_db_for_source input
let parse_query_return_map_string input = Parser_impl.parse_query_return_map_string parser_query_context input
let parse_query_return_map_string_with_pull_context ?default_pull_db ?pull_db_for_source input =
  Parser_impl.parse_query_return_map_string_with_pull_context parser_query_context ?default_pull_db ?pull_db_for_source input



let pull_string ?visitor db input entity_ref =
  pull ?visitor db (parse_pull_pattern_string db input) entity_ref

let pull_many_string ?visitor db input entity_refs =
  pull_many ?visitor db (parse_pull_pattern_string db input) entity_refs


module Query_api_impl = Query_api.Make (struct
  let empty_db () = empty_db ()
  let validate_rule_arities = validate_rule_arities
  let initial_query_context = initial_query_context
  let eval_clauses = eval_clauses
  let eval_relation_rows = eval_relation_rows
  let has_aggregates = has_aggregates
  let aggregate_rows = aggregate_rows
  let aggregate_rows_with = aggregate_rows_with
  let non_aggregate_rows_with = non_aggregate_rows_with
  let collect_find_specs = collect_find_specs
  let parse_query_string_with_pull_context = parse_query_string_with_pull_context
  let parse_query_return_string_with_pull_context = parse_query_return_string_with_pull_context
  let parse_query_return_map_string_with_pull_context = parse_query_return_map_string_with_pull_context
  let compare_value = compare_value
end)

module Query_impl = Query

let query_context = Query_api_impl.query_context




  type simple_row_slot =
    | Simple_entity_slot
    | Simple_value_slot of query_result option array

  let simple_same_entity_constant_rows ?inputs db query =
    let ( let* ) = Option.bind in
    match db.max_datom_e > 50_000L, inputs, query.rules, query.with_vars with
    | true, _, _, _ -> None
    | false, Some _, _, _ | false, _, _ :: _, _ | false, _, _, _ :: _ -> None
    | false, None, [], [] ->
      let* find_vars =
        query.find
        |> List.fold_left
             (fun vars -> function
               | Find_var var -> Option.map (fun vars -> var :: vars) vars
               | _ -> None)
             (Some [])
        |> Option.map List.rev
      in
      let pattern_summary =
        List.fold_left
          (fun acc -> function
            | Pattern (QVar entity_var, QAttr attr, value_term)
              when (not (is_reverse_ref attr)) && cardinality db attr = One ->
              (match acc with
               | None -> None
               | Some (e_var, value_var_attrs, constant_patterns) ->
                 let e_var =
                   match e_var with
                   | None -> Some entity_var
                   | Some existing when existing = entity_var -> e_var
                   | Some _ -> None
                 in
                 (match e_var, value_term with
                  | None, _ -> None
                  | Some _, QVar value_var when value_var <> entity_var ->
                    Some (e_var, (value_var, attr) :: value_var_attrs, constant_patterns)
                  | Some _, QValue value ->
                    Some (e_var, value_var_attrs, (attr, value) :: constant_patterns)
                  | _ -> None))
            | _ -> None)
          (Some (None, [], []))
          query.where
      in
      let* e_var, value_var_attrs, constant_patterns =
        Option.bind pattern_summary (function
          | Some e_var, value_var_attrs, constant_patterns ->
            Some (e_var, List.rev value_var_attrs, List.rev constant_patterns)
          | None, _, _ -> None)
      in
      if constant_patterns = [] then
        None
      else
        let duplicate_value_var =
          let seen = Hashtbl.create (List.length value_var_attrs) in
          List.exists
            (fun (value_var, _) ->
              if Hashtbl.mem seen value_var then true
              else (
                Hashtbl.add seen value_var ();
                false ))
            value_var_attrs
        in
        if duplicate_value_var then
          None
        else
          let constant_datoms =
            constant_patterns
            |> List.map (fun (attr, value) -> attr, datoms_by_attr_value db attr value)
          in
          if List.exists (fun (_, datoms) -> datoms = []) constant_datoms then
            Some []
          else
            let value_tables =
              value_var_attrs
              |> List.map (fun (value_var, attr) ->
                let values = Array.make (Int64.to_int db.max_datom_e + 1) None in
                primary_attr_datoms db Aevt attr
                |> List.iter (fun datom ->
                  if datom.e >= 0L && datom.e < Int64.of_int (Array.length values) then
                    values.(Int64.to_int datom.e) <- Some (Query_impl.result_of_datom_v datom));
                value_var, values)
            in
            let slot_for_find_var var =
              if var = e_var then
                Some Simple_entity_slot
              else
                Option.map
                  (fun values -> Simple_value_slot values)
                  (List.assoc_opt var value_tables)
            in
            let* row_slots =
              find_vars
              |> List.fold_left
                   (fun slots var ->
                     match slots with
                     | None -> None
                     | Some slots -> Option.map (fun slot -> slot :: slots) (slot_for_find_var var))
                   (Some [])
              |> Option.map List.rev
            in
            let constant_sets =
              constant_datoms
              |> List.map (fun (_, datoms) ->
                let entities = Bytes.make (Int64.to_int db.max_datom_e + 1) '\000' in
                List.iter
                  (fun datom ->
                    if datom.e >= 0L && datom.e < Int64.of_int (Bytes.length entities) then
                      Bytes.set entities (Int64.to_int datom.e) '\001')
                  datoms;
                entities)
            in
            let _, scan_datoms =
              constant_datoms
              |> List.sort (fun (_, left) (_, right) -> compare (List.length left) (List.length right))
              |> List.hd
            in
            let entity_allowed entity_id =
              constant_sets
              |> List.for_all (fun entities ->
                entity_id >= 0L
                && entity_id < Int64.of_int (Bytes.length entities)
                && Bytes.get entities (Int64.to_int entity_id) = '\001')
            in
            let value_of_slot entity_id = function
              | Simple_entity_slot -> Some (Result_entity entity_id)
              | Simple_value_slot values ->
                if entity_id >= 0L && entity_id < Int64.of_int (Array.length values) then values.(Int64.to_int entity_id) else None
            in
            let row_for_entity entity_id =
              row_slots
              |> List.fold_left
                   (fun row slot ->
                     match row with
                     | None -> None
                     | Some row -> Option.map (fun value -> value :: row) (value_of_slot entity_id slot))
                   (Some [])
              |> Option.map List.rev
            in
            scan_datoms
            |> List.filter_map (fun datom ->
              if entity_allowed datom.e then row_for_entity datom.e else None)
            |> List.sort_uniq compare
            |> fun rows -> Some rows

  let q ?inputs db query =
    match simple_same_entity_constant_rows ?inputs db query with
    | Some rows -> rows
    | None -> Query_impl.q query_context ?inputs db query

  let query_string_cache : (string, query) Hashtbl.t = Hashtbl.create 32
  let cached_query_string input =
    match Hashtbl.find_opt query_string_cache input with
    | Some query -> query
    | None ->
      let query = parse_query_string input in
      Hashtbl.replace query_string_cache input query;
      query

  let q_string ?inputs db input =
    if string_includes input "pull" then
      q ?inputs db (parse_query_string_with_pull_context ~default_pull_db:db input)
    else
      q ?inputs db (cached_query_string input)

  let q_with = Query_impl.q_with query_context
  let q_with_string = Query_impl.q_with_string query_context
  let q_sources = Query_impl.q_sources query_context
  let q_sources_string = Query_impl.q_sources_string query_context

  let only_source_inputs inputs =
    List.for_all
      (function
        | Input_source_decl _ -> true
        | _ -> false)
      inputs

  let entity_ids_with_attr db attr =
    let rec collect previous acc = function
      | [] -> List.rev acc
      | datom :: rest ->
        if Some datom.e = previous then
          collect previous acc rest
        else
          collect (Some datom.e) (datom.e :: acc) rest
    in
    collect None [] (primary_attr_datoms db Aevt attr)

  let exact_attr_value_entity_ids db attr value =
    match resolve_query_value_for_attr db attr value with
    | None -> []
    | Some value ->
      if query_attr_uses_avet db attr then
        Db_access_impl.datoms db Avet ~a:attr ~v:value ()
        |> Seq.map (fun datom -> datom.e)
        |> List.of_seq
        |> List.sort_uniq compare
      else
        let value_matches =
          match value with
          | String wanted ->
            fun actual ->
              (match actual with
               | String actual ->
                 (wanted = "" || String.contains actual wanted.[0]) && String.equal actual wanted
               | _ -> false)
          | _ -> fun actual -> value_equal actual value
        in
        let rec collect acc = function
          | [] -> acc
          | datom :: rest ->
            let acc =
              if value_matches datom.v then
                datom.e :: acc
              else
                acc
            in
            collect acc rest
        in
        collect [] (primary_attr_datoms db Aevt attr)
        |> fun entity_ids ->
        collect entity_ids (Option.value (Hashtbl.find_opt db.duplicate_aevt_by_attr attr) ~default:[])
        |> List.sort_uniq compare

  let intersect_sorted_entity_ids left right =
    let rec loop acc left right =
      match left, right with
      | [], _ | _, [] -> List.rev acc
      | left_id :: left_rest, right_id :: right_rest ->
        if left_id = right_id then
          loop (left_id :: acc) left_rest right_rest
        else if left_id < right_id then
          loop acc left_rest right
        else
          loop acc left right_rest
    in
    loop [] left right

  let simple_attr_entity_collection db query =
    match query.find, query.where, query.rules, query.with_vars with
    | [ Find_var find_var ], patterns, [], [] when only_source_inputs query.inputs ->
      let rec collect_attrs acc = function
        | [] -> Some (List.rev acc)
        | Pattern (QVar entity_var, QAttr attr, QWildcard) :: rest when find_var = entity_var ->
          collect_attrs (attr :: acc) rest
        | _ -> None
      in
      (match collect_attrs [] patterns with
       | Some (first_attr :: rest_attrs) ->
         let entity_ids =
           List.fold_left
             (fun entity_ids attr -> intersect_sorted_entity_ids entity_ids (entity_ids_with_attr db attr))
             (entity_ids_with_attr db first_attr)
             rest_attrs
         in
         Some (Query_collection (List.map (fun entity_id -> Result_entity entity_id) entity_ids))
       | Some [] | None -> None)
    | _ -> None

  let simple_attr_entity_ids db find_var query =
    match query.where, query.rules, query.with_vars with
    | patterns, [], [] when only_source_inputs query.inputs ->
      let rec collect_patterns acc = function
        | [] -> Some (List.rev acc)
        | Pattern (QVar entity_var, QAttr attr, QWildcard) :: rest when find_var = entity_var ->
          collect_patterns (`Attr attr :: acc) rest
        | Pattern (QVar entity_var, QAttr attr, QValue value) :: rest when find_var = entity_var ->
          collect_patterns (`Value (attr, value) :: acc) rest
        | _ -> None
      in
      let entity_ids_for_pattern = function
        | `Attr attr -> entity_ids_with_attr db attr
        | `Value (attr, value) -> exact_attr_value_entity_ids db attr value
      in
      (match collect_patterns [] patterns with
       | Some (first_pattern :: rest_patterns) ->
         Some
           (List.fold_left
              (fun entity_ids pattern -> intersect_sorted_entity_ids entity_ids (entity_ids_for_pattern pattern))
              (entity_ids_for_pattern first_pattern)
              rest_patterns)
       | Some [] | None -> None)
    | _ -> None

  let simple_attr_entity_pull_collection db query =
    let cached lookup =
      let table = Hashtbl.create 128 in
      fun attr ->
        match Hashtbl.find_opt table attr with
        | Some value -> value
        | None ->
          let value = lookup db attr in
          Hashtbl.replace table attr value;
          value
    in
    let cardinality_cached = cached cardinality in
    let is_ref_attr_cached = cached is_ref_attr in
    let is_component_cached = cached is_component in
    let simple_pull_attrs selector =
      let rec collect acc = function
        | [] -> Some (List.rev acc)
        | Pull_id :: rest | Pull_attr "db/id" :: rest -> collect ("db/id" :: acc) rest
        | Pull_attr attr :: rest when (not (is_ref_attr_cached attr)) && not (is_component_cached attr) ->
          collect (attr :: acc) rest
        | _ -> None
      in
      collect [] selector
    in
    let pulled_value attr values =
      let scalar value = Pulled_scalar value in
      match cardinality_cached attr, values with
      | Many, [] -> None
      | Many, [ value ] -> Some (Pulled_many [ scalar value ])
      | Many, values -> Some (Pulled_many (values |> List.sort compare_value |> List.map scalar))
      | One, [] -> None
      | One, [ value ] -> Some (Pulled_scalar value)
      | One, first :: rest ->
        let value =
          List.fold_left
            (fun max_value value -> if compare_value max_value value < 0 then value else max_value)
            first
            rest
        in
        Some (Pulled_scalar value)
    in
    let simple_pull_values attrs entity_ids =
      let wanted = Bytes.make (Int64.to_int db.max_datom_e + 1) '\000' in
      List.iter
        (fun entity_id ->
          if entity_id >= 0L && entity_id < Int64.of_int (Bytes.length wanted) then
            Bytes.set wanted (Int64.to_int entity_id) '\001')
        entity_ids;
      let wanted_entity entity_id =
        entity_id >= 0L && entity_id < Int64.of_int (Bytes.length wanted) && Bytes.get wanted (Int64.to_int entity_id) = '\001'
      in
      let attr_tables =
        attrs
        |> List.filter (fun attr -> attr <> "db/id")
        |> List.sort_uniq compare
        |> List.map (fun attr ->
          let table = Hashtbl.create (List.length entity_ids) in
          Db_access_impl.datoms db Aevt ~a:attr ()
          |> Seq.iter (fun datom ->
            if wanted_entity datom.e then
              let values = Option.value (Hashtbl.find_opt table datom.e) ~default:[] in
              Hashtbl.replace table datom.e (datom.v :: values));
          attr, table)
      in
      entity_ids
      |> List.map (fun entity_id ->
        let pulled_attrs =
          attrs
          |> List.filter_map (fun attr ->
            if attr = "db/id" then
              Some (Keyword "db/id", Pulled_scalar (Int64 entity_id))
            else
              let values =
                Option.bind (List.assoc_opt attr attr_tables) (fun table -> Hashtbl.find_opt table entity_id)
                |> Option.value ~default:[]
              in
              Option.map (fun value -> Keyword attr, value) (pulled_value attr values))
          |> List.sort (fun (left, _) (right, _) -> compare_value left right)
        in
        Result_pull { pulled_id = entity_id; pulled_attrs })
      |> fun values -> Query_collection values
    in
    let direct_simple_pull_values attrs entity_ids =
      let wanted_attrs =
        attrs
        |> List.filter (fun attr -> attr <> "db/id")
        |> List.sort_uniq compare
      in
      let wanted_attr attr = List.mem attr wanted_attrs in
      let values_for_entity entity_id =
        let tables =
          Db_access_impl.datoms db Eavt ~e:entity_id ()
          |> Seq.fold_left
               (fun tables datom ->
                 if wanted_attr datom.a then
                   let values = Option.value (List.assoc_opt datom.a tables) ~default:[] in
                   (datom.a, datom.v :: values) :: List.remove_assoc datom.a tables
                 else
                   tables)
               []
        in
        attrs
        |> List.filter_map (fun attr ->
          if attr = "db/id" then
            Some (Keyword "db/id", Pulled_scalar (Int64 entity_id))
          else
            let values = Option.value (List.assoc_opt attr tables) ~default:[] in
            Option.map (fun value -> Keyword attr, value) (pulled_value attr values))
        |> List.sort (fun (left, _) (right, _) -> compare_value left right)
      in
      entity_ids
      |> List.filter_map (fun entity_id ->
        match values_for_entity entity_id with
        | [] -> None
        | pulled_attrs -> Some (Result_pull { pulled_id = entity_id; pulled_attrs }))
      |> fun values -> Query_collection values
    in
    let entity_id_set entity_ids =
      let wanted = Bytes.make (Int64.to_int db.max_datom_e + 1) '\000' in
      List.iter
        (fun entity_id ->
          if entity_id >= 0L && entity_id < Int64.of_int (Bytes.length wanted) then
            Bytes.set wanted (Int64.to_int entity_id) '\001')
        entity_ids;
      wanted
    in
    let attr_value_table attr entity_ids wanted =
      let wanted_entity entity_id =
        entity_id >= 0L && entity_id < Int64.of_int (Bytes.length wanted) && Bytes.get wanted (Int64.to_int entity_id) = '\001'
      in
      let table = Hashtbl.create (List.length entity_ids) in
      Db_access_impl.datoms db Aevt ~a:attr ()
      |> Seq.iter (fun datom ->
        if wanted_entity datom.e then
          let values = Option.value (Hashtbl.find_opt table datom.e) ~default:[] in
          Hashtbl.replace table datom.e (datom.v :: values));
      table
    in
    let ref_id_of_value attr = function
      | Ref entity_id -> Some entity_id
      | Int64 entity_id when is_ref_attr_cached attr -> Some entity_id
      | _ -> None
    in
    let batch_nested_pull_values selector entity_ids =
      if List.length entity_ids < 100 then
        None
      else
      let nested_attrs selector =
        let rec collect acc = function
          | [] -> Some (List.rev acc)
          | Pull_id :: rest | Pull_attr "db/id" :: rest -> collect ("db/id" :: acc) rest
          | Pull_attr attr :: rest when (not (is_ref_attr_cached attr)) && not (is_component_cached attr) ->
            collect (attr :: acc) rest
          | _ -> None
        in
        collect [] selector
      in
      let rec collect_roots acc = function
        | [] -> Some (List.rev acc)
        | Pull_id :: rest | Pull_attr "db/id" :: rest -> collect_roots (`Attr "db/id" :: acc) rest
        | Pull_attr attr :: rest when (not (is_ref_attr_cached attr)) && not (is_component_cached attr) ->
          collect_roots (`Attr attr :: acc) rest
        | Pull_ref (attr, selector) :: rest when is_ref_attr_cached attr ->
          (match nested_attrs selector with
           | Some nested -> collect_roots (`Ref (attr, List.sort compare nested) :: acc) rest
           | None -> None)
        | _ -> None
      in
      match collect_roots [] selector with
      | None -> None
      | Some roots ->
        let root_key = function
          | `Attr attr | `Ref (attr, _) -> attr
        in
        let roots = List.sort (fun left right -> compare (root_key left) (root_key right)) roots in
        let root_attrs =
          roots
          |> List.filter_map (function
            | `Attr "db/id" -> None
            | `Attr attr | `Ref (attr, _) -> Some attr)
          |> List.sort_uniq compare
        in
        let root_wanted = entity_id_set entity_ids in
        let root_tables = List.map (fun attr -> attr, attr_value_table attr entity_ids root_wanted) root_attrs in
        let root_values attr entity_id =
          Option.bind (List.assoc_opt attr root_tables) (fun table -> Hashtbl.find_opt table entity_id)
          |> Option.value ~default:[]
        in
        let referenced_ids =
          roots
          |> List.concat_map (function
            | `Ref (attr, _) ->
              entity_ids
              |> List.concat_map (fun entity_id ->
                root_values attr entity_id
                |> List.filter_map (ref_id_of_value attr))
            | `Attr _ -> [])
          |> List.sort_uniq compare
        in
        let nested_attr_names =
          roots
          |> List.concat_map (function
            | `Ref (_, attrs) -> attrs
            | `Attr _ -> [])
          |> List.filter (fun attr -> attr <> "db/id")
          |> List.sort_uniq compare
        in
        let nested_wanted = entity_id_set referenced_ids in
        let nested_tables =
          List.map (fun attr -> attr, attr_value_table attr referenced_ids nested_wanted) nested_attr_names
        in
        let nested_values attr entity_id =
          Option.bind (List.assoc_opt attr nested_tables) (fun table -> Hashtbl.find_opt table entity_id)
          |> Option.value ~default:[]
        in
        let nested_entity attrs entity_id =
          let pulled_attrs =
            attrs
            |> List.filter_map (fun attr ->
              if attr = "db/id" then
                Some (Keyword "db/id", Pulled_scalar (Int64 entity_id))
              else
                Option.map
                  (fun value -> Keyword attr, value)
                  (pulled_value attr (nested_values attr entity_id)))
          in
          Pulled_entity { pulled_id = entity_id; pulled_attrs }
        in
        let ref_value attr nested values =
          let ids =
            values
            |> List.sort compare_value
            |> List.filter_map (ref_id_of_value attr)
          in
          match cardinality_cached attr, ids with
          | Many, [] | One, [] -> None
          | Many, ids -> Some (Pulled_many (List.map (nested_entity nested) ids))
          | One, ids -> Some (nested_entity nested (List.hd (List.rev ids)))
        in
        let values =
        entity_ids
        |> List.map (fun entity_id ->
          let pulled_attrs =
            roots
            |> List.filter_map (function
              | `Attr "db/id" -> Some (Keyword "db/id", Pulled_scalar (Int64 entity_id))
              | `Attr attr ->
                Option.map (fun value -> Keyword attr, value) (pulled_value attr (root_values attr entity_id))
            | `Ref (attr, nested) ->
              Option.map (fun value -> Keyword attr, value) (ref_value attr nested (root_values attr entity_id)))
        in
          Result_pull { pulled_id = entity_id; pulled_attrs })
        in
        Some (Query_collection values)
    in
    let title_tags_ident_pull_values selector entity_ids =
      let matches =
        match selector with
        | [ Pull_attr "block/title"; Pull_ref ("block/tags", [ Pull_attr "db/ident" ]) ]
        | [ Pull_ref ("block/tags", [ Pull_attr "db/ident" ]); Pull_attr "block/title" ] ->
          true
        | _ -> false
      in
      if not matches then
        None
      else
        let ident_cache = Hashtbl.create 64 in
        let tag_ident entity_id =
          match Hashtbl.find_opt ident_cache entity_id with
          | Some value -> value
          | None ->
            let value =
              Db_access_impl.datoms db Eavt ~e:entity_id ~a:"db/ident" ()
              |> Seq.find_map (fun datom -> Some datom.v)
            in
            Hashtbl.replace ident_cache entity_id value;
            value
        in
        let tag_entity entity_id =
          match tag_ident entity_id with
          | Some ident ->
            Some
              (Pulled_entity
                 { pulled_id = entity_id
                 ; pulled_attrs = [ Keyword "db/ident", Pulled_scalar ident ]
                 })
          | None -> None
        in
        let values =
          entity_ids
          |> List.map (fun entity_id ->
            let title = ref None in
            let tags = ref [] in
            Db_access_impl.datoms db Eavt ~e:entity_id ()
            |> Seq.iter (fun datom ->
              match datom.a, datom.v with
              | "block/title", value -> title := Some value
              | "block/tags", Ref tag_id -> tags := tag_id :: !tags
              | _ -> ());
            let pulled_attrs =
              []
              |> (fun attrs ->
                match !tags |> List.sort_uniq compare |> List.filter_map tag_entity with
                | [] -> attrs
                | tags -> (Keyword "block/tags", Pulled_many tags) :: attrs)
              |> (fun attrs ->
                match !title with
                | Some title -> attrs @ [ Keyword "block/title", Pulled_scalar title ]
                | None -> attrs)
            in
            Result_pull { pulled_id = entity_id; pulled_attrs })
        in
        Some (Query_collection values)
    in
    let cached_pull_context =
      { pull_api_context with
        cardinality = (fun _ attr -> cardinality_cached attr)
      ; is_ref_attr = (fun _ attr -> is_ref_attr_cached attr)
      ; is_component = (fun _ attr -> is_component_cached attr)
      }
    in
    let pull_values selector entity_ids =
      match selector, simple_pull_attrs selector with
      | [ Pull_wildcard ], _ when List.length entity_ids >= 10_000 ->
        Pull_api_impl.pull_wildcard_many_by_ids pull_api_context db entity_ids
        |> List.map (fun entity -> Result_pull entity)
        |> fun values -> Query_collection values
      | [ Pull_wildcard ], _ ->
        entity_ids
        |> List.filter_map (fun entity_id ->
          Pull_api_impl.pull cached_pull_context db selector (Entity_id entity_id)
          |> Option.map (fun entity -> Result_pull entity))
        |> fun values -> Query_collection values
      | _, Some attrs
        when List.length entity_ids <= 32
             || (List.length entity_ids <= 512 && List.mem "block/title" attrs) ->
        direct_simple_pull_values attrs entity_ids
      | _, Some attrs -> simple_pull_values attrs entity_ids
      | _ ->
        (match title_tags_ident_pull_values selector entity_ids with
         | Some result -> result
         | None ->
        (match batch_nested_pull_values selector entity_ids with
         | Some result -> result
         | None ->
           entity_ids
           |> List.filter_map (fun entity_id ->
             Pull_api_impl.pull cached_pull_context db selector (Entity_id entity_id)
             |> Option.map (fun entity -> Result_pull entity))
           |> fun values -> Query_collection values))
    in
    match query.find with
    | [ Find_pull (find_var, selector) ] ->
      simple_attr_entity_ids db find_var query
      |> Option.map (pull_values selector)
    | [ Find_pull_form (find_var, pattern) ] ->
      let selector = parse_pull_pattern db pattern in
      simple_attr_entity_ids db find_var query
      |> Option.map (pull_values selector)
    | _ -> None

  let ref_target_pull_relation db query =
    let wildcard_selector = function
      | [ Pull_wildcard ] -> Some [ Pull_wildcard ]
      | _ -> None
    in
    let find_pull =
      match query.find with
      | [ Find_pull (find_var, selector) ] ->
        Option.map (fun selector -> find_var, selector) (wildcard_selector selector)
      | [ Find_pull_form (find_var, pattern) ] ->
        let selector = parse_pull_pattern db pattern in
        Option.map (fun selector -> find_var, selector) (wildcard_selector selector)
      | _ -> None
    in
    let missing_clause find_var = function
      | Missing (QVar var, QAttr attr)
      | SourceMissing ("$", QVar var, QAttr attr) when var = find_var ->
        Some attr
      | _ -> None
    in
    let ref_pattern find_var = function
      | Pattern (QVar source_var, QAttr attr, QVar target_var) when target_var = find_var ->
        Some (source_var, attr)
      | _ -> None
    in
    let required_pattern source_var = function
      | Pattern (QVar var, QAttr attr, QWildcard) when var = source_var -> Some attr
      | _ -> None
    in
    let entity_has_attr entity_id attr =
      Option.is_some (Seq.uncons (Db_access_impl.datoms db Eavt ~e:entity_id ~a:attr ()))
    in
    match find_pull, query.rules, query.with_vars, only_source_inputs query.inputs with
    | Some (find_var, [ Pull_wildcard ]), [], [], true ->
      let missing_attrs = List.filter_map (missing_clause find_var) query.where in
      let ref_patterns = List.filter_map (ref_pattern find_var) query.where in
      (match missing_attrs, ref_patterns with
       | [ missing_attr ], [ source_var, ref_attr ] ->
         let required_attrs = List.filter_map (required_pattern source_var) query.where in
         (match required_attrs with
          | [ required_attr ] ->
            let source_entities = Bytes.make (Int64.to_int db.max_datom_e + 1) '\000' in
            entity_ids_with_attr db required_attr
            |> List.iter (fun entity_id ->
              if entity_id >= 0L && entity_id < Int64.of_int (Bytes.length source_entities) then
                Bytes.set source_entities (Int64.to_int entity_id) '\001');
            let source_has_required entity_id =
              entity_id >= 0L
              && entity_id < Int64.of_int (Bytes.length source_entities)
              && Bytes.get source_entities (Int64.to_int entity_id) = '\001'
            in
            let target_ids =
              primary_attr_datoms db Aevt ref_attr
              |> List.filter_map (fun datom ->
                match datom.v with
                | Ref target_id when source_has_required datom.e && not (entity_has_attr target_id missing_attr) ->
                  Some target_id
                | _ -> None)
              |> List.sort_uniq compare
            in
            let rows =
              Pull_api_impl.pull_wildcard_many_by_ids pull_api_context db target_ids
              |> List.map (fun entity -> [ Result_pull entity ])
            in
            Some (Query_relation rows)
          | _ -> None)
       | _ -> None)
    | _ -> None

  let scalar_input_bindings db query inputs =
    let rec collect acc declarations args =
      match declarations, args with
      | [], _ -> Some (List.rev acc)
      | Input_source_decl _ :: rest, _ -> collect acc rest args
      | Input_rules_decl :: rest, Arg_rules _ :: args -> collect acc rest args
      | Input_scalar_decl var :: rest, Arg_scalar value :: args -> collect ((var, value) :: acc) rest args
      | Input_scalar_decl var :: rest, Arg_entity_ref entity_ref :: args ->
        let value =
          match entity_id_of_ref db entity_ref with
          | Some entity_id -> Result_entity entity_id
          | None -> Result_value Nil
        in
        collect ((var, value) :: acc) rest args
      | (_ :: rest), (_ :: args) -> collect acc rest args
      | _ :: _, [] -> None
    in
    collect [] query.inputs inputs

  let input_rules query inputs =
    let rec collect declarations args =
      match declarations, args with
      | [], _ -> Some []
      | Input_source_decl _ :: rest, _ -> collect rest args
      | Input_rules_decl :: rest, Arg_rules rules :: args ->
        Option.map (fun rest_rules -> List.map norm_rule rules @ rest_rules) (collect rest args)
      | (_ :: rest), (_ :: args) -> collect rest args
      | _ :: _, [] -> None
    in
    collect query.inputs inputs

  let value_of_query_result = function
    | Result_value value -> Some value
    | Result_entity entity_id -> Some (Ref entity_id)
    | Result_attr attr -> Some (Keyword attr)
    | Result_db _ | Result_pull _ -> None

  let exact_title_input_entity_ids db inputs query find_var =
    let title_pattern find_var = function
      | Pattern (QVar entity_var, QAttr "block/title", QVar title_var) when entity_var = find_var ->
        Some title_var
      | _ -> None
    in
    match scalar_input_bindings db query inputs, query.rules, query.with_vars with
    | Some input_bindings, [], [] ->
      (match List.filter_map (title_pattern find_var) query.where with
       | [ title_var ] ->
         (match List.assoc_opt title_var input_bindings with
          | Some (Result_value (String _)) -> None
          | Some _ -> Some []
          | None -> None)
       | _ -> None)
    | _ -> None

  let exact_title_scalar_query db inputs query =
    let find_var =
      match query.find with
      | [ Find_var find_var ] -> Some find_var
      | _ -> None
    in
    match find_var with
    | Some find_var ->
      (match exact_title_input_entity_ids db inputs query find_var with
       | Some [] -> Some (Query_scalar None)
       | Some entity_ids when List.length query.where = 1 ->
         let value =
           entity_ids
           |> List.sort compare
           |> List.find_map (fun entity_id -> Some (Result_entity entity_id))
         in
         Some (Query_scalar value)
       | _ -> None)
    | None -> None

  let exact_title_pull_collection db inputs query =
    let find_pull =
      match query.find with
      | [ Find_pull (find_var, _) ] -> Some find_var
      | [ Find_pull_form (find_var, _) ] -> Some find_var
      | _ -> None
    in
    match find_pull with
    | Some find_var ->
      (match exact_title_input_entity_ids db inputs query find_var with
       | Some [] -> Some (Query_collection [])
       | _ -> None)
    | None -> None

  let bound_entity_required_pull_relation db inputs query =
    let find_pull =
      match query.find with
      | [ Find_pull (find_var, selector) ] -> Some (find_var, selector)
      | [ Find_pull_form (find_var, pattern) ] -> Some (find_var, parse_pull_pattern db pattern)
      | _ -> None
    in
    let required_attr find_var = function
      | Pattern (QVar entity_var, QAttr attr, QWildcard) when entity_var = find_var -> Some attr
      | _ -> None
    in
    match find_pull, scalar_input_bindings db query inputs, query.rules, query.with_vars with
    | Some (find_var, selector), Some input_bindings, [], [] ->
      (match List.assoc_opt find_var input_bindings, List.filter_map (required_attr find_var) query.where with
       | Some input, [ attr ] ->
         (match query_result_entity_id db input with
          | Some entity_id ->
            let has_attr = Option.is_some (Seq.uncons (Db_access_impl.datoms db Eavt ~e:entity_id ~a:attr ())) in
            if has_attr then
              let rows =
                pull db selector (Entity_id entity_id)
                |> Option.map (fun entity -> [ [ Result_pull entity ] ])
                |> Option.value ~default:[]
              in
              Some (Query_relation rows)
            else
              Some (Query_relation [])
          | None -> Some (Query_relation []))
       | _ -> None)
    | _ -> None

  let bounded_timestamp_pull_relation db inputs query =
    let find_pull =
      match query.find with
      | [ Find_pull (find_var, selector) ] -> Some (find_var, selector)
      | [ Find_pull_form (find_var, pattern) ] ->
        Some (find_var, parse_pull_pattern db pattern)
      | _ -> None
    in
    let entity_pattern find_var = function
      | Pattern (QVar entity_var, QAttr attr, QVar value_var) when entity_var = find_var ->
        Some (`Value (attr, value_var))
      | Pattern (QVar entity_var, QAttr attr, QWildcard) when entity_var = find_var ->
        Some (`Required attr)
      | _ -> None
    in
    let missing_clause find_var = function
      | Missing (QVar var, QAttr attr)
      | SourceMissing ("$", QVar var, QAttr attr) when var = find_var ->
        Some attr
      | _ -> None
    in
    let lower_bound timestamp_var input_bindings = function
      | ComparisonPredicate (GreaterOrEqual, QVar var, QVar input_var) when var = timestamp_var ->
        Option.bind (List.assoc_opt input_var input_bindings) value_of_query_result
      | ComparisonPredicateN (GreaterOrEqual, [ QVar var; QVar input_var ]) when var = timestamp_var ->
        Option.bind (List.assoc_opt input_var input_bindings) value_of_query_result
      | _ -> None
    in
    let upper_bound timestamp_var input_bindings = function
      | ComparisonPredicate (LessOrEqual, QVar var, QVar input_var) when var = timestamp_var ->
        Option.bind (List.assoc_opt input_var input_bindings) value_of_query_result
      | ComparisonPredicateN (LessOrEqual, [ QVar var; QVar input_var ]) when var = timestamp_var ->
        Option.bind (List.assoc_opt input_var input_bindings) value_of_query_result
      | _ -> None
    in
    let entity_set_for_attr attr =
      let entities = Bytes.make (Int64.to_int db.max_datom_e + 1) '\000' in
      entity_ids_with_attr db attr
      |> List.iter (fun entity_id ->
        if entity_id >= 0L && entity_id < Int64.of_int (Bytes.length entities) then
          Bytes.set entities (Int64.to_int entity_id) '\001');
      entities
    in
    let entity_in_set entities entity_id =
      entity_id >= 0L && entity_id < Int64.of_int (Bytes.length entities) && Bytes.get entities (Int64.to_int entity_id) = '\001'
    in
    let simple_pull_attrs selector =
      let rec collect acc = function
        | [] -> Some (List.rev acc)
        | Pull_id :: rest | Pull_attr "db/id" :: rest -> collect ("db/id" :: acc) rest
        | Pull_attr attr :: rest when (not (is_ref_attr db attr)) && not (is_component db attr) ->
          collect (attr :: acc) rest
        | _ -> None
      in
      collect [] selector
    in
    let simple_pulled_rows attrs entity_ids =
      let wanted = Bytes.make (Int64.to_int db.max_datom_e + 1) '\000' in
      List.iter
        (fun entity_id ->
          if entity_id >= 0L && entity_id < Int64.of_int (Bytes.length wanted) then
            Bytes.set wanted (Int64.to_int entity_id) '\001')
        entity_ids;
      let wanted_entity entity_id =
        entity_id >= 0L && entity_id < Int64.of_int (Bytes.length wanted) && Bytes.get wanted (Int64.to_int entity_id) = '\001'
      in
      let attr_tables =
        attrs
        |> List.filter (fun attr -> attr <> "db/id")
        |> List.sort_uniq compare
        |> List.map (fun attr ->
          let table = Hashtbl.create (List.length entity_ids) in
          Db_access_impl.datoms db Aevt ~a:attr ()
          |> Seq.iter (fun datom ->
            if wanted_entity datom.e then
              Hashtbl.replace table datom.e datom.v);
          attr, table)
      in
      entity_ids
      |> List.map (fun entity_id ->
        let pulled_attrs =
          attrs
          |> List.filter_map (fun attr ->
            if attr = "db/id" then
              Some (Keyword "db/id", Pulled_scalar (Int64 entity_id))
            else
              Option.bind (List.assoc_opt attr attr_tables) (fun table -> Hashtbl.find_opt table entity_id)
              |> Option.map (fun value -> Keyword attr, Pulled_scalar value))
          |> List.sort (fun (left, _) (right, _) -> compare_value left right)
        in
        [ Result_pull { pulled_id = entity_id; pulled_attrs } ])
    in
    let term_value bindings = function
      | QVar var -> Option.bind (List.assoc_opt var bindings) value_of_query_result
      | QValue value -> Some value
      | QEntity entity_id -> Some (Ref entity_id)
      | QAttr attr -> Some (Keyword attr)
      | QWildcard | QIdent _ | QLookupRef _ | QSource _ -> None
    in
    let derived_input_bindings input_bindings =
      query.where
      |> List.fold_left
           (fun bindings -> function
             | ArithmeticValue (op, terms, output_var) ->
               let values =
                 terms
                 |> List.fold_left
                      (fun values term ->
                        match values with
                        | None -> None
                        | Some values -> Option.map (fun value -> value :: values) (term_value bindings term))
                      (Some [])
                 |> Option.map List.rev
               in
               (match Option.bind values (Built_ins.eval_arithmetic op) with
                | Some value -> (output_var, Result_value value) :: List.remove_assoc output_var bindings
                | None -> bindings)
             | _ -> bindings)
           input_bindings
    in
    match find_pull, scalar_input_bindings db query inputs, query.rules, query.with_vars with
    | Some (find_var, selector), Some input_bindings, [], [] ->
      let input_bindings = derived_input_bindings input_bindings in
      let patterns = List.filter_map (entity_pattern find_var) query.where in
      let missing_attrs = List.filter_map (missing_clause find_var) query.where in
      let value_patterns =
        patterns
        |> List.filter_map (function
          | `Value (attr, value_var) -> Some (attr, value_var)
          | `Required _ -> None)
      in
      let required_attrs =
        patterns
        |> List.filter_map (function
          | `Required attr -> Some attr
          | `Value _ -> None)
      in
      (match value_patterns, missing_attrs with
       | _ :: _, ([] | [ _ ]) ->
         let timestamp_pattern =
           value_patterns
           |> List.find_map (fun (attr, value_var) ->
             let lower = List.find_map (lower_bound value_var input_bindings) query.where in
             let upper = List.find_map (upper_bound value_var input_bindings) query.where in
             match lower, upper with
             | Some _, _ | _, Some _ -> Some (attr, value_var, lower, upper)
             | _ -> None)
         in
         (match timestamp_pattern with
          | Some (timestamp_attr, timestamp_var, lower, upper) ->
            let required_attrs =
              value_patterns
              |> List.fold_left
                   (fun attrs (attr, value_var) ->
                     if value_var = timestamp_var then attrs else attr :: attrs)
                   required_attrs
            in
            let required_sets = List.map entity_set_for_attr required_attrs in
            let missing_set =
              match missing_attrs with
              | [ missing_attr ] -> Some (entity_set_for_attr missing_attr)
              | [] -> None
              | _ -> None
            in
            let timestamp_datoms =
              match lower, upper with
              | Some lower, Some upper when values_compare_equal_fast lower upper ->
                search_attr_value db timestamp_attr lower
              | Some lower, _ ->
                if Db_access_impl.is_avet_accessible db timestamp_attr then
                  Db_access_impl.index_range db timestamp_attr ~start:lower ()
                else
                  Db_access_impl.search_datoms db Aevt ~a:timestamp_attr ()
                  |> Seq.filter (fun datom -> compare_value datom.v lower >= 0)
              | _, Some upper ->
                if Db_access_impl.is_avet_accessible db timestamp_attr then
                  Db_access_impl.index_range db timestamp_attr ~stop:upper ()
                else
                  Db_access_impl.search_datoms db Aevt ~a:timestamp_attr ()
                  |> Seq.filter (fun datom -> compare_value datom.v upper <= 0)
              | None, None -> Seq.empty
            in
            let entity_ids =
              timestamp_datoms
              |> Seq.filter_map (fun datom ->
                if
                  List.for_all (fun entities -> entity_in_set entities datom.e) required_sets
                  &&
                  (match missing_set with
                   | Some missing_set -> not (entity_in_set missing_set datom.e)
                   | None -> true)
                then
                  Some datom.e
                else
                  None)
              |> List.of_seq
              |> List.sort_uniq compare
            in
            let rows =
              match selector, simple_pull_attrs selector with
              | [ Pull_wildcard ], _ when List.length entity_ids >= 100 ->
                Pull_api_impl.pull_wildcard_many_by_ids pull_api_context db entity_ids
                |> List.map (fun entity -> [ Result_pull entity ])
              | [ Pull_wildcard ], _ ->
                entity_ids
                |> List.filter_map (fun entity_id -> pull db selector (Entity_id entity_id))
                |> List.map (fun entity -> [ Result_pull entity ])
              | _, Some attrs -> simple_pulled_rows attrs entity_ids
              | _ ->
                entity_ids
                |> List.filter_map (fun entity_id -> pull db selector (Entity_id entity_id))
                |> List.map (fun entity -> [ Result_pull entity ])
            in
            Some (Query_relation rows)
          | _ -> None)
       | _ -> None)
    | _ -> None

  let title_includes_rule_relation db inputs query =
    let find_var =
      match query.find with
      | [ Find_var find_var ] -> Some find_var
      | _ -> None
    in
    let rule_call find_var = function
      | Rule (rule_name, [ QVar entity_var; QVar query_var ]) when entity_var = find_var ->
        Some (rule_name, query_var)
      | _ -> None
    in
    let block_content_rule rule_name query_param rule =
      if rule.rule_name <> rule_name then
        false
      else
        match rule.rule_params with
        | [ entity_param; rule_query_param ] when rule_query_param = query_param ->
          let title_vars =
            rule.rule_body
            |> List.filter_map (function
              | Pattern (QVar entity_var, QAttr "block/title", QVar title_var) when entity_var = entity_param ->
                Some title_var
              | _ -> None)
          in
          (match title_vars with
           | [ title_var ] ->
             List.exists
               (function
                 | StringIncludesValue (QVar left, QVar right) when left = title_var && right = rule_query_param ->
                   true
                 | _ -> false)
               rule.rule_body
           | _ -> false)
        | _ -> false
    in
    match find_var, scalar_input_bindings db query inputs, input_rules query inputs, query.rules, query.with_vars with
    | Some find_var, Some input_bindings, Some input_rules, [], [] ->
      (match List.filter_map (rule_call find_var) query.where with
       | [ rule_name, query_var ] ->
         (match List.assoc_opt query_var input_bindings with
          | Some (Result_value (String query_text))
            when List.exists (block_content_rule rule_name query_var) input_rules ->
            let rec collect acc = function
              | [] -> acc
              | datom :: rest ->
                let acc =
                  match datom.v with
                  | String title when string_includes_prefilter title query_text -> datom.e :: acc
                  | _ -> acc
                in
                collect acc rest
            in
            let rows =
              collect [] (primary_attr_datoms db Aevt "block/title")
              |> fun entity_ids ->
              collect entity_ids (Option.value (Hashtbl.find_opt db.duplicate_aevt_by_attr "block/title") ~default:[])
              |> List.sort_uniq compare
              |> List.map (fun entity_id -> [ Result_entity entity_id ])
            in
            Some (Query_relation rows)
          | _ -> None)
       | _ -> None)
    | _ -> None

  let title_includes_pull_collection db inputs query =
    let find_pull =
      match query.find with
      | [ Find_pull (find_var, selector) ] -> Some (find_var, selector)
      | [ Find_pull_form (find_var, pattern) ] -> Some (find_var, parse_pull_pattern db pattern)
      | _ -> None
    in
    let input_bindings = scalar_input_bindings db query inputs in
    let class_pattern find_var = function
      | Pattern (QVar var, QAttr "block/tags", QValue (Keyword class_ident)) when var = find_var ->
        Some class_ident
      | _ -> None
    in
    let title_pattern find_var = function
      | Pattern (QVar var, QAttr "block/title", QVar title_var) when var = find_var -> Some title_var
      | _ -> None
    in
    let lower_clause input_var = function
      | StringLowerCaseValue (QVar var, output_var) when var = input_var -> Some output_var
      | _ -> None
    in
    let includes_clause title_lower query_lower = function
      | StringIncludesValue (QVar left, QVar right) when left = title_lower && right = query_lower -> true
      | _ -> false
    in
    match find_pull, input_bindings, query.rules, query.with_vars with
    | Some (find_var, selector), Some input_bindings, [], [] ->
      let class_idents = List.filter_map (class_pattern find_var) query.where in
      let title_vars = List.filter_map (title_pattern find_var) query.where in
      (match class_idents, title_vars with
       | [ class_ident ], [ title_var ] ->
         let query_vars =
           query.inputs
           |> List.filter_map (function
             | Input_scalar_decl var -> Some var
             | _ -> None)
         in
         let query_var =
           query_vars
           |> List.find_opt (fun var -> var <> title_var)
         in
         (match query_var with
          | None -> None
          | Some query_var ->
            let title_lower = List.find_map (lower_clause title_var) query.where in
            let query_lower = List.find_map (lower_clause query_var) query.where in
            (match title_lower, query_lower, List.assoc_opt query_var input_bindings with
             | Some title_lower, Some query_lower, Some (Result_value (String query_text))
               when List.exists (includes_clause title_lower query_lower) query.where ->
               let query_text = String.lowercase_ascii query_text in
               let class_id = entity_id_of_ref db (Ident class_ident) in
               (match class_id with
                | None -> Some (Query_collection [])
                | Some class_id ->
                  let tagged =
                    search_attr_value db "block/tags" (Ref class_id)
                    |> Seq.map (fun datom -> datom.e)
                    |> List.of_seq
                    |> List.sort_uniq compare
                  in
                  let wanted = Bytes.make (Int64.to_int db.max_datom_e + 1) '\000' in
                  List.iter
                    (fun entity_id ->
                      if entity_id >= 0L && entity_id < Int64.of_int (Bytes.length wanted) then
                        Bytes.set wanted (Int64.to_int entity_id) '\001')
                    tagged;
                  let wanted_entity entity_id =
                    entity_id >= 0L && entity_id < Int64.of_int (Bytes.length wanted) && Bytes.get wanted (Int64.to_int entity_id) = '\001'
                  in
                  let entity_ids =
                    Db_access_impl.datoms db Aevt ~a:"block/title" ()
                    |> Seq.filter_map (fun datom ->
                      match datom.v with
                      | String title when wanted_entity datom.e && string_includes_prefilter (String.lowercase_ascii title) query_text ->
                        Some datom.e
                      | _ -> None)
                    |> List.of_seq
                    |> List.sort_uniq compare
                  in
                  entity_ids
                  |> List.filter_map (fun entity_id ->
                    pull db selector (Entity_id entity_id)
                    |> Option.map (fun entity -> Result_pull entity))
                  |> fun values -> Some (Query_collection values))
             | _ -> None))
       | _ -> None)
    | _ -> None

  let q_return ?inputs db return query =
    match return, inputs with
    | Return_collection, None ->
      (match simple_attr_entity_collection db query with
       | Some result -> result
       | None ->
         (match simple_attr_entity_pull_collection db query with
          | Some result -> result
          | None ->
            let rows = q db query in
            rows
            |> List.filter_map (function
              | value :: _ -> Some value
              | [] -> None)
            |> fun values -> Query_collection values))
    | Return_collection, Some inputs ->
      (match exact_title_pull_collection db inputs query with
       | Some result -> result
       | None ->
      (match title_includes_pull_collection db inputs query with
       | Some result -> result
       | None ->
      (match bounded_timestamp_pull_relation db inputs query with
       | Some (Query_relation rows) ->
         rows
         |> List.filter_map (function
           | value :: _ -> Some value
           | [] -> None)
         |> fun values -> Query_collection values
       | Some result -> result
       | None ->
         let rows = q ~inputs db query in
         rows
         |> List.filter_map (function
           | value :: _ -> Some value
           | [] -> None)
         |> fun values -> Query_collection values)))
    | Return_relation, None ->
      (match simple_attr_entity_pull_collection db query with
       | Some (Query_collection values) -> Query_relation (List.map (fun value -> [ value ]) values)
       | Some result -> result
       | None ->
      (match ref_target_pull_relation db query with
       | Some result -> result
       | None ->
         let rows = q db query in
         Query_relation rows))
    | Return_relation, Some inputs ->
      (match bound_entity_required_pull_relation db inputs query with
       | Some result -> result
       | None ->
      (match title_includes_rule_relation db inputs query with
       | Some result -> result
       | None ->
      (match bounded_timestamp_pull_relation db inputs query with
       | Some result -> result
       | None ->
         let rows = q ~inputs db query in
         Query_relation rows)))
    | Return_scalar, Some inputs ->
      (match exact_title_scalar_query db inputs query with
       | Some result -> result
       | None ->
         let rows = q ~inputs db query in
         let value =
           Option.bind
             (List.nth_opt rows 0)
             (function
               | value :: _ -> Some value
               | [] -> None)
         in
         Query_scalar value)
    | _ ->
      let rows = q ?inputs db query in
    (match return with
    | Return_relation -> Query_relation rows
    | Return_collection ->
      rows
      |> List.filter_map (function
        | value :: _ -> Some value
        | [] -> None)
      |> fun values -> Query_collection values
    | Return_tuple -> Query_tuple (List.nth_opt rows 0)
    | Return_scalar ->
      let value =
        Option.bind
          (List.nth_opt rows 0)
          (function
            | value :: _ -> Some value
            | [] -> None)
      in
      Query_scalar value)

  let q_return_string ?inputs db input =
    let return, query = parse_query_return_string_with_pull_context ~default_pull_db:db input in
    q_return ?inputs db return query

  let labels_of_return_map = function
    | Return_keys labels -> List.map (fun label -> Keyword label) labels
    | Return_syms labels -> List.map (fun label -> Symbol label) labels
    | Return_strs labels -> List.map (fun label -> String label) labels

  let map_query_row labels row =
    if List.length labels <> List.length row then
      invalid_arg "return map labels must match find count";
    List.combine labels row |> List.sort (fun (left, _) (right, _) -> compare_value left right)

  let q_return_map ?inputs db return return_map query =
    let labels = labels_of_return_map return_map in
    let rows = q ?inputs db query in
    match return with
    | Return_relation ->
      rows
      |> List.map (map_query_row labels)
      |> fun rows -> Query_relation_maps rows
    | Return_tuple ->
      List.nth_opt rows 0
      |> Option.map (map_query_row labels)
      |> fun row -> Query_tuple_map row
    | Return_collection | Return_scalar ->
      invalid_arg "return maps require relation or tuple query returns"

  let q_return_map_string ?inputs db input =
    let return, return_map, query =
      parse_query_return_map_string_with_pull_context ~default_pull_db:db input
    in
    match return_map with
    | Some return_map -> q_return_map ?inputs db return return_map query
    | None -> q_return ?inputs db return query


let db_datoms = datoms
let db_fold_datoms = fold_datoms
let db_datoms_ref = datoms_ref
let db_find_datom = find_datom
let db_find_datom_ref = find_datom_ref
let db_seek_datoms = seek_datoms
let db_seek_datoms_ref = seek_datoms_ref
let db_rseek_datoms = rseek_datoms
let db_rseek_datoms_ref = rseek_datoms_ref
let db_index_range = index_range

module Db_base = struct
  include Db_impl

  let datoms = db_datoms
  let fold_datoms = db_fold_datoms
  let datoms_ref = db_datoms_ref
  let find_datom = db_find_datom
  let find_datom_ref = db_find_datom_ref
  let seek_datoms = db_seek_datoms
  let seek_datoms_ref = db_seek_datoms_ref
  let rseek_datoms = db_rseek_datoms
  let rseek_datoms_ref = db_rseek_datoms_ref
  let index_range = db_index_range
end

(* ====================================================================== *)
(* Public API layer                                                        *)
(*                                                                       *)
(* The modules below are the curated public surface. They wrap or shadow  *)
(* the implementation-level aliases defined earlier in this file; the     *)
(* full implementation modules remain reachable under [Internal].         *)
(* ====================================================================== *)

module Internal = struct
  module Datascript_types = Datascript_types
  module PSet = PSet
  module Built_ins = Built_ins
  module Util = Util
  module Lru = Lru
  module Lookup_refs = Lookup_refs
  module Schema = Schema
  module Schema_access = Schema_access
  module Serialize = Serialize
  module Conn = Conn
  module Db = Db_impl
  module Entity = Entity
  module Storage = Storage
  module Transact = Transact
  module Transact_datoms = Transact_datoms
  module Db_access = Db_access
  module Entity_refs = Entity_refs
  module Query = Query_impl
  module Query_runtime = Query_runtime
  module Query_where = Query_where
  module Query_api = Query_api
  module Query_eval = Query_eval
  module Parser = Parser_impl
  module Pull_parser = Pull_parser_impl
  module Pull_api = Pull_api_impl
  module Data_readers = Data_readers_impl
  module Upsert = Upsert

  let default_parser_context = parser_query_context
  let default_pull_parser_context = pull_parser_context
  let default_pull_api_context = pull_api_context
  let default_data_readers_context = data_readers_context
  let default_entity_context = entity_context
  let default_storage_tail_context = storage_tail_context
  let default_storage_restore_context = storage_restore_context
  let empty_db ?(schema = []) ?storage () = empty_db ~schema ?storage ()
  let init_db ?(schema = []) ?storage datoms = init_db ~schema ?storage datoms
  let db_with tx_ops db = db_with tx_ops db
  let create_conn ?(schema = []) ?storage () = create_conn ~schema ?storage ()
  let conn_from_datoms ?(schema = []) ?storage datoms = conn_from_datoms ~schema ?storage datoms
  let conn_db = db
  let db_hash = db_hash
  let q = q
  let q_string = q_string
  let q_with = q_with
  let q_with_string = q_with_string
  let q_sources = q_sources
  let q_sources_string = q_sources_string
  let q_return = q_return
  let q_return_string = q_return_string
  let q_return_map = q_return_map
  let q_return_map_string = q_return_map_string
  let datoms = Db_access_impl.datoms
  let entid = entid
  let entid_ref = entid_ref
  let schema = schema
  let with_tx = with_tx
  let transact = transact
  let transact_conn = transact_conn
  let pull = pull
  let pull_many = pull_many
  let entity = entity
  let parse_query = parse_query
  let parse_query_string = parse_query_string
  let serializable = serializable
  let from_serializable = from_serializable
  let store = store
  let store_tail = store_tail
  let memory_storage = memory_storage
  let file_storage = file_storage
  let storage_addresses = storage_addresses
  let restore = restore
  let tail_compaction_threshold = Storage.tail_compaction_threshold
  let tail_datom_count = Storage.tail_datom_count
  let restore_root_snapshot = Storage.restore_root_snapshot
  let restore_tail_groups = restore_tail_groups
  let db_with_tail = db_with_tail
  let collect_garbage = collect_garbage
  let datom = Db_impl.datom
  let filter = filter
  let is_reverse_ref = is_reverse_ref
  let reverse_ref = reverse_ref
  let settings = settings
  let restore_conn = restore_conn
  let conn_from_db = conn_from_db
end

(* Semantic wrapper types: inside the library every domain is a plain
   [int]/[string]; the private declarations live in the interface. *)

module Entity_id = struct
  type t = int64

  let of_int64 (x : int64) : t = x
  let to_int64 (x : t) : int64 = x
  let of_int = Int64.of_int
  let to_int = Int64.to_int
  let equal = Int64.equal
  let compare = Int64.compare
end

module Tx_id = struct
  type t = int64

  let of_int64 (x : int64) : t = x
  let to_int64 (x : t) : int64 = x
  let of_int = Int64.of_int
  let to_int = Int64.to_int
  let equal = Int64.equal
  let compare = Int64.compare
end

let eid = Entity_id.of_int64
let txid = Tx_id.of_int64

let var_sigil_string name = "?" ^ normalize_var name
let source_sigil_string name =
  let name = normalize_source name in
  if name = "$" then "$" else "$" ^ name

let query_form_of_term = function
  | QVar name -> QueryFormSymbol (var_sigil_string name)
  | QEntity id -> QueryFormInt id
  | QIdent name -> QueryFormKeyword name
  | QAttr name -> QueryFormKeyword name
  | QValue value -> Parser_impl.query_form_of_value value
  | QLookupRef (attr, value) ->
    QueryFormVector [ QueryFormKeyword attr; Parser_impl.query_form_of_value value ]
  | QSource name -> QueryFormSymbol (source_sigil_string name)
  | QWildcard -> QueryFormSymbol "_"

let rec query_form_of_binding = function
  | Bind_scalar name -> QueryFormSymbol (var_sigil_string name)
  | Bind_ignore -> QueryFormSymbol "_"
  | Bind_collection binding ->
    QueryFormVector [ query_form_of_binding binding; QueryFormSymbol "..." ]
  | Bind_tuple bindings -> QueryFormVector (List.map query_form_of_binding bindings)

module Clause = struct
  type t = query_clause

  let of_form form = Parser_impl.parse_pattern_clause parser_query_context form

  let binding_of_out_vars = function
    | [ var ] -> Bind_scalar var
    | vars -> Bind_tuple (List.map (fun var -> Bind_scalar var) vars)

  let scalar_binding var = Bind_scalar var

  let scalar_pred name args = Pred_view { call_fn = name; call_args = args }
  let scalar_fn name args out = Fn_view ({ call_fn = name; call_args = args }, scalar_binding out)

  let value_predicate_name = function
    | NumberValue -> "number?"
    | IntegerValue -> "integer?"
    | StringValue -> "string?"
    | BooleanValue -> "boolean?"
    | KeywordValue -> "keyword?"

  let numeric_predicate_name = function
    | ZeroNumber -> "zero?"
    | PositiveNumber -> "pos?"
    | NegativeNumber -> "neg?"
    | EvenInteger -> "even?"
    | OddInteger -> "odd?"

  let comparison_predicate_name = function
    | LessThan -> "<"
    | GreaterThan -> ">"
    | LessOrEqual -> "<="
    | GreaterOrEqual -> ">="

  let arithmetic_op_name = function
    | AddNumbers -> "+"
    | SubtractNumbers -> "-"
    | MultiplyNumbers -> "*"
    | DivideNumbers -> "/"
    | QuotientNumbers -> "quot"
    | RemainderNumbers -> "rem"
    | ModuloNumbers -> "mod"
    | IncrementNumber -> "inc"
    | DecrementNumber -> "dec"

  let boolean_predicate_name = function
    | TrueValue -> "true?"
    | FalseValue -> "false?"
    | NilValue -> "nil?"
    | SomeValue -> "some?"

  let view clause =
    match (clause : t) with
    | Pattern (e, a, v) ->
      Pattern_view { pattern_e = e; pattern_a = a; pattern_v = v; pattern_tx = None; pattern_op = None }
    | PatternTx (e, a, v, tx) ->
      Pattern_view
        { pattern_e = e; pattern_a = a; pattern_v = v; pattern_tx = Some tx; pattern_op = None }
    | PatternTxOp (e, a, v, tx, op) ->
      Pattern_view
        { pattern_e = e
        ; pattern_a = a
        ; pattern_v = v
        ; pattern_tx = Some tx
        ; pattern_op = Some op
        }
    | SourcePattern (source, e, a, v) -> Source_view (source, Pattern (e, a, v))
    | SourcePatternTx (source, e, a, v, tx) -> Source_view (source, PatternTx (e, a, v, tx))
    | SourcePatternTxOp (source, e, a, v, tx, op) ->
      Source_view (source, PatternTxOp (e, a, v, tx, op))
    | SourceRelationPattern ("$", terms) -> Relation_view terms
    | SourceRelationPattern (source, terms) ->
      Source_view (source, SourceRelationPattern ("$", terms))
    | Missing (e, a) -> scalar_pred "missing?" [ e; a ]
    | SourceMissing (source, e, a) -> Source_view (source, Missing (e, a))
    | GetElse (e, a, default, out) -> scalar_fn "get-else" [ e; a; default ] out
    | SourceGetElse (source, e, a, default, out) ->
      Source_view (source, GetElse (e, a, default, out))
    | GetSome (e, attrs, attr_var, value_var) ->
      Fn_view
        ( { call_fn = "get-some"; call_args = e :: attrs }
        , Bind_tuple [ Bind_scalar attr_var; Bind_scalar value_var ] )
    | SourceGetSome (source, e, attrs, attr_var, value_var) ->
      Source_view (source, GetSome (e, attrs, attr_var, value_var))
    | GetValue (m, k, out) -> scalar_fn "get" [ m; k ] out
    | GetDefaultValue (m, k, default, out) -> scalar_fn "get" [ m; k; default ] out
    | CountValue (term, out) -> scalar_fn "count" [ term ] out
    | EmptyValue term -> scalar_pred "empty?" [ term ]
    | NotEmptyValue term -> scalar_pred "not-empty?" [ term ]
    | ContainsValue (collection, key) -> scalar_pred "contains?" [ collection; key ]
    | ValuePredicate (predicate, term) -> scalar_pred (value_predicate_name predicate) [ term ]
    | NumericPredicate (predicate, term) -> scalar_pred (numeric_predicate_name predicate) [ term ]
    | ComparisonPredicate (predicate, left, right) ->
      scalar_pred (comparison_predicate_name predicate) [ left; right ]
    | ComparisonPredicateN (predicate, terms) ->
      scalar_pred (comparison_predicate_name predicate) terms
    | EqualityPredicate (EqualValues, terms) -> scalar_pred "=" terms
    | EqualityPredicate (NotEqualValues, terms) -> scalar_pred "!=" terms
    | ArithmeticValue (op, terms, out) -> scalar_fn (arithmetic_op_name op) terms out
    | CompareValue (left, right, out) -> scalar_fn "compare" [ left; right ] out
    | ExtremumValue (MinimumValue, terms, out) -> scalar_fn "min" terms out
    | ExtremumValue (MaximumValue, terms, out) -> scalar_fn "max" terms out
    | BooleanPredicate (predicate, term) ->
      scalar_pred (boolean_predicate_name predicate) [ term ]
    | BooleanNotPredicate term -> scalar_pred "not" [ term ]
    | BooleanNotValue (term, out) -> scalar_fn "not" [ term ] out
    | IdentityValue (term, out) -> scalar_fn "identity" [ term ] out
    | BooleanAndPredicate terms -> scalar_pred "and" terms
    | BooleanAndValue (terms, out) -> scalar_fn "and" terms out
    | BooleanOrPredicate terms -> scalar_pred "or" terms
    | BooleanOrValue (terms, out) -> scalar_fn "or" terms out
    | RandomValue out -> scalar_fn "rand" [] out
    | RandomIntValue (bound, out) -> scalar_fn "rand-int" [ bound ] out
    | DifferPredicate terms -> scalar_pred "-differ?" terms
    | IdenticalPredicate (left, right) -> scalar_pred "identical?" [ left; right ]
    | TypeValue (term, out) -> scalar_fn "type" [ term ] out
    | MetaValue (term, out) -> scalar_fn "meta" [ term ] out
    | NameValue (term, out) -> scalar_fn "name" [ term ] out
    | NamespaceValue (term, out) -> scalar_fn "namespace" [ term ] out
    | KeywordFromName (term, out) -> scalar_fn "keyword" [ term ] out
    | KeywordFromNamespaceName (namespace, name, out) ->
      scalar_fn "keyword" [ namespace; name ] out
    | StringIncludesValue (left, right) -> scalar_pred "clojure.string/includes?" [ left; right ]
    | StringStartsWithValue (left, right) ->
      scalar_pred "clojure.string/starts-with?" [ left; right ]
    | StringEndsWithValue (left, right) -> scalar_pred "clojure.string/ends-with?" [ left; right ]
    | StringLowerCaseValue (term, out) -> scalar_fn "clojure.string/lower-case" [ term ] out
    | StringUpperCaseValue (term, out) -> scalar_fn "clojure.string/upper-case" [ term ] out
    | StringCapitalizeValue (term, out) -> scalar_fn "clojure.string/capitalize" [ term ] out
    | StringReverseValue (term, out) -> scalar_fn "clojure.string/reverse" [ term ] out
    | StringTrimValue (term, out) -> scalar_fn "clojure.string/trim" [ term ] out
    | StringTrimLeftValue (term, out) -> scalar_fn "clojure.string/triml" [ term ] out
    | StringTrimRightValue (term, out) -> scalar_fn "clojure.string/trimr" [ term ] out
    | StringTrimNewlineValue (term, out) -> scalar_fn "clojure.string/trim-newline" [ term ] out
    | StringIndexOfValue (value, needle, out) ->
      scalar_fn "clojure.string/index-of" [ value; needle ] out
    | StringLastIndexOfValue (value, needle, out) ->
      scalar_fn "clojure.string/last-index-of" [ value; needle ] out
    | StringSubstringValue (value, start, None, out) -> scalar_fn "subs" [ value; start ] out
    | StringSubstringValue (value, start, Some end_, out) ->
      scalar_fn "subs" [ value; start; end_ ] out
    | StringBuildValue (terms, out) -> scalar_fn "str" terms out
    | PrintStringValue (terms, out) -> scalar_fn "print-str" terms out
    | PrintLineStringValue (terms, out) -> scalar_fn "println-str" terms out
    | PrStringValue (terms, out) -> scalar_fn "pr-str" terms out
    | PrnStringValue (terms, out) -> scalar_fn "prn-str" terms out
    | StringJoinPlainValue (collection, out) ->
      scalar_fn "clojure.string/join" [ collection ] out
    | StringJoinValue (separator, collection, out) ->
      scalar_fn "clojure.string/join" [ separator; collection ] out
    | StringReplaceValue (value, pattern, replacement, out) ->
      scalar_fn "clojure.string/replace" [ value; pattern; replacement ] out
    | StringReplaceFirstValue (value, pattern, replacement, out) ->
      scalar_fn "clojure.string/replace-first" [ value; pattern; replacement ] out
    | StringEscapeValue (value, replacements, out) ->
      scalar_fn "clojure.string/escape" [ value; replacements ] out
    | RePatternValue (pattern, out) -> scalar_fn "re-pattern" [ pattern ] out
    | ReFindValue (pattern, value, out) -> scalar_fn "re-find" [ pattern; value ] out
    | ReMatchesValue (pattern, value, out) -> scalar_fn "re-matches" [ pattern; value ] out
    | ReSeqValue (pattern, value, out) -> scalar_fn "re-seq" [ pattern; value ] out
    | ReFindPredicate (pattern, value) -> scalar_pred "re-find" [ pattern; value ]
    | ReMatchesPredicate (pattern, value) -> scalar_pred "re-matches" [ pattern; value ]
    | StringBlankValue term -> scalar_pred "clojure.string/blank?" [ term ]
    | StringSplitValue (value, separator, out) ->
      scalar_fn "clojure.string/split" [ value; separator ] out
    | StringSplitLimitValue (value, separator, limit, out) ->
      scalar_fn "clojure.string/split" [ value; separator; limit ] out
    | StringSplitLinesValue (value, out) -> scalar_fn "clojure.string/split-lines" [ value ] out
    | Ground (value, out) -> scalar_fn "ground" [ QValue value ] out
    | GroundCollection (values, out) -> scalar_fn "ground" [ QValue (Vector values) ] out
    | GroundTuple (values, outs) ->
      Fn_view
        ( { call_fn = "ground"; call_args = [ QValue (Vector values) ] }
        , binding_of_out_vars outs )
    | GroundRelation (rows, outs) ->
      Fn_view
        ( { call_fn = "ground"; call_args = [ QValue (Vector (List.map (fun row -> Vector row) rows)) ] }
        , Bind_collection (Bind_tuple (List.map (fun var -> Bind_scalar var) outs)) )
    | GroundTerm (term, out) -> scalar_fn "ground" [ term ] out
    | GroundTermCollection (term, out) ->
      Fn_view
        ( { call_fn = "ground"; call_args = [ term ] }
        , Bind_collection (scalar_binding out) )
    | GroundTermTuple (term, outs) ->
      Fn_view ({ call_fn = "ground"; call_args = [ term ] }, binding_of_out_vars outs)
    | GroundTermRelation (term, outs) ->
      Fn_view
        ( { call_fn = "ground"; call_args = [ term ] }
        , Bind_collection (Bind_tuple (List.map (fun var -> Bind_scalar var) outs)) )
    | VectorValue (terms, out) -> scalar_fn "vector" terms out
    | ListValue (terms, out) -> scalar_fn "list" terms out
    | SetValue (terms, out) -> scalar_fn "set" terms out
    | HashMapValue (terms, out) -> scalar_fn "hash-map" terms out
    | ArrayMapValue (terms, out) -> scalar_fn "array-map" terms out
    | RangeEndValue (end_, out) -> scalar_fn "range" [ end_ ] out
    | RangeValue (start, end_, out) -> scalar_fn "range" [ start; end_ ] out
    | RangeStepValue (start, end_, step, out) -> scalar_fn "range" [ start; end_; step ] out
    | TupleFunction (terms, out) -> scalar_fn "tuple" terms out
    | UntupleFunction (term, outs) ->
      Fn_view ({ call_fn = "untuple"; call_args = [ term ] }, binding_of_out_vars outs)
    | Predicate (name, terms, _) -> scalar_pred name terms
    | Function (name, terms, outs, _) ->
      Fn_view ({ call_fn = name; call_args = terms }, binding_of_out_vars outs)
    | DynamicPredicate (name, terms) -> scalar_pred name terms
    | DynamicFunction (name, terms, outs) ->
      Fn_view ({ call_fn = name; call_args = terms }, binding_of_out_vars outs)
    | DynamicFunctionCollection (name, terms, out) ->
      Fn_view
        ( { call_fn = name; call_args = terms }
        , Bind_collection (scalar_binding out) )
    | DynamicFunctionRelation (name, terms, outs) ->
      Fn_view
        ( { call_fn = name; call_args = terms }
        , Bind_collection (Bind_tuple (List.map (fun var -> Bind_scalar var) outs)) )
    | SourceClause (source, clause) -> Source_view (source, clause)
    | Not clauses -> Not_view clauses
    | SourceNot (source, clauses) -> Source_view (source, Not clauses)
    | NotJoin (vars, clauses) -> Not_join_view (vars, clauses)
    | SourceNotJoin (source, vars, clauses) -> Source_view (source, NotJoin (vars, clauses))
    | Or branches -> Or_view branches
    | SourceOr (source, branches) -> Source_view (source, Or branches)
    | OrJoin (vars, branches) -> Or_join_view (vars, branches)
    | SourceOrJoin (source, vars, branches) -> Source_view (source, OrJoin (vars, branches))
    | OrJoinRequired (required, vars, branches) ->
      Or_join_required_view (required, vars, branches)
    | SourceOrJoinRequired (source, required, vars, branches) ->
      Source_view (source, OrJoinRequired (required, vars, branches))
    | Rule (name, args) -> Rule_view (name, args)
    | SourceRule (source, name, args) -> Source_view (source, Rule (name, args))

  let pattern ?tx ?op ?src e a v =
    match tx, op, src with
    | None, None, None -> Pattern (e, a, v)
    | Some tx, None, None -> PatternTx (e, a, v, tx)
    | Some tx, Some op, None -> PatternTxOp (e, a, v, tx, op)
    | None, Some _, None -> invalid_arg "pattern: ?op requires ?tx"
    | None, None, Some source -> SourcePattern (source, e, a, v)
    | Some tx, None, Some source -> SourcePatternTx (source, e, a, v, tx)
    | Some tx, Some op, Some source -> SourcePatternTxOp (source, e, a, v, tx, op)
    | None, Some _, Some _ -> invalid_arg "pattern: ?op requires ?tx"

  let relation ?src terms =
    match src with
    | None -> SourceRelationPattern ("$", terms)
    | Some source -> SourceRelationPattern (source, terms)

  let rule_call ?src name args =
    match src with
    | None -> Rule (name, args)
    | Some source -> SourceRule (source, name, args)

  let call ?src ?binding name args =
    let call_form = QueryFormList (QueryFormSymbol name :: List.map query_form_of_term args) in
    let binding_form = Option.map query_form_of_binding binding in
    let form =
      match src, binding_form with
      | None, None -> QueryFormVector [ call_form ]
      | None, Some binding -> QueryFormVector [ call_form; binding ]
      | Some source, None ->
        QueryFormVector [ QueryFormSymbol (source_sigil_string source); call_form ]
      | Some source, Some binding ->
        QueryFormVector
          [ QueryFormSymbol (source_sigil_string source); call_form; binding ]
    in
    of_form form

  let pred ?src name args = call ?src name args
  let fn ?src name args binding = call ?src ~binding name args

  let not_ ?src clauses =
    match clauses, src with
    | [], _ -> invalid_arg "not requires at least one clause"
    | clauses, None -> Not clauses
    | clauses, Some source -> SourceNot (source, clauses)

  let not_join ?src vars clauses =
    match clauses, src with
    | [], _ -> invalid_arg "not-join requires at least one clause"
    | clauses, None -> NotJoin (vars, clauses)
    | clauses, Some source -> SourceNotJoin (source, vars, clauses)

  let or_ ?src branches =
    match branches, src with
    | [], _ -> invalid_arg "or requires at least one branch"
    | branches, None -> Or branches
    | branches, Some source -> SourceOr (source, branches)

  let or_join ?src vars branches =
    match vars, branches, src with
    | [], _, _ -> invalid_arg "or-join requires join variables"
    | _, [], _ -> invalid_arg "or-join requires at least one branch"
    | vars, branches, None -> OrJoin (vars, branches)
    | vars, branches, Some source -> SourceOrJoin (source, vars, branches)

  let or_join_required ?src ~required vars branches =
    match vars, branches, src with
    | [], _, _ -> invalid_arg "or-join requires join variables"
    | _, [], _ -> invalid_arg "or-join requires at least one branch"
    | vars, branches, None -> OrJoinRequired (required, vars, branches)
    | vars, branches, Some source -> SourceOrJoinRequired (source, required, vars, branches)

  let missing ?src e a =
    match src with
    | None -> Missing (e, a)
    | Some source -> SourceMissing (source, e, a)

  let with_src ?src clause =
    match src with
    | None -> clause
    | Some source -> SourceClause (source, clause)

  let get_else ?src e a default out =
    match src with
    | None -> GetElse (e, a, default, out)
    | Some source -> SourceGetElse (source, e, a, default, out)

  let get_some ?src e attrs attr_var value_var =
    match src with
    | None -> GetSome (e, attrs, attr_var, value_var)
    | Some source ->
      SourceGetSome (source, e, attrs, attr_var, value_var)

  let get_value ?src e a out =
    with_src ?src (GetValue (e, a, out))

  let get_default_value ?src e a default out =
    with_src ?src (GetDefaultValue (e, a, default, out))

  let count_value ?src e out = with_src ?src (CountValue (e, out))

  let empty_value ?src e = with_src ?src (EmptyValue e)

  let not_empty_value ?src e = with_src ?src (NotEmptyValue e)

  let contains_value ?src e v = with_src ?src (ContainsValue (e, v))

  let value_pred ?src p term = with_src ?src (ValuePredicate (p, term))

  let numeric_pred ?src p term = with_src ?src (NumericPredicate (p, term))

  let boolean_pred ?src p term = with_src ?src (BooleanPredicate (p, term))

  let boolean_not_pred ?src term = with_src ?src (BooleanNotPredicate term)

  let boolean_and_pred ?src terms = with_src ?src (BooleanAndPredicate terms)

  let boolean_or_pred ?src terms = with_src ?src (BooleanOrPredicate terms)

  let differ_pred ?src terms = with_src ?src (DifferPredicate terms)

  let identical_pred ?src left right = with_src ?src (IdenticalPredicate (left, right))

  let comparison ?src p left right = with_src ?src (ComparisonPredicate (p, left, right))

  let comparison_n ?src p terms = with_src ?src (ComparisonPredicateN (p, terms))

  let equality ?src p terms = with_src ?src (EqualityPredicate (p, terms))

  let arithmetic ?src op terms out =
    with_src ?src (ArithmeticValue (op, terms, out))

  let custom_pred ?src name args f =
    with_src ?src (Predicate (name, args, f))

  let custom_fn ?src name args out_vars f =
    with_src ?src (Function (name, args, out_vars, f))
end

module Query = struct
  type t = query

  let q = q
  let q_string = q_string
  let q_with = q_with
  let q_with_string = q_with_string
  let q_sources = q_sources
  let q_sources_string = q_sources_string
  let q_return = q_return
  let q_return_string = q_return_string
  let q_return_map = q_return_map
  let q_return_map_string = q_return_map_string

  let v ?(in_ = []) ?(with_ = []) ?(rules = []) find where : t =
    let find = List.map norm_find find in
    let where = List.map norm_clause where in
    let inputs = List.map query_input_of_spec (List.map norm_spec in_) in
    let inputs = Query_impl.infer_default_inputs None find where inputs in
    Query_impl.validate_query
      { find
      ; inputs
      ; with_vars = norm_vars with_
      ; rules = List.map norm_rule rules
      ; where
      }

  let find (q : t) = q.find
  let where (q : t) = q.where
  let inputs (q : t) = List.map spec_of_query_input q.inputs
  let with_ (q : t) = q.with_vars
  let rules (q : t) = q.rules

  let of_form form = parse_query form
  let of_string input = parse_query_string input
end

module Tx = struct
  let add entity attr value = Add (entity, attr, value)
  let retract entity attr value = Retract (entity, attr, Some value)
  let retract_attr entity attr = RetractAttr (entity, attr)
  let retract_entity entity = RetractEntity entity
  let compare_and_set entity attr expected value = CompareAndSet (entity, attr, expected, value)
  let entity attrs = Entity attrs
  let raw_datom datom = Raw_datom datom
  let install_tx_fn entity f = InstallTxFn (entity, f)
  let call_ident entity args = CallIdent (entity, args)
  let call f = Call f
end

module Pull = struct
  type pattern = pull_selector list

  let parse = parse_pull_pattern
  let parse_string = parse_pull_pattern_string
  let pull = pull
  let pull_string = pull_string
  let pull_many = pull_many
  let pull_many_string = pull_many_string
end

(* The flat wrappers are captured here because [include Db] inside the
   curated [Db] module shadows some of them with the context-taking
   implementation versions. *)
let flat_datom = datom
let flat_empty_db = empty_db
let flat_init_db = init_db
let flat_filter = filter
let flat_unfiltered_db = unfiltered_db
let flat_schema = schema

module Db = struct
  include Db_base

  type t = db

  let datom = flat_datom

  let empty ?(schema = []) ?storage () = flat_empty_db ~schema ?storage ()
  let init ?(schema = []) ?storage datoms = flat_init_db ~schema ?storage datoms
  let serializable = serializable
  let from_serializable = from_serializable
  let db_from_reader_string = db_from_reader_string
  let filter = flat_filter
  let unfiltered = flat_unfiltered_db
  let with_ = db_with
  let with_string = db_with_string
  let with_tx = with_tx
  let with_tx_string = with_tx_string
  let schema = flat_schema
  let with_schema = with_schema
  let entid = entid
  let entid_ref = entid_ref
  let hash = db_hash
  let hash_cache_size = db_hash_cache_size
  let diff = diff
  let squuid = squuid
  let squuid_time_millis = squuid_time_millis
  let is_unique = is_unique
  let is_unique_identity = is_unique_identity
  let is_indexed = is_indexed
  let is_component = is_component
  let is_ref = is_ref_attr
  let is_tuple = is_tuple_attr
  let tuple_attrs = tuple_attrs
  let reverse_ref = reverse_ref
  let tx0 = tx0
  let is_datom = is_datom
  let value_equal = value_equal
  let same_fact = same_fact
end

module Conn = struct
  type t = conn

  let create ?(schema = []) ?storage () = create_conn ~schema ?storage ()
  let from_db = conn_from_db
  let from_datoms ?(schema = []) ?storage datoms = conn_from_datoms ~schema ?storage datoms
  let restore = restore_conn
  let db = db
  let update_db = Conn.update_db
  let storage_tail = Conn.storage_tail
  let listen = listen
  let listen_auto = listen_auto
  let unlisten = unlisten
  let reset ?tx_meta conn db = reset_conn ?tx_meta conn db
  let reset_schema = reset_schema
  let apply_report = apply_report
  let transact ?tx_meta conn tx_ops = transact_conn ?tx_meta conn tx_ops
  let transact_string ?tx_meta conn input = transact_conn_string ?tx_meta conn input
  let transact_async ?tx_meta conn tx_ops = transact_async ?tx_meta conn tx_ops
end

module Entity = struct
  type t = entity

  let id (e : entity) : entity_id = e.id
  let of_ref = entity
  let attr = entity_attr
  let attr_raw = entity_attr_raw
  let attrs = entity_attrs
  let db = entity_db
  let equal = entity_equal
  let hash = entity_hash
  let touch = touch
  let is_entity = is_entity
end

module Storage = struct
  type t = storage

  module type S = sig
    type t

    val write : t -> (storage_address * string) list -> unit
    val read : t -> storage_address list -> (storage_address * string) list
    val list : t -> storage_address list
    val delete : t -> storage_address list -> unit
  end

  let make (type s) (module Backend : S with type t = s) (backend : s) : t =
    Platform.make_storage
      ~write:(Backend.write backend)
      ~read:(Backend.read backend)
      ~list:(fun () -> Backend.list backend)
      ~delete:(Backend.delete backend)

  let memory = memory_storage
  let file = file_storage
  let root_address = Storage.root_address
  let tail_address = Storage.tail_address
  let store = store
  let store_tail = store_tail
  let tail_compaction_threshold = Storage.tail_compaction_threshold
  let tail_datom_count = Storage.tail_datom_count
  let restore_root_snapshot = Storage.restore_root_snapshot
  let restore_tail_groups = restore_tail_groups
  let db_with_tail = db_with_tail
  let restore = restore
  let addresses = addresses
  let of_db = storage
  let settings = settings
  let collect_garbage = collect_garbage
end

module Schema = struct
  type t = schema_attr

  (** Smart constructor for [schema_attr]. Tuple attributes and tuple
      types are expressed through {!tuple_spec}, so the invalid
      combinations of the flat record cannot be constructed: a tuple
      spec implies [valueType TupleType], and [tupleAttrs] and
      [tupleTypes] are mutually exclusive. *)
  let spec
      ?(indexed = false)
      ?value_type
      ?(cardinality = One)
      ?unique
      ?(is_component = false)
      ?(no_history = false)
      ?doc
      ?tuple
      () : t =
    let tuple_attrs, tuple_types =
      match tuple with
      | None -> None, None
      | Some (Tuple_attrs attrs) -> Some attrs, None
      | Some (Tuple_types types) -> None, Some types
    in
    let value_type =
      match value_type, tuple with
      | Some given, Some _ ->
        if given <> TupleType then
          invalid_arg "Schema.spec: a tuple spec implies valueType TupleType";
        Some given
      | Some given, None -> Some given
      | None, Some _ -> Some TupleType
      | None, None -> None
    in
    { cardinality; unique; indexed; is_component; no_history; doc; value_type
    ; tuple_attrs; tuple_types
    }

  let validate = validate_schema
  let fields = schema_fields
  let attr_by_name = Schema.schema_attr_by_name
  let cardinality (a : t) = a.cardinality
  let unique (a : t) = a.unique
  let indexed (a : t) = a.indexed
  let is_component (a : t) = a.is_component
  let no_history (a : t) = a.no_history
  let doc (a : t) = a.doc
  let value_type (a : t) = a.value_type
  let tuple_attrs (a : t) = a.tuple_attrs
  let tuple_types (a : t) = a.tuple_types
  let tuple (a : t) : tuple_spec option =
    match a.tuple_attrs, a.tuple_types with
    | Some attrs, _ -> Some (Tuple_attrs attrs)
    | None, Some types -> Some (Tuple_types types)
    | None, None -> None
  let is_ref = Schema.schema_attr_is_ref
  let is_tuple = Schema.schema_attr_is_tuple
  let is_avet_accessible = Schema.schema_attr_is_avet_accessible
  let has_no_history = Schema.schema_has_no_history
  let folded_datoms = Schema.folded_datoms
  let split_namespaced = Schema.split_namespaced_attr
  let join_namespaced = Schema.join_namespaced_attr
  let is_reverse_ref = is_reverse_ref
  let reverse_ref = reverse_ref
end

module Edn = struct
  type t = query_form

  let read = read_edn
  let read_string = read_edn
  let attr_of_key = attr_of_edn_key
  let tx_attr_of_key = tx_attr_of_edn_key
  let tx_op_name = tx_op_name_of_edn_form
  let is_attr_key = is_edn_attr_key
  let keyword_name = keyword_name_of_form
  let entity_ref = entity_ref_of_edn_form
  let to_tx_data = tx_data_of_edn_form
  let to_schema = schema_of_edn_form
  let to_db = db_from_reader_form
end

(** Deprecated entry points kept for source compatibility; the
    module-level API ([Db], [Conn], [Entity], ...) is the supported
    surface. *)
module Compat = struct
  let is_datom = is_datom
  let is_db = is_db
  let is_conn = is_conn
  let is_entity = is_entity
  let is_filtered = is_filtered
  let listen_bang = listen_bang
  let listen_bang_auto = listen_bang_auto
  let unlisten_bang = unlisten_bang
  let reset_conn_bang = reset_conn_bang
  let reset_schema_bang = reset_schema_bang
  let transact_bang = transact_bang
  let transact_bang_string = transact_bang_string
  let transact_async = transact_async
  let transact_async_string = transact_async_string
end

module Internal_convert = struct
  (* Boundary coercions: the public types share their representation with
     the implementation types (this file [include]s
     [Datascript_types]); the mli seals them apart, so these are
     identities. *)
  let internalize_db (x : db) : Datascript_types.db = x
  let externalize_db (x : Datascript_types.db) : db = x
  let internalize_storage (x : storage) : Datascript_types.storage = x
  let externalize_storage (x : Datascript_types.storage) : storage = x
  let internalize_entity (x : entity) : Datascript_types.entity = x
  let externalize_entity (x : Datascript_types.entity) : entity = x
  let internalize_conn (x : conn) : Conn.t = x
  let externalize_conn (x : Conn.t) : conn = x
  let internalize_query (x : query) : Datascript_types.query = x
  let externalize_query (x : Datascript_types.query) : query = x
  let internalize_query_input (x : query_input) : Datascript_types.query_input = x
  let externalize_query_input (x : Datascript_types.query_input) : query_input = x
  let internalize_schema_attr (x : schema_attr) : Datascript_types.schema_attr = x
  let externalize_schema_attr (x : Datascript_types.schema_attr) : schema_attr = x
  let internalize_value (x : value) : Datascript_types.value = x
  let externalize_value (x : Datascript_types.value) : value = x
  let internalize_datom (x : datom) : Datascript_types.datom = x
  let externalize_datom (x : Datascript_types.datom) : datom = x
  let internalize_entity_ref (x : entity_ref) : Datascript_types.entity_ref = x
  let externalize_entity_ref (x : Datascript_types.entity_ref) : entity_ref = x
  let internalize_entity_id (x : entity_id) : Datascript_types.entity_id = x
  let externalize_entity_id (x : Datascript_types.entity_id) : entity_id = x
  let internalize_tx (x : tx) : Datascript_types.tx = x
  let externalize_tx (x : Datascript_types.tx) : tx = x
  let internalize_tx_op (x : tx_op) : Datascript_types.tx_op = x
  let externalize_tx_op (x : Datascript_types.tx_op) : tx_op = x
  let internalize_query_result (x : query_result) : Datascript_types.query_result = x
  let externalize_query_result (x : Datascript_types.query_result) : query_result = x
  let internalize_query_rule (x : query_rule) : Datascript_types.query_rule = x
  let externalize_query_rule (x : Datascript_types.query_rule) : query_rule = x
  let internalize_query_form (x : query_form) : Datascript_types.query_form = x
  let externalize_query_form (x : Datascript_types.query_form) : query_form = x
  let internalize_input_binding (x : input_binding) : Datascript_types.input_binding = x
  let externalize_input_binding (x : Datascript_types.input_binding) : input_binding = x
  let internalize_query_output (x : query_output) : Datascript_types.query_output = x
  let externalize_query_output (x : Datascript_types.query_output) : query_output = x
  let internalize_tx_report (x : tx_report) : Datascript_types.tx_report = x
  let externalize_tx_report (x : Datascript_types.tx_report) : tx_report = x
end
