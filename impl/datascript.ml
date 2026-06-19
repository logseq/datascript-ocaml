include Datascript_types

module Built_ins = Built_ins
module Conn = Conn
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

module Entity = Entity
module Lru = Lru
module Lookup_refs = Lookup_refs
module Schema = Schema
module Serialize = Serialize
module Storage = Storage
module Util = Util
module Upsert = Upsert

let validate_entity_id = Db_impl.validate_entity_id

let datom = Db_impl.datom

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

let with_db_datoms = Db_impl.with_datoms

let empty_db ?(schema = []) ?storage () =
  Db_impl.empty_db db_core_context ~schema ?storage ()

let empty db = Db_impl.empty db_core_context db

let init_db ?(schema = []) ?storage datoms =
  Db_impl.init_db db_core_context ~schema ?storage datoms

let history db = Db_impl.history db_core_context db

let is_history = Db_impl.is_history

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
  let datom = datom
  let normalize_value = normalize_value
  let validate_entity_id = validate_entity_id
  let max_allocatable_entity_id = max_allocatable_entity_id
  let visible_datoms = visible_datoms
end)

let entity_attr_datoms = Transact_datoms_impl.entity_attr_datoms
let current_attr_value = Transact_datoms_impl.current_attr_value
let tuple_direct_write_matches_sources = Transact_datoms_impl.tuple_direct_write_matches_sources
let add_active_datom_with_report = Transact_datoms_impl.add_active_datom_with_report
let retract_active_datom_with_report = Transact_datoms_impl.retract_active_datom_with_report
let retract_entity_with_report = Transact_datoms_impl.retract_entity_with_report
let compare_and_set_matches = Transact_datoms_impl.compare_and_set_matches
let refresh_tuple_attrs_for_source = Transact_datoms_impl.refresh_tuple_attrs_for_source
let add_user_datom_with_report = Transact_datoms_impl.add_user_datom_with_report
let retract_user_attr_with_report = Transact_datoms_impl.retract_user_attr_with_report
let normalize_entity_attr_value = Transact_datoms_impl.normalize_entity_attr_value
let add_entity_attr_value = Transact_datoms_impl.add_entity_attr_value
let allocate_entity_id = Transact_datoms_impl.allocate_entity_id
let coerce_tuple_lookup_value = Transact_datoms_impl.coerce_tuple_lookup_value
let entid_in_datoms = Transact_datoms_impl.entid_in_datoms
let entid = Transact_datoms_impl.entid

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

module Transact_impl = Transact

let transact_resolve_context : Transact_impl.context =
  { validate_entity_id
  ; entid_in_datoms
  ; ident_attr
  ; allocate_entity_id
  ; lookup_ref_entity_id_in_datoms =
      (fun ~strict_missing db datoms attr value ->
        lookup_ref_entity_id_in_datoms ~strict_missing db datoms attr value)
  ; unresolved_lookup_ref_message
  ; normalize_value
  ; is_ref_attr
  ; is_reverse_ref
  ; reverse_ref
  ; cardinality
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
  let entid = entid
  let ident_attr = ident_attr
  let lookup_ref_entity_id ?strict_missing db attr value = lookup_ref_entity_id ?strict_missing db attr value
  let normalize_value = normalize_value
  let unresolved_entity_ref_message = unresolved_entity_ref_message
  let ref_attr_for_value_resolution = ref_attr_for_value_resolution
  let entity_ref_of_ref_attr_value = entity_ref_of_ref_attr_value
  let compare_value = compare_value
  let first_nonzero = first_nonzero
  let validate_entity_id = validate_entity_id
end)

let schema_fields = Schema.schema_fields

let schema_from_transaction_datoms = Schema.schema_from_transaction_datoms

let transact_apply_context : Transact_impl.apply_context =
  { resolve_context = transact_resolve_context
  ; is_filtered
  ; schema_from_transaction_datoms =
      (fun ~strict ~removed_attrs ~removed_fields ~ignored_schema_entities schema datoms ->
        schema_from_transaction_datoms ~strict ~removed_attrs ~removed_fields ~ignored_schema_entities schema datoms)
  ; schema_fields
  ; current_attr_value
  ; add_entity_attr_value
  ; same_fact
  ; add_user_datom_with_report
  ; is_tuple_attr
  ; tuple_attrs_for_source
  ; is_unique_identity
  ; with_db_datoms
  ; retract_user_attr_with_report
  ; retract_active_datom_with_report
  ; retract_entity_with_report
  ; compare_and_set_matches
  ; compare_and_set_failure_message
  ; datom
  ; normalize_datom_for_schema
  ; add_active_datom_with_report
  ; validate_explicit_upsert_target
  ; entity_unique_identity
  ; existing_unique_entity =
      (fun db attr value ->
        Db_access_impl.find_datom db Avet ~a:attr ~v:value ()
        |> Option.map (fun d -> d.e))
  ; value_equal
  ; normalize_entity_attr_value
  ; tuple_direct_write_matches_sources
  ; refresh_tuple_attrs_for_source
  ; refresh_db_indexes
  ; refresh_db_indexes_with_added_datoms
  ; refresh_db_identity
  }

let apply_tx tx_ops db =
  Transact_impl.apply_tx transact_apply_context tx_ops db

let db_with tx_ops db =
  let db_after, _, _ = apply_tx tx_ops db in
  db_after

let storage_tail_context : Storage.tail_context =
  { apply_group = (fun db group -> db_with (List.map (fun datom -> Raw_datom datom) group) db)
  }

let db_with_tail db tail =
  Storage.db_with_tail storage_tail_context db tail

let storage_restore_context : Storage.restore_context =
  { next_db_uid; db_with_tail }

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

let entid_ref = Db_access_impl.entid_ref
let datoms = Db_access_impl.datoms
let datoms_ref = Db_access_impl.datoms_ref
let datoms_list db index ?e ?a ?v ?tx () =
  datoms db index ?e ?a ?v ?tx () |> List.of_seq

let find_datom = Db_access_impl.find_datom
let find_datom_ref = Db_access_impl.find_datom_ref
let seek_datoms = Db_access_impl.seek_datoms
let seek_datoms_ref = Db_access_impl.seek_datoms_ref
let rseek_datoms = Db_access_impl.rseek_datoms
let rseek_datoms_ref = Db_access_impl.rseek_datoms_ref
let index_range = Db_access_impl.index_range

let diff = Db_impl.diff

let db_hash = Db_impl.hash

let db_hash_cache_size = Db_impl.hash_cache_size

let squuid = Db_impl.squuid

let squuid_time_millis = Db_impl.squuid_time_millis

let reset_conn ?(tx_meta = []) conn db =
  let context : Conn.reset_context =
    { store; datoms = (fun db -> datoms_list db Eavt ()) }
  in
  Conn.reset context ~tx_meta conn db

let reset_conn_bang ?tx_meta conn db = reset_conn ?tx_meta conn db

module Entity_refs_impl = Entity_refs.Make (struct
  let lookup_ref_entity_id db attr value = lookup_ref_entity_id db attr value
  let entid = entid
  let ident_attr = ident_attr
  let normalize_value = normalize_value
end)

let entity_id_of_ref = Entity_refs_impl.entity_id_of_ref
let resolve_ref_value = Entity_refs_impl.resolve_ref_value

let entity_context =
  { Entity.datoms_by_entity = (fun db entity_id -> datoms_list db Eavt ~e:entity_id ())
  ; all_datoms = (fun db -> datoms_list db Eavt ())
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
  ; datoms_by_entity = (fun db entity_id -> datoms_list db Eavt ~e:entity_id ())
  ; datoms_by_avet_ref = (fun db attr entity_id -> datoms_list db Avet ~a:attr ~v:(Ref entity_id) ())
  ; cardinality
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
  ; max_eid_with_entity_id = Db_impl.max_eid_with_entity_id
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

let query_entity_id_term = function
  | QEntity entity_id -> Some entity_id
  | QValue (Int entity_id) -> Some entity_id
  | _ -> None

let query_value_term = function
  | QValue value -> Some value
  | _ -> None

let query_tx_term = function
  | Some (QValue (Int tx)) -> Some tx
  | _ -> None

let query_attr_uses_avet db attr =
  is_ref_attr db attr || is_unique db attr || is_indexed db attr

let query_value_uses_avet = function
  | Nil | List _ | Vector _ | Map _ | Set _ | Tuple _ | TxRef -> false
  | Int _ | Float _ | String _ | Symbol _ | Bool _ | Keyword _ | Uuid _ | Instant _ | Regex _ | Ref _ | Ref_to _ -> true

let pattern_datoms db e_term a_term v_term tx_term =
  let e = query_entity_id_term e_term in
  let v = query_value_term v_term in
  let tx = query_tx_term tx_term in
  match a_term, v with
  | QAttr attr, _ when is_reverse_ref attr ->
    datoms db Aevt ~a:(reverse_ref attr) ?tx ()
  | QAttr attr, Some value when query_value_uses_avet value && query_attr_uses_avet db attr ->
    datoms db Avet ?e ~a:attr ~v:value ?tx ()
  | QAttr attr, _ ->
    datoms db Aevt ?e ~a:attr ?tx ()
  | _ -> datoms db Eavt ?e ?v ?tx ()

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

let collect_query_terms_exn db bindings terms =
  Query.collect_query_terms_exn (query_match_context db) bindings terms


let query_evaluator_context : Query_eval.evaluator_context =
  { result_resolution_context = query_result_context
  ; match_context = query_match_context
  ; datoms = datoms_list
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
  let normalize_value = normalize_value
end)

let eval_clauses = Query_where_impl.eval_clauses

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

module Query_api_impl = Query_api.Make (struct
  let empty_db () = empty_db ()
  let validate_rule_arities = validate_rule_arities
  let initial_query_context = initial_query_context
  let eval_clauses = eval_clauses
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
    ; pattern_datoms : db -> query_term -> query_term -> query_term -> query_term option -> datom Seq.t
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

  let query_has_runtime_features query =
    let default_inputs =
      match query.inputs with
      | [] | [ Input_source_decl "$" ] -> true
      | _ -> false
    in
    (not default_inputs) || query.with_vars <> [] || query.rules <> []

  let find_var_names = function
    | [ Find_var left; Find_var right ] -> Some [ left; right ]
    | _ -> None

  let entity_value_row entity_id value =
    let entity_result = Result_entity entity_id
    and value_result = Query_impl.result_of_ref (Result_value value) in
    entity_result, value_result

  let entity_value_row_builder find_vars e_var value_var =
    match find_vars with
    | [ find_e; find_v ] when find_e = e_var && find_v = value_var ->
      Some (fun entity_id value ->
        let entity_result, value_result = entity_value_row entity_id value in
        [ entity_result; value_result ])
    | [ find_v; find_e ] when find_e = e_var && find_v = value_var ->
      Some (fun entity_id value ->
        let entity_result, value_result = entity_value_row entity_id value in
        [ value_result; entity_result ])
    | _ -> None

  let datoms_by_attr_value db attr value =
    if query_value_uses_avet value && query_attr_uses_avet db attr then
      datoms_list db Avet ~a:attr ~v:value ()
    else
      datoms_list db Aevt ~a:attr ()
      |> List.filter (fun datom -> compare_value datom.v value = 0)

  let planned_entity_value_join db find_vars e_var match_attr match_value value_attr value_var =
    match entity_value_row_builder find_vars e_var value_var with
    | None -> []
    | Some row ->
      if cardinality db value_attr = One then
        datoms_by_attr_value db match_attr match_value
        |> List.filter_map (fun match_datom ->
          find_datom db Aevt ~e:match_datom.e ~a:value_attr ()
          |> Option.map (fun datom -> row datom.e datom.v))
      else
        datoms_by_attr_value db match_attr match_value
        |> List.fold_left
             (fun rows match_datom ->
               datoms_list db Aevt ~e:match_datom.e ~a:value_attr ()
               |> List.fold_left (fun rows datom -> row datom.e datom.v :: rows) rows)
             []
        |> List.rev

  let planned_comparison_scan db find_vars e_var attr value_var predicate threshold =
    match entity_value_row_builder find_vars e_var value_var with
    | None -> []
    | Some row ->
      let candidates =
        match predicate with
        | GreaterThan | GreaterOrEqual when query_attr_uses_avet db attr ->
          index_range db attr ~start:threshold ()
        | LessThan | LessOrEqual when query_attr_uses_avet db attr ->
          index_range db attr ~stop:threshold ()
        | _ -> datoms_list db Aevt ~a:attr ()
      in
      candidates
      |> List.filter_map (fun datom ->
        if Built_ins.matches_comparison_predicate predicate (compare_value datom.v threshold) then
          Some (row datom.e datom.v)
        else
          None)

  let planned_simple_query db query =
    match find_var_names query.find, query_has_runtime_features query, query.where with
    | Some find_vars, false,
      [ Pattern (QVar e1, QAttr match_attr, QValue match_value)
      ; Pattern (QVar e2, QAttr value_attr, QVar value_var)
      ]
      when e1 = e2 && e1 <> value_var ->
      Some (planned_entity_value_join db find_vars e1 match_attr match_value value_attr value_var)
    | Some find_vars, false,
      [ Pattern (QVar e2, QAttr value_attr, QVar value_var)
      ; Pattern (QVar e1, QAttr match_attr, QValue match_value)
      ]
      when e1 = e2 && e1 <> value_var ->
      Some (planned_entity_value_join db find_vars e1 match_attr match_value value_attr value_var)
    | Some find_vars, false,
      [ Pattern (QVar e, QAttr attr, QVar value_var)
      ; ComparisonPredicate (predicate, QVar compared_var, QValue threshold)
      ]
      when compared_var = value_var && e <> value_var ->
      Some (planned_comparison_scan db find_vars e attr value_var predicate threshold)
    | Some find_vars, false,
      [ ComparisonPredicate (predicate, QVar compared_var, QValue threshold)
      ; Pattern (QVar e, QAttr attr, QVar value_var)
      ]
      when compared_var = value_var && e <> value_var ->
      Some (planned_comparison_scan db find_vars e attr value_var predicate threshold)
    | _ -> None

  let q ?inputs db query =
    match inputs, planned_simple_query db query with
    | None, Some rows -> rows
    | Some _, _ | None, None -> Query_impl.q query_context ?inputs db query

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
      Query_impl.q_string query_context ?inputs db input
    else
      q ?inputs db (cached_query_string input)

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
