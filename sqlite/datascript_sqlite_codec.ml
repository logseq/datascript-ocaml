module Ds = Datascript
module DT = Ds.Internal.Datascript_types
module PSet = Persistent_sorted_set
module Transit = Transit_native.Transit.Json

open Ds

let schema_attr_default : DT.schema_attr =
  { DT.cardinality = DT.One
  ; DT.unique = None
  ; DT.indexed = false
  ; DT.is_component = false
  ; DT.no_history = false
  ; DT.doc = None
  ; DT.value_type = None
  ; DT.tuple_attrs = None
  ; DT.tuple_types = None
  }

let string_of_transit_key = function
  | Transit.Keyword value | Transit.String value -> Some value
  | _ -> None

let keyword_of_transit = function
  | Transit.Keyword value -> Some value
  | _ -> None

let bool_of_transit = function Transit.Bool value -> Some value | _ -> None

let string_of_transit = function
  | Transit.String value -> Some value
  | _ -> None

let int_of_transit_value = function
  | Transit.Int value -> Some value
  | Transit.Int64 value ->
      if
        Int64.compare value (Int64.of_int min_int) >= 0
        && Int64.compare value (Int64.of_int max_int) <= 0
      then Some (Int64.to_int value)
      else None
  | _ -> None

let int64_of_transit_value = function
  | Transit.Int value -> Some (Int64.of_int value)
  | Transit.Int64 value -> Some value
  | _ -> None

let lookup_transit_key key entries =
  List.find_map
    (fun (entry_key, value) ->
      match string_of_transit_key entry_key with
      | Some entry_key when String.equal entry_key key -> Some value
      | _ -> None)
    entries

let transit_of_cardinality = function
  | DT.One -> Transit.Keyword "db.cardinality/one"
  | DT.Many -> Transit.Keyword "db.cardinality/many"

let cardinality_of_transit = function
  | Transit.Keyword "db.cardinality/many" -> DT.Many
  | Transit.Keyword "db.cardinality/one" -> DT.One
  | _ -> DT.One

let transit_of_unique = function
  | DT.Value -> Transit.Keyword "db.unique/value"
  | DT.Identity -> Transit.Keyword "db.unique/identity"

let unique_of_transit = function
  | Transit.Keyword "db.unique/value" -> Some DT.Value
  | Transit.Keyword "db.unique/identity" -> Some DT.Identity
  | _ -> None

let transit_of_value_type = function
  | DT.RefType -> Transit.Keyword "db.type/ref"
  | DT.StringType -> Transit.Keyword "db.type/string"
  | DT.KeywordType -> Transit.Keyword "db.type/keyword"
  | DT.NumberType -> Transit.Keyword "db.type/number"
  | DT.UuidType -> Transit.Keyword "db.type/uuid"
  | DT.InstantType -> Transit.Keyword "db.type/instant"
  | DT.TupleType -> Transit.Keyword "db.type/tuple"

let value_type_of_transit = function
  | Transit.Keyword "db.type/ref" -> Some DT.RefType
  | Transit.Keyword "db.type/string" -> Some DT.StringType
  | Transit.Keyword "db.type/keyword" -> Some DT.KeywordType
  | Transit.Keyword "db.type/number" -> Some DT.NumberType
  | Transit.Keyword "db.type/uuid" -> Some DT.UuidType
  | Transit.Keyword "db.type/instant" -> Some DT.InstantType
  | Transit.Keyword "db.type/tuple" -> Some DT.TupleType
  | _ -> None

let transit_of_ref_type = function
  | PSet.Strong -> Transit.Keyword "strong"
  | PSet.Weak -> Transit.Keyword "weak"

let ref_type_of_transit = function
  | Transit.Keyword "soft" -> PSet.Weak
  | Transit.Keyword "weak" -> PSet.Weak
  | Transit.Keyword "strong" | _ -> PSet.Strong

let address_to_transit address = Transit.String address

let address_of_transit label = function
  | Transit.String address -> address
  | Transit.Int address -> string_of_int address
  | Transit.Int64 address -> Int64.to_string address
  | _ -> invalid_arg (label ^ " must be a storage address")

let transit_of_tuple_attrs attrs =
  Transit.Array (List.map (fun attr -> Transit.Keyword attr) attrs)

let transit_of_tuple_types types =
  Transit.Array (List.map transit_of_value_type types)

let schema_attr_to_transit ~ident_backed attr ident =
  let entries = ref [] in
  let add key value = entries := (Transit.Keyword key, value) :: !entries in
  (match attr.DT.cardinality with
  | DT.One -> ()
  | DT.Many -> add "db/cardinality" (transit_of_cardinality attr.DT.cardinality));
  Option.iter (fun unique -> add "db/unique" (transit_of_unique unique)) attr.DT.unique;
  if attr.DT.indexed then add "db/index" (Transit.Bool true);
  if attr.DT.is_component then add "db/isComponent" (Transit.Bool true);
  if attr.DT.no_history then add "db/noHistory" (Transit.Bool true);
  Option.iter (fun doc -> add "db/doc" (Transit.String doc)) attr.DT.doc;
  Option.iter
    (fun value_type -> add "db/valueType" (transit_of_value_type value_type)) attr.DT.value_type;
  Option.iter (fun attrs -> add "db/tupleAttrs" (transit_of_tuple_attrs attrs)) attr.DT.tuple_attrs;
  Option.iter (fun types -> add "db/tupleTypes" (transit_of_tuple_types types)) attr.DT.tuple_types;
  (* cljs update-schema merges {:db/ident ident} into the attr's spec map *)
  if ident_backed then add "db/ident" (Transit.Keyword ident);
  Transit.Map (List.rev !entries)

let schema_to_transit ?(eids = []) schema =
  Transit.Map
    (List.map
       (fun (attr, schema_attr) ->
         ( Transit.Keyword attr
         , schema_attr_to_transit
             ~ident_backed:(List.exists (fun (_, ident) -> String.equal ident attr) eids)
             schema_attr
             attr ))
       schema
     @ List.map (fun (eid, ident) -> (Transit.Int64 eid, Transit.Keyword ident)) eids)

let tuple_attrs_of_transit = function
  | Transit.Array values | Transit.List values -> Some (List.filter_map keyword_of_transit values)
  | _ -> None

let tuple_types_of_transit = function
  | Transit.Array values | Transit.List values ->
      let types = List.filter_map value_type_of_transit values in
      if List.length types = List.length values then Some types else None
  | _ -> None

let schema_attr_of_transit = function
  | Transit.Map props ->
      List.fold_left
        (fun schema (key, value) ->
          match keyword_of_transit key with
          | Some "db/cardinality" -> { schema with DT.cardinality = cardinality_of_transit value }
          | Some "db/unique" -> { schema with DT.unique = unique_of_transit value }
          | Some "db/index" ->
              { schema with DT.indexed = Option.value (bool_of_transit value) ~default:false }
          | Some "db/isComponent" ->
              { schema with DT.is_component = Option.value (bool_of_transit value) ~default:false }
          | Some "db/noHistory" ->
              { schema with DT.no_history = Option.value (bool_of_transit value) ~default:false }
          | Some "db/doc" -> { schema with DT.doc = string_of_transit value }
          | Some "db/valueType" -> { schema with DT.value_type = value_type_of_transit value }
          | Some "db/tupleAttrs" -> { schema with DT.tuple_attrs = tuple_attrs_of_transit value }
          | Some "db/tupleTypes" -> { schema with DT.tuple_types = tuple_types_of_transit value }
          | Some _ | None -> schema)
        schema_attr_default props
  | _ -> schema_attr_default

let schema_of_transit = function
  | Transit.Map entries ->
      List.filter_map
        (fun (attr, schema_attr) ->
          match keyword_of_transit attr with
          | Some attr -> Some (attr, schema_attr_of_transit schema_attr)
          | None -> None)
        entries
  | _ -> []

let schema_eids_of_transit = function
  | Transit.Map entries ->
      List.filter_map
        (fun (key, value) ->
          match int64_of_transit_value key, keyword_of_transit value with
          | Some eid, Some ident -> Some (eid, ident)
          | _ -> None)
        entries
  | _ -> []

let rec impl_value_to_transit = function
  | DT.Nil -> Transit.Null
  | DT.Int64 value -> Transit.Int64 value
  | DT.Float value -> Transit.Float value
  | DT.String value -> Transit.String value
  | DT.Symbol value -> Transit.Symbol value
  | DT.Bool value -> Transit.Bool value
  | DT.Keyword value -> Transit.Keyword value
  | DT.Uuid value -> Transit.Uuid value
  | DT.Instant value -> Transit.Date value
  | DT.Regex value -> Transit.Tagged ("regex", Transit.String value)
  | DT.Ref entity_id -> Transit.Int64 entity_id
  | DT.List values -> Transit.List (List.map impl_value_to_transit values)
  | DT.Vector values -> Transit.Array (List.map impl_value_to_transit values)
  | DT.Map entries ->
      Transit.Map (List.map (fun (key, value) -> (impl_value_to_transit key, impl_value_to_transit value)) entries)
  | DT.Set values -> Transit.Set (List.map impl_value_to_transit values)
  | DT.Tuple values ->
      Transit.Array
        (List.map
           (function
             | None -> Transit.Null
             | Some value -> impl_value_to_transit value)
           values)
  | DT.TxRef -> Transit.Keyword "db/current-tx"
  | DT.Ref_to _ -> invalid_arg "storage payload cannot contain unresolved refs"

let rec impl_value_of_transit = function
  | Transit.Null -> DT.Nil
  | Bool value -> DT.Bool value
  | String value -> DT.String value
  | Int value -> DT.Int64 (Int64.of_int value)
  | Int64 value -> DT.Int64 value
  | Float value -> DT.Float value
  | Binary value -> DT.String value
  | Big_decimal value -> DT.Float (float_of_string value)
  | Big_int value -> DT.Int64 (Int64.of_string value)
  | Date value -> DT.Instant value
  | Uuid value -> DT.Uuid (Internal.Util.uuid_canonicalize value)
  | Uri value -> DT.String value
  | Keyword value -> DT.Keyword value
  | Symbol value -> DT.Symbol value
  | Array values -> DT.Vector (List.map impl_value_of_transit values)
  | Map entries -> DT.Map (List.map (fun (key, value) -> (impl_value_of_transit key, impl_value_of_transit value)) entries)
  | Set values -> DT.Set (List.map impl_value_of_transit values)
  | List values -> DT.List (List.map impl_value_of_transit values)
  | Tagged ("u", Transit.String value) -> DT.Uuid (Internal.Util.uuid_canonicalize value)
  | Tagged ("m", Transit.Int value) -> DT.Instant (Int64.of_int value)
  | Tagged ("m", Transit.Int64 value) -> DT.Instant value
  | Tagged ("regex", Transit.String value) -> DT.Regex value
  | Tagged (tag, value) -> DT.Vector [ DT.String tag; impl_value_of_transit value ]

let rec value_to_transit = function
  | Ds.Nil -> Transit.Null
  | Int64 value -> Transit.Int64 value
  | Float value -> Transit.Float value
  | String value -> Transit.String value
  | Symbol value -> Transit.Symbol value
  | Bool value -> Transit.Bool value
  | Keyword value -> Transit.Keyword value
  | Uuid value -> Transit.Uuid value
  | Instant value -> Transit.Date value
  | Regex value -> Transit.Tagged ("regex", Transit.String value)
  | Ref entity_id -> Transit.Int64 (Entity_id.to_int64 entity_id)
  | List values -> Transit.List (List.map value_to_transit values)
  | Vector values -> Transit.Array (List.map value_to_transit values)
  | Map entries ->
      Transit.Map (List.map (fun (key, value) -> (value_to_transit key, value_to_transit value)) entries)
  | Set values -> Transit.Set (List.map value_to_transit values)
  | Tuple values ->
      Transit.Array
        (List.map
           (function
             | None -> Transit.Null
             | Some value -> value_to_transit value)
           values)
  | TxRef -> Transit.Keyword "db/current-tx"
  | Ref_to _ -> invalid_arg "storage payload cannot contain unresolved refs"

let rec value_of_transit = function
  | Transit.Null -> Ds.Nil
  | Bool value -> Bool value
  | String value -> String value
  | Int value -> Int64 (Int64.of_int value)
  | Int64 value -> Int64 value
  | Float value -> Float value
  | Binary value -> String value
  | Big_decimal value -> Float (float_of_string value)
  | Big_int value -> Int64 (Int64.of_string value)
  | Date value -> Instant value
  | Uuid value -> Uuid (Internal.Util.uuid_canonicalize value)
  | Uri value -> String value
  | Keyword value -> Keyword value
  | Symbol value -> Symbol value
  | Array values -> Vector (List.map value_of_transit values)
  | Map entries -> Map (List.map (fun (key, value) -> (value_of_transit key, value_of_transit value)) entries)
  | Set values -> Set (List.map value_of_transit values)
  | List values -> List (List.map value_of_transit values)
  | Tagged ("u", Transit.String value) -> Uuid (Internal.Util.uuid_canonicalize value)
  | Tagged ("m", Transit.Int value) -> Instant (Int64.of_int value)
  | Tagged ("m", Transit.Int64 value) -> Instant value
  | Tagged ("regex", Transit.String value) -> Regex value
  | Tagged (tag, value) -> Vector [ String tag; value_of_transit value ]

let datom_to_transit datom =
  let tx = if datom.DT.added then datom.DT.tx else Int64.neg datom.DT.tx in
  Transit.Array
    [ Transit.Int64 datom.DT.e
    ; Transit.Keyword datom.DT.a
    ; impl_value_to_transit datom.DT.v
    ; Transit.Int64 tx
    ]

let int_of_transit label value =
  match int_of_transit_value value with
  | Some value -> value
  | None -> invalid_arg (label ^ " must be a Transit integer")

let int64_of_transit label value =
  match int64_of_transit_value value with
  | Some value -> value
  | None -> invalid_arg (label ^ " must be a Transit integer")

let datom_of_transit = function
  | Transit.Array [ entity; attr; value; tx ] ->
      let e = int64_of_transit "datom entity" entity in
      let a =
        match keyword_of_transit attr with
        | Some attr -> attr
        | None -> invalid_arg "datom attr must be a Transit keyword"
      in
      let tx = int64_of_transit "datom tx" tx in
      { DT.e; DT.a = a; DT.v = impl_value_of_transit value; DT.tx = Int64.abs tx; DT.added = tx >= 0L }
  | _ -> invalid_arg "storage datom must be [e a v tx]"

let datoms_to_transit datoms = Transit.Array (List.map datom_to_transit datoms)

let datoms_of_transit = function
  | Transit.Array datoms | Transit.List datoms -> List.map datom_of_transit datoms
  | _ -> invalid_arg "storage datoms must be a Transit array"

let index_metadata_to_transit metadata =
  Transit.Map
    [
      (Transit.Keyword "count", Transit.Int metadata.DT.storage_index_count);
      (Transit.Keyword "shift", Transit.Int metadata.DT.storage_index_shift);
    ]

let optional_metadata_entry key = function
  | Some metadata -> [ (Transit.Keyword key, index_metadata_to_transit metadata) ]
  | None -> []

let storage_root_to_transit root =
  Transit.Map
    ([
       (Transit.Keyword "schema", schema_to_transit ~eids:root.DT.storage_schema_idents root.DT.storage_schema);
       (Transit.Keyword "max-eid", Transit.Int64 root.DT.storage_max_eid);
       (Transit.Keyword "max-tx", Transit.Int64 root.DT.storage_max_tx);
       (Transit.Keyword "eavt", address_to_transit root.DT.storage_eavt);
       (Transit.Keyword "aevt", address_to_transit root.DT.storage_aevt);
       (Transit.Keyword "avet", address_to_transit root.DT.storage_avet);
       (Transit.Keyword "duplicate-datoms", datoms_to_transit root.DT.storage_duplicate_datoms);
       (Transit.Keyword "max-addr", Transit.Int root.DT.storage_max_addr);
       (Transit.Keyword "branching-factor", Transit.Int root.DT.storage_branching_factor);
       (Transit.Keyword "ref-type", transit_of_ref_type root.DT.storage_ref_type);
     ]
    @ optional_metadata_entry "eavt-metadata" root.DT.storage_eavt_metadata
    @ optional_metadata_entry "aevt-metadata" root.DT.storage_aevt_metadata
    @ optional_metadata_entry "avet-metadata" root.DT.storage_avet_metadata)

let storage_node_to_transit = function
  | PSet.Leaf datoms ->
      Transit.Map [ (Transit.Keyword "keys", datoms_to_transit (Array.to_list datoms)) ]
  | PSet.Branch (keys, child_addresses) ->
      Transit.Map
        [
          (Transit.Keyword "keys", datoms_to_transit (Array.to_list keys));
          (Transit.Keyword "children", Transit.Array (List.map address_to_transit (Array.to_list child_addresses)));
        ]

let storage_tail_to_transit groups =
  Transit.Array (List.map (fun group -> datoms_to_transit group) groups)

let payload_to_transit = function
  | DT.Storage_root root -> storage_root_to_transit root
  | DT.Storage_node node -> storage_node_to_transit node
  | DT.Storage_tail groups -> storage_tail_to_transit groups

let require_key key entries =
  match lookup_transit_key key entries with
  | Some value -> value
  | None -> invalid_arg ("storage payload is missing :" ^ key)

let optional_datoms key entries =
  match lookup_transit_key key entries with
  | None -> []
  | Some value -> datoms_of_transit value

let index_metadata_of_transit entries =
  match lookup_transit_key "count" entries, lookup_transit_key "shift" entries with
  | Some count, Some shift ->
      Some
        {
          DT.storage_index_count = int_of_transit "index metadata :count" count;
          DT.storage_index_shift = int_of_transit "index metadata :shift" shift;
        }
  | _ -> None

let optional_metadata key entries =
  match lookup_transit_key key entries with
  | Some (Transit.Map metadata) -> index_metadata_of_transit metadata
  | _ -> None

let storage_root_of_transit entries =
  {
    DT.storage_schema = schema_of_transit (require_key "schema" entries);
    DT.storage_schema_idents = schema_eids_of_transit (require_key "schema" entries);
    DT.storage_max_eid = int64_of_transit "storage root :max-eid" (require_key "max-eid" entries);
    DT.storage_max_tx = int64_of_transit "storage root :max-tx" (require_key "max-tx" entries);
    DT.storage_eavt = address_of_transit "storage root :eavt" (require_key "eavt" entries);
    DT.storage_aevt = address_of_transit "storage root :aevt" (require_key "aevt" entries);
    DT.storage_avet = address_of_transit "storage root :avet" (require_key "avet" entries);
    DT.storage_eavt_metadata = optional_metadata "eavt-metadata" entries;
    DT.storage_aevt_metadata = optional_metadata "aevt-metadata" entries;
    DT.storage_avet_metadata = optional_metadata "avet-metadata" entries;
    DT.storage_duplicate_datoms = optional_datoms "duplicate-datoms" entries;
    DT.storage_max_addr = int_of_transit "storage root :max-addr" (require_key "max-addr" entries);
    storage_branching_factor =
      int_of_transit "storage root :branching-factor" (require_key "branching-factor" entries);
    DT.storage_ref_type = ref_type_of_transit (require_key "ref-type" entries);
  }

let child_addresses_of_transit = function
  | Transit.Array values | Transit.List values ->
      List.map (address_of_transit "storage node :children") values
  | _ -> invalid_arg "storage node :children must be a Transit array"

let storage_node_of_transit entries =
  let keys = Array.of_list (datoms_of_transit (require_key "keys" entries)) in
  match lookup_transit_key "children" entries with
  | None -> PSet.Leaf keys
  | Some children ->
      PSet.Branch (keys, Array.of_list (child_addresses_of_transit children))

let storage_tail_of_transit = function
  | Transit.Array groups | Transit.List groups -> List.map datoms_of_transit groups
  | _ -> invalid_arg "storage tail must be a Transit array"

let payload_of_transit = function
  | Transit.Map entries ->
      if Option.is_some (lookup_transit_key "schema" entries) then DT.Storage_root (storage_root_of_transit entries)
      else if Option.is_some (lookup_transit_key "keys" entries) then DT.Storage_node (storage_node_of_transit entries)
      else invalid_arg "unknown storage payload map"
  | (Transit.Array _ | Transit.List _) as tail -> DT.Storage_tail (storage_tail_of_transit tail)
  | _ -> invalid_arg "unknown storage payload"

(* cljs datascript writes storage payloads in transit's non-verbose JSON
   mode: maps as ["^ ",k,v] arrays with the write cache's ^N refs *)
let encode payload = payload |> payload_to_transit |> Transit.to_string ~mode:Transit.Normal
let decode content = content |> Transit.of_string |> payload_of_transit
