module Ds = Datascript_types
module PSet = Persistent_sorted_set
module Transit = Transit_melange.Transit.Json

open Ds

let schema_attr_default : Ds.schema_attr =
  {
    cardinality = One;
    unique = None;
    indexed = false;
    is_component = false;
    no_history = false;
    doc = None;
    value_type = None;
    tuple_attrs = None;
    tuple_types = None;
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

let lookup_transit_key key entries =
  List.find_map
    (fun (entry_key, value) ->
      match string_of_transit_key entry_key with
      | Some entry_key when String.equal entry_key key -> Some value
      | _ -> None)
    entries

let transit_of_cardinality = function
  | One -> Transit.Keyword "db.cardinality/one"
  | Many -> Transit.Keyword "db.cardinality/many"

let cardinality_of_transit = function
  | Transit.Keyword "db.cardinality/many" -> Many
  | Transit.Keyword "db.cardinality/one" -> One
  | _ -> One

let transit_of_unique = function
  | Value -> Transit.Keyword "db.unique/value"
  | Identity -> Transit.Keyword "db.unique/identity"

let unique_of_transit = function
  | Transit.Keyword "db.unique/value" -> Some Value
  | Transit.Keyword "db.unique/identity" -> Some Identity
  | _ -> None

let transit_of_value_type = function
  | RefType -> Transit.Keyword "db.type/ref"
  | StringType -> Transit.Keyword "db.type/string"
  | KeywordType -> Transit.Keyword "db.type/keyword"
  | NumberType -> Transit.Keyword "db.type/number"
  | UuidType -> Transit.Keyword "db.type/uuid"
  | InstantType -> Transit.Keyword "db.type/instant"
  | TupleType -> Transit.Keyword "db.type/tuple"

let value_type_of_transit = function
  | Transit.Keyword "db.type/ref" -> Some RefType
  | Transit.Keyword "db.type/string" -> Some StringType
  | Transit.Keyword "db.type/keyword" -> Some KeywordType
  | Transit.Keyword "db.type/number" -> Some NumberType
  | Transit.Keyword "db.type/uuid" -> Some UuidType
  | Transit.Keyword "db.type/instant" -> Some InstantType
  | Transit.Keyword "db.type/tuple" -> Some TupleType
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

let schema_attr_to_transit attr =
  let entries = ref [] in
  let add key value = entries := (Transit.Keyword key, value) :: !entries in
  (match attr.cardinality with
  | One -> ()
  | Many -> add "db/cardinality" (transit_of_cardinality attr.cardinality));
  Option.iter (fun unique -> add "db/unique" (transit_of_unique unique)) attr.unique;
  if attr.indexed then add "db/index" (Transit.Bool true);
  if attr.is_component then add "db/isComponent" (Transit.Bool true);
  if attr.no_history then add "db/noHistory" (Transit.Bool true);
  Option.iter (fun doc -> add "db/doc" (Transit.String doc)) attr.doc;
  Option.iter
    (fun value_type -> add "db/valueType" (transit_of_value_type value_type))
    attr.value_type;
  Option.iter (fun attrs -> add "db/tupleAttrs" (transit_of_tuple_attrs attrs)) attr.tuple_attrs;
  Option.iter (fun types -> add "db/tupleTypes" (transit_of_tuple_types types)) attr.tuple_types;
  Transit.Map (List.rev !entries)

let schema_to_transit schema =
  Transit.Map
    (List.map
       (fun (attr, schema_attr) -> (Transit.Keyword attr, schema_attr_to_transit schema_attr))
       schema)

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
          | Some "db/cardinality" -> { schema with cardinality = cardinality_of_transit value }
          | Some "db/unique" -> { schema with unique = unique_of_transit value }
          | Some "db/index" ->
              { schema with indexed = Option.value (bool_of_transit value) ~default:false }
          | Some "db/isComponent" ->
              { schema with is_component = Option.value (bool_of_transit value) ~default:false }
          | Some "db/noHistory" ->
              { schema with no_history = Option.value (bool_of_transit value) ~default:false }
          | Some "db/doc" -> { schema with doc = string_of_transit value }
          | Some "db/valueType" -> { schema with value_type = value_type_of_transit value }
          | Some "db/tupleAttrs" -> { schema with tuple_attrs = tuple_attrs_of_transit value }
          | Some "db/tupleTypes" -> { schema with tuple_types = tuple_types_of_transit value }
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

let rec value_to_transit = function
  | Ds.Nil -> Transit.Null
  | Int value -> Transit.Int value
  | Float value -> Transit.Float value
  | String value -> Transit.String value
  | Symbol value -> Transit.Symbol value
  | Bool value -> Transit.Bool value
  | Keyword value -> Transit.Keyword value
  | Uuid value -> Transit.Tagged ("u", Transit.String value)
  | Instant value -> Transit.Tagged ("m", Transit.Int value)
  | Regex value -> Transit.Tagged ("regex", Transit.String value)
  | Ref entity_id -> Transit.Int entity_id
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
  | Int value -> Int value
  | Int64 value ->
      if
        Int64.compare value (Int64.of_int min_int) >= 0
        && Int64.compare value (Int64.of_int max_int) <= 0
      then Int (Int64.to_int value)
      else Instant (Int64.to_int value)
  | Float value -> Float value
  | Binary value -> String value
  | Big_decimal value -> Float (float_of_string value)
  | Big_int value -> Transit.Int64 (Int64.of_string value) |> value_of_transit
  | Date value -> Instant (Int64.to_int value)
  | Uuid value -> Uuid value
  | Uri value -> String value
  | Keyword value -> Keyword value
  | Symbol value -> Symbol value
  | Array values -> Vector (List.map value_of_transit values)
  | Map entries -> Map (List.map (fun (key, value) -> (value_of_transit key, value_of_transit value)) entries)
  | Set values -> Set (List.map value_of_transit values)
  | List values -> List (List.map value_of_transit values)
  | Tagged ("u", Transit.String value) -> Uuid value
  | Tagged ("m", Transit.Int value) -> Instant value
  | Tagged ("m", Transit.Int64 value) -> Instant (Int64.to_int value)
  | Tagged ("regex", Transit.String value) -> Regex value
  | Tagged (tag, value) -> Vector [ String tag; value_of_transit value ]

let datom_to_transit datom =
  let tx = if datom.Ds.added then datom.tx else -datom.tx in
  Transit.Array [ Transit.Int datom.e; Transit.Keyword datom.a; value_to_transit datom.v; Transit.Int tx ]

let int_of_transit label value =
  match int_of_transit_value value with
  | Some value -> value
  | None -> invalid_arg (label ^ " must be a Transit integer")

let datom_of_transit = function
  | Transit.Array [ entity; attr; value; tx ] ->
      let e = int_of_transit "datom entity" entity in
      let a =
        match keyword_of_transit attr with
        | Some attr -> attr
        | None -> invalid_arg "datom attr must be a Transit keyword"
      in
      let tx = int_of_transit "datom tx" tx in
      { Ds.e; a; v = value_of_transit value; tx = abs tx; added = tx >= 0 }
  | _ -> invalid_arg "storage datom must be [e a v tx]"

let datoms_to_transit datoms = Transit.Array (List.map datom_to_transit datoms)

let datoms_of_transit = function
  | Transit.Array datoms | Transit.List datoms -> List.map datom_of_transit datoms
  | _ -> invalid_arg "storage datoms must be a Transit array"

let storage_root_to_transit root =
  Transit.Map
    [
      (Transit.Keyword "schema", schema_to_transit root.storage_schema);
      (Transit.Keyword "max-eid", Transit.Int root.storage_max_eid);
      (Transit.Keyword "max-tx", Transit.Int root.storage_max_tx);
      (Transit.Keyword "eavt", address_to_transit root.storage_eavt);
      (Transit.Keyword "aevt", address_to_transit root.storage_aevt);
      (Transit.Keyword "avet", address_to_transit root.storage_avet);
      (Transit.Keyword "duplicate-datoms", datoms_to_transit root.storage_duplicate_datoms);
      (Transit.Keyword "max-addr", Transit.Int root.storage_max_addr);
      (Transit.Keyword "branching-factor", Transit.Int root.storage_branching_factor);
      (Transit.Keyword "ref-type", transit_of_ref_type root.storage_ref_type);
    ]

let storage_node_to_transit = function
  | PSet.Leaf datoms -> Transit.Map [ (Transit.Keyword "keys", datoms_to_transit datoms) ]
  | PSet.Branch (keys, child_addresses) ->
      Transit.Map
        [
          (Transit.Keyword "keys", datoms_to_transit keys);
          (Transit.Keyword "children", Transit.Array (List.map address_to_transit child_addresses));
        ]

let storage_tail_to_transit groups =
  Transit.Array (List.map (fun group -> datoms_to_transit group) groups)

let payload_to_transit = function
  | Ds.Storage_root root -> storage_root_to_transit root
  | Storage_node node -> storage_node_to_transit node
  | Storage_tail groups -> storage_tail_to_transit groups

let require_key key entries =
  match lookup_transit_key key entries with
  | Some value -> value
  | None -> invalid_arg ("storage payload is missing :" ^ key)

let optional_datoms key entries =
  match lookup_transit_key key entries with
  | None -> []
  | Some value -> datoms_of_transit value

let storage_root_of_transit entries =
  {
    Ds.storage_schema = schema_of_transit (require_key "schema" entries);
    storage_max_eid = int_of_transit "storage root :max-eid" (require_key "max-eid" entries);
    storage_max_tx = int_of_transit "storage root :max-tx" (require_key "max-tx" entries);
    storage_eavt = address_of_transit "storage root :eavt" (require_key "eavt" entries);
    storage_aevt = address_of_transit "storage root :aevt" (require_key "aevt" entries);
    storage_avet = address_of_transit "storage root :avet" (require_key "avet" entries);
    storage_duplicate_datoms = optional_datoms "duplicate-datoms" entries;
    storage_max_addr = int_of_transit "storage root :max-addr" (require_key "max-addr" entries);
    storage_branching_factor =
      int_of_transit "storage root :branching-factor" (require_key "branching-factor" entries);
    storage_ref_type = ref_type_of_transit (require_key "ref-type" entries);
  }

let child_addresses_of_transit = function
  | Transit.Array values | Transit.List values ->
      List.map (address_of_transit "storage node :children") values
  | _ -> invalid_arg "storage node :children must be a Transit array"

let storage_node_of_transit entries =
  let keys = datoms_of_transit (require_key "keys" entries) in
  match lookup_transit_key "children" entries with
  | None -> PSet.Leaf keys
  | Some children -> PSet.Branch (keys, child_addresses_of_transit children)

let storage_tail_of_transit = function
  | Transit.Array groups | Transit.List groups -> List.map datoms_of_transit groups
  | _ -> invalid_arg "storage tail must be a Transit array"

let payload_of_transit = function
  | Transit.Map entries ->
      if Option.is_some (lookup_transit_key "schema" entries) then Storage_root (storage_root_of_transit entries)
      else if Option.is_some (lookup_transit_key "keys" entries) then Storage_node (storage_node_of_transit entries)
      else invalid_arg "unknown storage payload map"
  | (Transit.Array _ | Transit.List _) as tail -> Storage_tail (storage_tail_of_transit tail)
  | _ -> invalid_arg "unknown storage payload"

let encode payload = payload |> payload_to_transit |> Transit.to_string ~mode:Transit.Verbose
let decode content = content |> Transit.of_string |> payload_of_transit
