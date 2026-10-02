module Ds = Datascript
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
  | Int64 value -> Transit.Int64 value
  | Float value -> Transit.Float value
  | String value -> Transit.String value
  | Symbol value -> Transit.Symbol value
  | Bool value -> Transit.Bool value
  | Keyword value -> Transit.Keyword value
  | Uuid value -> Transit.Uuid value
  | Instant value -> Transit.Date value
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
  | Int value -> Int64 (Int64.of_int value)
  | Int64 value -> Int64 value
  | Float value -> Float value
  | Binary value -> String value
  | Big_decimal value -> Float (float_of_string value)
  | Big_int value -> Int64 (Int64.of_string value)
  | Date value -> Instant value
  | Uuid value -> Uuid (Util.uuid_canonicalize value)
  | Uri value -> String value
  | Keyword value -> Keyword value
  | Symbol value -> Symbol value
  | Array values -> Vector (List.map value_of_transit values)
  | Map entries -> Map (List.map (fun (key, value) -> (value_of_transit key, value_of_transit value)) entries)
  | Set values -> Set (List.map value_of_transit values)
  | List values -> List (List.map value_of_transit values)
  | Tagged ("u", Transit.String value) -> Uuid (Util.uuid_canonicalize value)
  | Tagged ("m", Transit.Int value) -> Instant (Int64.of_int value)
  | Tagged ("m", Transit.Int64 value) -> Instant value
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

let index_metadata_to_transit metadata =
  Transit.Map
    [
      (Transit.Keyword "count", Transit.Int metadata.storage_index_count);
      (Transit.Keyword "shift", Transit.Int metadata.storage_index_shift);
    ]

let optional_metadata_entry key = function
  | Some metadata -> [ (Transit.Keyword key, index_metadata_to_transit metadata) ]
  | None -> []

let storage_root_to_transit root =
  Transit.Map
    ([
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
    @ optional_metadata_entry "eavt-metadata" root.storage_eavt_metadata
    @ optional_metadata_entry "aevt-metadata" root.storage_aevt_metadata
    @ optional_metadata_entry "avet-metadata" root.storage_avet_metadata)

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

let index_metadata_of_transit entries =
  match lookup_transit_key "count" entries, lookup_transit_key "shift" entries with
  | Some count, Some shift ->
      Some
        {
          Ds.storage_index_count = int_of_transit "index metadata :count" count;
          storage_index_shift = int_of_transit "index metadata :shift" shift;
        }
  | _ -> None

let optional_metadata key entries =
  match lookup_transit_key key entries with
  | Some (Transit.Map metadata) -> index_metadata_of_transit metadata
  | _ -> None

let storage_root_of_transit entries =
  {
    Ds.storage_schema = schema_of_transit (require_key "schema" entries);
    storage_max_eid = int_of_transit "storage root :max-eid" (require_key "max-eid" entries);
    storage_max_tx = int_of_transit "storage root :max-tx" (require_key "max-tx" entries);
    storage_eavt = address_of_transit "storage root :eavt" (require_key "eavt" entries);
    storage_aevt = address_of_transit "storage root :aevt" (require_key "aevt" entries);
    storage_avet = address_of_transit "storage root :avet" (require_key "avet" entries);
    storage_eavt_metadata = optional_metadata "eavt-metadata" entries;
    storage_aevt_metadata = optional_metadata "aevt-metadata" entries;
    storage_avet_metadata = optional_metadata "avet-metadata" entries;
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
      if Option.is_some (lookup_transit_key "schema" entries) then Storage_root (storage_root_of_transit entries)
      else if Option.is_some (lookup_transit_key "keys" entries) then Storage_node (storage_node_of_transit entries)
      else invalid_arg "unknown storage payload map"
  | (Transit.Array _ | Transit.List _) as tail -> Storage_tail (storage_tail_of_transit tail)
  | _ -> invalid_arg "unknown storage payload"

(* Raw transit-js decode for the hot storage shapes (datom nodes and
   tails). cljs storage consumes the transit-js objects directly, so routing
   every node through the intermediate Transit.value tree costs a full extra
   walk plus one allocation layer per decoded node. Obj.magic is unavoidable
   here: the transit-js FFI type is [any] and the concrete runtime shape is
   only known after Js.Json.classify. *)
module Raw = struct
  type t
  type reader
  type reader_options

  external reader_options : preferBuffers:bool -> unit -> reader_options = ""
  [@@mel.obj]

  external reader : string -> reader_options -> reader = "reader"
  [@@mel.module "transit-js"]

  external read : reader -> string -> t = "read" [@@mel.send]
  external is_keyword : t -> bool = "isKeyword" [@@mel.module "transit-js"]
  external is_symbol : t -> bool = "isSymbol" [@@mel.module "transit-js"]
  external is_uuid : t -> bool = "isUUID" [@@mel.module "transit-js"]
  external is_big_int : t -> bool = "isBigInt" [@@mel.module "transit-js"]
  external is_big_decimal : t -> bool = "isBigDec" [@@mel.module "transit-js"]
  external is_binary : t -> bool = "isBinary" [@@mel.module "transit-js"]
  external is_uri : t -> bool = "isURI" [@@mel.module "transit-js"]
  external is_map : t -> bool = "isMap" [@@mel.module "transit-js"]
  external is_set : t -> bool = "isSet" [@@mel.module "transit-js"]
  external is_list : t -> bool = "isList" [@@mel.module "transit-js"]

  external is_tagged_value : t -> bool = "isTaggedValue"
  [@@mel.module "transit-js"]

  external is_integer : t -> bool = "isInteger" [@@mel.module "transit-js"]
  external to_string : t -> string = "toString" [@@mel.send]
  external tag : t -> string = "tag" [@@mel.get]
  external rep : t -> t = "rep" [@@mel.get]

  external for_each_map : t -> ((t -> t -> unit)[@mel.uncurry]) -> unit = "forEach"
  [@@mel.send]

  external for_each_set : t -> ((t -> t -> unit)[@mel.uncurry]) -> unit = "forEach"
  [@@mel.send]

  external get_time_method : t -> (unit -> float) Js.undefined = "getTime"
  [@@mel.get]

  external get_time : t -> float = "getTime" [@@mel.send]
  external length : t -> int = "length" [@@mel.get]
  external get_index : t -> int -> int = "" [@@mel.get_index]
end

let raw_jsobject (v : Raw.t) : Raw.t Js.Dict.t = (Obj.magic v : Raw.t Js.Dict.t)

let raw_uint8_string v =
  let output = Buffer.create (Raw.length v) in
  for index = 0 to Raw.length v - 1 do
    Buffer.add_char output (Char.chr (Raw.get_index v index))
  done;
  Buffer.contents output

let keyword_name_of text =
  if String.length text > 0 && Char.equal text.[0] ':' then
    String.sub text 1 (String.length text - 1)
  else text

let is_raw_date v = Option.is_some (Js.undefinedToOption (Raw.get_time_method v))

let int_of_raw_opt (v : Raw.t) =
  match Js.Json.classify (Obj.magic v) with
  | JSONNumber number
    when Float.equal number (floor number)
         && number >= Float.of_int min_int
         && number <= Float.of_int max_int ->
    Some (int_of_float number)
  | _ when Raw.is_integer v -> int_of_string_opt (Raw.to_string v)
  | _ -> None

let int_of_raw label v =
  match int_of_raw_opt v with
  | Some value -> value
  | None -> invalid_arg (label ^ " must be a Transit integer")

let rec value_of_raw (v : Raw.t) : Ds.value =
  match Js.Json.classify (Obj.magic v) with
  | JSONNull -> Nil
  | JSONFalse -> Bool false
  | JSONTrue -> Bool true
  | JSONString text -> String text
  | JSONNumber number ->
    if Float.equal number (floor number)
       && number >= Float.of_int min_int
       && number <= Float.of_int max_int
    then Int64 (Int64.of_float number)
    else Float number
  | JSONArray values ->
    Vector (List.map (fun item -> value_of_raw (Obj.magic item)) (Array.to_list values))
  | JSONObject _ ->
    if Raw.is_keyword v then Keyword (keyword_name_of (Raw.to_string v))
    else if Raw.is_symbol v then Symbol (Raw.to_string v)
    else if Raw.is_uuid v then Uuid (Util.uuid_canonicalize (Raw.to_string v))
    else if Raw.is_big_int v then Int64 (Int64.of_string (Raw.to_string (Raw.rep v)))
    else if Raw.is_big_decimal v then Float (float_of_string (Raw.to_string (Raw.rep v)))
    else if Raw.is_binary v then String (raw_uint8_string v)
    else if Raw.is_uri v then String (Raw.to_string (Raw.rep v))
    else if Raw.is_map v then raw_map_value v
    else if Raw.is_set v then raw_set_value v
    else if Raw.is_list v then raw_list_value v
    else if Raw.is_tagged_value v then raw_tagged_value v
    else if Raw.is_integer v then Int64 (Int64.of_string (Raw.to_string v))
    else if is_raw_date v then Instant (Int64.of_float (Raw.get_time v))
    else
      Map
        (List.map
           (fun (key, value) -> (String key, value_of_raw (Obj.magic value)))
           (Array.to_list (Js.Dict.entries (raw_jsobject v))))

and raw_map_value v =
  let entries = ref [] in
  Raw.for_each_map v (fun value key -> entries := (value_of_raw key, value_of_raw value) :: !entries);
  Map (List.rev !entries)

and raw_set_value v =
  let values = ref [] in
  Raw.for_each_set v (fun value _ -> values := value_of_raw value :: !values);
  Set (List.rev !values)

and raw_list_value v =
  match Js.Json.classify (Obj.magic (Raw.rep v)) with
  | JSONArray values ->
    List (List.map (fun item -> value_of_raw (Obj.magic item)) (Array.to_list values))
  | _ -> invalid_arg "Transit list expects an array representation"

and raw_tagged_value v =
  match Raw.tag v with
  | "u" ->
    (match Js.Json.classify (Obj.magic (Raw.rep v)) with
     | JSONString text -> Uuid (Util.uuid_canonicalize text)
     | _ -> invalid_arg "Transit uuid tag expects a string rep")
  | "m" ->
    (match int_of_raw_opt (Raw.rep v) with
     | Some value -> Instant (Int64.of_int value)
     | None -> invalid_arg "Transit instant tag expects an integer rep")
  | "regex" ->
    (match Js.Json.classify (Obj.magic (Raw.rep v)) with
     | JSONString text -> Regex text
     | _ -> invalid_arg "Transit regex tag expects a string rep")
  | tag -> Vector [ String tag; value_of_raw (Raw.rep v) ]

let datom_of_raw (v : Raw.t) : Ds.datom =
  match Js.Json.classify (Obj.magic v) with
  | JSONArray _ ->
    let items : Raw.t array = (Obj.magic v : Raw.t array) in
    if Array.length items <> 4 then invalid_arg "storage datom must be [e a v tx]";
    let e = int_of_raw "datom entity" items.(0) in
    let attr = items.(1) in
    let a =
      if Raw.is_keyword attr then keyword_name_of (Raw.to_string attr)
      else invalid_arg "datom attr must be a Transit keyword"
    in
    let tx = int_of_raw "datom tx" items.(3) in
    { Ds.e; a; v = value_of_raw items.(2); tx = abs tx; added = tx >= 0 }
  | _ -> invalid_arg "storage datom must be [e a v tx]"

let datoms_of_raw label (v : Raw.t) : Ds.datom array =
  match Js.Json.classify (Obj.magic v) with
  | JSONArray _ ->
    Array.map (fun item -> datom_of_raw (Obj.magic item)) ((Obj.magic v : Raw.t array))
  | _ -> invalid_arg (label ^ " must be a Transit array")

let raw_address_of (v : Raw.t) : storage_address =
  match Js.Json.classify (Obj.magic v) with
  | JSONString address -> address
  | JSONNumber number when Float.equal number (floor number) ->
    string_of_int (int_of_float number)
  | _ when Raw.is_integer v -> Raw.to_string v
  | _ -> invalid_arg "storage node :children must contain addresses"

(* transit-js map keys are keyword objects; verbose JSON-map keys carry the
   "~:name" tag prefix. Both normalize to the bare name. *)
let raw_map_entries (v : Raw.t) : (string * Raw.t) list =
  if Raw.is_map v then (
    let entries = ref [] in
    Raw.for_each_map v (fun value key ->
        entries := (keyword_name_of (Raw.to_string key), value) :: !entries);
    !entries)
  else
    raw_jsobject v |> Js.Dict.entries |> Array.to_list
    |> List.map (fun (key, value) ->
         let name =
           if String.length key > 2 && String.equal (String.sub key 0 2) "~:" then
             String.sub key 2 (String.length key - 2)
           else key
         in
         (name, (Obj.magic value : Raw.t)))

let lookup_raw_key key entries =
  List.find_map
    (fun (entry_key, value) ->
      if String.equal entry_key key then Some value else None)
    entries

exception Raw_payload_fallback

(* Returns [Some payload] for the hot node/tail shapes; [None] for anything
   else (storage root, unknown payloads) so the caller can fall back to the
   slow full decode. *)
let payload_of_raw_opt (v : Raw.t) : Ds.storage_payload option =
  match Js.Json.classify (Obj.magic v) with
  | JSONArray groups ->
    Some
      (Storage_tail
         (List.map
            (fun group ->
              Array.to_list (datoms_of_raw "storage tail" (Obj.magic group)))
            (Array.to_list groups)))
  | JSONObject _ ->
    (match lookup_raw_key "keys" (raw_map_entries v) with
     | None -> None
     | Some keys ->
       let keys = datoms_of_raw "storage node :keys" keys in
       (match lookup_raw_key "children" (raw_map_entries v) with
        | None -> Some (Storage_node (PSet.Leaf keys))
        | Some children ->
          (match Js.Json.classify (Obj.magic children) with
           | JSONArray _ ->
             Some
               (Storage_node
                  (PSet.Branch
                     ( keys
                     , Array.map
                         (fun item -> raw_address_of (Obj.magic item))
                         ((Obj.magic children : Raw.t array)) )))
           | _ -> invalid_arg "storage node :children must be a Transit array")))
  | _ -> None

let encode payload = payload |> payload_to_transit |> Transit.to_string ~mode:Transit.Verbose

let raw_reader () = Raw.reader "json" (Raw.reader_options ~preferBuffers:false ())

let decode content =
  match payload_of_raw_opt (Raw.read (raw_reader ()) content) with
  | Some payload -> payload
  | None -> content |> Transit.of_string |> payload_of_transit
