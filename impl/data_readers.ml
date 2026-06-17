open Datascript_types

type context =
  { tx0 : tx
  ; read_edn : string -> query_form
  ; query_value_of_form : query_form -> value
  ; datom : ?tx:tx -> ?added:bool -> e:entity_id -> a:attr -> v:value -> unit -> datom
  ; validate_schema : schema -> schema
  ; empty_db : ?schema:schema -> unit -> db
  ; max_eid_in_value : int -> value -> int
  ; resolve_value_for_attr :
      db ->
      attr ->
      datom list ->
      tx ->
      int ->
      (string * entity_id) list ->
      value ->
      value * int * (string * entity_id) list
  ; init_db : ?schema:schema -> datom list -> db
  }

let attr_of_edn_key = function
  | QueryFormKeyword attr | QueryFormString attr | QueryFormSymbol attr -> attr
  | _ -> invalid_arg "expected EDN keyword, string, or symbol attr"

let tx_attr_of_edn_key key =
  match attr_of_edn_key key with
  | attr -> attr
  | exception Invalid_argument _ -> invalid_arg "Bad entity attribute"

let tx_op_name_of_edn_form form =
  match attr_of_edn_key form with
  | op -> op
  | exception Invalid_argument _ -> invalid_arg "Unknown operation"

let is_edn_attr_key = function
  | QueryFormKeyword _ | QueryFormString _ | QueryFormSymbol _ -> true
  | _ -> false

let keyword_name_of_form = function
  | QueryFormKeyword value | QueryFormSymbol value | QueryFormString value -> value
  | _ -> invalid_arg "expected EDN keyword or symbol"

let rec entity_ref_of_edn_form context = function
  | QueryFormInt entity_id when entity_id < 0 -> Temp_id (string_of_int entity_id)
  | QueryFormInt entity_id -> Entity_id entity_id
  | QueryFormString tempid -> Temp_id tempid
  | QueryFormKeyword "db/current-tx"
  | QueryFormSymbol "db/current-tx" -> CurrentTx
  | QueryFormSymbol ("datomic.tx" | "datascript.tx" as tempid) -> Temp_id tempid
  | QueryFormKeyword ident -> Ident ident
  | QueryFormVector [ attr; value ] | QueryFormList [ attr; value ] ->
    Lookup_ref (attr_of_edn_key attr, tx_scalar_value_of_edn_form context value)
  | _ -> invalid_arg "expected EDN entity ref"

and tx_db_id_ref_of_edn_form context form =
  match entity_ref_of_edn_form context form with
  | entity_ref -> entity_ref
  | exception Invalid_argument _ -> invalid_arg "Expected number, string or lookup ref for :db/id"

and tx_entity_ref_of_edn_form context form =
  match entity_ref_of_edn_form context form with
  | entity_ref -> entity_ref
  | exception Invalid_argument _ -> invalid_arg "Expected number or lookup ref for entity id"

and tx_scalar_value_of_edn_form context = function
  | QueryFormVector [ QueryFormKeyword "db/id"; ref_form ]
  | QueryFormVector [ QueryFormSymbol "db/id"; ref_form ]
  | QueryFormList [ QueryFormKeyword "db/id"; ref_form ]
  | QueryFormList [ QueryFormSymbol "db/id"; ref_form ] ->
    Ref_to (entity_ref_of_edn_form context ref_form)
  | form -> context.query_value_of_form form

and tx_value_of_edn_form context = function
  | QueryFormMap entries -> One_entity (tx_entity_of_edn_map context entries)
  | QueryFormSet values when List.for_all (function QueryFormMap _ -> true | _ -> false) values ->
    Many_entities
      (List.map
         (function
           | QueryFormMap entries -> tx_entity_of_edn_map context entries
           | _ -> assert false)
         values)
  | QueryFormSet values -> Many_values (List.map (tx_scalar_value_of_edn_form context) values)
  | (QueryFormVector [ QueryFormKeyword "db/id"; _ ]
    | QueryFormVector [ QueryFormSymbol "db/id"; _ ]
    | QueryFormList [ QueryFormKeyword "db/id"; _ ]
    | QueryFormList [ QueryFormSymbol "db/id"; _ ] as form) ->
    One_value (tx_scalar_value_of_edn_form context form)
  | (QueryFormVector [ attr; _ ] | QueryFormList [ attr; _ ] as form) when is_edn_attr_key attr ->
    One_value (tx_scalar_value_of_edn_form context form)
  | QueryFormVector values | QueryFormList values ->
    if List.for_all (function QueryFormMap _ -> true | _ -> false) values then
      Many_entities
        (List.map
           (function
             | QueryFormMap entries -> tx_entity_of_edn_map context entries
             | _ -> assert false)
           values)
    else
      Many_values (List.map (tx_scalar_value_of_edn_form context) values)
  | form -> One_value (tx_scalar_value_of_edn_form context form)

and tx_attr_values_of_edn_form context attr = function
  | (QueryFormVector [ key; _ ] | QueryFormList [ key; _ ] as form) when is_edn_attr_key key ->
    [ attr, tx_value_of_edn_form context form ]
  | (QueryFormVector values | QueryFormList values | QueryFormSet values as form) ->
    let nested, scalars =
      List.fold_left
        (fun (nested, scalars) -> function
          | QueryFormMap entries -> tx_entity_of_edn_map context entries :: nested, scalars
          | form -> nested, tx_scalar_value_of_edn_form context form :: scalars)
        ([], [])
        values
    in
    let scalar_collection values =
      match form with
      | QueryFormSet _ -> Set values
      | QueryFormVector _ -> Vector values
      | _ -> List values
    in
    (match List.rev nested, List.rev scalars with
     | [], scalars -> [ attr, One_value (scalar_collection scalars) ]
     | nested, [] -> [ attr, Many_entities nested ]
     | nested, scalars -> [ attr, Many_entities nested; attr, One_value (scalar_collection scalars) ])
  | form -> [ attr, tx_value_of_edn_form context form ]

and tx_entity_of_edn_map context entries =
  let db_id, attrs =
    List.fold_left
      (fun (db_id, attrs) (key, value) ->
        match tx_attr_of_edn_key key with
        | "db/id" -> Some (tx_db_id_ref_of_edn_form context value), attrs
        | attr -> db_id, List.rev_append (tx_attr_values_of_edn_form context attr value) attrs)
      (None, [])
      entries
  in
  { db_id; attrs = List.rev attrs }

let explicit_tx_of_edn_form = function
  | QueryFormInt tx -> tx
  | _ -> invalid_arg "explicit transaction tx must be an integer"

let entity_id_of_explicit_datom_edn_form context form =
  match entity_ref_of_edn_form context form with
  | Entity_id entity_id -> entity_id
  | _ -> invalid_arg "explicit transaction datoms require entity ids"

let raw_datom_of_edn_forms context ?(added = true) entity_ref attr value tx =
  Raw_datom
    (context.datom
       ~tx:(explicit_tx_of_edn_form tx)
       ~added
       ~e:(entity_id_of_explicit_datom_edn_form context entity_ref)
       ~a:(tx_attr_of_edn_key attr)
       ~v:(tx_scalar_value_of_edn_form context value)
       ())

let raw_datom_of_tagged_edn_form context = function
  | QueryFormVector [ entity_ref; attr; value ]
  | QueryFormList [ entity_ref; attr; value ] ->
    raw_datom_of_edn_forms context entity_ref attr value (QueryFormInt context.tx0)
  | QueryFormVector [ entity_ref; attr; value; tx ]
  | QueryFormList [ entity_ref; attr; value; tx ] ->
    raw_datom_of_edn_forms context entity_ref attr value tx
  | QueryFormVector [ entity_ref; attr; value; tx; QueryFormBool added ]
  | QueryFormList [ entity_ref; attr; value; tx; QueryFormBool added ] ->
    raw_datom_of_edn_forms context ~added entity_ref attr value tx
  | _ -> invalid_arg "datascript/Datom literal requires [e a v], [e a v tx], or [e a v tx added]"

let tx_op_of_edn_form context = function
  | QueryFormTagged ("datascript/Datom", form) -> raw_datom_of_tagged_edn_form context form
  | QueryFormMap entries -> Entity (tx_entity_of_edn_map context entries)
  | QueryFormVector forms | QueryFormList forms ->
    (match forms with
     | op :: entity_ref :: attr :: value :: [] ->
       (match tx_op_name_of_edn_form op with
        | "add" | "db/add" ->
          Add (tx_entity_ref_of_edn_form context entity_ref, tx_attr_of_edn_key attr, tx_scalar_value_of_edn_form context value)
        | "retract" | "db/retract" ->
          Retract (tx_entity_ref_of_edn_form context entity_ref, tx_attr_of_edn_key attr, Some (tx_scalar_value_of_edn_form context value))
        | "db/cas" | "db.fn/cas" ->
          invalid_arg "db/cas requires entity, attr, expected value, and new value"
        | _ -> invalid_arg "Unknown operation")
     | op :: entity_ref :: attr :: expected :: value_or_tx :: [] ->
       (match tx_op_name_of_edn_form op with
        | "add" | "db/add" -> raw_datom_of_edn_forms context entity_ref attr expected value_or_tx
        | "retract" | "db/retract" -> raw_datom_of_edn_forms context ~added:false entity_ref attr expected value_or_tx
        | "db/cas" | "db.fn/cas" ->
          CompareAndSet
            ( tx_entity_ref_of_edn_form context entity_ref
            , tx_attr_of_edn_key attr
            , (match expected with
               | QueryFormNil -> None
               | _ -> Some (tx_scalar_value_of_edn_form context expected))
            , tx_scalar_value_of_edn_form context value_or_tx
            )
        | _ -> invalid_arg "Unknown operation")
     | [ op; entity_ref; attr ] ->
       (match tx_op_name_of_edn_form op with
        | "retract" | "db/retract" -> Retract (tx_entity_ref_of_edn_form context entity_ref, tx_attr_of_edn_key attr, None)
        | "db/retractAttribute" | "db.fn/retractAttribute" ->
          RetractAttr (tx_entity_ref_of_edn_form context entity_ref, tx_attr_of_edn_key attr)
        | _ -> invalid_arg "Unknown operation")
     | [ op; entity_ref ] ->
       (match tx_op_name_of_edn_form op with
        | "db/retractEntity" | "db.fn/retractEntity" ->
          RetractEntity (tx_entity_ref_of_edn_form context entity_ref)
        | _ -> invalid_arg "Unknown operation")
     | [] -> invalid_arg "empty EDN transaction vector"
     | _ :: _ -> invalid_arg "Unknown operation")
  | _ -> invalid_arg "Bad entity type at"

let tx_data_of_edn_form context form =
  match form with
  | QueryFormVector entries | QueryFormList entries ->
    entries
    |> List.filter (function QueryFormNil -> false | _ -> true)
    |> List.map (tx_op_of_edn_form context)
  | QueryFormNil -> []
  | QueryFormMap _ -> invalid_arg "Bad transaction data"
  | _ -> [ tx_op_of_edn_form context form ]

let parse_tx_data_string context input =
  tx_data_of_edn_form context (context.read_edn input)

let default_schema_attr_for_edn =
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

let cardinality_of_edn_form form =
  match keyword_name_of_form form with
  | "db.cardinality/many" -> Many
  | "db.cardinality/one" -> One
  | value -> invalid_arg ("unsupported EDN schema cardinality: " ^ value)

let unique_of_edn_form form =
  match keyword_name_of_form form with
  | "db.unique/identity" -> Identity
  | "db.unique/value" -> Value
  | value -> invalid_arg ("unsupported EDN schema unique value: " ^ value)

let value_type_of_edn_form form =
  match keyword_name_of_form form with
  | "db.type/ref" -> RefType
  | "db.type/tuple" -> TupleType
  | "db.type/string" -> StringType
  | "db.type/keyword" -> KeywordType
  | "db.type/number" -> NumberType
  | "db.type/uuid" -> UuidType
  | "db.type/instant" -> InstantType
  | value -> invalid_arg ("unsupported EDN schema value type: " ^ value)

let bool_of_edn_form = function
  | QueryFormBool value -> value
  | _ -> invalid_arg "expected EDN boolean schema value"

let string_of_edn_form = function
  | QueryFormString value -> value
  | _ -> invalid_arg "expected EDN string schema value"

let list_of_edn_forms = function
  | QueryFormVector values | QueryFormList values -> values
  | _ -> invalid_arg "expected EDN vector schema value"

let schema_attr_of_edn_form = function
  | QueryFormMap entries ->
    let spec =
      List.fold_left
        (fun spec (key, value) ->
          match attr_of_edn_key key with
          | "db/cardinality" -> { spec with cardinality = cardinality_of_edn_form value }
          | "db/unique" ->
            { spec with unique = Some (unique_of_edn_form value); indexed = true }
          | "db/index" -> { spec with indexed = bool_of_edn_form value }
          | "db/isComponent" -> { spec with is_component = bool_of_edn_form value }
          | "db/noHistory" -> { spec with no_history = bool_of_edn_form value }
          | "db/doc" -> { spec with doc = Some (string_of_edn_form value) }
          | "db/valueType" | "db/type" -> { spec with value_type = Some (value_type_of_edn_form value) }
          | "db/tupleAttrs" ->
            { spec with
              value_type = Some TupleType
            ; tuple_attrs = Some (List.map attr_of_edn_key (list_of_edn_forms value))
            ; indexed = true
            }
          | "db/tupleTypes" ->
            { spec with
              value_type = Some TupleType
            ; tuple_types = Some (List.map value_type_of_edn_form (list_of_edn_forms value))
            ; indexed = true
            }
          | "db/tupleType" | "db.install/_attribute" -> spec
          | attr -> invalid_arg ("unsupported EDN schema key: " ^ attr))
        default_schema_attr_for_edn
        entries
    in
    spec
  | _ -> invalid_arg "EDN schema attr spec must be a map"

let schema_of_edn_form context = function
  | QueryFormMap entries ->
    entries
    |> List.map (fun (attr, spec) -> attr_of_edn_key attr, schema_attr_of_edn_form spec)
    |> context.validate_schema
  | _ -> invalid_arg "EDN schema must be a map"

let schema_of_edn_string context input =
  schema_of_edn_form context (context.read_edn input)

let db_reader_field name entries =
  entries
  |> List.find_map (fun (key, value) ->
    if attr_of_edn_key key = name then Some value else None)

let raw_reader_datom_of_edn_form context = function
  | QueryFormVector [ entity_ref; attr; value ]
  | QueryFormList [ entity_ref; attr; value ] ->
    context.datom
      ~e:(entity_id_of_explicit_datom_edn_form context entity_ref)
      ~a:(attr_of_edn_key attr)
      ~v:(context.query_value_of_form value)
      ()
  | QueryFormVector [ entity_ref; attr; value; tx ]
  | QueryFormList [ entity_ref; attr; value; tx ] ->
    context.datom
      ~tx:(explicit_tx_of_edn_form tx)
      ~e:(entity_id_of_explicit_datom_edn_form context entity_ref)
      ~a:(attr_of_edn_key attr)
      ~v:(context.query_value_of_form value)
      ()
  | _ -> invalid_arg "datascript/DB datoms require [e a v] or [e a v tx]"

let db_reader_datoms_of_edn_form context schema = function
  | QueryFormVector forms | QueryFormList forms ->
    let db = context.empty_db ~schema () in
    let raw_datoms = List.map (raw_reader_datom_of_edn_form context) forms in
    let max_eid =
      List.fold_left
        (fun max_eid datom -> context.max_eid_in_value (max max_eid datom.e) datom.v)
        0
        raw_datoms
    in
    List.map
      (fun raw_datom ->
        let value, _, _ =
          context.resolve_value_for_attr db raw_datom.a raw_datoms raw_datom.tx max_eid [] raw_datom.v
        in
        { raw_datom with v = value })
      raw_datoms
  | _ -> invalid_arg "datascript/DB :datoms must be a vector or list"

let db_from_reader_form context = function
  | QueryFormTagged ("datascript/DB", QueryFormMap entries) ->
    let schema =
      match db_reader_field "schema" entries with
      | None -> []
      | Some form -> schema_of_edn_form context form
    in
    let datoms =
      match db_reader_field "datoms" entries with
      | None -> []
      | Some form -> db_reader_datoms_of_edn_form context schema form
    in
    context.init_db ~schema datoms
  | QueryFormTagged ("datascript/DB", _) ->
    invalid_arg "datascript/DB literal requires a map"
  | _ -> invalid_arg "expected datascript/DB literal"

let db_from_reader_string context input =
  db_from_reader_form context (context.read_edn input)
