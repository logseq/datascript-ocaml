open Js_of_ocaml
open Datascript

module Json = Yojson.Safe

let js_type value =
  Js.to_string (Js.typeof value)

let is_undefined value =
  js_type value = "undefined"

let json_of_js value =
  if is_undefined value then `Null
  else
    Js.Unsafe.meth_call Js._JSON "stringify" [| Js.Unsafe.inject value |]
    |> Js.to_string
    |> Json.from_string

let js_of_json json =
  Js.Unsafe.meth_call Js._JSON "parse" [| Js.Unsafe.inject (Js.string (Json.to_string json)) |]

let strip_prefix prefix value =
  let prefix_length = String.length prefix in
  if String.length value >= prefix_length && String.sub value 0 prefix_length = prefix then
    String.sub value prefix_length (String.length value - prefix_length)
  else
    value

let attr_name value =
  strip_prefix ":" value

let schema_key name =
  attr_name name

let assoc_opt key entries =
  List.assoc_opt key entries

let assoc_any keys entries =
  List.find_map (fun key -> assoc_opt key entries) keys

let string_value = function
  | `String value -> Some value
  | _ -> None

let bool_value = function
  | `Bool value -> Some value
  | _ -> None

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

let schema_attr_of_json = function
  | `Assoc entries ->
    let cardinality =
      match Option.bind (assoc_any [ ":db/cardinality"; "db/cardinality" ] entries) string_value with
      | Some value when attr_name value = "db.cardinality/many" -> Many
      | Some _ | None -> One
    in
    let unique =
      match Option.bind (assoc_any [ ":db/unique"; "db/unique" ] entries) string_value with
      | Some value when attr_name value = "db.unique/identity" -> Some Identity
      | Some value when attr_name value = "db.unique/value" -> Some Value
      | Some _ | None -> None
    in
    let value_type =
      match Option.bind (assoc_any [ ":db/valueType"; "db/valueType"; ":db/type"; "db/type" ] entries) string_value with
      | Some value when attr_name value = "db.type/ref" -> Some RefType
      | Some value when attr_name value = "db.type/string" -> Some StringType
      | Some value when attr_name value = "db.type/keyword" -> Some KeywordType
      | Some value when attr_name value = "db.type/number" -> Some NumberType
      | Some value when attr_name value = "db.type/uuid" -> Some UuidType
      | Some value when attr_name value = "db.type/instant" -> Some InstantType
      | Some _ | None -> None
    in
    let indexed =
      Option.value ~default:false (Option.bind (assoc_any [ ":db/index"; "db/index" ] entries) bool_value)
      || Option.is_some unique
    in
    let is_component =
      Option.value ~default:false (Option.bind (assoc_any [ ":db/isComponent"; "db/isComponent" ] entries) bool_value)
    in
    let no_history =
      Option.value ~default:false (Option.bind (assoc_any [ ":db/noHistory"; "db/noHistory" ] entries) bool_value)
    in
    { default_schema_attr with cardinality; unique; indexed; is_component; no_history; value_type }
  | _ -> default_schema_attr

let schema_of_json = function
  | `Assoc entries -> List.map (fun (attr, spec) -> schema_key attr, schema_attr_of_json spec) entries
  | `Null -> []
  | _ -> invalid_arg "schema must be a JavaScript object"

let rec value_of_json = function
  | `Null -> Nil
  | `Bool value -> Bool value
  | `Int value -> Int value
  | `Intlit value -> Int (int_of_string value)
  | `Float value -> Float value
  | `String value when String.starts_with ~prefix:":" value -> Keyword (attr_name value)
  | `String value -> String value
  | `List values -> Vector (List.map value_of_json values)
  | `Assoc entries ->
    Map (List.map (fun (key, value) -> String key, value_of_json value) entries)
  | `Tuple values -> Vector (List.map value_of_json values)
  | `Variant _ -> invalid_arg "unsupported JavaScript value"

let schema_attr_for_db db attr =
  Option.bind db (fun db ->
    match List.assoc_opt attr (schema db) with
    | Some attr_schema -> Some attr_schema
    | None -> List.assoc_opt (":" ^ attr) (schema db))

let schema_attr_is_many db attr =
  match schema_attr_for_db db attr with
  | Some { cardinality = Many; _ } -> true
  | Some _ | None -> false

let schema_attr_is_ref db attr =
  match schema_attr_for_db db attr with
  | Some { value_type = Some RefType; _ } -> true
  | Some _ | None -> false

let value_of_json_for_attr ?db attr json =
  match value_of_json json with
  | Int entity_id when schema_attr_is_ref db attr -> Ref entity_id
  | value -> value

let tx_value_of_json_for_attr ?db attr = function
  | `List values when schema_attr_is_many db attr ->
    Many_values (List.map (value_of_json_for_attr ?db attr) values)
  | json -> One_value (value_of_json_for_attr ?db attr json)

let entity_ref_of_json = function
  | `Int entity_id when entity_id < 0 -> Temp_id (string_of_int entity_id)
  | `Int entity_id -> Entity_id entity_id
  | `Intlit value ->
    let entity_id = int_of_string value in
    if entity_id < 0 then Temp_id value else Entity_id entity_id
  | `String "db/current-tx" | `String ":db/current-tx" -> CurrentTx
  | `String value when String.starts_with ~prefix:":" value -> Ident (attr_name value)
  | `String value -> Temp_id value
  | `List [ `String attr; value ] -> Lookup_ref (attr_name attr, value_of_json value)
  | json -> invalid_arg ("unsupported entity reference: " ^ Json.to_string json)

let datom_of_json = function
  | `List [ e; `String a; v ] ->
    datom ~e:(match entity_ref_of_json e with Entity_id entity_id -> entity_id | _ -> invalid_arg "datom entity must be an id") ~a:(attr_name a) ~v:(value_of_json v) ()
  | `List [ e; `String a; v; `Int tx ] ->
    datom ~tx ~e:(match entity_ref_of_json e with Entity_id entity_id -> entity_id | _ -> invalid_arg "datom entity must be an id") ~a:(attr_name a) ~v:(value_of_json v) ()
  | `List [ e; `String a; v; `Int tx; `Bool added ] ->
    datom ~tx ~added ~e:(match entity_ref_of_json e with Entity_id entity_id -> entity_id | _ -> invalid_arg "datom entity must be an id") ~a:(attr_name a) ~v:(value_of_json v) ()
  | `Assoc entries ->
    let int_field name =
      match assoc_opt name entries with
      | Some (`Int value) -> value
      | _ -> invalid_arg ("datom requires numeric field " ^ name)
    in
    let string_field name =
      match assoc_opt name entries with
      | Some (`String value) -> value
      | _ -> invalid_arg ("datom requires string field " ^ name)
    in
    let tx = Option.value ~default:tx0 (Option.bind (assoc_opt "tx" entries) (function `Int value -> Some value | _ -> None)) in
    let added = Option.value ~default:true (Option.bind (assoc_opt "added" entries) bool_value) in
    datom ~tx ~added ~e:(int_field "e") ~a:(attr_name (string_field "a")) ~v:(value_of_json (Option.get (assoc_opt "v" entries))) ()
  | json -> invalid_arg ("unsupported datom: " ^ Json.to_string json)

let tx_entity_of_assoc ?db entries =
  let db_id = Option.map entity_ref_of_json (assoc_any [ ":db/id"; "db/id" ] entries) in
  let attrs =
    entries
    |> List.filter_map (fun (key, value) ->
      match key with
      | ":db/id" | "db/id" -> None
      | key ->
        let attr = attr_name key in
        Some (attr, tx_value_of_json_for_attr ?db attr value))
  in
  Entity { db_id; attrs }

let tx_op_of_json ?db = function
  | `Assoc entries -> tx_entity_of_assoc ?db entries
  | `List (`String op :: e :: `String a :: rest) ->
    let attr = attr_name a in
    (match attr_name op, rest with
     | "db/add", [ v ] -> Add (entity_ref_of_json e, attr, value_of_json_for_attr ?db attr v)
     | "db/retract", [ v ] -> Retract (entity_ref_of_json e, attr, Some (value_of_json_for_attr ?db attr v))
     | "db/retract", [] -> Retract (entity_ref_of_json e, attr, None)
     | _ -> invalid_arg ("unsupported transaction op: " ^ attr_name op))
  | `List [ `String op; e ] when attr_name op = "db.fn/retractEntity" ->
    RetractEntity (entity_ref_of_json e)
  | json -> invalid_arg ("unsupported transaction data: " ^ Json.to_string json)

let tx_ops_of_json ?db = function
  | `List values -> List.map (tx_op_of_json ?db) values
  | json -> invalid_arg ("transaction data must be a JavaScript array: " ^ Json.to_string json)

let datoms_of_json = function
  | `List values -> List.map datom_of_json values
  | json -> invalid_arg ("datoms must be a JavaScript array: " ^ Json.to_string json)

let rec json_of_value = function
  | Nil -> `Null
  | Int value -> `Int value
  | Float value -> `Float value
  | String value -> `String value
  | Symbol value -> `String value
  | Bool value -> `Bool value
  | Keyword value -> `String (":" ^ value)
  | Uuid value -> `String value
  | Instant value -> `Int value
  | Regex value -> `String value
  | Ref entity_id -> `Int entity_id
  | List values | Vector values | Set values -> `List (List.map json_of_value values)
  | Tuple values -> `List (List.map (function None -> `Null | Some value -> json_of_value value) values)
  | Map entries -> `Assoc (List.map (fun (key, value) -> Datascript.Built_ins.print_query_value ~readably:false key, json_of_value value) entries)
  | TxRef -> `String ":db/current-tx"
  | Ref_to _ -> `Null

let rec json_of_pulled_entity entity =
  `Assoc
    (List.map
       (fun (key, value) ->
         pull_key_string key, json_of_pulled_value value)
       entity.pulled_attrs)

and pull_key_string = function
  | Keyword attr | String attr -> attr_name attr
  | value -> Datascript.Built_ins.print_query_value ~readably:false value

and json_of_pulled_value = function
  | Pulled_scalar value -> json_of_value value
  | Pulled_many values -> `List (List.map json_of_pulled_value values)
  | Pulled_entity entity -> json_of_pulled_entity entity

let json_of_query_result = function
  | Result_entity entity_id -> `Int entity_id
  | Result_attr attr -> `String (":" ^ attr)
  | Result_value value -> json_of_value value
  | Result_db _ -> `String "<db>"
  | Result_pull entity -> json_of_pulled_entity entity

let json_of_datom d =
  `Assoc
    [ "e", `Int d.e
    ; "a", `String (":" ^ d.a)
    ; "v", json_of_value d.v
    ; "tx", `Int d.tx
    ; "added", `Bool d.added
    ]

let index_of_string value =
  match attr_name value with
  | "eavt" -> Eavt
  | "aevt" -> Aevt
  | "avet" -> Avet
  | other -> invalid_arg ("unknown index: " ^ other)

let value_option_of_js value =
  if is_undefined value then None else Some (value_of_json (json_of_js value))

let entity_ref_of_js value =
  entity_ref_of_json (json_of_js value)

let tempids_object tempids =
  tempids
  |> List.map (fun (tempid, entity_id) -> tempid, `Int entity_id)
  |> fun entries -> `Assoc entries

let squuid_time_millis_json uuid =
  if String.length uuid < 8 then invalid_arg "invalid squuid";
  let seconds = Int32.to_float (Int32.of_string ("0x" ^ String.sub uuid 0 8)) in
  `Float (seconds *. 1000.0)

let json_of_tx_report report =
  `Assoc
    [ "db_before", `String "<db>"
    ; "db_after", `String "<db>"
    ; "tx_data", `List (List.map json_of_datom report.tx_data)
    ; "tempids", tempids_object report.tempids
    ; "tx_meta", `List (List.map (fun (key, value) -> `List [ `String key; json_of_value value ]) report.tx_meta)
    ]

let tx_report_object report =
  let obj = js_of_json (json_of_tx_report report) in
  Js.Unsafe.set obj "db_before" (Js.Unsafe.inject report.db_before);
  Js.Unsafe.set obj "db_after" (Js.Unsafe.inject report.db_after);
  obj

let conn_db_object conn =
  Js.Unsafe.inject (db conn)

let () =
  Js.export_all
    (Js.Unsafe.obj
       [| ( "empty_db"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun schema ->
                 Js.Unsafe.inject (empty_db ~schema:(schema_of_json (json_of_js schema)) ()))) )
        ; ( "init_db"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun datoms schema ->
                 Js.Unsafe.inject (init_db ~schema:(schema_of_json (json_of_js schema)) (datoms_of_json (json_of_js datoms))))) )
        ; ( "q"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun query db ->
                 q_string db (Js.to_string query)
                 |> List.map (fun row -> `List (List.map json_of_query_result row))
                 |> fun rows -> js_of_json (`List rows))) )
        ; ( "pull"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun db pattern eid ->
                 match pull_string db (Js.to_string pattern) (entity_ref_of_js eid) with
                 | None -> Js.Unsafe.inject Js.null
                 | Some entity -> js_of_json (json_of_pulled_entity entity))) )
        ; ( "pull_many"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun db pattern eids ->
                 let eids =
                   match json_of_js eids with
                   | `List values -> List.map entity_ref_of_json values
                   | _ -> invalid_arg "pull_many expects an array of entity ids"
                 in
                 pull_many_string db (Js.to_string pattern) eids
                 |> List.map (function None -> `Null | Some entity -> json_of_pulled_entity entity)
                 |> fun values -> js_of_json (`List values))) )
        ; ( "db_with"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun db entities ->
                 Js.Unsafe.inject (db_with (tx_ops_of_json ~db (json_of_js entities)) db))) )
        ; ( "create_conn"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun schema ->
                 Js.Unsafe.inject (create_conn ~schema:(schema_of_json (json_of_js schema)) ()))) )
        ; "conn_from_db", Js.Unsafe.inject (Js.wrap_callback (fun db -> Js.Unsafe.inject (conn_from_db db)))
        ; ( "conn_from_datoms"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun datoms schema ->
                 Js.Unsafe.inject (conn_from_datoms ~schema:(schema_of_json (json_of_js schema)) (datoms_of_json (json_of_js datoms))))) )
        ; "db", Js.Unsafe.inject (Js.wrap_callback conn_db_object)
        ; ( "transact"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun conn entities ->
                 let db_before = db conn in
                 let report = transact_conn conn (tx_ops_of_json ~db:db_before (json_of_js entities)) in
                 tx_report_object report)) )
        ; ( "reset_conn"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun conn db ->
                 Js.Unsafe.inject (reset_conn conn db))) )
        ; ( "resolve_tempid"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun tempids tempid ->
                 let key =
                   match json_of_js tempid with
                   | `Int value -> string_of_int value
                   | `String value -> value
                   | _ -> invalid_arg "tempid must be a string or number"
                 in
                 Js.Unsafe.get tempids key)) )
        ; ( "datoms"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun db index ->
                 datoms db (index_of_string (Js.to_string index)) ()
                 |> List.of_seq
                 |> List.map json_of_datom
                 |> fun values -> js_of_json (`List values))) )
        ; ( "seek_datoms"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun db index ->
                 seek_datoms db (index_of_string (Js.to_string index)) ()
                 |> List.of_seq
                 |> List.map json_of_datom
                 |> fun values -> js_of_json (`List values))) )
        ; ( "index_range"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun db attr start stop ->
                 index_range
                   db
                   (attr_name (Js.to_string attr))
                   ?start:(value_option_of_js start)
                   ?stop:(value_option_of_js stop)
                   ()
                 |> List.of_seq
                 |> List.map json_of_datom
                 |> fun values -> js_of_json (`List values))) )
        ; "squuid", Js.Unsafe.inject (Js.wrap_callback (fun () -> js_of_json (json_of_value (squuid ()))))
        ; ( "squuid_time_millis"
          , Js.Unsafe.inject
              (Js.wrap_callback (fun uuid ->
                 js_of_json (squuid_time_millis_json (Js.to_string uuid)))) )
       |])
