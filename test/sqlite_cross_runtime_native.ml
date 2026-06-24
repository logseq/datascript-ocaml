open Datascript

module Json = Yojson.Safe

let failf fmt =
  Printf.ksprintf failwith fmt

let sql_quote text =
  "'" ^ String.concat "''" (String.split_on_char '\'' text) ^ "'"

let with_sqlite db_path f =
  let db = Sqlite3.db_open db_path in
  Fun.protect
    ~finally:(fun () ->
      if not (Sqlite3.db_close db) then failf "failed to close SQLite database: %s" db_path)
    (fun () -> f db)

let exec_sql db sql =
  let rc = Sqlite3.exec db sql in
  if not (Sqlite3.Rc.is_success rc) then
    failf "SQLite statement failed with %s for %S: %s" (Sqlite3.Rc.to_string rc) sql (Sqlite3.errmsg db)

let attr_name value =
  if String.length value > 0 && value.[0] = ':' then
    String.sub value 1 (String.length value - 1)
  else
    value

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
  | `Assoc entries -> List.map (fun (attr, spec) -> attr_name attr, schema_attr_of_json spec) entries
  | json -> failf "schema must be an object: %s" (Json.to_string json)

let rec value_of_json = function
  | `Null -> Nil
  | `Bool value -> Bool value
  | `Int value -> Int value
  | `Intlit value -> Int (int_of_string value)
  | `Float value -> Float value
  | `String value when String.length value > 0 && value.[0] = ':' -> Keyword (attr_name value)
  | `String value -> String value
  | `List values -> Vector (List.map value_of_json values)
  | `Assoc entries -> Map (List.map (fun (key, value) -> String key, value_of_json value) entries)
  | json -> failf "unsupported value: %s" (Json.to_string json)
[@@warning "-11"]

let entity_ref_of_json = function
  | `Int entity_id -> Entity_id entity_id
  | `Intlit value -> Entity_id (int_of_string value)
  | json -> failf "unsupported entity ref: %s" (Json.to_string json)

let tx_op_of_json = function
  | `List (`String op :: e :: `String a :: rest) ->
    (match attr_name op, rest with
     | "db/add", [ value ] -> Add (entity_ref_of_json e, attr_name a, value_of_json value)
     | "db/retract", [ value ] -> Retract (entity_ref_of_json e, attr_name a, Some (value_of_json value))
     | "db/retract", [] -> Retract (entity_ref_of_json e, attr_name a, None)
     | _ -> failf "unsupported tx op: %s" (Json.to_string (`List (`String op :: e :: `String a :: rest))))
  | `List [ `String op; e ] when attr_name op = "db.fn/retractEntity" -> RetractEntity (entity_ref_of_json e)
  | json -> failf "unsupported tx op: %s" (Json.to_string json)

let tx_batch_of_json = function
  | `List values -> List.map tx_op_of_json values
  | json -> failf "transaction batch must be an array: %s" (Json.to_string json)

let string_of_value = function
  | Nil -> "nil"
  | Int value -> "int:" ^ string_of_int value
  | Float value -> "float:" ^ string_of_float value
  | String value -> "string:" ^ value
  | Symbol value -> "symbol:" ^ value
  | Bool value -> "bool:" ^ string_of_bool value
  | Keyword value -> "keyword:" ^ value
  | Uuid value -> "uuid:" ^ value
  | Instant value -> "instant:" ^ string_of_int value
  | Regex value -> "regex:" ^ value
  | Ref entity_id -> "ref:" ^ string_of_int entity_id
  | List _ | Vector _ | Set _ | Map _ | Tuple _ -> "compound"
  | TxRef -> "tx-ref"
  | Ref_to _ -> "ref-to"

let canonical_datom_line datom =
  Printf.sprintf "datom\t%d\t%s\t%s\t%d\t%b" datom.e datom.a (string_of_value datom.v) datom.tx datom.added

let write_lines path lines =
  let channel = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () ->
      List.iter
        (fun line ->
          output_string channel line;
          output_char channel '\n')
        lines)

let write_sqlite db_path lines =
  with_sqlite db_path (fun db ->
    exec_sql db "pragma journal_mode = delete;";
    exec_sql db "pragma page_size = 4096;";
    exec_sql db "vacuum;";
    exec_sql db "create table kvs (addr INTEGER primary key, content TEXT, addresses JSON);";
    exec_sql db "begin immediate;";
    exec_sql db "insert into kvs (addr, content, addresses) values (0, 'datascript-sqlite-parity-v1', '[]');";
    List.iteri
      (fun index line ->
        exec_sql
          db
          (Printf.sprintf
             "insert into kvs (addr, content, addresses) values (%d, %s, '[]');"
             (index + 1)
             (sql_quote line)))
      lines;
    exec_sql db "commit;";
    exec_sql db "vacuum;")

let input = ref None
let sqlite = ref None
let datoms_out = ref None

let speclist =
  [ "--input", Arg.String (fun value -> input := Some value), "Path to generated parity input JSON"
  ; "--sqlite", Arg.String (fun value -> sqlite := Some value), "Path to output SQLite database"
  ; "--datoms", Arg.String (fun value -> datoms_out := Some value), "Path to output canonical datoms"
  ]

let required name = function
  | Some value -> value
  | None -> failf "missing required argument: %s" name

let () =
  Arg.parse speclist (fun value -> failf "unexpected argument: %s" value) "sqlite_cross_runtime_native";
  let input_path = required "--input" !input in
  let sqlite_path = required "--sqlite" !sqlite in
  let datoms_path = required "--datoms" !datoms_out in
  let json = Json.from_file input_path in
  let schema, batches =
    match json with
    | `Assoc entries ->
      let schema =
        match assoc_opt "schema" entries with
        | Some schema -> schema_of_json schema
        | None -> failwith "input is missing schema"
      in
      let batches =
        match assoc_opt "batches" entries with
        | Some (`List batches) -> List.map tx_batch_of_json batches
        | Some json -> failf "input batches must be an array: %s" (Json.to_string json)
        | None -> failwith "input is missing batches"
      in
      schema, batches
    | _ -> failwith "input must be an object"
  in
  let conn = create_conn ~schema () in
  List.iter (fun batch -> ignore (transact_conn conn batch)) batches;
  let lines =
    datoms (conn_db conn) Eavt ()
    |> List.of_seq
    |> List.map canonical_datom_line
    |> List.sort String.compare
  in
  write_lines datoms_path lines;
  write_sqlite sqlite_path lines
