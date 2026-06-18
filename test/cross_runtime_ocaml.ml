open Datascript

let json_string value =
  let buffer = Buffer.create (String.length value + 8) in
  Buffer.add_char buffer '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\b' -> Buffer.add_string buffer "\\b"
      | '\012' -> Buffer.add_string buffer "\\f"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | ch ->
        let code = Char.code ch in
        if code < 0x20 then Buffer.add_string buffer (Printf.sprintf "\\u%04x" code)
        else Buffer.add_char buffer ch)
    value;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let json_bool = function
  | true -> "true"
  | false -> "false"

let json_null = "null"
let json_int = string_of_int
let json_list values = "[" ^ String.concat "," values ^ "]"
let json_field key value = json_string key ^ ":" ^ value
let json_obj fields = "{" ^ String.concat "," (List.map (fun (key, value) -> json_field key value) fields) ^ "}"

let emit name value =
  print_endline (name ^ "\t" ^ value)

let schema_attr_json attr =
  let cardinality =
    match attr.cardinality with
    | One -> "one"
    | Many -> "many"
  in
  let unique =
    match attr.unique with
    | None -> json_null
    | Some Identity -> json_string "identity"
    | Some Value -> json_string "value"
  in
  let value_type =
    match attr.value_type with
    | Some RefType -> json_string "ref"
    | _ -> json_null
  in
  json_obj
    [ "cardinality", json_string cardinality
    ; "indexed", json_bool attr.indexed
    ; "unique", unique
    ; "value_type", value_type
    ]

let schema_json db =
  db
  |> schema
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  |> List.map (fun (attr, spec) -> json_list [ json_string attr; schema_attr_json spec ])
  |> json_list

let rec value_json = function
  | Nil -> json_null
  | Int value -> json_int value
  | Float value -> string_of_float value
  | String value -> json_string value
  | Symbol value -> json_string value
  | Bool value -> json_bool value
  | Keyword value -> json_string (":" ^ value)
  | Uuid value -> json_string value
  | Instant value -> json_int value
  | Regex value -> json_string value
  | Ref value -> json_int value
  | List values | Vector values | Set values -> json_list (List.map value_json values)
  | Map entries ->
    entries
    |> List.map (fun (key, value) -> json_field (value_key key) (value_json value))
    |> String.concat ","
    |> fun fields -> "{" ^ fields ^ "}"
  | Tuple values ->
    values
    |> List.map (function Some value -> value_json value | None -> json_null)
    |> json_list
  | TxRef -> json_string ":db/current-tx"
  | Ref_to _ -> json_string "#ref"

and value_key = function
  | String value -> value
  | Keyword value -> value
  | Symbol value -> value
  | value -> value_json value

let datom_json datom =
  json_list
    [ json_int datom.e
    ; json_string datom.a
    ; value_json datom.v
    ; json_int datom.tx
    ; json_bool datom.added
    ]

let datoms_json datoms =
  datoms |> List.map datom_json |> json_list

let rec result_json = function
  | Result_entity entity_id -> json_int entity_id
  | Result_attr attr -> json_string attr
  | Result_value value -> value_json value
  | Result_pull entity -> pulled_entity_json entity
  | Result_db _ -> json_string "#db"

and pulled_entity_json entity =
  entity.pulled_attrs
  |> List.map (fun (key, value) -> value_key key, pulled_value_json value)
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  |> json_obj

and pulled_value_json = function
  | Pulled_scalar value -> value_json value
  | Pulled_many values -> json_list (List.map pulled_value_json values)
  | Pulled_entity entity -> pulled_entity_json entity

let query_rows_json rows =
  rows
  |> List.map (fun row -> json_list (List.map result_json row))
  |> List.sort String.compare
  |> json_list

let tempids_json tempids =
  tempids
  |> List.map (fun (tempid, entity_id) -> (if String.length tempid > 0 && tempid.[0] = ':' then String.sub tempid 1 (String.length tempid - 1) else tempid), entity_id)
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  |> List.map (fun (tempid, entity_id) -> tempid, json_int entity_id)
  |> json_obj

let normalize_error f =
  match f () with
  | () -> json_obj [ "outcome", json_string "ok" ]
  | exception Invalid_argument message ->
    let category =
      if String.contains message 'u' && String.ends_with ~suffix:"constraint" message
      then "unique constraint"
      else message
    in
    json_obj [ "category", json_string category; "outcome", json_string "error" ]
  | exception exn ->
    json_obj [ "category", json_string (Printexc.to_string exn); "outcome", json_string "error" ]

let one =
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

let indexed = { one with indexed = true }
let unique_identity = { indexed with unique = Some Identity }
let unique_value = { indexed with unique = Some Value }
let ref_attr = { one with value_type = Some RefType }
let many = { one with cardinality = Many }

let () =
  let schema = [ "name", unique_identity; "age", indexed; "friend", ref_attr; "aka", many ] in
  let conn = create_conn ~schema () in
  let first_report =
    transact_conn
      conn
      [ Entity
          { db_id = Some (Temp_id "-1")
          ; attrs =
              [ "name", One_value (String "Ivan")
              ; "age", One_value (Int 31)
              ; "aka", Many_values [ String "Vanya"; String "I" ]
              ; "friend", One_value (Ref_to (Temp_id "-2"))
              ]
          }
      ; Entity
          { db_id = Some (Temp_id "-2")
          ; attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 44) ]
          }
      ]
  in
  let first_db = first_report.db_after in
  emit "schema" (schema_json first_db);
  emit "tx.first.tempids" (tempids_json first_report.tempids);
  emit "tx.first.datoms" (datoms_json first_report.tx_data);
  emit "datoms.eavt.after_first" (datoms_json (datoms first_db Eavt ()));
  emit "query.names_ages" (query_rows_json (q_string first_db "[:find ?n ?a :where [?e :name ?n] [?e :age ?a]]"));
  (match pull first_db [ Pull_attr "name"; Pull_ref ("friend", [ Pull_attr "name" ]) ] (Entity_id 1) with
   | Some entity -> emit "pull.friend" (pulled_entity_json entity)
   | None -> emit "pull.friend" json_null);
  let second_report =
    transact_conn
      conn
      [ Add (Entity_id 1, "age", Int 32); Retract (Entity_id 1, "aka", Some (String "I")) ]
  in
  let second_db = second_report.db_after in
  emit "tx.second.datoms" (datoms_json second_report.tx_data);
  emit "datoms.eavt.after_second" (datoms_json (datoms second_db Eavt ()));
  emit
    "error.unique_value"
    (normalize_error (fun () ->
       let error_conn = create_conn ~schema:[ "email", unique_value ] () in
       ignore
         (transact_conn
            error_conn
            [ Entity { db_id = Some (Entity_id 1); attrs = [ "email", One_value (String "a@example.test") ] }
            ; Entity { db_id = Some (Entity_id 2); attrs = [ "email", One_value (String "a@example.test") ] }
            ])))
