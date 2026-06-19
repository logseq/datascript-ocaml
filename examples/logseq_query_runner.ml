open Datascript

module Storage = Logseq_sqlite_storage

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

let json_field key value = json_string key ^ ":" ^ value
let json_obj fields = "{" ^ String.concat "," (List.map (fun (key, value) -> json_field key value) fields) ^ "}"

let exception_message = function
  | Invalid_argument message | Failure message -> message
  | exn -> Printexc.to_string exn

let edn_keyword value = ":" ^ value

let rec edn_value = function
  | Nil -> "nil"
  | Int value -> string_of_int value
  | Float value -> string_of_float value
  | String value -> Built_ins.print_query_value ~readably:true (String value)
  | Symbol value -> value
  | Bool true -> "true"
  | Bool false -> "false"
  | Keyword value -> edn_keyword value
  | Uuid value -> "#uuid " ^ json_string value
  | Instant value -> string_of_int value
  | Regex value -> "#\"" ^ String.escaped value ^ "\""
  | Ref value -> string_of_int value
  | List values -> "(" ^ String.concat " " (List.map edn_value values) ^ ")"
  | Vector values -> "[" ^ String.concat " " (List.map edn_value values) ^ "]"
  | Set values -> "#{" ^ String.concat " " (List.map edn_value values) ^ "}"
  | Tuple values ->
    "[" ^ String.concat " " (List.map (function Some value -> edn_value value | None -> "nil") values) ^ "]"
  | Map entries ->
    entries
    |> List.map (fun (key, value) -> edn_value key ^ " " ^ edn_value value)
    |> String.concat " "
    |> fun body -> "{" ^ body ^ "}"
  | TxRef -> ":db/current-tx"
  | Ref_to _ -> "#datascript-ocaml/ref-to"

let edn_schema_attr attr =
  let props =
    [ Some
        ( ":db/cardinality"
        , (match attr.cardinality with
           | One -> ":db.cardinality/one"
           | Many -> ":db.cardinality/many") )
    ; (match attr.unique with
       | None -> None
       | Some Identity -> Some (":db/unique", ":db.unique/identity")
       | Some Value -> Some (":db/unique", ":db.unique/value"))
    ; (if attr.indexed then Some (":db/index", "true") else None)
    ; (if attr.is_component then Some (":db/isComponent", "true") else None)
    ; (if attr.no_history then Some (":db/noHistory", "true") else None)
    ; (match attr.value_type with
       | None -> None
       | Some RefType -> Some (":db/valueType", ":db.type/ref")
       | Some TupleType -> Some (":db/valueType", ":db.type/tuple")
       | Some StringType -> Some (":db/valueType", ":db.type/string")
       | Some KeywordType -> Some (":db/valueType", ":db.type/keyword")
       | Some NumberType -> Some (":db/valueType", ":db.type/number")
       | Some UuidType -> Some (":db/valueType", ":db.type/uuid")
       | Some InstantType -> Some (":db/valueType", ":db.type/instant"))
    ]
    |> List.filter_map Fun.id
    |> List.map (fun (key, value) -> key ^ " " ^ value)
  in
  "{" ^ String.concat " " props ^ "}"

let edn_schema_entry (attr, spec) =
  "[" ^ edn_keyword attr ^ " " ^ edn_schema_attr spec ^ "]"

let edn_datom datom =
  Printf.sprintf
    "[%d %s %s %d %b]"
    datom.e
    (edn_keyword datom.a)
    (edn_value datom.v)
    datom.tx
    datom.added

let graph_edn schema datoms =
  "{:schema ["
  ^ String.concat "\n" (List.map edn_schema_entry schema)
  ^ "]\n:datoms ["
  ^ String.concat "\n" (List.map edn_datom datoms)
  ^ "]}\n"

let load_graph_data db_path =
  let schema = Storage.schema_of_logseq_graph ~read_only:true db_path in
  let datoms = Storage.datoms_of_logseq_graph ~read_only:true db_path in
  schema, datoms

let rec edn_pulled_value = function
  | Pulled_scalar value -> edn_value value
  | Pulled_many values -> "[" ^ String.concat " " (List.map edn_pulled_value values) ^ "]"
  | Pulled_entity entity -> edn_pulled_entity entity

and edn_pulled_entity entity =
  let attrs =
    entity.pulled_attrs
    |> List.sort (fun (left, _) (right, _) -> compare left right)
    |> List.map (fun (key, value) -> edn_value key ^ " " ^ edn_pulled_value value)
  in
  "{" ^ String.concat " " attrs ^ "}"

let edn_query_result = function
  | Result_entity entity_id -> string_of_int entity_id
  | Result_attr attr -> edn_keyword attr
  | Result_value value -> edn_value value
  | Result_db _ -> "#datascript/DB"
  | Result_pull entity -> edn_pulled_entity entity

let edn_list values = "[" ^ String.concat " " values ^ "]"
let edn_result_row row = edn_list (List.map edn_query_result row)

let edn_query_output = function
  | Query_relation rows -> edn_list (List.map edn_result_row rows)
  | Query_collection values -> edn_list (List.map edn_query_result values)
  | Query_tuple None -> "nil"
  | Query_tuple (Some row) -> edn_result_row row
  | Query_scalar None -> "nil"
  | Query_scalar (Some value) -> edn_query_result value
  | Query_relation_maps rows ->
    rows
    |> List.map (fun row ->
      row
      |> List.map (fun (key, value) -> edn_value key ^ " " ^ edn_query_result value)
      |> String.concat " "
      |> fun body -> "{" ^ body ^ "}")
    |> edn_list
  | Query_tuple_map None -> "nil"
  | Query_tuple_map (Some row) ->
    row
    |> List.map (fun (key, value) -> edn_value key ^ " " ^ edn_query_result value)
    |> String.concat " "
    |> fun body -> "{" ^ body ^ "}"

let json_member key = function
  | `Assoc fields ->
    (match List.assoc_opt key fields with
     | Some (`String value) -> value
     | _ -> invalid_arg ("query input field must be a string: " ^ key))
  | _ -> invalid_arg "query input line must be a JSON object"

let run_query db_path id query =
  try
    let value = Storage.query_logseq_graph ~read_only:true db_path query |> edn_query_output in
    print_endline
      (json_obj [ "id", json_string id; "status", json_string "ok"; "value", json_string value ])
  with
  | exn ->
    print_endline
      (json_obj
         [ "id", json_string id
         ; "status", json_string "error"
         ; "message", json_string (exception_message exn)
         ])

let run_queries db_path queries_path =
  let channel = open_in queries_path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () ->
      try
        while true do
          let line = input_line channel in
          if String.trim line <> "" then
            let json = Yojson.Safe.from_string line in
            run_query db_path (json_member "id" json) (json_member "query" json)
        done
      with
      | End_of_file -> ())

let dump_graph db_path out_path =
  let schema, datoms = load_graph_data db_path in
  let channel = open_out out_path in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel (graph_edn schema datoms))

let dump_query_graph db_path query out_path =
  let _, _, _, parsed_query = Storage.parse_logseq_query_with_schema ~read_only:true db_path query in
  let attrs = Storage.query_attrs parsed_query in
  let schema = Storage.schema_of_logseq_graph ~read_only:true db_path in
  let datoms = Storage.datoms_of_logseq_graph_for_attrs ~read_only:true db_path attrs in
  let channel = open_out out_path in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel (graph_edn schema datoms))

let usage () =
  prerr_endline "Usage:";
  prerr_endline "  logseq_query_runner dump-graph <db.sqlite> <out.edn>";
  prerr_endline "  logseq_query_runner dump-query-graph <db.sqlite> <query> <out.edn>";
  prerr_endline "  logseq_query_runner run <db.sqlite> <queries.jsonl>";
  exit 2

let () =
  match Array.to_list Sys.argv with
  | [ _; "dump-graph"; db_path; out_path ] -> dump_graph db_path out_path
  | [ _; "dump-query-graph"; db_path; query; out_path ] -> dump_query_graph db_path query out_path
  | [ _; "run"; db_path; queries_path ] -> run_queries db_path queries_path
  | _ -> usage ()
