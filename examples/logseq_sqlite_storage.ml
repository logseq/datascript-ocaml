open Datascript

type content_format =
  | Ocaml_marshal
  | Logseq_transit
  | Empty
  | Unknown

type summary =
  { has_kvs_table : bool
  ; row_count : int
  ; has_root : bool
  ; has_tail : bool
  ; root_content_format : content_format
  ; root_keys : string list
  ; root_index_addresses : int list
  }

let kvs_schema =
  "create table if not exists kvs (addr INTEGER primary key, content TEXT, addresses JSON)"

let ocaml_payload_prefix = "ocaml-marshal-hex:"

let uri_hex_digit value =
  Char.chr (if value < 10 then Char.code '0' + value else Char.code 'A' + value - 10)

let uri_escape_path path =
  let buffer = Buffer.create (String.length path) in
  String.iter
    (fun ch ->
      match ch with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '/' | '-' | '_' | '.' | '~' ->
        Buffer.add_char buffer ch
      | ch ->
        let code = Char.code ch in
        Buffer.add_char buffer '%';
        Buffer.add_char buffer (uri_hex_digit (code lsr 4));
        Buffer.add_char buffer (uri_hex_digit (code land 0x0f)))
    path;
  Buffer.contents buffer

let readonly_uri db_path =
  "file:" ^ uri_escape_path db_path ^ "?mode=ro&immutable=1"

let with_db ?(read_only = false) db_path f =
  let db =
    if read_only then Sqlite3.db_open ~uri:true (readonly_uri db_path) else Sqlite3.db_open db_path
  in
  Fun.protect
    ~finally:(fun () ->
      if not (Sqlite3.db_close db) then invalid_arg ("failed to close SQLite database: " ^ db_path))
    (fun () -> f db)

let check_sql db sql rc =
  if not (Sqlite3.Rc.is_success rc) then
    invalid_arg
      (Printf.sprintf
         "SQLite statement failed with %s while reading %s: %s"
         (Sqlite3.Rc.to_string rc)
         sql
         (Sqlite3.errmsg db))

let exec_sql ?(read_only = false) db_path sql =
  with_db ~read_only db_path (fun db -> check_sql db sql (Sqlite3.exec db sql))

let select_map ?(read_only = false) db_path sql f =
  with_db ~read_only db_path (fun db ->
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> check_sql db sql (Sqlite3.finalize stmt))
      (fun () ->
        let rec loop acc =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW -> loop (f stmt :: acc)
          | Sqlite3.Rc.DONE -> List.rev acc
          | rc ->
            check_sql db sql rc;
            List.rev acc
        in
        loop []))

let sql_quote value =
  "'" ^ String.concat "''" (String.split_on_char '\'' value) ^ "'"

let hex_digit value =
  Char.chr (if value < 10 then Char.code '0' + value else Char.code 'a' + value - 10)

let hex_value = function
  | '0' .. '9' as ch -> Char.code ch - Char.code '0'
  | 'a' .. 'f' as ch -> Char.code ch - Char.code 'a' + 10
  | 'A' .. 'F' as ch -> Char.code ch - Char.code 'A' + 10
  | ch -> invalid_arg ("invalid hex digit: " ^ String.make 1 ch)

let hex_encode bytes =
  String.init
    (String.length bytes * 2)
    (fun index ->
      let code = Char.code bytes.[index / 2] in
      if index mod 2 = 0 then hex_digit (code lsr 4) else hex_digit (code land 0x0f))

let hex_decode encoded =
  if String.length encoded mod 2 <> 0 then invalid_arg "hex string has odd length";
  String.init
    (String.length encoded / 2)
    (fun index ->
      let high = hex_value encoded.[index * 2] in
      let low = hex_value encoded.[index * 2 + 1] in
      Char.chr ((high lsl 4) lor low))

let starts_with prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len && String.sub value 0 prefix_len = prefix

let contains_substring value pattern =
  let value_len = String.length value in
  let pattern_len = String.length pattern in
  if pattern_len = 0 then
    true
  else if pattern_len > value_len then
    false
  else
    let rec loop index =
      if index + pattern_len > value_len then
        false
      else if String.sub value index pattern_len = pattern then
        true
      else
        loop (index + 1)
    in
    loop 0

let payload_to_content payload =
  ocaml_payload_prefix ^ hex_encode (Marshal.to_string payload [])

let payload_of_content content =
  if starts_with ocaml_payload_prefix content then
    let encoded =
      String.sub
        content
        (String.length ocaml_payload_prefix)
        (String.length content - String.length ocaml_payload_prefix)
    in
    Some (Marshal.from_string (hex_decode encoded) 0 : storage_payload)
  else
    None

let sqlite_addr_of_storage_address = function
  | "0" -> 0
  | "1" -> 1
  | address ->
    (try int_of_string address with
     | Failure _ ->
       invalid_arg
         ("SQLite Logseq storage uses integer addresses; unsupported address: " ^ address))

let storage_address_of_sqlite_addr = function
  | 0 -> "0"
  | 1 -> "1"
  | address -> string_of_int address

let create_kvs_table db_path =
  exec_sql db_path (kvs_schema ^ ";")

let select_single_int ?(read_only = false) db_path sql =
  match select_map ~read_only db_path sql (fun stmt -> Sqlite3.column_int stmt 0) with
  | [] -> 0
  | value :: _ -> value

let select_single_string ?(read_only = false) db_path sql =
  match select_map ~read_only db_path sql (fun stmt -> Sqlite3.column_text stmt 0) with
  | [] -> None
  | first :: _ -> Some first

let content_format content =
  if content = "" then Empty
  else if starts_with ocaml_payload_prefix content then Ocaml_marshal
  else if starts_with "[\"^ \"" content || String.contains content '~' then Logseq_transit
  else Unknown

let string_of_transit_key = function
  | Logseq_transit.Keyword value | Logseq_transit.String value -> Some value
  | _ -> None

let int_of_transit_value = function
  | Logseq_transit.Int value -> Some value
  | Logseq_transit.Int64 value ->
    if value >= Int64.of_int min_int && value <= Int64.of_int max_int then
      Some (Int64.to_int value)
    else
      None
  | _ -> None

let lookup_transit_key key entries =
  List.find_map
    (fun (entry_key, value) ->
      match string_of_transit_key entry_key with
      | Some entry_key when entry_key = key -> Some value
      | _ -> None)
    entries

let string_of_root_json_key = function
  | `String text when starts_with "~:" text ->
    Some (String.sub text 2 (String.length text - 2))
  | `String text when text <> "^ " && not (starts_with "^" text) -> Some text
  | _ -> None

let int_of_root_json_value = function
  | `Int value -> Some value
  | `Intlit value -> int_of_string_opt value
  | _ -> None

let rec shallow_root_entries = function
  | key :: value :: rest ->
    (key, value) :: shallow_root_entries rest
  | [] -> []
  | [ _ ] -> []

let decode_shallow_root_metadata content =
  match Yojson.Safe.from_string content with
  | `List (`String "^ " :: entries) ->
    let entries = shallow_root_entries entries in
    let root_keys =
      entries
      |> List.filter_map (fun (key, _) -> string_of_root_json_key key)
      |> List.sort_uniq compare
    in
    let find_address key =
      entries
      |> List.find_map (fun (entry_key, value) ->
        match string_of_root_json_key entry_key with
        | Some entry_key when entry_key = key -> int_of_root_json_value value
        | _ -> None)
    in
    root_keys, List.filter_map find_address [ "eavt"; "aevt"; "avet" ]
  | _ -> [], []

let decode_root_metadata content =
  match content_format content with
  | Logseq_transit ->
    (try
       match Logseq_transit.of_string content with
       | Logseq_transit.Map entries ->
         let root_keys =
           entries
           |> List.filter_map (fun (key, _) -> string_of_transit_key key)
           |> List.sort_uniq compare
         in
         let root_index_addresses =
           [ "eavt"; "aevt"; "avet" ]
           |> List.filter_map (fun key -> Option.bind (lookup_transit_key key entries) int_of_transit_value)
         in
         root_keys, root_index_addresses
       | _ -> decode_shallow_root_metadata content
     with
     | Logseq_transit.Decode_error _ | Yojson.Json_error _ -> decode_shallow_root_metadata content)
  | Ocaml_marshal | Empty | Unknown -> [], []

let inspect ?(read_only = false) db_path =
  let has_kvs_table =
    select_single_int
      ~read_only
      db_path
      "select count(*) from sqlite_master where type = 'table' and name = 'kvs';"
    > 0
  in
  if not has_kvs_table then
    { has_kvs_table = false
    ; row_count = 0
    ; has_root = false
    ; has_tail = false
    ; root_content_format = Empty
    ; root_keys = []
    ; root_index_addresses = []
    }
  else
    let count sql = select_single_int ~read_only db_path sql in
    let root_content =
      select_single_string ~read_only db_path "select content from kvs where addr = 0 limit 1;"
    in
    let root_keys, root_index_addresses =
      match root_content with
      | None -> [], []
      | Some content -> decode_root_metadata content
    in
    { has_kvs_table = true
    ; row_count = count "select count(*) from kvs;"
    ; has_root = count "select count(*) from kvs where addr = 0;" > 0
    ; has_tail = count "select count(*) from kvs where addr = 1;" > 0
    ; root_content_format =
        (match root_content with
         | None -> Empty
         | Some content -> content_format content)
    ; root_keys
    ; root_index_addresses
    }

let graph_db_paths graphs_dir =
  if not (Sys.file_exists graphs_dir) then
    []
  else
    Sys.readdir graphs_dir
    |> Array.to_list
    |> List.filter_map (fun name ->
      let graph_dir = Filename.concat graphs_dir name in
      let db_path = Filename.concat graph_dir "db.sqlite" in
      if Sys.file_exists graph_dir && Sys.is_directory graph_dir && Sys.file_exists db_path then
        Some db_path
      else
        None)
    |> List.sort String.compare

let logseq_schema_default_attr =
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

let keyword_of_transit = function
  | Logseq_transit.Keyword value -> Some value
  | _ -> None

let bool_of_transit = function
  | Logseq_transit.Bool value -> Some value
  | _ -> None

let string_of_transit = function
  | Logseq_transit.String value -> Some value
  | _ -> None

let logseq_cardinality_of_transit = function
  | Logseq_transit.Keyword "db.cardinality/many" -> Many
  | Logseq_transit.Keyword "db.cardinality/one" -> One
  | _ -> One

let logseq_unique_of_transit = function
  | Logseq_transit.Keyword "db.unique/value" -> Some Value
  | Logseq_transit.Keyword "db.unique/identity" -> Some Identity
  | _ -> None

let logseq_value_type_of_transit = function
  | Logseq_transit.Keyword "db.type/ref" -> Some RefType
  | Logseq_transit.Keyword "db.type/tuple" -> Some TupleType
  | Logseq_transit.Keyword "db.type/string" -> Some StringType
  | Logseq_transit.Keyword "db.type/keyword" -> Some KeywordType
  | Logseq_transit.Keyword "db.type/number" -> Some NumberType
  | Logseq_transit.Keyword "db.type/uuid" -> Some UuidType
  | Logseq_transit.Keyword "db.type/instant" -> Some InstantType
  | _ -> None

let logseq_schema_attr_of_transit = function
  | Logseq_transit.Map props ->
    List.fold_left
      (fun schema (key, value) ->
        match keyword_of_transit key with
        | Some "db/cardinality" ->
          { schema with cardinality = logseq_cardinality_of_transit value }
        | Some "db/unique" -> { schema with unique = logseq_unique_of_transit value }
        | Some "db/index" ->
          { schema with indexed = Option.value ~default:false (bool_of_transit value) }
        | Some "db/isComponent" ->
          { schema with is_component = Option.value ~default:false (bool_of_transit value) }
        | Some "db/noHistory" ->
          { schema with no_history = Option.value ~default:false (bool_of_transit value) }
        | Some "db/doc" -> { schema with doc = string_of_transit value }
        | Some "db/valueType" ->
          { schema with value_type = logseq_value_type_of_transit value }
        | Some _ | None -> schema)
      logseq_schema_default_attr
      props
  | _ -> logseq_schema_default_attr

let logseq_timestamp_attrs =
  [ "created-at"; "updated-at"; "block/created-at"; "block/updated-at" ]

let ends_with suffix value =
  let suffix_len = String.length suffix in
  let value_len = String.length value in
  value_len >= suffix_len && String.sub value (value_len - suffix_len) suffix_len = suffix

let logseq_timestamp_attr attr =
  List.mem attr logseq_timestamp_attrs
  || ends_with "/graph-created-at" attr
  || ends_with "/graph-last-gc-at" attr
  || ends_with "/imported-at" attr
  || ends_with "/imported-last-updated-at" attr
  || ends_with "-created-at" attr
  || ends_with "-updated-at" attr

let normalize_logseq_schema_attr attr schema =
  if logseq_timestamp_attr attr then { schema with value_type = None } else schema

type shallow_reader =
  { mutable shallow_cache : string array
  ; shallow_cache_all_strings : bool
  }

let shallow_cache_code_digits = 44
let shallow_base_char_code = Char.code '0'

let shallow_cache_code_to_index text =
  match String.length text with
  | 2 -> Char.code text.[1] - shallow_base_char_code
  | 3 ->
    ((Char.code text.[1] - shallow_base_char_code) * shallow_cache_code_digits)
    + (Char.code text.[2] - shallow_base_char_code)
  | _ -> -1

let shallow_cacheable reader text =
  String.length text > 3
  && (reader.shallow_cache_all_strings
      || starts_with "~:" text
      || starts_with "~$" text)

let shallow_is_cache_code text =
  String.length text >= 2 && String.length text <= 3 && text.[0] = '^'
  && not (String.equal text "^ ")

let shallow_remember reader text =
  if shallow_cacheable reader text then
    reader.shallow_cache <- Array.append reader.shallow_cache [| text |]

let shallow_decode_string reader text =
  if shallow_is_cache_code text then
    let index = shallow_cache_code_to_index text in
    if index >= 0 && index < Array.length reader.shallow_cache then reader.shallow_cache.(index) else text
  else begin
    shallow_remember reader text;
    text
  end

let shallow_keyword reader = function
  | `String text ->
    let text = shallow_decode_string reader text in
    if starts_with "~:" text then Some (String.sub text 2 (String.length text - 2)) else None
  | _ -> None

let shallow_bool = function
  | `Bool value -> Some value
  | _ -> None

let rec shallow_scan reader = function
  | `String text ->
    ignore (shallow_decode_string reader text)
  | `List values -> List.iter (shallow_scan reader) values
  | `Assoc entries -> List.iter (fun (key, value) -> shallow_scan reader (`String key); shallow_scan reader value) entries
  | `Tuple values -> List.iter (shallow_scan reader) values
  | `Variant (tag, value) ->
    shallow_scan reader (`String tag);
    Option.iter (shallow_scan reader) value
  | `Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `Floatlit _ -> ()

let rec shallow_pairs = function
  | key :: value :: rest -> (key, value) :: shallow_pairs rest
  | [] | [ _ ] -> []

let shallow_value_type reader value =
  match shallow_keyword reader value with
  | Some "db.type/ref" -> Some RefType
  | Some "db.type/tuple" -> Some TupleType
  | Some "db.type/string" -> Some StringType
  | Some "db.type/keyword" -> Some KeywordType
  | Some "db.type/number" -> Some NumberType
  | Some "db.type/uuid" -> Some UuidType
  | Some "db.type/instant" -> Some InstantType
  | Some _ | None -> None

let shallow_unique reader value =
  match shallow_keyword reader value with
  | Some "db.unique/value" -> Some Value
  | Some "db.unique/identity" -> Some Identity
  | Some _ | None -> None

let shallow_schema_attr reader = function
  | `List (`String "^ " :: props) ->
    List.fold_left
      (fun schema (key, value) ->
        match shallow_keyword reader key with
        | Some "db/cardinality" ->
          let cardinality =
            match shallow_keyword reader value with
            | Some "db.cardinality/many" -> Many
            | _ -> One
          in
          { schema with cardinality }
        | Some "db/unique" -> { schema with unique = shallow_unique reader value }
        | Some "db/index" ->
          { schema with indexed = Option.value ~default:false (shallow_bool value) }
        | Some "db/isComponent" ->
          { schema with is_component = Option.value ~default:false (shallow_bool value) }
        | Some "db/noHistory" ->
          { schema with no_history = Option.value ~default:false (shallow_bool value) }
        | Some "db/valueType" ->
          { schema with value_type = shallow_value_type reader value }
        | Some "db/doc" ->
          (match value with
           | `String text -> { schema with doc = Some (shallow_decode_string reader text) }
           | _ -> schema)
        | Some _ | None ->
          shallow_scan reader value;
          schema)
      logseq_schema_default_attr
      (shallow_pairs props)
  | json ->
    shallow_scan reader json;
    logseq_schema_default_attr

let shallow_schema_of_root_content content =
  let reader = { shallow_cache = [||]; shallow_cache_all_strings = true } in
  match Yojson.Safe.from_string content with
  | `List (`String "^ " :: entries) ->
    shallow_pairs entries
    |> List.find_map (fun (key, value) ->
      match shallow_keyword reader key, value with
      | Some "schema", `List (`String "^ " :: schema_entries) ->
        Some
          (schema_entries
           |> shallow_pairs
           |> List.filter_map (fun (attr, schema) ->
             match shallow_keyword reader attr with
             | Some attr -> Some (attr, shallow_schema_attr reader schema |> normalize_logseq_schema_attr attr)
             | None ->
               shallow_scan reader schema;
               None))
      | _ ->
        shallow_scan reader value;
        None)
  | _ -> None

let logseq_root_content ?(read_only = false) db_path =
  match select_single_string ~read_only db_path "select content from kvs where addr = 0 limit 1;" with
  | Some content -> content
  | None -> invalid_arg "Logseq graph has no root metadata row"

let logseq_root_entries ?(read_only = false) db_path =
  match Logseq_transit.of_string (logseq_root_content ~read_only db_path) with
  | Logseq_transit.Map entries -> entries
  | _ -> invalid_arg "Logseq graph root metadata must be a Transit map"

let schema_of_logseq_graph ?(read_only = false) db_path =
  let content = logseq_root_content ~read_only db_path in
  try
    let root_entries =
      match Logseq_transit.of_string content with
      | Logseq_transit.Map entries -> entries
      | _ -> invalid_arg "Logseq graph root metadata must be a Transit map"
    in
    match lookup_transit_key "schema" root_entries with
    | Some (Logseq_transit.Map entries) ->
      entries
      |> List.filter_map (fun (attr, schema) ->
        match keyword_of_transit attr with
        | Some attr -> Some (attr, logseq_schema_attr_of_transit schema |> normalize_logseq_schema_attr attr)
        | None -> None)
    | Some _ -> invalid_arg "Logseq graph root :schema must be a Transit map"
    | None -> invalid_arg "Logseq graph root metadata has no :schema"
  with
  | Logseq_transit.Decode_error _ | Yojson.Json_error _ ->
    (match shallow_schema_of_root_content content with
     | Some schema -> schema
     | None -> invalid_arg "Logseq graph root metadata has no decodable :schema")

let int_of_shallow_string text =
  match int_of_string_opt text with
  | Some value -> value
  | None -> invalid_arg ("invalid Logseq integer value: " ^ text)

let rec logseq_value_of_shallow_json reader = function
  | `Null -> Nil
  | `Bool value -> Bool value
  | `Int value -> Int value
  | `Intlit value -> Int (int_of_shallow_string value)
  | `Float value -> Float value
  | `Floatlit value -> Float (float_of_string value)
  | `String text ->
    let text = shallow_decode_string reader text in
    if starts_with "~:" text then Keyword (String.sub text 2 (String.length text - 2))
    else if starts_with "~$" text then Symbol (String.sub text 2 (String.length text - 2))
    else if starts_with "~i" text then Int (int_of_shallow_string (String.sub text 2 (String.length text - 2)))
    else if starts_with "~u" text then Uuid (String.sub text 2 (String.length text - 2))
    else if starts_with "~?" text then
      (match String.sub text 2 (String.length text - 2) with
       | "t" -> Bool true
       | "f" -> Bool false
       | value -> invalid_arg ("invalid Logseq boolean value: " ^ value))
    else if text = "~_" then Nil
    else if starts_with "~~" text || starts_with "~^" text || starts_with "~`" text then
      String (String.sub text 1 (String.length text - 1))
    else
      String text
  | `List (`String "^ " :: entries) ->
    Map
      (shallow_pairs entries
       |> List.map (fun (key, value) ->
         logseq_value_of_shallow_json reader key, logseq_value_of_shallow_json reader value))
  | `List [ `String tag; `List values ] ->
    let tag = shallow_decode_string reader tag in
    if starts_with "~#" tag then
      match String.sub tag 2 (String.length tag - 2) with
      | "list" -> List (List.map (logseq_value_of_shallow_json reader) values)
      | "set" -> Set (List.map (logseq_value_of_shallow_json reader) values)
      | "cmap" ->
        Map
          (shallow_pairs values
           |> List.map (fun (key, value) ->
             logseq_value_of_shallow_json reader key, logseq_value_of_shallow_json reader value))
      | _ ->
        Vector [ String tag; Vector (List.map (logseq_value_of_shallow_json reader) values) ]
    else
      Vector [ String tag; Vector (List.map (logseq_value_of_shallow_json reader) values) ]
  | `List values -> Vector (List.map (logseq_value_of_shallow_json reader) values)
  | `Assoc entries ->
    Map
      (entries
       |> List.map (fun (key, value) ->
         String (shallow_decode_string reader key), logseq_value_of_shallow_json reader value))
  | `Tuple values -> List (List.map (logseq_value_of_shallow_json reader) values)
  | `Variant (tag, value) ->
    List
      [ String (shallow_decode_string reader tag)
      ; Option.value ~default:Nil (Option.map (logseq_value_of_shallow_json reader) value)
      ]

let logseq_attr_of_shallow_json reader = function
  | `String text ->
    let text = shallow_decode_string reader text in
    if starts_with "~:" text then String.sub text 2 (String.length text - 2) else text
  | _ -> invalid_arg "Logseq datom attr must be a Transit keyword string"

let logseq_int_of_shallow_json reader = function
  | `Int value -> value
  | `Intlit value -> int_of_shallow_string value
  | `String text ->
    let text = shallow_decode_string reader text in
    if starts_with "~i" text then int_of_shallow_string (String.sub text 2 (String.length text - 2))
    else int_of_shallow_string text
  | _ -> invalid_arg "Logseq datom integer field must be an integer"

let logseq_datom_of_shallow_json reader = function
  | `List [ entity; attr; value; tx ] ->
    let e = logseq_int_of_shallow_json reader entity in
    let a = logseq_attr_of_shallow_json reader attr in
    let v = logseq_value_of_shallow_json reader value in
    let tx = logseq_int_of_shallow_json reader tx in
    datom ~e ~a ~v ~tx ()
  | _ -> invalid_arg "Logseq graph :keys entries must be [e a v tx] datoms"

let logseq_datoms_of_row_with_reader reader content =
  match Yojson.Safe.from_string content with
  | `List (`String "^ " :: entries) ->
    (match entries with
     | `String text :: _ when not (shallow_is_cache_code text) -> reader.shallow_cache <- [||]
     | _ -> ());
    shallow_pairs entries
    |> List.find_map (fun (key, value) ->
      match shallow_keyword reader key, value with
      | Some "keys", `List datoms -> Some (List.map (logseq_datom_of_shallow_json reader) datoms)
      | _ -> None)
    |> Option.value ~default:[]
  | _ -> []

let logseq_datoms_of_row content =
  logseq_datoms_of_row_with_reader { shallow_cache = [||]; shallow_cache_all_strings = false } content

let add_query_attr acc = function
  | QAttr attr -> attr :: acc
  | QValue (Keyword attr | String attr | Symbol attr) -> attr :: acc
  | QVar _ | QEntity _ | QIdent _ | QLookupRef _ | QValue _ | QSource _ | QWildcard -> acc

let add_short_pattern_attrs acc = function
  | _ :: attr :: _ -> add_query_attr acc attr
  | _ -> acc

let rec add_query_clause_attrs acc = function
  | Pattern (_, attr, _)
  | PatternTx (_, attr, _, _)
  | PatternTxOp (_, attr, _, _, _)
  | SourcePattern (_, _, attr, _)
  | SourcePatternTx (_, _, attr, _, _)
  | SourcePatternTxOp (_, _, attr, _, _, _) ->
    add_query_attr acc attr
  | Missing (_, attr)
  | SourceMissing (_, _, attr)
  | GetElse (_, attr, _, _)
  | SourceGetElse (_, _, attr, _, _) ->
    attr :: acc
  | GetSome (_, attrs, _, _) | SourceGetSome (_, _, attrs, _, _) -> attrs @ acc
  | SourceClause (_, clause) -> add_query_clause_attrs acc clause
  | Not clauses | SourceNot (_, clauses) | NotJoin (_, clauses) | SourceNotJoin (_, _, clauses) ->
    List.fold_left add_query_clause_attrs acc clauses
  | Or branches
  | SourceOr (_, branches)
  | OrJoin (_, branches)
  | SourceOrJoin (_, _, branches)
  | OrJoinRequired (_, _, branches)
  | SourceOrJoinRequired (_, _, _, branches) ->
    List.fold_left
      (fun acc branch -> List.fold_left add_query_clause_attrs acc branch)
      acc
      branches
  | SourceRelationPattern (_, terms) ->
    add_short_pattern_attrs acc terms
  | GetValue _
  | GetDefaultValue _
  | CountValue _
  | EmptyValue _
  | NotEmptyValue _
  | ContainsValue _
  | ValuePredicate _
  | NumericPredicate _
  | ComparisonPredicate _
  | ComparisonPredicateN _
  | EqualityPredicate _
  | ArithmeticValue _
  | CompareValue _
  | ExtremumValue _
  | BooleanPredicate _
  | BooleanNotPredicate _
  | BooleanNotValue _
  | IdentityValue _
  | BooleanAndPredicate _
  | BooleanAndValue _
  | BooleanOrPredicate _
  | BooleanOrValue _
  | RandomValue _
  | RandomIntValue _
  | DifferPredicate _
  | IdenticalPredicate _
  | TypeValue _
  | MetaValue _
  | NameValue _
  | NamespaceValue _
  | KeywordFromName _
  | KeywordFromNamespaceName _
  | StringIncludesValue _
  | StringStartsWithValue _
  | StringEndsWithValue _
  | StringLowerCaseValue _
  | StringUpperCaseValue _
  | StringCapitalizeValue _
  | StringReverseValue _
  | StringTrimValue _
  | StringTrimLeftValue _
  | StringTrimRightValue _
  | StringTrimNewlineValue _
  | StringIndexOfValue _
  | StringLastIndexOfValue _
  | StringSubstringValue _
  | StringBuildValue _
  | PrintStringValue _
  | PrintLineStringValue _
  | PrStringValue _
  | PrnStringValue _
  | StringJoinPlainValue _
  | StringJoinValue _
  | StringReplaceValue _
  | StringReplaceFirstValue _
  | StringEscapeValue _
  | RePatternValue _
  | ReFindValue _
  | ReMatchesValue _
  | ReSeqValue _
  | ReFindPredicate _
  | ReMatchesPredicate _
  | StringBlankValue _
  | StringSplitValue _
  | StringSplitLimitValue _
  | StringSplitLinesValue _
  | Ground _
  | GroundCollection _
  | GroundTuple _
  | GroundRelation _
  | GroundTerm _
  | GroundTermCollection _
  | GroundTermTuple _
  | GroundTermRelation _
  | VectorValue _
  | ListValue _
  | SetValue _
  | HashMapValue _
  | ArrayMapValue _
  | RangeEndValue _
  | RangeValue _
  | RangeStepValue _
  | TupleFunction _
  | UntupleFunction _
  | Predicate _
  | Function _
  | DynamicPredicate _
  | DynamicFunction _
  | DynamicFunctionCollection _
  | DynamicFunctionRelation _
  | Rule _
  | SourceRule _ ->
    acc

let rec add_pull_selector_attrs acc = function
  | Pull_id -> "db/id" :: acc
  | Pull_wildcard -> acc
  | Pull_attr attr
  | Pull_attr_default (attr, _)
  | Pull_attr_limit (attr, _)
  | Pull_attr_unlimited attr
  | Pull_attr_xform (attr, _)
  | Pull_attr_default_xform (attr, _, _) ->
    attr :: acc
  | Pull_ref (attr, selectors)
  | Pull_ref_default (attr, selectors, _)
  | Pull_ref_limit (attr, selectors, _)
  | Pull_ref_unlimited (attr, selectors)
  | Pull_ref_xform (attr, selectors, _)
  | Pull_recursive_ref (attr, selectors, _)
  | Pull_reverse_ref (attr, selectors)
  | Pull_reverse_ref_default (attr, selectors, _)
  | Pull_reverse_ref_limit (attr, selectors, _)
  | Pull_reverse_ref_unlimited (attr, selectors)
  | Pull_reverse_ref_xform (attr, selectors, _) ->
    List.fold_left add_pull_selector_attrs (attr :: acc) selectors
  | Pull_as (selector, _) -> add_pull_selector_attrs acc selector

let add_find_spec_attrs acc = function
  | Find_pull (_, selectors) | Find_pull_source (_, _, selectors) ->
    List.fold_left add_pull_selector_attrs acc selectors
  | Find_var _
  | Find_pull_var _
  | Find_pull_source_var _
  | Find_aggregate _ ->
    acc

let query_attrs query =
  let attrs =
    List.fold_left add_query_clause_attrs [] query.where
    |> fun attrs -> List.fold_left add_find_spec_attrs attrs query.find
    |> List.cons "db/ident"
    |> List.sort_uniq String.compare
  in
  query.rules
  |> List.fold_left
       (fun attrs rule -> List.fold_left add_query_clause_attrs attrs rule.rule_body)
       attrs
  |> List.sort_uniq String.compare

let sql_like_pattern text =
  "'%" ^ String.concat "''" (String.split_on_char '\'' text) ^ "%'"

let logseq_keys_or_shorthand_row_sql =
  "(content like " ^ sql_like_pattern "~:keys" ^ " or content like " ^ sql_like_pattern "[\"^ \",\"^" ^ ")"

let logseq_keys_and_attrs_sql attrs =
  match attrs with
  | [] -> logseq_keys_or_shorthand_row_sql
  | attrs ->
    let attr_sql =
      attrs
      |> List.map (fun attr -> "content like " ^ sql_like_pattern ("~:" ^ attr))
      |> String.concat " or "
    in
    "content like " ^ sql_like_pattern "~:keys" ^ " and (" ^ attr_sql ^ ")"

let datoms_of_logseq_graph_for_attrs ?(read_only = false) db_path attrs =
  let include_all = attrs = [] in
  let row_starts_segment content =
    starts_with "[\"^ \",\"" content && not (starts_with "[\"^ \",\"^" content)
  in
  let row_mentions_attr content attr =
    contains_substring content ("~:" ^ attr)
  in
  let segment_mentions_attr rows =
    include_all || List.exists (fun row -> List.exists (row_mentions_attr row) attrs) rows
  in
  let decode_segment rows =
    if not (segment_mentions_attr rows) then
      []
    else
      let reader = { shallow_cache = [||]; shallow_cache_all_strings = false } in
      rows
      |> List.concat_map (fun content ->
        let datoms = logseq_datoms_of_row_with_reader reader content in
        if include_all then datoms else List.filter (fun datom -> List.mem datom.a attrs) datoms)
  in
  let flush_segment segment acc =
    match segment with
    | [] -> acc
    | segment -> List.rev_append (decode_segment (List.rev segment)) acc
  in
  let rows =
    select_map
    ~read_only
    db_path
    ("select content from kvs where addr not in (0, 1) and "
     ^ logseq_keys_or_shorthand_row_sql
     ^ " order by addr;")
    (fun stmt -> Sqlite3.column_text stmt 0)
  in
  let rec collect current acc = function
    | [] -> List.rev (flush_segment current acc)
    | row :: rest when row_starts_segment row && current <> [] ->
      collect [ row ] (flush_segment current acc) rest
    | row :: rest -> collect (row :: current) acc rest
  in
  collect [] [] rows

let datoms_of_logseq_graph ?(read_only = false) ?limit db_path =
  let limit_sql =
    match limit with
    | None -> ""
    | Some limit -> " limit " ^ string_of_int limit
  in
  let reader = { shallow_cache = [||]; shallow_cache_all_strings = false } in
  select_map
    ~read_only
    db_path
    ("select content from kvs where addr not in (0, 1) and "
     ^ logseq_keys_or_shorthand_row_sql
     ^ " order by addr"
     ^ limit_sql
     ^ ";")
    (fun stmt -> Sqlite3.column_text stmt 0)
  |> List.concat_map (logseq_datoms_of_row_with_reader reader)

let parse_logseq_query_with_schema ?(read_only = false) db_path query_string =
  let schema = schema_of_logseq_graph ~read_only db_path in
  let schema_db = empty_db ~schema () in
  let return, return_map, query =
    parse_query_return_map_string_with_pull_context ~default_pull_db:schema_db query_string
  in
  schema, return, return_map, query

let query_logseq_graph ?(read_only = false) db_path query_string =
  let schema, _, _, query = parse_logseq_query_with_schema ~read_only db_path query_string in
  let graph_datoms = datoms_of_logseq_graph_for_attrs ~read_only db_path (query_attrs query) in
  let db = init_db ~schema graph_datoms in
  q_return_map_string db query_string

let delete_sql addresses =
  match addresses with
  | [] -> ""
  | _ ->
    "delete from kvs where addr in ("
    ^ (addresses
       |> List.map sqlite_addr_of_storage_address
       |> List.map string_of_int
       |> String.concat ",")
    ^ ");"

let upsert_sql (address, payload) =
  let addr = sqlite_addr_of_storage_address address in
  let content = payload_to_content payload in
  Printf.sprintf
    "insert into kvs (addr, content, addresses) values (%d, %s, null) \
     on conflict(addr) do update set content = excluded.content, addresses = excluded.addresses;"
    addr
    (sql_quote content)

let storage db_path =
  create_kvs_table db_path;
  let store entries =
    let sql = String.concat "" (List.map upsert_sql entries) in
    if sql <> "" then exec_sql db_path sql
  in
  let restore address =
    let addr = sqlite_addr_of_storage_address address in
    let sql = Printf.sprintf "select content from kvs where addr = %d limit 1;" addr in
    Option.bind (select_single_string db_path sql) payload_of_content
  in
  let list_addresses () =
    select_map
      db_path
      "select addr from kvs order by addr;"
      (fun stmt -> storage_address_of_sqlite_addr (Sqlite3.column_int stmt 0))
  in
  let delete addresses =
    match delete_sql addresses with
    | "" -> ()
    | sql -> exec_sql db_path sql
  in
  { storage_store = store
  ; storage_restore = restore
  ; storage_list_addresses = list_addresses
  ; storage_delete = delete
  }
