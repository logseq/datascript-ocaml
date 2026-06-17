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

let read_all channel =
  let buffer = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string buffer (input_line channel);
       Buffer.add_char buffer '\n'
     done
   with
   | End_of_file -> ());
  Buffer.contents buffer

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

let run_sql ?(read_only = false) db_path sql =
  let sqlite_db_arg = if read_only then readonly_uri db_path else db_path in
  let argv =
    Array.of_list
      ([ "sqlite3"; "-batch"; "-noheader"; "-list"; "-separator"; "\t" ]
       @ [ sqlite_db_arg ])
  in
  let stdout, stdin, stderr = Unix.open_process_args_full "sqlite3" argv [||] in
  output_string stdin sql;
  if sql = "" || sql.[String.length sql - 1] <> '\n' then output_char stdin '\n';
  close_out stdin;
  let output = read_all stdout in
  let error = read_all stderr in
  match Unix.close_process_full (stdout, stdin, stderr) with
  | Unix.WEXITED 0 -> output
  | Unix.WEXITED code ->
    invalid_arg (Printf.sprintf "sqlite3 exited with %d while reading %s: %s" code db_path error)
  | Unix.WSIGNALED signal ->
    invalid_arg (Printf.sprintf "sqlite3 killed by signal %d while reading %s: %s" signal db_path error)
  | Unix.WSTOPPED signal ->
    invalid_arg (Printf.sprintf "sqlite3 stopped by signal %d while reading %s: %s" signal db_path error)

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
  | "datascript/root" | "0" -> 0
  | "datascript/tail" | "1" -> 1
  | address ->
    (try int_of_string address with
     | Failure _ ->
       invalid_arg
         ("SQLite Logseq storage uses integer addresses; unsupported address: " ^ address))

let storage_address_of_sqlite_addr = function
  | 0 -> "datascript/root"
  | 1 -> "datascript/tail"
  | address -> string_of_int address

let create_kvs_table db_path =
  ignore (run_sql db_path (kvs_schema ^ ";"))

let parse_single_int output =
  match String.trim output with
  | "" -> 0
  | value -> int_of_string value

let select_single_string ?(read_only = false) db_path sql =
  match String.split_on_char '\n' (run_sql ~read_only db_path sql |> String.trim) with
  | [] | [ "" ] -> None
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
    parse_single_int
      (run_sql
         ~read_only
         db_path
         "select count(*) from sqlite_master where type = 'table' and name = 'kvs';")
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
    let count sql = parse_single_int (run_sql ~read_only db_path sql) in
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
  let store entries delete_addresses =
    let sql =
      delete_sql delete_addresses ^ String.concat "" (List.map upsert_sql entries)
    in
    if sql <> "" then ignore (run_sql db_path sql)
  in
  let restore address =
    let addr = sqlite_addr_of_storage_address address in
    let sql = Printf.sprintf "select content from kvs where addr = %d limit 1;" addr in
    Option.bind (select_single_string db_path sql) payload_of_content
  in
  let list_addresses () =
    run_sql db_path "select addr from kvs order by addr;"
    |> String.split_on_char '\n'
    |> List.filter_map (fun line ->
      match String.trim line with
      | "" -> None
      | value -> Some (storage_address_of_sqlite_addr (int_of_string value)))
  in
  let delete addresses =
    match delete_sql addresses with
    | "" -> ()
    | sql -> ignore (run_sql db_path sql)
  in
  { storage_store = store
  ; storage_restore = restore
  ; storage_list_addresses = list_addresses
  ; storage_delete = delete
  }
