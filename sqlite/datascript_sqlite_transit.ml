type value =
  | Null
  | Bool of bool
  | String of string
  | Int of int
  | Int64 of int64
  | Float of float
  | Keyword of string
  | Symbol of string
  | Array of value list
  | Map of (value * value) list
  | Set of value list
  | List of value list
  | Tagged of string * value

exception Decode_error of string

type reader = { mutable cache : string array }

type json =
  | J_null
  | J_bool of bool
  | J_int of int
  | J_intlit of string
  | J_floatlit of string
  | J_string of string
  | J_list of json list
  | J_assoc of (string * json) list

let cache_code_digits = 44
let cache_size = cache_code_digits * cache_code_digits
let base_char_code = Char.code '0'
let max_safe_json_int = 9_007_199_254_740_992L
let decode_error message = raise (Decode_error message)

let cache_code_to_index text =
  match String.length text with
  | 2 -> Char.code text.[1] - base_char_code
  | 3 ->
      ((Char.code text.[1] - base_char_code) * cache_code_digits)
      + (Char.code text.[2] - base_char_code)
  | _ -> decode_error ("invalid cache code: " ^ text)

let reader_cacheable text = String.length text > 3

let remember reader text =
  if reader_cacheable text then (
    let len = Array.length reader.cache in
    if len >= cache_size then reader.cache <- [||];
    reader.cache <- Array.append reader.cache [| text |])

let lookup_cache reader text =
  let index = cache_code_to_index text in
  if index < 0 || index >= Array.length reader.cache then
    decode_error ("unknown cache code: " ^ text)
  else reader.cache.(index)

let is_cache_code text =
  String.length text >= 2
  && String.length text <= 3
  && text.[0] = '^'
  && not (String.equal text "^ ")

let is_safe_json_int value =
  Int64.compare value (Int64.neg max_safe_json_int) > 0
  && Int64.compare value max_safe_json_int < 0

let int_value text =
  match Int64.of_string_opt text with
  | Some value when is_safe_json_int value -> (
      match int_of_string_opt text with
      | Some value -> Int value
      | None -> Int64 value)
  | Some value -> Int64 value
  | None -> decode_error ("invalid integer: " ^ text)

let int64_value text =
  match Int64.of_string_opt text with
  | Some value -> value
  | None -> decode_error ("invalid int64: " ^ text)

let drop_prefix text = String.sub text 2 (String.length text - 2)

let rec read_string reader ?(remember_value = true) text =
  if is_cache_code text then
    read_string reader ~remember_value:false (lookup_cache reader text)
  else if String.length text = 0 then String text
  else if text.[0] <> '~' then (
    if remember_value then remember reader text;
    String text)
  else if String.length text = 1 then String text
  else
    match text.[1] with
    | '~' | '^' | '`' -> String (String.sub text 1 (String.length text - 1))
    | '_' when String.length text = 2 -> Null
    | '?' -> (
        match drop_prefix text with
        | "t" -> Bool true
        | "f" -> Bool false
        | value -> decode_error ("invalid boolean: " ^ value))
    | 'i' -> int_value (drop_prefix text)
    | 'd' -> Float (float_of_string (drop_prefix text))
    | ':' ->
        if remember_value then remember reader text;
        Keyword (drop_prefix text)
    | '$' ->
        if remember_value then remember reader text;
        Symbol (drop_prefix text)
    | 'm' -> Int64 (int64_value (drop_prefix text))
    | 'z' -> (
        match drop_prefix text with
        | "NaN" -> Float Float.nan
        | "INF" -> Float infinity
        | "-INF" -> Float neg_infinity
        | value -> decode_error ("invalid special number: " ^ value))
    | _ -> String text

and read_list reader values = Stdlib.List.map (read reader) values

and read_key reader json =
  match json with
  | J_string text -> read_string reader text
  | _ -> read reader json

and read_tag reader text =
  let remember_value = not (is_cache_code text) in
  let text = if is_cache_code text then lookup_cache reader text else text in
  if String.length text >= 3 && String.sub text 0 2 = "~#" then (
    if remember_value then remember reader text;
    String.sub text 2 (String.length text - 2))
  else decode_error ("invalid tag: " ^ text)

and read_composite reader tag rep =
  match (tag, rep) with
  | "set", J_list values -> Set (read_list reader values)
  | "list", J_list values -> List (read_list reader values)
  | "cmap", J_list values -> Map (read_flat_entries reader values)
  | "m", value -> Int64 (read_time_rep reader value)
  | tag, value -> Tagged (tag, read reader value)

and read_flat_entries reader = function
  | [] -> []
  | key :: value :: rest ->
      let key = read reader key in
      let value = read reader value in
      (key, value) :: read_flat_entries reader rest
  | _ -> decode_error "map requires an even number of elements"

and read_time_rep reader = function
  | J_int value -> Int64.of_int value
  | J_intlit value -> int64_value value
  | json -> (
      match read reader json with
      | Int value -> Int64.of_int value
      | Int64 value -> value
      | _ -> decode_error "time rep must be an integer")

and read_map_array reader = function
  | J_string "^ " :: entries -> Map (read_stringable_entries reader entries)
  | values -> Array (read_list reader values)

and read_stringable_entries reader = function
  | [] -> []
  | key :: value :: rest ->
      let key = read_key reader key in
      let value = read reader value in
      (key, value) :: read_stringable_entries reader rest
  | _ -> decode_error "map-as-array requires an even number of elements"

and read_assoc reader entries =
  match entries with
  | [ (tag, rep) ] when String.length tag >= 2 && tag.[0] = '~' && tag.[1] = '#'
    ->
      read_composite reader (read_tag reader tag) rep
  | _ -> Map (read_assoc_entries reader entries)

and read_assoc_entries reader = function
  | [] -> []
  | (key, value) :: rest ->
      (read_string reader key, read reader value)
      :: read_assoc_entries reader rest

and read reader = function
  | J_null -> Null
  | J_bool value -> Bool value
  | J_int value -> Int value
  | J_intlit value -> int_value value
  | J_floatlit value -> Float (float_of_string value)
  | J_string text -> read_string reader text
  | J_list [ J_string tag; rep ]
    when String.length tag >= 2
         && (tag.[0] = '^' || (tag.[0] = '~' && tag.[1] = '#')) ->
      read_composite reader (read_tag reader tag) rep
  | J_list values -> read_map_array reader values
  | J_assoc entries -> read_assoc reader entries

module Json = struct
  type parser = { text : string; mutable index : int; length : int }

  let fail parser message =
    decode_error (message ^ " at byte " ^ string_of_int parser.index)

  let create text = { text; index = 0; length = String.length text }

  let peek parser =
    if parser.index >= parser.length then None
    else Some parser.text.[parser.index]

  let take parser =
    match peek parser with
    | None -> fail parser "unexpected end of JSON"
    | Some ch ->
        parser.index <- parser.index + 1;
        ch

  let rec skip_ws parser =
    match peek parser with
    | Some (' ' | '\n' | '\r' | '\t') ->
        parser.index <- parser.index + 1;
        skip_ws parser
    | _ -> ()

  let expect parser expected =
    let actual = take parser in
    if not (Char.equal actual expected) then
      fail parser ("expected '" ^ String.make 1 expected ^ "'")

  let starts_with parser value =
    let value_len = String.length value in
    parser.index + value_len <= parser.length
    && String.sub parser.text parser.index value_len = value

  let consume_literal parser value result =
    if starts_with parser value then (
      parser.index <- parser.index + String.length value;
      result)
    else fail parser ("expected " ^ value)

  let hex_value = function
    | '0' .. '9' as ch -> Char.code ch - Char.code '0'
    | 'a' .. 'f' as ch -> 10 + Char.code ch - Char.code 'a'
    | 'A' .. 'F' as ch -> 10 + Char.code ch - Char.code 'A'
    | _ -> -1

  let append_utf8 buffer code =
    Uchar.of_int code |> Buffer.add_utf_8_uchar buffer

  let parse_hex4 parser =
    let code = ref 0 in
    for _ = 1 to 4 do
      let value = hex_value (take parser) in
      if value < 0 then fail parser "invalid unicode escape";
      code := (!code lsl 4) lor value
    done;
    !code

  let parse_string parser =
    expect parser '"';
    let buffer = Buffer.create 32 in
    let rec loop () =
      match take parser with
      | '"' -> Buffer.contents buffer
      | '\\' ->
          (match take parser with
          | '"' -> Buffer.add_char buffer '"'
          | '\\' -> Buffer.add_char buffer '\\'
          | '/' -> Buffer.add_char buffer '/'
          | 'b' -> Buffer.add_char buffer '\b'
          | 'f' -> Buffer.add_char buffer '\012'
          | 'n' -> Buffer.add_char buffer '\n'
          | 'r' -> Buffer.add_char buffer '\r'
          | 't' -> Buffer.add_char buffer '\t'
          | 'u' -> append_utf8 buffer (parse_hex4 parser)
          | _ -> fail parser "invalid string escape");
          loop ()
      | ch ->
          Buffer.add_char buffer ch;
          loop ()
    in
    loop ()

  let is_number_char = function
    | '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> true
    | _ -> false

  let parse_number parser =
    let start = parser.index in
    while
      match peek parser with
      | Some ch when is_number_char ch ->
          parser.index <- parser.index + 1;
          true
      | _ -> false
    do
      ()
    done;
    let token = String.sub parser.text start (parser.index - start) in
    if
      String.contains token '.' || String.contains token 'e'
      || String.contains token 'E'
    then J_floatlit token
    else
      match int_of_string_opt token with
      | Some value -> J_int value
      | None -> J_intlit token

  let rec parse_value parser =
    skip_ws parser;
    match peek parser with
    | Some '"' -> J_string (parse_string parser)
    | Some '[' -> parse_array parser
    | Some '{' -> parse_object parser
    | Some 't' -> consume_literal parser "true" (J_bool true)
    | Some 'f' -> consume_literal parser "false" (J_bool false)
    | Some 'n' -> consume_literal parser "null" J_null
    | Some ('-' | '0' .. '9') -> parse_number parser
    | Some _ -> fail parser "unexpected JSON character"
    | None -> fail parser "empty JSON"

  and parse_array parser =
    expect parser '[';
    skip_ws parser;
    match peek parser with
    | Some ']' ->
        parser.index <- parser.index + 1;
        J_list []
    | _ ->
        let rec loop acc =
          let value = parse_value parser in
          skip_ws parser;
          match take parser with
          | ',' -> loop (value :: acc)
          | ']' -> J_list (List.rev (value :: acc))
          | _ -> fail parser "expected array separator"
        in
        loop []

  and parse_object parser =
    expect parser '{';
    skip_ws parser;
    match peek parser with
    | Some '}' ->
        parser.index <- parser.index + 1;
        J_assoc []
    | _ ->
        let rec loop acc =
          skip_ws parser;
          let key = parse_string parser in
          skip_ws parser;
          expect parser ':';
          let value = parse_value parser in
          skip_ws parser;
          match take parser with
          | ',' -> loop ((key, value) :: acc)
          | '}' -> J_assoc (List.rev ((key, value) :: acc))
          | _ -> fail parser "expected object separator"
        in
        loop []

  let parse text =
    let parser = create text in
    let value = parse_value parser in
    skip_ws parser;
    if parser.index <> parser.length then fail parser "trailing JSON input";
    value

  let write_string text =
    let buffer = Buffer.create (String.length text + 2) in
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
        | ch when Char.code ch < 0x20 ->
            Buffer.add_string buffer (Printf.sprintf "\\u%04x" (Char.code ch))
        | ch -> Buffer.add_char buffer ch)
      text;
    Buffer.add_char buffer '"';
    Buffer.contents buffer
end

let of_string text = Json.parse text |> read { cache = [||] }

let escape_string text =
  if String.length text > 0 then
    match text.[0] with '~' | '^' | '`' -> "~" ^ text | _ -> text
  else text

let rec write = function
  | Null -> J_string "~_"
  | Bool value -> J_bool value
  | String value -> J_string (escape_string value)
  | Int value -> J_int value
  | Int64 value
    when value >= Int64.of_int min_int && value <= Int64.of_int max_int ->
      J_int (Int64.to_int value)
  | Int64 value -> J_string ("~i" ^ Int64.to_string value)
  | Float value -> J_floatlit (string_of_float value)
  | Keyword value -> J_string ("~:" ^ value)
  | Symbol value -> J_string ("~$" ^ value)
  | Array values -> J_list (Stdlib.List.map write values)
  | Map entries ->
      J_list
        (J_string "^ "
        :: Stdlib.List.concat
             (Stdlib.List.map
                (fun (key, value) -> [ write key; write value ])
                entries))
  | Set values ->
      J_list [ J_string "~#set"; J_list (Stdlib.List.map write values) ]
  | List values ->
      J_list [ J_string "~#list"; J_list (Stdlib.List.map write values) ]
  | Tagged (tag, value) -> J_list [ J_string ("~#" ^ tag); write value ]

let rec json_to_string = function
  | J_null -> "null"
  | J_bool true -> "true"
  | J_bool false -> "false"
  | J_int value -> string_of_int value
  | J_intlit value | J_floatlit value -> value
  | J_string value -> Json.write_string value
  | J_list values ->
      "[" ^ String.concat "," (Stdlib.List.map json_to_string values) ^ "]"
  | J_assoc entries ->
      let entry_to_string (key, value) =
        Json.write_string key ^ ":" ^ json_to_string value
      in
      "{" ^ String.concat "," (Stdlib.List.map entry_to_string entries) ^ "}"

let to_string value = value |> write |> json_to_string
