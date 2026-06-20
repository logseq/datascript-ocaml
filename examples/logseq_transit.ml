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

let cache_code_digits = 44
let cache_size = cache_code_digits * cache_code_digits
let base_char_code = Char.code '0'

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
  if reader_cacheable text then begin
    let len = Array.length reader.cache in
    if len >= cache_size then reader.cache <- [||];
    reader.cache <- Array.append reader.cache [| text |]
  end

let lookup_cache reader text =
  let index = cache_code_to_index text in
  if index < 0 || index >= Array.length reader.cache then
    decode_error ("unknown cache code: " ^ text)
  else
    reader.cache.(index)

let is_cache_code text =
  String.length text >= 2 && String.length text <= 3 && text.[0] = '^'
  && not (String.equal text "^ ")

let max_safe_json_int = 9_007_199_254_740_992L

let is_safe_json_int value =
  Int64.compare value (Int64.neg max_safe_json_int) > 0
  && Int64.compare value max_safe_json_int < 0

let int_value text =
  match Int64.of_string_opt text with
  | Some value when is_safe_json_int value ->
    (match int_of_string_opt text with
     | Some value -> Int value
     | None -> Int64 value)
  | Some value -> Int64 value
  | None -> decode_error ("invalid integer: " ^ text)

let int64_value text =
  match Int64.of_string_opt text with
  | Some value -> value
  | None -> decode_error ("invalid int64: " ^ text)

let drop_prefix text =
  String.sub text 2 (String.length text - 2)

let rec read_string reader ?(cache_string_key = false) ?(remember_value = true) text =
  if is_cache_code text then
    read_string reader ~cache_string_key ~remember_value:false (lookup_cache reader text)
  else if String.length text = 0 then
    String text
  else if text.[0] <> '~' then begin
    if remember_value then remember reader text;
    String text
  end
  else if String.length text = 1 then
    String text
  else
    match text.[1] with
    | '~' | '^' | '`' -> String (String.sub text 1 (String.length text - 1))
    | '_' when String.length text = 2 -> Null
    | '?' ->
      (match drop_prefix text with
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
    | 'z' ->
      (match drop_prefix text with
       | "NaN" -> Float Float.nan
       | "INF" -> Float infinity
       | "-INF" -> Float neg_infinity
       | value -> decode_error ("invalid special number: " ^ value))
    | _ -> String text

and read_list reader values =
  match values with
  | [] -> []
  | value :: rest -> read reader value :: read_list reader rest

and read_key reader json =
  match json with
  | `String text -> read_string reader ~cache_string_key:true text
  | _ -> read reader json

and read_tag reader text =
  let remember_value = not (is_cache_code text) in
  let text = if is_cache_code text then lookup_cache reader text else text in
  if String.length text >= 3 && String.sub text 0 2 = "~#" then begin
    if remember_value then remember reader text;
    String.sub text 2 (String.length text - 2)
  end
  else
    decode_error ("invalid tag: " ^ text)

and read_composite reader tag rep =
  match tag, rep with
  | "set", `List values -> Set (read_list reader values)
  | "list", `List values -> List (read_list reader values)
  | "cmap", `List values -> Map (read_flat_entries reader values)
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
  | `Int value -> Int64.of_int value
  | `Intlit value -> int64_value value
  | json ->
    (match read reader json with
     | Int value -> Int64.of_int value
     | Int64 value -> value
     | _ -> decode_error "time rep must be an integer")

and read_map_array reader = function
  | `String "^ " :: entries -> Map (read_stringable_entries reader entries)
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
  | [ tag, rep ] when String.length tag >= 2 && tag.[0] = '~' && tag.[1] = '#' ->
    read_composite reader (read_tag reader tag) rep
  | _ -> Map (read_assoc_entries reader entries)

and read_assoc_entries reader = function
  | [] -> []
  | (key, value) :: rest ->
    (read_string reader key, read reader value) :: read_assoc_entries reader rest

and read reader = function
  | `Null -> Null
  | `Bool value -> Bool value
  | `Int value -> Int value
  | `Intlit value -> int_value value
  | `Float value -> Float value
  | `Floatlit value -> Float (float_of_string value)
  | `String text -> read_string reader text
  | `List [ `String tag; rep ]
    when String.length tag >= 2 && (tag.[0] = '^' || (tag.[0] = '~' && tag.[1] = '#'))
    -> read_composite reader (read_tag reader tag) rep
  | `List values -> read_map_array reader values
  | `Assoc entries -> read_assoc reader entries
  | `Tuple values -> Array (read_list reader values)
  | `Variant (tag, None) -> Tagged (tag, Null)
  | `Variant (tag, Some value) -> Tagged (tag, read reader value)

let of_string text =
  Yojson.Safe.from_string text |> read { cache = [||] }

let escape_string text =
  if String.length text > 0 then
    match text.[0] with
    | '~' | '^' | '`' -> "~" ^ text
    | _ -> text
  else
    text

let rec write = function
  | Null -> `String "~_"
  | Bool value -> `Bool value
  | String value -> `String (escape_string value)
  | Int value -> `Int value
  | Int64 value when value >= Int64.of_int min_int && value <= Int64.of_int max_int ->
    `Int (Int64.to_int value)
  | Int64 value -> `String ("~i" ^ Int64.to_string value)
  | Float value -> `Float value
  | Keyword value -> `String ("~:" ^ value)
  | Symbol value -> `String ("~$" ^ value)
  | Array values -> `List (List.map write values)
  | Map entries ->
    `List (`String "^ " :: List.concat_map (fun (key, value) -> [ write key; write value ]) entries)
  | Set values -> `List [ `String "~#set"; `List (List.map write values) ]
  | List values -> `List [ `String "~#list"; `List (List.map write values) ]
  | Tagged (tag, value) -> `List [ `String ("~#" ^ tag); write value ]

let to_string value =
  Yojson.Safe.to_string (write value)
