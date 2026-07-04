type regex = Str.regexp

open Datascript_types

let now_seconds = Unix.gettimeofday

let ensure_storage_dir dir =
  if Sys.file_exists dir then begin
    if not (Sys.is_directory dir) then
      invalid_arg ("storage path is not a directory: " ^ dir)
  end
  else Sys.mkdir dir 0o755

let hex_digit value =
  Char.chr (if value < 10 then Char.code '0' + value else Char.code 'a' + value - 10)

let hex_value = function
  | '0' .. '9' as ch -> Char.code ch - Char.code '0'
  | 'a' .. 'f' as ch -> Char.code ch - Char.code 'a' + 10
  | 'A' .. 'F' as ch -> Char.code ch - Char.code 'A' + 10
  | ch -> invalid_arg ("invalid storage address hex digit: " ^ String.make 1 ch)

let encode_storage_address address =
  String.init
    (String.length address * 2)
    (fun index ->
      let code = Char.code address.[index / 2] in
      if index mod 2 = 0 then hex_digit (code lsr 4) else hex_digit (code land 0x0f))

let decode_storage_address encoded =
  if String.length encoded mod 2 <> 0 then
    invalid_arg ("invalid storage address filename: " ^ encoded);
  String.init
    (String.length encoded / 2)
    (fun index ->
      let high = hex_value encoded.[index * 2] in
      let low = hex_value encoded.[index * 2 + 1] in
      Char.chr ((high lsl 4) lor low))

let storage_payload_path dir address =
  Filename.concat dir (encode_storage_address address ^ ".bin")

let file_storage dir =
  ensure_storage_dir dir;
  let write_payload address payload =
    let channel = open_out_bin (storage_payload_path dir address) in
    Fun.protect
      ~finally:(fun () -> close_out_noerr channel)
      (fun () -> Marshal.to_channel channel payload [])
  in
  let read_payload address =
    let path = storage_payload_path dir address in
    if not (Sys.file_exists path) then None
    else
      let channel = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () -> Some (Marshal.from_channel channel : storage_payload))
  in
  let list_addresses () =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter_map (fun filename ->
      if Filename.extension filename = ".bin" then
        let base = Filename.remove_extension filename in
        Some (decode_storage_address base)
      else
        None)
    |> List.sort_uniq compare
  in
  let delete addresses =
    List.iter
      (fun address ->
        let path = storage_payload_path dir address in
        if Sys.file_exists path then Sys.remove path)
      addresses
  in
  { storage_store =
      (fun entries ->
        List.iter (fun (address, payload) -> write_payload address payload) entries)
  ; storage_restore = read_payload
  ; storage_list_addresses = list_addresses
  ; storage_delete = delete
  }

let str_pattern_of_pattern pattern =
  let buffer = Buffer.create (String.length pattern) in
  let add_escaped = function
    | 'd' -> Buffer.add_string buffer "[0-9]"
    | 'D' -> Buffer.add_string buffer "[^0-9]"
    | 's' -> Buffer.add_string buffer "[ \t\n\r\012]"
    | 'S' -> Buffer.add_string buffer "[^ \t\n\r\012]"
    | 'w' -> Buffer.add_string buffer "[A-Za-z0-9_]"
    | 'W' -> Buffer.add_string buffer "[^A-Za-z0-9_]"
    | ch ->
      Buffer.add_char buffer '\\';
      Buffer.add_char buffer ch
  in
  let rec loop index in_char_class escaped =
    if index >= String.length pattern then ()
    else
      let ch = pattern.[index] in
      if escaped then (
        if in_char_class then (
          Buffer.add_char buffer '\\';
          Buffer.add_char buffer ch)
        else add_escaped ch;
        loop (index + 1) in_char_class false)
      else
        match ch with
        | '\\' -> loop (index + 1) in_char_class true
        | '[' ->
          Buffer.add_char buffer ch;
          loop (index + 1) true false
        | ']' ->
          Buffer.add_char buffer ch;
          loop (index + 1) false false
        | _ ->
          Buffer.add_char buffer ch;
          loop (index + 1) in_char_class false
  in
  loop 0 false false;
  Buffer.contents buffer

let compile_regex pattern = Str.regexp (str_pattern_of_pattern pattern)

let replace_regex ~first_only regex value replacement =
  if first_only then Str.replace_first regex replacement value
  else Str.global_replace regex replacement value

let regex_find regex value =
  try
    ignore (Str.search_forward regex value 0);
    Some (Str.matched_string value)
  with
  | Not_found -> None

let regex_matches regex value =
  if Str.string_match regex value 0 && Str.match_end () = String.length value then Some value
  else None

let regex_seq regex value =
  let rec collect index acc =
    if index > String.length value then List.rev acc
    else
      match Str.search_forward regex value index with
      | start ->
        let matched = Str.matched_string value in
        let stop = Str.match_end () in
        let next_index = if stop <= start then start + 1 else stop in
        collect next_index (matched :: acc)
      | exception Not_found -> List.rev acc
  in
  collect 0 []

let split_regex regex value = Str.split regex value

let split_regex_limited regex value limit =
  if limit = 1 then [ value ]
  else if limit <= 0 then Str.split regex value
  else Str.bounded_split regex value limit
