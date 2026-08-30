type regex = Str.regexp

let now_seconds = Unix.gettimeofday

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
