type atom =
  | Literal of char
  | Char_class of (char * char) list

type token = { atom : atom; one_or_more : bool }
type regex = token list

external now_seconds : unit -> float = "datascript_now_seconds"

let parse_char_class pattern start =
  let rec loop index ranges =
    if index >= String.length pattern then invalid_arg "unterminated character class"
    else if pattern.[index] = ']' then (List.rev ranges, index + 1)
    else if index + 2 < String.length pattern && pattern.[index + 1] = '-' then
      loop (index + 3) ((pattern.[index], pattern.[index + 2]) :: ranges)
    else loop (index + 1) ((pattern.[index], pattern.[index]) :: ranges)
  in
  loop start []

let compile_regex pattern =
  let rec loop index tokens =
    if index >= String.length pattern then List.rev tokens
    else
      let atom, next_index =
        match pattern.[index] with
        | '[' ->
            let ranges, next_index = parse_char_class pattern (index + 1) in
            (Char_class ranges, next_index)
        | '\\' when index + 1 < String.length pattern ->
            (Literal pattern.[index + 1], index + 2)
        | ch -> (Literal ch, index + 1)
      in
      let one_or_more =
        next_index < String.length pattern && pattern.[next_index] = '+'
      in
      let next_index = if one_or_more then next_index + 1 else next_index in
      loop next_index ({ atom; one_or_more } :: tokens)
  in
  loop 0 []

let atom_matches atom ch =
  match atom with
  | Literal expected -> Char.equal expected ch
  | Char_class ranges ->
      List.exists
        (fun (start, stop) -> Char.compare ch start >= 0 && Char.compare ch stop <= 0)
        ranges

let rec match_tokens tokens value index =
  match tokens with
  | [] -> Some index
  | token :: rest ->
      if token.one_or_more then
        let rec consume current =
          if current < String.length value && atom_matches token.atom value.[current] then
            consume (current + 1)
          else current
        in
        let stop = consume index in
        if stop = index then None
        else
          let rec try_lengths current =
            if current < index then None
            else
              match match_tokens rest value current with
              | Some _ as matched -> matched
              | None -> try_lengths (current - 1)
          in
          try_lengths stop
      else if index < String.length value && atom_matches token.atom value.[index] then
        match_tokens rest value (index + 1)
      else None

let find_match regex value start =
  let rec loop index =
    if index > String.length value then None
    else
      match match_tokens regex value index with
      | Some stop -> Some (index, stop)
      | None -> loop (index + 1)
  in
  loop start

let regex_find regex value =
  match find_match regex value 0 with
  | Some (start, stop) -> Some (String.sub value start (stop - start))
  | None -> None

let regex_matches regex value =
  match match_tokens regex value 0 with
  | Some stop when stop = String.length value -> Some value
  | _ -> None

let regex_seq regex value =
  let rec collect index acc =
    match find_match regex value index with
    | None -> List.rev acc
    | Some (start, stop) ->
        let matched = String.sub value start (stop - start) in
        let next_index = if stop <= start then start + 1 else stop in
        collect next_index (matched :: acc)
  in
  collect 0 []

let replace_regex ~first_only regex value replacement =
  let buffer = Buffer.create (String.length value) in
  let rec loop index replaced =
    match find_match regex value index with
    | None -> Buffer.add_substring buffer value index (String.length value - index)
    | Some (start, stop) ->
        Buffer.add_substring buffer value index (start - index);
        Buffer.add_string buffer replacement;
        if first_only && not replaced then
          Buffer.add_substring buffer value stop (String.length value - stop)
        else loop stop true
  in
  loop 0 false;
  Buffer.contents buffer

let split_regex_limited regex value limit =
  let limit = if limit <= 0 then max_int else limit in
  let rec collect index remaining acc =
    if remaining = 1 then List.rev (String.sub value index (String.length value - index) :: acc)
    else
      match find_match regex value index with
      | None -> List.rev (String.sub value index (String.length value - index) :: acc)
      | Some (start, stop) ->
          let next_index = if stop <= start then start + 1 else stop in
          collect next_index (remaining - 1) (String.sub value index (start - index) :: acc)
  in
  if limit = 1 then [ value ]
  else collect 0 limit []

let split_regex regex value = split_regex_limited regex value 0
