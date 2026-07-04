type regex = string

external date_now : unit -> float = "now" [@@mel.scope "Date"]
external make_regexp : string -> string -> Js.Re.t = "RegExp" [@@mel.new]
external set_last_index : Js.Re.t -> int -> unit = "lastIndex" [@@mel.set]

let now_seconds () = date_now () /. 1000.0

let file_storage _dir =
  invalid_arg "file_storage is not supported on Melange"

let compile_regex pattern = pattern

let regexp ?(global = false) pattern =
  make_regexp pattern (if global then "g" else "")

let first_capture result =
  let captures = Js.Re.captures result in
  if Array.length captures = 0 then ""
  else Option.value ~default:"" (Js.Nullable.toOption captures.(0))

let replace_regex ~first_only pattern value replacement =
  Js.String.replaceByRe
    ~regexp:(regexp ~global:(not first_only) pattern)
    ~replacement
    value

let regex_find pattern value =
  match Js.Re.exec ~str:value (regexp pattern) with
  | None -> None
  | Some result -> Some (first_capture result)

let regex_matches pattern value =
  match Js.Re.exec ~str:value (regexp pattern) with
  | Some result ->
    let matched = first_capture result in
    if Js.Re.index result = 0 && String.length matched = String.length value then Some value
    else None
  | None -> None

let regex_seq pattern value =
  let regex = regexp ~global:true pattern in
  let rec collect acc =
    match Js.Re.exec ~str:value regex with
    | None -> List.rev acc
    | Some result ->
      let matched = first_capture result in
      if String.length matched = 0 then set_last_index regex (Js.Re.index result + 1);
      collect (matched :: acc)
  in
  collect []

let split_regex_limited pattern value limit =
  let limit = if limit <= 0 then max_int else limit in
  if limit = 1 then [ value ]
  else
    let regex = regexp ~global:true pattern in
    let rec collect index remaining acc =
      if remaining = 1 then
        List.rev (String.sub value index (String.length value - index) :: acc)
      else
        match Js.Re.exec ~str:value regex with
        | None -> List.rev (String.sub value index (String.length value - index) :: acc)
        | Some result ->
          let start = Js.Re.index result in
          let matched = first_capture result in
          let stop = start + String.length matched in
          let next_index = if stop <= start then start + 1 else stop in
          if stop <= start then set_last_index regex next_index;
          collect next_index (remaining - 1) (String.sub value index (start - index) :: acc)
    in
    collect 0 limit []

let split_regex pattern value =
  split_regex_limited pattern value 0
