open Js_of_ocaml

type regex = Regexp.regexp

let now_seconds () =
  let now_ms = Js.Unsafe.meth_call Js.date "now" [||] in
  Js.to_float now_ms /. 1000.0

let compile_regex = Regexp.regexp

let replace_regex ~first_only regex value replacement =
  if first_only then Regexp.replace_first regex value replacement
  else Regexp.global_replace regex value replacement

let regex_find regex value =
  match Regexp.search_forward regex value 0 with
  | Some (_, result) -> Some (Regexp.matched_string result)
  | None -> None

let regex_matches regex value =
  match Regexp.string_match regex value 0 with
  | Some result when String.length (Regexp.matched_string result) = String.length value ->
    Some value
  | Some _ | None -> None

let regex_seq regex value =
  let rec collect index acc =
    if index > String.length value then List.rev acc
    else
      match Regexp.search_forward regex value index with
      | None -> List.rev acc
      | Some (start, result) ->
        let matched = Regexp.matched_string result in
        let stop = start + String.length matched in
        let next_index = if stop <= start then start + 1 else stop in
        collect next_index (matched :: acc)
  in
  collect 0 []

let split_regex regex value = Regexp.split regex value

let split_regex_limited regex value limit =
  if limit = 1 then [ value ]
  else if limit <= 0 then Regexp.split regex value
  else Regexp.bounded_split regex value limit
