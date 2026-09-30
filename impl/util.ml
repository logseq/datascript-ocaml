open Datascript_types

let rec list_equal_by equal left right =
  match left, right with
  | [], [] -> true
  | left :: left_rest, right :: right_rest ->
    equal left right && list_equal_by equal left_rest right_rest
  | [], _ :: _ | _ :: _, [] -> false

let int64_to_int value =
  if Int64.compare value (Int64.of_int min_int) >= 0
     && Int64.compare value (Int64.of_int max_int) <= 0
  then Some (Int64.to_int value)
  else None

let int64_to_int_exn label value =
  match int64_to_int value with
  | Some value -> value
  | None -> invalid_arg (label ^ ": int64 out of int range: " ^ Int64.to_string value)

(* days-from-civil inverse (Howard Hinnant's civil calendar algorithm) *)
let civil_from_days days =
  let days = Int64.add days 719468L in
  let era = Int64.div (if Int64.compare days 0L >= 0 then days else Int64.sub days 146096L) 146097L in
  let day_of_era = Int64.sub days (Int64.mul era 146097L) in
  let year_of_era =
    Int64.div
      (Int64.sub day_of_era (Int64.add (Int64.div day_of_era 1460L) (Int64.sub (Int64.div day_of_era 36524L) (Int64.div day_of_era 146096L))))
      365L
  in
  let year = Int64.add year_of_era (Int64.mul era 400L) in
  let day_of_year =
    Int64.sub day_of_era
      (Int64.add (Int64.mul 365L year_of_era) (Int64.sub (Int64.div year_of_era 4L) (Int64.div year_of_era 100L)))
  in
  let month_prime = Int64.div (Int64.add (Int64.mul 5L day_of_year) 2L) 153L in
  let day = Int64.sub day_of_year (Int64.sub (Int64.div (Int64.add (Int64.mul 153L month_prime) 2L) 5L) 1L) in
  let month = if Int64.compare month_prime 10L < 0 then Int64.add month_prime 3L else Int64.sub month_prime 9L in
  let year = if Int64.compare month 2L <= 0 then Int64.add year 1L else year in
  Int64.to_int year, Int64.to_int month, Int64.to_int day

(* "YYYY-MM-DDTHH:MM:SS.mmmZ" — the canonical #inst literal form *)
let string_of_instant_millis millis =
  let days =
    let d = Int64.div millis 86400000L in
    (* floor division: shift down when millis is negative and not a whole day *)
    if Int64.compare millis 0L < 0 && Int64.rem millis 86400000L <> 0L then Int64.sub d 1L else d
  in
  let rem = Int64.sub millis (Int64.mul days 86400000L) in
  let year, month, day = civil_from_days days in
  let hour = Int64.to_int (Int64.div rem 3600000L) in
  let minute = Int64.to_int (Int64.div (Int64.rem rem 3600000L) 60000L) in
  let second = Int64.to_int (Int64.div (Int64.rem rem 60000L) 1000L) in
  let ms = Int64.to_int (Int64.rem rem 1000L) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ" year month day hour minute second ms

let rec entity_ref_equal left right =
  match left, right with
  | Entity_id left, Entity_id right -> left = right
  | Temp_id left, Temp_id right -> left = right
  | CurrentTx, CurrentTx -> true
  | Ident left, Ident right -> left = right
  | Lookup_ref (left_attr, left_value), Lookup_ref (right_attr, right_value) ->
    left_attr = right_attr && value_equal left_value right_value
  | _ -> false

and value_equal left right =
  match left, right with
  | Nil, Nil -> true
  | Int64 left, Int64 right -> Int64.equal left right
  | Float left, Float right ->
    (classify_float left = FP_nan && classify_float right = FP_nan) || left = right
  | String left, String right -> left = right
  | Symbol left, Symbol right -> left = right
  | Bool left, Bool right -> left = right
  | Keyword left, Keyword right -> left = right
  | Uuid left, Uuid right -> left = right
  | Instant left, Instant right -> left = right
  | Regex left, Regex right -> left = right
  | Ref left, Ref right -> left = right
  | List left, List right -> list_equal_by value_equal left right
  | Vector left, Vector right -> list_equal_by value_equal left right
  | Set left, Set right -> list_equal_by value_equal left right
  | Map left, Map right ->
    list_equal_by
      (fun (left_key, left_value) (right_key, right_value) ->
         value_equal left_key right_key && value_equal left_value right_value)
      left
      right
  | Tuple left, Tuple right ->
    list_equal_by
      (fun left right ->
         match left, right with
         | None, None -> true
         | Some left, Some right -> value_equal left right
         | None, Some _ | Some _, None -> false)
      left
      right
  | TxRef, TxRef -> true
  | Ref_to left, Ref_to right -> entity_ref_equal left right
  | _ -> false

let compare_list_with = Datascript_types.Compare.compare_list_with
let compare_option_with = Datascript_types.Compare.compare_option_with
let split_keyword = Datascript_types.Compare.split_keyword
let compare_attr = Datascript_types.Compare.compare_attr
let compare_value = Datascript_types.Compare.compare_value
let compare_datom = Datascript_types.Compare.compare_datom
let compare_map_entry = Datascript_types.Compare.compare_map_entry


let first_nonzero comparisons =
  List.find_opt (( <> ) 0) comparisons
  |> Option.value ~default:0

let first_nonzero4 = Datascript_types.Compare.first_nonzero4

(* transit-js UUIDfromString semantics: strip '-', take the first 32
   chars as 16 hex pairs, and parseInt each pair at radix 16 (leading
   hex prefix of the pair, else 0). cljs ~u decode produces this
   canonical form for any input, so a uuid value read back from storage
   is always canonical; applying the same transform at construction
   keeps in-memory and materialized values identical. *)
let uuid_canonicalize uuid =
  let hex_opt ch =
    match ch with
    | '0' .. '9' -> Some (Char.code ch - Char.code '0')
    | 'a' .. 'f' -> Some (10 + Char.code ch - Char.code 'a')
    | 'A' .. 'F' -> Some (10 + Char.code ch - Char.code 'A')
    | _ -> None
  in
  let pair_value first second =
    match hex_opt first, second with
    | Some h1, Some c2 -> (
        match hex_opt c2 with
        | Some h2 -> (h1 * 16) + h2
        | None -> h1)
    | Some h1, None -> h1
    | None, _ -> 0
  in
  let digits =
    uuid
    |> String.to_seq
    |> Seq.filter (fun ch -> ch <> '-')
    |> List.of_seq
  in
  let bytes = Array.make 16 0 in
  let rec loop index ds =
    if index >= 16 then ()
    else
      match ds with
      | first :: second :: rest ->
          bytes.(index) <- pair_value first (Some second);
          loop (index + 1) rest
      | [ first ] -> bytes.(index) <- pair_value first None
      | [] -> ()
  in
  loop 0 digits;
  let b = Bytes.make 36 '-' in
  for i = 0 to 15 do
    let offset =
      i * 2
      + (if i >= 10 then 4
         else if i >= 8 then 3
         else if i >= 6 then 2
         else if i >= 4 then 1
         else 0)
    in
    let hi = bytes.(i) lsr 4 and lo = bytes.(i) land 0xf in
    Bytes.set b offset
      (Char.chr (hi + (if hi < 10 then Char.code '0' else Char.code 'a' - 10)));
    Bytes.set b (offset + 1)
      (Char.chr (lo + (if lo < 10 then Char.code '0' else Char.code 'a' - 10)))
  done;
  Bytes.unsafe_to_string b

let rec normalize_value = function
  | List values -> List (List.map normalize_value values)
  | Vector values -> Vector (List.map normalize_value values)
  | Map entries ->
    entries
    |> List.map (fun (key, value) -> normalize_value key, normalize_value value)
    |> List.sort_uniq compare_map_entry
    |> fun entries -> Map entries
  | Set values ->
    values
    |> List.map normalize_value
    |> List.sort_uniq compare_value
    |> fun values -> Set values
  | Tuple values ->
    Tuple (List.map (Option.map normalize_value) values)
  | value -> value

let normalize_datom_value d =
  { d with v = normalize_value d.v }
