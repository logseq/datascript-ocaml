open Datascript_types

let compare_value = Util.compare_value

let map_get_value entries key =
  entries
  |> List.find_map (fun (entry_key, entry_value) ->
    if compare_value entry_key key = 0 then Some entry_value else None)

let value_get collection key =
  match collection, key with
  | Map entries, key -> map_get_value entries key
  | Set values, key ->
    if List.exists (fun value -> compare_value value key = 0) values then Some key else None
  | (List values | Vector values), Int index ->
    if index >= 0 && index < List.length values then Some (List.nth values index) else None
  | Tuple values, Int index ->
    if index >= 0 && index < List.length values then
      match List.nth values index with
      | Some value -> Some value
      | None -> Some Nil
    else
      None
  | _ -> None

let value_count = function
  | String value -> Some (String.length value)
  | List values | Vector values | Set values -> Some (List.length values)
  | Map entries -> Some (List.length entries)
  | Tuple values -> Some (List.length values)
  | Nil | Int _ | Float _ | Bool _ | Keyword _ | Symbol _ | Uuid _ | Instant _ | Regex _ | Ref _ | TxRef | Ref_to _ -> None

let value_has_count expected value =
  match value_count value with
  | Some count -> count = expected
  | None -> false

let value_is_not_empty value =
  match value_count value with
  | Some count -> count > 0
  | None -> false

let matches_value_predicate predicate value =
  match predicate, value with
  | NumberValue, (Int _ | Float _) -> true
  | IntegerValue, Int _ -> true
  | StringValue, String _ -> true
  | BooleanValue, Bool _ -> true
  | KeywordValue, Keyword _ -> true
  | _ -> false

let matches_numeric_predicate predicate value =
  match predicate, value with
  | ZeroNumber, Int value -> value = 0
  | ZeroNumber, Float value -> value = 0.0
  | PositiveNumber, Int value -> value > 0
  | PositiveNumber, Float value -> value > 0.0
  | NegativeNumber, Int value -> value < 0
  | NegativeNumber, Float value -> value < 0.0
  | EvenInteger, Int value -> value mod 2 = 0
  | OddInteger, Int value -> value mod 2 <> 0
  | (EvenInteger | OddInteger), Float _ -> false
  | _, _ -> false

let matches_comparison_predicate predicate comparison =
  match predicate with
  | LessThan -> comparison < 0
  | GreaterThan -> comparison > 0
  | LessOrEqual -> comparison <= 0
  | GreaterOrEqual -> comparison >= 0

let comparison_chain_matches predicate = function
  | [] -> invalid_arg "comparison predicate requires at least one argument"
  | [ _ ] -> true
  | first :: rest ->
    let rec matches left = function
      | [] -> true
      | right :: rest ->
        matches_comparison_predicate predicate (compare_value left right) && matches right rest
    in
    matches first rest

let all_values_equal = function
  | [] | [ _ ] -> true
  | first :: rest -> List.for_all (fun value -> compare_value first value = 0) rest

let value_is_truthy = function
  | Nil | Bool false -> false
  | _ -> true

let boolean_and_value = function
  | [] -> Bool true
  | first :: rest ->
    let rec last_truthy current = function
      | [] -> current
      | value :: rest ->
        if value_is_truthy value then
          last_truthy value rest
        else
          value
    in
    if value_is_truthy first then
      last_truthy first rest
    else
      first

let boolean_or_value = function
  | [] -> Nil
  | first :: rest ->
    let rec first_truthy current = function
      | [] -> current
      | value :: rest ->
        if value_is_truthy current then
          current
        else
          first_truthy value rest
    in
    first_truthy first rest

let split_at count values =
  let rec split index left right =
    if index = 0 then
      List.rev left, right
    else
      match right with
      | [] -> List.rev left, []
      | value :: rest -> split (index - 1) (value :: left) rest
  in
  split count [] values

let values_equal left right =
  compare_value left right = 0

let type_keyword_of_value = function
  | Int _ -> "type/int"
  | Float _ -> "type/float"
  | String _ -> "type/string"
  | Symbol _ -> "type/symbol"
  | Bool _ -> "type/bool"
  | Nil -> "type/nil"
  | Keyword _ -> "type/keyword"
  | Uuid _ -> "type/uuid"
  | Instant _ -> "type/instant"
  | Regex _ -> "type/regex"
  | Ref _ -> "type/ref"
  | List _ -> "type/list"
  | Vector _ -> "type/vector"
  | Map _ -> "type/map"
  | Set _ -> "type/set"
  | Tuple _ -> "type/tuple"
  | TxRef -> "type/tx-ref"
  | Ref_to _ -> "type/ref-to"

let value_contains collection key =
  match collection, key with
  | Map entries, key ->
    List.exists (fun (entry_key, _) -> compare_value entry_key key = 0) entries
  | Set values, key ->
    List.exists (fun value -> compare_value value key = 0) values
  | (List values | Vector values), Int index ->
    index >= 0 && index < List.length values
  | Tuple values, Int index ->
    index >= 0 && index < List.length values
  | _ -> false

let range_values start_value end_value step =
  if step = 0 then invalid_arg "range step cannot be zero";
  let rec collect value acc =
    if (step > 0 && value >= end_value) || (step < 0 && value <= end_value) then
      List.rev acc
    else
      collect (value + step) (value :: acc)
  in
  collect start_value []
