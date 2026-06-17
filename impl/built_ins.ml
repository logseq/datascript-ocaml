open Datascript_types

let compare_value = Util.compare_value

let normalize_value = Util.normalize_value

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

let numeric_value = function
  | Int value -> Some (`Int value)
  | Float value -> Some (`Float value)
  | _ -> None

let numeric_result prefer_float value =
  if prefer_float then
    Float value
  else
    Int (int_of_float value)

let arithmetic_values values =
  let rec collect acc has_float = function
    | [] -> Some (List.rev acc, has_float)
    | value :: rest ->
      (match numeric_value value with
       | None -> None
       | Some (`Int value) -> collect (float_of_int value :: acc) has_float rest
       | Some (`Float value) -> collect (value :: acc) true rest)
  in
  collect [] false values

let integer_pair = function
  | [ Int left; Int right ] -> Some (left, right)
  | _ -> None

let clojure_mod left right =
  let remainder = left mod right in
  if remainder = 0 || (remainder > 0) = (right > 0) then
    remainder
  else
    remainder + right

let eval_arithmetic op values =
  match op, values, arithmetic_values values with
  | QuotientNumbers, _, _ ->
    let left, right =
      match integer_pair values with
      | Some pair -> pair
      | None -> invalid_arg "integer arithmetic expects two integer values"
    in
    Some (Int (left / right))
  | RemainderNumbers, _, _ ->
    let left, right =
      match integer_pair values with
      | Some pair -> pair
      | None -> invalid_arg "integer arithmetic expects two integer values"
    in
    Some (Int (left mod right))
  | ModuloNumbers, _, _ ->
    let left, right =
      match integer_pair values with
      | Some pair -> pair
      | None -> invalid_arg "integer arithmetic expects two integer values"
    in
    Some (Int (clojure_mod left right))
  | _, _, None -> invalid_arg "arithmetic expects numeric values"
  | IncrementNumber, _, Some ([ value ], has_float) -> Some (numeric_result has_float (value +. 1.0))
  | DecrementNumber, _, Some ([ value ], has_float) -> Some (numeric_result has_float (value -. 1.0))
  | (IncrementNumber | DecrementNumber), _, _ -> invalid_arg "unary arithmetic expects one value"
  | AddNumbers, _, Some (values, has_float) ->
    Some (numeric_result has_float (List.fold_left ( +. ) 0.0 values))
  | SubtractNumbers, _, Some ([], _) -> invalid_arg "subtraction expects at least one value"
  | SubtractNumbers, _, Some ([ value ], has_float) -> Some (numeric_result has_float (~-. value))
  | SubtractNumbers, _, Some (first :: rest, has_float) ->
    Some (numeric_result has_float (List.fold_left ( -. ) first rest))
  | MultiplyNumbers, _, Some (values, has_float) ->
    Some (numeric_result has_float (List.fold_left ( *. ) 1.0 values))
  | DivideNumbers, _, Some ([], _) -> invalid_arg "division expects at least one value"
  | DivideNumbers, _, Some ([ value ], _) -> Some (Float (1.0 /. value))
  | DivideNumbers, _, Some (first :: rest, has_float) ->
    let result = List.fold_left ( /. ) first rest in
    let integral = Float.is_integer result in
    Some (numeric_result (has_float || not integral) result)

let normalized_comparison comparison =
  if comparison < 0 then -1 else if comparison > 0 then 1 else 0

let extremum_value op first rest =
  let better =
    match op with
    | MinimumValue -> fun current candidate -> compare_value candidate current < 0
    | MaximumValue -> fun current candidate -> compare_value candidate current > 0
  in
  List.fold_left (fun current candidate -> if better current candidate then candidate else current) first rest

let query_result_value = function
  | Result_value value -> Some value
  | Result_entity entity_id -> Some (Ref entity_id)
  | Result_attr attr -> Some (Keyword attr)
  | Result_db _ | Result_pull _ -> None

let float_of_result = function
  | Result_value (Int value) -> float_of_int value
  | Result_value (Float value) -> value
  | _ -> invalid_arg "aggregate expects numeric values"

let numeric_values values = List.map float_of_result values

let sum_result values =
  let rec sum int_total float_total has_float = function
    | [] ->
      if has_float then Result_value (Float float_total) else Result_value (Int int_total)
    | Result_value (Int value) :: rest ->
      sum (int_total + value) (float_total +. float_of_int value) has_float rest
    | Result_value (Float value) :: rest ->
      sum int_total (float_total +. value) true rest
    | _ -> invalid_arg "aggregate expects numeric values"
  in
  sum 0 0.0 false values

let average values =
  let values = numeric_values values in
  match values with
  | [] -> invalid_arg "aggregate over empty input"
  | values ->
    List.fold_left ( +. ) 0.0 values /. float_of_int (List.length values)

let median values =
  let values = numeric_values values |> List.sort compare in
  match values with
  | [] -> invalid_arg "aggregate over empty input"
  | values ->
    let len = List.length values in
    if len mod 2 = 1 then
      List.nth values (len / 2)
    else
      let upper = List.nth values (len / 2) in
      let lower = List.nth values ((len / 2) - 1) in
      (lower +. upper) /. 2.0

let variance values =
  let values = numeric_values values in
  match values with
  | [] -> invalid_arg "aggregate over empty input"
  | values ->
    let mean = List.fold_left ( +. ) 0.0 values /. float_of_int (List.length values) in
    values
    |> List.map (fun value ->
      let diff = value -. mean in
      diff *. diff)
    |> List.fold_left ( +. ) 0.0
    |> fun sum -> sum /. float_of_int (List.length values)

let rec take n values =
  if n <= 0 then
    []
  else
    match values with
    | [] -> []
    | value :: rest -> value :: take (n - 1) rest

let rec drop n values =
  if n <= 0 then
    values
  else
    match values with
    | [] -> []
    | _ :: rest -> drop (n - 1) rest

let tuple_of_results values =
  Tuple
    (List.map
       (function
         | Result_value value -> Some value
         | Result_entity entity_id -> Some (Ref entity_id)
         | Result_attr attr -> Some (Keyword attr)
         | Result_db _ | Result_pull _ -> None)
       values)

let compare_result_for_aggregate left right =
  match left, right with
  | Result_value left, Result_value right -> compare_value left right
  | _ -> compare left right

let min_result_for_aggregate left right =
  if compare_result_for_aggregate left right <= 0 then left else right

let max_result_for_aggregate left right =
  if compare_result_for_aggregate left right >= 0 then left else right

let random_result values =
  match values with
  | [] -> invalid_arg "aggregate over empty input"
  | values -> List.nth values (Random.int (List.length values))

let random_results amount values =
  List.init amount (fun _ -> random_result values)

let sample_results amount values =
  values
  |> List.map (fun value -> Random.bits (), value)
  |> List.sort compare
  |> List.map snd
  |> take amount

let aggregate_result aggregate values =
  match aggregate, values with
  | Count, values -> Result_value (Int (List.length values))
  | CountDistinct, values -> Result_value (Int (List.length (List.sort_uniq compare values)))
  | Distinct, values ->
    values
    |> List.filter_map query_result_value
    |> fun values -> Result_value (normalize_value (Set values))
  | Sum, values -> sum_result values
  | Avg, _ when values = [] -> invalid_arg "aggregate over empty input"
  | Avg, values -> Result_value (Float (average values))
  | Median, values -> Result_value (Float (median values))
  | Variance, values -> Result_value (Float (variance values))
  | Stddev, values -> Result_value (Float (sqrt (variance values)))
  | Min, first :: rest -> List.fold_left min_result_for_aggregate first rest
  | Max, first :: rest -> List.fold_left max_result_for_aggregate first rest
  | MinN amount, values ->
    values
    |> List.sort compare_result_for_aggregate
    |> take amount
    |> tuple_of_results
    |> fun value -> Result_value value
  | MaxN amount, values ->
    let values = List.sort compare_result_for_aggregate values in
    values
    |> drop (List.length values - amount)
    |> tuple_of_results
    |> fun value -> Result_value value
  | Rand, values -> random_result values
  | RandN amount, values ->
    values
    |> random_results amount
    |> tuple_of_results
    |> fun value -> Result_value value
  | Sample amount, values ->
    values
    |> sample_results amount
    |> tuple_of_results
    |> fun value -> Result_value value
  | (Min | Max), [] -> invalid_arg "aggregate over empty input"
  | (MinNVar _ | MaxNVar _ | RandNVar _ | SampleVar _), _ ->
    invalid_arg "dynamic aggregate amount was not resolved"
  | CustomVar _, _ -> invalid_arg "custom aggregate input was not resolved"
  | Custom f, values -> f values

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
