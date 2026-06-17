open Datascript_types

let rec list_equal_by equal left right =
  match left, right with
  | [], [] -> true
  | left :: left_rest, right :: right_rest ->
    equal left right && list_equal_by equal left_rest right_rest
  | [], _ :: _ | _ :: _, [] -> false

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
  | Float left, Float right ->
    (classify_float left = FP_nan && classify_float right = FP_nan) || left = right
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
  | Ref_to left, Ref_to right -> entity_ref_equal left right
  | _ -> left = right

let split_keyword keyword =
  match String.index_opt keyword '/' with
  | None -> "", keyword
  | Some index ->
    let namespace = String.sub keyword 0 index in
    let name = String.sub keyword (index + 1) (String.length keyword - index - 1) in
    namespace, name

let rec compare_list_items_with compare_item left right =
  match left, right with
  | [], [] -> 0
  | left :: left_rest, right :: right_rest ->
    let comparison = compare_item left right in
    if comparison <> 0 then comparison else compare_list_items_with compare_item left_rest right_rest
  | [], _ | _, [] -> 0

let compare_list_with compare_item left right =
  let length_comparison = compare (List.length left) (List.length right) in
  if length_comparison <> 0 then length_comparison
  else compare_list_items_with compare_item left right

let compare_option_with compare_item left right =
  match left, right with
  | None, None -> 0
  | None, Some _ -> -1
  | Some _, None -> 1
  | Some left, Some right -> compare_item left right

let value_type_rank = function
  | Nil -> 0
  | Keyword _ -> 1
  | Symbol _ -> 2
  | Map _ -> 3
  | Set _ -> 4
  | List _ -> 5
  | Vector _ -> 6
  | Tuple _ -> 7
  | Bool _ -> 8
  | Int _ | Float _ | Ref _ -> 9
  | String _ -> 10
  | Regex _ -> 11
  | Instant _ -> 12
  | Uuid _ -> 13
  | TxRef -> 14
  | Ref_to _ -> 15

let rec compare_value left right =
  match left, right with
  | Int left, Int right -> compare left right
  | Float left, Float right -> compare left right
  | Int left, Float right -> compare (float_of_int left) right
  | Float left, Int right -> compare left (float_of_int right)
  | Ref left, Ref right -> compare left right
  | Int left, Ref right -> compare left right
  | Ref left, Int right -> compare left right
  | Float left, Ref right -> compare left (float_of_int right)
  | Ref left, Float right -> compare (float_of_int left) right
  | String left, String right -> compare left right
  | Symbol left, Symbol right -> compare (split_keyword left) (split_keyword right)
  | Bool left, Bool right -> compare left right
  | Uuid left, Uuid right -> compare left right
  | Instant left, Instant right -> compare left right
  | Regex left, Regex right -> compare left right
  | Nil, Nil -> 0
  | Keyword left, Keyword right -> compare (split_keyword left) (split_keyword right)
  | List left, List right -> compare_list_with compare_value left right
  | Vector left, Vector right -> compare_list_with compare_value left right
  | List left, Tuple right ->
    compare_list_with (compare_option_with compare_value) (List.map (fun value -> Some value) left) right
  | Set left, Set right -> compare_list_with compare_value left right
  | Map left, Map right -> compare_list_with compare_map_entry left right
  | Tuple left, Tuple right -> compare_list_with (compare_option_with compare_value) left right
  | Tuple left, List right ->
    compare_list_with (compare_option_with compare_value) left (List.map (fun value -> Some value) right)
  | _ ->
    let rank_comparison = compare (value_type_rank left) (value_type_rank right) in
    if rank_comparison <> 0 then rank_comparison else compare left right

and compare_map_entry (left_key, left_value) (right_key, right_value) =
  let comparison = compare_value left_key right_key in
  if comparison <> 0 then comparison else compare_value left_value right_value

let first_nonzero comparisons =
  List.find_opt (( <> ) 0) comparisons
  |> Option.value ~default:0

let compare_datom index left right =
  match index with
  | Eavt ->
    first_nonzero
      [ compare left.e right.e
      ; compare left.a right.a
      ; compare_value left.v right.v
      ; compare left.tx right.tx
      ]
  | Aevt ->
    first_nonzero
      [ compare left.a right.a
      ; compare left.e right.e
      ; compare_value left.v right.v
      ; compare left.tx right.tx
      ]
  | Avet ->
    first_nonzero
      [ compare left.a right.a
      ; compare_value left.v right.v
      ; compare left.e right.e
      ; compare left.tx right.tx
      ]
  | Vaet ->
    first_nonzero
      [ compare_value left.v right.v
      ; compare left.a right.a
      ; compare left.e right.e
      ; compare left.tx right.tx
      ]

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
