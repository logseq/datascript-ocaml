open Datascript_types

type context =
  { empty_db : unit -> db
  ; q_sources :
      ?inputs:query_arg list ->
      db ->
      (string * query_source) list ->
      query ->
      query_result list list
  ; q_with :
      ?inputs:query_arg list ->
      db ->
      string list ->
      query ->
      query_result list list
  ; parse_query_string_with_pull_context :
      ?default_pull_db:db ->
      ?pull_db_for_source:(string -> db) ->
      string ->
      query
  ; parse_query_return_string_with_pull_context :
      ?default_pull_db:db ->
      ?pull_db_for_source:(string -> db) ->
      string ->
      query_return * query
  ; parse_query_return_map_string_with_pull_context :
      ?default_pull_db:db ->
      ?pull_db_for_source:(string -> db) ->
      string ->
      query_return * query_return_map option * query
  ; compare_value : value -> value -> int
  }

let q_sources context ?inputs db sources query =
  context.q_sources ?inputs db sources query

let q context ?inputs db query =
  q_sources context ?inputs db [] query

let q_string context ?inputs db input =
  q context ?inputs db (context.parse_query_string_with_pull_context ~default_pull_db:db input)

let q_with context ?inputs db with_vars query =
  context.q_with ?inputs db with_vars query

let q_with_string context ?inputs db with_vars input =
  q_with context ?inputs db with_vars (context.parse_query_string_with_pull_context ~default_pull_db:db input)

let q_sources_string context ?inputs db sources input =
  let pull_db_for_source source =
    match List.assoc_opt source sources with
    | Some (Db_source source_db) -> source_db
    | Some (Relation_source _) -> context.empty_db ()
    | None when source = "$" -> db
    | None -> context.empty_db ()
  in
  let default_pull_db = pull_db_for_source "$" in
  q_sources
    context
    ?inputs
    db
    sources
    (context.parse_query_string_with_pull_context ~default_pull_db ~pull_db_for_source input)

let q_return context ?inputs db return query =
  let rows = q context ?inputs db query in
  match return with
  | Return_relation -> Query_relation rows
  | Return_collection ->
    rows
    |> List.filter_map (function
      | value :: _ -> Some value
      | [] -> None)
    |> List.sort_uniq compare
    |> fun values -> Query_collection values
  | Return_tuple -> Query_tuple (List.nth_opt rows 0)
  | Return_scalar ->
    let value =
      Option.bind
        (List.nth_opt rows 0)
        (function
          | value :: _ -> Some value
          | [] -> None)
    in
    Query_scalar value

let q_return_string context ?inputs db input =
  let return, query = context.parse_query_return_string_with_pull_context ~default_pull_db:db input in
  q_return context ?inputs db return query

let labels_of_return_map = function
  | Return_keys labels -> List.map (fun label -> Keyword label) labels
  | Return_syms labels -> List.map (fun label -> Symbol label) labels
  | Return_strs labels -> List.map (fun label -> String label) labels

let map_query_row context labels row =
  if List.length labels <> List.length row then
    invalid_arg "return map labels must match find count";
  List.combine labels row |> List.sort (fun (left, _) (right, _) -> context.compare_value left right)

let q_return_map context ?inputs db return return_map query =
  let labels = labels_of_return_map return_map in
  let rows = q context ?inputs db query in
  match return with
  | Return_relation ->
    rows
    |> List.map (map_query_row context labels)
    |> fun rows -> Query_relation_maps rows
  | Return_tuple ->
    List.nth_opt rows 0
    |> Option.map (map_query_row context labels)
    |> fun row -> Query_tuple_map row
  | Return_collection | Return_scalar ->
    invalid_arg "return maps require relation or tuple query returns"

let q_return_map_string context ?inputs db input =
  let return, return_map, query =
    context.parse_query_return_map_string_with_pull_context ~default_pull_db:db input
  in
  match return_map with
  | Some return_map -> q_return_map context ?inputs db return return_map query
  | None -> q_return context ?inputs db return query
