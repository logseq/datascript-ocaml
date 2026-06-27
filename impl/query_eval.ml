open Datascript_types
open Query

type evaluator_context =
  { result_resolution_context : db -> result_resolution_context
  ; match_context : db -> match_context
  ; datoms : db -> index -> ?e:entity_id -> ?a:attr -> ?v:value -> ?tx:tx -> unit -> datom Seq.t
  ; is_reverse_ref : attr -> bool
  ; reverse_ref : attr -> attr
  ; compare_value : value -> value -> int
  ; split_keyword : string -> string * string
  ; normalize_value : value -> value
  }

let attr_value_for_query context db entity_id attr =
  if context.is_reverse_ref attr then
    let forward_attr = context.reverse_ref attr in
    context.datoms db Eavt ()
    |> Seq.find_map (fun d ->
      if d.a = forward_attr && d.v = Ref entity_id then Some (Ref d.e) else None)
  else
    context.datoms db Eavt ~e:entity_id ~a:attr ()
    |> Seq.find_map (fun d -> Some d.v)

let attr_present_for_query context db entity_id attr =
  Option.is_some (attr_value_for_query context db entity_id attr)

let eval_attr_term context db bindings attr_term =
  match eval_query_term (context.match_context db) bindings attr_term with
  | Some (Result_attr attr) -> Some attr
  | Some (Result_value (Keyword attr | String attr | Symbol attr)) -> Some attr
  | Some _ -> invalid_arg "query attribute must resolve to an attribute"
  | None -> None

let eval_missing_clause context clause_db bindings entity_term attr_term =
  match query_term_entity_id (context.match_context clause_db) bindings entity_term, eval_attr_term context clause_db bindings attr_term with
  | Some entity_id, Some attr when not (attr_present_for_query context clause_db entity_id attr) -> [ bindings ]
  | _ -> []

let eval_get_else_clause context clause_db bindings entity_term attr_term default_term output_var =
  let default =
    match eval_query_term (context.match_context clause_db) bindings default_term with
    | Some (Result_value value) -> value
    | Some _ -> invalid_arg "get-else default must resolve to a value"
    | None -> invalid_arg "insufficient bindings"
  in
  if default = Nil then invalid_arg "get-else: nil default value is not supported";
  match query_term_entity_id (context.match_context clause_db) bindings entity_term, eval_attr_term context clause_db bindings attr_term with
  | Some entity_id, Some attr ->
    let value = Option.value (attr_value_for_query context clause_db entity_id attr) ~default in
    (match bind_var (context.result_resolution_context clause_db) output_var (Result_value value) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | _ -> []

let eval_get_some_clause context clause_db bindings entity_term attr_terms attr_var value_var =
  match query_term_entity_id (context.match_context clause_db) bindings entity_term with
  | None -> []
  | Some entity_id ->
    attr_terms
    |> List.filter_map (eval_attr_term context clause_db bindings)
    |> List.find_map (fun attr ->
      Option.map (fun value -> attr, value) (attr_value_for_query context clause_db entity_id attr))
    |> (function
      | None -> []
      | Some (attr, value) ->
        (match bind_var (context.result_resolution_context clause_db) attr_var (Result_attr attr) bindings with
         | None -> []
         | Some bindings ->
           (match bind_var (context.result_resolution_context clause_db) value_var (Result_value value) bindings with
            | Some bindings -> [ bindings ]
            | None -> [])))

let eval_ground_tuple context db bindings values output_vars =
  if List.length values <> List.length output_vars then
    invalid_arg "ground tuple arity mismatch";
  List.fold_left2
    (fun binding value output_var ->
      match binding, output_var with
      | None, _ -> None
      | Some binding, "_" -> Some binding
      | Some binding, output_var -> bind_var (context.result_resolution_context db) output_var (Result_value value) binding)
    (Some bindings)
    values
    output_vars
  |> (function
    | Some bindings -> [ bindings ]
    | None -> [])

let eval_ground_result context db bindings result output_var =
  match output_var with
  | "_" -> [ bindings ]
  | _ ->
    (match bind_var (context.result_resolution_context db) output_var result bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let value_of_query_result = function
  | Result_value value -> Some value
  | Result_entity entity_id -> Some (Ref entity_id)
  | Result_attr attr -> Some (Keyword attr)
  | Result_db _ | Result_pull _ -> None

let collect_query_values context db bindings terms =
  let ( let* ) = Option.bind in
  let* results = collect_query_terms (context.match_context db) bindings terms in
  let rec collect acc = function
    | [] -> Some (List.rev acc)
    | result :: rest ->
      let* value = value_of_query_result result in
      collect (value :: acc) rest
  in
  collect [] results

let value_get = Built_ins.value_get

let bind_get_value context db bindings output_var value =
  match bind_var (context.result_resolution_context db) output_var (result_of_ref (Result_value value)) bindings with
  | Some bindings -> [ bindings ]
  | None -> []

let eval_get_value_clause context db bindings map_term key_term output_var =
  match collect_query_terms_exn (context.match_context db) bindings [ map_term; key_term ] with
  | [ Result_value collection; key_result ] ->
    (match Option.bind (value_of_query_result key_result) (value_get collection) with
     | None -> []
     | Some value -> bind_get_value context db bindings output_var value)
  | _ -> []

let eval_get_default_value_clause context db bindings map_term key_term default_term output_var =
  match collect_query_values context db bindings [ map_term; key_term; default_term ] with
  | Some [ collection; key; default ] ->
    let value =
      match value_get collection key with
      | Some value -> value
      | None -> default
    in
    bind_get_value context db bindings output_var value
  | Some _ | None -> []

let value_count = Built_ins.value_count

let eval_count_value_clause context db bindings term output_var =
  match eval_query_term (context.match_context db) bindings term with
  | Some (Result_value value) ->
    (match value_count value with
     | None -> []
     | Some count ->
       (match bind_var (context.result_resolution_context db) output_var (Result_value (Int count)) bindings with
        | Some bindings -> [ bindings ]
        | None -> []))
  | Some (Result_entity _) | Some (Result_attr _) | Some (Result_db _) | Some (Result_pull _) | None -> []

let value_has_count = Built_ins.value_has_count

let value_is_not_empty = Built_ins.value_is_not_empty

let eval_value_predicate_clause context db bindings term predicate =
  match eval_query_term (context.match_context db) bindings term with
  | Some (Result_value value) when predicate value -> [ bindings ]
  | Some _ | None -> []

let matches_value_predicate = Built_ins.matches_value_predicate

let eval_type_predicate_clause context db bindings predicate term =
  eval_value_predicate_clause context db bindings term (matches_value_predicate predicate)

let matches_numeric_predicate = Built_ins.matches_numeric_predicate

let eval_numeric_predicate_clause context db bindings predicate term =
  eval_value_predicate_clause context db bindings term (matches_numeric_predicate predicate)

let matches_comparison_predicate = Built_ins.matches_comparison_predicate

let eval_comparison_predicate_clause context db bindings predicate left_term right_term =
  match collect_query_values context db bindings [ left_term; right_term ] with
  | Some [ left; right ] when matches_comparison_predicate predicate (context.compare_value left right) -> [ bindings ]
  | Some _ | None -> []

let comparison_chain_matches = Built_ins.comparison_chain_matches

let eval_comparison_predicate_n_clause context db bindings predicate terms =
  match collect_query_values context db bindings terms with
  | Some values when comparison_chain_matches predicate values -> [ bindings ]
  | Some _ | None -> []

let all_values_equal = Built_ins.all_values_equal

let eval_equality_predicate_clause context db bindings predicate terms =
  match collect_query_values context db bindings terms with
  | None -> []
  | Some values ->
    let equal = all_values_equal values in
    let matches =
      match predicate with
      | EqualValues -> equal
      | NotEqualValues -> not equal
    in
    if matches then [ bindings ] else []

let eval_arithmetic = Built_ins.eval_arithmetic

let eval_arithmetic_clause context db bindings op terms output_var =
  match collect_query_values context db bindings terms with
  | None -> []
  | Some values ->
    (match eval_arithmetic op values with
     | None -> []
     | Some value ->
       (match bind_var (context.result_resolution_context db) output_var (Result_value value) bindings with
        | Some bindings -> [ bindings ]
        | None -> []))

let normalized_comparison = Built_ins.normalized_comparison

let eval_compare_value_clause context db bindings left_term right_term output_var =
  match collect_query_values context db bindings [ left_term; right_term ] with
  | Some [ left; right ] ->
    (match bind_var (context.result_resolution_context db) output_var (Result_value (Int (normalized_comparison (context.compare_value left right)))) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let extremum_value = Built_ins.extremum_value

let eval_extremum_value_clause context db bindings op terms output_var =
  match collect_query_values context db bindings terms with
  | None -> []
  | Some [] -> invalid_arg "min/max expects at least one value"
  | Some (first :: rest) ->
    (match bind_var (context.result_resolution_context db) output_var (Result_value (extremum_value op first rest)) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let matches_boolean_predicate predicate result =
  match predicate, result with
  | TrueValue, Result_value (Bool true) -> true
  | FalseValue, Result_value (Bool false) -> true
  | NilValue, Result_value Nil -> true
  | SomeValue, Result_value Nil -> false
  | SomeValue, (Result_value _ | Result_entity _ | Result_attr _ | Result_db _) -> true
  | _ -> false

let eval_boolean_predicate_clause context db bindings predicate term =
  match eval_query_term (context.match_context db) bindings term with
  | Some result when matches_boolean_predicate predicate result -> [ bindings ]
  | Some _ | None -> []

let value_is_truthy = Built_ins.value_is_truthy

let query_result_is_truthy = function
  | Result_value value -> value_is_truthy value
  | Result_entity _ | Result_attr _ | Result_db _ | Result_pull _ -> true

let eval_boolean_not_predicate_clause context db bindings term =
  match eval_query_term (context.match_context db) bindings term with
  | Some result when not (query_result_is_truthy result) -> [ bindings ]
  | Some _ | None -> []

let eval_boolean_not_clause context db bindings term output_var =
  match eval_query_term (context.match_context db) bindings term with
  | Some result ->
    (match bind_var (context.result_resolution_context db) output_var (Result_value (Bool (not (query_result_is_truthy result)))) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | None -> []

let eval_identity_value_clause context db bindings term output_var =
  match eval_query_term (context.match_context db) bindings term with
  | Some result ->
    (match bind_var (context.result_resolution_context db) output_var result bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | None -> []

let eval_boolean_and_predicate_clause context db bindings terms =
  match collect_query_terms (context.match_context db) bindings terms with
  | Some results when List.for_all query_result_is_truthy results -> [ bindings ]
  | Some _ | None -> []

let boolean_and_value = Built_ins.boolean_and_value

let eval_boolean_and_clause context db bindings terms output_var =
  match collect_query_values context db bindings terms with
  | None -> []
  | Some values ->
    (match bind_var (context.result_resolution_context db) output_var (Result_value (boolean_and_value values)) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let eval_boolean_or_predicate_clause context db bindings terms =
  match collect_query_terms (context.match_context db) bindings terms with
  | Some results when List.exists query_result_is_truthy results -> [ bindings ]
  | Some _ | None -> []

let boolean_or_value = Built_ins.boolean_or_value

let eval_boolean_or_clause context db bindings terms output_var =
  match collect_query_values context db bindings terms with
  | None -> []
  | Some values ->
    (match bind_var (context.result_resolution_context db) output_var (Result_value (boolean_or_value values)) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let eval_random_value_clause context db bindings output_var =
  match bind_var (context.result_resolution_context db) output_var (Result_value (Float (Random.float 1.0))) bindings with
  | Some bindings -> [ bindings ]
  | None -> []

let eval_random_int_value_clause context db bindings bound_term output_var =
  match eval_query_term (context.match_context db) bindings bound_term with
  | Some (Result_value (Int bound)) when bound > 0 ->
    (match bind_var (context.result_resolution_context db) output_var (Result_value (Int (Random.int bound))) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_value (Int _)) -> invalid_arg "rand-int bound must be positive"
  | Some _ | None -> []

let split_at = Built_ins.split_at

let values_equal = Built_ins.values_equal

let eval_differ_predicate_clause context db bindings terms =
  match collect_query_values context db bindings terms with
  | None -> []
  | Some values ->
    let left, right = split_at (List.length values / 2) values in
    if not (List.length left = List.length right && List.for_all2 values_equal left right) then
      [ bindings ]
    else
      []

let eval_identical_predicate_clause context db bindings left_term right_term =
  match collect_query_values context db bindings [ left_term; right_term ] with
  | Some [ left; right ] when values_equal left right -> [ bindings ]
  | Some _ | None -> []

let type_keyword_of_value = Built_ins.type_keyword_of_value

let eval_type_value_clause context db bindings term output_var =
  match eval_query_term (context.match_context db) bindings term with
  | Some (Result_value value) ->
    (match bind_var (context.result_resolution_context db) output_var (Result_value (Keyword (type_keyword_of_value value))) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_entity _) ->
    (match bind_var (context.result_resolution_context db) output_var (Result_value (Keyword "type/entity")) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_attr _) ->
    (match bind_var (context.result_resolution_context db) output_var (Result_value (Keyword "type/attr")) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_db _) | Some (Result_pull _) | None -> []

let eval_meta_value_clause context db bindings term output_var =
  match eval_query_term (context.match_context db) bindings term with
  | Some _ ->
    (match bind_var (context.result_resolution_context db) output_var (Result_value Nil) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | None -> []

let bind_string_value context db output_var value bindings =
  bind_var (context.result_resolution_context db) output_var (Result_value (String value)) bindings

let bind_keyword_value context db output_var value bindings =
  bind_var (context.result_resolution_context db) output_var (Result_value (Keyword value)) bindings

let eval_name_value_clause context db bindings term output_var =
  match eval_query_term (context.match_context db) bindings term with
  | Some (Result_value (Keyword keyword)) ->
    let _, name = context.split_keyword keyword in
    (match bind_string_value context db output_var name bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_attr attr) ->
    let _, name = context.split_keyword attr in
    (match bind_string_value context db output_var name bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_value (String value)) ->
    (match bind_string_value context db output_var value bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let eval_namespace_value_clause context db bindings term output_var =
  match eval_query_term (context.match_context db) bindings term with
  | Some (Result_value (Keyword keyword)) ->
    let namespace, _ = context.split_keyword keyword in
    if namespace = "" then
      []
    else
      (match bind_string_value context db output_var namespace bindings with
       | Some bindings -> [ bindings ]
       | None -> [])
  | Some (Result_attr attr) ->
    let namespace, _ = context.split_keyword attr in
    if namespace = "" then
      []
    else
      (match bind_string_value context db output_var namespace bindings with
       | Some bindings -> [ bindings ]
       | None -> [])
  | Some _ | None -> []

let eval_keyword_from_name_clause context db bindings term output_var =
  match eval_query_term (context.match_context db) bindings term with
  | Some (Result_value (String value)) ->
    (match bind_keyword_value context db output_var value bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some (Result_value (Keyword keyword)) | Some (Result_attr keyword) ->
    (match bind_keyword_value context db output_var keyword bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let eval_keyword_from_namespace_name_clause context db bindings namespace_term name_term output_var =
  match collect_query_terms (context.match_context db) bindings [ namespace_term; name_term ] with
  | Some [ Result_value (String namespace); Result_value (String name) ] ->
    (match bind_keyword_value context db output_var (namespace ^ "/" ^ name) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let string_starts_with = Built_ins.string_starts_with
let string_ends_with = Built_ins.string_ends_with
let string_index_of = Built_ins.string_index_of
let string_includes = Built_ins.string_includes
let string_last_index_of = Built_ins.string_last_index_of

let eval_string_predicate_clause context db bindings left_term right_term predicate =
  match collect_query_terms (context.match_context db) bindings [ left_term; right_term ] with
  | Some [ Result_value (String left); Result_value (String right) ] when predicate left right -> [ bindings ]
  | Some _ | None -> []

let eval_string_index_clause context db bindings value_term needle_term output_var index_of =
  match collect_query_terms (context.match_context db) bindings [ value_term; needle_term ] with
  | Some [ Result_value (String value); Result_value (String needle) ] ->
    (match index_of value needle with
     | None -> []
     | Some index ->
       (match bind_var (context.result_resolution_context db) output_var (Result_value (Int index)) bindings with
        | Some bindings -> [ bindings ]
        | None -> []))
  | Some _ | None -> []

let query_result_int = function
  | Result_value (Int value) -> Some value
  | Result_value _ | Result_entity _ | Result_attr _ | Result_db _ | Result_pull _ -> None

let eval_string_substring_clause context db bindings value_term start_term end_term output_var =
  let terms = value_term :: start_term :: Option.to_list end_term in
  match collect_query_terms (context.match_context db) bindings terms with
  | Some (Result_value (String value) :: start_result :: rest) ->
    (match query_result_int start_result, rest with
     | Some start_index, [] ->
       if start_index < 0 || start_index > String.length value then
         invalid_arg "substring index out of bounds";
       (match bind_string_value context db output_var (String.sub value start_index (String.length value - start_index)) bindings with
        | Some bindings -> [ bindings ]
        | None -> [])
     | Some start_index, [ end_result ] ->
       (match query_result_int end_result with
        | None -> invalid_arg "substring indexes must be integers"
        | Some end_index ->
          if start_index < 0 || end_index < start_index || end_index > String.length value then
            invalid_arg "substring index out of bounds";
          (match bind_string_value context db output_var (String.sub value start_index (end_index - start_index)) bindings with
           | Some bindings -> [ bindings ]
           | None -> []))
     | _ -> invalid_arg "substring indexes must be integers")
  | Some _ | None -> []

let string_of_query_value = Built_ins.string_of_query_value
let print_query_values = Built_ins.print_query_values

let eval_print_string_clause context db bindings terms output_var ~readably ~newline =
  match collect_query_values context db bindings terms with
  | None -> []
  | Some values ->
    let printed = print_query_values ~readably values ^ (if newline then "\n" else "") in
    (match bind_string_value context db output_var printed bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let eval_string_build_clause context db bindings terms output_var =
  match collect_query_values context db bindings terms with
  | None -> []
  | Some values ->
    (match bind_string_value context db output_var (values |> List.map string_of_query_value |> String.concat "") bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let collection_string_values = Built_ins.collection_string_values

let eval_string_join_clause context db bindings separator_term collection_term output_var =
  match collect_query_terms (context.match_context db) bindings [ separator_term; collection_term ] with
  | Some [ Result_value (String separator); Result_value collection ] ->
    (match collection_string_values collection with
     | None -> []
     | Some values ->
       (match bind_string_value context db output_var (String.concat separator values) bindings with
        | Some bindings -> [ bindings ]
        | None -> []))
  | Some _ | None -> []

let eval_string_join_plain_clause context db bindings collection_term output_var =
  match eval_query_term (context.match_context db) bindings collection_term with
  | Some (Result_value collection) ->
    (match collection_string_values collection with
     | None -> []
     | Some values ->
       (match bind_string_value context db output_var (String.concat "" values) bindings with
        | Some bindings -> [ bindings ]
        | None -> []))
  | Some _ | None -> []

let replace_string = Built_ins.replace_string
let replace_regex = Built_ins.replace_regex

let eval_string_replace_clause context db bindings value_term pattern_term replacement_term output_var first_only =
  match collect_query_terms (context.match_context db) bindings [ value_term; pattern_term; replacement_term ] with
  | Some [ Result_value (String value); Result_value (String pattern); Result_value (String replacement) ] ->
    (match bind_string_value context db output_var (replace_string ~first_only value pattern replacement) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some [ Result_value (String value); Result_value (Regex pattern); Result_value (String replacement) ] ->
    (match bind_string_value context db output_var (replace_regex ~first_only value pattern replacement) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let escape_string = Built_ins.escape_string

let eval_string_escape_clause context db bindings value_term replacement_term output_var =
  match collect_query_terms (context.match_context db) bindings [ value_term; replacement_term ] with
  | Some [ Result_value (String value); Result_value (Map replacements) ] ->
    (match bind_string_value context db output_var (escape_string value replacements) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let regex_pattern_of_result = Built_ins.regex_pattern_of_result
let regex_find = Built_ins.regex_find
let regex_matches = Built_ins.regex_matches
let regex_seq = Built_ins.regex_seq

let eval_re_pattern_value_clause context db bindings pattern_term output_var =
  match eval_query_term (context.match_context db) bindings pattern_term with
  | Some (Result_value (String pattern)) | Some (Result_value (Regex pattern)) ->
    (match bind_var (context.result_resolution_context db) output_var (Result_value (Regex pattern)) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let eval_regex_string_clause context db bindings pattern_term value_term output_var f =
  match collect_query_terms (context.match_context db) bindings [ pattern_term; value_term ] with
  | Some [ pattern_result; Result_value (String value) ] ->
    (match Option.bind (regex_pattern_of_result pattern_result) (fun pattern -> f pattern value) with
     | None -> []
     | Some matched ->
       (match bind_string_value context db output_var matched bindings with
        | Some bindings -> [ bindings ]
        | None -> []))
  | Some _ | None -> []

let eval_regex_predicate_clause context db bindings pattern_term value_term f =
  match collect_query_terms (context.match_context db) bindings [ pattern_term; value_term ] with
  | Some [ pattern_result; Result_value (String value) ] ->
    (match Option.bind (regex_pattern_of_result pattern_result) (fun pattern -> f pattern value) with
     | Some _ -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let eval_re_seq_value_clause context db bindings pattern_term value_term output_var =
  match collect_query_terms (context.match_context db) bindings [ pattern_term; value_term ] with
  | Some [ pattern_result; Result_value (String value) ] ->
    (match regex_pattern_of_result pattern_result with
     | None -> []
     | Some pattern ->
       (match regex_seq pattern value with
        | [] -> []
        | matches ->
          let values = List.map (fun value -> String value) matches in
          (match bind_var (context.result_resolution_context db) output_var (Result_value (List values)) bindings with
           | Some bindings -> [ bindings ]
           | None -> [])))
  | Some _ | None -> []

let string_is_blank = Built_ins.string_is_blank

let eval_string_blank_clause context db bindings term =
  match eval_query_term (context.match_context db) bindings term with
  | Some (Result_value (String value)) when string_is_blank value -> [ bindings ]
  | Some _ | None -> []

let split_string = Built_ins.split_string
let split_string_limited = Built_ins.split_string_limited

let split_regex = Built_ins.split_regex
let split_regex_limited = Built_ins.split_regex_limited

let is_ascii_whitespace = Built_ins.is_ascii_whitespace
let split_lines = Built_ins.split_lines

let bind_string_list context db output_var values bindings =
  bind_var (context.result_resolution_context db) output_var (Result_value (List (List.map (fun value -> String value) values))) bindings

let eval_string_split_clause context db bindings value_term separator_term output_var =
  match collect_query_terms (context.match_context db) bindings [ value_term; separator_term ] with
  | Some [ Result_value (String value); Result_value (String separator) ] ->
    (match bind_string_list context db output_var (split_string value separator) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some [ Result_value (String value); Result_value (Regex pattern) ] ->
    (match bind_string_list context db output_var (split_regex value pattern) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let eval_string_split_limit_clause context db bindings value_term separator_term limit_term output_var =
  match collect_query_terms (context.match_context db) bindings [ value_term; separator_term; limit_term ] with
  | Some [ Result_value (String value); Result_value (String separator); Result_value (Int limit) ] ->
    (match bind_string_list context db output_var (split_string_limited value separator limit) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some [ Result_value (String value); Result_value (Regex pattern); Result_value (Int limit) ] ->
    (match bind_string_list context db output_var (split_regex_limited value pattern limit) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let eval_string_split_lines_clause context db bindings value_term output_var =
  match eval_query_term (context.match_context db) bindings value_term with
  | Some (Result_value (String value)) ->
    (match bind_string_list context db output_var (split_lines value) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let reverse_string = Built_ins.reverse_string
let capitalize_string = Built_ins.capitalize_string
let trim_left_with = Built_ins.trim_left_with
let trim_right_with = Built_ins.trim_right_with
let trim_with = Built_ins.trim_with
let is_newline = Built_ins.is_newline

let eval_string_transform_clause context db bindings term output_var transform =
  match eval_query_term (context.match_context db) bindings term with
  | Some (Result_value (String value)) ->
    (match bind_string_value context db output_var (transform value) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])
  | Some _ | None -> []

let value_contains = Built_ins.value_contains

let eval_contains_value_clause context db bindings collection_term key_term =
  match collect_query_terms (context.match_context db) bindings [ collection_term; key_term ] with
  | Some [ Result_value collection; key_result ] ->
    (match value_of_query_result key_result with
     | Some key when value_contains collection key -> [ bindings ]
     | Some _ | None -> [])
  | Some _ | None -> []

let eval_tuple_function context db bindings terms output_var =
  match collect_query_values context db bindings terms with
  | None -> []
  | Some values ->
    let tuple = Tuple (List.map (fun value -> Some value) values) in
    (match bind_var (context.result_resolution_context db) output_var (Result_value tuple) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let eval_collection_value_clause context db bindings terms output_var make_value =
  match collect_query_values context db bindings terms with
  | None -> []
  | Some values ->
    (match bind_var (context.result_resolution_context db) output_var (Result_value (make_value values)) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let eval_hash_map_value_clause context db bindings terms output_var =
  if List.length terms mod 2 <> 0 then
    invalid_arg "hash-map arity mismatch";
  match collect_query_values context db bindings terms with
  | None -> []
  | Some values ->
    let rec pairs acc = function
      | [] -> List.rev acc
      | key :: value :: rest -> pairs ((key, value) :: acc) rest
      | [ _ ] -> invalid_arg "hash-map arity mismatch"
    in
    let map = context.normalize_value (Map (pairs [] values)) in
    (match bind_var (context.result_resolution_context db) output_var (Result_value map) bindings with
     | Some bindings -> [ bindings ]
     | None -> [])

let range_values = Built_ins.range_values

let eval_range_values context db bindings output_var start_value end_value step =
  range_values start_value end_value step
  |> List.filter_map (fun value -> bind_var (context.result_resolution_context db) output_var (Result_value (Int value)) bindings)

let eval_range_end_value_clause context db bindings end_term output_var =
  match collect_query_terms (context.match_context db) bindings [ end_term ] with
  | None -> []
  | Some [ Result_value (Int end_value) ] -> eval_range_values context db bindings output_var 0 end_value 1
  | Some _ -> invalid_arg "range requires integer bounds"

let eval_range_value_clause context db bindings start_term end_term output_var =
  match collect_query_terms (context.match_context db) bindings [ start_term; end_term ] with
  | None -> []
  | Some [ Result_value (Int start_value); Result_value (Int end_value) ] ->
    eval_range_values context db bindings output_var start_value end_value 1
  | Some _ -> invalid_arg "range requires integer bounds"

let eval_range_step_value_clause context db bindings start_term end_term step_term output_var =
  match collect_query_terms (context.match_context db) bindings [ start_term; end_term; step_term ] with
  | None -> []
  | Some [ Result_value (Int start_value); Result_value (Int end_value); Result_value (Int step) ] ->
    eval_range_values context db bindings output_var start_value end_value step
  | Some _ -> invalid_arg "range requires integer bounds"

let eval_untuple_values context db bindings output_vars values =
  if List.length values <> List.length output_vars then
    invalid_arg "untuple arity mismatch";
  List.fold_left2
    (fun binding output_var value ->
      match binding, output_var, value with
      | None, _, _ | _, _, None -> None
      | Some binding, "_", Some _ -> Some binding
      | Some binding, output_var, Some value -> bind_var (context.result_resolution_context db) output_var (result_of_ref (Result_value value)) binding)
    (Some bindings)
    output_vars
    values
  |> (function
    | Some bindings -> [ bindings ]
    | None -> [])

let eval_untuple_function context db bindings tuple_term output_vars =
  match eval_query_term (context.match_context db) bindings tuple_term with
  | Some (Result_value (Tuple values)) -> eval_untuple_values context db bindings output_vars values
  | Some (Result_value (List values | Vector values)) ->
    eval_untuple_values context db bindings output_vars (List.map (fun value -> Some value) values)
  | Some _ | None -> []
