open Datascript_types

type context =
  { cardinality : db -> attr -> cardinality
  ; is_ref_attr : db -> attr -> bool
  ; is_reverse_ref : attr -> bool
  ; reverse_ref : attr -> attr
  ; query_value_of_form : query_form -> value
  ; read_edn : string -> query_form
  ; split_keyword : string -> string * string
  }

let rec with_pull_default selector default =
  match selector with
  | Pull_attr attr -> Pull_attr_default (attr, default)
  | Pull_attr_xform (attr, f) -> Pull_attr_default_xform (attr, default, f)
  | Pull_ref (attr, pattern) -> Pull_ref_default (attr, pattern, default)
  | Pull_reverse_ref (attr, pattern) -> Pull_reverse_ref_default (attr, pattern, default)
  | Pull_as (selector, alias) -> Pull_as (with_pull_default selector default, alias)
  | _ -> invalid_arg "pull :default applies only to attrs and refs"

let rec with_pull_limit selector limit =
  if limit < 0 then invalid_arg "pull :limit must be non-negative";
  match selector with
  | Pull_attr attr -> Pull_attr_limit (attr, limit)
  | Pull_ref (attr, pattern) -> Pull_ref_limit (attr, pattern, limit)
  | Pull_reverse_ref (attr, pattern) -> Pull_reverse_ref_limit (attr, pattern, limit)
  | Pull_as (selector, alias) -> Pull_as (with_pull_limit selector limit, alias)
  | _ -> invalid_arg "pull :limit applies only to attrs and refs"

let rec with_pull_unlimited selector =
  match selector with
  | Pull_attr attr -> Pull_attr_unlimited attr
  | Pull_ref (attr, pattern) -> Pull_ref_unlimited (attr, pattern)
  | Pull_reverse_ref (attr, pattern) -> Pull_reverse_ref_unlimited (attr, pattern)
  | Pull_as (selector, alias) -> Pull_as (with_pull_unlimited selector, alias)
  | _ -> invalid_arg "pull :limit nil applies only to attrs and refs"

let pull_ref_attr context attr =
  if context.is_reverse_ref attr then context.reverse_ref attr else attr

let validate_pull_ref_attr context db attr =
  let name = pull_ref_attr context attr in
  if not (context.is_ref_attr db name) then
    invalid_arg ("pull map spec requires ref attr: " ^ name)

let validate_pull_attr_name context db attr =
  if context.is_reverse_ref attr then validate_pull_ref_attr context db attr;
  attr

let validate_pull_string_attr_name context db attr =
  match attr with
  | ":db/id" -> "db/id"
  | "limit" | "default" -> invalid_arg ("reserved pull string attr name: " ^ attr)
  | _ -> validate_pull_attr_name context db attr

let rec pull_limit_attr context = function
  | Pull_attr attr
  | Pull_attr_default (attr, _)
  | Pull_attr_limit (attr, _)
  | Pull_attr_unlimited attr
  | Pull_attr_xform (attr, _)
  | Pull_attr_default_xform (attr, _, _) ->
    Some (pull_ref_attr context attr)
  | Pull_ref (attr, _)
  | Pull_ref_default (attr, _, _)
  | Pull_ref_limit (attr, _, _)
  | Pull_ref_unlimited (attr, _)
  | Pull_ref_xform (attr, _, _)
  | Pull_reverse_ref (attr, _)
  | Pull_reverse_ref_default (attr, _, _)
  | Pull_reverse_ref_limit (attr, _, _)
  | Pull_reverse_ref_unlimited (attr, _)
  | Pull_reverse_ref_xform (attr, _, _) ->
    Some attr
  | Pull_as (selector, _) -> pull_limit_attr context selector
  | Pull_id | Pull_wildcard | Pull_recursive_ref _ -> None

let validate_pull_limit_target context db selector =
  match pull_limit_attr context selector with
  | Some attr when context.cardinality db attr = Many -> ()
  | Some attr -> invalid_arg ("pull :limit requires cardinality many attr: " ^ attr)
  | None -> invalid_arg "pull :limit applies only to attrs and refs"

let with_pull_limit_form context db selector limit_form =
  validate_pull_limit_target context db selector;
  match limit_form with
  | QueryFormInt limit ->
    if limit <= 0 then invalid_arg "pull :limit must be positive";
    with_pull_limit selector limit
  | QueryFormNil -> with_pull_unlimited selector
  | _ -> invalid_arg "pull :limit requires an integer or nil"

let pull_string_of_value = function
  | String value | Symbol value -> value
  | Nil -> ""
  | Int value -> string_of_int value
  | Float value -> string_of_float value
  | Bool true -> "true"
  | Bool false -> "false"
  | Keyword value -> ":" ^ value
  | Uuid value -> value
  | Instant value -> string_of_int value
  | Regex value -> value
  | Ref entity_id -> string_of_int entity_id
  | List _ | Vector _ | Map _ | Set _ | Tuple _ | TxRef | Ref_to _ -> invalid_arg "cannot stringify composite pull value"

let pull_name_value context = function
  | Keyword value | Symbol value ->
    let _, name = context.split_keyword value in
    Some (String name)
  | String value -> Some (String value)
  | _ -> None

let pull_namespace_value context = function
  | Keyword value | Symbol value ->
    let namespace, _ = context.split_keyword value in
    if namespace = "" then None else Some (String namespace)
  | _ -> None

let pull_scalar_xform f = function
  | Pulled_scalar value ->
    (match f value with
     | Some value -> Pulled_scalar value
     | None -> Pulled_many [])
  | _ -> Pulled_many []

let pull_vector_xform value = Pulled_many [ value ]

let pull_xform_of_form context = function
  | QueryFormSymbol "identity" -> Fun.id
  | QueryFormSymbol "vector" -> pull_vector_xform
  | QueryFormSymbol "name" -> pull_scalar_xform (pull_name_value context)
  | QueryFormSymbol "namespace" -> pull_scalar_xform (pull_namespace_value context)
  | QueryFormSymbol "str" -> pull_scalar_xform (fun value -> Some (String (pull_string_of_value value)))
  | QueryFormSymbol symbol -> invalid_arg ("cannot resolve pull xform: " ^ symbol)
  | _ -> invalid_arg "pull :xform requires a symbol"

let rec with_pull_xform selector f =
  match selector with
  | Pull_attr attr -> Pull_attr_xform (attr, f)
  | Pull_attr_default (attr, default) -> Pull_attr_default_xform (attr, default, f)
  | Pull_ref (attr, pattern) -> Pull_ref_xform (attr, pattern, f)
  | Pull_reverse_ref (attr, pattern) -> Pull_reverse_ref_xform (attr, pattern, f)
  | Pull_as (selector, alias) -> Pull_as (with_pull_xform selector f, alias)
  | _ -> invalid_arg "pull :xform applies only to attrs and refs"

let pull_alias_key_of_form = function
  | QueryFormKeyword alias -> Keyword alias
  | QueryFormString alias -> String alias
  | QueryFormInt alias -> Int alias
  | QueryFormNil -> Nil
  | _ -> invalid_arg "pull :as requires keyword, string, integer, or nil"

let rec apply_pull_attr_options context db selector = function
  | [] -> selector
  | QueryFormKeyword "as" :: alias :: rest ->
    apply_pull_attr_options context db (Pull_as (selector, pull_alias_key_of_form alias)) rest
  | QueryFormKeyword "default" :: default :: rest ->
    apply_pull_attr_options context db (with_pull_default selector (context.query_value_of_form default)) rest
  | QueryFormKeyword "limit" :: limit :: rest ->
    apply_pull_attr_options context db (with_pull_limit_form context db selector limit) rest
  | QueryFormKeyword "xform" :: xform :: rest ->
    apply_pull_attr_options context db (with_pull_xform selector (pull_xform_of_form context xform)) rest
  | _ -> invalid_arg "unsupported pull attr option"

let rec parse_pull_attr_spec context db = function
  | QueryFormKeyword attr -> Pull_attr (validate_pull_attr_name context db attr)
  | QueryFormString attr -> Pull_attr (validate_pull_string_attr_name context db attr)
  | QueryFormVector [ QueryFormString "limit"; attr_form; limit ]
  | QueryFormVector [ QueryFormSymbol "limit"; attr_form; limit ]
  | QueryFormList [ QueryFormString "limit"; attr_form; limit ]
  | QueryFormList [ QueryFormSymbol "limit"; attr_form; limit ] ->
    with_pull_limit_form context db (parse_pull_attr_spec context db attr_form) limit
  | QueryFormVector [ QueryFormString "default"; attr_form; default ]
  | QueryFormVector [ QueryFormSymbol "default"; attr_form; default ]
  | QueryFormList [ QueryFormString "default"; attr_form; default ]
  | QueryFormList [ QueryFormSymbol "default"; attr_form; default ] ->
    with_pull_default (parse_pull_attr_spec context db attr_form) (context.query_value_of_form default)
  | QueryFormVector (attr_form :: options)
  | QueryFormList (attr_form :: options) ->
    apply_pull_attr_options context db (parse_pull_attr_spec context db attr_form) options
  | _ -> invalid_arg "pull attr spec must be an attribute name or expression"

let checked_pull_ref_attr context db attr =
  validate_pull_ref_attr context db attr;
  pull_ref_attr context attr

let rec with_pull_ref_pattern context db selector pattern =
  match selector with
  | Pull_attr attr ->
    let name = checked_pull_ref_attr context db attr in
    if context.is_reverse_ref attr then Pull_reverse_ref (name, pattern) else Pull_ref (name, pattern)
  | Pull_attr_default (attr, default) ->
    let name = checked_pull_ref_attr context db attr in
    if context.is_reverse_ref attr then
      Pull_reverse_ref_default (name, pattern, default)
    else
      Pull_ref_default (name, pattern, default)
  | Pull_attr_limit (attr, limit) ->
    let name = checked_pull_ref_attr context db attr in
    if context.is_reverse_ref attr then
      Pull_reverse_ref_limit (name, pattern, limit)
    else
      Pull_ref_limit (name, pattern, limit)
  | Pull_attr_unlimited attr ->
    let name = checked_pull_ref_attr context db attr in
    if context.is_reverse_ref attr then
      Pull_reverse_ref_unlimited (name, pattern)
    else
      Pull_ref_unlimited (name, pattern)
  | Pull_attr_xform (attr, f) ->
    let name = checked_pull_ref_attr context db attr in
    if context.is_reverse_ref attr then
      Pull_reverse_ref_xform (name, pattern, f)
    else
      Pull_ref_xform (name, pattern, f)
  | Pull_as (selector, alias) -> Pull_as (with_pull_ref_pattern context db selector pattern, alias)
  | _ -> invalid_arg "pull map spec must use an attr selector"

let rec with_pull_recursive_ref context db selector depth =
  match selector with
  | Pull_attr attr -> Pull_recursive_ref (checked_pull_ref_attr context db attr, [], depth)
  | Pull_as (selector, alias) -> Pull_as (with_pull_recursive_ref context db selector depth, alias)
  | _ -> invalid_arg "recursive pull applies only to attr selectors"

let rec pull_selector_is_recursive = function
  | Pull_recursive_ref _ -> true
  | Pull_as (selector, _) -> pull_selector_is_recursive selector
  | _ -> false

let rec apply_pull_recursive_context context = function
  | Pull_recursive_ref (attr, [], depth) -> Pull_recursive_ref (attr, context, depth)
  | Pull_as (selector, alias) -> Pull_as (apply_pull_recursive_context context selector, alias)
  | selector -> selector

let with_pull_recursive_context selectors =
  let context =
    selectors
    |> List.filter (fun selector -> not (pull_selector_is_recursive selector))
  in
  List.map (apply_pull_recursive_context context) selectors

let rec parse_pull_selector context db = function
  | QueryFormSymbol "*" | QueryFormString "*" | QueryFormKeyword "*" -> Pull_wildcard
  | QueryFormKeyword attr -> Pull_attr (validate_pull_attr_name context db attr)
  | QueryFormString attr -> Pull_attr (validate_pull_string_attr_name context db attr)
  | QueryFormVector _ | QueryFormList _ as attr_spec -> parse_pull_attr_spec context db attr_spec
  | QueryFormMap [ attr_spec, pattern ] -> parse_pull_map_spec context db attr_spec pattern
  | QueryFormMap [] -> invalid_arg "pull map spec cannot be empty"
  | QueryFormMap _ -> invalid_arg "pull map spec must contain one attr pattern pair"
  | _ -> invalid_arg "unsupported pull selector form"

and parse_pull_selectors context db = function
  | QueryFormMap [] -> invalid_arg "pull map spec cannot be empty"
  | QueryFormMap entries ->
    List.map (fun (attr_spec, pattern) -> parse_pull_map_spec context db attr_spec pattern) entries
  | selector -> [ parse_pull_selector context db selector ]

and parse_pull_map_spec context db attr_spec pattern =
  let selector = parse_pull_attr_spec context db attr_spec in
  match pattern with
  | QueryFormSymbol "..." | QueryFormString "..." ->
    with_pull_recursive_ref context db selector None
  | QueryFormInt depth ->
    if depth <= 0 then invalid_arg "recursive pull depth must be positive";
    with_pull_recursive_ref context db selector (Some depth)
  | _ -> with_pull_ref_pattern context db selector (parse_pattern context db pattern)

and parse_pattern context db = function
  | QueryFormVector selectors | QueryFormList selectors ->
    selectors
    |> List.concat_map (parse_pull_selectors context db)
    |> with_pull_recursive_context
  | _ -> invalid_arg "pull pattern must be sequential"

let parse_pattern_string context db input =
  parse_pattern context db (context.read_edn input)
