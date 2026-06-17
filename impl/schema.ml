open Datascript_types

let validate_schema schema =
  let is_tuple_attr attr =
    match List.assoc_opt attr schema with
    | Some { tuple_attrs = Some _; _ } -> true
    | _ -> false
  in
  let is_many_attr attr =
    match List.assoc_opt attr schema with
    | Some { cardinality = Many; _ } -> true
    | _ -> false
  in
  List.iter
    (fun (attr, spec) ->
      if spec.is_component && spec.value_type <> Some RefType then
        invalid_arg ("component attribute requires ref value type: " ^ attr);
      if spec.value_type = Some TupleType && spec.tuple_attrs = None && spec.tuple_types = None then
        invalid_arg ("tuple value type requires tuple attrs or tuple types: " ^ attr);
      (match spec.tuple_types with
       | Some [] -> invalid_arg ("tuple types cannot be empty: " ^ attr)
       | _ -> ());
      match spec.tuple_attrs with
      | None -> ()
      | Some [] -> invalid_arg ("tuple attrs cannot be empty: " ^ attr)
      | Some source_attrs ->
        if spec.cardinality = Many then
          invalid_arg ("tuple attrs must be cardinality one: " ^ attr);
        List.iter
          (fun source_attr ->
            if is_tuple_attr source_attr then
              invalid_arg ("tuple attrs cannot depend on another tuple attr: " ^ attr);
            if is_many_attr source_attr then
              invalid_arg ("tuple attrs cannot depend on cardinality many attr: " ^ attr))
          source_attrs)
    schema;
  schema

let schema_attr_by_name schema attr = List.assoc_opt attr schema

let schema_attr_is_ref schema attr =
  match schema_attr_by_name schema attr with
  | Some { value_type = Some RefType; _ } -> true
  | _ -> false

let schema_attr_is_tuple = function
  | Some { tuple_attrs = Some _; _ } -> true
  | _ -> false

let schema_attr_is_avet_accessible schema attr =
  attr = "db/ident"
  || schema_attr_is_tuple (schema_attr_by_name schema attr)
  ||
  match schema_attr_by_name schema attr with
  | Some { value_type = Some RefType; _ }
  | Some { unique = Some _; _ }
  | Some { indexed = true; _ } -> true
  | _ -> false

let schema_has_no_history schema attr =
  match List.assoc_opt attr schema with
  | Some { no_history = true; _ } -> true
  | _ -> false

let default_schema_attr =
  { cardinality = One
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let schema_keyword_values datoms e attr =
  datoms
  |> List.filter_map (fun d ->
    match d.e = e, d.a = attr, d.v with
    | true, true, Keyword value -> Some value
    | _ -> None)
  |> List.rev

let schema_bool_value datoms e attr =
  datoms
  |> List.find_map (fun d ->
    match d.e = e, d.a = attr, d.v with
    | true, true, Bool value -> Some value
    | _ -> None)

let schema_string_value datoms e attr =
  datoms
  |> List.find_map (fun d ->
    match d.e = e, d.a = attr, d.v with
    | true, true, String value -> Some value
    | _ -> None)

let schema_keyword_value datoms e attr =
  match schema_keyword_values datoms e attr with
  | value :: _ -> Some value
  | [] -> None

let is_db_namespace_ident ident =
  String.length ident >= 3 && String.sub ident 0 3 = "db/"

let schema_fields =
  [ "db/cardinality"
  ; "db/valueType"
  ; "db/type"
  ; "db/unique"
  ; "db/index"
  ; "db/isComponent"
  ; "db/noHistory"
  ; "db/doc"
  ; "db/tupleAttrs"
  ; "db/tupleTypes"
  ]

let schema_field_removed removed_fields attr field =
  List.mem (attr, field) removed_fields

let schema_value_type_removed removed_fields attr =
  schema_field_removed removed_fields attr "db/valueType"
  || schema_field_removed removed_fields attr "db/type"

let value_type_of_schema_keyword = function
  | "db.type/ref" -> RefType
  | "db.type/tuple" -> TupleType
  | "db.type/string" -> StringType
  | "db.type/keyword" -> KeywordType
  | "db.type/number" -> NumberType
  | "db.type/uuid" -> UuidType
  | "db.type/instant" -> InstantType
  | value -> invalid_arg ("unknown schema value type: " ^ value)

let schema_attr_from_datoms
      ?(strict = true)
      ?(ignored_schema_entities = [])
      ?(removed_fields = [])
      current
      datoms
      e
  =
  let ident = schema_keyword_value datoms e "db/ident" in
  let has_schema_fields =
    List.exists
      (fun d ->
        d.e = e && List.mem d.a schema_fields)
      datoms
  in
  if has_schema_fields then begin
    (match ident with
     | Some ident when is_db_namespace_ident ident ->
       if strict then invalid_arg "schema transaction cannot install db namespace attrs"
     | _ -> ());
    let has_attr attr = List.exists (fun d -> d.e = e && d.a = attr) datoms in
    if has_attr "db/cardinality" || has_attr "db/valueType" || has_attr "db/type" then
      match ident, schema_keyword_value datoms e "db/cardinality" with
      | Some _, Some _ -> ()
      | None, _ when List.mem e ignored_schema_entities -> ()
      | _ ->
        if strict then invalid_arg "incomplete schema transaction attributes"
  end;
  match ident, has_schema_fields with
  | Some attr, true ->
    let base =
      match List.assoc_opt attr current with
      | Some spec -> spec
      | None -> default_schema_attr
    in
    let unique_removed = schema_field_removed removed_fields attr "db/unique" in
    let base =
      { cardinality =
          (if schema_field_removed removed_fields attr "db/cardinality" then
             default_schema_attr.cardinality
           else
             base.cardinality)
      ; value_type =
          (if schema_value_type_removed removed_fields attr then
             default_schema_attr.value_type
           else
             base.value_type)
      ; unique =
          (if unique_removed then
             default_schema_attr.unique
           else
             base.unique)
      ; indexed =
          (if
             schema_field_removed removed_fields attr "db/index"
             || (unique_removed && schema_bool_value datoms e "db/index" = None)
           then
             default_schema_attr.indexed
           else
             base.indexed)
      ; is_component =
          (if schema_field_removed removed_fields attr "db/isComponent" then
             default_schema_attr.is_component
           else
             base.is_component)
      ; no_history =
          (if schema_field_removed removed_fields attr "db/noHistory" then
             default_schema_attr.no_history
           else
             base.no_history)
      ; doc =
          (if schema_field_removed removed_fields attr "db/doc" then
             default_schema_attr.doc
           else
             base.doc)
      ; tuple_attrs =
          (if schema_field_removed removed_fields attr "db/tupleAttrs" then
             default_schema_attr.tuple_attrs
           else
             base.tuple_attrs)
      ; tuple_types =
          (if schema_field_removed removed_fields attr "db/tupleTypes" then
             default_schema_attr.tuple_types
           else
             base.tuple_types)
      }
    in
    let unique =
      match schema_keyword_value datoms e "db/unique" with
      | Some "db.unique/identity" -> Some Identity
      | Some "db.unique/value" -> Some Value
      | _ -> base.unique
    in
    let spec =
      { cardinality =
          (match schema_keyword_value datoms e "db/cardinality" with
           | Some "db.cardinality/many" -> Many
           | Some "db.cardinality/one" -> One
           | _ -> base.cardinality)
      ; value_type =
          (match
             match schema_keyword_value datoms e "db/valueType" with
             | Some _ as value_type -> value_type
             | None -> schema_keyword_value datoms e "db/type"
           with
           | Some value -> Some (value_type_of_schema_keyword value)
           | _ -> base.value_type)
      ; unique
      ; indexed =
          (match schema_bool_value datoms e "db/index" with
           | Some value -> value
           | None -> base.indexed || Option.is_some unique)
      ; is_component =
          (match schema_bool_value datoms e "db/isComponent" with
           | Some value -> value
           | None -> base.is_component)
      ; no_history =
          (match schema_bool_value datoms e "db/noHistory" with
           | Some value -> value
           | None -> base.no_history)
      ; doc =
          (match schema_string_value datoms e "db/doc" with
           | Some value -> Some value
           | None -> base.doc)
      ; tuple_attrs =
          (match schema_keyword_values datoms e "db/tupleAttrs" with
           | [] -> base.tuple_attrs
           | attrs -> Some attrs)
      ; tuple_types =
          (match schema_keyword_values datoms e "db/tupleTypes" with
           | [] -> base.tuple_types
           | types -> Some (List.map value_type_of_schema_keyword types))
      }
    in
    Some (attr, spec)
  | _ -> None

let schema_idents_from_datoms datoms =
  datoms
  |> List.filter_map (fun d ->
    match d.a, d.v with
    | "db/ident", Keyword ident -> Some ident
    | _ -> None)
  |> List.sort_uniq compare

let replace_schema_attr schema (attr, spec) =
  let schema = List.remove_assoc attr schema in
  schema @ [ attr, spec ]

let schema_from_transaction_datoms
      ?(strict = true)
      ?(removed_attrs = [])
      ?(removed_fields = [])
      ?(ignored_schema_entities = [])
      current
      datoms
  =
  let schema =
    let described_attrs = schema_idents_from_datoms datoms @ removed_attrs |> List.sort_uniq compare in
    List.filter (fun (attr, _) -> not (List.mem attr described_attrs)) current
  in
  datoms
  |> List.fold_left
       (fun schema d ->
         match schema_attr_from_datoms ~strict ~ignored_schema_entities ~removed_fields current datoms d.e with
         | Some entry -> replace_schema_attr schema entry
         | None -> schema)
       schema
  |> validate_schema


let split_namespaced_attr attr =
  match String.index_opt attr '/' with
  | None -> None, attr
  | Some index ->
    let namespace = String.sub attr 0 index in
    let name = String.sub attr (index + 1) (String.length attr - index - 1) in
    Some namespace, name

let join_namespaced_attr namespace name =
  match namespace with
  | None -> name
  | Some namespace -> namespace ^ "/" ^ name

let is_reverse_ref attr =
  let _, name = split_namespaced_attr attr in
  String.length name > 0 && name.[0] = '_'

let reverse_ref attr =
  let namespace, name = split_namespaced_attr attr in
  if is_reverse_ref attr then
    join_namespaced_attr namespace (String.sub name 1 (String.length name - 1))
  else
    join_namespaced_attr namespace ("_" ^ name)
