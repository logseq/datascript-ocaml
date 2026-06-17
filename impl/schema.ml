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
