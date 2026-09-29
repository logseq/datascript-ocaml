open Datascript_types

(* Schema assoc lists are immutable, so physical equality on the list is a
   valid staleness check: any schema update produces a new list. *)
let last_schema : schema ref = ref []
let last_tbl = ref (Hashtbl.create 0)

let schema_attr db attr =
  if not (db.schema == !last_schema) then (
    let tbl = Hashtbl.create (List.length db.schema) in
    List.iter
      (fun (attr, schema_attr) ->
        if not (Hashtbl.mem tbl attr) then Hashtbl.add tbl attr schema_attr)
      db.schema;
    last_tbl := tbl;
    last_schema := db.schema);
  Hashtbl.find_opt !last_tbl attr
  
  let ident_attr = "db/ident"
  
  let cardinality db attr =
    if attr = "db/tupleAttrs" || attr = "db/tupleTypes" then Many
    else
      match schema_attr db attr with
      | Some schema_attr -> schema_attr.cardinality
      | None -> One
  
  let is_unique_identity db attr =
    attr = ident_attr
    ||
    match schema_attr db attr with
    | Some { unique = Some Identity; _ } -> true
    | _ -> false
  
  let is_unique db attr =
    attr = ident_attr
    ||
    match schema_attr db attr with
    | Some { unique = Some _; _ } -> true
    | _ -> false
  
  let tuple_attrs db attr =
    match schema_attr db attr with
    | Some { tuple_attrs = Some attrs; _ } -> Some attrs
    | _ -> None
  
  let is_tuple_attr db attr = Option.is_some (tuple_attrs db attr)
  
  let is_indexed db attr =
    attr = ident_attr
    ||
    is_tuple_attr db attr
    ||
    match schema_attr db attr with
    | Some { indexed = true; _ } -> true
    | _ -> false
  
  let is_component db attr =
    match schema_attr db attr with
    | Some { is_component = true; _ } -> true
    | _ -> false
  
  let is_ref_attr db attr =
    match schema_attr db attr with
    | Some { value_type = Some RefType; _ } -> true
    | _ -> false
  
  let tuple_attrs_for_source db source_attr =
    db.schema
    |> List.filter_map (fun (attr, schema_attr) ->
      match schema_attr.tuple_attrs with
      | Some source_attrs when List.mem source_attr source_attrs -> Some (attr, source_attrs)
      | _ -> None)
  
