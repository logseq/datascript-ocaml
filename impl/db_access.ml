open Datascript_types

module Make (Context : sig
  val is_ref_attr : db -> attr -> bool
  val is_unique : db -> attr -> bool
  val is_indexed : db -> attr -> bool
  val entid : db -> attr -> value -> entity_id option
  val ident_attr : attr
  val lookup_ref_entity_id : ?strict_missing:bool -> db -> attr -> value -> entity_id option
  val normalize_value : value -> value
  val unresolved_entity_ref_message : entity_ref -> string
  val ref_attr_for_value_resolution : db -> attr -> attr option
  val entity_ref_of_ref_attr_value : value -> entity_ref option
  val compare_value : value -> value -> int
  val first_nonzero : int list -> int
  val validate_entity_id : int -> entity_id
end) = struct
  open Context

  let is_avet_accessible db attr =
    is_ref_attr db attr
    || is_unique db attr
    || is_indexed db attr
  
  let rec resolve_index_entity_ref db = function
    | Entity_id entity_id -> Some entity_id
    | Ident ident -> entid db ident_attr (Keyword ident)
    | Lookup_ref (attr, value) ->
      let value = resolve_index_value db value in
      lookup_ref_entity_id ~strict_missing:true db attr value
    | CurrentTx | Temp_id _ -> None
  
  and resolve_index_value db = function
    | Ref_to entity_ref ->
      (match resolve_index_entity_ref db entity_ref with
       | Some entity_id -> Ref entity_id
       | None -> invalid_arg (unresolved_entity_ref_message entity_ref))
    | List values ->
      normalize_value (List (List.map (resolve_index_value db) values))
    | Vector values ->
      normalize_value (Vector (List.map (resolve_index_value db) values))
    | Map entries ->
      normalize_value
        (Map
           (List.map
              (fun (key, value) ->
                resolve_index_value db key, resolve_index_value db value)
              entries))
    | Set values ->
      normalize_value (Set (List.map (resolve_index_value db) values))
    | Tuple values ->
      normalize_value
        (Tuple
           (List.map
              (function
                | None -> None
                | Some value -> Some (resolve_index_value db value))
              values))
    | value -> normalize_value value
  
  let entid_ref db = function
    | Entity_id entity_id -> Some (validate_entity_id entity_id)
    | Ident ident -> entid db ident_attr (Keyword ident)
    | Lookup_ref (attr, value) -> lookup_ref_entity_id db attr (resolve_index_value db value)
    | CurrentTx | Temp_id _ -> invalid_arg "transaction-local entity refs cannot be resolved from a db"
  
  let resolve_index_value_option db = Option.map (resolve_index_value db)
  
  let resolve_index_value_for_attr db attr value =
    match ref_attr_for_value_resolution db attr, entity_ref_of_ref_attr_value value with
    | Some _, Some entity_ref ->
      (match resolve_index_entity_ref db entity_ref with
       | Some entity_id -> Ref entity_id
       | None -> invalid_arg (unresolved_entity_ref_message entity_ref))
    | _ -> resolve_index_value db value
  
  let resolve_index_value_option_for_attr db attr = Option.map (resolve_index_value_for_attr db attr)
  
  let resolve_index_value_option_for_optional_attr db attr value =
    match attr with
    | Some attr -> resolve_index_value_option_for_attr db attr value
    | None -> resolve_index_value_option db value
  
  let resolve_index_entity_ref_exn db entity_ref =
    match resolve_index_entity_ref db entity_ref with
    | Some entity_id -> entity_id
    | None -> invalid_arg (unresolved_entity_ref_message entity_ref)
  
  let db_index_context : Db.index_context =
    { is_avet_accessible
    ; resolve_entity_ref = resolve_index_entity_ref_exn
    ; resolve_value_for_optional_attr =
        (fun db attr value -> resolve_index_value_option_for_optional_attr db attr (Some value) |> Option.get)
    ; resolve_value_for_attr = resolve_index_value_for_attr
    ; compare_value
    ; first_nonzero
    }
  
  let datoms db index ?e ?a ?v ?tx () =
    Db.datoms db_index_context db index ?e ?a ?v ?tx ()
  
  let datoms_ref db index ?e ?a ?v ?tx () =
    Db.datoms_ref db_index_context db index ?e ?a ?v ?tx ()

  let find_datom db index ?e ?a ?v ?tx () =
    Db.find_datom db_index_context db index ?e ?a ?v ?tx ()

  let find_datom_ref db index ?e ?a ?v ?tx () =
    Db.find_datom_ref db_index_context db index ?e ?a ?v ?tx ()

  let seek_datoms db index ?e ?a ?v ?tx () =
    Db.seek_datoms db_index_context db index ?e ?a ?v ?tx ()

  let seek_datoms_ref db index ?e ?a ?v ?tx () =
    Db.seek_datoms_ref db_index_context db index ?e ?a ?v ?tx ()

  let rseek_datoms db index ?e ?a ?v ?tx () =
    Db.rseek_datoms db_index_context db index ?e ?a ?v ?tx ()

  let rseek_datoms_ref db index ?e ?a ?v ?tx () =
    Db.rseek_datoms_ref db_index_context db index ?e ?a ?v ?tx ()

  let index_range db attr ?start ?stop () =
    Db.index_range db_index_context db attr ?start ?stop ()
  
end
