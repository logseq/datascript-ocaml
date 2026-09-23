open Datascript_types

type context =
  { validate_entity_id : int -> entity_id
  ; entid : db -> attr -> value -> entity_id option
  ; ident_attr : attr
  ; allocate_entity_id : entity_id -> entity_id
  ; lookup_ref_entity_id : strict_missing:bool -> db -> attr -> value -> entity_id option
  ; unresolved_lookup_ref_message : attr -> value -> string
  ; normalize_value : value -> value
  ; is_ref_attr : db -> attr -> bool
  ; is_reverse_ref : attr -> bool
  ; reverse_ref : attr -> attr
  ; cardinality : db -> attr -> cardinality
  ; is_unique_identity : db -> attr -> bool
  ; max_eid_with_entity_id : int -> entity_id -> entity_id
  ; max_eid_in_value : int -> value -> int
  }

let remember_tempid tempids tempid eid =
  match List.assoc_opt tempid tempids with
  | Some existing when existing = eid -> tempids
  | Some _ -> invalid_arg ("conflicting tempid: " ^ tempid)
  | None -> tempids @ [ tempid, eid ]

let remember_current_tx tempids tx =
  remember_tempid tempids "db/current-tx" tx

let ensure_current_tx_tempid tempids tx =
  ("db/current-tx", tx) :: List.remove_assoc "db/current-tx" tempids

let is_current_tx_alias = function
  | ":db/current-tx" | "datomic.tx" | "datascript.tx" -> true
  | _ -> false

let remember_current_tx_alias tempids tx alias =
  let tempids = ensure_current_tx_tempid tempids tx in
  match List.assoc_opt alias tempids with
  | Some existing when existing = tx -> tempids
  | Some _ -> invalid_arg ("conflicting tempid: " ^ alias)
  | None ->
    let rec insert_after_current_tx_aliases prefix = function
      | ((tempid, entity_id) as entry) :: rest when entity_id = tx && (tempid = "db/current-tx" || is_current_tx_alias tempid) ->
        insert_after_current_tx_aliases (entry :: prefix) rest
      | rest -> List.rev prefix @ ((alias, tx) :: rest)
    in
    insert_after_current_tx_aliases [] tempids

(* Raised when a lookup ref cannot be resolved yet: the target may be defined
   by a later op in the same transaction. apply_ops defers such ops and retries
   them as the tx datoms accumulate; if they still cannot resolve at tx end the
   original "Nothing found" error is raised (matching upstream's strict
   sequential behavior for genuinely missing targets). *)
exception Unresolved_lookup_ref of attr * value

(* upstream retry-with-tempid: when a tempid that already allocated an eid in
   this tx upserts to a different entity, the whole transaction re-runs with the
   tempids bound so far, the conflicting one pre-seeded to the upsert target. *)
exception Upsert_retry of string * entity_id * (string * entity_id) list

(* Tempids threaded through a transaction. Upstream keeps a mutable
   tempid -> entity id map; the port mirrors that with a Hashtbl plus an
   insertion-order list so the report's :tempids keeps its list shape.
   (A plain assoc list costs O(n) per lookup and per append, which is
   quadratic on imports that mint tens of thousands of tempids.) *)
type tempid_map =
  { tbl : (string, entity_id) Hashtbl.t
  ; mutable rev : (string * entity_id) list (* newest first *)
  ; frozen : bool (* upsert probes resolve without recording tempid writes *)
  }

let tempid_map_empty () = { tbl = Hashtbl.create 16; rev = []; frozen = false }

let tempid_map_of_list list =
  let tbl = Hashtbl.create (List.length list) in
  List.iter (fun (tempid, eid) -> Hashtbl.replace tbl tempid eid) list;
  { tbl; rev = List.rev list; frozen = false }

let list_of_tempid_map m = List.rev m.rev

let tempid_find m tempid = Hashtbl.find_opt m.tbl tempid

(* rebuild the map from an authoritative insertion-order list; used by the
   rare current-tx paths so their ordering logic stays in one place *)
let sync_tempid_map m list =
  if m.frozen then m
  else begin
    Hashtbl.reset m.tbl;
    List.iter (fun (tempid, eid) -> Hashtbl.replace m.tbl tempid eid) list;
    m.rev <- List.rev list;
    m
  end

let remember_tempid_m m tempid eid =
  if m.frozen then m
  else
    match Hashtbl.find_opt m.tbl tempid with
    | Some existing when existing = eid -> m
    | Some _ -> invalid_arg ("conflicting tempid: " ^ tempid)
    | None ->
      Hashtbl.add m.tbl tempid eid;
      m.rev <- (tempid, eid) :: m.rev;
      m

(* bulk-path insert where the caller already knows the tempid is fresh *)
let tempid_remember_unchecked m tempid eid =
  if m.frozen then m
  else begin
    Hashtbl.replace m.tbl tempid eid;
    m.rev <- (tempid, eid) :: m.rev;
    m
  end

(* tempid bindings minted inside an op that aborts (e.g. an unresolved lookup
   ref queued for retry) must roll back together with the discarded store:
   the mutable tbl outlives the state tuple, so rebuild it from the
   pre-attempt rev list *)
let restore_tempid_map m saved_rev =
  if not m.frozen && m.rev != saved_rev then begin
    Hashtbl.reset m.tbl;
    List.iter (fun (tempid, eid) -> Hashtbl.replace m.tbl tempid eid) (List.rev saved_rev);
    m.rev <- saved_rev
  end

let remember_current_tx_m m tx = remember_tempid_m m "db/current-tx" tx

let ensure_current_tx_tempid_m m tx =
  sync_tempid_map m (ensure_current_tx_tempid (list_of_tempid_map m) tx)

let remember_current_tx_alias_m m tx alias =
  sync_tempid_map m (remember_current_tx_alias (list_of_tempid_map m) tx alias)

let rec resolve_entity_ref_m context db datoms tx max_eid tempids = function
  | Entity_id e when e < 0 ->
    (* upstream datascript: a negative integer :db/id is a tempid *)
    resolve_entity_ref_m context db datoms tx max_eid tempids
      (Temp_id (string_of_int e))
  | Entity_id e ->
    let e = context.validate_entity_id e in
    e, context.max_eid_with_entity_id max_eid e, tempids
  | CurrentTx -> tx, max_eid, remember_current_tx_m tempids tx
  | Ident ident ->
    (match context.entid datoms context.ident_attr (Keyword ident) with
     | Some e -> e, context.max_eid_with_entity_id max_eid e, tempids
     | None -> invalid_arg "ident did not resolve")
  | Temp_id tempid ->
    if is_current_tx_alias tempid then
      tx, max_eid, remember_current_tx_alias_m tempids tx tempid
    else
      (match tempid_find tempids tempid with
       | Some e -> e, context.max_eid_with_entity_id max_eid e, tempids
       | None ->
         let e = context.allocate_entity_id max_eid in
         e, context.max_eid_with_entity_id max_eid e, remember_tempid_m tempids tempid e)
  | Lookup_ref (attr, value) ->
    let value, max_eid, tempids = resolve_value_m context db datoms tx max_eid tempids value in
    (match context.lookup_ref_entity_id ~strict_missing:false datoms attr value with
     | Some e -> e, context.max_eid_with_entity_id max_eid e, tempids
     | None -> raise (Unresolved_lookup_ref (attr, value)))

and resolve_value_m context db datoms tx max_eid tempids = function
  | TxRef -> Ref tx, max_eid, remember_current_tx_m tempids tx
  | Ref e ->
    (* upstream advances max-eid only on datom.e; ref values never bump it *)
    let e = context.validate_entity_id e in
    Ref e, max_eid, tempids
  | Ref_to entity_ref ->
    let e, max_eid, tempids =
      match entity_ref with
      | Entity_id e ->
        let e = context.validate_entity_id e in
        e, max_eid, tempids
      | entity_ref -> resolve_entity_ref_m context db datoms tx max_eid tempids entity_ref
    in
    Ref e, max_eid, tempids
  | List values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value_m context db datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    context.normalize_value (List (List.rev values)), max_eid, tempids
  | Vector values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value_m context db datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    context.normalize_value (Vector (List.rev values)), max_eid, tempids
  | Map entries ->
    let entries, max_eid, tempids =
      List.fold_left
        (fun (entries, max_eid, tempids) (key, value) ->
          let key, max_eid, tempids = resolve_value_m context db datoms tx max_eid tempids key in
          let value, max_eid, tempids = resolve_value_m context db datoms tx max_eid tempids value in
          (key, value) :: entries, max_eid, tempids)
        ([], max_eid, tempids)
        entries
    in
    context.normalize_value (Map (List.rev entries)), max_eid, tempids
  | Set values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value_m context db datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    context.normalize_value (Set (List.rev values)), max_eid, tempids
  | Tuple values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          match value with
          | None -> None :: values, max_eid, tempids
          | Some value ->
            let value, max_eid, tempids = resolve_value_m context db datoms tx max_eid tempids value in
            Some value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    context.normalize_value (Tuple (List.rev values)), max_eid, tempids
  | value -> value, max_eid, tempids

let attr_name_of_value = function
  | Keyword attr | String attr | Symbol attr -> Some attr
  | _ -> None

let entity_ref_of_ref_attr_value = function
  | TxRef -> Some CurrentTx
  | Ref entity_id -> Some (Entity_id entity_id)
  | Ref_to entity_ref -> Some entity_ref
  | Int entity_id when entity_id < 0 -> Some (Temp_id (string_of_int entity_id))
  | Int entity_id -> Some (Entity_id entity_id)
  | String tempid -> Some (Temp_id tempid)
  | Keyword "db/current-tx" -> Some CurrentTx
  | Keyword ident -> Some (Ident ident)
  | Symbol "db/current-tx" -> Some CurrentTx
  | Symbol ("datomic.tx" | "datascript.tx" as tempid) -> Some (Temp_id tempid)
  | List [ attr; value ] | Vector [ attr; value ] ->
    attr_name_of_value attr |> Option.map (fun attr -> Lookup_ref (attr, value))
  | _ -> None

let ref_attr_for_value_resolution context db attr =
  if context.is_ref_attr db attr then
    Some attr
  else if context.is_reverse_ref attr && context.is_ref_attr db (context.reverse_ref attr) then
    Some (context.reverse_ref attr)
  else
    None

let resolve_value_for_attr_m context db attr datoms tx max_eid tempids value =
  match ref_attr_for_value_resolution context db attr, entity_ref_of_ref_attr_value value with
  | Some _, Some entity_ref ->
    (match value with
     | Ref e -> ignore (context.validate_entity_id e)
     | _ -> ());
    (* upstream advances max-eid only on datom.e; ref values never bump it *)
    let entity_id, max_eid, tempids =
      match entity_ref with
      | Entity_id e -> context.validate_entity_id e, max_eid, tempids
      | entity_ref -> resolve_entity_ref_m context db datoms tx max_eid tempids entity_ref
    in
    Ref entity_id, max_eid, tempids
  | Some _, None -> invalid_arg "Expected number or lookup ref for entity id"
  | _ ->
    resolve_value_m context db datoms tx max_eid tempids value

let attr_expands_collection context db attr =
  context.cardinality db attr = Many || context.is_reverse_ref attr

(* upstream maybe-wrap-multival: in a multival context a 2-element collection is
   a lookup ref only when its head names a :db.unique/identity attr; any other
   collection expands into individual values *)
let ref_lookup_collection_value context db = function
  | List [ attr; _ ] | Vector [ attr; _ ] ->
    (match attr_name_of_value attr with
     | Some attr -> context.is_unique_identity db attr
     | None -> false)
  | _ -> false

let resolve_existing_entity_ref_m context db datoms tx max_eid tempids = function
  | Temp_id _ -> invalid_arg "Tempids are allowed in :db/add only"
  | entity_ref -> resolve_entity_ref_m context db datoms tx max_eid tempids entity_ref

let resolve_optional_existing_entity_ref_m context db datoms tx max_eid tempids = function
  | Temp_id _ -> invalid_arg "Tempids are allowed in :db/add only"
  | Entity_id e ->
    (* retract positions emit no added datom, so upstream never advances *)
    let e = context.validate_entity_id e in
    Some e, max_eid, tempids
  | Lookup_ref (attr, value) ->
    let value, max_eid, tempids = resolve_value_m context db datoms tx max_eid tempids value in
    (match context.lookup_ref_entity_id ~strict_missing:false datoms attr value with
     | Some e -> Some e, context.max_eid_with_entity_id max_eid e, tempids
     | None -> None, max_eid, tempids)
  | entity_ref ->
    let e, max_eid, tempids = resolve_entity_ref_m context db datoms tx max_eid tempids entity_ref in
    Some e, max_eid, tempids

(* upstream assoc-auto-tempids only wraps map values into nested entities for
   ref attributes; under a non-ref attribute a map stays a literal value. The
   EDN parser is schema-agnostic and produces One_entity for every map form, so
   apply-time resolution converts it back to a plain Map value here. *)
let rec scalar_value_of_tx_value = function
  | One_value value -> value
  | Many_values values -> Set values
  | One_entity entity -> Map (map_entries_of_tx_entity entity)
  | Many_entities entities -> Set (List.map (fun entity -> Map (map_entries_of_tx_entity entity)) entities)

and map_entries_of_tx_entity (entity : tx_entity) : (value * value) list =
  let entries =
    List.map
      (fun (attr, tx_value) -> Keyword attr, scalar_value_of_tx_value tx_value)
      entity.attrs
  in
  match entity.db_id with
  | Some entity_ref -> (Keyword "db/id", value_of_entity_ref entity_ref) :: entries
  | None -> entries

and value_of_entity_ref = function
  | Entity_id entity_id -> Int entity_id
  | Temp_id tempid -> (match int_of_string_opt tempid with Some n -> Int n | None -> String tempid)
  | Ident ident -> Keyword ident
  | Lookup_ref (attr, value) -> Vector [ Keyword attr; value ]
  | CurrentTx -> Keyword "db/current-tx"

let resolve_tx_value_for_attr_m context db attr datoms tx max_eid tempids = function
  | One_value ((List values | Vector values) as value) when attr_expands_collection context db attr && not (ref_lookup_collection_value context db value) ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value_for_attr_m context db attr datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    Many_values (List.rev values), max_eid, tempids
  | One_value (Set values) when attr_expands_collection context db attr ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value_for_attr_m context db attr datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    Many_values (List.rev values), max_eid, tempids
  | One_value value ->
    let value, max_eid, tempids = resolve_value_for_attr_m context db attr datoms tx max_eid tempids value in
    One_value value, max_eid, tempids
  | Many_values values ->
    let values, max_eid, tempids =
      List.fold_left
        (fun (values, max_eid, tempids) value ->
          let value, max_eid, tempids = resolve_value_for_attr_m context db attr datoms tx max_eid tempids value in
          value :: values, max_eid, tempids)
        ([], max_eid, tempids)
        values
    in
    Many_values (List.rev values), max_eid, tempids
  | One_entity entity -> One_entity entity, max_eid, tempids
  | Many_entities entities -> Many_entities entities, max_eid, tempids

let resolve_optional_value_for_attr_m context db attr datoms tx max_eid tempids = function
  | Some value ->
    let value, max_eid, tempids = resolve_value_for_attr_m context db attr datoms tx max_eid tempids value in
    Some value, max_eid, tempids
  | None -> None, max_eid, tempids

let resolve_entity_attrs_m context db datoms tx max_eid tempids attrs =
  let attrs, max_eid, tempids =
    List.fold_left
      (fun (attrs, max_eid, tempids) (attr, tx_value) ->
        let tx_value, max_eid, tempids = resolve_tx_value_for_attr_m context db attr datoms tx max_eid tempids tx_value in
        (attr, tx_value) :: attrs, max_eid, tempids)
      ([], max_eid, tempids)
      attrs
  in
  List.rev attrs, max_eid, tempids

(* the .mli-exposed helpers keep their assoc-list signatures; the map-based
   _m variants exist for the hot apply_tx path — conversions here are cold *)
let resolve_entity_ref context db datoms tx max_eid tempids entity_ref =
  let e, max_eid, tempids =
    resolve_entity_ref_m
      context
      db
      datoms
      tx
      max_eid
      (tempid_map_of_list tempids)
      entity_ref
  in
  e, max_eid, list_of_tempid_map tempids

let resolve_value context db datoms tx max_eid tempids value =
  let value, max_eid, tempids =
    resolve_value_m
      context
      db
      datoms
      tx
      max_eid
      (tempid_map_of_list tempids)
      value
  in
  value, max_eid, list_of_tempid_map tempids

let resolve_value_for_attr context db attr datoms tx max_eid tempids value =
  let value, max_eid, tempids =
    resolve_value_for_attr_m
      context
      db
      attr
      datoms
      tx
      max_eid
      (tempid_map_of_list tempids)
      value
  in
  value, max_eid, list_of_tempid_map tempids

let resolve_existing_entity_ref context db datoms tx max_eid tempids entity_ref =
  let e, max_eid, tempids =
    resolve_existing_entity_ref_m
      context
      db
      datoms
      tx
      max_eid
      (tempid_map_of_list tempids)
      entity_ref
  in
  e, max_eid, list_of_tempid_map tempids

let resolve_optional_existing_entity_ref context db datoms tx max_eid tempids entity_ref =
  let e, max_eid, tempids =
    resolve_optional_existing_entity_ref_m
      context
      db
      datoms
      tx
      max_eid
      (tempid_map_of_list tempids)
      entity_ref
  in
  e, max_eid, list_of_tempid_map tempids

let resolve_tx_value_for_attr context db attr datoms tx max_eid tempids tx_value =
  let tx_value, max_eid, tempids =
    resolve_tx_value_for_attr_m
      context
      db
      attr
      datoms
      tx
      max_eid
      (tempid_map_of_list tempids)
      tx_value
  in
  tx_value, max_eid, list_of_tempid_map tempids

let resolve_optional_value_for_attr context db attr datoms tx max_eid tempids value =
  let value, max_eid, tempids =
    resolve_optional_value_for_attr_m
      context
      db
      attr
      datoms
      tx
      max_eid
      (tempid_map_of_list tempids)
      value
  in
  value, max_eid, list_of_tempid_map tempids

let resolve_entity_attrs context db datoms tx max_eid tempids attrs =
  let attrs, max_eid, tempids =
    resolve_entity_attrs_m
      context
      db
      datoms
      tx
      max_eid
      (tempid_map_of_list tempids)
      attrs
  in
  attrs, max_eid, list_of_tempid_map tempids

type apply_context =
  { resolve_context : context
  ; is_filtered : db -> bool
  ; schema_from_transaction_datoms : ?validate:bool -> ?removed_field_attrs:attr list -> strict:bool -> removed_attrs:attr list -> removed_fields:(attr * attr) list -> ignored_schema_entities:entity_id list -> schema -> datom list -> schema
  ; schema_datoms : db -> datom list -> datom list
  ; schema_fields : attr list
  ; current_attr_value : db -> entity_id -> attr -> value option
  ; add_entity_attr_value : db -> tx -> db -> entity_id -> attr -> value -> db * datom list
  ; same_fact : datom -> datom -> bool
  ; add_user_datom_with_report : db -> tx -> db -> datom -> db * datom list
  ; is_tuple_attr : db -> attr -> bool
  ; tuple_attrs_for_source : db -> attr -> (attr * attr list) list
  ; is_unique_identity : db -> attr -> bool
  ; retract_user_attr_with_report : db -> tx -> db -> entity_id -> attr -> value option -> db * datom list
  ; retract_active_datom_with_report : tx -> db -> entity_id -> attr -> value option -> db * datom list
  ; retract_entity_with_report : db -> tx -> db -> entity_id -> db * datom list
  ; compare_and_set_matches : db -> entity_id -> attr -> value option -> bool
  ; compare_and_set_failure_message : db -> entity_id -> attr -> value option -> string
  ; datom : ?tx:tx -> ?added:bool -> e:entity_id -> a:attr -> v:value -> unit -> datom
  ; normalize_datom_for_schema : schema -> datom -> datom
  ; add_active_datom_with_report : ?allow_tuple:bool -> ?validate_value:bool -> db -> tx -> db -> datom -> db * datom list
  ; validate_explicit_upsert_target : db -> db -> entity_id -> (attr * tx_value) list -> unit
  ; entity_unique_identity : db -> db -> (attr * tx_value) list -> entity_id option
  ; existing_unique_entity : db -> attr -> value -> entity_id option
  ; existing_entity_datoms : db -> entity_id -> datom list
  ; existing_entity_attr_datoms : db -> entity_id -> attr -> datom list
  ; datoms_referencing_entity : db -> entity_id -> datom list
  ; value_equal : value -> value -> bool
  ; normalize_entity_attr_value : db -> entity_id -> attr -> value -> entity_id * attr * value
  ; tuple_direct_write_matches_sources : db -> db -> datom -> bool
  ; refresh_tuple_attrs_for_source : db -> tx -> db -> entity_id -> attr -> datom list -> db * datom list
  ; refresh_db_indexes_with_added_datoms : db -> datom list -> db
  ; refresh_db_indexes_with_tx_data : db -> datom list -> db
  ; refresh_db_identity : db -> db
  }

let apply_tx context tx_ops db =
  if context.is_filtered db then invalid_arg "filtered db is read-only";
  (* Mid-tx schema refresh is incremental: schema datoms appended to tx_data
     are recorded here and each refresh folds only the entities touched by
     the datoms seen since the previous refresh, instead of rescanning all
     of tx_data every time (O(n^2) on seed transactions). *)
  let pending_schema_rev = ref [] in
  let inc_tx_schema = Hashtbl.create 64 in
  (* removals already dropped from !current_schema by a previous incremental
     refresh — the removal sets below stay cumulative (the end-of-tx
     rebuild needs them all), so track what each refresh already applied *)
  let applied_removed_attrs = ref [] in
  let applied_removed_fields = ref [] in
  let append_tx_data tx_data_rev datom_tx_data =
    List.iter
      (fun d ->
        if d.a = "db/ident" || List.mem d.a context.schema_fields then begin
          pending_schema_rev := d :: !pending_schema_rev;
          if d.added then
            Hashtbl.replace
              inc_tx_schema
              d.e
              (d :: Option.value (Hashtbl.find_opt inc_tx_schema d.e) ~default:[])
        end)
      datom_tx_data;
    List.rev_append datom_tx_data tx_data_rev
  in
  let tx = db.max_tx + 1 in
  let current_schema = ref db.schema in
  let current_tx_fns = ref db.tx_fns in
  let removed_schema_attrs = ref [] in
  let removed_schema_fields = ref [] in
  let ignored_schema_entities = ref [] in
  let current_db () = { db with schema = !current_schema; tx_fns = !current_tx_fns } in
  let db_with_current_metadata tx_db =
    { tx_db with schema = !current_schema; tx_fns = !current_tx_fns }
  in
  let dedup_schema_facts datoms =
    (* first-occurrence (e,a,v) dedup, bucketed by (e,a) — same semantics
       as the whole-tx schema_datoms pass *)
    let seen = Hashtbl.create 8 in
    List.fold_left
      (fun acc d ->
        let key = d.e, d.a in
        match Hashtbl.find_opt seen key with
        | Some bucket when List.exists (context.same_fact d) bucket -> acc
        | Some bucket ->
          Hashtbl.replace seen key (d :: bucket);
          d :: acc
        | None ->
          Hashtbl.replace seen key [ d ];
          d :: acc)
      []
      datoms
    |> List.rev
  in
  let refresh_schema tx_db =
    match !pending_schema_rev with
    | [] -> ()
    | pending_rev ->
      pending_schema_rev := [];
      let touched =
        (* every entity with a fresh schema-relevant datom — including
           db/ident-only and retracted entities — mirrors the whole-tx
           schema_datoms pass's touched_schema_entities, ordered by last
           fresh datom so replacement order matches *)
        let last_pos = Hashtbl.create 8 in
        List.iteri (fun i d -> Hashtbl.replace last_pos d.e i) pending_rev;
        pending_rev
        |> List.map (fun d -> d.e)
        |> List.sort_uniq compare
        |> List.sort (fun e1 e2 ->
          compare (Hashtbl.find last_pos e1) (Hashtbl.find last_pos e2))
      in
      (match touched with
       | [] -> ()
       | _ ->
         let schema_db = db_with_current_metadata tx_db in
         let entity_datoms e =
           (* inc_tx_schema accumulates newest-first — the same order the
              whole-tx schema_datoms pass produces for asserted datoms *)
           let tx_datoms =
             Option.value (Hashtbl.find_opt inc_tx_schema e) ~default:[]
           in
           let actives =
             context.existing_entity_datoms schema_db e
             |> List.filter (fun d ->
               d.a = "db/ident" || List.mem d.a context.schema_fields)
           in
           dedup_schema_facts (tx_datoms @ actives)
         in
         let datoms = List.concat_map entity_datoms touched in
         (* apply each removal only once: the whole-tx pass re-derived
            every schema entity each refresh so re-stripping was harmless;
            here a stale removal would drop an attr whose entry an earlier
            batch already restored *)
         let new_removed_attrs =
           List.filter
             (fun a -> not (List.mem a !applied_removed_attrs))
             !removed_schema_attrs
         in
         let new_removed_field_attrs =
           !removed_schema_fields
           |> List.filter (fun p -> not (List.mem p !applied_removed_fields))
           |> List.map fst
         in
         applied_removed_attrs := !removed_schema_attrs;
         applied_removed_fields := !removed_schema_fields;
         (* mid-tx refreshes can see a partially-installed schema spec (e.g.
            db.type/tuple before its db/tupleTypes land); upstream does not
            revalidate the whole schema during a transaction *)
         current_schema
         := context.schema_from_transaction_datoms
              ~validate:false
              ~strict:false
              ~removed_attrs:new_removed_attrs
              ~removed_fields:!removed_schema_fields
              ~removed_field_attrs:new_removed_field_attrs
              ~ignored_schema_entities:!ignored_schema_entities
              !current_schema
              datoms)
  in
  (* upstream allocates eids sequentially from db.max_eid and advances only on
     added datom.e / allocations; no pre-scan of explicit ids in tx ops *)
  let initial_max_eid = db.max_eid in

  let max_tx_seen = ref tx in
  let deferred_ops = ref [] in
  let mark_entity_tempid entity_tempids = function
    | Temp_id tempid -> tempid :: entity_tempids
    | Entity_id e when e < 0 -> string_of_int e :: entity_tempids
    | _ -> entity_tempids
  in
  let validate_tempid_usage tempids entity_tempids =
    let entity_tempid_set = Hashtbl.create (List.length entity_tempids) in
    List.iter (fun tempid -> Hashtbl.replace entity_tempid_set tempid ()) entity_tempids;
    let value_only =
      tempids
      |> List.filter_map (fun (tempid, _) ->
        if
          tempid <> "db/current-tx"
          && not (is_current_tx_alias tempid)
          && not (Hashtbl.mem entity_tempid_set tempid)
        then
          Some tempid
        else
          None)
    in
    match value_only with
    | [] -> ()
    | tempids ->
      invalid_arg
        ("Tempids used only as value in transaction: ("
         ^ String.concat " " tempids
         ^ ")")
  in
  let rec tx_value_has_assertions attr = function
    | One_value (List []) | One_value (Vector []) | One_value (Set []) when attr_expands_collection context.resolve_context db attr -> false
    | Many_values [] | Many_entities [] -> false
    | One_entity _ | Many_entities _ -> true
    | One_value _ | Many_values _ -> true
  and tx_entity_has_assertions (entity : tx_entity) =
    List.exists (fun (attr, tx_value) -> tx_value_has_assertions attr tx_value) entity.attrs
  in
  let remember_removed_schema_ident entity_id ident =
    if not (List.mem ident !removed_schema_attrs) then
      removed_schema_attrs := ident :: !removed_schema_attrs;
    if not (List.mem entity_id !ignored_schema_entities) then
      ignored_schema_entities := entity_id :: !ignored_schema_entities
  in
  let note_schema_ident_retraction datoms entity_id = function
    | Some (Keyword ident) -> remember_removed_schema_ident entity_id ident
    | None ->
      (match context.current_attr_value datoms entity_id "db/ident" with
       | Some (Keyword ident) -> remember_removed_schema_ident entity_id ident
       | _ -> ())
    | Some _ -> ()
  in
  let note_schema_field_retraction datoms entity_id field =
    if List.mem field context.schema_fields then
      match context.current_attr_value datoms entity_id "db/ident" with
      | Some (Keyword ident) ->
        let removed = ident, field in
        if not (List.mem removed !removed_schema_fields) then
          removed_schema_fields := removed :: !removed_schema_fields
      | _ -> ()
  in
  let add_resolved_attr_value e attr value (datoms, max_eid, tempids, entity_tempids, tx_data) =
    let db = current_db () in
    let datoms, datom_tx_data = context.add_entity_attr_value db tx datoms e attr value in
    let tx_data = append_tx_data tx_data datom_tx_data in
    (* upstream updates the schema after each schema-field datom, so later
       datoms of the same entity see it *)
    (if attr = "db/ident" || List.mem attr context.schema_fields then
       refresh_schema datoms);
    datoms, max_eid, tempids, entity_tempids, tx_data
  in
  let upsert_conflict tempid old_e target_e tempids =
    (* upstream raises "Conflicting upsert" only once a tempid is bound to an
       existing entity; a freshly-allocated eid resolves via retry-with-tempid
       instead, so the allocated eid can never collide with a later explicit
       entity id. *)
    if old_e <= (current_db ()).max_eid then
      invalid_arg
        ("Conflicting upsert: "
         ^ tempid
         ^ " resolves both to "
         ^ string_of_int old_e
         ^ " and "
         ^ string_of_int target_e)
    else
      raise
        (Upsert_retry
           (tempid, target_e, (tempid, target_e) :: List.remove_assoc tempid (list_of_tempid_map tempids)))
  in
  let tuple_identity_target_for_add datoms e attr value =
    let db = current_db () in
    context.tuple_attrs_for_source db attr
    |> List.find_map (fun (tuple_attr, source_attrs) ->
      if context.is_unique_identity db tuple_attr then
        let values =
          List.map
            (fun source_attr ->
              if source_attr = attr then Some value
              else context.current_attr_value datoms e source_attr)
            source_attrs
        in
        if List.for_all Option.is_some values then
          match context.resolve_context.entid datoms tuple_attr (Tuple values) with
          | Some target_e when target_e <> e -> Some target_e
          | _ -> None
        else
          None
      else
        None)
  in
  let resolve_add_tempid datoms max_eid tempids tx_data tempid attr value =
    let db = current_db () in
    if context.is_unique_identity db attr then
      match context.resolve_context.entid datoms attr value, tempid_find tempids tempid with
      | Some target_e, Some old_e when old_e <> target_e ->
        upsert_conflict tempid old_e target_e tempids
      | Some target_e, _ ->
        target_e, datoms, context.resolve_context.max_eid_with_entity_id max_eid target_e, remember_tempid_m tempids tempid target_e, tx_data
      | None, _ ->
        let e, max_eid, tempids = resolve_entity_ref_m context.resolve_context db datoms tx max_eid tempids (Temp_id tempid) in
        e, datoms, max_eid, tempids, tx_data
    else
      let e, max_eid, tempids = resolve_entity_ref_m context.resolve_context db datoms tx max_eid tempids (Temp_id tempid) in
      match tuple_identity_target_for_add datoms e attr value with
      | Some target_e -> upsert_conflict tempid e target_e tempids
      | None -> e, datoms, max_eid, tempids, tx_data
  in
  let is_forward_nested_attr = function
    | attr, (One_entity _ | Many_entities _) -> not (context.resolve_context.is_reverse_ref attr)
    | _ -> false
  in
  let has_only_forward_nested_attrs (entity : tx_entity) =
    entity.attrs <> [] && List.for_all is_forward_nested_attr entity.attrs
  in
  let rec tx_value_has_schema_fields = function
    | One_value _ | Many_values _ -> false
    | One_entity entity -> tx_entity_has_schema_fields entity
    | Many_entities entities -> List.exists tx_entity_has_schema_fields entities
  and tx_entity_has_schema_fields entity =
    entity.attrs
    |> List.exists (fun (attr, value) -> attr = "db/ident" || List.mem attr context.schema_fields || tx_value_has_schema_fields value)
  in
  let tx_op_affects_schema = function
    | Add (_, attr, _) | Raw_datom { a = attr; _ } ->
      attr = "db/ident" || List.mem attr context.schema_fields
    | Entity entity -> tx_entity_has_schema_fields entity
    | Retract _ | RetractEntity _ | RetractAttr _ -> true
    | CompareAndSet (_, attr, _, _) -> attr = "db/ident" || List.mem attr context.schema_fields
    | InstallTxFn _ | CallIdent _ | Call _ -> false
  in
  let resolve_transaction_function_ref datoms max_eid tempids entity_ref =
    match entity_ref with
    | Ident ident ->
      (match context.resolve_context.entid datoms context.resolve_context.ident_attr (Keyword ident) with
       | Some e -> e, context.resolve_context.max_eid_with_entity_id max_eid e, tempids
       | None -> invalid_arg ("Cannot find entity for transaction fn: " ^ ident))
    | _ ->
      resolve_entity_ref_m context.resolve_context (current_db ()) datoms tx max_eid tempids entity_ref
  in
  let resolve_call_args datoms max_eid tempids args =
    args
    |> List.fold_left
         (fun (args, max_eid, tempids) arg ->
           let arg, max_eid, tempids = resolve_value_m context.resolve_context (current_db ()) datoms tx max_eid tempids arg in
           arg :: args, max_eid, tempids)
         ([], max_eid, tempids)
    |> fun (args, max_eid, tempids) -> List.rev args, max_eid, tempids
  in
  let rec apply_op (datoms, max_eid, tempids, entity_tempids, tx_data) tx_op =
    let db = current_db () in
    match tx_op with
    | Add (e, a, v) ->
      let entity_ref = e in
      let e, v, datoms, max_eid, tempids, tx_data =
        match e with
        | Temp_id tempid ->
          let v, max_eid, tempids = resolve_value_for_attr_m context.resolve_context db a datoms tx max_eid tempids v in
          let e, datoms, max_eid, tempids, tx_data =
            resolve_add_tempid datoms max_eid tempids tx_data tempid a v
          in
          e, v, datoms, max_eid, tempids, tx_data
        | _ ->
          let e, max_eid, tempids = resolve_entity_ref_m context.resolve_context db datoms tx max_eid tempids e in
          let v, max_eid, tempids = resolve_value_for_attr_m context.resolve_context db a datoms tx max_eid tempids v in
          e, v, datoms, max_eid, tempids, tx_data
      in
      let entity_tempids = mark_entity_tempid entity_tempids entity_ref in
      let d = context.datom ~tx ~e ~a ~v () in
      let datoms, datom_tx_data = context.add_user_datom_with_report db tx datoms d in
      datoms, max_eid, tempids, entity_tempids, append_tx_data tx_data datom_tx_data
    | Retract (e, a, value) ->
      let e, max_eid, tempids = resolve_optional_existing_entity_ref_m context.resolve_context db datoms tx max_eid tempids e in
      (match e with
       | None -> datoms, max_eid, tempids, entity_tempids, tx_data
       | Some e ->
         let value, max_eid, tempids = resolve_optional_value_for_attr_m context.resolve_context db a datoms tx max_eid tempids value in
         if a = "db/ident" then note_schema_ident_retraction datoms e value;
         note_schema_field_retraction datoms e a;
         let datoms, datom_tx_data = context.retract_user_attr_with_report db tx datoms e a value in
         datoms, max_eid, tempids, entity_tempids, append_tx_data tx_data datom_tx_data)
    | RetractEntity e ->
      let e, max_eid, tempids = resolve_optional_existing_entity_ref_m context.resolve_context db datoms tx max_eid tempids e in
      (match e with
       | None -> datoms, max_eid, tempids, entity_tempids, tx_data
       | Some e ->
         note_schema_ident_retraction datoms e None;
         let datoms, datom_tx_data = context.retract_entity_with_report db tx datoms e in
         datoms, max_eid, tempids, entity_tempids, append_tx_data tx_data datom_tx_data)
    | RetractAttr (e, a) ->
      let e, max_eid, tempids = resolve_optional_existing_entity_ref_m context.resolve_context db datoms tx max_eid tempids e in
      (match e with
       | None -> datoms, max_eid, tempids, entity_tempids, tx_data
       | Some e ->
         if a = "db/ident" then note_schema_ident_retraction datoms e None;
         note_schema_field_retraction datoms e a;
         let datoms, datom_tx_data = context.retract_user_attr_with_report db tx datoms e a None in
         datoms, max_eid, tempids, entity_tempids, append_tx_data tx_data datom_tx_data)
    | CompareAndSet (e, a, expected, new_value) ->
      let e, max_eid, tempids = resolve_existing_entity_ref_m context.resolve_context db datoms tx max_eid tempids e in
      let expected, max_eid, tempids = resolve_optional_value_for_attr_m context.resolve_context db a datoms tx max_eid tempids expected in
      let new_value, max_eid, tempids = resolve_value_for_attr_m context.resolve_context db a datoms tx max_eid tempids new_value in
      let tx_db = db_with_current_metadata datoms in
      if not (context.compare_and_set_matches tx_db e a expected) then
        invalid_arg (context.compare_and_set_failure_message tx_db e a expected);
      let d = context.datom ~tx ~e ~a ~v:new_value () in
      let datoms, datom_tx_data = context.add_user_datom_with_report db tx datoms d in
      datoms, max_eid, tempids, entity_tempids, append_tx_data tx_data datom_tx_data
    | Raw_datom d ->
      let d = context.normalize_datom_for_schema db.schema d in
      max_tx_seen := max !max_tx_seen d.tx;
      if d.added then
        let datoms, datom_tx_data =
          context.add_active_datom_with_report ~allow_tuple:true ~validate_value:false db d.tx datoms d
        in
        (* upstream with-datom advances max-eid on added datom.e only *)
        datoms, context.resolve_context.max_eid_with_entity_id max_eid d.e, tempids, entity_tempids, append_tx_data tx_data datom_tx_data
      else
        begin
        if d.a = "db/ident" then note_schema_ident_retraction datoms d.e (Some d.v);
        note_schema_field_retraction datoms d.e d.a;
        let datoms, datom_tx_data = context.retract_active_datom_with_report d.tx datoms d.e d.a (Some d.v) in
        datoms, max_eid, tempids, entity_tempids, append_tx_data tx_data datom_tx_data
        end
    | Call f ->
      let db_for_call = { (db_with_current_metadata datoms) with max_eid } in
      apply_ops (datoms, max_eid, tempids, entity_tempids, tx_data) (f db_for_call)
    | InstallTxFn (entity_ref, f) ->
      let e, max_eid, tempids = resolve_transaction_function_ref datoms max_eid tempids entity_ref in
      current_tx_fns := (e, f) :: List.remove_assoc e !current_tx_fns;
      datoms, max_eid, tempids, mark_entity_tempid entity_tempids entity_ref, tx_data
    | CallIdent (entity_ref, args) ->
      let e, max_eid, tempids = resolve_transaction_function_ref datoms max_eid tempids entity_ref in
      let args, max_eid, tempids = resolve_call_args datoms max_eid tempids args in
      (match List.assoc_opt e !current_tx_fns with
       | Some f ->
         let db_for_call = { (db_with_current_metadata datoms) with max_eid } in
         apply_ops (datoms, max_eid, tempids, entity_tempids, tx_data) (f db_for_call args)
       | None -> invalid_arg "Entity expected to have transaction function metadata")
    | Entity entity when not (tx_entity_has_assertions entity) ->
      datoms, max_eid, tempids, entity_tempids, tx_data
    | Entity entity ->
      let datoms, max_eid, tempids, entity_tempids, tx_data, _ =
        apply_entity_map (datoms, max_eid, tempids, entity_tempids, tx_data) entity
      in
      datoms, max_eid, tempids, entity_tempids, tx_data
  and apply_entity_map (datoms, max_eid, tempids, entity_tempids, tx_data) entity =
    let db = current_db () in
    let entity =
      { entity with attrs = List.filter (fun (attr, _) -> attr <> "db/id") entity.attrs }
    in
    if entity.db_id = None && has_only_forward_nested_attrs entity then
      apply_nested_first_entity_map (datoms, max_eid, tempids, entity_tempids, tx_data) entity
    else
      (* Upsert probes resolve non-strictly against the pre-entity datoms:
         unresolvable refs keep their raw form and simply never match. Strict
         resolution happens per attr at add time, matching upstream's
         sequential [:db/add] resolution order. *)
      (* the probe resolves values on a frozen tempid map: minting eids for
         referenced tempids here would both leak writes into the real map and
         mint them before the entity's own :db/id, which upstream resolves
         first. *)
      let probe_attrs =
        let probe_attrs, _, _ =
          List.fold_left
            (fun (probe_attrs, max_eid, tempids) (attr, tx_value) ->
              match
                (try
                   Some
                     (resolve_tx_value_for_attr_m
                        context.resolve_context
                        db
                        attr
                        datoms
                        tx
                        max_eid
                        tempids
                        tx_value)
                 with Invalid_argument _ | Unresolved_lookup_ref _ -> None)
              with
              | Some (tx_value, max_eid, tempids) ->
                (attr, tx_value) :: probe_attrs, max_eid, tempids
              | None -> (attr, tx_value) :: probe_attrs, max_eid, tempids)
            ([], max_eid, { tempids with frozen = true })
            entity.attrs
        in
        List.rev probe_attrs
      in
      let db_id_ref =
        match entity.db_id with
        | Some (Entity_id e) when e < 0 ->
          (* upstream datascript: a negative integer :db/id is a tempid *)
          Some (Temp_id (string_of_int e))
        | db_id -> db_id
      in
      let e, datoms, max_eid, tempids, tx_data =
        match db_id_ref with
        | Some (Temp_id tempid) ->
          (match context.entity_unique_identity db datoms probe_attrs with
           | Some target_e ->
             let datoms, tempids, tx_data =
               match tempid_find tempids tempid with
               | Some old_e when old_e <> target_e ->
                 upsert_conflict tempid old_e target_e tempids
               | Some _ -> datoms, tempids, tx_data
               | None -> datoms, remember_tempid_m tempids tempid target_e, tx_data
             in
             target_e, datoms, context.resolve_context.max_eid_with_entity_id max_eid target_e, tempids, tx_data
           | None ->
             let e, max_eid, tempids =
               resolve_entity_ref_m context.resolve_context db datoms tx max_eid tempids (Temp_id tempid)
             in
             e, datoms, max_eid, tempids, tx_data)
        | Some entity_ref ->
          let e, max_eid, tempids = resolve_entity_ref_m context.resolve_context db datoms tx max_eid tempids entity_ref in
          context.validate_explicit_upsert_target db datoms e probe_attrs;
          e, datoms, max_eid, tempids, tx_data
        | None ->
          let e = context.resolve_context.allocate_entity_id max_eid in
          (match context.entity_unique_identity db datoms probe_attrs with
           | Some e -> e, datoms, context.resolve_context.max_eid_with_entity_id max_eid e, tempids, tx_data
           | None -> e, datoms, context.resolve_context.max_eid_with_entity_id max_eid e, tempids, tx_data)
      in
      let entity_tempids =
        match db_id_ref with
        | Some entity_ref -> mark_entity_tempid entity_tempids entity_ref
        | None -> entity_tempids
      in
      let tuple_identity_lookup_writes =
        probe_attrs
        |> List.filter_map (function
          | attr, One_value value when context.is_tuple_attr db attr && context.is_unique_identity db attr ->
            (match context.resolve_context.entid datoms attr value with
             | Some target_e when target_e = e -> Some (attr, value)
             | _ -> None)
          | _ -> None)
      in
      let tuple_identity_write_was_lookup attr value =
        List.exists
          (fun (lookup_attr, lookup_value) ->
            lookup_attr = attr && context.value_equal lookup_value value)
          tuple_identity_lookup_writes
      in
      let add_entity_map_attr_value
            parent_e
            attr
            value
            (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
        =
        let db = current_db () in
        let actual_e, actual_attr, actual_value = context.normalize_entity_attr_value db parent_e attr value in
        if context.is_tuple_attr db actual_attr then
          if tuple_identity_write_was_lookup actual_attr actual_value then
            datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes
          else
            ( datoms
            , max_eid
            , tempids
            , entity_tempids
            , tx_data
            , tuple_sources
            , (actual_e, actual_attr, actual_value) :: direct_tuple_writes )
        else if context.tuple_attrs_for_source db actual_attr <> [] then
          let datoms, datom_tx_data =
            context.add_active_datom_with_report db tx datoms (context.datom ~tx ~e:actual_e ~a:actual_attr ~v:actual_value ())
          in
          ( datoms
          , max_eid
          , tempids
          , entity_tempids
          , append_tx_data tx_data datom_tx_data
          , (actual_e, actual_attr) :: tuple_sources
          , direct_tuple_writes )
        else
          let datoms, max_eid, tempids, entity_tempids, tx_data =
            add_resolved_attr_value parent_e attr value (datoms, max_eid, tempids, entity_tempids, tx_data)
          in
          datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes
      in
      let apply_nested_entity
            parent_e
            attr
            (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
            (nested : tx_entity)
        =
        let db = current_db () in
        if context.resolve_context.is_reverse_ref attr then
          begin
          if not (context.resolve_context.is_ref_attr db (context.resolve_context.reverse_ref attr)) then
            invalid_arg "reverse nested entity attribute requires ref schema";
          let nested = { nested with attrs = nested.attrs @ [ context.resolve_context.reverse_ref attr, One_value (Ref parent_e) ] } in
          let datoms, max_eid, tempids, entity_tempids, tx_data, _ =
            apply_entity_map (datoms, max_eid, tempids, entity_tempids, tx_data) nested
          in
          datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes
          end
        else
          begin
          if not (context.resolve_context.is_ref_attr db attr) then
            invalid_arg "nested entity attribute requires ref schema";
          let datoms, max_eid, tempids, entity_tempids, tx_data, nested_e =
            apply_entity_map (datoms, max_eid, tempids, entity_tempids, tx_data) nested
          in
          add_entity_map_attr_value
            parent_e
            attr
            (Ref nested_e)
            (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
          end
      in
      let apply_attr (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes) (attr, tx_value) =
        let db = current_db () in
        let tx_value, max_eid, tempids =
          resolve_tx_value_for_attr_m context.resolve_context db attr datoms tx max_eid tempids tx_value
        in
        match tx_value with
        | One_value (List values | Vector values) when attr_expands_collection context.resolve_context db attr ->
          List.fold_left
            (fun state value -> add_entity_map_attr_value e attr value state)
            (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
            values
        | One_value (Set values) when attr_expands_collection context.resolve_context db attr ->
          List.fold_left
            (fun state value -> add_entity_map_attr_value e attr value state)
            (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
            values
        | One_value value ->
          add_entity_map_attr_value e attr value (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
        | Many_values values ->
          List.fold_left
            (fun state value -> add_entity_map_attr_value e attr value state)
            (datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes)
            values
        | One_entity nested ->
          let state = datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes in
          if context.resolve_context.is_reverse_ref attr || context.resolve_context.is_ref_attr db attr then
            apply_nested_entity e attr state nested
          else
            add_entity_map_attr_value e attr (Map (map_entries_of_tx_entity nested)) state
        | Many_entities nested_entities ->
          let state = datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes in
          if context.resolve_context.is_reverse_ref attr || context.resolve_context.is_ref_attr db attr then
            List.fold_left (apply_nested_entity e attr) state nested_entities
          else
            add_entity_map_attr_value e attr
              (Set (List.map (fun nested -> Map (map_entries_of_tx_entity nested)) nested_entities))
              state
      in
      let datoms, max_eid, tempids, entity_tempids, tx_data, tuple_sources, direct_tuple_writes =
        List.fold_left apply_attr (datoms, max_eid, tempids, entity_tempids, tx_data, [], []) entity.attrs
      in
      let tuple_sources = List.sort_uniq compare tuple_sources in
      let datoms, tx_data =
        List.fold_left
          (fun (datoms, tx_data) (entity_id, source_attr) ->
            let datoms, tuple_tx_data = context.refresh_tuple_attrs_for_source db tx datoms entity_id source_attr [] in
            datoms, append_tx_data tx_data tuple_tx_data)
          (datoms, tx_data)
          tuple_sources
      in
      List.iter
        (fun (e, a, v) ->
          if not (context.tuple_direct_write_matches_sources db datoms (context.datom ~tx ~e ~a ~v ())) then
            invalid_arg "cannot modify tuple attributes directly")
        direct_tuple_writes;
      datoms, max_eid, tempids, entity_tempids, tx_data, e
  and apply_nested_first_entity_map state entity =
      let db = current_db () in
      let transact_nested state nested =
        let datoms, max_eid, tempids, entity_tempids, tx_data, nested_e =
          apply_entity_map state nested
        in
        (datoms, max_eid, tempids, entity_tempids, tx_data), Ref nested_e
      in
      let transact_nested_attr (state, attrs) (attr, tx_value) =
        if not (context.resolve_context.is_ref_attr db attr) then
          invalid_arg "nested entity attribute requires ref schema";
        match tx_value with
        | One_entity nested ->
          let state, ref_value = transact_nested state nested in
          state, (attr, One_value ref_value) :: attrs
        | Many_entities nested_entities ->
          let state, values =
            List.fold_left
              (fun (state, values) nested ->
                let state, ref_value = transact_nested state nested in
                state, ref_value :: values)
              (state, [])
              nested_entities
          in
          state, (attr, Many_values (List.rev values)) :: attrs
        | One_value _ | Many_values _ -> state, attrs
      in
      let (datoms, max_eid, tempids, entity_tempids, tx_data), attrs =
        List.fold_left transact_nested_attr (state, []) entity.attrs
      in
      let attrs = List.rev attrs in
      let e, max_eid =
        match context.entity_unique_identity db datoms attrs with
        | Some e -> e, context.resolve_context.max_eid_with_entity_id max_eid e
        | None ->
          let e = context.resolve_context.allocate_entity_id max_eid in
          e, context.resolve_context.max_eid_with_entity_id max_eid e
      in
      let apply_attr (datoms, max_eid, tempids, entity_tempids, tx_data) (attr, tx_value) =
        match tx_value with
        | One_value value -> add_resolved_attr_value e attr value (datoms, max_eid, tempids, entity_tempids, tx_data)
        | Many_values values ->
          List.fold_left
            (fun state value -> add_resolved_attr_value e attr value state)
            (datoms, max_eid, tempids, entity_tempids, tx_data)
            values
        | One_entity _ | Many_entities _ -> datoms, max_eid, tempids, entity_tempids, tx_data
      in
      let datoms, max_eid, tempids, entity_tempids, tx_data =
        List.fold_left apply_attr (datoms, max_eid, tempids, entity_tempids, tx_data) attrs
      in
      datoms, max_eid, tempids, entity_tempids, tx_data, e
  and try_apply_bulk_explicit_entities () =
    let rec has_complex_ref_value value =
      match value with
      | Ref_to _ | TxRef -> true
      | List values | Vector values | Set values -> List.exists has_complex_ref_value values
      | Map entries ->
        List.exists
          (fun (key, value) -> has_complex_ref_value key || has_complex_ref_value value)
          entries
      | Tuple values ->
        List.exists
          (function
            | None -> false
            | Some value -> has_complex_ref_value value)
          values
      | Nil | Int _ | Float _ | String _ | Symbol _ | Bool _ | Keyword _ | Uuid _ | Instant _ | Regex _ | Ref _ -> false
    in
    let unique_attr attr =
      attr = "db/ident"
      ||
      match List.assoc_opt attr db.schema with
      | Some { unique = Some _; _ } -> true
      | _ -> false
    in
    let attr_is_supported attr =
      let supported_value_type =
        match List.assoc_opt attr db.schema with
        | Some { value_type = Some TupleType; _ }
        | Some { value_type = Some StringType; _ }
        | Some { value_type = Some KeywordType; _ }
        | Some { value_type = Some NumberType; _ }
        | Some { value_type = Some UuidType; _ }
        | Some { value_type = Some InstantType; _ } ->
          false
        | Some { value_type = Some RefType; _ } | Some { value_type = None; _ } | None -> true
      in
      attr <> "db/id"
      && not (String.starts_with ~prefix:"db/" attr)
      && not (context.resolve_context.is_reverse_ref attr)
      && supported_value_type
      && not (context.is_tuple_attr db attr)
      && context.tuple_attrs_for_source db attr = []
    in
    let simple_lookup_value = function
      | Nil | Ref_to _ | TxRef | List _ | Vector _ | Map _ | Set _ | Tuple _ -> false
      | Int _ | Float _ | String _ | Symbol _ | Bool _ | Keyword _ | Uuid _ | Instant _ | Regex _ | Ref _ -> true
    in
    let supported_ref_lookup = function
      | Ref_to (Lookup_ref (lookup_attr, lookup_value)) ->
        unique_attr lookup_attr && simple_lookup_value lookup_value
      | _ -> false
    in
    let supported_value attr value =
      value <> Nil
      &&
      if context.resolve_context.is_ref_attr db attr then
        match value with
        | Ref _ | Int _ -> true
        | value when supported_ref_lookup value -> true
        | _ -> false
      else
        not (has_complex_ref_value value)
    in
    let supported_tx_value attr = function
      | One_value (List _ | Vector _ | Set _) when context.resolve_context.cardinality db attr = Many ->
        false
      | One_value value | Many_values [ value ] -> supported_value attr value
      | Many_values values -> List.for_all (supported_value attr) values
      | One_entity _ | Many_entities _ -> false
    in
    let supported_entity = function
      | Entity { db_id = Some (Entity_id _ | Temp_id _); attrs } ->
        attrs <> []
        && List.for_all (fun (attr, tx_value) -> attr_is_supported attr && supported_tx_value attr tx_value) attrs
      | _ -> false
    in
    let supported_add = function
      | Add (Entity_id _, attr, value) ->
        attr_is_supported attr
        && supported_value attr value
      | Add (Lookup_ref (lookup_attr, lookup_value), attr, value) ->
        unique_attr lookup_attr
        && supported_value lookup_attr lookup_value
        && attr_is_supported attr
        && supported_value attr value
      | Add (Temp_id tempid, attr, value) ->
        not (is_current_tx_alias tempid)
        && attr_is_supported attr
        && supported_value attr value
      | _ -> false
    in
    let supported_tx_op tx_op =
      supported_entity tx_op || supported_add tx_op
    in
    let duplicate_cardinality_one attrs =
      let seen = Hashtbl.create (List.length attrs) in
      List.exists
        (fun (attr, _) ->
          context.resolve_context.cardinality db attr = One
          &&
          if Hashtbl.mem seen attr then true
          else (
            Hashtbl.add seen attr ();
            false ))
        attrs
    in
    let duplicate_fact facts =
      let seen = Hashtbl.create (List.length facts) in
      List.exists
        (fun d ->
          let key = d.e, d.a, d.v in
          if Hashtbl.mem seen key then true
          else (
            Hashtbl.add seen key ();
            false ))
        facts
    in
    let duplicate_unique facts =
      let seen = Hashtbl.create (List.length facts) in
      List.exists
        (fun d ->
          unique_attr d.a
          &&
          let key = d.a, d.v in
          match Hashtbl.find_opt seen key with
          | Some entity_id -> entity_id <> d.e
          | None ->
            Hashtbl.add seen key d.e;
            false)
        facts
    in
    let duplicate_cardinality_one_fact facts =
      let seen = Hashtbl.create (List.length facts) in
      List.exists
        (fun d ->
          context.resolve_context.cardinality db d.a = One
          &&
          let key = d.e, d.a in
          if Hashtbl.mem seen key then true
          else (
            Hashtbl.add seen key ();
            false ))
        facts
    in
    let entity_is_new d =
      d.e > db.max_datom_e
    in
    let existing_unique_conflict d =
      unique_attr d.a
      &&
      match context.existing_unique_entity db d.a d.v with
      | Some entity_id -> entity_id <> d.e
      | None -> false
    in
    let conflicts_with_existing facts =
      List.exists existing_unique_conflict facts
    in
    let retraction_datom d =
      { d with tx; added = false }
    in
    let compare_eavt_datom left right =
      compare
        (left.e, left.a, left.v, left.tx)
        (right.e, right.a, right.v, right.tx)
    in
    let existing_attr_datoms d =
      if entity_is_new d then [] else context.existing_entity_attr_datoms db d.e d.a
    in
    let tx_data_for_fact d =
      let d = { d with v = context.resolve_context.normalize_value d.v } in
      let existing = existing_attr_datoms d in
      let same_fact_exists = List.exists (context.same_fact d) existing in
      match context.resolve_context.cardinality db d.a with
      | Many -> if same_fact_exists then [] else [ d ]
      | One ->
        if same_fact_exists then
          []
        else
          (existing
           |> List.sort compare_eavt_datom
           |> List.map retraction_datom)
          @ [ d ]
    in
    let resolve_fast_value_for_attr attr value max_eid =
      match value with
      | Ref_to (Lookup_ref (lookup_attr, lookup_value)) when context.resolve_context.is_ref_attr db attr ->
        let lookup_value, max_eid, _ =
          resolve_value_for_attr_m
            context.resolve_context
            db
            lookup_attr
            db
            tx
            max_eid
            (tempid_map_empty ())
            lookup_value
        in
        (match context.existing_unique_entity db lookup_attr lookup_value with
         | Some entity_id -> Ref entity_id, context.resolve_context.max_eid_with_entity_id max_eid entity_id
         | None -> raise (Unresolved_lookup_ref (lookup_attr, lookup_value)))
      | _ ->
        let value, max_eid, _ =
          resolve_value_for_attr_m
            context.resolve_context
            db
            attr
            db
            tx
            max_eid
            (tempid_map_empty ())
            value
        in
        value, max_eid
    in
    let resolve_fast_tx_value attr max_eid = function
      | One_value value ->
        let value, max_eid = resolve_fast_value_for_attr attr value max_eid in
        One_value value, max_eid
      | Many_values values ->
        let values, max_eid =
          values
          |> List.fold_left
               (fun (values, max_eid) value ->
                 let value, max_eid = resolve_fast_value_for_attr attr value max_eid in
                 value :: values, max_eid)
               ([], max_eid)
        in
        Many_values (List.rev values), max_eid
      | One_entity _ | Many_entities _ as tx_value -> tx_value, max_eid
    in
    let resolve_fast_attrs max_eid attrs =
      attrs
      |> List.fold_left
           (fun (attrs, max_eid) (attr, tx_value) ->
             let tx_value, max_eid = resolve_fast_tx_value attr max_eid tx_value in
             (attr, tx_value) :: attrs, max_eid)
           ([], max_eid)
      |> fun (attrs, max_eid) -> List.rev attrs, max_eid
    in
    let facts_for_resolved_attrs entity_id resolved_attrs =
      resolved_attrs
      |> List.concat_map (fun (attr, tx_value) ->
        match tx_value with
        | One_value value -> [ context.datom ~tx ~e:entity_id ~a:attr ~v:value () ]
        | Many_values values ->
          List.map
            (fun value -> context.datom ~tx ~e:entity_id ~a:attr ~v:value ())
            values
        | One_entity _ | Many_entities _ -> [])
    in
    let unique_identity_target resolved_attrs =
      let targets =
        resolved_attrs
        |> List.concat_map (function
          | attr, One_value value when context.is_unique_identity db attr ->
            (match context.existing_unique_entity db attr value with
             | Some entity_id -> [ attr, value, entity_id ]
             | None -> [])
          | attr, Many_values values when context.is_unique_identity db attr ->
            values
            |> List.filter_map (fun value ->
              Option.map (fun entity_id -> attr, value, entity_id) (context.existing_unique_entity db attr value))
          | _ -> [])
      in
      match targets with
      | [] -> Some None
      | (_, _, entity_id) :: rest ->
        if List.for_all (fun (_, _, other_entity_id) -> other_entity_id = entity_id) rest then
          Some (Some entity_id)
        else
          None
    in
    let has_unique_identity_attr attrs =
      List.exists
        (function
          | attr, One_value _ | attr, Many_values _ -> context.is_unique_identity db attr
          | _ -> false)
        attrs
    in
    let remember_fast_tempid = remember_tempid_m in
    let ambiguous_tempid_entity_without_unique =
      let counts = Hashtbl.create (List.length tx_ops) in
      tx_ops
      |> List.iter (function
        | Entity { db_id = Some (Temp_id tempid); _ } | Add (Temp_id tempid, _, _) ->
          let count = Option.value (Hashtbl.find_opt counts tempid) ~default:0 in
          Hashtbl.replace counts tempid (count + 1)
        | _ -> ());
      tx_ops
      |> List.exists (function
        | Entity { db_id = Some (Temp_id tempid); attrs } ->
          (not (has_unique_identity_attr attrs))
          && Option.value (Hashtbl.find_opt counts tempid) ~default:0 > 1
        | _ -> false)
    in
    let bulk_tempid_entities_without_unique =
      tx_ops
      |> List.filter (function
        | Entity { db_id = Some (Temp_id _); attrs } -> not (has_unique_identity_attr attrs)
        | _ -> false)
      |> List.length
      |> fun count -> count > 1
    in
    let try_new_tempid_entities () =
      let tempid_counts = Hashtbl.create (List.length tx_ops) in
      let count_tempid tempid =
        let count = Option.value (Hashtbl.find_opt tempid_counts tempid) ~default:0 in
        Hashtbl.replace tempid_counts tempid (count + 1)
      in
      tx_ops
      |> List.iter (function
        | Entity { db_id = Some (Temp_id tempid); _ } -> count_tempid tempid
        | _ -> ());
      let supported_new_entity = function
        | Entity { db_id = Some (Temp_id tempid); attrs } ->
          Option.value (Hashtbl.find_opt tempid_counts tempid) ~default:0 = 1
          && not (is_current_tx_alias tempid)
          && not (has_unique_identity_attr attrs)
          && attrs <> []
          && not (duplicate_cardinality_one attrs)
          && List.for_all (fun (attr, tx_value) -> attr_is_supported attr && supported_tx_value attr tx_value) attrs
        | _ -> false
      in
      if not (List.for_all supported_new_entity tx_ops) then
        None
      else
        let add_entity_fact facts_rev fact =
          if List.exists (context.same_fact fact) facts_rev then facts_rev else fact :: facts_rev
        in
        let prepend_entity_facts entity_id resolved_attrs facts_rev =
          let entity_facts_rev =
            resolved_attrs
            |> List.fold_left
                 (fun entity_facts_rev (attr, tx_value) ->
                   match tx_value with
                   | One_value value ->
                     add_entity_fact
                       entity_facts_rev
                       (context.datom ~tx ~e:entity_id ~a:attr ~v:value ())
                   | Many_values values ->
                     List.fold_left
                       (fun entity_facts_rev value ->
                         add_entity_fact
                           entity_facts_rev
                           (context.datom ~tx ~e:entity_id ~a:attr ~v:value ()))
                       entity_facts_rev
                       values
                   | One_entity _ | Many_entities _ -> entity_facts_rev)
                 []
          in
          List.rev_append (List.rev entity_facts_rev) facts_rev
        in
        let facts_rev, max_eid, tempids_rev, entity_tempids =
          tx_ops
          |> List.fold_left
               (fun (facts_rev, max_eid, tempids_rev, entity_tempids) -> function
                 | Entity { db_id = Some (Temp_id tempid); attrs } ->
                   let entity_id = context.resolve_context.allocate_entity_id max_eid in
                   let max_eid = context.resolve_context.max_eid_with_entity_id max_eid entity_id in
                   let resolved_attrs, max_eid = resolve_fast_attrs max_eid attrs in
                   ( prepend_entity_facts entity_id resolved_attrs facts_rev
                   , max_eid
                   , tempid_remember_unchecked tempids_rev tempid entity_id
                   , mark_entity_tempid entity_tempids (Temp_id tempid) )
                 | _ -> invalid_arg "new tempid entity fast path expects entity maps")
               ([], initial_max_eid, tempid_map_empty (), [])
        in
        let tx_data = List.rev facts_rev in
        Some
          ( db
          , max_eid
          , tempids_rev
          , entity_tempids
          , tx_data )
    in
    match try_new_tempid_entities () with
    | Some result -> Some result
    | None when not (List.for_all supported_tx_op tx_ops) -> None
    | None when ambiguous_tempid_entity_without_unique -> None
    | None when bulk_tempid_entities_without_unique -> None
    | None ->
      let add_tempid_attr groups tempid attr value =
        let rec loop = function
          | [] -> [ tempid, [ attr, value ] ]
          | (existing_tempid, attrs) :: rest when existing_tempid = tempid ->
            (existing_tempid, attrs @ [ attr, value ]) :: rest
          | group :: rest -> group :: loop rest
        in
        loop groups
      in
      let tempid_add_groups =
        tx_ops
        |> List.fold_left
             (fun groups -> function
               | Add (Temp_id tempid, attr, value) -> add_tempid_attr groups tempid attr value
               | _ -> groups)
             []
      in
      let resolve_tempid_add_attrs max_eid attrs =
        attrs
        |> List.map (fun (attr, value) -> attr, One_value value)
        |> resolve_fast_attrs max_eid
      in
      let prepare_tempid_adds max_eid =
        let rec loop max_eid tempids entity_tempids = function
          | [] -> Some (max_eid, tempids, entity_tempids)
          | (tempid, attrs) :: rest ->
            let resolved_attrs, max_eid = resolve_tempid_add_attrs max_eid attrs in
            if duplicate_cardinality_one resolved_attrs then
              None
            else
              (match unique_identity_target resolved_attrs with
               | None -> None
               | Some target ->
                 let existing_tempid = tempid_find tempids tempid in
                 let entity_id, max_eid =
                   match target, existing_tempid with
                   | Some target_e, Some old_e when old_e <> target_e ->
                     upsert_conflict tempid old_e target_e tempids
                   | Some entity_id, _ ->
                     entity_id, context.resolve_context.max_eid_with_entity_id max_eid entity_id
                   | None, Some entity_id -> entity_id, max_eid
                   | None, None ->
                     let entity_id = context.resolve_context.allocate_entity_id max_eid in
                     entity_id, context.resolve_context.max_eid_with_entity_id max_eid entity_id
                 in
                 loop
                   max_eid
                   (remember_fast_tempid tempids tempid entity_id)
                   (mark_entity_tempid entity_tempids (Temp_id tempid))
                   rest)
        in
        loop max_eid (tempid_map_empty ()) [] tempid_add_groups
      in
      let build_entity (facts_rev, max_eid, tempids, entity_tempids) = function
        | Entity { db_id = Some (Entity_id entity_id); attrs } ->
          if duplicate_cardinality_one attrs then
            None
          else
            let entity_id = context.resolve_context.validate_entity_id entity_id in
            let resolved_attrs, max_eid = resolve_fast_attrs max_eid attrs in
            let entity_facts = facts_for_resolved_attrs entity_id resolved_attrs in
            Some
              ( List.rev_append entity_facts facts_rev
              , context.resolve_context.max_eid_with_entity_id max_eid entity_id
              , tempids
              , entity_tempids )
        | Entity { db_id = Some (Temp_id tempid); attrs } ->
          if duplicate_cardinality_one attrs then
            None
          else
            let resolved_attrs, max_eid = resolve_fast_attrs max_eid attrs in
            (match unique_identity_target resolved_attrs with
             | None -> None
             | Some target ->
               let existing_tempid = tempid_find tempids tempid in
               let entity_id, max_eid =
                 match target, existing_tempid with
                 | Some target_e, Some old_e when old_e <> target_e ->
                   upsert_conflict tempid old_e target_e tempids
                 | Some entity_id, _ ->
                   entity_id, context.resolve_context.max_eid_with_entity_id max_eid entity_id
                 | None, Some entity_id -> entity_id, max_eid
                 | None, None ->
                   let entity_id = context.resolve_context.allocate_entity_id max_eid in
                   entity_id, context.resolve_context.max_eid_with_entity_id max_eid entity_id
               in
               let entity_facts = facts_for_resolved_attrs entity_id resolved_attrs in
               Some
                 ( List.rev_append entity_facts facts_rev
                 , max_eid
                 , remember_fast_tempid tempids tempid entity_id
                 , mark_entity_tempid entity_tempids (Temp_id tempid) ))
        | _ -> None
      in
      let build_add (facts_rev, max_eid, tempids, entity_tempids) = function
        | Add (Entity_id entity_id, attr, value) ->
          let value, max_eid = resolve_fast_value_for_attr attr value max_eid in
          let fact = context.datom ~tx ~e:entity_id ~a:attr ~v:value () in
          Some
            ( fact :: facts_rev
            , context.resolve_context.max_eid_with_entity_id max_eid entity_id
            , tempids
            , entity_tempids )
        | Add (Lookup_ref (lookup_attr, lookup_value), attr, value) ->
          let lookup_value, max_eid, _ =
            resolve_value_for_attr_m context.resolve_context db lookup_attr db tx max_eid (tempid_map_empty ()) lookup_value
          in
          (match context.existing_unique_entity db lookup_attr lookup_value with
           | None -> None
           | Some entity_id ->
             let value, max_eid = resolve_fast_value_for_attr attr value max_eid in
             let fact = context.datom ~tx ~e:entity_id ~a:attr ~v:value () in
             Some (fact :: facts_rev, max_eid, tempids, entity_tempids))
        | Add (Temp_id tempid, attr, value) ->
          (match tempid_find tempids tempid with
           | None -> None
           | Some entity_id ->
             let value, max_eid = resolve_fast_value_for_attr attr value max_eid in
             let fact = context.datom ~tx ~e:entity_id ~a:attr ~v:value () in
             Some (fact :: facts_rev, max_eid, tempids, entity_tempids))
        | _ -> None
      in
      let rec build state = function
        | [] -> Some state
        | tx_op :: rest ->
          let state =
            match tx_op with
            | Entity _ -> build_entity state tx_op
            | Add _ -> build_add state tx_op
            | _ -> None
          in
          (match state with
           | None -> None
           | Some state -> build state rest)
      in
      (match prepare_tempid_adds initial_max_eid with
       | None -> None
       | Some (initial_max_eid, tempids, entity_tempids) ->
      (match build ([], initial_max_eid, tempids, entity_tempids) tx_ops with
       | None -> None
       | Some (facts_rev, max_eid, tempids, entity_tempids) ->
         let facts = List.rev facts_rev in
         if duplicate_fact facts || duplicate_unique facts || duplicate_cardinality_one_fact facts || conflicts_with_existing facts then
           None
         else
           let tx_data = List.concat_map tx_data_for_fact facts in
           (* upstream advances max-eid on added datom.e only, never on v *)
           let max_eid =
             List.fold_left
               (fun max_eid d ->
                 if d.added then context.resolve_context.max_eid_with_entity_id max_eid d.e else max_eid)
               max_eid
               facts
           in
           Some
             ( db
             , max_eid
             , tempids
             , entity_tempids
             , tx_data )))
  and apply_ops state tx_ops =
    List.fold_left
      (fun state tx_op ->
        let _, _, tempids, _, _ = state in
        let saved_rev = tempids.rev in
        (try
          let state = apply_op state tx_op in
          let datoms, _, _, _, _ = state in
          if tx_op_affects_schema tx_op then refresh_schema datoms;
          state
        with Unresolved_lookup_ref (attr, value) ->
          (* The lookup ref target may be defined by a later op in this tx:
             queue the op and retry it once more datoms accumulate; bindings
             minted by the aborted attempt roll back with its store. *)
          restore_tempid_map tempids saved_rev;
          deferred_ops := (tx_op, attr, value) :: !deferred_ops;
          state))
      state
      tx_ops
  in
  let rec drain_deferred_ops state =
    let pending = List.rev !deferred_ops in
    match pending with
    | [] -> state
    | (_, first_attr, first_value) :: _ ->
      deferred_ops := [];
      let progressed = ref false in
      let state =
        List.fold_left
          (fun state (tx_op, _, _) ->
            let _, _, tempids, _, _ = state in
            let saved_rev = tempids.rev in
            (try
              let state = apply_op state tx_op in
              let datoms, _, _, _, _ = state in
              if tx_op_affects_schema tx_op then refresh_schema datoms;
              progressed := true;
              state
            with Unresolved_lookup_ref (attr', value') ->
              restore_tempid_map tempids saved_rev;
              deferred_ops := (tx_op, attr', value') :: !deferred_ops;
              state))
          state
          pending
      in
      (* No progress means the remaining targets never resolve: raise the same
         error upstream raises for a lookup ref that points nowhere. *)
      if not !progressed then
        invalid_arg (context.resolve_context.unresolved_lookup_ref_message first_attr first_value)
      else
        drain_deferred_ops state
  in
  let rec compute_report upserted_tempids seeded_tempids =
    deferred_ops := [];
    max_tx_seen := tx;
    current_schema := db.schema;
    current_tx_fns := db.tx_fns;
    removed_schema_attrs := [];
    removed_schema_fields := [];
    ignored_schema_entities := [];
    try
      (* On a retry pass the seeded tempids keep upserted entities stable, so
         the bulk fast paths are skipped in favor of the general apply_ops. *)
      match
        (if upserted_tempids = [] then
           try try_apply_bulk_explicit_entities () with Unresolved_lookup_ref _ -> None
         else
           None)
      with
      | Some (datoms, max_eid, tempids, entity_tempids, tx_data) ->
        datoms, max_eid, tempids, entity_tempids, tx_data, Some tx_data
      | None ->
        let state = apply_ops (db, initial_max_eid, tempid_map_of_list seeded_tempids, [], []) tx_ops in
        let datoms, max_eid, tempids, entity_tempids, tx_data = drain_deferred_ops state in
        datoms, max_eid, tempids, entity_tempids, tx_data, None
    with Upsert_retry (tempid, target_e, seeded_tempids) ->
      (match List.assoc_opt tempid upserted_tempids with
       | Some previous_e when previous_e <> target_e ->
         invalid_arg
           ("Conflicting upsert: "
            ^ tempid
            ^ " resolves both to "
            ^ string_of_int target_e
            ^ " and "
            ^ string_of_int previous_e)
       | _ -> compute_report ((tempid, target_e) :: upserted_tempids) seeded_tempids)
  in
  let datoms, max_eid, tempids, entity_tempids, tx_data, fast_tx_data =
    compute_report [] []
  in
  let tx_data =
    match fast_tx_data with
    | Some tx_data -> tx_data
    | None -> List.rev tx_data
  in
  let tempids = ensure_current_tx_tempid_m tempids tx in
  validate_tempid_usage (list_of_tempid_map tempids) entity_tempids;
  let schema =
    match fast_tx_data with
    | Some _ -> db.schema
    | None ->
      let schema_datoms = context.schema_datoms (db_with_current_metadata datoms) tx_data in
      context.schema_from_transaction_datoms
        ~strict:true
        ~removed_attrs:!removed_schema_attrs
        ~removed_fields:!removed_schema_fields
        ~ignored_schema_entities:!ignored_schema_entities
        db.schema
        schema_datoms
  in
  let db_after =
    { db with
      schema
    ; max_eid
    ; max_tx = !max_tx_seen
    ; tx_fns = !current_tx_fns
    }
  in
  ( (match fast_tx_data with
     | Some _ ->
       db_after
       |> (fun db -> context.refresh_db_indexes_with_tx_data db tx_data)
       |> context.refresh_db_identity
     | None ->
       db_after
       |> (fun db ->
         if List.exists tx_op_affects_schema tx_ops then
           { datoms with
             schema = db.schema
           ; max_eid = db.max_eid
           ; max_tx = db.max_tx
           ; tx_fns = db.tx_fns
           }
         else
           context.refresh_db_indexes_with_tx_data db tx_data)
       |> context.refresh_db_identity)
  , list_of_tempid_map tempids
  , tx_data
  )
