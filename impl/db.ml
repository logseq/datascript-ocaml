open Datascript_types

module Index = Index

let tx0 = 0x20000000

let datom ?(tx = tx0) ?(added = true) ~e ~a ~v () = { e; a; v; tx; added }

let is_datom (_ : datom) = true

type core_context =
  { next_db_uid : unit -> int
  }

let max_entity_id = 0x7fffffff
let max_allocatable_entity_id = tx0 - 1

let validate_entity_id entity_id =
  if entity_id < 0 then
    invalid_arg ("entity id must not be negative: " ^ string_of_int entity_id);
  if entity_id > max_entity_id then
    invalid_arg
      ("Highest supported entity id is "
       ^ string_of_int max_entity_id
       ^ ", got "
       ^ string_of_int entity_id);
  entity_id

let max_eid_with_entity_id max_eid entity_id =
  let entity_id = validate_entity_id entity_id in
  if entity_id <= max_allocatable_entity_id then max max_eid entity_id else max_eid

let refresh_identity context db =
  { db with db_uid = context.next_db_uid () }

let rec max_eid_in_value max_eid = function
  | Ref entity_id -> max_eid_with_entity_id max_eid entity_id
  | List values | Vector values ->
    List.fold_left max_eid_in_value max_eid values
  | Map entries ->
    List.fold_left
      (fun max_eid (key, value) ->
        max_eid_in_value (max_eid_in_value max_eid key) value)
      max_eid
      entries
  | Set values ->
    List.fold_left max_eid_in_value max_eid values
  | Tuple values ->
    List.fold_left
      (fun max_eid -> function
        | None -> max_eid
        | Some value -> max_eid_in_value max_eid value)
      max_eid
      values
  | Nil | Int _ | Float _ | String _ | Symbol _ | Bool _ | Keyword _ | Uuid _ | Instant _ | Regex _ | TxRef | Ref_to _ -> max_eid

let value_equal = Util.value_equal

let same_fact left right = left.e = right.e && left.a = right.a && value_equal left.v right.v

let normalize_datom_for_schema schema d =
  ignore schema;
  Util.normalize_datom_value d

let empty_index index lmdb = Index.empty index lmdb

let duplicate_datoms datoms =
  let datoms = List.sort (Util.compare_datom Eavt) datoms in
  let rec loop previous duplicates = function
    | [] -> List.rev duplicates
    | datom :: rest ->
      (match previous with
       | Some previous when Util.compare_datom Eavt previous datom = 0 ->
         loop (Some datom) (datom :: duplicates) rest
       | _ -> loop (Some datom) duplicates rest)
  in
  loop None [] datoms

let primary_datoms index datoms =
  let datoms = List.sort (Util.compare_datom index) datoms in
  let rec loop previous primary = function
    | [] -> List.rev primary
    | datom :: rest ->
      (match previous with
       | Some previous when Util.compare_datom index previous datom = 0 ->
         loop (Some datom) primary rest
       | _ -> loop (Some datom) (datom :: primary) rest)
  in
  loop None [] datoms

let duplicate_eavt_by_entity duplicate_datoms =
  let table = Hashtbl.create 1024 in
  List.iter
    (fun datom ->
      let existing = Option.value (Hashtbl.find_opt table datom.e) ~default:[] in
      Hashtbl.replace table datom.e (datom :: existing))
    duplicate_datoms;
  Hashtbl.iter (fun entity_id datoms -> Hashtbl.replace table entity_id (List.rev datoms)) table;
  table

let duplicate_datoms_by_attr duplicate_datoms =
  let table = Hashtbl.create 1024 in
  List.iter
    (fun datom ->
      let existing = Option.value (Hashtbl.find_opt table datom.a) ~default:[] in
      Hashtbl.replace table datom.a (datom :: existing))
    duplicate_datoms;
  Hashtbl.iter (fun attr datoms -> Hashtbl.replace table attr (List.rev datoms)) table;
  table

(** Copy-on-write drop of cache entries for attributes touched by [datoms].
    Unaffected attr slices stay warm across append-only transactions. *)
let invalidate_attr_tables_for_datoms datoms db =
  let attrs =
    datoms
    |> List.map (fun d -> d.a)
    |> List.sort_uniq String.compare
  in
  match attrs with
  | [] -> db
  | attrs ->
    let drop_attr attr = List.mem attr attrs in
    let copy_attr_table src =
      if Hashtbl.length src = 0 then Hashtbl.create 0
      else
        let dst = Hashtbl.create (Hashtbl.length src) in
        Hashtbl.iter
          (fun attr value -> if not (drop_attr attr) then Hashtbl.replace dst attr value)
          src;
        dst
    in
    let copy_avet_entities src =
      if Hashtbl.length src = 0 then Hashtbl.create 0
      else
        let dst = Hashtbl.create (Hashtbl.length src) in
        Hashtbl.iter
          (fun ((attr, _) as key) value ->
            if not (drop_attr attr) then Hashtbl.replace dst key value)
          src;
        dst
    in
    { db with
      aevt_by_attr = copy_attr_table db.aevt_by_attr
    ; avet_by_attr = copy_attr_table db.avet_by_attr
    ; avet_entities_by_attr_value = copy_avet_entities db.avet_entities_by_attr_value
    }

(** Temporal shallow copies must not share mutable current-fact caches with the
    live DB; history/as_of reads rebuild slices from raw indexes instead. *)
let detach_attr_caches db =
  { db with
    aevt_by_attr = Hashtbl.create 0
  ; avet_by_attr = Hashtbl.create 0
  ; avet_entities_by_attr_value = Hashtbl.create 0
  }

let view_bounds db =
  { Tx_visibility.view_tx = db.max_tx; since_tx = db.since_tx; history = db.history }

let apply_db_view db datoms = Tx_visibility.apply_view db.schema (view_bounds db) datoms

let apply_db_view_seq db seq = Tx_visibility.filter_seq db.schema (view_bounds db) seq

(** Apply temporal cancel to a descending (rseek) sequence by restoring ascending order. *)
let apply_db_view_reverse_seq db seq =
  if Option.is_some db.as_of_tx || Option.is_some db.since_tx || db.history then
    (* Cancel pairs need ascending order; only materialize for temporal views. *)
    seq |> List.of_seq |> List.rev |> apply_db_view db |> List.rev |> List.to_seq
  else
    (* Live index streams current facts; keep descending order and stay lazy. *)
    apply_db_view_seq db seq

let array_rev_seq arr =
  let len = Array.length arr in
  let rec loop i () =
    if i < 0 then Seq.Nil else Seq.Cons (Array.unsafe_get arr i, loop (i - 1))
  in
  loop (len - 1)

let indexes_on_storage db = Option.is_some db.storage_ref

let merged_index db = db.duplicate_datoms <> []

let pending_overlay db = db.pending_datoms <> []

let pending_for_index db index =
  let datoms =
    match index with
    | Avet ->
      List.filter (fun d -> Schema.schema_attr_is_avet_accessible db.schema d.a) db.pending_datoms
    | Eavt | Aevt | Tave -> db.pending_datoms
  in
  List.sort (Util.compare_datom index) datoms

let flush_pending_datoms db =
  match db.pending_datoms with
  | [] -> db
  | pending ->
    let avet attr = Schema.schema_attr_is_avet_accessible db.schema attr in
    let eavt_index, aevt_index, avet_index =
      Index.append_tx_data ~avet pending db.eavt_index db.aevt_index db.avet_index
    in
    { db with pending_datoms = []; eavt_index; aevt_index; avet_index }

let index_db_of_db db =
  try Index.index_db_of (Index.db_of db.eavt_index)
  with Invalid_argument _ ->
    let index_db, _ = Index.create_index_db db.storage_ref in
    index_db

let lmdb_of_db = index_db_of_db

let group_sorted_datoms_by_attr datoms =
  let table = Hashtbl.create 32 in
  let rec flush attr group = function
    | [] -> Hashtbl.replace table attr (Array.of_list (List.rev group))
    | datom :: rest when datom.a = attr ->
      flush attr (datom :: group) rest
    | datom :: rest ->
      Hashtbl.replace table attr (Array.of_list (List.rev group));
      flush datom.a [ datom ] rest
  in
  (match datoms with
   | [] -> ()
   | datom :: rest -> flush datom.a [ datom ] rest);
  table

let index_avet_entities_by_attr_value avet_sorted =
  let table = Hashtbl.create 256 in
  List.iter
    (fun datom ->
      let key = (datom.a, datom.v) in
      let existing = Option.value (Hashtbl.find_opt table key) ~default:[] in
      Hashtbl.replace table key (datom.e :: existing))
    avet_sorted;
  let array_table = Hashtbl.create (Hashtbl.length table) in
  Hashtbl.iter
    (fun key entity_ids -> Hashtbl.replace array_table key (Array.of_list (List.rev entity_ids)))
    table;
  array_table

let datoms_of_avet_entities attr value entity_ids =
  entity_ids
  |> Array.to_list
  |> List.map (fun e -> { e; a = attr; v = value; tx = tx0; added = true })

let set_indexes_from_datoms db datoms =
  let lmdb = lmdb_of_db db in
  let duplicate_datoms = duplicate_datoms datoms in
  let eavt_datoms = primary_datoms Eavt datoms in
  let aevt_sorted = List.sort (Util.compare_datom Aevt) eavt_datoms in
  let avet_sorted =
    eavt_datoms
    |> List.filter (fun d -> Schema.schema_attr_is_avet_accessible db.schema d.a)
    |> List.sort (Util.compare_datom Avet)
  in
  Index.of_eavt_datoms
    ~avet:(Schema.schema_attr_is_avet_accessible db.schema)
    eavt_datoms
    lmdb;
  let eavt_index = Index.empty Eavt lmdb
  and aevt_index = Index.empty Aevt lmdb
  and avet_index = Index.empty Avet lmdb
  and tave_index = Index.empty Tave lmdb in
  let duplicate_aevt_datoms = List.sort (Util.compare_datom Aevt) duplicate_datoms in
  let duplicate_avet_datoms =
    duplicate_datoms
    |> List.filter (fun datom -> Schema.schema_attr_is_avet_accessible db.schema datom.a)
    |> List.sort (Util.compare_datom Avet)
  in
  let duplicate_eavt_by_entity = duplicate_eavt_by_entity duplicate_datoms in
  let duplicate_aevt_by_attr = duplicate_datoms_by_attr duplicate_aevt_datoms in
  let duplicate_avet_by_attr = duplicate_datoms_by_attr duplicate_avet_datoms in
  let max_datom_e = List.fold_left (fun max_e d -> max max_e d.e) 0 datoms in
  { db with
    eavt_index
  ; aevt_index
  ; avet_index
  ; tave_index
  ; aevt_by_attr = group_sorted_datoms_by_attr aevt_sorted
  ; avet_by_attr = group_sorted_datoms_by_attr avet_sorted
  ; avet_entities_by_attr_value = index_avet_entities_by_attr_value avet_sorted
  ; duplicate_datoms
  ; duplicate_aevt_datoms
  ; duplicate_avet_datoms
  ; duplicate_eavt_by_entity
  ; duplicate_aevt_by_attr
  ; duplicate_avet_by_attr
  ; max_datom_e
  ; pending_datoms = []
  }

let eavt_datoms db =
  Index.to_list db.eavt_index @ db.duplicate_datoms @ db.pending_datoms
  |> List.sort (Util.compare_datom Eavt)
  |> apply_db_view db

let refresh_indexes db =
  set_indexes_from_datoms db (eavt_datoms db)

let add_datoms_to_index include_datom datoms index_set =
  List.fold_left
    (fun index_set datom ->
      if include_datom datom then Index.add datom index_set else index_set)
    index_set
    datoms

let refresh_indexes_with_added_datoms db added_datoms =
  let max_datom_e = List.fold_left (fun max_e d -> max max_e d.e) db.max_datom_e added_datoms in
  if indexes_on_storage db then
    { db with
      eavt_index = add_datoms_to_index (fun _ -> true) added_datoms db.eavt_index
    ; aevt_index = add_datoms_to_index (fun _ -> true) added_datoms db.aevt_index
    ; avet_index =
        add_datoms_to_index
          (fun d -> Schema.schema_attr_is_avet_accessible db.schema d.a)
          added_datoms
          db.avet_index
    ; tave_index = add_datoms_to_index (fun _ -> true) added_datoms db.tave_index
    ; duplicate_datoms = db.duplicate_datoms
    ; duplicate_aevt_datoms = db.duplicate_aevt_datoms
    ; duplicate_avet_datoms = db.duplicate_avet_datoms
    ; duplicate_eavt_by_entity = db.duplicate_eavt_by_entity
    ; duplicate_aevt_by_attr = db.duplicate_aevt_by_attr
    ; duplicate_avet_by_attr = db.duplicate_avet_by_attr
    ; max_datom_e
    }
    |> invalidate_attr_tables_for_datoms added_datoms
  else
    { db with pending_datoms = db.pending_datoms @ added_datoms; max_datom_e }
    |> invalidate_attr_tables_for_datoms added_datoms

let refresh_indexes_with_tx_data db tx_data =
  if tx_data = [] then db
  else
    let max_datom_e = List.fold_left (fun max_e d -> max max_e d.e) db.max_datom_e tx_data in
    if indexes_on_storage db then
      let avet attr = Schema.schema_attr_is_avet_accessible db.schema attr in
      let eavt_index, aevt_index, avet_index =
        Index.append_tx_data ~avet tx_data db.eavt_index db.aevt_index db.avet_index
      in
      { db with eavt_index; aevt_index; avet_index; max_datom_e }
      |> invalidate_attr_tables_for_datoms tx_data
    else
      { db with pending_datoms = db.pending_datoms @ tx_data; max_datom_e }
      |> invalidate_attr_tables_for_datoms tx_data

let same_stored_datom left right =
  left.e = right.e
  && left.a = right.a
  && left.tx = right.tx
  && left.added = right.added
  && value_equal left.v right.v

let without_stored_datoms removed datoms =
  List.filter (fun datom -> not (List.exists (same_stored_datom datom) removed)) datoms

let refresh_indexes_with_removed_datoms db removed_datoms =
  if removed_datoms = [] then db
  else
    let remove_from index =
      List.fold_left (fun index datom -> Index.remove datom index) index removed_datoms
    in
    let eavt_index = remove_from db.eavt_index in
    let aevt_index = remove_from db.aevt_index in
    let avet_index = remove_from db.avet_index in
    let tave_index = remove_from db.tave_index in
    let duplicate_datoms = without_stored_datoms removed_datoms db.duplicate_datoms in
    let duplicate_aevt_datoms = without_stored_datoms removed_datoms db.duplicate_aevt_datoms in
    let duplicate_avet_datoms = without_stored_datoms removed_datoms db.duplicate_avet_datoms in
    let pending_datoms = without_stored_datoms removed_datoms db.pending_datoms in
    { db with
      eavt_index
    ; aevt_index
    ; avet_index
    ; tave_index
    ; duplicate_datoms
    ; duplicate_aevt_datoms
    ; duplicate_avet_datoms
    ; pending_datoms
    }
    |> invalidate_attr_tables_for_datoms removed_datoms

let snapshot_db db = db

let temporal_view db =
  Option.is_some db.as_of_tx || Option.is_some db.since_tx || db.history

(** Rolling TAVE retention window in days. [0] disables pruning (index still written). *)
let tave_retention_days = ref 30

let set_tave_retention_days days = tave_retention_days := max 0 days

let get_tave_retention_days () = !tave_retention_days

let millis_per_day = 86_400_000

let basis_tx db = db.max_tx

let as_of_t db = db.as_of_tx

let since_t db = db.since_tx

let as_of tx db =
  if tx > db.store_max_tx then
    invalid_arg
      ("as_of tx "
       ^ string_of_int tx
       ^ " is after database basis "
       ^ string_of_int db.store_max_tx);
  detach_attr_caches { db with max_tx = tx; as_of_tx = Some tx }

let since tx db = detach_attr_caches { db with since_tx = Some tx }

let history db = detach_attr_caches { db with history = true }

let is_history db = db.history

let as_of_tx = as_of_t

let since_tx = since_t

let with_datoms db datoms =
  set_indexes_from_datoms db datoms

let storage_ref_of ?storage auto_storage_ref =
  match storage with
  | Some attached_storage -> Some attached_storage
  | None -> auto_storage_ref

let empty_db context ?(schema = []) ?storage () =
  let schema = Schema.validate_schema schema in
  let index_db, auto_storage_ref = Index.create_index_db storage in
  { db_uid = context.next_db_uid ()
  ; schema
  ; eavt_index = empty_index Eavt index_db
  ; aevt_index = empty_index Aevt index_db
  ; avet_index = empty_index Avet index_db
  ; tave_index = empty_index Tave index_db
  ; aevt_by_attr = Hashtbl.create 0
  ; avet_by_attr = Hashtbl.create 0
  ; avet_entities_by_attr_value = Hashtbl.create 0
  ; duplicate_datoms = []
  ; duplicate_aevt_datoms = []
  ; duplicate_avet_datoms = []
  ; duplicate_eavt_by_entity = Hashtbl.create 0
  ; duplicate_aevt_by_attr = Hashtbl.create 0
  ; duplicate_avet_by_attr = Hashtbl.create 0
  ; max_eid = 0
  ; max_datom_e = 0
  ; max_tx = tx0
  ; store_max_tx = tx0
  ; as_of_tx = None
  ; since_tx = None
  ; history = false
  ; filter_pred = None
  ; pending_datoms = []
  ; storage_ref = storage_ref_of ?storage auto_storage_ref
  ; tx_fns = []
  }

let empty context db = empty_db context ~schema:db.schema ?storage:db.storage_ref ()

let init_db context ?(schema = []) ?storage datoms =
  let schema = Schema.validate_schema schema in
  let datoms = List.map (normalize_datom_for_schema schema) datoms in
  let max_eid =
    List.fold_left (fun max_eid d -> max_eid_in_value (max_eid_with_entity_id max_eid d.e) d.v) 0 datoms
  in
  let max_tx = List.fold_left (fun max_tx d -> max max_tx d.tx) tx0 datoms in
  let index_db, auto_storage_ref = Index.create_index_db storage in
  { db_uid = context.next_db_uid ()
  ; schema
  ; eavt_index = empty_index Eavt index_db
  ; aevt_index = empty_index Aevt index_db
  ; avet_index = empty_index Avet index_db
  ; tave_index = empty_index Tave index_db
  ; aevt_by_attr = Hashtbl.create 0
  ; avet_by_attr = Hashtbl.create 0
  ; avet_entities_by_attr_value = Hashtbl.create 0
  ; duplicate_datoms = []
  ; duplicate_aevt_datoms = []
  ; duplicate_avet_datoms = []
  ; duplicate_eavt_by_entity = Hashtbl.create 0
  ; duplicate_aevt_by_attr = Hashtbl.create 0
  ; duplicate_avet_by_attr = Hashtbl.create 0
  ; max_eid
  ; max_datom_e = 0
  ; max_tx
  ; store_max_tx = max_tx
  ; as_of_tx = None
  ; since_tx = None
  ; history = false
  ; filter_pred = None
  ; pending_datoms = []
  ; storage_ref = storage_ref_of ?storage auto_storage_ref
  ; tx_fns = []
  }
  |> fun db -> with_datoms db datoms

let visible_datoms db =
  match db.filter_pred with
  | None -> eavt_datoms db
  | Some pred -> List.filter pred (eavt_datoms db)

let is_filtered db = Option.is_some db.filter_pred

let unfiltered context db = refresh_identity context { db with filter_pred = None }

let filter context db pred =
  let unfiltered_db = unfiltered context db in
  let filter_pred =
    match db.filter_pred with
    | None -> fun datom -> pred unfiltered_db datom
    | Some existing -> fun datom -> existing datom && pred unfiltered_db datom
  in
  refresh_identity context { db with filter_pred = Some filter_pred }

type index_context =
  { is_avet_accessible : db -> attr -> bool
  ; resolve_entity_ref : db -> entity_ref -> entity_id
  ; resolve_value_for_optional_attr : db -> attr option -> value -> value
  ; resolve_value_for_attr : db -> attr -> value -> value
  ; compare_value : value -> value -> int
  ; first_nonzero : int list -> int
  }

let hash_cache : (int, int) Hashtbl.t = Hashtbl.create 128

let hash db =
  match Hashtbl.find_opt hash_cache db.db_uid with
  | Some hash -> hash
  | None ->
    let hash =
      Hashtbl.hash
        ( db.schema
        , eavt_datoms db
        , db.max_eid
        , db.max_tx
        )
    in
    Hashtbl.replace hash_cache db.db_uid hash;
    hash

let hash_cache_size () = Hashtbl.length hash_cache

let stored_index db = function
  | Eavt -> db.eavt_index
  | Aevt -> db.aevt_index
  | Avet -> db.avet_index
  | Tave -> db.tave_index

let merge_sorted_datoms index left right =
  let cmp = Util.compare_datom index in
  let rec merge acc left right =
    match left, right with
    | [], rest | rest, [] -> List.rev_append acc rest
    | left_datom :: left_rest, right_datom :: right_rest ->
      if cmp left_datom right_datom <= 0 then
        merge (left_datom :: acc) left_rest right
      else
        merge (right_datom :: acc) left right_rest
  in
  merge [] left right

let merge_sorted_datom_seqs compare_datom left right =
  let rec merge left_node right_node () =
    match left_node, right_node with
    | Seq.Nil, Seq.Nil -> Seq.Nil
    | Seq.Nil, Seq.Cons (datom, right_tail) -> Seq.Cons (datom, right_tail)
    | Seq.Cons (datom, left_tail), Seq.Nil -> Seq.Cons (datom, left_tail)
    | Seq.Cons (left_datom, left_rest), Seq.Cons (right_datom, right_rest) ->
      if compare_datom left_datom right_datom <= 0 then
        Seq.Cons (left_datom, merge (left_rest ()) right_node)
      else
        Seq.Cons (right_datom, merge left_node (right_rest ()))
  in
  merge (left ()) (right ())

let duplicate_index_datoms db index =
  match index with
  | Eavt | Tave -> db.duplicate_datoms
  | Aevt -> db.duplicate_aevt_datoms
  | Avet -> db.duplicate_avet_datoms

let duplicate_attr_datoms db index attr =
  match index with
  | Aevt -> Option.value (Hashtbl.find_opt db.duplicate_aevt_by_attr attr) ~default:[]
  | Avet -> Option.value (Hashtbl.find_opt db.duplicate_avet_by_attr attr) ~default:[]
  | Eavt | Tave -> duplicate_index_datoms db index

let cache_avet_entities_for_attr db attr datoms =
  let by_value = Hashtbl.create 16 in
  List.iter
    (fun datom ->
      let existing = Option.value (Hashtbl.find_opt by_value datom.v) ~default:[] in
      Hashtbl.replace by_value datom.v (datom.e :: existing))
    datoms;
  Hashtbl.iter
    (fun value entity_ids ->
      Hashtbl.replace
        db.avet_entities_by_attr_value
        (attr, value)
        (Array.of_list (List.rev entity_ids)))
    by_value

let primary_attr_datoms db index attr =
  let attr_prefix_datoms _index index_set =
    Index.fold_attr_prefix (fun acc datom -> datom :: acc) [] index_set attr |> List.rev
  in
  let pending_attr =
    List.filter (fun d -> d.a = attr) db.pending_datoms |> List.sort (Util.compare_datom index)
  in
  (* Attr caches are shared across as_of/since/history shallow copies and hold
     current-basis slices. Temporal views must rebuild from raw indexes so
     retracted/history facts remain available for apply_db_view. *)
  let temporal = temporal_view db in
  match index with
  | Aevt ->
    (match db.since_tx with
     | Some from_tx when temporal && not (merged_index db) && not (pending_overlay db) ->
       (* Prefer TAVE for since-bounded attr scans (API unchanged). *)
       Index.fold_tave_range
         (fun acc datom -> datom :: acc)
         [] db.tave_index ~from_tx ~to_tx:db.max_tx ~attr ()
       |> List.rev
       |> apply_db_view db
     | _ ->
       (match (if temporal then None else Hashtbl.find_opt db.aevt_by_attr attr) with
        | Some datoms -> Array.to_list datoms
        | None ->
          let datoms =
            merge_sorted_datoms Aevt (attr_prefix_datoms Aevt db.aevt_index) pending_attr
            |> apply_db_view db
          in
          if not temporal then Hashtbl.replace db.aevt_by_attr attr (Array.of_list datoms);
          datoms))
  | Avet ->
    (match (if temporal then None else Hashtbl.find_opt db.avet_by_attr attr) with
     | Some datoms -> Array.to_list datoms
     | None ->
       let datoms =
         merge_sorted_datoms Avet (attr_prefix_datoms Avet db.avet_index) pending_attr
         |> apply_db_view db
       in
       if not temporal then (
         Hashtbl.replace db.avet_by_attr attr (Array.of_list datoms);
         cache_avet_entities_for_attr db attr datoms);
       datoms)
  | Eavt | Tave ->
    merge_sorted_datoms index (Index.to_list (stored_index db index)) pending_attr
    |> apply_db_view db

let aevt_attr_array db attr =
  ignore (primary_attr_datoms db Aevt attr);
  Hashtbl.find_opt db.aevt_by_attr attr

let duplicate_prefix_datoms db index e a =
  match index, e, a with
  | Eavt, Some entity_id, _ -> Option.value (Hashtbl.find_opt db.duplicate_eavt_by_entity entity_id) ~default:[]
  | (Aevt | Avet), _, Some attr -> duplicate_attr_datoms db index attr
  | _ -> duplicate_index_datoms db index

let raw_index_datoms_list db index =
  merge_sorted_datoms index
    (stored_index db index |> Index.to_list)
    (pending_for_index db index @ duplicate_index_datoms db index)

let visible_index_datoms db index =
  let datoms = apply_db_view db (raw_index_datoms_list db index) in
  match db.filter_pred with
  | None -> datoms
  | Some pred -> List.filter pred datoms

let index_datoms_seq db index =
  match merged_index db, pending_overlay db with
  | false, false ->
    stored_index db index |> Index.seq |> Index.to_seq |> apply_db_view_seq db
  | false, true ->
    let stored = stored_index db index |> Index.seq |> Index.to_seq in
    let pending = pending_for_index db index |> List.to_seq in
    merge_sorted_datom_seqs (Util.compare_datom index) stored pending |> apply_db_view_seq db
  | true, _ ->
    raw_index_datoms_list db index |> apply_db_view db |> List.to_seq

let reverse_index_datoms_seq db index =
  match merged_index db, pending_overlay db with
  | false, false ->
    stored_index db index |> Index.rslice_seq |> Index.to_seq
  | false, true ->
    let stored = stored_index db index |> Index.rslice_seq |> Index.to_seq in
    let pending = pending_for_index db index |> List.rev |> List.to_seq in
    merge_sorted_datom_seqs (fun left right -> Util.compare_datom index right left) stored pending
  | true, _ ->
    let indexed = stored_index db index |> Index.rslice_seq |> Index.to_seq in
    let duplicates = duplicate_index_datoms db index |> List.rev |> List.to_seq in
    merge_sorted_datom_seqs
      (fun left right -> Util.compare_datom index right left)
      indexed
      duplicates

let instant_millis = function
  | Instant ms -> Some ms
  | _ -> None

(** Resolve the latest transaction whose [:db/txInstant] is <= [instant]. *)
let resolve_tx_at_instant instant db =
  let target =
    match instant with
    | Instant ms -> ms
    | _ -> invalid_arg "as_of_instant requires Instant value"
  in
  let best = ref None in
  let consider datom =
    match instant_millis datom.v with
    | Some ms when ms <= target && datom.added ->
      (match !best with
       | None -> best := Some (datom.e, ms)
       | Some (_, best_ms) when ms >= best_ms -> best := Some (datom.e, ms)
       | Some _ -> ())
    | _ -> ()
  in
  (match primary_attr_datoms db Aevt "db/txInstant" with
   | [] ->
     List.iter
       (fun d -> if d.a = "db/txInstant" then consider d)
       (raw_index_datoms_list db Eavt)
   | datoms -> List.iter consider datoms);
  match !best with
  | Some (tx, _) -> tx
  | None ->
    invalid_arg "as_of_instant: no :db/txInstant at or before the given Instant"

let as_of_instant instant db = as_of (resolve_tx_at_instant instant db) db

let try_resolve_tx_at_instant instant db =
  try Some (resolve_tx_at_instant instant db) with Invalid_argument _ -> None

(** Lowest tx still inside the TAVE retention window, if resolvable via txInstant. *)
let retention_tx_lo db =
  match !tave_retention_days with
  | 0 -> None
  | days ->
    let now_ms = int_of_float (Platform.now_seconds () *. 1000.) in
    let cutoff = Instant (now_ms - (days * millis_per_day)) in
    try_resolve_tx_at_instant cutoff db

let prune_tave_to_retention db =
  match retention_tx_lo db with
  | None -> ()
  | Some tx_lo -> Index.prune_tave_before db.tave_index ~before_tx:tx_lo

(** Physically drop history facts with [tx < before] while keeping the current
    projection and all datoms at or after [before]. *)
let purge_history_before before db =
  if temporal_view db then
    invalid_arg "Cannot purge history against an as-of/since/history database value";
  if before > db.store_max_tx then
    invalid_arg
      ("purge_history_before tx "
       ^ string_of_int before
       ^ " is after database basis "
       ^ string_of_int db.store_max_tx);
  let raw = raw_index_datoms_list db Eavt in
  let visible = apply_db_view db raw in
  let visible_keys = Hashtbl.create (List.length visible) in
  List.iter
    (fun d -> Hashtbl.replace visible_keys (d.e, d.a, d.v, d.tx, d.added) ())
    visible;
  let keep d =
    d.tx >= before || Hashtbl.mem visible_keys (d.e, d.a, d.v, d.tx, d.added)
  in
  let kept = List.filter keep raw in
  let removed = List.filter (fun d -> not (keep d)) raw in
  if removed = [] then db, []
  else set_indexes_from_datoms db kept, removed

let apply_filter_pred db seq =
  match db.filter_pred with
  | None -> seq
  | Some pred -> Seq.filter pred seq

let matches maybe expected = Option.fold ~none:true ~some:(fun actual -> actual = expected) maybe

let values_compare_equal context actual expected =
  match actual, expected with
  | Nil, Nil -> true
  | Int actual, Int expected
  | Ref actual, Ref expected
  | Int actual, Ref expected
  | Ref actual, Int expected ->
    actual = expected
  | String actual, String expected
  | Symbol actual, Symbol expected
  | Keyword actual, Keyword expected
  | Uuid actual, Uuid expected
  | Regex actual, Regex expected ->
    actual = expected
  | Bool actual, Bool expected -> actual = expected
  | Instant actual, Instant expected -> actual = expected
  | TxRef, TxRef -> true
  | _ -> context.compare_value actual expected = 0

let matches_value context maybe expected =
  Option.fold ~none:true ~some:(fun actual -> values_compare_equal context actual expected) maybe

type bound_fields =
  { bound_e : bool
  ; bound_a : bool
  ; bound_v : bool
  ; bound_tx : bool
  }

let bound_datom ?(e = 0) ?(a = "") ?(v = Nil) ?(tx = tx0) () =
  { e; a; v; tx; added = true }

let first_nonzero4 first second third fourth =
  if first <> 0 then first
  else if second <> 0 then second
  else if third <> 0 then third
  else fourth

let compare_bound_e fields left right =
  if fields.bound_e then compare left.e right.e else 0

let compare_bound_a fields left right =
  if fields.bound_a then compare left.a right.a else 0

let compare_bound_v context fields left right =
  if fields.bound_v then context.compare_value left.v right.v else 0

let compare_bound_tx fields left right =
  if fields.bound_tx then compare left.tx right.tx else 0

let compare_bound_fields context fields left right = function
  | Eavt ->
    first_nonzero4
      (compare_bound_e fields left right)
      (compare_bound_a fields left right)
      (compare_bound_v context fields left right)
      (compare_bound_tx fields left right)
  | Aevt ->
    first_nonzero4
      (compare_bound_a fields left right)
      (compare_bound_e fields left right)
      (compare_bound_v context fields left right)
      (compare_bound_tx fields left right)
  | Avet ->
    first_nonzero4
      (compare_bound_a fields left right)
      (compare_bound_v context fields left right)
      (compare_bound_e fields left right)
      (compare_bound_tx fields left right)
  | Tave ->
    first_nonzero4
      (compare_bound_tx fields left right)
      (compare_bound_a fields left right)
      (compare_bound_v context fields left right)
      (compare_bound_e fields left right)

let array_attr_value_slice context index bound bound_fields arr =
  let prefix left right = compare_bound_fields context bound_fields left right index in
  let len = Array.length arr in
  let rec lower lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if prefix arr.(mid) bound < 0 then lower (mid + 1) hi else lower lo mid
  in
  let start = lower 0 len in
  let rec upper index =
    if index >= len || prefix arr.(index) bound > 0 then index else upper (index + 1)
  in
  let stop = upper start in
  if start >= stop then []
  else Array.sub arr start (stop - start) |> Array.to_list

let array_attr_value_seq context index bound bound_fields arr =
  let prefix left right = compare_bound_fields context bound_fields left right index in
  let len = Array.length arr in
  let rec lower lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if prefix arr.(mid) bound < 0 then lower (mid + 1) hi else lower lo mid
  in
  let start = lower 0 len in
  let rec upper index =
    if index >= len || prefix arr.(index) bound > 0 then index else upper (index + 1)
  in
  let stop = upper start in
  let rec loop index () =
    if index >= stop then Seq.Nil else Seq.Cons (arr.(index), loop (index + 1))
  in
  loop start

let array_range_bounds context index from_bound from_fields to_bound to_fields arr =
  let below_from left right = compare_bound_fields context from_fields left right index in
  let above_to left right = compare_bound_fields context to_fields left right index in
  let len = Array.length arr in
  let rec lower lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if below_from arr.(mid) from_bound < 0 then lower (mid + 1) hi else lower lo mid
  in
  let start = lower 0 len in
  let rec upper index =
    if index >= len || above_to arr.(index) to_bound > 0 then index else upper (index + 1)
  in
  (start, upper start)

let array_range_fold f init context index from_bound from_fields to_bound to_fields arr =
  let start, stop = array_range_bounds context index from_bound from_fields to_bound to_fields arr in
  let rec loop index acc =
    if index >= stop then acc else loop (index + 1) (f acc arr.(index))
  in
  loop start init

let array_range_seq context index from_bound from_fields to_bound to_fields arr =
  let start, stop = array_range_bounds context index from_bound from_fields to_bound to_fields arr in
  let rec loop index () =
    if index >= stop then Seq.Nil else Seq.Cons (arr.(index), loop (index + 1))
  in
  loop start

let array_exact_prefix_slice cmp bound arr =
  let len = Array.length arr in
  let rec lower lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if cmp arr.(mid) bound < 0 then lower (mid + 1) hi else lower lo mid
  in
  let start = lower 0 len in
  let rec upper index =
    if index >= len || cmp arr.(index) bound <> 0 then index else upper (index + 1)
  in
  let stop = upper start in
  if start >= stop then []
  else Array.sub arr start (stop - start) |> Array.to_list

let find_entity_in_aevt_array arr entity_id =
  let len = Array.length arr in
  if len = 0 then None
  else
    let rec lower lo hi =
      if lo >= hi then lo
      else
        let mid = (lo + hi) / 2 in
        let mid_e = arr.(mid).e in
        if mid_e < entity_id then lower (mid + 1) hi
        else if mid_e > entity_id then lower lo mid
        else mid
    in
    let index = lower 0 len in
    if index >= len || arr.(index).e <> entity_id then None else Some arr.(index)

let find_datom_in_sorted_array index arr datom =
  let len = Array.length arr in
  if len = 0 then None
  else
    let cmp = Util.compare_datom index in
    let rec lower lo hi =
      if lo >= hi then lo
      else
        let mid = (lo + hi) / 2 in
        if cmp arr.(mid) datom < 0 then lower (mid + 1) hi else lower lo mid
    in
    let at = lower 0 len in
    if at >= len || cmp arr.(at) datom <> 0 then None else Some arr.(at)

let rehydrate_datom_value db index datom =
  match index with
  | Avet -> (
    match Hashtbl.find_opt db.avet_by_attr datom.a with
    | None -> datom
    | Some arr ->
      (match find_datom_in_sorted_array Avet arr datom with
       | None -> datom
       | Some cached -> { datom with v = cached.v }))
  | Aevt -> (
    match Hashtbl.find_opt db.aevt_by_attr datom.a with
    | None -> datom
    | Some arr ->
      (match find_datom_in_sorted_array Aevt arr datom with
       | None -> datom
       | Some cached -> { datom with v = cached.v }))
  | Eavt | Tave -> datom

let rehydrate_datom_seq db index seq = Seq.map (rehydrate_datom_value db index) seq

let find_primary_aevt_entity_attr db entity_id attr =
  match Hashtbl.find_opt db.aevt_by_attr attr with
  | None -> None
  | Some arr -> find_entity_in_aevt_array arr entity_id

let exact_sorted_slice cmp bound datoms =
  array_exact_prefix_slice cmp bound (Array.of_list datoms)

let slice_cmp context index from_bound from_fields to_bound to_fields left right =
  if right == from_bound then
    compare_bound_fields context from_fields left right index
  else if left == from_bound then
    -compare_bound_fields context from_fields right left index
  else if right == to_bound then
    compare_bound_fields context to_fields left right index
  else if left == to_bound then
    -compare_bound_fields context to_fields right left index
  else
    Util.compare_datom index left right

let single_field_prefix_cmp index bound left right =
  let compare_bound left right =
    match index with
    | Eavt -> compare left.e right.e
    | Aevt | Avet -> compare left.a right.a
    | Tave -> compare left.tx right.tx
  in
  if right == bound then
    compare_bound left right
  else if left == bound then
    -compare_bound right left
  else
    match index with
    | Eavt ->
      first_nonzero4
        (compare left.e right.e)
        (compare left.a right.a)
        (Util.compare_value left.v right.v)
        (compare left.tx right.tx)
    | Aevt ->
      first_nonzero4
        (compare left.a right.a)
        (compare left.e right.e)
        (Util.compare_value left.v right.v)
        (compare left.tx right.tx)
    | Avet ->
      first_nonzero4
        (compare left.a right.a)
        (Util.compare_value left.v right.v)
        (compare left.e right.e)
        (compare left.tx right.tx)
    | Tave ->
      first_nonzero4
        (compare left.tx right.tx)
        (compare left.a right.a)
        (Util.compare_value left.v right.v)
        (compare left.e right.e)

let exact_prefix_slice_cmp context index bound bound_fields =
  match index, bound_fields with
  | Eavt, { bound_e = true; bound_a = false; bound_v = false; bound_tx = false }
  | Aevt, { bound_e = false; bound_a = true; bound_v = false; bound_tx = false }
  | Avet, { bound_e = false; bound_a = true; bound_v = false; bound_tx = false } ->
    single_field_prefix_cmp index bound
  | _ -> slice_cmp context index bound bound_fields bound bound_fields

let fields ?(e = false) ?(a = false) ?(v = false) ?(tx = false) () =
  { bound_e = e; bound_a = a; bound_v = v; bound_tx = tx }

let exact_prefix_bound index e a v tx =
  match index with
  | Eavt ->
    (match e, a, v, tx with
     | Some e, None, None, None ->
       Some (bound_datom ~e (), fields ~e:true ())
     | Some e, Some a, None, None ->
       Some (bound_datom ~e ~a (), fields ~e:true ~a:true ())
     | Some e, Some a, Some v, None ->
       Some (bound_datom ~e ~a ~v (), fields ~e:true ~a:true ~v:true ())
     | Some e, Some a, Some v, Some tx ->
       Some (bound_datom ~e ~a ~v ~tx (), fields ~e:true ~a:true ~v:true ~tx:true ())
     | _ -> None)
  | Aevt ->
    (match a, e, v, tx with
     | Some a, None, None, None ->
       Some (bound_datom ~a (), fields ~a:true ())
     | Some a, Some e, None, None ->
       Some (bound_datom ~e ~a (), fields ~e:true ~a:true ())
     | Some a, Some e, Some v, None ->
       Some (bound_datom ~e ~a ~v (), fields ~e:true ~a:true ~v:true ())
     | Some a, Some e, Some v, Some tx ->
       Some (bound_datom ~e ~a ~v ~tx (), fields ~e:true ~a:true ~v:true ~tx:true ())
     | _ -> None)
  | Avet ->
    (match a, v, e, tx with
     | Some a, None, None, None ->
       Some (bound_datom ~a (), fields ~a:true ())
     | Some a, Some v, None, None ->
       Some (bound_datom ~a ~v (), fields ~a:true ~v:true ())
     | Some a, Some v, Some e, None ->
       Some (bound_datom ~e ~a ~v (), fields ~e:true ~a:true ~v:true ())
     | Some a, Some v, Some e, Some tx ->
       Some (bound_datom ~e ~a ~v ~tx (), fields ~e:true ~a:true ~v:true ~tx:true ())
     | _ -> None)
  | Tave -> None

let avet_entity_ids_by_attr_value context db attr value =
  if temporal_view db then
    Some
      (primary_attr_datoms db Avet attr
       |> List.filter (fun datom -> datom.a = attr && context.compare_value datom.v value = 0)
       |> List.map (fun datom -> datom.e)
       |> Array.of_list)
  else
    match Hashtbl.find_opt db.avet_entities_by_attr_value (attr, value) with
    | Some entity_ids -> Some entity_ids
    | None -> (
      match Hashtbl.find_opt db.avet_by_attr attr with
      | Some datoms ->
        let bound = bound_datom ~a:attr ~v:value () in
        let bound_fields = fields ~a:true ~v:true () in
        Some
          (array_attr_value_slice context Avet bound bound_fields datoms
           |> List.map (fun datom -> datom.e)
           |> Array.of_list)
      | None -> None)

let avet_datoms_by_value context db attr value =
  let bound = bound_datom ~a:attr ~v:value () in
  let bound_fields = fields ~a:true ~v:true () in
  if temporal_view db then
    primary_attr_datoms db Avet attr
    |> List.filter (fun datom -> datom.a = attr && context.compare_value datom.v value = 0)
  else
    match Hashtbl.find_opt db.avet_entities_by_attr_value (attr, value) with
    | Some entity_ids -> datoms_of_avet_entities attr value entity_ids
    | None -> (
      match Hashtbl.find_opt db.avet_by_attr attr with
      | Some datoms -> array_attr_value_slice context Avet bound bound_fields datoms
      | None ->
        if merged_index db || pending_overlay db then
          primary_attr_datoms db Avet attr
          |> List.filter (fun datom -> datom.a = attr && context.compare_value datom.v value = 0)
        else
          let cmp = exact_prefix_slice_cmp context Avet bound bound_fields in
          Index.slice_seq ~from_:bound ~to_:bound ~cmp (stored_index db Avet)
          |> Index.seq_to_list)

let avet_datoms_by_value_seq context db attr value =
  let bound = bound_datom ~a:attr ~v:value () in
  let bound_fields = fields ~a:true ~v:true () in
  if temporal_view db then
    avet_datoms_by_value context db attr value |> List.to_seq
  else
    match Hashtbl.find_opt db.avet_by_attr attr with
    | Some datoms -> array_attr_value_seq context Avet bound bound_fields datoms
    | None ->
      if merged_index db then
        primary_attr_datoms db Avet attr
        |> List.filter (fun datom -> datom.a = attr && context.compare_value datom.v value = 0)
        |> List.to_seq
      else
        let cmp = exact_prefix_slice_cmp context Avet bound bound_fields in
        Index.slice_seq ~from_:bound ~to_:bound ~cmp (stored_index db Avet) |> Index.to_seq

let exact_prefix_datoms context db index e a v tx =
  match exact_prefix_bound index e a v tx with
  | None -> None
  | Some (bound, bound_fields) ->
    (match index, e, a, v, tx with
     | (Aevt | Avet), None, Some attr, None, None when merged_index db || pending_overlay db ->
       let indexed = primary_attr_datoms db index attr in
       let duplicates = duplicate_attr_datoms db index attr in
       Some (merge_sorted_datom_seqs (Util.compare_datom index) (List.to_seq indexed) (List.to_seq duplicates))
     | _ ->
       let cmp = exact_prefix_slice_cmp context index bound bound_fields in
       (match index, a, merged_index db || pending_overlay db with
        | (Aevt | Avet), Some attr, true ->
          let indexed = primary_attr_datoms db index attr |> exact_sorted_slice cmp bound in
          let duplicates = duplicate_prefix_datoms db index e a |> exact_sorted_slice cmp bound in
          Some (merge_sorted_datom_seqs (Util.compare_datom index) (List.to_seq indexed) (List.to_seq duplicates))
        | _ ->
          (match merged_index db || pending_overlay db, index, e, a, v, tx with
           | false, Avet, None, Some _, Some _, None ->
             Some (avet_datoms_by_value_seq context db (Option.get a) (Option.get v))
           | false, Aevt, _, Some attr, _, _ -> (
             match temporal_view db, Hashtbl.find_opt db.aevt_by_attr attr with
             | false, Some arr ->
               Some (List.to_seq (array_exact_prefix_slice cmp bound arr))
             | true, _ | _, None ->
               Some (Index.slice_seq ~from_:bound ~to_:bound ~cmp (stored_index db Aevt) |> Index.to_seq))
           | false, _, _, _, _, _ ->
             Some (Index.slice_seq ~from_:bound ~to_:bound ~cmp (stored_index db index) |> Index.to_seq)
           | true, _, _, _, _, _ ->
             if merged_index db then
               let datoms =
                 raw_index_datoms_list db index |> exact_sorted_slice cmp bound
               in
               Some (List.to_seq datoms)
             else
               let indexed = Index.slice_seq ~from_:bound ~to_:bound ~cmp (stored_index db index) |> Index.to_seq in
               let pending =
                 pending_for_index db index |> exact_sorted_slice cmp bound |> List.to_seq
               in
               Some (merge_sorted_datom_seqs (Util.compare_datom index) indexed pending))))

let exact_prefix_datoms_list context db index e a v tx =
  match exact_prefix_bound index e a v tx with
  | None -> None
  | Some (bound, bound_fields) ->
    let cmp = exact_prefix_slice_cmp context index bound bound_fields in
    let exact_attr_prefix =
      match index, e, a, v, tx with
      | Aevt, None, Some _, None, None -> true
      | _ -> false
    in
    (match merged_index db || pending_overlay db with
     | false ->
       Some
         (match index, a, v, exact_attr_prefix with
          | Avet, Some attr, Some value, false -> avet_datoms_by_value context db attr value
          | (Aevt | Avet), Some attr, None, true -> primary_attr_datoms db index attr
          | Aevt, Some attr, _, false -> (
            match temporal_view db, Hashtbl.find_opt db.aevt_by_attr attr with
            | false, Some arr -> array_exact_prefix_slice cmp bound arr
            | true, _ | _, None ->
              Index.slice_seq ~from_:bound ~to_:bound ~cmp (stored_index db Aevt)
              |> Index.seq_to_list)
          | _ ->
            Index.slice_seq ~from_:bound ~to_:bound ~cmp (stored_index db index)
            |> Index.seq_to_list)
     | true ->
       exact_prefix_datoms context db index e a v tx
       |> Option.map List.of_seq)

let lower_prefix_datoms context db index e a v tx =
  match exact_prefix_bound index e a v tx with
  | None -> None
  | Some (bound, bound_fields) ->
    let cmp = slice_cmp context index bound bound_fields bound bound_fields in
    let indexed =
      match index, e, a, v, tx with
      | (Aevt | Avet), None, Some attr, None, None when merged_index db || pending_overlay db ->
        primary_attr_datoms db index attr
        |> List.filter (fun datom -> cmp datom bound >= 0)
        |> List.to_seq
      | _ when pending_overlay db && not (merged_index db) ->
        let stored = Index.slice_seq ~from_:bound ~cmp (stored_index db index) |> Index.to_seq in
        let pending =
          pending_for_index db index
          |> List.filter (fun datom -> cmp datom bound >= 0)
          |> List.to_seq
        in
        merge_sorted_datom_seqs (Util.compare_datom index) stored pending
      | _ -> Index.slice_seq ~from_:bound ~cmp (stored_index db index) |> Index.to_seq
    in
    (match merged_index db || pending_overlay db with
     | false -> Some indexed
     | true ->
       let duplicates = duplicate_prefix_datoms db index e a |> List.filter (fun datom -> cmp datom bound >= 0) in
       Some (merge_sorted_datom_seqs (Util.compare_datom index) indexed (List.to_seq duplicates)))

let reverse_upper_prefix_datoms context db index e a v tx =
  match exact_prefix_bound index e a v tx with
  | None -> None
  | Some (bound, bound_fields) ->
    let cmp = slice_cmp context index bound bound_fields bound bound_fields in
    let debug = match Sys.getenv_opt "DS_DEBUG_INDEX" with Some "1" -> true | _ -> false in
    let indexed =
      match index, e, a, v, tx with
      | (Aevt | Avet), None, Some attr, None, None ->
        let from_cache =
          match index with
          | Aevt -> Hashtbl.find_opt db.aevt_by_attr attr
          | Avet -> Hashtbl.find_opt db.avet_by_attr attr
          | Eavt | Tave -> None
        in
        if debug then
          Printf.eprintf
            "[DS_DEBUG_INDEX] reverse_upper_prefix branch=attr_rev index=%s attr=%s cache=%b merged=%b pending=%b\n%!"
            (match index with Eavt -> "eavt" | Aevt -> "aevt" | Avet -> "avet" | Tave -> "tave")
            attr
            (Option.is_some from_cache)
            (merged_index db) (pending_overlay db);
        (match from_cache with
         | Some arr when not (merged_index db || pending_overlay db || temporal_view db) ->
           array_rev_seq arr
         | _ when not (merged_index db || pending_overlay db || temporal_view db) ->
           (* Stay lazy like upstream rseq — do not materialize the whole attr
              into avet_by_attr / aevt_by_attr just to reverse-scan. *)
           Index.rslice_seq ~from_:bound ~to_:bound ~cmp (stored_index db index) |> Index.to_seq
         | _ ->
           primary_attr_datoms db index attr
           |> List.filter (fun datom -> cmp datom bound <= 0)
           |> List.rev
           |> List.to_seq)
      | _ when pending_overlay db && not (merged_index db) ->
        if debug then
          Printf.eprintf
            "[DS_DEBUG_INDEX] reverse_upper_prefix branch=rslice+pending index=%s pending=%d\n%!"
            (match index with Eavt -> "eavt" | Aevt -> "aevt" | Avet -> "avet" | Tave -> "tave")
            (List.length db.pending_datoms);
        let attr_only =
          match index, e, a, v, tx with
          | (Aevt | Avet), None, Some _, None, None -> true
          | _ -> false
        in
        let stored =
          if attr_only then
            Index.rslice_seq ~from_:bound ~to_:bound ~cmp (stored_index db index) |> Index.to_seq
          else
            Index.rslice_seq ~from_:bound ~cmp (stored_index db index) |> Index.to_seq
        in
        let pending =
          pending_for_index db index
          |> List.filter (fun datom -> cmp datom bound <= 0)
          |> List.rev
          |> List.to_seq
        in
        merge_sorted_datom_seqs (fun left right -> Util.compare_datom index right left) stored pending
      | _ ->
        if debug then
          Printf.eprintf
            "[DS_DEBUG_INDEX] reverse_upper_prefix branch=rslice index=%s merged=%b pending=%b a=%s\n%!"
            (match index with Eavt -> "eavt" | Aevt -> "aevt" | Avet -> "avet" | Tave -> "tave")
            (merged_index db) (pending_overlay db)
            (match a with Some a -> a | None -> "-");
        (* Attr-only reverse keeps ~to_ so the scan stays inside that attr
           (single-field prefix cmp). Multi-component rseek matches upstream:
           start at the upper bound and walk backward with no lower clamp. *)
        let attr_only =
          match index, e, a, v, tx with
          | (Aevt | Avet), None, Some _, None, None -> true
          | _ -> false
        in
        if attr_only then
          Index.rslice_seq ~from_:bound ~to_:bound ~cmp (stored_index db index) |> Index.to_seq
        else
          Index.rslice_seq ~from_:bound ~cmp (stored_index db index) |> Index.to_seq
    in
    (match merged_index db || pending_overlay db with
     | false -> Some indexed
     | true ->
       let duplicates = duplicate_prefix_datoms db index e a |> List.filter (fun datom -> cmp datom bound <= 0) |> List.rev in
       if debug then
         Printf.eprintf "[DS_DEBUG_INDEX] reverse_upper_prefix merge_duplicates n=%d\n%!" (List.length duplicates);
       Some
         (merge_sorted_datom_seqs
            (fun left right -> Util.compare_datom index right left)
            indexed
            (List.to_seq duplicates)))

let avet_range_bounds context db attr start stop =
  let start = Option.map (context.resolve_value_for_attr db attr) start in
  let stop = Option.map (context.resolve_value_for_attr db attr) stop in
  let from_bound =
    match start with
    | Some value -> bound_datom ~a:attr ~v:value ()
    | None -> bound_datom ~a:attr ()
  in
  let from_fields =
    match start with
    | Some _ -> fields ~a:true ~v:true ()
    | None -> fields ~a:true ()
  in
  let to_bound =
    match stop with
    | Some value -> bound_datom ~a:attr ~v:value ()
    | None -> bound_datom ~a:attr ()
  in
  let to_fields =
    match stop with
    | Some _ -> fields ~a:true ~v:true ()
    | None -> fields ~a:true ()
  in
  let lower_matches datom =
    match start with
    | None -> datom.a = attr
    | Some start -> datom.a = attr && context.compare_value datom.v start >= 0
  in
  let upper_matches datom =
    match stop with
    | None -> datom.a = attr
    | Some stop -> datom.a = attr && context.compare_value datom.v stop <= 0
  in
  (from_bound, from_fields, to_bound, to_fields, lower_matches, upper_matches)

let avet_range_datoms context db attr start stop =
  let from_bound, from_fields, to_bound, to_fields, lower_matches, upper_matches =
    avet_range_bounds context db attr start stop
  in
  if temporal_view db then
    let cmp = slice_cmp context Avet from_bound from_fields to_bound to_fields in
    primary_attr_datoms db Avet attr
    |> List.filter (fun datom ->
      cmp datom from_bound >= 0
      && cmp datom to_bound <= 0
      && lower_matches datom
      && upper_matches datom)
    |> List.to_seq
  else
    let attr_cache = Hashtbl.find_opt db.avet_by_attr attr in
    let indexed =
      match attr_cache with
      | Some arr ->
          array_range_seq context Avet from_bound from_fields to_bound to_fields arr
      | None ->
          let cmp = slice_cmp context Avet from_bound from_fields to_bound to_fields in
          Index.slice_seq ~from_:from_bound ~to_:to_bound ~cmp db.avet_index |> Index.to_seq
    in
    if not (merged_index db) && not (pending_overlay db) then indexed
    else if not (merged_index db) then
      (match attr_cache with
       | Some _ -> indexed
       | None ->
         let duplicates =
           pending_for_index db Avet
           |> List.filter (fun datom -> lower_matches datom && upper_matches datom)
         in
         merge_sorted_datom_seqs (Util.compare_datom Avet) indexed (List.to_seq duplicates))
    else if not (pending_overlay db) then
      let duplicates =
        duplicate_attr_datoms db Avet attr
        |> List.filter (fun datom -> lower_matches datom && upper_matches datom)
      in
      merge_sorted_datom_seqs (Util.compare_datom Avet) indexed (List.to_seq duplicates)
    else
      let pending =
        pending_for_index db Avet
        |> List.filter (fun datom -> lower_matches datom && upper_matches datom)
      in
      let duplicates =
        duplicate_attr_datoms db Avet attr
        |> List.filter (fun datom -> lower_matches datom && upper_matches datom)
      in
      merge_sorted_datom_seqs (Util.compare_datom Avet) indexed
        (List.to_seq (merge_sorted_datoms Avet pending duplicates))

let indexed_attr_required_message attr =
  "Attribute :" ^ attr ^ " should be marked as :db/index true"

let validate_index_access context db index attr =
  match index, attr with
  | Avet, Some attr when not (context.is_avet_accessible db attr) ->
    invalid_arg (indexed_attr_required_message attr)
  | _ -> ()

let resolved_entity_ref_option context db = Option.map (context.resolve_entity_ref db)

let resolved_value_option_for_optional_attr context db attr =
  Option.map (context.resolve_value_for_optional_attr db attr)

let datoms context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  let v = resolved_value_option_for_optional_attr context db a v in
  let datoms, exact =
    let prefix_v, prefix_tx =
      match index, e, a, v with
      | Aevt, None, Some _, Some _ -> None, None
      | _ -> v, tx
    in
    match exact_prefix_datoms context db index e a prefix_v prefix_tx with
    | Some datoms -> datoms, true
    | None -> index_datoms_seq db index, false
  in
  let exact_attr_prefix =
    match index, e, a, v, tx with
    | Aevt, None, Some _, None, None -> exact
    | Avet, None, Some _, Some _, None -> exact
    | _ -> false
  in
  let datoms =
    if exact_attr_prefix || (e, a, v, tx) = (None, None, None, None) then
      datoms
    else
      datoms
      |> Seq.filter (fun d -> matches e d.e && matches a d.a && matches_value context v d.v && matches tx d.tx)
  in
  apply_db_view_seq db datoms |> apply_filter_pred db

let fold_datoms f init context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  if temporal_view db then
    datoms context db index ?e ?a ?v ?tx () |> Seq.fold_left f init
  else
  let v = resolved_value_option_for_optional_attr context db a v in
  let prefix_v, prefix_tx =
    match index, e, a, v with
    | Aevt, None, Some _, Some _ -> None, None
    | _ -> v, tx
  in
  let exact_attr_prefix =
    match index, e, a, v, tx with
    | Aevt, None, Some _, None, None -> true
    | _ -> false
  in
  let fold_filter acc datom =
    if matches e datom.e && matches a datom.a && matches_value context v datom.v && matches tx datom.tx then
      f acc datom
    else
      acc
  in
  let fold_filter_pred acc datom =
    match db.filter_pred with
    | None -> f acc datom
    | Some pred -> if pred datom then f acc datom else acc
  in
  let fold_filter_and_pred acc datom =
    match db.filter_pred with
    | Some pred when not (pred datom) -> acc
    | _ -> fold_filter acc datom
  in
  match merged_index db || pending_overlay db, exact_prefix_bound index e a prefix_v prefix_tx with
  | false, Some (bound, bound_fields) ->
    let cmp = exact_prefix_slice_cmp context index bound bound_fields in
    let fold =
      match exact_attr_prefix || (e, a, v, tx) = (None, None, None, None), db.filter_pred with
      | true, None -> f
      | true, Some _ -> fold_filter_pred
      | false, None -> fold_filter
      | false, Some _ -> fold_filter_and_pred
    in
    (match exact_attr_prefix, index, a with
     | true, (Aevt | Avet), Some attr when not (temporal_view db) ->
       (* Fold the attr prefix in place — do not materialize via primary_attr_datoms. *)
       Index.fold_attr_prefix fold init (stored_index db index) attr
     | true, (Aevt | Avet), Some attr ->
       List.fold_left fold init (primary_attr_datoms db index attr)
     | _ ->
       let seq = Index.slice_seq ~from_:bound ~to_:bound ~cmp (stored_index db index) in
       Index.fold_seq fold init seq)
  | false, None when (e, a, v, tx) = (None, None, None, None) ->
    (match db.filter_pred with
     | None -> Index.fold f init (stored_index db index)
     | Some pred ->
       Index.fold (fun acc datom -> if pred datom then f acc datom else acc) init (stored_index db index))
  | _ ->
    datoms context db index ?e ?a ?v ?tx () |> Seq.fold_left f init

let apply_filter_pred_list db datoms =
  match db.filter_pred with
  | None -> datoms
  | Some pred -> List.filter pred datoms

let datoms_list context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  let v = resolved_value_option_for_optional_attr context db a v in
  let datoms, exact =
    let prefix_v, prefix_tx =
      match index, e, a, v with
      | Aevt, None, Some _, Some _ -> None, None
      | _ -> v, tx
    in
    match exact_prefix_datoms_list context db index e a prefix_v prefix_tx with
    | Some datoms -> datoms, true
    | None -> raw_index_datoms_list db index, false
  in
  let exact_attr_prefix =
    match index, e, a, v, tx with
    | Aevt, None, Some _, None, None -> exact
    | _ -> false
  in
  let datoms =
    if exact_attr_prefix || (e, a, v, tx) = (None, None, None, None) then
      datoms
    else
      datoms
      |> List.filter (fun d -> matches e d.e && matches a d.a && matches_value context v d.v && matches tx d.tx)
  in
  apply_db_view db datoms |> apply_filter_pred_list db

let datoms_ref context db index ?e ?a ?v ?tx () =
  let e = resolved_entity_ref_option context db e in
  datoms context db index ?e ?a ?v ?tx ()

let find_datom context db index ?e ?a ?v ?tx () =
  match temporal_view db, db.filter_pred, index, e, a, v, tx with
  | false, None, Aevt, Some entity_id, Some attr, None, None when not (merged_index db || pending_overlay db) ->
    find_primary_aevt_entity_attr db entity_id attr
  | false, None, Eavt, Some entity_id, Some attr, None, None when not (merged_index db || pending_overlay db) ->
    let bound = bound_datom ~e:entity_id ~a:attr () in
    let bound_fields = fields ~e:true ~a:true () in
    let cmp = exact_prefix_slice_cmp context Eavt bound bound_fields in
    Index.find_first_slice ~from_:bound ~to_:bound ~cmp (stored_index db Eavt)
  | _ -> datoms context db index ?e ?a ?v ?tx () |> Seq.uncons |> Option.map fst

let find_datom_ref context db index ?e ?a ?v ?tx () =
  datoms_ref context db index ?e ?a ?v ?tx () |> Seq.uncons |> Option.map fst

let compare_optional actual = function
  | Some expected -> compare actual expected
  | None -> 0

let compare_optional_with compare_item actual = function
  | Some expected -> compare_item actual expected
  | None -> 0

let compare_datom_to_bound context index d e a v tx =
  match index with
  | Eavt ->
    context.first_nonzero
      [ compare_optional d.e e
      ; compare_optional d.a a
      ; compare_optional_with context.compare_value d.v v
      ; compare_optional d.tx tx
      ]
  | Aevt ->
    context.first_nonzero
      [ compare_optional d.a a
      ; compare_optional d.e e
      ; compare_optional_with context.compare_value d.v v
      ; compare_optional d.tx tx
      ]
  | Avet ->
    context.first_nonzero
      [ compare_optional d.a a
      ; compare_optional_with context.compare_value d.v v
      ; compare_optional d.e e
      ; compare_optional d.tx tx
      ]
  | Tave ->
    context.first_nonzero
      [ compare_optional d.tx tx
      ; compare_optional d.a a
      ; compare_optional_with context.compare_value d.v v
      ; compare_optional d.e e
      ]

let seek_datoms context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  let v = resolved_value_option_for_optional_attr context db a v in
  match lower_prefix_datoms context db index e a v tx with
  | Some datoms ->
    apply_db_view_seq db (rehydrate_datom_seq db index datoms) |> apply_filter_pred db
  | None ->
    datoms context db index ()
    |> Seq.filter (fun d -> compare_datom_to_bound context index d e a v tx >= 0)
    |> rehydrate_datom_seq db index
    |> apply_filter_pred db

let seek_datoms_ref context db index ?e ?a ?v ?tx () =
  let e = resolved_entity_ref_option context db e in
  seek_datoms context db index ?e ?a ?v ?tx ()

let rseek_datoms context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  let v = resolved_value_option_for_optional_attr context db a v in
  match reverse_upper_prefix_datoms context db index e a v tx with
  | Some datoms ->
    apply_db_view_reverse_seq db (rehydrate_datom_seq db index datoms) |> apply_filter_pred db
  | None ->
    reverse_index_datoms_seq db index
    |> apply_db_view_reverse_seq db
    |> Seq.filter (fun d -> compare_datom_to_bound context index d e a v tx <= 0)
    |> rehydrate_datom_seq db index
    |> apply_filter_pred db

let rseek_datoms_ref context db index ?e ?a ?v ?tx () =
  let e = resolved_entity_ref_option context db e in
  rseek_datoms context db index ?e ?a ?v ?tx ()

let index_range context db attr ?start ?stop () =
  if not (context.is_avet_accessible db attr) then
    invalid_arg (indexed_attr_required_message attr);
  avet_range_datoms context db attr start stop
  |> apply_filter_pred db

let fold_index_range f init context db attr ?start ?stop () =
  if not (context.is_avet_accessible db attr) then
    invalid_arg (indexed_attr_required_message attr);
  let from_bound, from_fields, to_bound, to_fields, lower_matches, upper_matches =
    avet_range_bounds context db attr start stop
  in
  let fold_with_filter acc datom =
    match db.filter_pred with
    | None -> f acc datom
    | Some pred -> if pred datom then f acc datom else acc
  in
  let attr_cache =
    if temporal_view db then None else Hashtbl.find_opt db.avet_by_attr attr
  in
  if temporal_view db then
    (* Rebuild through primary_attr so pending/history facts are visible. *)
    let cmp = slice_cmp context Avet from_bound from_fields to_bound to_fields in
    primary_attr_datoms db Avet attr
    |> List.filter (fun datom ->
      cmp datom from_bound >= 0
      && cmp datom to_bound <= 0
      && lower_matches datom
      && upper_matches datom)
    |> List.fold_left fold_with_filter init
  else
    let acc =
      match attr_cache with
      | Some arr ->
          array_range_fold fold_with_filter init context Avet from_bound from_fields to_bound to_fields
            arr
      | None ->
          let cmp = slice_cmp context Avet from_bound from_fields to_bound to_fields in
          Index.fold_slice fold_with_filter init ~from_:from_bound ~to_:to_bound ~cmp db.avet_index
    in
    if not (merged_index db) && not (pending_overlay db) then acc
    else if not (merged_index db) then
      (match attr_cache with
       | Some _ -> acc
       | None ->
         pending_for_index db Avet
         |> List.filter (fun datom -> lower_matches datom && upper_matches datom)
         |> List.fold_left fold_with_filter acc)
    else if not (pending_overlay db) then
      duplicate_attr_datoms db Avet attr
      |> List.filter (fun datom -> lower_matches datom && upper_matches datom)
      |> List.fold_left fold_with_filter acc
    else
      let pending =
        pending_for_index db Avet
        |> List.filter (fun datom -> lower_matches datom && upper_matches datom)
      in
      let duplicates =
        duplicate_attr_datoms db Avet attr
        |> List.filter (fun datom -> lower_matches datom && upper_matches datom)
      in
      merge_sorted_datoms Avet pending duplicates |> List.fold_left fold_with_filter acc

let diff left right =
  let left_datoms = visible_index_datoms left Eavt in
  let right_datoms = visible_index_datoms right Eavt in
  ( List.filter (fun d -> not (List.exists (same_fact d) right_datoms)) left_datoms
  , List.filter (fun d -> not (List.exists (same_fact d) left_datoms)) right_datoms
  , List.filter (fun d -> List.exists (same_fact d) right_datoms) left_datoms
  )

let squuid_counter = ref 0
let squuid_random_initialized = ref false
let hex_digits = "0123456789abcdef"

let ensure_squuid_random_initialized () =
  if not !squuid_random_initialized then (
    Random.self_init ();
    squuid_random_initialized := true)

let hex8_of_seconds seconds =
  let bytes = Bytes.make 8 '0' in
  let rec loop index value =
    if index >= 0 then (
      let digit = int_of_float (mod_float value 16.0) in
      Bytes.set bytes index hex_digits.[digit];
      loop (index - 1) (floor (value /. 16.0)))
  in
  loop 7 (floor seconds);
  Bytes.unsafe_to_string bytes

let squuid ?msec () =
  ensure_squuid_random_initialized ();
  incr squuid_counter;
  let seconds =
    match msec with
    | Some msec -> Float.of_int msec /. 1000.0
    | None -> Platform.now_seconds ()
  in
  let seconds_hex = hex8_of_seconds seconds in
  let r1 = Random.bits () land 0xffff in
  let r2 = ((Random.bits () land 0x0fff) lor 0x4000) land 0xffff in
  let r3 = ((Random.bits () land 0x3fff) lor 0x8000) land 0xffff in
  let r4 = !squuid_counter land 0xffff in
  let r5 = Random.bits () land 0xffff in
  let r6 = Random.bits () land 0xffff in
  Uuid (Printf.sprintf "%s-%04x-%04x-%04x-%04x%04x%04x" seconds_hex r1 r2 r3 r4 r5 r6)

let squuid_time_millis = function
  | Uuid uuid ->
    if String.length uuid < 8 then invalid_arg "invalid squuid";
    int_of_string ("0x" ^ String.sub uuid 0 8) * 1000
  | _ -> invalid_arg "squuid_time_millis expects a uuid value"
