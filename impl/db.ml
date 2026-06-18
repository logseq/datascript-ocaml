open Datascript_types

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
  let d = Util.normalize_datom_value d in
  if Schema.schema_attr_is_ref schema d.a then
    match d.v with
    | Int entity_id -> { d with v = Ref (validate_entity_id entity_id) }
    | _ -> d
  else
    d

let datom_has_ref_value = function
  | { v = Ref _; _ } -> true
  | _ -> false

let build_index index datoms =
  datoms |> List.sort (Util.compare_datom index)

let build_avet_index schema datoms =
  datoms
  |> List.filter (fun d -> Schema.schema_attr_is_avet_accessible schema d.a)
  |> build_index Avet

let build_vaet_index datoms =
  datoms
  |> List.filter datom_has_ref_value
  |> build_index Vaet

let build_unique_index schema datoms =
  datoms
  |> List.filter_map (fun d ->
    if d.a = "db/ident" || Option.fold ~none:false ~some:(fun attr -> Option.is_some attr.unique) (List.assoc_opt d.a schema) then
      Some (d.a, d.v, d.e)
    else
      None)

let refresh_indexes db =
  let eavt_index = build_index Eavt db.datoms in
  let aevt_index = build_index Aevt db.datoms in
  let avet_index = build_avet_index db.schema db.datoms in
  let vaet_index = build_vaet_index db.datoms in
  let max_datom_e = List.fold_left (fun max_e d -> max max_e d.e) 0 db.datoms in
  let unique_index = build_unique_index db.schema db.datoms in
  { db with
    eavt_index
  ; aevt_index
  ; avet_index
  ; vaet_index
  ; max_datom_e
  ; unique_index
  ; eavt_array = Array.of_list eavt_index
  ; aevt_array = Array.of_list aevt_index
  ; avet_array = Array.of_list avet_index
  ; vaet_array = Array.of_list vaet_index
  ; index_lists_valid = true
  ; index_arrays_valid = true
  }

let rec merge_sorted compare left right =
  match left, right with
  | [], items | items, [] -> items
  | left_item :: left_rest, right_item :: right_rest ->
    if compare left_item right_item <= 0 then
      left_item :: merge_sorted compare left_rest right
    else
      right_item :: merge_sorted compare left right_rest

let refresh_indexes_with_added_datoms db added_datoms =
  let source_indexes_empty =
    db.eavt_index = [] && db.aevt_index = [] && db.avet_index = [] && db.vaet_index = []
  in
  let max_datom_e = List.fold_left (fun max_e d -> max max_e d.e) db.max_datom_e added_datoms in
  let unique_index = build_unique_index db.schema added_datoms @ db.unique_index in
  if source_indexes_empty then
    let eavt_index =
      merge_sorted (Util.compare_datom Eavt) (build_index Eavt added_datoms) db.eavt_index
    in
    let aevt_index =
      merge_sorted (Util.compare_datom Aevt) (build_index Aevt added_datoms) db.aevt_index
    in
    let avet_index =
      merge_sorted (Util.compare_datom Avet) (build_avet_index db.schema added_datoms) db.avet_index
    in
    let vaet_index =
      merge_sorted (Util.compare_datom Vaet) (build_vaet_index added_datoms) db.vaet_index
    in
    { db with
      eavt_index
    ; aevt_index
    ; avet_index
    ; vaet_index
    ; max_datom_e
    ; unique_index
    ; eavt_array = Array.of_list eavt_index
    ; aevt_array = Array.of_list aevt_index
    ; avet_array = Array.of_list avet_index
    ; vaet_array = Array.of_list vaet_index
    ; index_lists_valid = true
    ; index_arrays_valid = true
    }
  else
    { db with
      max_datom_e
    ; unique_index
    ; eavt_array = [||]
    ; aevt_array = [||]
    ; avet_array = [||]
    ; vaet_array = [||]
    ; index_lists_valid = false
    ; index_arrays_valid = false
    }

let with_datoms db datoms =
  refresh_indexes { db with datoms }

let empty_db context ?(schema = []) ?storage () =
  let schema = Schema.validate_schema schema in
  refresh_indexes
    { db_uid = context.next_db_uid ()
    ; schema
    ; datoms = []
    ; eavt_index = []
    ; aevt_index = []
    ; avet_index = []
    ; vaet_index = []
    ; eavt_array = [||]
    ; aevt_array = [||]
    ; avet_array = [||]
    ; vaet_array = [||]
    ; index_lists_valid = true
    ; index_arrays_valid = true
    ; history_datoms = []
    ; historical = false
    ; max_eid = 0
    ; max_datom_e = 0
    ; max_tx = tx0
    ; unique_index = []
    ; filter_pred = None
    ; storage_ref = storage
    ; tx_fns = []
    }

let empty context db = empty_db context ~schema:db.schema ?storage:db.storage_ref ()

let history_datoms_for_schema schema tx_data =
  List.filter (fun d -> not (Schema.schema_has_no_history schema d.a)) tx_data

let init_db context ?(schema = []) ?storage datoms =
  let schema = Schema.validate_schema schema in
  let datoms = List.map (normalize_datom_for_schema schema) datoms in
  let history_datoms = history_datoms_for_schema schema datoms in
  let max_eid =
    List.fold_left (fun max_eid d -> max_eid_in_value (max_eid_with_entity_id max_eid d.e) d.v) 0 datoms
  in
  let max_tx = List.fold_left (fun max_tx d -> max max_tx d.tx) tx0 datoms in
  refresh_indexes
    { db_uid = context.next_db_uid ()
    ; schema
    ; datoms
    ; eavt_index = []
    ; aevt_index = []
    ; avet_index = []
    ; vaet_index = []
    ; eavt_array = [||]
    ; aevt_array = [||]
    ; avet_array = [||]
    ; vaet_array = [||]
    ; index_lists_valid = true
    ; index_arrays_valid = true
    ; history_datoms
    ; historical = false
    ; max_eid
    ; max_datom_e = 0
    ; max_tx
    ; unique_index = []
    ; filter_pred = None
    ; storage_ref = storage
    ; tx_fns = []
    }

let history context db = with_datoms (refresh_identity context { db with historical = true }) db.history_datoms

let is_history db = db.historical

let visible_active_datoms db =
  match db.filter_pred with
  | None -> db.datoms
  | Some pred -> List.filter pred db.datoms

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
        , db.datoms
        , db.history_datoms
        , db.historical
        , db.max_eid
        , db.max_tx
        )
    in
    Hashtbl.replace hash_cache db.db_uid hash;
    hash

let hash_cache_size () = Hashtbl.length hash_cache

let build_index_for_db db = function
  | Eavt -> build_index Eavt db.datoms
  | Aevt -> build_index Aevt db.datoms
  | Avet -> build_avet_index db.schema db.datoms
  | Vaet -> build_vaet_index db.datoms

let stored_index_datoms_list db = function
  | Eavt -> db.eavt_index
  | Aevt -> db.aevt_index
  | Avet -> db.avet_index
  | Vaet -> db.vaet_index

let raw_index_datoms_list db index =
  if db.index_lists_valid then
    stored_index_datoms_list db index
  else
    build_index_for_db db index

let visible_index_datoms db index =
  let datoms = raw_index_datoms_list db index in
  match db.filter_pred with
  | None -> datoms
  | Some pred -> List.filter pred datoms

let raw_index_datoms_array db index =
  match index with
  | Eavt -> db.eavt_array
  | Aevt -> db.aevt_array
  | Avet -> db.avet_array
  | Vaet -> db.vaet_array

let lower_bound_before before items =
  let rec loop low high =
    if low >= high then
      low
    else
      let middle = low + ((high - low) / 2) in
      if before items.(middle) then
        loop (middle + 1) high
      else
        loop low middle
  in
  loop 0 (Array.length items)

let rec array_seq_take_while items index pred () =
  if index >= Array.length items then
    Seq.Nil
  else
    let item = items.(index) in
    if pred item then
      Seq.Cons (item, array_seq_take_while items (index + 1) pred)
    else
      Seq.Nil

let rec array_seq_range items index stop () =
  if index >= stop then
    Seq.Nil
  else
    Seq.Cons (items.(index), array_seq_range items (index + 1) stop)

let seq_slice ~before ~inside items =
  let start = lower_bound_before before items in
  array_seq_take_while items start inside

let seq_range ~before_start ~before_stop items =
  let start = lower_bound_before before_start items in
  let stop = lower_bound_before before_stop items in
  array_seq_range items start stop

let compare_attr = compare
let compare_entity = compare
let compare_tx = compare

let compare_value_order context left right =
  context.compare_value left right

let value_order_equal context left right =
  compare_value_order context left right = 0

let slice_eavt context items e a v tx =
  match e, a, v, tx with
  | Some e, Some a, Some v, Some tx ->
    seq_slice
      ~before:(fun d ->
        compare_entity d.e e < 0
        || (d.e = e && compare_attr d.a a < 0)
        || (d.e = e && d.a = a && compare_value_order context d.v v < 0)
        || (d.e = e && d.a = a && value_order_equal context d.v v && compare_tx d.tx tx < 0))
      ~inside:(fun d -> d.e = e && d.a = a && value_order_equal context d.v v && d.tx = tx)
      items
  | Some e, Some a, Some v, None ->
    seq_slice
      ~before:(fun d ->
        compare_entity d.e e < 0
        || (d.e = e && compare_attr d.a a < 0)
        || (d.e = e && d.a = a && compare_value_order context d.v v < 0))
      ~inside:(fun d -> d.e = e && d.a = a && value_order_equal context d.v v)
      items
  | Some e, Some a, _, _ ->
    seq_slice
      ~before:(fun d -> compare_entity d.e e < 0 || (d.e = e && compare_attr d.a a < 0))
      ~inside:(fun d -> d.e = e && d.a = a)
      items
  | Some e, _, _, _ ->
    seq_range
      ~before_start:(fun d -> compare_entity d.e e < 0)
      ~before_stop:(fun d -> compare_entity d.e e <= 0)
      items
  | _ -> Array.to_seq items

let slice_aevt context items e a v tx =
  match a, e, v, tx with
  | Some a, Some e, Some v, Some tx ->
    seq_slice
      ~before:(fun d ->
        compare_attr d.a a < 0
        || (d.a = a && compare_entity d.e e < 0)
        || (d.a = a && d.e = e && compare_value_order context d.v v < 0)
        || (d.a = a && d.e = e && value_order_equal context d.v v && compare_tx d.tx tx < 0))
      ~inside:(fun d -> d.a = a && d.e = e && value_order_equal context d.v v && d.tx = tx)
      items
  | Some a, Some e, Some v, None ->
    seq_slice
      ~before:(fun d ->
        compare_attr d.a a < 0
        || (d.a = a && compare_entity d.e e < 0)
        || (d.a = a && d.e = e && compare_value_order context d.v v < 0))
      ~inside:(fun d -> d.a = a && d.e = e && value_order_equal context d.v v)
      items
  | Some a, Some e, _, _ ->
    seq_slice
      ~before:(fun d -> compare_attr d.a a < 0 || (d.a = a && compare_entity d.e e < 0))
      ~inside:(fun d -> d.a = a && d.e = e)
      items
  | Some a, _, _, _ ->
    seq_range
      ~before_start:(fun d -> compare_attr d.a a < 0)
      ~before_stop:(fun d -> compare_attr d.a a <= 0)
      items
  | _ -> Array.to_seq items

let slice_avet context items e a v tx =
  match a, v, e, tx with
  | Some a, Some v, Some e, Some tx ->
    seq_slice
      ~before:(fun d ->
        compare_attr d.a a < 0
        || (d.a = a && compare_value_order context d.v v < 0)
        || (d.a = a && value_order_equal context d.v v && compare_entity d.e e < 0)
        || (d.a = a && value_order_equal context d.v v && d.e = e && compare_tx d.tx tx < 0))
      ~inside:(fun d -> d.a = a && value_order_equal context d.v v && d.e = e && d.tx = tx)
      items
  | Some a, Some v, Some e, None ->
    seq_slice
      ~before:(fun d ->
        compare_attr d.a a < 0
        || (d.a = a && compare_value_order context d.v v < 0)
        || (d.a = a && value_order_equal context d.v v && compare_entity d.e e < 0))
      ~inside:(fun d -> d.a = a && value_order_equal context d.v v && d.e = e)
      items
  | Some a, Some v, _, _ ->
    seq_slice
      ~before:(fun d -> compare_attr d.a a < 0 || (d.a = a && compare_value_order context d.v v < 0))
      ~inside:(fun d -> d.a = a && value_order_equal context d.v v)
      items
  | Some a, _, _, _ ->
    seq_range
      ~before_start:(fun d -> compare_attr d.a a < 0)
      ~before_stop:(fun d -> compare_attr d.a a <= 0)
      items
  | _ -> Array.to_seq items

let slice_vaet context items e a v tx =
  match v, a, e, tx with
  | Some v, Some a, Some e, Some tx ->
    seq_slice
      ~before:(fun d ->
        compare_value_order context d.v v < 0
        || (value_order_equal context d.v v && compare_attr d.a a < 0)
        || (value_order_equal context d.v v && d.a = a && compare_entity d.e e < 0)
        || (value_order_equal context d.v v && d.a = a && d.e = e && compare_tx d.tx tx < 0))
      ~inside:(fun d -> value_order_equal context d.v v && d.a = a && d.e = e && d.tx = tx)
      items
  | Some v, Some a, Some e, None ->
    seq_slice
      ~before:(fun d ->
        compare_value_order context d.v v < 0
        || (value_order_equal context d.v v && compare_attr d.a a < 0)
        || (value_order_equal context d.v v && d.a = a && compare_entity d.e e < 0))
      ~inside:(fun d -> value_order_equal context d.v v && d.a = a && d.e = e)
      items
  | Some v, Some a, _, _ ->
    seq_slice
      ~before:(fun d -> compare_value_order context d.v v < 0 || (value_order_equal context d.v v && compare_attr d.a a < 0))
      ~inside:(fun d -> value_order_equal context d.v v && d.a = a)
      items
  | Some v, _, _, _ ->
    seq_range
      ~before_start:(fun d -> compare_value_order context d.v v < 0)
      ~before_stop:(fun d -> compare_value_order context d.v v <= 0)
      items
  | _ -> Array.to_seq items

let bounded_index_datoms_seq context db index e a v tx =
  if db.index_arrays_valid then
    let items = raw_index_datoms_array db index in
    match index with
    | Eavt -> slice_eavt context items e a v tx
    | Aevt -> slice_aevt context items e a v tx
    | Avet -> slice_avet context items e a v tx
    | Vaet -> slice_vaet context items e a v tx
  else
    raw_index_datoms_list db index |> List.to_seq

let apply_filter_pred db seq =
  match db.filter_pred with
  | None -> seq
  | Some pred -> Seq.filter pred seq

let matches maybe expected = Option.fold ~none:true ~some:(fun actual -> actual = expected) maybe

let matches_value maybe expected =
  Option.fold ~none:true ~some:(fun actual -> value_equal actual expected) maybe

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

let slice_covers_components index e a v tx =
  match index with
  | Eavt ->
    (match e, a, v, tx with
     | None, None, None, None -> true
     | Some _, None, None, None -> true
     | Some _, Some _, None, None -> true
     | Some _, Some _, Some _, None -> true
     | Some _, Some _, Some _, Some _ -> true
     | _ -> false)
  | Aevt ->
    (match a, e, v, tx with
     | None, None, None, None -> true
     | Some _, None, None, None -> true
     | Some _, Some _, None, None -> true
     | Some _, Some _, Some _, None -> true
     | Some _, Some _, Some _, Some _ -> true
     | _ -> false)
  | Avet ->
    (match a, v, e, tx with
     | None, None, None, None -> true
     | Some _, None, None, None -> true
     | Some _, Some _, None, None -> true
     | Some _, Some _, Some _, None -> true
     | Some _, Some _, Some _, Some _ -> true
     | _ -> false)
  | Vaet ->
    (match v, a, e, tx with
     | None, None, None, None -> true
     | Some _, None, None, None -> true
     | Some _, Some _, None, None -> true
     | Some _, Some _, Some _, None -> true
     | Some _, Some _, Some _, Some _ -> true
     | _ -> false)

let datoms context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  let v = resolved_value_option_for_optional_attr context db a v in
  let datoms = bounded_index_datoms_seq context db index e a v tx in
  let datoms =
    if
      (e, a, v, tx) = (None, None, None, None)
      || (db.index_arrays_valid && slice_covers_components index e a v tx)
    then
      datoms
    else
      datoms
      |> Seq.filter (fun d -> matches e d.e && matches a d.a && matches_value v d.v && matches tx d.tx)
  in
  apply_filter_pred db datoms

let datoms_ref context db index ?e ?a ?v ?tx () =
  let e = resolved_entity_ref_option context db e in
  datoms context db index ?e ?a ?v ?tx ()

let find_datom context db index ?e ?a ?v ?tx () =
  datoms context db index ?e ?a ?v ?tx () |> Seq.uncons |> Option.map fst

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
  | Vaet ->
    context.first_nonzero
      [ compare_optional_with context.compare_value d.v v
      ; compare_optional d.a a
      ; compare_optional d.e e
      ; compare_optional d.tx tx
      ]

let seek_datoms context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  let v = resolved_value_option_for_optional_attr context db a v in
  datoms context db index ()
  |> Seq.filter (fun d -> compare_datom_to_bound context index d e a v tx >= 0)
  |> List.of_seq

let seek_datoms_ref context db index ?e ?a ?v ?tx () =
  let e = resolved_entity_ref_option context db e in
  seek_datoms context db index ?e ?a ?v ?tx ()

let rseek_datoms context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  let v = resolved_value_option_for_optional_attr context db a v in
  datoms context db index ()
  |> Seq.filter (fun d -> compare_datom_to_bound context index d e a v tx <= 0)
  |> List.of_seq
  |> List.rev

let rseek_datoms_ref context db index ?e ?a ?v ?tx () =
  let e = resolved_entity_ref_option context db e in
  rseek_datoms context db index ?e ?a ?v ?tx ()

let index_range context db attr ?start ?stop () =
  if not (context.is_avet_accessible db attr) then
    invalid_arg (indexed_attr_required_message attr);
  let start = Option.map (context.resolve_value_for_attr db attr) start in
  let stop = Option.map (context.resolve_value_for_attr db attr) stop in
  datoms context db Avet ~a:attr ()
  |> Seq.filter (fun d ->
    Option.fold ~none:true ~some:(fun start -> context.compare_value d.v start >= 0) start
    && Option.fold ~none:true ~some:(fun stop -> context.compare_value d.v stop <= 0) stop)
  |> List.of_seq

let diff left right =
  let left_datoms = visible_index_datoms left Eavt in
  let right_datoms = visible_index_datoms right Eavt in
  ( List.filter (fun d -> not (List.exists (same_fact d) right_datoms)) left_datoms
  , List.filter (fun d -> not (List.exists (same_fact d) left_datoms)) right_datoms
  , List.filter (fun d -> List.exists (same_fact d) right_datoms) left_datoms
  )

let squuid_counter = ref 0

let squuid ?msec () =
  incr squuid_counter;
  let msec =
    match msec with
    | Some msec -> msec
    | None -> int_of_float (Unix.gettimeofday () *. 1000.0)
  in
  let seconds = msec / 1000 in
  let r1 = Random.bits () land 0xffff in
  let r2 = ((Random.bits () land 0x0fff) lor 0x4000) land 0xffff in
  let r3 = ((Random.bits () land 0x3fff) lor 0x8000) land 0xffff in
  let r4 = !squuid_counter land 0xffff in
  let r5 = Random.bits () land 0xffff in
  let r6 = Random.bits () land 0xffff in
  Uuid (Printf.sprintf "%08x-%04x-%04x-%04x-%04x%04x%04x" seconds r1 r2 r3 r4 r5 r6)

let squuid_time_millis = function
  | Uuid uuid ->
    if String.length uuid < 8 then invalid_arg "invalid squuid";
    int_of_string ("0x" ^ String.sub uuid 0 8) * 1000
  | _ -> invalid_arg "squuid_time_millis expects a uuid value"
