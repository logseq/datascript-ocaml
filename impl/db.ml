open Datascript_types

module PSet = Persistent_sorted_set

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

let empty_index index =
  PSet.empty_by ~cmp:(Util.compare_datom index) ()

let build_index index datoms =
  let cmp = Util.compare_datom index in
  let items = Array.of_list datoms in
  Array.sort cmp items;
  PSet.of_sorted_array_by ~cmp items

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

let duplicate_eavt_by_entity duplicate_datoms =
  let table = Hashtbl.create 1024 in
  List.iter
    (fun datom ->
      let existing = Option.value (Hashtbl.find_opt table datom.e) ~default:[] in
      Hashtbl.replace table datom.e (datom :: existing))
    duplicate_datoms;
  Hashtbl.iter (fun entity_id datoms -> Hashtbl.replace table entity_id (List.rev datoms)) table;
  table

let build_avet_index schema datoms =
  datoms
  |> List.filter (fun d -> Schema.schema_attr_is_avet_accessible schema d.a)
  |> build_index Avet

let set_indexes_from_datoms db datoms =
  let eavt_index = build_index Eavt datoms in
  let aevt_index = build_index Aevt datoms in
  let avet_index = build_avet_index db.schema datoms in
  let duplicate_datoms = duplicate_datoms datoms in
  let duplicate_aevt_datoms = List.sort (Util.compare_datom Aevt) duplicate_datoms in
  let duplicate_avet_datoms =
    duplicate_datoms
    |> List.filter (fun datom -> Schema.schema_attr_is_avet_accessible db.schema datom.a)
    |> List.sort (Util.compare_datom Avet)
  in
  let duplicate_eavt_by_entity = duplicate_eavt_by_entity duplicate_datoms in
  let max_datom_e = List.fold_left (fun max_e d -> max max_e d.e) 0 datoms in
  { db with
    eavt_index
  ; aevt_index
  ; avet_index
  ; duplicate_datoms
  ; duplicate_aevt_datoms
  ; duplicate_avet_datoms
  ; duplicate_eavt_by_entity
  ; max_datom_e
  }

let eavt_datoms db =
  PSet.to_list db.eavt_index @ db.duplicate_datoms |> List.sort (Util.compare_datom Eavt)

let refresh_indexes db =
  set_indexes_from_datoms db (eavt_datoms db)

let add_datoms_to_index include_datom datoms index_set =
  List.fold_left
    (fun index_set datom ->
      if include_datom datom then PSet.add datom index_set else index_set)
    index_set
    datoms

let refresh_indexes_with_added_datoms db added_datoms =
  let max_datom_e = List.fold_left (fun max_e d -> max max_e d.e) db.max_datom_e added_datoms in
  { db with
    eavt_index = add_datoms_to_index (fun _ -> true) added_datoms db.eavt_index
  ; aevt_index = add_datoms_to_index (fun _ -> true) added_datoms db.aevt_index
  ; avet_index =
      add_datoms_to_index
        (fun d -> Schema.schema_attr_is_avet_accessible db.schema d.a)
        added_datoms
        db.avet_index
  ; duplicate_datoms = db.duplicate_datoms
  ; duplicate_aevt_datoms = db.duplicate_aevt_datoms
  ; duplicate_avet_datoms = db.duplicate_avet_datoms
  ; duplicate_eavt_by_entity = db.duplicate_eavt_by_entity
  ; max_datom_e
  }

let with_datoms db datoms =
  set_indexes_from_datoms db datoms

let empty_db context ?(schema = []) ?storage () =
  let schema = Schema.validate_schema schema in
  { db_uid = context.next_db_uid ()
  ; schema
  ; eavt_index = empty_index Eavt
  ; aevt_index = empty_index Aevt
  ; avet_index = empty_index Avet
  ; duplicate_datoms = []
  ; duplicate_aevt_datoms = []
  ; duplicate_avet_datoms = []
  ; duplicate_eavt_by_entity = Hashtbl.create 0
  ; max_eid = 0
  ; max_datom_e = 0
  ; max_tx = tx0
  ; filter_pred = None
  ; storage_ref = storage
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
  { db_uid = context.next_db_uid ()
  ; schema
  ; eavt_index = empty_index Eavt
  ; aevt_index = empty_index Aevt
  ; avet_index = empty_index Avet
  ; duplicate_datoms = []
  ; duplicate_aevt_datoms = []
  ; duplicate_avet_datoms = []
  ; duplicate_eavt_by_entity = Hashtbl.create 0
  ; max_eid
  ; max_datom_e = 0
  ; max_tx
  ; filter_pred = None
  ; storage_ref = storage
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

let duplicate_index_datoms db index =
  match index with
  | Eavt -> db.duplicate_datoms
  | Aevt -> db.duplicate_aevt_datoms
  | Avet -> db.duplicate_avet_datoms

let duplicate_exact_prefix_datoms db index e =
  match index, e with
  | Eavt, Some entity_id -> Option.value (Hashtbl.find_opt db.duplicate_eavt_by_entity entity_id) ~default:[]
  | _ -> duplicate_index_datoms db index

let exact_sorted_slice cmp bound datoms =
  let rec drop_before = function
    | datom :: rest when cmp datom bound < 0 -> drop_before rest
    | datoms -> take_equal [] datoms
  and take_equal acc = function
    | datom :: rest when cmp datom bound = 0 -> take_equal (datom :: acc) rest
    | _ -> List.rev acc
  in
  drop_before datoms

let raw_index_datoms_list db index =
  merge_sorted_datoms index (stored_index db index |> PSet.to_list) (duplicate_index_datoms db index)

let visible_index_datoms db index =
  let datoms = raw_index_datoms_list db index in
  match db.filter_pred with
  | None -> datoms
  | Some pred -> List.filter pred datoms

let index_datoms_seq db index =
  match db.duplicate_datoms with
  | [] -> stored_index db index |> PSet.seq |> PSet.to_seq
  | _ -> raw_index_datoms_list db index |> List.to_seq

let apply_filter_pred db seq =
  match db.filter_pred with
  | None -> seq
  | Some pred -> Seq.filter pred seq

let matches maybe expected = Option.fold ~none:true ~some:(fun actual -> actual = expected) maybe

let matches_value context maybe expected =
  Option.fold ~none:true ~some:(fun actual -> context.compare_value actual expected = 0) maybe

type bound_fields =
  { bound_e : bool
  ; bound_a : bool
  ; bound_v : bool
  ; bound_tx : bool
  }

let bound_datom ?(e = 0) ?(a = "") ?(v = Nil) ?(tx = tx0) () =
  { e; a; v; tx; added = true }

let compare_bound_fields context fields left right order =
  let compare_field = function
    | `E when fields.bound_e -> compare left.e right.e
    | `A when fields.bound_a -> compare left.a right.a
    | `V when fields.bound_v -> context.compare_value left.v right.v
    | `Tx when fields.bound_tx -> compare left.tx right.tx
    | _ -> 0
  in
  order |> List.map compare_field |> context.first_nonzero

let index_order = function
  | Eavt -> [ `E; `A; `V; `Tx ]
  | Aevt -> [ `A; `E; `V; `Tx ]
  | Avet -> [ `A; `V; `E; `Tx ]

let slice_cmp context index from_bound from_fields to_bound to_fields left right =
  if right == from_bound then
    compare_bound_fields context from_fields left right (index_order index)
  else if right == to_bound then
    compare_bound_fields context to_fields left right (index_order index)
  else
    Util.compare_datom index left right

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

let exact_prefix_datoms context db index e a v tx =
  match exact_prefix_bound index e a v tx with
  | None -> None
  | Some (bound, bound_fields) ->
    let cmp = slice_cmp context index bound bound_fields bound bound_fields in
    (match db.duplicate_datoms with
     | [] -> Some (PSet.slice_seq ~from_:bound ~to_:bound ~cmp (stored_index db index) |> PSet.to_seq)
     | _ ->
       let indexed = PSet.slice ~from_:bound ~to_:bound ~cmp (stored_index db index) in
       let duplicates = duplicate_exact_prefix_datoms db index e |> exact_sorted_slice cmp bound in
       Some (merge_sorted_datoms index indexed duplicates |> List.to_seq))

let lower_prefix_datoms context db index e a v tx =
  match exact_prefix_bound index e a v tx with
  | None -> None
  | Some (bound, bound_fields) ->
    let cmp = slice_cmp context index bound bound_fields bound bound_fields in
    let indexed = PSet.slice ~from_:bound ~cmp (stored_index db index) in
    (match db.duplicate_datoms with
     | [] -> Some indexed
     | _ ->
       let duplicates =
         duplicate_index_datoms db index
         |> List.filter (fun datom -> cmp datom bound >= 0)
       in
       Some (merge_sorted_datoms index indexed duplicates))

let reverse_upper_prefix_datoms context db index e a v tx =
  match exact_prefix_bound index e a v tx with
  | None -> None
  | Some (bound, bound_fields) ->
    let cmp = slice_cmp context index bound bound_fields bound bound_fields in
    let indexed = PSet.rslice ~from_:bound ~cmp (stored_index db index) in
    (match db.duplicate_datoms with
     | [] -> Some indexed
     | _ ->
       let duplicates =
         duplicate_index_datoms db index
         |> List.filter (fun datom -> cmp datom bound <= 0)
       in
       Some ((indexed @ duplicates) |> List.sort (fun left right -> Util.compare_datom index right left)))

let avet_range_datoms context db attr start stop =
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
  let cmp = slice_cmp context Avet from_bound from_fields to_bound to_fields in
  let indexed = PSet.slice ~from_:from_bound ~to_:to_bound ~cmp db.avet_index in
  match db.duplicate_datoms with
  | [] -> indexed
  | _ ->
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
    let duplicates =
      duplicate_index_datoms db Avet
      |> List.filter (fun datom -> lower_matches datom && upper_matches datom)
    in
    merge_sorted_datoms Avet indexed duplicates

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
  let datoms =
    match exact_prefix_datoms context db index e a v tx with
    | Some datoms -> datoms
    | None -> index_datoms_seq db index
  in
  let datoms =
    if (e, a, v, tx) = (None, None, None, None) then
      datoms
    else
      datoms
      |> Seq.filter (fun d -> matches e d.e && matches a d.a && matches_value context v d.v && matches tx d.tx)
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

let seek_datoms context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  let v = resolved_value_option_for_optional_attr context db a v in
  match lower_prefix_datoms context db index e a v tx with
  | Some datoms -> apply_filter_pred db (List.to_seq datoms) |> List.of_seq
  | None ->
    datoms context db index ()
    |> Seq.filter (fun d -> compare_datom_to_bound context index d e a v tx >= 0)
    |> List.of_seq

let seek_datoms_ref context db index ?e ?a ?v ?tx () =
  let e = resolved_entity_ref_option context db e in
  seek_datoms context db index ?e ?a ?v ?tx ()

let rseek_datoms context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  let v = resolved_value_option_for_optional_attr context db a v in
  match reverse_upper_prefix_datoms context db index e a v tx with
  | Some datoms -> apply_filter_pred db (List.to_seq datoms) |> List.of_seq
  | None ->
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
  avet_range_datoms context db attr start stop
  |> List.to_seq
  |> apply_filter_pred db
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
