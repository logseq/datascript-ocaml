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

let duplicate_datoms_by_attr duplicate_datoms =
  let table = Hashtbl.create 1024 in
  List.iter
    (fun datom ->
      let existing = Option.value (Hashtbl.find_opt table datom.a) ~default:[] in
      Hashtbl.replace table datom.a (datom :: existing))
    duplicate_datoms;
  Hashtbl.iter (fun attr datoms -> Hashtbl.replace table attr (List.rev datoms)) table;
  table

let build_avet_index schema datoms =
  datoms
  |> List.filter (fun d -> Schema.schema_attr_is_avet_accessible schema d.a)
  |> build_index Avet

let datoms_by_attr datoms =
  let table = Hashtbl.create 1024 in
  List.iter
    (fun datom ->
      let existing = Option.value (Hashtbl.find_opt table datom.a) ~default:[] in
      Hashtbl.replace table datom.a (datom :: existing))
    datoms;
  Hashtbl.iter (fun attr datoms -> Hashtbl.replace table attr (List.rev datoms)) table;
  table

let attr_tables_are_lazy db =
  Option.is_some db.storage_ref
  && Hashtbl.length db.aevt_by_attr = 0
  && Hashtbl.length db.avet_by_attr = 0

let insert_sorted compare_datom datom datoms =
  let rec loop acc = function
    | [] -> List.rev (datom :: acc)
    | current :: rest when compare_datom datom current <= 0 ->
      List.rev_append acc (datom :: current :: rest)
    | current :: rest -> loop (current :: acc) rest
  in
  loop [] datoms

let same_indexed_datom left right =
  left.e = right.e
  && left.a = right.a
  && value_equal left.v right.v
  && left.tx = right.tx
  && left.added = right.added

let remove_sorted_datom datom datoms =
  let rec loop acc = function
    | [] -> List.rev acc
    | current :: rest when same_indexed_datom current datom -> List.rev_append acc rest
    | current :: rest -> loop (current :: acc) rest
  in
  loop [] datoms

let add_attr_table_datom table compare_datom datom =
  let table = Hashtbl.copy table in
  let existing = Option.value (Hashtbl.find_opt table datom.a) ~default:[] in
  Hashtbl.replace table datom.a (insert_sorted compare_datom datom existing);
  table

let remove_attr_table_datom table datom =
  let table = Hashtbl.copy table in
  (match Hashtbl.find_opt table datom.a with
   | None -> ()
   | Some existing ->
     let remaining = remove_sorted_datom datom existing in
     if remaining = [] then Hashtbl.remove table datom.a
     else Hashtbl.replace table datom.a remaining);
  table

let add_datom_to_attr_tables db datom =
  if attr_tables_are_lazy db then
    db
  else
    { db with
      aevt_by_attr = add_attr_table_datom db.aevt_by_attr (Util.compare_datom Aevt) datom
    ; avet_by_attr =
        if Schema.schema_attr_is_avet_accessible db.schema datom.a then
          add_attr_table_datom db.avet_by_attr (Util.compare_datom Avet) datom
        else
          db.avet_by_attr
    }

let remove_datom_from_attr_tables db datom =
  if attr_tables_are_lazy db then
    db
  else
    { db with
      aevt_by_attr = remove_attr_table_datom db.aevt_by_attr datom
    ; avet_by_attr =
        if Schema.schema_attr_is_avet_accessible db.schema datom.a then
          remove_attr_table_datom db.avet_by_attr datom
        else
          db.avet_by_attr
    }

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
  let duplicate_aevt_by_attr = duplicate_datoms_by_attr duplicate_aevt_datoms in
  let duplicate_avet_by_attr = duplicate_datoms_by_attr duplicate_avet_datoms in
  let max_datom_e = List.fold_left (fun max_e d -> max max_e d.e) 0 datoms in
  { db with
    eavt_index
  ; aevt_index
  ; avet_index
  ; aevt_by_attr = datoms_by_attr (PSet.to_list aevt_index)
  ; avet_by_attr = datoms_by_attr (PSet.to_list avet_index)
  ; duplicate_datoms
  ; duplicate_aevt_datoms
  ; duplicate_avet_datoms
  ; duplicate_eavt_by_entity
  ; duplicate_aevt_by_attr
  ; duplicate_avet_by_attr
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
  ; duplicate_aevt_by_attr = db.duplicate_aevt_by_attr
  ; duplicate_avet_by_attr = db.duplicate_avet_by_attr
  ; max_datom_e
  }
  |> fun db -> List.fold_left add_datom_to_attr_tables db added_datoms

let find_active_datom_by_fact db datom =
  let bound = { datom with tx = tx0; added = true } in
  let compare_to_fact left right =
    Util.first_nonzero
      [ compare left.e right.e
      ; compare left.a right.a
      ; Util.compare_value left.v right.v
      ]
  in
  let cmp left right =
    if right == bound then
      compare_to_fact left right
    else
      Util.compare_datom Eavt left right
  in
  let duplicate_matches =
    Option.value (Hashtbl.find_opt db.duplicate_eavt_by_entity datom.e) ~default:[]
    |> List.filter (fun active -> active.a = datom.a && value_equal active.v datom.v)
  in
  match PSet.slice ~from_:bound ~to_:bound ~cmp db.eavt_index @ duplicate_matches with
  | [] -> None
  | matches -> Some (matches |> List.sort (Util.compare_datom Eavt) |> List.hd)

let add_datom_to_indexes db datom =
  { db with
    eavt_index = PSet.add datom db.eavt_index
  ; aevt_index = PSet.add datom db.aevt_index
  ; avet_index =
      if Schema.schema_attr_is_avet_accessible db.schema datom.a then
        PSet.add datom db.avet_index
      else
        db.avet_index
  ; max_datom_e = max db.max_datom_e datom.e
  }
  |> fun db -> add_datom_to_attr_tables db datom

let remove_datom_from_indexes db datom =
  match find_active_datom_by_fact db datom with
  | None -> db
  | Some active ->
    { db with
      eavt_index = PSet.remove active db.eavt_index
    ; aevt_index = PSet.remove active db.aevt_index
    ; avet_index = PSet.remove active db.avet_index
    }
    |> fun db -> remove_datom_from_attr_tables db active

let refresh_indexes_with_tx_data db tx_data =
  List.fold_left
    (fun db datom ->
      if datom.added then add_datom_to_indexes db datom
      else remove_datom_from_indexes db datom)
    db
    tx_data

let with_datoms db datoms =
  set_indexes_from_datoms db datoms

let empty_db context ?(schema = []) ?storage () =
  let schema = Schema.validate_schema schema in
  { db_uid = context.next_db_uid ()
  ; schema
  ; eavt_index = empty_index Eavt
  ; aevt_index = empty_index Aevt
  ; avet_index = empty_index Avet
  ; aevt_by_attr = Hashtbl.create 0
  ; avet_by_attr = Hashtbl.create 0
  ; duplicate_datoms = []
  ; duplicate_aevt_datoms = []
  ; duplicate_avet_datoms = []
  ; duplicate_eavt_by_entity = Hashtbl.create 0
  ; duplicate_aevt_by_attr = Hashtbl.create 0
  ; duplicate_avet_by_attr = Hashtbl.create 0
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
  ; aevt_by_attr = Hashtbl.create 0
  ; avet_by_attr = Hashtbl.create 0
  ; duplicate_datoms = []
  ; duplicate_aevt_datoms = []
  ; duplicate_avet_datoms = []
  ; duplicate_eavt_by_entity = Hashtbl.create 0
  ; duplicate_aevt_by_attr = Hashtbl.create 0
  ; duplicate_avet_by_attr = Hashtbl.create 0
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
  | Eavt -> db.duplicate_datoms
  | Aevt -> db.duplicate_aevt_datoms
  | Avet -> db.duplicate_avet_datoms

let duplicate_attr_datoms db index attr =
  match index with
  | Aevt -> Option.value (Hashtbl.find_opt db.duplicate_aevt_by_attr attr) ~default:[]
  | Avet -> Option.value (Hashtbl.find_opt db.duplicate_avet_by_attr attr) ~default:[]
  | Eavt -> duplicate_index_datoms db index

let primary_attr_datoms db index attr =
  let attr_prefix_datoms index index_set =
    let bound = datom ~e:0 ~a:attr ~v:Nil () in
    let compare_prefix left right = compare left.a right.a in
    let cmp left right =
      if right == bound then compare_prefix left right
      else if left == bound then -compare_prefix right left
      else Util.compare_datom index left right
    in
    PSet.slice ~from_:bound ~to_:bound ~cmp index_set
  in
  match index with
  | Aevt ->
    (match Hashtbl.find_opt db.aevt_by_attr attr with
     | Some datoms -> datoms
     | None -> attr_prefix_datoms Aevt db.aevt_index)
  | Avet ->
    (match Hashtbl.find_opt db.avet_by_attr attr with
     | Some datoms -> datoms
     | None -> attr_prefix_datoms Avet db.avet_index)
  | Eavt -> PSet.to_list db.eavt_index

let duplicate_prefix_datoms db index e a =
  match index, e, a with
  | Eavt, Some entity_id, _ -> Option.value (Hashtbl.find_opt db.duplicate_eavt_by_entity entity_id) ~default:[]
  | (Aevt | Avet), _, Some attr -> duplicate_attr_datoms db index attr
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

let reverse_index_datoms_seq db index =
  match db.duplicate_datoms with
  | [] -> stored_index db index |> PSet.rslice_seq |> PSet.to_seq
  | _ ->
    let indexed = stored_index db index |> PSet.rslice_seq |> PSet.to_seq in
    let duplicates = duplicate_index_datoms db index |> List.rev |> List.to_seq in
    merge_sorted_datom_seqs
      (fun left right -> Util.compare_datom index right left)
      indexed
      duplicates

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

let exact_prefix_datoms context db index e a v tx =
  match exact_prefix_bound index e a v tx with
  | None -> None
  | Some (bound, bound_fields) ->
    (match index, e, a, v, tx with
     | (Aevt | Avet), None, Some attr, None, None when db.duplicate_datoms <> [] ->
       let indexed = primary_attr_datoms db index attr in
       let duplicates = duplicate_attr_datoms db index attr in
       Some (merge_sorted_datom_seqs (Util.compare_datom index) (List.to_seq indexed) (List.to_seq duplicates))
     | _ ->
       let cmp = exact_prefix_slice_cmp context index bound bound_fields in
       (match index, a, db.duplicate_datoms with
        | (Aevt | Avet), Some attr, _ :: _ ->
          let indexed = primary_attr_datoms db index attr |> exact_sorted_slice cmp bound in
          let duplicates = duplicate_prefix_datoms db index e a |> exact_sorted_slice cmp bound in
          Some (merge_sorted_datom_seqs (Util.compare_datom index) (List.to_seq indexed) (List.to_seq duplicates))
        | _ ->
          (match db.duplicate_datoms with
           | [] -> Some (PSet.slice_seq ~from_:bound ~to_:bound ~cmp (stored_index db index) |> PSet.to_seq)
           | _ ->
             let indexed = PSet.slice_seq ~from_:bound ~to_:bound ~cmp (stored_index db index) |> PSet.to_seq in
             let duplicates = duplicate_prefix_datoms db index e a |> exact_sorted_slice cmp bound in
             Some (merge_sorted_datom_seqs (Util.compare_datom index) indexed (List.to_seq duplicates)))))

let exact_prefix_datoms_list context db index e a v tx =
  match exact_prefix_bound index e a v tx with
  | None -> None
  | Some (bound, bound_fields) ->
    let cmp = exact_prefix_slice_cmp context index bound bound_fields in
    (match db.duplicate_datoms with
     | [] ->
       Some
         (PSet.slice_seq ~from_:bound ~to_:bound ~cmp (stored_index db index)
          |> PSet.seq_to_list)
     | _ ->
       exact_prefix_datoms context db index e a v tx
       |> Option.map List.of_seq)

let lower_prefix_datoms context db index e a v tx =
  match exact_prefix_bound index e a v tx with
  | None -> None
  | Some (bound, bound_fields) ->
    let cmp = slice_cmp context index bound bound_fields bound bound_fields in
    let indexed =
      match index, e, a, v, tx with
      | (Aevt | Avet), None, Some attr, None, None when db.duplicate_datoms <> [] ->
        primary_attr_datoms db index attr
        |> List.filter (fun datom -> cmp datom bound >= 0)
        |> List.to_seq
      | _ -> PSet.slice_seq ~from_:bound ~cmp (stored_index db index) |> PSet.to_seq
    in
    (match db.duplicate_datoms with
     | [] -> Some indexed
     | _ ->
       let duplicates = duplicate_prefix_datoms db index e a |> List.filter (fun datom -> cmp datom bound >= 0) in
       Some (merge_sorted_datom_seqs (Util.compare_datom index) indexed (List.to_seq duplicates)))

let reverse_upper_prefix_datoms context db index e a v tx =
  match exact_prefix_bound index e a v tx with
  | None -> None
  | Some (bound, bound_fields) ->
    let cmp = slice_cmp context index bound bound_fields bound bound_fields in
    let indexed =
      match index, e, a, v, tx with
      | (Aevt | Avet), None, Some attr, None, None when db.duplicate_datoms <> [] ->
        primary_attr_datoms db index attr
        |> List.filter (fun datom -> cmp datom bound <= 0)
        |> List.rev
        |> List.to_seq
      | _ -> PSet.rslice_seq ~from_:bound ~cmp (stored_index db index) |> PSet.to_seq
    in
    (match db.duplicate_datoms with
     | [] -> Some indexed
     | _ ->
       let duplicates = duplicate_prefix_datoms db index e a |> List.filter (fun datom -> cmp datom bound <= 0) |> List.rev in
       Some
         (merge_sorted_datom_seqs
            (fun left right -> Util.compare_datom index right left)
            indexed
            (List.to_seq duplicates)))

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
  let indexed =
    match db.duplicate_datoms with
    | [] ->
      let cmp = slice_cmp context Avet from_bound from_fields to_bound to_fields in
      PSet.slice_seq ~from_:from_bound ~to_:to_bound ~cmp db.avet_index |> PSet.to_seq
    | _ ->
      primary_attr_datoms db Avet attr
      |> List.filter (fun datom -> lower_matches datom && upper_matches datom)
      |> List.to_seq
  in
  match db.duplicate_datoms with
  | [] -> indexed
  | _ ->
    let duplicates =
      duplicate_attr_datoms db Avet attr
      |> List.filter (fun datom -> lower_matches datom && upper_matches datom)
    in
    merge_sorted_datom_seqs (Util.compare_datom Avet) indexed (List.to_seq duplicates)

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
    | _ -> false
  in
  let datoms =
    if exact_attr_prefix || (e, a, v, tx) = (None, None, None, None) then
      datoms
    else
      datoms
      |> Seq.filter (fun d -> matches e d.e && matches a d.a && matches_value context v d.v && matches tx d.tx)
  in
  apply_filter_pred db datoms

let fold_datoms f init context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
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
  match db.duplicate_datoms, exact_prefix_bound index e a prefix_v prefix_tx with
  | [], Some (bound, bound_fields) ->
    let cmp = exact_prefix_slice_cmp context index bound bound_fields in
    let seq = PSet.slice_seq ~from_:bound ~to_:bound ~cmp (stored_index db index) in
    let fold =
      match exact_attr_prefix || (e, a, v, tx) = (None, None, None, None), db.filter_pred with
      | true, None -> f
      | true, Some _ -> fold_filter_pred
      | false, None -> fold_filter
      | false, Some _ -> fold_filter_and_pred
    in
    PSet.fold_seq fold init seq
  | [], None when (e, a, v, tx) = (None, None, None, None) ->
    (match db.filter_pred with
     | None -> PSet.fold f init (stored_index db index)
     | Some pred ->
       PSet.fold (fun acc datom -> if pred datom then f acc datom else acc) init (stored_index db index))
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
  apply_filter_pred_list db datoms

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
  | Some datoms -> apply_filter_pred db datoms
  | None ->
    datoms context db index ()
    |> Seq.filter (fun d -> compare_datom_to_bound context index d e a v tx >= 0)

let seek_datoms_ref context db index ?e ?a ?v ?tx () =
  let e = resolved_entity_ref_option context db e in
  seek_datoms context db index ?e ?a ?v ?tx ()

let rseek_datoms context db index ?e ?a ?v ?tx () =
  validate_index_access context db index a;
  let v = resolved_value_option_for_optional_attr context db a v in
  match reverse_upper_prefix_datoms context db index e a v tx with
  | Some datoms -> apply_filter_pred db datoms
  | None ->
    reverse_index_datoms_seq db index
    |> Seq.filter (fun d -> compare_datom_to_bound context index d e a v tx <= 0)
    |> apply_filter_pred db

let rseek_datoms_ref context db index ?e ?a ?v ?tx () =
  let e = resolved_entity_ref_option context db e in
  rseek_datoms context db index ?e ?a ?v ?tx ()

let index_range context db attr ?start ?stop () =
  if not (context.is_avet_accessible db attr) then
    invalid_arg (indexed_attr_required_message attr);
  let start = Option.map (context.resolve_value_for_attr db attr) start in
  let stop = Option.map (context.resolve_value_for_attr db attr) stop in
  avet_range_datoms context db attr start stop
  |> apply_filter_pred db

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
    | None -> Unix.gettimeofday ()
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
