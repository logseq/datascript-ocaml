open Datascript_types

type t =
  { db : Datascript_lmdb_db.t
  ; which : index
  ; additions : datom list
  ; additions_arr : datom array option
  ; removals : datom list
  ; bulk : bool
  }

type 'a seq = { cmp : datom -> datom -> int; datoms : datom list; offset : int }

exception Stop_search

let db_of t = t.db
let make index db = { db; which = index; additions = []; additions_arr = None; removals = []; bulk = false }
let cmp_for index = Datascript_types.Compare.compare_datom index
let overlay_empty t = t.additions = [] && t.removals = []

let datom_key t datom = Datascript_lmdb_codec.encode_datom_key t.which datom

let decode_entry index key value =
  let datom = Datascript_lmdb_codec.decode_datom_key index key in
  let payload = Datascript_lmdb_codec.decode_datom_value value in
  { datom with added = payload.added; v = payload.v }

let put_datom_txn txn t datom =
  let key = datom_key t datom in
  let value = Datascript_lmdb_codec.encode_datom_value datom in
  Datascript_lmdb_db.put_index_txn t.which txn t.db key value

let remove_datom_txn txn t datom =
  let key = datom_key t datom in
  Datascript_lmdb_db.remove_index_txn t.which txn t.db key

let empty index db = make index db

let of_sorted_list index datoms db =
  let t = empty index db in
  if datoms = [] then t
  else (
    Datascript_lmdb_db.with_write_txn db (fun txn ->
      List.iter (put_datom_txn txn t) datoms);
    t)

let of_sorted_lists index_datoms db =
  Datascript_lmdb_db.with_write_txn db (fun txn ->
    List.iter
      (fun (index, datoms) ->
        let t = make index db in
        List.iter (put_datom_txn txn t) datoms)
      index_datoms)

let of_eavt_datoms ~avet eavt_datoms db =
  if eavt_datoms = [] then ()
  else (
    let eavt = make Eavt db in
    let aevt = make Aevt db in
    let avet_index = make Avet db in
    Datascript_lmdb_db.with_write_txn db (fun txn ->
      List.iter
        (fun datom ->
          put_datom_txn txn eavt datom;
          put_datom_txn txn aevt datom;
          if avet datom.a then put_datom_txn txn avet_index datom)
        eavt_datoms))

let of_bulk index datoms db =
  { db; which = index; additions = []; additions_arr = Some (Array.of_list datoms); removals = []; bulk = true }

let array_find_first cmp bound arr =
  let len = Array.length arr in
  let rec lower lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if cmp arr.(mid) bound < 0 then lower (mid + 1) hi else lower lo mid
  in
  let index = lower 0 len in
  if index < len && cmp arr.(index) bound = 0 then Some arr.(index) else None

let array_lower_bound cmp bound arr =
  let len = Array.length arr in
  let rec lower lo hi =
    if lo >= hi then lo
    else
      let mid = (lo + hi) / 2 in
      if cmp arr.(mid) bound < 0 then lower (mid + 1) hi else lower lo mid
  in
  lower 0 len

let sorted_bulk_array t =
  match t.additions_arr with
  | Some arr -> arr
  | None ->
    let arr = Array.of_list t.additions in
    Array.sort (cmp_for t.which) arr;
    arr

let bulk_datoms t =
  let base = Array.to_list (sorted_bulk_array t) in
  match t.additions with
  | [] -> base
  | overlay -> List.merge (cmp_for t.which) (List.sort (cmp_for t.which) overlay) base

let in_range cmp lower upper datom =
  let above_lower =
    match lower with
    | None -> true
    | Some lower -> cmp datom lower >= 0
  in
  let below_upper =
    match upper with
    | None -> true
    | Some upper -> cmp datom upper <= 0
  in
  above_lower && below_upper

let array_fold_in_range cmp from_ to_ arr f init =
  let len = Array.length arr in
  let start =
    match from_ with
    | None -> 0
    | Some bound -> array_lower_bound cmp bound arr
  in
  let rec loop index acc =
    if index >= len then acc
    else
      let datom = arr.(index) in
      if not (in_range cmp from_ to_ datom) then acc
      else loop (index + 1) (f acc datom)
  in
  loop start init

let array_materialize_range cmp from_ to_ arr =
  array_fold_in_range cmp from_ to_ arr (fun acc datom -> datom :: acc) [] |> List.rev

let array_fold_attr_prefix f init attr arr index =
  let cmp = cmp_for index in
  let bound = { e = 0; a = attr; v = Nil; tx = 0; added = true } in
  let start = array_lower_bound cmp bound arr in
  let len = Array.length arr in
  let rec loop i acc =
    if i >= len then acc
    else
      let datom = arr.(i) in
      if datom.a <> attr then acc else loop (i + 1) (f acc datom)
  in
  loop start init

let values_equal left right =
  match left, right with
  | String left, String right
  | Symbol left, Symbol right
  | Keyword left, Keyword right
  | Uuid left, Uuid right
  | Regex left, Regex right ->
    left = right
  | Bool left, Bool right -> left = right
  | Int left, Int right
  | Ref left, Ref right
  | Int left, Ref right
  | Ref left, Int right ->
    left = right
  | Instant left, Instant right -> left = right
  | Nil, Nil -> true
  | TxRef, TxRef -> true
  | _ -> Compare.compare_value left right = 0

let array_fold_attr_value_prefix f init attr value arr index =
  let cmp = cmp_for index in
  let bound = { e = 0; a = attr; v = value; tx = 0; added = true } in
  let start = array_lower_bound cmp bound arr in
  let len = Array.length arr in
  let rec loop i acc =
    if i >= len then acc
    else
      let datom = arr.(i) in
      if datom.a <> attr || not (values_equal datom.v value) then acc
      else loop (i + 1) (f acc datom)
  in
  loop start init

let additions_only t =
  t.bulk && t.removals = [] && (t.additions <> [] || Option.is_some t.additions_arr)

let add datom t =
  if additions_only t then
    { t with additions = datom :: t.additions }
  else (
    let key = datom_key t datom in
    let additions = datom :: List.filter (fun d -> datom_key t d <> key) t.additions in
    let removals = List.filter (fun d -> datom_key t d <> key) t.removals in
    { t with additions; removals })

let remove datom t =
  let key = datom_key t datom in
  let stored_additions =
    match t.additions_arr with
    | Some arr -> Array.to_list arr
    | None -> t.additions
  in
  let additions = List.filter (fun d -> datom_key t d <> key) stored_additions in
  let already_removed = List.exists (fun d -> datom_key t d = key) t.removals in
  let removals =
    if already_removed || List.exists (fun d -> datom_key t d = key) stored_additions then t.removals
    else datom :: t.removals
  in
  { t with additions; additions_arr = None; removals }

let overlay_tables t =
  let stored_additions =
    match t.additions_arr with
    | Some arr -> Array.to_list arr
    | None -> t.additions
  in
  let removed = Hashtbl.create (List.length t.removals) in
  List.iter (fun datom -> Hashtbl.add removed (datom_key t datom) ()) t.removals;
  let added = Hashtbl.create (List.length stored_additions) in
  List.iter (fun datom -> Hashtbl.replace added (datom_key t datom) datom) stored_additions;
  removed, added

let stored_visible key removed added =
  not (Hashtbl.mem removed key || Hashtbl.mem added key)

let bound_key t = function
  | None -> None
  | Some datom -> Some (datom_key t datom)

let fold_stored t f acc =
  if overlay_empty t then
    let acc = ref acc in
    Datascript_lmdb_db.fold_index t.which t.db (fun key value ->
      acc := f !acc (decode_entry t.which key value));
    !acc
  else
    let removed, added = overlay_tables t in
    let acc = ref acc in
    Datascript_lmdb_db.fold_index t.which t.db (fun key value ->
      if stored_visible key removed added then
        acc := f !acc (decode_entry t.which key value));
    !acc

let fold_stored_prefix t attr f acc =
  let prefix = attr ^ "\000" in
  if overlay_empty t then
    let acc = ref acc in
    Datascript_lmdb_db.fold_index_prefix t.which t.db prefix (fun key value ->
      acc := f !acc (decode_entry t.which key value));
    !acc
  else
    let removed, added = overlay_tables t in
    let acc = ref acc in
    Datascript_lmdb_db.fold_index_prefix t.which t.db prefix (fun key value ->
      if stored_visible key removed added then
        acc := f !acc (decode_entry t.which key value));
    !acc

let fold_stored_attr_value_prefix t attr value f acc =
  let prefix = Datascript_lmdb_codec.encode_index_attr_value_prefix t.which attr value in
  if overlay_empty t then
    let acc = ref acc in
    Datascript_lmdb_db.fold_index_prefix t.which t.db prefix (fun key value ->
      acc := f !acc (decode_entry t.which key value));
    !acc
  else
    let removed, added = overlay_tables t in
    let acc = ref acc in
    Datascript_lmdb_db.fold_index_prefix t.which t.db prefix (fun key value ->
      if stored_visible key removed added then
        acc := f !acc (decode_entry t.which key value));
    !acc

let fold_stored_bounded t ?from_ ?to_ cmp f acc =
  match bound_key t from_ with
  | None -> fold_stored t f acc
  | Some from_key ->
    let removed, added =
      if overlay_empty t then (Hashtbl.create 0, Hashtbl.create 0) else overlay_tables t
    in
    let acc = ref acc in
    Datascript_lmdb_db.fold_index_range_until t.which t.db ~from_key
      ~stop:(fun key value ->
        if not (stored_visible key removed added) then false
        else
          match to_ with
          | Some bound ->
            let datom = decode_entry t.which key value in
            cmp datom bound > 0
          | None -> false)
      (fun key value ->
        if stored_visible key removed added then
          let datom = decode_entry t.which key value in
          if in_range cmp from_ to_ datom then acc := f !acc datom);
    !acc

let fold_overlay t f acc = List.fold_left f acc t.additions

let fold_bulk_slice f init ?from_ ?to_ ?cmp t =
  let cmp = Option.value ~default:(cmp_for t.which) cmp in
  let apply acc datom = if in_range cmp from_ to_ datom then f acc datom else acc in
  let arr = sorted_bulk_array t in
  let acc =
    match from_, to_ with
    | Some bound, Some bound' when bound == bound' && bound.a <> "" && bound.v <> Nil && bound.e = 0 ->
      array_fold_attr_value_prefix f init bound.a bound.v arr t.which
    | _ -> array_fold_in_range cmp from_ to_ arr f init
  in
  List.fold_left apply acc t.additions

let fold_datoms f init t =
  if additions_only t then
    let acc = Array.fold_left (fun acc datom -> f acc datom) init (sorted_bulk_array t) in
    List.fold_left f acc t.additions
  else (
    let acc = fold_stored t f init in
    fold_overlay t f acc)

let collect_datoms t =
  if overlay_empty t then fold_stored t (fun acc datom -> datom :: acc) []
  else fold_datoms (fun acc datom -> datom :: acc) [] t |> List.sort (cmp_for t.which)

let clear_index_txn txn index lmdb =
  Datascript_lmdb_db.fold_index index lmdb (fun key _ ->
    Datascript_lmdb_db.remove_index_txn index txn lmdb key)

let sync_merged_to_lmdb t target_lmdb =
  let write_datom_txn txn datom =
    let key = datom_key t datom in
    let value = Datascript_lmdb_codec.encode_datom_value datom in
    Datascript_lmdb_db.put_index_txn t.which txn target_lmdb key value
  in
  if additions_only t then
    Datascript_lmdb_db.with_write_txn target_lmdb (fun txn ->
      Array.iter (write_datom_txn txn) (sorted_bulk_array t);
      List.iter (write_datom_txn txn) t.additions)
  else if overlay_empty t then
    Datascript_lmdb_db.with_write_txn target_lmdb (fun txn ->
      clear_index_txn txn t.which target_lmdb;
      Datascript_lmdb_db.copy_index_txn t.which txn t.db target_lmdb)
  else
    let merged = collect_datoms t in
    Datascript_lmdb_db.with_write_txn target_lmdb (fun txn ->
      clear_index_txn txn t.which target_lmdb;
      List.iter (write_datom_txn txn) merged)

let copy_list xs = List.map (fun x -> x) xs

let copy t = { t with additions = copy_list t.additions; removals = copy_list t.removals }

let flush t =
  if overlay_empty t then t
  else (
    Datascript_lmdb_db.with_write_txn t.db (fun txn ->
      List.iter (remove_datom_txn txn t) t.removals;
      List.iter (put_datom_txn txn t) t.additions);
    { t with additions = []; additions_arr = None; removals = [] })

let to_list t =
  if additions_only t then bulk_datoms t
  else if overlay_empty t then List.rev (fold_stored t (fun acc datom -> datom :: acc) [])
  else collect_datoms t

let fold f init t = fold_datoms f init t

let lookup t datom =
  let key = datom_key t datom in
  if List.exists (fun d -> datom_key t d = key) t.removals then None
  else
    (match List.find_opt (fun d -> datom_key t d = key) t.additions with
     | Some datom -> Some datom
     | None -> (
       match t.additions_arr with
       | Some arr ->
         let cmp = cmp_for t.which in
         (match array_find_first cmp datom arr with
          | Some found when datom_key t found = key -> Some found
          | _ -> None)
       | None -> (
         match Datascript_lmdb_db.get_index t.which t.db key with
         | None -> None
         | Some value -> Some (decode_entry t.which key value))))

let fold_slice f init ?from_ ?to_ ?cmp t =
  if additions_only t then fold_bulk_slice f init ?from_ ?to_ ?cmp t
  else
    let cmp = Option.value ~default:(cmp_for t.which) cmp in
    let apply acc datom = if in_range cmp from_ to_ datom then f acc datom else acc in
    if not (overlay_empty t) then
      collect_datoms t
      |> List.filter (fun datom -> in_range cmp from_ to_ datom)
      |> List.fold_left f init
    else
      match from_, to_ with
    | Some bound, Some bound' when bound == bound' && bound.a <> "" && bound.e = 0 && bound.v = Nil
      && (t.which = Aevt || t.which = Avet) ->
      fold_stored_prefix t bound.a apply init
    | Some bound, Some bound' when bound == bound' && bound.a <> "" && bound.v <> Nil && bound.e = 0 ->
      fold_stored_attr_value_prefix t bound.a bound.v apply init
    | _ -> fold_stored_bounded t ?from_ ?to_ cmp apply init

let find_first_slice ?from_ ?to_ ?cmp t =
  let cmp = Option.value ~default:(cmp_for t.which) cmp in
  let found = ref None in
  let consider datom =
    if !found = None && in_range cmp from_ to_ datom then (
      found := Some datom;
      raise Stop_search)
  in
  (try
     if additions_only t then (
       List.iter consider t.additions;
       match from_, to_ with
       | Some bound, Some bound' when bound == bound' && bound.a <> "" && bound.v <> Nil && bound.e = 0 ->
         ignore
           (array_fold_attr_value_prefix
              (fun () datom -> consider datom)
              ()
              bound.a
              bound.v
              (sorted_bulk_array t)
              t.which)
       | Some bound, Some bound' when bound == bound' -> (
         match array_find_first cmp bound (sorted_bulk_array t) with
         | Some datom when !found = None && in_range cmp from_ to_ datom ->
           found := Some datom;
           raise Stop_search
         | _ -> ())
       | _ ->
         if !found = None then
           ignore (array_fold_in_range cmp from_ to_ (sorted_bulk_array t) (fun () datom -> consider datom) ()))
     else if not (overlay_empty t) then
       collect_datoms t |> List.iter consider
     else
       match from_, to_ with
       | Some bound, Some bound' when bound == bound' && bound.a <> "" && bound.e = 0 && bound.v = Nil
         && (t.which = Aevt || t.which = Avet) ->
         fold_stored_prefix t bound.a (fun () datom -> consider datom) ()
       | Some bound, Some bound' when bound == bound' && bound.a <> "" && bound.v <> Nil && bound.e = 0 ->
         fold_stored_attr_value_prefix t bound.a bound.v (fun () datom -> consider datom) ()
       | _ -> fold_stored_bounded t ?from_ ?to_ cmp (fun () datom -> consider datom) ()
   with Stop_search -> ());
  !found

let fold_attr_prefix f init t attr =
  let apply acc datom = if datom.a = attr then f acc datom else acc in
  if additions_only t then
    let arr = sorted_bulk_array t in
    let acc = array_fold_attr_prefix f init attr arr t.which in
    List.fold_left apply acc t.additions
  else if not (overlay_empty t) then
    collect_datoms t
    |> List.filter (fun datom -> datom.a = attr)
    |> List.fold_left f init
  else
    fold_stored_prefix t attr apply init

let materialize_range t ?from_ ?to_ cmp =
  if additions_only t then array_materialize_range cmp from_ to_ (sorted_bulk_array t)
  else fold_slice (fun acc datom -> datom :: acc) [] ?from_ ?to_ ~cmp t |> List.rev

let make_seq cmp datoms = { cmp; datoms; offset = 0 }

let to_seq ({ cmp = _; datoms; offset = start }) =
  let rec loop index () =
    if index >= List.length datoms then Seq.Nil
    else Seq.Cons (List.nth datoms index, loop (index + 1))
  in
  loop start

let seq t = make_seq (cmp_for t.which) (to_list t)

let slice_seq ?from_ ?to_ ?cmp t =
  let cmp = Option.value ~default:(cmp_for t.which) cmp in
  make_seq cmp (materialize_range t ?from_ ?to_ cmp)

let rslice_seq ?from_ ?to_ ?cmp t =
  let cmp = Option.value ~default:(cmp_for t.which) cmp in
  let datoms =
    to_list t
    |> List.filter (fun datom ->
      match from_ with
      | None -> true
      | Some bound -> cmp datom bound <= 0)
    |> List.filter (fun datom ->
      match to_ with
      | None -> true
      | Some bound -> cmp datom bound >= 0)
    |> List.rev
  in
  make_seq cmp datoms

let seq_to_list seq = to_seq seq |> List.of_seq

let fold_seq f init { cmp = _; datoms; offset } =
  let rec loop index acc =
    if index >= List.length datoms then acc
    else loop (index + 1) (f acc (List.nth datoms index))
  in
  loop offset init

let slice ?from_ ?to_ ?cmp t = slice_seq ?from_ ?to_ ?cmp t |> seq_to_list

let seek bound seq =
  let rec count index =
    if index >= List.length seq.datoms then index
    else if seq.cmp (List.nth seq.datoms index) bound >= 0 then index
    else count (index + 1)
  in
  { seq with offset = count 0 }
