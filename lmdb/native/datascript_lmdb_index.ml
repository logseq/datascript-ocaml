open Datascript_types

type t = { db : Datascript_lmdb_db.t; which : index }

type 'a seq = { cmp : datom -> datom -> int; datoms : datom list; offset : int }

exception Stop_search

let db_of t = t.db
let make index db = { db; which = index }
let cmp_for index = Datascript_types.Compare.compare_datom index

let datom_key t datom = Datascript_lmdb_codec.encode_datom_key t.which datom

let decode_entry index key value =
  let datom = Datascript_lmdb_codec.decode_datom_key index key in
  let payload = Datascript_lmdb_codec.decode_datom_value value in
  { datom with added = payload.added; v = payload.v }

let put_datom_txn txn t datom =
  let key = datom_key t datom in
  let value = Datascript_lmdb_codec.encode_datom_value datom in
  Datascript_lmdb_db.put_index_txn t.which txn t.db key value

let empty index db = make index db

let write_datoms t datoms =
  if datoms = [] then t
  else (
    Datascript_lmdb_db.with_write_txn t.db (fun txn -> List.iter (put_datom_txn txn t) datoms);
    t)

let of_sorted_list index datoms db = write_datoms (empty index db) datoms

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

let of_bulk index datoms db = of_sorted_list index datoms db

let append_tx_data ~avet:is_avet datoms eavt aevt avet_index =
  if datoms = [] then (eavt, aevt, avet_index)
  else (
    Datascript_lmdb_db.with_write_txn eavt.db (fun txn ->
      List.iter
        (fun datom ->
          put_datom_txn txn eavt datom;
          put_datom_txn txn aevt datom;
          if is_avet datom.a then put_datom_txn txn avet_index datom)
        datoms);
    (eavt, aevt, avet_index))

let append_datoms datoms t = write_datoms t datoms

let add datom t = write_datoms t [ datom ]

let remove _datom t = t

let bound_key t = function
  | None -> None
  | Some datom -> Some (datom_key t datom)

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

let fold_stored t f acc =
  let acc = ref acc in
  Datascript_lmdb_db.fold_index t.which t.db (fun key value ->
    acc := f !acc (decode_entry t.which key value));
  !acc

let fold_stored_prefix t attr f acc =
  let prefix = attr ^ "\000" in
  let acc = ref acc in
  Datascript_lmdb_db.fold_index_prefix t.which t.db prefix (fun key value ->
    acc := f !acc (decode_entry t.which key value));
  !acc

let fold_stored_attr_value_prefix t attr value f acc =
  let prefix = Datascript_lmdb_codec.encode_index_attr_value_prefix t.which attr value in
  let acc = ref acc in
  Datascript_lmdb_db.fold_index_prefix t.which t.db prefix (fun key value ->
    acc := f !acc (decode_entry t.which key value));
  !acc

let fold_stored_bounded t ?from_ ?to_ cmp f acc =
  match bound_key t from_ with
  | None -> fold_stored t f acc
  | Some from_key ->
    let acc = ref acc in
    Datascript_lmdb_db.fold_index_range_until t.which t.db ~from_key
      ~stop:(fun _key _value ->
        match to_ with
        | Some bound ->
          let datom = decode_entry t.which _key _value in
          cmp datom bound > 0
        | None -> false)
      (fun key value ->
        let datom = decode_entry t.which key value in
        if in_range cmp from_ to_ datom then acc := f !acc datom);
    !acc

let clear_index_txn txn index lmdb =
  Datascript_lmdb_db.fold_index index lmdb (fun key _ ->
    Datascript_lmdb_db.remove_index_txn index txn lmdb key)

let sync_merged_to_lmdb t target_lmdb =
  if t.db == target_lmdb then ()
  else
    Datascript_lmdb_db.with_write_txn target_lmdb (fun txn ->
      clear_index_txn txn t.which target_lmdb;
      Datascript_lmdb_db.copy_index_txn t.which txn t.db target_lmdb)

let copy t = t

let flush t = t

let to_list t = List.rev (fold_stored t (fun acc datom -> datom :: acc) [])

let fold f init t = fold_stored t f init

let lookup t datom =
  match Datascript_lmdb_db.get_index t.which t.db (datom_key t datom) with
  | None -> None
  | Some value -> Some (decode_entry t.which (datom_key t datom) value)

let fold_slice f init ?from_ ?to_ ?cmp t =
  let cmp = Option.value ~default:(cmp_for t.which) cmp in
  let apply acc datom = if in_range cmp from_ to_ datom then f acc datom else acc in
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
  fold_stored_prefix t attr (fun acc datom -> if datom.a = attr then f acc datom else acc) init

let materialize_range t ?from_ ?to_ cmp =
  fold_slice (fun acc datom -> datom :: acc) [] ?from_ ?to_ ~cmp t |> List.rev

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
