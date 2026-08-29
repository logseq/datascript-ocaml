open Datascript_types

type t = { db : Datascript_sqlite_db.t; which : index }

(* Lazy stream seq — matches upstream BTSet iteration (no full-range materialize). *)
type 'a seq =
  { cmp : datom -> datom -> int
  ; stream : datom Seq.t
  ; seek_bound : datom option
  }

exception Stop_search

let db_of t = t.db
let make index db = { db; which = index }
let cmp_for index = Datascript_types.Compare.compare_datom index

let datom_key t datom = Datascript_index_codec.encode_datom_key t.which datom

let decode_entry index key value = Datascript_index_codec.decode_index_entry index key value

let put_datom_txn t datom =
  let key = datom_key t datom in
  let value = Datascript_index_codec.encode_index_value t.which datom in
  Datascript_sqlite_db.put_index_txn t.which t.db key value

let empty index db = make index db

let write_datoms t datoms =
  if datoms = [] then t
  else (
    Datascript_sqlite_db.with_write_txn t.db (fun () -> List.iter (put_datom_txn t) datoms);
    t)

let of_sorted_list index datoms db = write_datoms (empty index db) datoms

let of_sorted_lists index_datoms db =
  Datascript_sqlite_db.with_write_txn db (fun () ->
    List.iter
      (fun (index, datoms) ->
        let t = make index db in
        List.iter (put_datom_txn t) datoms)
      index_datoms)

let of_eavt_datoms ~avet eavt_datoms db =
  if eavt_datoms = [] then ()
  else (
    let eavt = make Eavt db in
    let aevt = make Aevt db in
    let avet_index = make Avet db in
    let tave = make Tave db in
    Datascript_sqlite_db.with_write_txn db (fun () ->
      List.iter
        (fun datom ->
          put_datom_txn eavt datom;
          put_datom_txn aevt datom;
          put_datom_txn tave datom;
          if avet datom.a then put_datom_txn avet_index datom)
        eavt_datoms))

let of_bulk index datoms db = of_sorted_list index datoms db

let append_tx_data ~avet:is_avet datoms eavt aevt avet_index =
  if datoms = [] then (eavt, aevt, avet_index)
  else (
    let tave = make Tave eavt.db in
    Datascript_sqlite_db.with_write_txn eavt.db (fun () ->
      List.iter
        (fun datom ->
          put_datom_txn eavt datom;
          put_datom_txn aevt datom;
          put_datom_txn tave datom;
          if is_avet datom.a then put_datom_txn avet_index datom)
        datoms);
    (eavt, aevt, avet_index))

let append_datoms datoms t = write_datoms t datoms

let add datom t = write_datoms t [ datom ]

let remove_datom_txn t datom =
  let key = datom_key t datom in
  Datascript_sqlite_db.remove_index_txn t.which t.db key

let remove datom t =
  Datascript_sqlite_db.with_write_txn t.db (fun () -> remove_datom_txn t datom);
  t

let remove_datoms datoms t =
  if datoms = [] then t
  else (
    Datascript_sqlite_db.with_write_txn t.db (fun () -> List.iter (remove_datom_txn t) datoms);
    t)

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

let same_prefix_bound left right =
  left.e = right.e && left.a = right.a && left.v = right.v

let is_attr_only_prefix_bound bound =
  bound.a <> "" && bound.e = 0 && bound.v = Nil

let attr_exact_prefix from_ to_ index =
  match from_, to_ with
  | Some from, Some to_
    when same_prefix_bound from to_
         && is_attr_only_prefix_bound from
         && (index = Aevt || index = Avet) ->
      Some from.a
  | _ -> None

let attr_value_exact_prefix from_ to_ =
  match from_, to_ with
  | Some from, Some to_
    when same_prefix_bound from to_
         && from.a <> "" && from.e = 0 && from.v <> Nil ->
      Some (from.a, from.v)
  | _ -> None

let fold_stored t f acc =
  let acc = ref acc in
  Datascript_sqlite_db.fold_index t.which t.db (fun key value ->
    acc := f !acc (decode_entry t.which key value));
  !acc

let fold_stored_prefix t attr f acc =
  let prefix = attr ^ "\000" in
  let acc = ref acc in
  Datascript_sqlite_db.fold_index_prefix t.which t.db prefix (fun key value ->
    let datom =
      match t.which with
      | Avet -> Datascript_index_codec.decode_avet_key_at attr key
      | _ -> decode_entry t.which key value
    in
    acc := f !acc datom);
  !acc

let fold_attr_exact_prefix f init t attr =
  fold_stored_prefix t attr (fun acc datom -> if datom.a = attr then f acc datom else acc) init

let fold_stored_attr_value_prefix t attr value f acc =
  let prefix = Datascript_index_codec.encode_index_attr_value_prefix t.which attr value in
  let acc = ref acc in
  Datascript_sqlite_db.fold_index_prefix t.which t.db prefix (fun key value ->
    let datom =
      match t.which with
      | Avet -> Datascript_index_codec.decode_avet_key_at attr key
      | _ -> decode_entry t.which key value
    in
    acc := f !acc datom);
  !acc

let avet_attr_prefix attr =
  let buffer = Buffer.create (String.length attr + 1) in
  Buffer.add_string buffer attr;
  Buffer.add_char buffer '\000';
  Buffer.contents buffer

let fold_stored_avet_value_range t attr ?start_value ?stop_value _compare_value f acc =
  let from_key =
    match start_value with
    | Some value -> Datascript_index_codec.encode_index_attr_value_prefix Avet attr value
    | None -> avet_attr_prefix attr
  in
  let acc = ref acc in
  Datascript_sqlite_db.fold_index_range_until Avet t.db ~from_key
    ~stop:(fun key _value ->
      if Datascript_index_codec.avet_key_attr key <> attr then
        true
      else
        match stop_value with
        | None -> false
        | Some stop ->
            Datascript_types.Compare.compare_value (Datascript_index_codec.avet_key_value key) stop > 0)
    (fun key _value ->
      let datom = Datascript_index_codec.decode_avet_key_at attr key in
      match start_value with
      | None -> acc := f !acc datom
      | Some _ -> acc := f !acc datom);
  !acc

let fold_stored_bounded t ?from_ ?to_ cmp f acc =
  match bound_key t from_ with
  | None -> fold_stored t f acc
  | Some from_key ->
    let acc = ref acc in
    Datascript_sqlite_db.fold_index_range_until t.which t.db ~from_key
      ~stop:(fun key value ->
        match to_ with
        | Some bound ->
            let datom = decode_entry t.which key value in
            cmp datom bound > 0
        | None -> false)
      (fun key value ->
        let datom = decode_entry t.which key value in
        if in_range cmp from_ to_ datom then acc := f !acc datom);
    !acc

let avet_value_range_bounds from_ to_ =
  (* Require an upper bound: open-ended AVET seeks must continue across attrs. *)
  match from_, to_ with
  | Some from, Some to_ when from.a <> "" && from.e = 0 && to_.a = from.a && to_.e = 0 ->
      let start_value = if from.v = Nil then None else Some from.v in
      let stop_value = if to_.v = Nil then None else Some to_.v in
      Some (from.a, start_value, stop_value)
  | _ -> None

let sync_append_since_tx ~since_tx t target_sqlite =
  if t.db == target_sqlite then ()
  else
    let target = make t.which target_sqlite in
    Datascript_sqlite_db.with_write_txn target_sqlite (fun () ->
      fold_stored t (fun () datom ->
        if datom.tx > since_tx then put_datom_txn target datom)
      ())

let copy t = t

let flush t = t

let to_list t = List.rev (fold_stored t (fun acc datom -> datom :: acc) [])

let fold f init t = fold_stored t f init

let lookup t datom =
  match Datascript_sqlite_db.get_index t.which t.db (datom_key t datom) with
  | None -> None
  | Some value -> Some (decode_entry t.which (datom_key t datom) value)

let fold_slice f init ?from_ ?to_ ?cmp t =
  let cmp = Option.value ~default:(cmp_for t.which) cmp in
  let apply acc datom = if in_range cmp from_ to_ datom then f acc datom else acc in
  match t.which, avet_value_range_bounds from_ to_ with
  | Avet, Some (attr, start_value, stop_value) ->
      fold_stored_avet_value_range t attr ?start_value:start_value ?stop_value:stop_value
        Datascript_types.Compare.compare_value f init
  | _ -> (
    match attr_exact_prefix from_ to_ t.which with
    | Some attr -> fold_attr_exact_prefix f init t attr
    | None -> (
      match attr_value_exact_prefix from_ to_ with
      | Some (attr, value) -> fold_stored_attr_value_prefix t attr value f init
      | None -> fold_stored_bounded t ?from_ ?to_ cmp apply init))

let find_first_slice ?from_ ?to_ ?cmp t =
  let cmp = Option.value ~default:(cmp_for t.which) cmp in
  let found = ref None in
  let consider datom =
    if !found = None && in_range cmp from_ to_ datom then (
      found := Some datom;
      raise Stop_search)
  in
  (try
     match attr_exact_prefix from_ to_ t.which with
     | Some attr -> fold_attr_exact_prefix (fun () datom -> consider datom) () t attr
     | None -> (
       match attr_value_exact_prefix from_ to_ with
       | Some (attr, value) ->
           fold_stored_attr_value_prefix t attr value (fun () datom -> consider datom) ()
       | _ -> fold_stored_bounded t ?from_ ?to_ cmp (fun () datom -> consider datom) ())
   with Stop_search -> ());
  !found

let fold_attr_prefix f init t attr = fold_attr_exact_prefix f init t attr

let make_seq cmp stream = { cmp; stream; seek_bound = None }

let to_seq seq =
  match seq.seek_bound with
  | None -> seq.stream
  | Some bound -> Seq.drop_while (fun datom -> seq.cmp datom bound < 0) seq.stream

let map_kv_to_datoms t seq =
  Seq.map (fun (key, value) -> decode_entry t.which key value) seq

let stream_attr_exact_prefix t attr =
  let prefix = attr ^ "\000" in
  Datascript_sqlite_db.seq_index_prefix t.which t.db prefix ()
  |> map_kv_to_datoms t
  |> Seq.filter (fun datom -> datom.a = attr)

let stream_attr_value_exact_prefix t attr value =
  let prefix = Datascript_index_codec.encode_index_attr_value_prefix t.which attr value in
  Datascript_sqlite_db.seq_index_prefix t.which t.db prefix ()
  |> map_kv_to_datoms t

let stream_avet_value_range t attr ?start_value ?stop_value () =
  let from_key =
    match start_value with
    | Some value -> Datascript_index_codec.encode_index_attr_value_prefix Avet attr value
    | None -> avet_attr_prefix attr
  in
  Datascript_sqlite_db.seq_index_range_until Avet t.db ~from_key
    ~stop:(fun key _value ->
      if Datascript_index_codec.avet_key_attr key <> attr then true
      else
        match stop_value with
        | None -> false
        | Some stop ->
            Datascript_types.Compare.compare_value (Datascript_index_codec.avet_key_value key) stop
            > 0)
    ()
  |> Seq.map (fun (key, _value) -> Datascript_index_codec.decode_avet_key_at attr key)

let stream_bounded t ?from_ ?to_ cmp =
  match bound_key t from_ with
  | None ->
      Datascript_sqlite_db.seq_index_range_until t.which t.db ()
      |> map_kv_to_datoms t
      |> Seq.filter (fun datom -> in_range cmp from_ to_ datom)
  | Some from_key ->
      Datascript_sqlite_db.seq_index_range_until t.which t.db ~from_key
        ~stop:(fun key value ->
          match to_ with
          | Some bound ->
              let datom = decode_entry t.which key value in
              cmp datom bound > 0
          | None -> false)
        ()
      |> map_kv_to_datoms t
      |> Seq.filter (fun datom -> in_range cmp from_ to_ datom)

let stream_slice ?from_ ?to_ ~cmp t =
  match t.which, avet_value_range_bounds from_ to_ with
  | Avet, Some (attr, start_value, stop_value) ->
      stream_avet_value_range t attr ?start_value:start_value ?stop_value:stop_value ()
  | _ -> (
    match attr_exact_prefix from_ to_ t.which with
    | Some attr -> stream_attr_exact_prefix t attr
    | None -> (
      match attr_value_exact_prefix from_ to_ with
      | Some (attr, value) -> stream_attr_value_exact_prefix t attr value
      | None -> stream_bounded t ?from_ ?to_ cmp))

let seq t = make_seq (cmp_for t.which) (stream_bounded t (cmp_for t.which))

let slice_seq ?from_ ?to_ ?cmp t =
  let cmp = Option.value ~default:(cmp_for t.which) cmp in
  make_seq cmp (stream_slice ?from_ ?to_ ~cmp t)

let stream_rslice_desc t ~cmp ?from_ ?to_ () =
  (* from_ is the upper (hi) bound for rslice; to_ is the lower bound.
     Use datom-level bounds (not key<=hi_key alone): prefix bounds encode e=0 and
     would drop matching datoms under a strict key comparison. *)
  Datascript_sqlite_db.seq_index_range_desc_until t.which t.db
    ~stop:(fun key value ->
      let datom = decode_entry t.which key value in
      let under_hi =
        match from_ with
        | None -> true
        | Some bound -> cmp datom bound <= 0
      in
      under_hi
      &&
      match to_ with
      | Some bound -> cmp datom bound < 0
      | None -> false)
    ()
  |> map_kv_to_datoms t
  |> Seq.filter (fun datom -> in_range cmp to_ from_ datom)

let stream_attr_exact_prefix_desc t attr =
  (* Exclusive end of attr\0… keys is attr\001 when the separator is \000. *)
  let hi_key = attr ^ "\001" in
  Datascript_sqlite_db.seq_index_range_desc_until t.which t.db ~hi_key
    ~stop:(fun key _value ->
      let prefix = attr ^ "\000" in
      let prefix_len = String.length prefix in
      String.length key < prefix_len || String.sub key 0 prefix_len <> prefix)
    ()
  |> map_kv_to_datoms t
  |> Seq.filter (fun datom -> datom.a = attr)

let rslice_seq ?from_ ?to_ ?cmp t =
  let cmp = Option.value ~default:(cmp_for t.which) cmp in
  let stream =
    match attr_exact_prefix from_ to_ t.which with
    | Some attr -> stream_attr_exact_prefix_desc t attr
    | None -> stream_rslice_desc t ~cmp ?from_ ?to_ ()
  in
  make_seq cmp stream

let seq_to_list seq = to_seq seq |> List.of_seq

let fold_seq f init seq = Seq.fold_left f init (to_seq seq)

let slice ?from_ ?to_ ?cmp t = slice_seq ?from_ ?to_ ?cmp t |> seq_to_list

let seek bound seq = { seq with seek_bound = Some bound }

(** Fold TAVE keys with [tx > from_tx], optionally restricted to [attr]. *)
let fold_tave_range f init db ~from_tx ?to_tx ?attr () =
  let from_key = Datascript_index_codec.encode_tave_tx_prefix (from_tx + 1) in
  let acc = ref init in
  Datascript_sqlite_db.fold_index_range_until Tave db ~from_key
    ~stop:(fun key _value ->
      match to_tx with
      | Some hi -> Datascript_index_codec.tave_key_tx key > hi
      | None -> false)
    (fun key value ->
      let datom = decode_entry Tave key value in
      let attr_ok =
        match attr with
        | None -> true
        | Some a -> datom.a = a
      in
      let tx_ok =
        datom.tx > from_tx
        && (match to_tx with None -> true | Some hi -> datom.tx <= hi)
      in
      if attr_ok && tx_ok then acc := f !acc datom);
  !acc

(** Delete TAVE keys with [tx <= before_tx] (rolling retention). *)
let prune_tave_before db ~before_tx =
  if before_tx < 0 then ()
  else
    let to_delete = ref [] in
    let stop_key = Datascript_index_codec.encode_tave_tx_prefix (before_tx + 1) in
    Datascript_sqlite_db.fold_index_range_until Tave db
      ~stop:(fun key _ -> key >= stop_key)
      (fun key _value ->
        if Datascript_index_codec.tave_key_tx key <= before_tx then
          to_delete := key :: !to_delete);
    if !to_delete <> [] then
      Datascript_sqlite_db.with_write_txn db (fun () ->
        List.iter
          (fun key -> Datascript_sqlite_db.remove_index_txn Tave db key)
          !to_delete)
