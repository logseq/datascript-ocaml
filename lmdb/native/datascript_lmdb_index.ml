open Datascript_types

type t =
  { db : Datascript_lmdb_db.t
  ; which : index
  ; additions : datom list
  ; removals : datom list
  }

type 'a seq = { cmp : datom -> datom -> int; datoms : datom list; offset : int }

let db_of t = t.db
let make index db = { db; which = index; additions = []; removals = [] }
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
  Datascript_lmdb_db.with_write_txn db (fun txn ->
    List.iter (put_datom_txn txn t) datoms);
  t

let add datom t =
  let key = datom_key t datom in
  let additions = datom :: List.filter (fun d -> datom_key t d <> key) t.additions in
  let removals = List.filter (fun d -> datom_key t d <> key) t.removals in
  { t with additions; removals }

let remove datom t =
  let key = datom_key t datom in
  let additions = List.filter (fun d -> datom_key t d <> key) t.additions in
  let already_removed = List.exists (fun d -> datom_key t d = key) t.removals in
  let removals =
    if already_removed || List.exists (fun d -> datom_key t d = key) t.additions then t.removals
    else datom :: t.removals
  in
  { t with additions; removals }

let removal_keys t =
  let table = Hashtbl.create (List.length t.removals) in
  List.iter (fun datom -> Hashtbl.add table (datom_key t datom) ()) t.removals;
  table

let addition_keys t =
  let table = Hashtbl.create (List.length t.additions) in
  List.iter (fun datom -> Hashtbl.replace table (datom_key t datom) datom) t.additions;
  table

let fold_stored t f acc =
  let removed = removal_keys t in
  let added = addition_keys t in
  let acc = ref acc in
  Datascript_lmdb_db.fold_index t.which t.db (fun key value ->
    if not (Hashtbl.mem removed key || Hashtbl.mem added key) then
      acc := f !acc (decode_entry t.which key value));
  !acc

let fold_stored_range t ?from_key ?to_key f acc =
  let removed = removal_keys t in
  let added = addition_keys t in
  let acc = ref acc in
  Datascript_lmdb_db.fold_index_range t.which t.db ?from_key ?to_key (fun key value ->
    if not (Hashtbl.mem removed key || Hashtbl.mem added key) then
      acc := f !acc (decode_entry t.which key value));
  !acc

let fold_overlay t f acc = List.fold_left f acc t.additions

let fold_datoms f init t =
  let acc = fold_stored t f init in
  fold_overlay t f acc

let collect_datoms t =
  fold_datoms (fun acc datom -> datom :: acc) [] t |> List.sort (cmp_for t.which)

let clear_index_txn txn index lmdb =
  Datascript_lmdb_db.fold_index index lmdb (fun key _ ->
    Datascript_lmdb_db.remove_index_txn index txn lmdb key)

let sync_merged_to_lmdb t target_lmdb =
  let merged = collect_datoms t in
  Datascript_lmdb_db.with_write_txn target_lmdb (fun txn ->
    clear_index_txn txn t.which target_lmdb;
    List.iter
      (fun datom ->
        let key = datom_key t datom in
        let value = Datascript_lmdb_codec.encode_datom_value datom in
        Datascript_lmdb_db.put_index_txn t.which txn target_lmdb key value)
      merged)

let copy_list xs = List.map (fun x -> x) xs

let copy t = { t with additions = copy_list t.additions; removals = copy_list t.removals }

let flush t =
  if overlay_empty t then t
  else (
    Datascript_lmdb_db.with_write_txn t.db (fun txn ->
      List.iter (remove_datom_txn txn t) t.removals;
      List.iter (put_datom_txn txn t) t.additions);
    { t with additions = []; removals = [] })

let to_list t = collect_datoms t
let fold f init t = fold_datoms f init t

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

let bound_key t = function
  | None -> None
  | Some datom -> Some (datom_key t datom)

let materialize_range t ?from_ ?to_ cmp =
  let filter datoms = List.filter (in_range cmp from_ to_) datoms in
  if overlay_empty t then
    match bound_key t from_ with
    | None -> filter (to_list t)
    | Some from_key ->
      fold_stored_range t ~from_key (fun acc datom -> datom :: acc) []
      |> List.rev
      |> filter
  else
    filter (to_list t)

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
let slice ?from_ ?to_ ?cmp t = slice_seq ?from_ ?to_ ?cmp t |> seq_to_list
let fold_seq f init seq = List.fold_left f init (seq_to_list seq)

let seek bound seq =
  let rec count index =
    if index >= List.length seq.datoms then index
    else if seq.cmp (List.nth seq.datoms index) bound >= 0 then index
    else count (index + 1)
  in
  { seq with offset = count 0 }
