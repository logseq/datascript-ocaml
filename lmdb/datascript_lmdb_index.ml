open Datascript_types

type t = { db : Datascript_lmdb_db.t; which : index }

type 'a seq = { cmp : datom -> datom -> int; datoms : datom list; offset : int }

let db_of t = t.db
let make index db = { db; which = index }
let cmp_for index = Datascript_types.Compare.compare_datom index

let decode_entry index key value =
  let datom = Datascript_lmdb_codec.decode_datom_key index key in
  let payload = Datascript_lmdb_codec.decode_datom_value value in
  { datom with added = payload.added; v = payload.v }

let put_datom t datom =
  let key = Datascript_lmdb_codec.encode_datom_key t.which datom in
  let value = Datascript_lmdb_codec.encode_datom_value datom in
  Datascript_lmdb_db.put_index t.which t.db key value

let remove_datom t datom =
  let key = Datascript_lmdb_codec.encode_datom_key t.which datom in
  Datascript_lmdb_db.remove_index t.which t.db key

let empty index db = make index db

let of_sorted_list index datoms db =
  let t = empty index db in
  List.iter (put_datom t) datoms;
  t

let add datom t =
  put_datom t datom;
  t

let remove datom t =
  remove_datom t datom;
  t

let collect_datoms t =
  let datoms = ref [] in
  Datascript_lmdb_db.fold_index t.which t.db (fun key value ->
    datoms := decode_entry t.which key value :: !datoms);
  List.rev !datoms

let to_list t = collect_datoms t
let fold f init t = List.fold_left f init (to_list t)

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

let make_seq ?(cmp = cmp_for Eavt) ?from_ ?to_ datoms =
  let datoms = List.filter (in_range cmp from_ to_) datoms in
  { cmp; datoms; offset = 0 }

let to_seq ({ datoms; offset } as seq) =
  let rec loop index () =
    if index >= List.length datoms then Seq.Nil
    else Seq.Cons (List.nth datoms index, loop (index + 1))
  in
  loop seq.offset

let seq t = make_seq ~cmp:(cmp_for t.which) (to_list t)

let slice_seq ?from_ ?to_ ?cmp t =
  let cmp = Option.value ~default:(cmp_for t.which) cmp in
  make_seq ~cmp ?from_ ?to_ (to_list t)

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
  make_seq ~cmp datoms

let slice ?from_ ?to_ ?cmp t = slice_seq ?from_ ?to_ ?cmp t |> seq_to_list

let seq_to_list seq = to_seq seq |> List.of_seq
let fold_seq f init seq = List.fold_left f init (seq_to_list seq)

let seek bound seq =
  let rec count index =
    if index >= List.length seq.datoms then index
    else if seq.cmp (List.nth seq.datoms index) bound >= 0 then index
    else count (index + 1)
  in
  { seq with offset = count 0 }
