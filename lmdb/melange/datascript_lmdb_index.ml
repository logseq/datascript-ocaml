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

let datom_key t datom = Datascript_lmdb_codec.encode_datom_key t.which datom

let decode_entry index key value =
  let datom = Datascript_lmdb_codec.decode_datom_key index key in
  let payload = Datascript_lmdb_codec.decode_datom_value value in
  { datom with added = payload.added; v = payload.v }

let put_datom t datom =
  let key = datom_key t datom in
  let value = Datascript_lmdb_codec.encode_datom_value datom in
  Datascript_lmdb_db.put_index t.which t.db key value

let remove_datom t datom =
  let key = datom_key t datom in
  Datascript_lmdb_db.remove_index t.which t.db key

let empty index db = make index db

let of_sorted_list index datoms db =
  let t = empty index db in
  List.iter (put_datom t) datoms;
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

let collect_stored t =
  let datoms = ref [] in
  Datascript_lmdb_db.fold_index t.which t.db (fun key value ->
    datoms := decode_entry t.which key value :: !datoms);
  List.rev !datoms

let merge_overlay t base =
  let cmp = cmp_for t.which in
  let removed_keys = List.map (datom_key t) t.removals in
  let addition_keys = List.map (datom_key t) t.additions in
  let base =
    base
    |> List.filter (fun datom ->
      let key = datom_key t datom in
      not (List.mem key removed_keys || List.mem key addition_keys))
  in
  List.sort cmp (base @ t.additions)

let collect_datoms t = merge_overlay t (collect_stored t)

let put_datom_in index lmdb datom =
  let key = Datascript_lmdb_codec.encode_datom_key index datom in
  let value = Datascript_lmdb_codec.encode_datom_value datom in
  Datascript_lmdb_db.put_index index lmdb key value

let clear_index index lmdb =
  let keys = ref [] in
  Datascript_lmdb_db.fold_index index lmdb (fun key _ -> keys := key :: !keys);
  List.iter (fun key -> Datascript_lmdb_db.remove_index index lmdb key) !keys

let sync_merged_to_lmdb t target_lmdb =
  clear_index t.which target_lmdb;
  List.iter (put_datom_in t.which target_lmdb) (collect_datoms t)

let copy_list xs = List.map (fun x -> x) xs

let copy t = { t with additions = copy_list t.additions; removals = copy_list t.removals }

let flush t =
  List.iter (remove_datom t) t.removals;
  List.iter (put_datom t) t.additions;
  { t with additions = []; removals = [] }

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

let to_seq ({ cmp = _; datoms; offset = start }) =
  let rec loop index () =
    if index >= List.length datoms then Seq.Nil
    else Seq.Cons (List.nth datoms index, loop (index + 1))
  in
  loop start

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
