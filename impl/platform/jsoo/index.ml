open Datascript_types

(* Native LMDB indexes use identity coercions because [index_set] stays abstract in
   [Datascript_types] while this module owns the concrete LMDB representation. *)
external inject : Datascript_lmdb_index.t -> index_set = "%identity"
external project : index_set -> Datascript_lmdb_index.t = "%identity"

type t = index_set
type 'a seq = 'a Datascript_lmdb_index.seq
type lmdb = Datascript_lmdb_db.t

let create_lmdb storage =
  match storage with
  | Some storage -> (Datascript_storage_lmdb.lmdb storage, Some storage)
  | None -> (Datascript_lmdb_db.create_temp (), None)

let lmdb_of lmdb = lmdb
let db_of t = Datascript_lmdb_index.db_of (project t)

let lmdb_for_storage storage = Datascript_storage_lmdb.lmdb storage

let sync_indexes_to_storage eavt aevt avet target_storage =
  let target = Datascript_storage_lmdb.lmdb target_storage in
  Datascript_lmdb_index.sync_merged_to_lmdb (project eavt) target;
  Datascript_lmdb_index.sync_merged_to_lmdb (project aevt) target;
  Datascript_lmdb_index.sync_merged_to_lmdb (project avet) target

let load_indexes_from_storage storage target_lmdb =
  let source = Datascript_storage_lmdb.lmdb storage in
  if source != target_lmdb then Datascript_storage_lmdb.sync_indexes source target_lmdb

let empty index lmdb = Datascript_lmdb_index.empty index lmdb |> inject
let of_sorted_list index datoms lmdb = Datascript_lmdb_index.of_sorted_list index datoms lmdb |> inject
let of_sorted_lists index_datoms lmdb = Datascript_lmdb_index.of_sorted_lists index_datoms lmdb
let of_eavt_datoms ~avet datoms lmdb = Datascript_lmdb_index.of_eavt_datoms ~avet datoms lmdb
let of_bulk index datoms lmdb = Datascript_lmdb_index.of_bulk index datoms lmdb |> inject

let append_tx_data ~avet:is_avet datoms eavt_index aevt_index avet_index =
  let eavt, aevt, avet_index' =
    Datascript_lmdb_index.append_tx_data ~avet:is_avet datoms (project eavt_index) (project aevt_index)
      (project avet_index)
  in
  inject eavt, inject aevt, inject avet_index'

let append_datoms datoms t = Datascript_lmdb_index.append_datoms datoms (project t) |> inject

let add datom t = Datascript_lmdb_index.add datom (project t) |> inject
let remove datom t = Datascript_lmdb_index.remove datom (project t) |> inject
let lookup t datom = Datascript_lmdb_index.lookup (project t) datom
let to_list t = Datascript_lmdb_index.to_list (project t)
let fold f init t = Datascript_lmdb_index.fold f init (project t)
let fold_slice f init ?from_ ?to_ ?cmp t =
  Datascript_lmdb_index.fold_slice f init ?from_ ?to_ ?cmp (project t)
let find_first_slice ?from_ ?to_ ?cmp t =
  Datascript_lmdb_index.find_first_slice ?from_ ?to_ ?cmp (project t)

let fold_attr_prefix f init t attr =
  Datascript_lmdb_index.fold_attr_prefix f init (project t) attr
let slice ?from_ ?to_ ?cmp t = Datascript_lmdb_index.slice ?from_ ?to_ ?cmp (project t)
let slice_seq ?from_ ?to_ ?cmp t = Datascript_lmdb_index.slice_seq ?from_ ?to_ ?cmp (project t)
let rslice_seq ?from_ ?to_ ?cmp t = Datascript_lmdb_index.rslice_seq ?from_ ?to_ ?cmp (project t)
let seq t = Datascript_lmdb_index.seq (project t)
let seq_to_list = Datascript_lmdb_index.seq_to_list
let fold_seq = Datascript_lmdb_index.fold_seq
let to_seq = Datascript_lmdb_index.to_seq
let seek = Datascript_lmdb_index.seek
let flush t = Datascript_lmdb_index.flush (project t) |> inject
let copy t = Datascript_lmdb_index.copy (project t) |> inject
