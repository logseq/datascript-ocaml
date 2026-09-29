open Datascript_types

type db = Datascript_lmdb_db.t
type t = Datascript_lmdb_index.t
type seq = datom Datascript_lmdb_index.seq

let kind = storage_kind_lmdb
let sync = Datascript_lmdb_db.sync
let meta_get = Datascript_lmdb_db.meta_get
let meta_set = Datascript_lmdb_db.meta_set
let empty = Datascript_lmdb_index.empty
let db_of = Datascript_lmdb_index.db_of
let of_sorted_list = Datascript_lmdb_index.of_sorted_list
let of_sorted_lists = Datascript_lmdb_index.of_sorted_lists
let of_eavt_datoms = Datascript_lmdb_index.of_eavt_datoms
let of_bulk = Datascript_lmdb_index.of_bulk
let append_datoms = Datascript_lmdb_index.append_datoms

let append_tx_data ~avet datoms db =
  ignore
    (Datascript_lmdb_index.append_tx_data ~avet datoms
       (Datascript_lmdb_index.empty Eavt db)
       (Datascript_lmdb_index.empty Aevt db)
       (Datascript_lmdb_index.empty Avet db))

let add = Datascript_lmdb_index.add
let remove = Datascript_lmdb_index.remove
let remove_datoms = Datascript_lmdb_index.remove_datoms
let lookup = Datascript_lmdb_index.lookup
let to_list = Datascript_lmdb_index.to_list
let fold = Datascript_lmdb_index.fold
let fold_slice = Datascript_lmdb_index.fold_slice
let find_first_slice = Datascript_lmdb_index.find_first_slice
let fold_attr_prefix = Datascript_lmdb_index.fold_attr_prefix
let slice = Datascript_lmdb_index.slice
let slice_seq = Datascript_lmdb_index.slice_seq
let rslice_seq = Datascript_lmdb_index.rslice_seq
let seq = Datascript_lmdb_index.seq
let seq_to_list = Datascript_lmdb_index.seq_to_list
let fold_seq = Datascript_lmdb_index.fold_seq
let to_seq = Datascript_lmdb_index.to_seq
let seek = Datascript_lmdb_index.seek
let flush = Datascript_lmdb_index.flush
let copy = Datascript_lmdb_index.copy
let fold_tave_range = Datascript_lmdb_index.fold_tave_range
let prune_tave_before = Datascript_lmdb_index.prune_tave_before
