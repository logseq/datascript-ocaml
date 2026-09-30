open Datascript_types

type db = Datascript_sqlite_db.t
type t = Datascript_sqlite_index.t
type seq = datom Datascript_sqlite_index.seq

let kind = storage_kind_sqlite
let sync = Datascript_sqlite_db.sync
let meta_get = Datascript_sqlite_db.meta_get
let meta_set = Datascript_sqlite_db.meta_set
let empty = Datascript_sqlite_index.empty
let db_of = Datascript_sqlite_index.db_of
let of_sorted_list = Datascript_sqlite_index.of_sorted_list
let of_sorted_lists = Datascript_sqlite_index.of_sorted_lists
let of_eavt_datoms = Datascript_sqlite_index.of_eavt_datoms
let of_bulk = Datascript_sqlite_index.of_bulk
let append_datoms = Datascript_sqlite_index.append_datoms

let append_tx_data ~avet ~tave datoms db =
  ignore
    (Datascript_sqlite_index.append_tx_data ~avet ~tave datoms
       (Datascript_sqlite_index.empty Eavt db)
       (Datascript_sqlite_index.empty Aevt db)
       (Datascript_sqlite_index.empty Avet db))

let add = Datascript_sqlite_index.add
let remove = Datascript_sqlite_index.remove
let remove_datoms = Datascript_sqlite_index.remove_datoms
let lookup = Datascript_sqlite_index.lookup
let to_list = Datascript_sqlite_index.to_list
let fold = Datascript_sqlite_index.fold
let fold_slice = Datascript_sqlite_index.fold_slice
let find_first_slice = Datascript_sqlite_index.find_first_slice
let fold_attr_prefix = Datascript_sqlite_index.fold_attr_prefix
let slice = Datascript_sqlite_index.slice
let slice_seq = Datascript_sqlite_index.slice_seq
let rslice_seq = Datascript_sqlite_index.rslice_seq
let seq = Datascript_sqlite_index.seq
let seq_to_list = Datascript_sqlite_index.seq_to_list
let fold_seq = Datascript_sqlite_index.fold_seq
let to_seq = Datascript_sqlite_index.to_seq
let seek = Datascript_sqlite_index.seek
let flush = Datascript_sqlite_index.flush
let copy = Datascript_sqlite_index.copy
let fold_tave_range = Datascript_sqlite_index.fold_tave_range
let prune_tave_before = Datascript_sqlite_index.prune_tave_before
