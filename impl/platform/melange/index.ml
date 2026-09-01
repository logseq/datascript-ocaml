open Datascript_types

(* Melange indexes keep [index_set] abstract in [Datascript_types] while this module
   owns the concrete LMDB | SQLite representation. [%identity] is the established
   platform pattern for that boundary. *)
type concrete_index =
  | Lmdb of Datascript_lmdb_index.t
  | Sqlite of Datascript_sqlite_index.t

external inject : concrete_index -> index_set = "%identity"
external project : index_set -> concrete_index = "%identity"

type t = index_set

type 'a seq =
  | Lmdb_seq of 'a Datascript_lmdb_index.seq
  | Sqlite_seq of 'a Datascript_sqlite_index.seq

type index_db = Datascript_storage_protocol.index_db
type lmdb = index_db

let same_storage_db storage index_db =
  Datascript_storage_protocol.same_storage_db storage index_db

let create_index_db storage = Datascript_storage_protocol.create_index_db storage
let create_lmdb = create_index_db

let index_db_of index_db = index_db
let lmdb_of = index_db_of

let db_of t =
  match project t with
  | Lmdb i -> Datascript_storage_protocol.Lmdb (Datascript_lmdb_index.db_of i)
  | Sqlite i -> Datascript_storage_protocol.Sqlite (Datascript_sqlite_index.db_of i)

let index_db_for_storage storage = Datascript_storage_protocol.db_for_storage storage
let lmdb_for_storage = index_db_for_storage

let sync_indexes_to_storage ~since_tx eavt aevt avet target_storage =
  let sync_tave_lmdb src_db target =
    let tave = Datascript_lmdb_index.empty Tave src_db in
    Datascript_lmdb_index.sync_append_since_tx ~since_tx tave target
  in
  let sync_tave_sqlite src_db target =
    let tave = Datascript_sqlite_index.empty Tave src_db in
    Datascript_sqlite_index.sync_append_since_tx ~since_tx tave target
  in
  match project eavt, project aevt, project avet, Datascript_storage_protocol.db_for_storage target_storage with
  | Lmdb e, Lmdb a, Lmdb v, Datascript_storage_protocol.Lmdb target ->
      Datascript_lmdb_index.sync_append_since_tx ~since_tx e target;
      Datascript_lmdb_index.sync_append_since_tx ~since_tx a target;
      Datascript_lmdb_index.sync_append_since_tx ~since_tx v target;
      sync_tave_lmdb (Datascript_lmdb_index.db_of e) target
  | Sqlite e, Sqlite a, Sqlite v, Datascript_storage_protocol.Sqlite target ->
      Datascript_sqlite_index.sync_append_since_tx ~since_tx e target;
      Datascript_sqlite_index.sync_append_since_tx ~since_tx a target;
      Datascript_sqlite_index.sync_append_since_tx ~since_tx v target;
      sync_tave_sqlite (Datascript_sqlite_index.db_of e) target
  | Lmdb e, Lmdb a, Lmdb v, Datascript_storage_protocol.Sqlite target ->
      let put_since index_t which =
        let dest = Datascript_sqlite_index.empty which target in
        Datascript_lmdb_index.fold
          (fun () datom ->
            if datom.tx > since_tx then ignore (Datascript_sqlite_index.add datom dest))
          () index_t
      in
      put_since e Eavt;
      put_since a Aevt;
      put_since v Avet;
      put_since (Datascript_lmdb_index.empty Tave (Datascript_lmdb_index.db_of e)) Tave
  | _ ->
      invalid_arg "Index.sync_indexes_to_storage: unsupported index/storage backend combination"

let sync_removals_to_storage removed_datoms eavt aevt avet target_storage =
  ignore (eavt, aevt, avet);
  match Datascript_storage_protocol.db_for_storage target_storage with
  | Datascript_storage_protocol.Lmdb target ->
      let remove which =
        let t = Datascript_lmdb_index.empty which target in
        ignore (Datascript_lmdb_index.remove_datoms removed_datoms t)
      in
      remove Eavt;
      remove Aevt;
      remove Avet;
      remove Tave
  | Datascript_storage_protocol.Sqlite target ->
      let remove which =
        let t = Datascript_sqlite_index.empty which target in
        ignore (Datascript_sqlite_index.remove_datoms removed_datoms t)
      in
      remove Eavt;
      remove Aevt;
      remove Avet;
      remove Tave

let load_indexes_from_storage storage target =
  Datascript_storage_protocol.load_indexes_from_storage storage target

let empty index = function
  | Datascript_storage_protocol.Lmdb db -> Datascript_lmdb_index.empty index db |> fun i -> inject (Lmdb i)
  | Datascript_storage_protocol.Sqlite db -> Datascript_sqlite_index.empty index db |> fun i -> inject (Sqlite i)

let of_sorted_list index datoms = function
  | Datascript_storage_protocol.Lmdb db ->
      Datascript_lmdb_index.of_sorted_list index datoms db |> fun i -> inject (Lmdb i)
  | Datascript_storage_protocol.Sqlite db ->
      Datascript_sqlite_index.of_sorted_list index datoms db |> fun i -> inject (Sqlite i)

let of_sorted_lists index_datoms = function
  | Datascript_storage_protocol.Lmdb db -> Datascript_lmdb_index.of_sorted_lists index_datoms db
  | Datascript_storage_protocol.Sqlite db -> Datascript_sqlite_index.of_sorted_lists index_datoms db

let of_eavt_datoms ~avet datoms = function
  | Datascript_storage_protocol.Lmdb db -> Datascript_lmdb_index.of_eavt_datoms ~avet datoms db
  | Datascript_storage_protocol.Sqlite db -> Datascript_sqlite_index.of_eavt_datoms ~avet datoms db

let of_bulk index datoms = function
  | Datascript_storage_protocol.Lmdb db ->
      Datascript_lmdb_index.of_bulk index datoms db |> fun i -> inject (Lmdb i)
  | Datascript_storage_protocol.Sqlite db ->
      Datascript_sqlite_index.of_bulk index datoms db |> fun i -> inject (Sqlite i)

let append_tx_data ~avet:is_avet datoms eavt_index aevt_index avet_index =
  match project eavt_index, project aevt_index, project avet_index with
  | Lmdb eavt, Lmdb aevt, Lmdb avet_index' ->
      let eavt, aevt, avet_index' =
        Datascript_lmdb_index.append_tx_data ~avet:is_avet datoms eavt aevt avet_index'
      in
      inject (Lmdb eavt), inject (Lmdb aevt), inject (Lmdb avet_index')
  | Sqlite eavt, Sqlite aevt, Sqlite avet_index' ->
      let eavt, aevt, avet_index' =
        Datascript_sqlite_index.append_tx_data ~avet:is_avet datoms eavt aevt avet_index'
      in
      inject (Sqlite eavt), inject (Sqlite aevt), inject (Sqlite avet_index')
  | _ -> invalid_arg "Index.append_tx_data: mixed LMDB/SQLite index backends"

let append_datoms datoms t =
  match project t with
  | Lmdb i -> Datascript_lmdb_index.append_datoms datoms i |> fun i -> inject (Lmdb i)
  | Sqlite i -> Datascript_sqlite_index.append_datoms datoms i |> fun i -> inject (Sqlite i)

let add datom t =
  match project t with
  | Lmdb i -> Datascript_lmdb_index.add datom i |> fun i -> inject (Lmdb i)
  | Sqlite i -> Datascript_sqlite_index.add datom i |> fun i -> inject (Sqlite i)

let remove datom t =
  match project t with
  | Lmdb i -> Datascript_lmdb_index.remove datom i |> fun i -> inject (Lmdb i)
  | Sqlite i -> Datascript_sqlite_index.remove datom i |> fun i -> inject (Sqlite i)

let lookup t datom =
  match project t with
  | Lmdb i -> Datascript_lmdb_index.lookup i datom
  | Sqlite i -> Datascript_sqlite_index.lookup i datom

let to_list t =
  match project t with
  | Lmdb i -> Datascript_lmdb_index.to_list i
  | Sqlite i -> Datascript_sqlite_index.to_list i

let fold f init t =
  match project t with
  | Lmdb i -> Datascript_lmdb_index.fold f init i
  | Sqlite i -> Datascript_sqlite_index.fold f init i

let fold_slice f init ?from_ ?to_ ?cmp t =
  match project t with
  | Lmdb i -> Datascript_lmdb_index.fold_slice f init ?from_ ?to_ ?cmp i
  | Sqlite i -> Datascript_sqlite_index.fold_slice f init ?from_ ?to_ ?cmp i

let find_first_slice ?from_ ?to_ ?cmp t =
  match project t with
  | Lmdb i -> Datascript_lmdb_index.find_first_slice ?from_ ?to_ ?cmp i
  | Sqlite i -> Datascript_sqlite_index.find_first_slice ?from_ ?to_ ?cmp i

let fold_attr_prefix f init t attr =
  match project t with
  | Lmdb i -> Datascript_lmdb_index.fold_attr_prefix f init i attr
  | Sqlite i -> Datascript_sqlite_index.fold_attr_prefix f init i attr

let slice ?from_ ?to_ ?cmp t =
  match project t with
  | Lmdb i -> Datascript_lmdb_index.slice ?from_ ?to_ ?cmp i
  | Sqlite i -> Datascript_sqlite_index.slice ?from_ ?to_ ?cmp i

let slice_seq ?from_ ?to_ ?cmp t =
  match project t with
  | Lmdb i -> Lmdb_seq (Datascript_lmdb_index.slice_seq ?from_ ?to_ ?cmp i)
  | Sqlite i -> Sqlite_seq (Datascript_sqlite_index.slice_seq ?from_ ?to_ ?cmp i)

let rslice_seq ?from_ ?to_ ?cmp t =
  match project t with
  | Lmdb i -> Lmdb_seq (Datascript_lmdb_index.rslice_seq ?from_ ?to_ ?cmp i)
  | Sqlite i -> Sqlite_seq (Datascript_sqlite_index.rslice_seq ?from_ ?to_ ?cmp i)

let seq t =
  match project t with
  | Lmdb i -> Lmdb_seq (Datascript_lmdb_index.seq i)
  | Sqlite i -> Sqlite_seq (Datascript_sqlite_index.seq i)

let to_seq = function
  | Lmdb_seq s -> Datascript_lmdb_index.to_seq s
  | Sqlite_seq s -> Datascript_sqlite_index.to_seq s

let seq_to_list = function
  | Lmdb_seq s -> Datascript_lmdb_index.seq_to_list s
  | Sqlite_seq s -> Datascript_sqlite_index.seq_to_list s

let fold_seq f init = function
  | Lmdb_seq s -> Datascript_lmdb_index.fold_seq f init s
  | Sqlite_seq s -> Datascript_sqlite_index.fold_seq f init s

let seek bound = function
  | Lmdb_seq s -> Lmdb_seq (Datascript_lmdb_index.seek bound s)
  | Sqlite_seq s -> Sqlite_seq (Datascript_sqlite_index.seek bound s)

let flush t =
  match project t with
  | Lmdb i -> Datascript_lmdb_index.flush i |> fun i -> inject (Lmdb i)
  | Sqlite i -> Datascript_sqlite_index.flush i |> fun i -> inject (Sqlite i)

let copy t =
  match project t with
  | Lmdb i -> Datascript_lmdb_index.copy i |> fun i -> inject (Lmdb i)
  | Sqlite i -> Datascript_sqlite_index.copy i |> fun i -> inject (Sqlite i)

let fold_tave_range f init t ~from_tx ?to_tx ?attr () =
  match project t with
  | Lmdb i ->
      Datascript_lmdb_index.fold_tave_range f init (Datascript_lmdb_index.db_of i) ~from_tx ?to_tx
        ?attr ()
  | Sqlite i ->
      Datascript_sqlite_index.fold_tave_range f init (Datascript_sqlite_index.db_of i) ~from_tx ?to_tx
        ?attr ()

let prune_tave_before t ~before_tx =
  match project t with
  | Lmdb i -> Datascript_lmdb_index.prune_tave_before (Datascript_lmdb_index.db_of i) ~before_tx
  | Sqlite i -> Datascript_sqlite_index.prune_tave_before (Datascript_sqlite_index.db_of i) ~before_tx
