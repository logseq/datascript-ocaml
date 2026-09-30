open Datascript_types

(* Native indexes keep [index_set] abstract in [Datascript_types] while this module
   owns the concrete representation: a first-class module implementing
   [Datascript_index_backend.S] packed with its index handle and the identity
   token of the [index_db] it was built from. [%identity] is the established
   platform pattern for that boundary (see prior LMDB-only Index). *)
type concrete_index =
  | Concrete : 'idx 'db 'sq.
      'idx * int
      * (module Datascript_index_backend.S
           with type db = 'db
            and type t = 'idx
            and type seq = 'sq)
      -> concrete_index

external inject : concrete_index -> index_set = "%identity"
external project : index_set -> concrete_index = "%identity"

type t = index_set

type 'a seq =
  | Seq : 'sq * (module Datascript_index_backend.S with type seq = 'sq) -> 'a seq

type index_db = Datascript_storage_protocol.index_db
type lmdb = index_db

let same_storage_db storage index_db =
  Datascript_storage_protocol.same_storage_db storage index_db

let create_index_db storage = Datascript_storage_protocol.create_index_db storage
let create_lmdb = create_index_db

let index_db_of index_db = index_db
let lmdb_of = index_db_of

let db_of t =
  let (Concrete (i, tok, (module B))) = project t in
  Datascript_storage_protocol.Index_db (tok, B.db_of i, (module B))

let index_db_for_storage storage = Datascript_storage_protocol.db_for_storage storage
let lmdb_for_storage = index_db_for_storage

let sync_indexes_to_storage ~since_tx eavt aevt avet target_storage =
  let (Datascript_storage_protocol.Index_db (_, target_db, (module B_tgt))) =
    Datascript_storage_protocol.db_for_storage target_storage
  in
  let put which (Concrete (i, _, (module B_src))) =
    let dest = B_tgt.empty which target_db in
    B_src.fold
      (fun () datom -> if datom.tx > since_tx then ignore (B_tgt.add datom dest))
      () i
  in
  put Eavt (project eavt);
  put Aevt (project aevt);
  put Avet (project avet);
  let (Concrete (e, _, (module B_src))) = project eavt in
  put Tave (Concrete (B_src.empty Tave (B_src.db_of e), 0, (module B_src)))

let sync_removals_to_storage removed_datoms eavt aevt avet target_storage =
  ignore (eavt, aevt, avet);
  let (Datascript_storage_protocol.Index_db (_, db, (module B))) =
    Datascript_storage_protocol.db_for_storage target_storage
  in
  List.iter
    (fun which -> ignore (B.remove_datoms removed_datoms (B.empty which db)))
    [ Eavt; Aevt; Avet; Tave ]

let load_indexes_from_storage storage target =
  Datascript_storage_protocol.load_indexes_from_storage storage target

let pack (type db idx sq) (i : idx) tok
    (module B : Datascript_index_backend.S
      with type db = db
       and type t = idx
       and type seq = sq) =
  inject (Concrete (i, tok, (module B)))

let empty index = function
  | Datascript_storage_protocol.Index_db (tok, db, (module B)) ->
      pack (B.empty index db) tok (module B)

let of_sorted_list index datoms = function
  | Datascript_storage_protocol.Index_db (tok, db, (module B)) ->
      pack (B.of_sorted_list index datoms db) tok (module B)

let of_sorted_lists index_datoms = function
  | Datascript_storage_protocol.Index_db (_, db, (module B)) ->
      B.of_sorted_lists index_datoms db

let of_eavt_datoms ~avet ~tave datoms = function
  | Datascript_storage_protocol.Index_db (_, db, (module B)) ->
      B.of_eavt_datoms ~avet ~tave datoms db

let of_bulk index datoms = function
  | Datascript_storage_protocol.Index_db (tok, db, (module B)) ->
      pack (B.of_bulk index datoms db) tok (module B)

let append_tx_data ~avet:is_avet ~tave datoms eavt aevt avet =
  let (Concrete (e, tok, (module B))) = project eavt in
  let check_same = function
    | Concrete (_, tok', _) when tok' = tok -> ()
    | _ -> invalid_arg "Index.append_tx_data: mixed index backends"
  in
  check_same (project aevt);
  check_same (project avet);
  B.append_tx_data ~avet:is_avet ~tave datoms (B.db_of e);
  eavt, aevt, avet

let append_datoms datoms t =
  let (Concrete (i, tok, (module B))) = project t in
  pack (B.append_datoms datoms i) tok (module B)

let add datom t =
  let (Concrete (i, tok, (module B))) = project t in
  pack (B.add datom i) tok (module B)

let remove datom t =
  let (Concrete (i, tok, (module B))) = project t in
  pack (B.remove datom i) tok (module B)

let lookup t datom =
  let (Concrete (i, _, (module B))) = project t in
  B.lookup i datom

let to_list t =
  let (Concrete (i, _, (module B))) = project t in
  B.to_list i

let fold f init t =
  let (Concrete (i, _, (module B))) = project t in
  B.fold f init i

let fold_slice f init ?from_ ?to_ ?cmp t =
  let (Concrete (i, _, (module B))) = project t in
  B.fold_slice f init ?from_ ?to_ ?cmp i

let find_first_slice ?from_ ?to_ ?cmp t =
  let (Concrete (i, _, (module B))) = project t in
  B.find_first_slice ?from_ ?to_ ?cmp i

let fold_attr_prefix f init t attr =
  let (Concrete (i, _, (module B))) = project t in
  B.fold_attr_prefix f init i attr

let slice ?from_ ?to_ ?cmp t =
  let (Concrete (i, _, (module B))) = project t in
  B.slice ?from_ ?to_ ?cmp i

let slice_seq ?from_ ?to_ ?cmp t =
  let (Concrete (i, _, (module B))) = project t in
  Seq (B.slice_seq ?from_ ?to_ ?cmp i, (module B))

let rslice_seq ?from_ ?to_ ?cmp t =
  let (Concrete (i, _, (module B))) = project t in
  Seq (B.rslice_seq ?from_ ?to_ ?cmp i, (module B))

let seq t =
  let (Concrete (i, _, (module B))) = project t in
  Seq (B.seq i, (module B))

let to_seq (Seq (s, (module B))) = B.to_seq s

let seq_to_list (Seq (s, (module B))) = B.seq_to_list s

let fold_seq f init (Seq (s, (module B))) = B.fold_seq f init s

let seek bound (Seq (s, (module B))) = Seq (B.seek bound s, (module B))

let flush t =
  let (Concrete (i, tok, (module B))) = project t in
  pack (B.flush i) tok (module B)

let copy t =
  let (Concrete (i, tok, (module B))) = project t in
  pack (B.copy i) tok (module B)

let fold_tave_range f init t ~from_tx ?to_tx ?attr () =
  let (Concrete (i, _, (module B))) = project t in
  B.fold_tave_range f init (B.db_of i) ~from_tx ?to_tx ?attr ()

let prune_tave_before t ~before_tx =
  let (Concrete (i, _, (module B))) = project t in
  B.prune_tave_before (B.db_of i) ~before_tx
