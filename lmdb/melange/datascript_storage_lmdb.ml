open Datascript_types

type t = Datascript_lmdb_db.t

module Storage_registry = struct
  type t = storage

  let equal left right = left == right

  let hash storage = Hashtbl.hash (Obj.repr storage)
end

module Registry = Hashtbl.Make (Storage_registry)

let registry = Registry.create 16

let lmdb storage =
  match Registry.find_opt registry storage with
  | Some lmdb -> lmdb
  | None -> invalid_arg "storage is not LMDB-backed"

let register storage lmdb = Registry.replace registry storage lmdb

let create_temp () = Datascript_lmdb_db.create_temp ()
let open_path path = Datascript_lmdb_db.open_path path
let close = Datascript_lmdb_db.close
let sync = Datascript_lmdb_db.sync

let meta_get = Datascript_lmdb_db.meta_get
let meta_set = Datascript_lmdb_db.meta_set

let meta_schema_key = "schema"
let meta_max_eid_key = "max_eid"
let meta_max_tx_key = "max_tx"
let meta_duplicates_key = "duplicate_datoms"

let wrap lmdb =
  let storage =
    { storage_store =
        (fun _entries -> sync lmdb)
    ; storage_restore =
        (fun address ->
          if String.equal address "lmdb" then Some Storage_session else None)
    ; storage_list_addresses = (fun () -> [ "lmdb" ])
    ; storage_delete = (fun _addresses -> ())
    }
  in
  register storage lmdb;
  storage

let memory_storage () = wrap (create_temp ())

let encode_int value =
  Datascript_lmdb_codec.encode_datoms
    [ { e = value; a = ""; v = Nil; tx = 0; added = true } ]

let decode_int bytes =
  match Datascript_lmdb_codec.decode_datoms bytes with
  | { e; _ } :: _ -> e
  | [] -> 0

let store_meta lmdb db =
  meta_set lmdb meta_schema_key (Datascript_lmdb_codec.encode_schema db.schema);
  meta_set lmdb meta_max_eid_key (encode_int db.max_eid);
  meta_set lmdb meta_max_tx_key (encode_int db.max_tx);
  meta_set lmdb meta_duplicates_key (Datascript_lmdb_codec.encode_datoms db.duplicate_datoms);
  sync lmdb

let restore_meta lmdb =
  let schema =
    match meta_get lmdb meta_schema_key with
    | None -> []
    | Some bytes -> Datascript_lmdb_codec.decode_schema bytes
  in
  let max_eid =
    match meta_get lmdb meta_max_eid_key with
    | None -> 0
    | Some bytes -> decode_int bytes
  in
  let max_tx =
    match meta_get lmdb meta_max_tx_key with
    | None -> 0x20000000
    | Some bytes -> decode_int bytes
  in
  let duplicate_datoms =
    match meta_get lmdb meta_duplicates_key with
    | None -> []
    | Some bytes -> Datascript_lmdb_codec.decode_datoms bytes
  in
  schema, max_eid, max_tx, duplicate_datoms

let sync_indexes from_lmdb to_lmdb =
  let clear_index index db =
    let keys = ref [] in
    Datascript_lmdb_db.fold_index index db (fun key _ -> keys := key :: !keys);
    List.iter (fun key -> Datascript_lmdb_db.remove_index index db key) !keys
  in
  List.iter (fun index -> clear_index index to_lmdb) [ Eavt; Aevt; Avet ];
  List.iter
    (fun index ->
      Datascript_lmdb_db.fold_index index from_lmdb (fun key value ->
        Datascript_lmdb_db.put_index index to_lmdb key value))
    [ Eavt; Aevt; Avet ]

let store_db storage db =
  store_meta (lmdb storage) db
