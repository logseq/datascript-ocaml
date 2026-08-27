open Datascript_types

let meta_schema_key = "schema"
let meta_max_eid_key = "max_eid"
let meta_max_tx_key = "max_tx"
let meta_duplicates_key = "duplicate_datoms"

let encode_int value =
  Datascript_lmdb_codec.encode_datoms
    [ { e = value; a = ""; v = Nil; tx = 0; added = true } ]

let decode_int bytes =
  match Datascript_lmdb_codec.decode_datoms bytes with
  | { e; _ } :: _ -> e
  | [] -> 0

type meta_get = string -> string option
type meta_set = string -> string -> unit

let store_meta meta_set db =
  meta_set meta_schema_key (Datascript_lmdb_codec.encode_schema db.schema);
  meta_set meta_max_eid_key (encode_int db.max_eid);
  meta_set meta_max_tx_key (encode_int db.max_tx);
  meta_set meta_duplicates_key (Datascript_lmdb_codec.encode_datoms db.duplicate_datoms)

let restore_meta meta_get =
  let schema =
    match meta_get meta_schema_key with
    | None -> []
    | Some bytes -> Datascript_lmdb_codec.decode_schema bytes
  in
  let max_eid =
    match meta_get meta_max_eid_key with
    | None -> 0
    | Some bytes -> decode_int bytes
  in
  let max_tx =
    match meta_get meta_max_tx_key with
    | None -> 0x20000000
    | Some bytes -> decode_int bytes
  in
  let duplicate_datoms =
    match meta_get meta_duplicates_key with
    | None -> []
    | Some bytes -> Datascript_lmdb_codec.decode_datoms bytes
  in
  schema, max_eid, max_tx, duplicate_datoms
