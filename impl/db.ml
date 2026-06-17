open Datascript_types

let tx0 = 0x20000000

let datom ?(tx = tx0) ?(added = true) ~e ~a ~v () = { e; a; v; tx; added }

let is_datom (_ : datom) = true

let value_equal = Util.value_equal

let same_fact left right = left.e = right.e && left.a = right.a && value_equal left.v right.v

let hash_cache : (int, int) Hashtbl.t = Hashtbl.create 128

let hash db =
  match Hashtbl.find_opt hash_cache db.db_uid with
  | Some hash -> hash
  | None ->
    let hash =
      Hashtbl.hash
        ( db.schema
        , db.datoms
        , db.history_datoms
        , db.historical
        , db.max_eid
        , db.max_tx
        )
    in
    Hashtbl.replace hash_cache db.db_uid hash;
    hash

let hash_cache_size () = Hashtbl.length hash_cache

let visible_datoms db index =
  let datoms =
    match index with
    | Eavt -> db.eavt_index
    | Aevt -> db.aevt_index
    | Avet -> db.avet_index
    | Vaet -> db.vaet_index
  in
  match db.filter_pred with
  | None -> datoms
  | Some pred -> List.filter pred datoms

let diff left right =
  let left_datoms = visible_datoms left Eavt in
  let right_datoms = visible_datoms right Eavt in
  ( List.filter (fun d -> not (List.exists (same_fact d) right_datoms)) left_datoms
  , List.filter (fun d -> not (List.exists (same_fact d) left_datoms)) right_datoms
  , List.filter (fun d -> List.exists (same_fact d) right_datoms) left_datoms
  )

let squuid_counter = ref 0

let squuid ?msec () =
  incr squuid_counter;
  let msec =
    match msec with
    | Some msec -> msec
    | None -> int_of_float (Unix.gettimeofday () *. 1000.0)
  in
  let seconds = msec / 1000 in
  let r1 = Random.bits () land 0xffff in
  let r2 = ((Random.bits () land 0x0fff) lor 0x4000) land 0xffff in
  let r3 = ((Random.bits () land 0x3fff) lor 0x8000) land 0xffff in
  let r4 = !squuid_counter land 0xffff in
  let r5 = Random.bits () land 0xffff in
  let r6 = Random.bits () land 0xffff in
  Uuid (Printf.sprintf "%08x-%04x-%04x-%04x-%04x%04x%04x" seconds r1 r2 r3 r4 r5 r6)

let squuid_time_millis = function
  | Uuid uuid ->
    if String.length uuid < 8 then invalid_arg "invalid squuid";
    int_of_string ("0x" ^ String.sub uuid 0 8) * 1000
  | _ -> invalid_arg "squuid_time_millis expects a uuid value"
