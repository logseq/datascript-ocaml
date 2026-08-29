open Datascript_types

module Txn = struct
  type t = unit
end

type map = (string, string) Hashtbl.t

type t =
  { path : string
  ; eavt : map
  ; aevt : map
  ; avet : map
  ; tave : map
  ; meta : map
  ; mutable closed : bool
  }

let make_map () = Hashtbl.create 256

let remove_path _path = ()

let open_db path =
  { path
  ; eavt = make_map ()
  ; aevt = make_map ()
  ; avet = make_map ()
  ; tave = make_map ()
  ; meta = make_map ()
  ; closed = false
  }

let open_path path = open_db path

let ensure_open db =
  if db.closed then invalid_arg ("LMDB database is closed: " ^ db.path)

let close db =
  if not db.closed then db.closed <- true

let temps_created = ref 0

let create_temp () =
  let db = open_db ("melange:" ^ string_of_int !temps_created) in
  incr temps_created;
  db

let sync _db = ()

let map_for_index index db =
  match index with
  | Eavt -> db.eavt
  | Aevt -> db.aevt
  | Avet -> db.avet
  | Tave -> db.tave

let meta_get db key =
  ensure_open db;
  Hashtbl.find_opt db.meta key

let meta_set db key value =
  ensure_open db;
  Hashtbl.replace db.meta key value

let with_write_txn db f =
  ensure_open db;
  f ()

let put_index_txn index _txn db key value =
  Hashtbl.replace (map_for_index index db) key value

let remove_index_txn index _txn db key =
  Hashtbl.remove (map_for_index index db) key

let put_index index db key value =
  with_write_txn db (fun txn -> put_index_txn index txn db key value)

let remove_index index db key =
  with_write_txn db (fun txn -> remove_index_txn index txn db key)

let get_index index db key =
  ensure_open db;
  Hashtbl.find_opt (map_for_index index db) key

let sorted_entries map =
  Hashtbl.to_seq map
  |> Seq.map (fun (key, value) -> (key, value))
  |> List.of_seq
  |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)

let fold_index index db f =
  ensure_open db;
  List.iter (fun (key, value) -> f key value) (sorted_entries (map_for_index index db))

let fold_index_prefix index db prefix f =
  ensure_open db;
  let prefix_len = String.length prefix in
  List.iter
    (fun (key, value) ->
      if String.length key >= prefix_len && String.sub key 0 prefix_len = prefix then f key value)
    (sorted_entries (map_for_index index db))

let fold_index_range index db ?from_key ?to_key f =
  ensure_open db;
  List.iter
    (fun (key, value) ->
      (match from_key with
       | Some bound when String.compare key bound < 0 -> ()
       | _ -> (
         match to_key with
         | Some bound when String.compare key bound > 0 -> ()
         | _ -> f key value)))
    (sorted_entries (map_for_index index db))

let fold_index_range_until index db ?from_key ?stop f =
  ensure_open db;
  let rec iter = function
    | [] -> ()
    | (key, value) :: rest ->
        (match from_key with
         | Some bound when String.compare key bound < 0 -> iter rest
         | _ -> (
           match stop with
           | Some stop when stop key value -> ()
           | _ ->
               f key value;
               iter rest))
  in
  iter (sorted_entries (map_for_index index db))

let fold_index_range_desc_until index db ?hi_key ?stop f =
  ensure_open db;
  let entries = List.rev (sorted_entries (map_for_index index db)) in
  let rec iter = function
    | [] -> ()
    | (key, value) :: rest ->
        (match hi_key with
         | Some bound when String.compare key bound > 0 -> iter rest
         | _ -> (
           match stop with
           | Some stop when stop key value -> ()
           | _ ->
               f key value;
               iter rest))
  in
  iter entries

let copy_index_txn index txn from_db to_db =
  fold_index index from_db (fun key value -> put_index_txn index txn to_db key value)
