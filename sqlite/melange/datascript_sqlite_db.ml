open Datascript_types

module Key_map = Map.Make (String)

type remote_conn = {
  close : unit -> unit
; exec : string -> unit
; meta_get : string -> string option
; meta_set : string -> string -> unit
; put : string -> string -> string -> unit
; put_many : string -> (string * string) list -> unit
; get : string -> string -> string option
; remove : string -> string -> unit
; fold : string -> string option -> bool -> (string -> string -> bool) -> unit
; begin_txn : unit -> unit
; commit : unit -> unit
; rollback : unit -> unit
}

let open_remote : (string -> remote_conn) option ref = ref None

let set_driver_open opener = open_remote := Some opener

let has_driver () = Option.is_some !open_remote

type memory_tables =
  { mutable eavt : string Key_map.t
  ; mutable aevt : string Key_map.t
  ; mutable avet : string Key_map.t
  ; mutable tave : string Key_map.t
  ; mutable meta : string Key_map.t
  }

type impl =
  | Memory of memory_tables
  | Remote of remote_conn

type t =
  { path : string
  ; mutable closed : bool
  ; impl : impl
  ; mutable sync_count : int
  ; mutable full_index_scan_count : int
  }

let table_name = function
  | Eavt -> "ds_eavt"
  | Aevt -> "ds_aevt"
  | Avet -> "ds_avet"
  | Tave -> "ds_tave"

let empty_tables () =
  { eavt = Key_map.empty
  ; aevt = Key_map.empty
  ; avet = Key_map.empty
  ; tave = Key_map.empty
  ; meta = Key_map.empty
  }

let map_for_index index tables =
  match index with
  | Eavt -> tables.eavt
  | Aevt -> tables.aevt
  | Avet -> tables.avet
  | Tave -> tables.tave

let set_map_for_index index tables map =
  match index with
  | Eavt -> tables.eavt <- map
  | Aevt -> tables.aevt <- map
  | Avet -> tables.avet <- map
  | Tave -> tables.tave <- map

let ensure_open t =
  if t.closed then invalid_arg ("SQLite database is closed: " ^ t.path)

let schema_sql index =
  Printf.sprintf
    "CREATE TABLE IF NOT EXISTS %s (\n\
    \  key BLOB PRIMARY KEY NOT NULL,\n\
    \  value BLOB NOT NULL\n\
     ) WITHOUT ROWID;"
    (table_name index)

let meta_sql =
  "CREATE TABLE IF NOT EXISTS ds_meta (\n\
  \  key TEXT PRIMARY KEY NOT NULL,\n\
  \  value BLOB NOT NULL\n\
   ) WITHOUT ROWID;"

let ensure_schema conn =
  List.iter (fun index -> conn.exec (schema_sql index)) [ Eavt; Aevt; Avet; Tave ];
  conn.exec meta_sql

let open_path path =
  match !open_remote with
  | None -> invalid_arg "Datascript SQLite driver is not installed"
  | Some opener ->
      let conn = opener path in
      ensure_schema conn;
      { path; closed = false; impl = Remote conn; sync_count = 0; full_index_scan_count = 0 }

let temps_created = ref 0

let close t =
  if not t.closed then (
    (match t.impl with
     | Memory _ -> ()
     | Remote conn -> conn.close ());
    t.closed <- true)

let create_temp () =
  let path = "memory:" ^ string_of_int !temps_created in
  incr temps_created;
  { path
  ; closed = false
  ; impl = Memory (empty_tables ())
  ; sync_count = 0
  ; full_index_scan_count = 0
  }

let sync t =
  ensure_open t;
  t.sync_count <- t.sync_count + 1

let sync_count t = t.sync_count
let full_index_scan_count t = t.full_index_scan_count

let meta_get t key =
  ensure_open t;
  match t.impl with
  | Memory tables -> Key_map.find_opt key tables.meta
  | Remote conn -> conn.meta_get key

let meta_set t key value =
  ensure_open t;
  match t.impl with
  | Memory tables -> tables.meta <- Key_map.add key value tables.meta
  | Remote conn -> conn.meta_set key value

let with_write_txn t f =
  ensure_open t;
  match t.impl with
  | Memory _ -> f ()
  | Remote conn ->
      conn.begin_txn ();
      (try
         f ();
         conn.commit ()
       with exn ->
         (try conn.rollback () with _ -> ());
         raise exn)

let with_bulk_write_txn t f = with_write_txn t f

let put_index_txn index t key value =
  ensure_open t;
  match t.impl with
  | Memory tables -> set_map_for_index index tables (Key_map.add key value (map_for_index index tables))
  | Remote conn -> conn.put (table_name index) key value

let put_index_entries_txn index t entries =
  match entries with
  | [] -> ()
  | _ -> (
    ensure_open t;
    match t.impl with
    | Memory _ -> List.iter (fun (key, value) -> put_index_txn index t key value) entries
    | Remote conn -> conn.put_many (table_name index) entries)

let remove_index_txn index t key =
  ensure_open t;
  match t.impl with
  | Memory tables ->
      set_map_for_index index tables (Key_map.remove key (map_for_index index tables))
  | Remote conn -> conn.remove (table_name index) key

let put_index index t key value = with_write_txn t (fun () -> put_index_txn index t key value)

let remove_index index t key = with_write_txn t (fun () -> remove_index_txn index t key)

let get_index index t key =
  ensure_open t;
  match t.impl with
  | Memory tables -> Key_map.find_opt key (map_for_index index tables)
  | Remote conn -> conn.get (table_name index) key

let fold_memory_range map ?from_key ~desc ~stop f =
  let seq = if desc then Key_map.to_rev_seq map else Key_map.to_seq map in
  let rec loop seq =
    match seq () with
    | Seq.Nil -> ()
    | Seq.Cons ((key, value), rest) ->
        let skip =
          match from_key, desc with
          | Some start, false -> String.compare key start < 0
          | Some start, true -> String.compare key start > 0
          | None, _ -> false
        in
        if skip then loop rest
        else if stop key value then ()
        else (
          f key value;
          loop rest)
  in
  loop seq

let fold_index_range_until index t ?from_key ?stop f =
  ensure_open t;
  let stop = Option.value ~default:(fun _ _ -> false) stop in
  match t.impl with
  | Memory tables ->
      fold_memory_range (map_for_index index tables) ?from_key ~desc:false ~stop f
  | Remote conn ->
      conn.fold (table_name index) from_key false (fun key value ->
        if stop key value then true
        else (
          f key value;
          false))

let fold_index_range_desc_until index t ?hi_key ?stop f =
  ensure_open t;
  let stop = Option.value ~default:(fun _ _ -> false) stop in
  match t.impl with
  | Memory tables ->
      fold_memory_range (map_for_index index tables) ?from_key:hi_key ~desc:true ~stop f
  | Remote conn ->
      conn.fold (table_name index) hi_key true (fun key value ->
        if stop key value then true
        else (
          f key value;
          false))

let fold_index index t f =
  t.full_index_scan_count <- t.full_index_scan_count + 1;
  fold_index_range_until index t (fun key value -> f key value)

let fold_index_prefix index t prefix f =
  let prefix_len = String.length prefix in
  fold_index_range_until index t ~from_key:prefix
    ~stop:(fun key _value ->
      String.length key < prefix_len || String.sub key 0 prefix_len <> prefix)
    f

let stream_batch_size = 64

type asc_cont =
  | Asc_start
  | Asc_from of { key : string; include_key : bool }
  | Asc_done

let seq_index_range_until index db ?from_key ?stop () =
  ensure_open db;
  let cont =
    ref
      (match from_key with
       | None -> Asc_start
       | Some key -> Asc_from { key; include_key = true })
  in
  let fetch () =
    match !cont with
    | Asc_done -> []
    | _ ->
        let batch = ref [] in
        let count = ref 0 in
        let from_key, include_key =
          match !cont with
          | Asc_start -> None, true
          | Asc_from { key; include_key } -> Some key, include_key
          | Asc_done -> None, true
        in
        let skipping = ref (match from_key with Some _ when not include_key -> true | _ -> false) in
        let hit_end = ref true in
        fold_index_range_until index db ?from_key
          ~stop:(fun key value ->
            if !count >= stream_batch_size then (
              hit_end := false;
              true)
            else if !skipping then false
            else
              match stop with
              | Some stop when stop key value ->
                  cont := Asc_done;
                  true
              | _ -> false)
          (fun key value ->
            if !skipping then (
              match from_key with
              | Some cont_key when key = cont_key -> ()
              | _ ->
                  skipping := false;
                  batch := (key, value) :: !batch;
                  incr count;
                  cont := Asc_from { key; include_key = false })
            else (
              batch := (key, value) :: !batch;
              incr count;
              cont := Asc_from { key; include_key = false }));
        if !hit_end && !count < stream_batch_size then cont := Asc_done;
        List.rev !batch
  in
  let rec stream () =
    match fetch () with
    | [] -> Seq.Nil
    | items ->
        let rec of_list = function
          | [] -> stream
          | x :: xs -> fun () -> Seq.Cons (x, of_list xs)
        in
        of_list items ()
  in
  stream

let seq_index_prefix index db prefix () =
  let prefix_len = String.length prefix in
  seq_index_range_until index db ~from_key:prefix
    ~stop:(fun key _value ->
      String.length key < prefix_len || String.sub key 0 prefix_len <> prefix)
    ()

type desc_cont =
  | Desc_start of string option
  | Desc_after of string
  | Desc_done

let seq_index_range_desc_until index db ?hi_key ?stop () =
  ensure_open db;
  let cont = ref (Desc_start hi_key) in
  let fetch () =
    match !cont with
    | Desc_done -> []
    | _ ->
        let batch = ref [] in
        let count = ref 0 in
        let hi_key, skip_hi =
          match !cont with
          | Desc_start hi -> hi, false
          | Desc_after key -> Some key, true
          | Desc_done -> None, false
        in
        let skipping = ref skip_hi in
        let hit_end = ref true in
        fold_index_range_desc_until index db ?hi_key
          ~stop:(fun key value ->
            if !count >= stream_batch_size then (
              hit_end := false;
              true)
            else if !skipping then false
            else
              match stop with
              | Some stop when stop key value ->
                  cont := Desc_done;
                  true
              | _ -> false)
          (fun key value ->
            if !skipping then (
              match hi_key with
              | Some cont_key when key = cont_key -> ()
              | _ ->
                  skipping := false;
                  batch := (key, value) :: !batch;
                  incr count;
                  cont := Desc_after key)
            else (
              batch := (key, value) :: !batch;
              incr count;
              cont := Desc_after key));
        if !hit_end && !count < stream_batch_size then cont := Desc_done;
        List.rev !batch
  in
  let rec stream () =
    match fetch () with
    | [] -> Seq.Nil
    | items ->
        let rec of_list = function
          | [] -> stream
          | x :: xs -> fun () -> Seq.Cons (x, of_list xs)
        in
        of_list items ()
  in
  stream

let copy_index index from_db to_db =
  fold_index index from_db (fun key value -> put_index index to_db key value)
