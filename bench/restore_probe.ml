open Datascript

let indexed =
  { cardinality = One
  ; unique = None
  ; indexed = true
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let unique_identity = { indexed with unique = Some Identity }

let schema =
  [ "block/id", unique_identity
  ; "block/journal-day", indexed
  ; "block/content", indexed
  ; "block/order", indexed
  ; "block/collapsed", indexed
  ]

let block_tx base count =
  List.init count (fun index ->
      let i = base + index in
      Entity
        { db_id = Some (Temp_id (Printf.sprintf "b-%d" i))
        ; attrs =
            [ "block/id", One_value (String (Printf.sprintf "block-%07d" i))
            ; "block/journal-day", One_value (String "2026-06-27")
            ; "block/content", One_value (String (Printf.sprintf "Block content %07d lorem ipsum dolor" i))
            ; "block/order", One_value (Float (Float.of_int i))
            ; "block/collapsed", One_value (Bool false)
            ]
        })

let kvs_rows db =
  let stmt = Sqlite3.prepare db "select count(*) from kvs" in
  let n =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> Sqlite3.column_int stmt 0
    | rc -> failwith (Sqlite3.Rc.to_string rc)
  in
  ignore (Sqlite3.finalize stmt);
  n

let stores = ref 0
let restores = ref 0
let leaf_writes = ref 0
let branch_writes = ref 0
let node_write_bytes = ref 0
let delete_calls = ref 0
let call_sizes = ref []

let counting_storage inner =
  { storage_store =
      (fun entries ->
        incr stores;
        call_sizes := List.length entries :: !call_sizes;
        List.iter
          (fun (_addr, payload) ->
            match payload with
            | Storage_node (Persistent_sorted_set.Leaf _) ->
                incr leaf_writes;
                node_write_bytes :=
                  !node_write_bytes + String.length (Datascript_sqlite_codec.encode payload)
            | Storage_node (Persistent_sorted_set.Branch _) ->
                incr branch_writes;
                node_write_bytes :=
                  !node_write_bytes + String.length (Datascript_sqlite_codec.encode payload)
            | _ -> ())
          entries;
        inner.storage_store entries)
  ; storage_restore =
      (fun addr ->
        incr restores;
        inner.storage_restore addr)
  ; storage_list_addresses = inner.storage_list_addresses
  ; storage_delete =
      (fun addrs ->
        incr delete_calls;
        inner.storage_delete addrs)
  }

let () =
  let seed = int_of_string Sys.argv.(1) in
  let txs = int_of_string Sys.argv.(2) in
  let per_tx = int_of_string Sys.argv.(3) in
  let db_path = Filename.temp_file "restore-probe" ".sqlite3" in
  let db = Sqlite3.db_open db_path in
  (* phase 1: seed via a fresh conn *)
  let session = Datascript_sqlite.open_session db_path in
  let storage = counting_storage (Datascript_sqlite.storage session) in
  let conn = create_conn ~schema ~storage () in
  ignore (transact_conn conn (block_tx 0 seed));
  Printf.printf "seeded %d blocks: stores=%d leaf=%d branch=%d rows=%d\n%!" seed
    !stores !leaf_writes !branch_writes (kvs_rows db);
  Datascript_sqlite.close session;
  (* phase 2: reopen via restore_conn — backed deferred indexes *)
  stores := 0; leaf_writes := 0; branch_writes := 0; node_write_bytes := 0;
  restores := 0; delete_calls := 0;
  let session = Datascript_sqlite.open_session db_path in
  let storage = counting_storage (Datascript_sqlite.storage session) in
  let conn =
    match restore_conn storage with
    | Some conn -> conn
    | None -> failwith "restore failed"
  in
  Printf.printf "restored conn\n%!";
  let rows0 = kvs_rows db in
  for b = 1 to txs do
    ignore (transact_conn conn (block_tx (1000000 + b * 1000) per_tx))
  done;
  let rows1 = kvs_rows db in
  let fsz p = if Sys.file_exists p then (Unix.stat p).st_size else 0 in
  Printf.printf "file=%d wal=%d bytes\n%!" (fsz db_path) (fsz (db_path ^ "-wal"));
  Printf.printf
    "after %d txs x %d blocks: stores=%d leaf_writes=%d branch_writes=%d node_bytes=%d restores=%d deletes=%d rows %d -> %d (+%d)\n%!"
    txs per_tx !stores !leaf_writes !branch_writes !node_write_bytes !restores
    !delete_calls rows0 rows1 (rows1 - rows0);
  let sizes = List.rev !call_sizes in
  let show label xs =
    Printf.printf "%s store-call sizes: %s\n%!" label
      (String.concat "," (List.map string_of_int xs))
  in
  let rec take n xs = match n, xs with 0, _ | _, [] -> [] | n, x :: r -> x :: take (n - 1) r in
  let rec drop n xs = match n, xs with 0, _ -> xs | _, [] -> [] | n, _ :: r -> drop (n - 1) r in
  show "first-100" (take 100 sizes);
  show "last-100" (drop (List.length sizes - 100) sizes);
  ignore (Sqlite3.db_close db);
  Datascript_sqlite.close session;
  Sys.remove db_path
