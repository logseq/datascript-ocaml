open Datascript
module DT = Internal.Datascript_types
open DT

let indexed =
  { DT.cardinality = DT.One; unique = None; indexed = true; is_component = false; no_history = false; doc = None; value_type = None; tuple_attrs = None; tuple_types = None }

let unique_identity = { indexed with DT.unique = Some DT.Identity }

let schema =
  [
    ("block/id", unique_identity);
    ("block/content", indexed);
    ("block/order", indexed);
  ]

let block_tx base count =
  List.init count (fun index ->
      let i = base + index in
      Entity
        {
          db_id = Some (Temp_id (Printf.sprintf "b-%d" i));
          attrs =
            [
              ("block/id", One_value (String (Printf.sprintf "b-%d" i)));
              ("block/content", One_value (String (Printf.sprintf "Block %d" i)));
              ("block/order", One_value (Float (Float.of_int i)));
            ];
        })

let kvs_rows db_path =
  let db = Sqlite3.db_open db_path in
  let stmt = Sqlite3.prepare db "select count(*) from kvs" in
  let n =
    match Sqlite3.step stmt with
    | Sqlite3.Rc.ROW -> Sqlite3.column_int stmt 0
    | rc -> failwith (Sqlite3.Rc.to_string rc)
  in
  ignore (Sqlite3.finalize stmt);
  ignore (Sqlite3.db_close db);
  n

let () =
  let batches = int_of_string Sys.argv.(1) in
  let per_batch = int_of_string Sys.argv.(2) in
  let db_path = Filename.temp_file "write-probe" ".sqlite3" in
  let session = Datascript_sqlite.open_session db_path in
  let storage = Datascript_sqlite.storage session in
  let conn = Internal.create_conn ~schema ~storage () in
  let mutable_rows = ref (kvs_rows db_path) in
  Printf.printf "start rows=%d\n%!" !mutable_rows;
  for b = 1 to batches do
    ignore (Internal.transact_conn conn (block_tx (b * 10000) per_batch));
    let rows = kvs_rows db_path in
    Printf.printf "batch %d: rows=%d (+%d)\n%!" b rows (rows - !mutable_rows);
    mutable_rows := rows
  done;
  Datascript_sqlite.close session;
  Sys.remove db_path
