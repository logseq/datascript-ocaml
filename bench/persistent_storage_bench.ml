open Datascript

type timing = { label : string; elapsed_ms : float }

let now_ms () = Unix.gettimeofday () *. 1000.

let time label f =
  let start = now_ms () in
  let result = f () in
  ({ label; elapsed_ms = now_ms () -. start }, result)

let print_timing prefix { label; elapsed_ms } =
  Printf.printf "%s%s\t%.2f\n%!" prefix label elapsed_ms

let indexed =
  {
    cardinality = One;
    unique = None;
    indexed = true;
    is_component = false;
    no_history = false;
    doc = None;
    value_type = None;
    tuple_attrs = None;
    tuple_types = None;
  }

let unique_identity = { indexed with unique = Some Identity }

let schema =
  [
    ("block/id", unique_identity);
    ("block/journal-day", indexed);
    ("block/content", indexed);
    ("block/order", indexed);
    ("block/collapsed", indexed);
  ]

let block_tx count =
  List.init count (fun index ->
      let i = index + 1 in
      Entity
        {
          db_id = Some (Temp_id (Printf.sprintf "block-%05d" i));
          attrs =
            [
              ("block/id", One_value (String (Printf.sprintf "block-%05d" i)));
              ("block/journal-day", One_value (String "2026-06-27"));
              ("block/content", One_value (String (Printf.sprintf "Block %05d" i)));
              ("block/order", One_value (Float (Float.of_int i)));
              ("block/collapsed", One_value (Bool false));
            ];
        })

let add_block_tx id order =
  [
    Entity
      {
        db_id = Some (Temp_id id);
        attrs =
          [
            ("block/id", One_value (String id));
            ("block/journal-day", One_value (String "2026-06-27"));
            ("block/content", One_value (String id));
            ("block/order", One_value (Float order));
            ("block/collapsed", One_value (Bool false));
          ];
      };
  ]

let update_content_tx id content =
  [ Add (Lookup_ref ("block/id", String id), "block/content", String content) ]

let seq_length seq = Seq.fold_left (fun count _ -> count + 1) 0 seq

let file_size path =
  if Sys.file_exists path then (Unix.stat path).st_size else 0

let remove_if_exists path = if Sys.file_exists path then Sys.remove path

let row_count _storage = 1

module type BACKEND = sig
  val name : string
  val extension : string
  type session
  val open_session : string -> session
  val close_session : session -> unit
  val storage : session -> storage
  val cleanup : string -> unit
end

module Sqlite_backend : BACKEND = struct
  type session = Datascript_sqlite.session

  let name = "sqlite"
  let extension = "sqlite3"
  let open_session = Datascript_sqlite.open_session
  let close_session = Datascript_sqlite.close
  let storage = Datascript_sqlite.storage
  let cleanup _path = ()
end

module Lmdb_backend : BACKEND = struct
  type session = Datascript_lmdb.session

  let name = "lmdb"
  let extension = "lmdb"
  let open_session = Datascript_lmdb.open_session
  let close_session = Datascript_lmdb.close
  let storage = Datascript_lmdb.storage

  let cleanup path =
    let lock = path ^ "-lock" in
    if Sys.file_exists lock then Sys.remove lock
end

let run_backend (module B : BACKEND) size tx =
  let prefix = B.name ^ "-" in
  let db_path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "datascript-persistent-%s-%d.%s" B.name size B.extension)
  in
  remove_if_exists db_path;
  let session = B.open_session db_path in
  Fun.protect
    ~finally:(fun () ->
      B.close_session session;
      remove_if_exists db_path;
      B.cleanup db_path)
    (fun () ->
      let storage = B.storage session in
      let persistent_build, persistent_db =
        time "snapshot-build-and-store" (fun () ->
            let db = db_with tx (empty_db ~schema ~storage ()) in
            store db;
            db)
      in
      print_timing prefix persistent_build;
      Printf.printf "%ssnapshot-build-datoms\t%d\n%!" prefix
        (seq_length (datoms persistent_db Eavt ()));
      Printf.printf "%ssnapshot-kvs-rows-after-build\t%d\n%!" prefix (row_count storage);
      Printf.printf "%ssnapshot-file-size-after-build\t%d\n%!" prefix (file_size db_path);
      let restore_timing, restored_db =
        time "snapshot-restore" (fun () ->
            match restore storage with
            | Some db -> db
            | None -> failwith (B.name ^ " persistent db should restore"))
      in
      print_timing prefix restore_timing;
      let persistent_add, restored_db =
        time "snapshot-add-one-and-store-after-restore" (fun () ->
            let db =
              db_with
                (add_block_tx "persistent-new" (Float.of_int (size + 1)))
                restored_db
            in
            store db;
            db)
      in
      print_timing prefix persistent_add;
      Printf.printf "%ssnapshot-kvs-rows-after-add\t%d\n%!" prefix (row_count storage);
      Printf.printf "%ssnapshot-file-size-after-add\t%d\n%!" prefix (file_size db_path);
      let persistent_update, restored_db =
        time "snapshot-update-one-and-store-after-add" (fun () ->
            let db = db_with (update_content_tx "block-00001" "Edited") restored_db in
            store db;
            db)
      in
      print_timing prefix persistent_update;
      Printf.printf "%ssnapshot-kvs-rows-after-update\t%d\n%!" prefix (row_count storage);
      Printf.printf "%ssnapshot-file-size-after-update\t%d\n%!" prefix (file_size db_path);
      Printf.printf "%ssnapshot-datoms\t%d\n%!" prefix
        (seq_length (datoms restored_db Eavt ()));
      let conn_db_path =
        Filename.concat
          (Filename.get_temp_dir_name ())
          (Printf.sprintf "datascript-persistent-%s-conn-%d.%s" B.name size B.extension)
      in
      remove_if_exists conn_db_path;
      let session = B.open_session conn_db_path in
      Fun.protect
        ~finally:(fun () ->
          B.close_session session;
          remove_if_exists conn_db_path;
          B.cleanup conn_db_path)
        (fun () ->
          let storage = B.storage session in
          let conn_build, conn =
            time "conn-build" (fun () ->
                let conn = create_conn ~schema ~storage () in
                ignore (transact_conn conn tx);
                conn)
          in
          print_timing prefix conn_build;
          Printf.printf "%sconn-build-datoms\t%d\n%!" prefix
            (seq_length (datoms (db conn) Eavt ()));
          Printf.printf "%sconn-kvs-rows-after-build\t%d\n%!" prefix (row_count storage);
          Printf.printf "%sconn-file-size-after-build\t%d\n%!" prefix (file_size conn_db_path);
          let conn_restore, conn =
            time "conn-restore" (fun () ->
                match restore_conn storage with
                | Some conn -> conn
                | None -> failwith (B.name ^ " persistent conn should restore"))
          in
          print_timing prefix conn_restore;
          let conn_add, _report =
            time "conn-add-one-after-restore" (fun () ->
                transact_conn conn (add_block_tx "conn-new" (Float.of_int (size + 1))))
          in
          print_timing prefix conn_add;
          Printf.printf "%sconn-kvs-rows-after-add\t%d\n%!" prefix (row_count storage);
          Printf.printf "%sconn-file-size-after-add\t%d\n%!" prefix (file_size conn_db_path);
          let conn_update, _report =
            time "conn-update-one-after-add" (fun () ->
                transact_conn conn (update_content_tx "block-00001" "Edited"))
          in
          print_timing prefix conn_update;
          Printf.printf "%sconn-kvs-rows-after-update\t%d\n%!" prefix (row_count storage);
          Printf.printf "%sconn-file-size-after-update\t%d\n%!" prefix (file_size conn_db_path);
          Printf.printf "%sconn-datoms\t%d\n%!" prefix (seq_length (datoms (db conn) Eavt ()))))

let run_size size =
  Printf.printf "size\t%d\n%!" size;
  let tx = block_tx size in
  let memory_build, memory_db =
    time "memory-build" (fun () -> db_with tx (empty_db ~schema ()))
  in
  print_timing "" memory_build;
  let memory_add, memory_db =
    time "memory-add-one" (fun () ->
        db_with (add_block_tx "memory-new" (Float.of_int (size + 1))) memory_db)
  in
  print_timing "" memory_add;
  let _memory_update, memory_db =
    time "memory-update-one" (fun () ->
        db_with (update_content_tx "block-00001" "Edited") memory_db)
  in
  print_timing "" _memory_update;
  Printf.printf "memory-datoms\t%d\n%!" (seq_length (datoms memory_db Eavt ()));
  run_backend (module Sqlite_backend) size tx;
  run_backend (module Lmdb_backend) size tx

let parse_sizes () =
  let rec loop sizes = function
    | [] -> List.rev sizes
    | "--size" :: size :: rest -> loop (int_of_string size :: sizes) rest
    | "--sizes" :: value :: rest ->
        let parsed =
          value
          |> String.split_on_char ','
          |> List.filter (fun value -> String.length value > 0)
          |> List.map int_of_string
        in
        loop (List.rev_append parsed sizes) rest
    | arg :: _ -> invalid_arg ("unknown benchmark argument: " ^ arg)
  in
  match loop [] (Sys.argv |> Array.to_list |> List.tl) with
  | [] -> [ 100; 1000; 5000 ]
  | sizes -> sizes

let () = List.iter run_size (parse_sizes ())
