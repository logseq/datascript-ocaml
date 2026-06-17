open Datascript

module Sqlite_storage = Logseq_sqlite_storage

let failf fmt = Printf.ksprintf failwith fmt

let read_all channel =
  let buffer = Buffer.create 256 in
  (try
     while true do
       Buffer.add_string buffer (input_line channel);
       Buffer.add_char buffer '\n'
     done
   with
   | End_of_file -> ());
  Buffer.contents buffer

let sqlite3_available () =
  match Sys.command "command -v sqlite3 >/dev/null 2>&1" with
  | 0 -> true
  | _ -> false

let run_sql db_path sql =
  let argv =
    [| "sqlite3"; "-batch"; "-noheader"; "-list"; "-separator"; "\t"; db_path |]
  in
  let stdout, stdin, stderr = Unix.open_process_args_full "sqlite3" argv [||] in
  output_string stdin sql;
  if sql = "" || sql.[String.length sql - 1] <> '\n' then output_char stdin '\n';
  close_out stdin;
  let output = read_all stdout in
  let error = read_all stderr in
  match Unix.close_process_full (stdout, stdin, stderr) with
  | Unix.WEXITED 0 -> output
  | Unix.WEXITED code -> failf "sqlite3 exited with %d: %s" code error
  | Unix.WSIGNALED signal -> failf "sqlite3 killed by signal %d: %s" signal error
  | Unix.WSTOPPED signal -> failf "sqlite3 stopped by signal %d: %s" signal error

let with_temp_db f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      ("datascript_ocaml_sqlite_" ^ string_of_int (Random.bits ()))
  in
  Unix.mkdir dir 0o755;
  let db_path = Filename.concat dir "db.sqlite" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists db_path then Sys.remove db_path;
      if Sys.file_exists dir then Unix.rmdir dir)
    (fun () -> f db_path)

let assert_equal label expected actual =
  if expected <> actual then failf "%s: expected %S, got %S" label expected actual

let assert_equal_int label expected actual =
  if expected <> actual then failf "%s: expected %d, got %d" label expected actual

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

let test_sqlite_storage_round_trips_ocaml_payloads () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage round trip: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let db =
        init_db
          ~schema:[ "name", indexed ]
          [ datom ~e:1 ~a:"name" ~v:(String "Ada") () ]
      in
      store ~storage db;
      assert_equal
        "kvs schema"
        "CREATE TABLE kvs (addr INTEGER primary key, content TEXT, addresses JSON);\n"
        (run_sql db_path ".schema kvs");
      assert_equal_int "row count" 2 (Sqlite_storage.inspect db_path).row_count;
      assert_equal
        "storage addresses"
        "datascript/root,datascript/tail"
        (String.concat "," (storage_addresses storage));
      match restore (Sqlite_storage.storage db_path) with
      | None -> failwith "SQLite storage should restore the stored db"
      | Some restored ->
        let names = datoms restored Avet ~a:"name" () in
        if List.map (fun datom -> datom.e, datom.a, datom.v) names <> [ 1, "name", String "Ada" ] then
          failwith "SQLite storage should preserve stored datoms")

let default_logseq_graph_db =
  "/Users/tiensonqin/logseq/graphs/demo/db.sqlite"

let test_existing_logseq_graph_is_recognized_read_only () =
  if (not (sqlite3_available ())) || not (Sys.file_exists default_logseq_graph_db) then
    prerr_endline "Skipping Logseq graph inspection: sqlite3 or demo graph is unavailable"
  else
    let before = (Unix.stat default_logseq_graph_db).Unix.st_mtime in
    let summary = Sqlite_storage.inspect ~read_only:true default_logseq_graph_db in
    let after = (Unix.stat default_logseq_graph_db).Unix.st_mtime in
    if not summary.has_kvs_table then failwith "Logseq graph should contain a kvs table";
    if not summary.has_root then failwith "Logseq graph should contain addr 0 root metadata";
    if not summary.has_tail then failwith "Logseq graph should contain addr 1 tail";
    if summary.row_count <= 2 then failwith "Logseq graph should contain persisted index nodes";
    if summary.root_content_format <> Sqlite_storage.Logseq_transit then
      failwith "Logseq graph root should be recognized as Transit JSON";
    if not (List.mem "schema" summary.root_keys) then
      failwith "Logseq graph root should decode Transit metadata keys";
    if not (List.mem "max-eid" summary.root_keys) then
      failwith "Logseq graph root should expose max-eid metadata";
    if List.length summary.root_index_addresses <> 3 then
      failwith "Logseq graph root should expose eavt/aevt/avet addresses";
    if before <> after then failwith "read-only inspection should not modify the graph file"

let test_existing_logseq_graph_schema_supports_query_and_transact () =
  if (not (sqlite3_available ())) || not (Sys.file_exists default_logseq_graph_db) then
    prerr_endline "Skipping Logseq graph query/transact smoke: sqlite3 or demo graph is unavailable"
  else
    let before = (Unix.stat default_logseq_graph_db).Unix.st_mtime in
    let schema = Sqlite_storage.schema_of_logseq_graph ~read_only:true default_logseq_graph_db in
    let after = (Unix.stat default_logseq_graph_db).Unix.st_mtime in
    let block_name_schema =
      match List.assoc_opt "block/name" schema with
      | Some schema -> schema
      | None -> failwith "Logseq graph schema should expose :block/name"
    in
    if not block_name_schema.indexed then failwith ":block/name should be indexed in Logseq schema";
    let db = empty_db ~schema () in
    let report = transact db [ Add (Entity_id 1, "block/name", String "from-logseq-schema") ] in
    assert_equal_int
      "query synthetic datom with Logseq schema"
      1
      (List.length
         (q_string
            report.db_after
            "[:find ?e :where [?e :block/name \"from-logseq-schema\"]]"));
    if before <> after then failwith "read-only schema loading should not modify the graph file"

let test_existing_logseq_graph_datoms_support_query_and_transact () =
  if (not (sqlite3_available ())) || not (Sys.file_exists default_logseq_graph_db) then
    prerr_endline "Skipping Logseq graph datom query/transact smoke: sqlite3 or demo graph is unavailable"
  else
    let before = (Unix.stat default_logseq_graph_db).Unix.st_mtime in
    let schema = Sqlite_storage.schema_of_logseq_graph ~read_only:true default_logseq_graph_db in
    let datoms = Sqlite_storage.datoms_of_logseq_graph ~read_only:true ~limit:1 default_logseq_graph_db in
    let after = (Unix.stat default_logseq_graph_db).Unix.st_mtime in
    if not (List.exists (fun datom -> datom.e = 1 && datom.a = "block/name" && datom.v = String "root tag") datoms)
    then failwith "Logseq graph datoms should include the root tag page name";
    let db = init_db ~schema datoms in
    assert_equal_int
      "query decoded Logseq graph datom"
      1
      (List.length (q_string db "[:find ?e :where [?e :block/name \"root tag\"]]"));
    let report =
      transact db [ Add (Entity_id 9_999_999, "block/name", String "ocaml local graph smoke") ]
    in
    assert_equal_int
      "transact against decoded Logseq graph schema"
      1
      (List.length
         (q_string
            report.db_after
            "[:find ?e :where [?e :block/name \"ocaml local graph smoke\"]]"));
    if before <> after then failwith "read-only datom loading should not modify the graph file"

let () =
  Random.self_init ();
  test_sqlite_storage_round_trips_ocaml_payloads ();
  test_existing_logseq_graph_is_recognized_read_only ();
  test_existing_logseq_graph_schema_supports_query_and_transact ();
  test_existing_logseq_graph_datoms_support_query_and_transact ()
