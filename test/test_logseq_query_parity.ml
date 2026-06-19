open Datascript

module Sqlite_storage = Logseq_sqlite_storage

let failf fmt = Printf.ksprintf failwith fmt

let with_sqlite db_path f =
  let db = Sqlite3.db_open db_path in
  Fun.protect
    ~finally:(fun () ->
      if not (Sqlite3.db_close db) then failf "failed to close SQLite database: %s" db_path)
    (fun () -> f db)

let check_sql db sql rc =
  if not (Sqlite3.Rc.is_success rc) then
    failf "SQLite statement failed with %s for %S: %s" (Sqlite3.Rc.to_string rc) sql (Sqlite3.errmsg db)

let run_sql db_path sql =
  with_sqlite db_path (fun db -> check_sql db sql (Sqlite3.exec db sql))

let sql_quote text =
  "'" ^ String.concat "''" (String.split_on_char '\'' text) ^ "'"

let with_temp_db f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      ("datascript_ocaml_logseq_query_parity_" ^ string_of_int (Random.bits ()))
  in
  Unix.mkdir dir 0o755;
  let db_path = Filename.concat dir "db.sqlite" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists db_path then Sys.remove db_path;
      if Sys.file_exists dir then Unix.rmdir dir)
    (fun () -> f db_path)

let int_collection = function
  | Query_collection values ->
    values
    |> List.map (function
      | Result_entity entity_id -> entity_id
      | _ -> failwith "expected entity result")
    |> List.sort compare
  | _ -> failwith "expected collection result"

let assert_equal_ints label expected actual =
  let actual = List.sort compare actual in
  if expected <> actual then
    failf
      "%s: expected [%s], got [%s]"
      label
      (expected |> List.map string_of_int |> String.concat "; ")
      (actual |> List.map string_of_int |> String.concat "; ")

let pulled_attr attr entity =
  List.assoc_opt (Keyword attr) entity.pulled_attrs

let test_attr_filtered_query_preserves_transit_shorthand_segment () =
  with_temp_db (fun db_path ->
    let root_content =
      {|["^ ","~:schema",["^ ","~:db/ident",["^ ","~:db/unique","~:db.unique/identity","~:db/index",true]]]|}
    in
    let ident_row =
      {|["^ ","~:keys",[[1,"~:db/ident","~:alpha",536870913]]]|}
    in
    let shorthand_ident_row =
      {|["^ ","^0",[[2,"^1","~:beta",536870913]]]|}
    in
    let unrelated_broken_row = {|["^ ","~:keys",|} in
    run_sql
      db_path
      ("create table kvs (addr INTEGER primary key, content TEXT, addresses JSON);\n"
       ^ "insert into kvs (addr, content, addresses) values (0, "
       ^ sql_quote root_content
       ^ ", '[]');\n"
       ^ "insert into kvs (addr, content, addresses) values (2, "
       ^ sql_quote ident_row
       ^ ", '[]');\n"
       ^ "insert into kvs (addr, content, addresses) values (3, "
       ^ sql_quote shorthand_ident_row
       ^ ", '[]');\n"
       ^ "insert into kvs (addr, content, addresses) values (4, "
       ^ sql_quote unrelated_broken_row
       ^ ", '[]');");
    Sqlite_storage.query_logseq_graph
      ~read_only:true
      db_path
      "[:find [?e ...] :where [?e :db/ident]]"
    |> int_collection
    |> assert_equal_ints "Logseq query slicer should decode shorthand rows in a matching Transit segment" [ 1; 2 ])

let test_attr_filtered_query_keeps_idents_for_keyword_ref_constants () =
  with_temp_db (fun db_path ->
    let root_content =
      {|["^ ","~:schema",["^ ","~:db/ident",["^ ","~:db/unique","~:db.unique/identity","~:db/index",true],"~:block/tags",["^ ","~:db/valueType","~:db.type/ref"]]]|}
    in
    let graph_row =
      {|["^ ","~:keys",[[10,"~:block/tags",20,536870913],[20,"~:db/ident","~:logseq.class/Journal",536870913]]]|}
    in
    run_sql
      db_path
      ("create table kvs (addr INTEGER primary key, content TEXT, addresses JSON);\n"
       ^ "insert into kvs (addr, content, addresses) values (0, "
       ^ sql_quote root_content
       ^ ", '[]');\n"
       ^ "insert into kvs (addr, content, addresses) values (2, "
       ^ sql_quote graph_row
       ^ ", '[]');");
    Sqlite_storage.query_logseq_graph
      ~read_only:true
      db_path
      "[:find [?e ...] :where [?e :block/tags :logseq.class/Journal]]"
    |> int_collection
    |> assert_equal_ints "Logseq query slicer should keep :db/ident datoms for keyword ref constants" [ 10 ])

let test_attr_filtered_query_keeps_pull_selector_attrs () =
  with_temp_db (fun db_path ->
    let root_content =
      {|["^ ","~:schema",["^ ","~:db/ident",["^ ","~:db/unique","~:db.unique/identity","~:db/index",true]]]|}
    in
    let graph_row =
      {|["^ ","~:keys",[[10,"~:file/path","logseq/config.edn",536870913],[10,"~:file/content","{:feature/markdown-mirror? true}",536870913]]]|}
    in
    run_sql
      db_path
      ("create table kvs (addr INTEGER primary key, content TEXT, addresses JSON);\n"
       ^ "insert into kvs (addr, content, addresses) values (0, "
       ^ sql_quote root_content
       ^ ", '[]');\n"
       ^ "insert into kvs (addr, content, addresses) values (2, "
       ^ sql_quote graph_row
       ^ ", '[]');");
    match
      Sqlite_storage.query_logseq_graph
        ~read_only:true
        db_path
        "[:find [(pull ?e [:file/path :file/content]) ...] :where [?e :file/path]]"
    with
    | Query_collection [ Result_pull entity ] ->
      (match pulled_attr "file/content" entity with
       | Some (Pulled_scalar (String "{:feature/markdown-mirror? true}")) -> ()
       | _ -> failwith "Logseq query slicer should keep pull selector attrs")
    | _ -> failwith "expected one pulled entity")

let () =
  Random.self_init ();
  test_attr_filtered_query_preserves_transit_shorthand_segment ();
  test_attr_filtered_query_keeps_idents_for_keyword_ref_constants ();
  test_attr_filtered_query_keeps_pull_selector_attrs ()
