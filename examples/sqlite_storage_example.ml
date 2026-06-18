open Datascript

module Storage = Logseq_sqlite_storage

let format_content_format = function
  | Storage.Ocaml_marshal -> "ocaml-marshal"
  | Storage.Logseq_transit -> "logseq-transit"
  | Storage.Empty -> "empty"
  | Storage.Unknown -> "unknown"

let print_summary db_path summary =
  Printf.printf "db: %s\n" db_path;
  Printf.printf "kvs table: %b\n" summary.Storage.has_kvs_table;
  Printf.printf "rows: %d\n" summary.Storage.row_count;
  Printf.printf "root addr 0: %b\n" summary.Storage.has_root;
  Printf.printf "tail addr 1: %b\n" summary.Storage.has_tail;
  Printf.printf
    "root content format: %s\n"
    (format_content_format summary.Storage.root_content_format);
  Printf.printf "root keys: %s\n" (String.concat "," summary.Storage.root_keys);
  Printf.printf
    "root index addresses: %s\n"
    (summary.Storage.root_index_addresses
     |> List.map string_of_int
     |> String.concat ",")

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

let run_roundtrip db_path =
  let storage = Storage.storage db_path in
  let db =
    init_db
      ~schema:[ "name", indexed ]
      [ datom ~e:1 ~a:"name" ~v:(String "SQLite example") () ]
  in
  store ~storage db;
  match restore (Storage.storage db_path) with
  | None -> failwith "failed to restore SQLite-backed db"
  | Some restored ->
    let count = Seq.fold_left (fun count _ -> count + 1) 0 (datoms restored Eavt ()) in
    Printf.printf "stored and restored %d datom(s)\n" count

let inspect_graphs graphs_dir =
  match Storage.graph_db_paths graphs_dir with
  | [] -> Printf.printf "no Logseq db.sqlite files found in %s\n" graphs_dir
  | db_paths ->
    List.iter
      (fun db_path ->
        Storage.inspect ~read_only:true db_path |> print_summary db_path;
        print_endline "")
      db_paths

let rec edn_of_pulled_value = function
  | Pulled_scalar value -> Built_ins.print_query_value ~readably:true value
  | Pulled_many values -> "[" ^ String.concat " " (List.map edn_of_pulled_value values) ^ "]"
  | Pulled_entity entity -> edn_of_pulled_entity entity

and edn_of_pulled_entity entity =
  let attrs =
    (Keyword "db/id", Pulled_scalar (Int entity.pulled_id)) :: entity.pulled_attrs
    |> List.sort (fun (left, _) (right, _) -> compare left right)
    |> List.map (fun (key, value) ->
      Built_ins.print_query_value ~readably:true key ^ " " ^ edn_of_pulled_value value)
  in
  "{" ^ String.concat " " attrs ^ "}"

let edn_of_query_result = function
  | Result_entity entity_id -> string_of_int entity_id
  | Result_attr attr -> ":" ^ attr
  | Result_value value -> Built_ins.print_query_value ~readably:true value
  | Result_db _ -> "#datascript/DB"
  | Result_pull entity -> edn_of_pulled_entity entity

let edn_list values = "[" ^ String.concat " " values ^ "]"

let edn_of_result_row row =
  edn_list (List.map edn_of_query_result row)

let edn_of_query_output = function
  | Query_relation rows -> edn_list (List.map edn_of_result_row rows)
  | Query_collection values ->
    edn_list (List.map edn_of_query_result values)
  | Query_tuple None -> "nil"
  | Query_tuple (Some row) -> edn_of_result_row row
  | Query_scalar None -> "nil"
  | Query_scalar (Some value) -> edn_of_query_result value
  | Query_relation_maps rows ->
    rows
    |> List.map (fun row ->
      row
      |> List.map (fun (key, value) ->
        Built_ins.print_query_value ~readably:true key ^ " " ^ edn_of_query_result value)
      |> String.concat " "
      |> fun body -> "{" ^ body ^ "}")
    |> edn_list
  | Query_tuple_map None -> "nil"
  | Query_tuple_map (Some row) ->
    row
    |> List.map (fun (key, value) ->
      Built_ins.print_query_value ~readably:true key ^ " " ^ edn_of_query_result value)
    |> String.concat " "
    |> fun body -> "{" ^ body ^ "}"

let run_query db_path query =
  let schema = Storage.schema_of_logseq_graph ~read_only:true db_path in
  let graph_datoms = Storage.datoms_of_logseq_graph ~read_only:true db_path in
  let db = init_db ~schema graph_datoms in
  q_return_string db query |> edn_of_query_output |> print_endline

let usage () =
  prerr_endline "Usage:";
  prerr_endline "  sqlite_storage_example inspect <db.sqlite>";
  prerr_endline "  sqlite_storage_example inspect-graphs <graphs-dir>";
  prerr_endline "  sqlite_storage_example query <db.sqlite> <query>";
  prerr_endline "  sqlite_storage_example roundtrip <db.sqlite>";
  exit 2

let () =
  match Array.to_list Sys.argv with
  | [ _; "inspect"; db_path ] ->
    Storage.inspect ~read_only:true db_path |> print_summary db_path
  | [ _; "inspect-graphs"; graphs_dir ] -> inspect_graphs graphs_dir
  | [ _; "query"; db_path; query ] -> run_query db_path query
  | [ _; "roundtrip"; db_path ] -> run_roundtrip db_path
  | _ -> usage ()
