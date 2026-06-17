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
    let count = List.length (datoms restored Eavt ()) in
    Printf.printf "stored and restored %d datom(s)\n" count

let usage () =
  prerr_endline "Usage:";
  prerr_endline "  sqlite_storage_example inspect <db.sqlite>";
  prerr_endline "  sqlite_storage_example roundtrip <db.sqlite>";
  exit 2

let () =
  match Array.to_list Sys.argv with
  | [ _; "inspect"; db_path ] ->
    Storage.inspect ~read_only:true db_path |> print_summary db_path
  | [ _; "roundtrip"; db_path ] -> run_roundtrip db_path
  | _ -> usage ()
