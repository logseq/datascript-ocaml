(* Seek / rseek / datoms parity across memory (LMDB temp), file LMDB, and SQLite.
   Hot paths must stay lazy-capable on every Share backend. *)

open Alcotest
open Datascript

let check_bool = Test_alcotest_support.check_bool

let temp_path name ext =
  let path = Filename.temp_file name ext in
  Sys.remove path;
  path

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

let many_indexed = { indexed with cardinality = Many }

let schema = [ "likes", many_indexed; "name", indexed; "age", indexed ]

(* Use string ages so Share index codecs (float-encoded numbers) do not change
   the observed value type vs bare memory LMDB temp. *)
let seed_ops =
  [ Add (Entity_id 1, "likes", String "fries")
  ; Add (Entity_id 1, "likes", String "pizza")
  ; Add (Entity_id 1, "name", String "Ivan")
  ; Add (Entity_id 2, "likes", String "pie")
  ; Add (Entity_id 2, "age", String "25")
  ; Add (Entity_id 3, "age", String "11")
  ; Add (Entity_id 3, "name", String "Sergey")
  ]

let triples seq = List.map (fun d -> d.e, d.a, d.v) (List.of_seq seq)

let expect_triples label expected actual =
  let got = triples actual in
  if expected <> got then
    let fmt rows =
      rows
      |> List.map (fun (e, a, v) ->
           Printf.sprintf
             "(%d,%s,%s)"
             e
             a
             (match v with
              | String s -> Printf.sprintf "%S" s
              | Int n -> string_of_int n
              | Float f -> string_of_float f
              | _ -> "?"))
      |> String.concat "; "
    in
    failwith (Printf.sprintf "%s: expected [%s], got [%s]" label (fmt expected) (fmt got))

let assert_scan_suite label db =
  expect_triples
    (label ^ " rseek eavt from likes/pizza")
    [ 1, "likes", String "pizza"; 1, "likes", String "fries" ]
    (rseek_datoms db Eavt ~e:1 ~a:"likes" ~v:(String "pizza") ());
  expect_triples
    (label ^ " seek eavt from likes/pizza")
    [ 1, "likes", String "pizza"
    ; 1, "name", String "Ivan"
    ; 2, "age", String "25"
    ; 2, "likes", String "pie"
    ; 3, "age", String "11"
    ; 3, "name", String "Sergey"
    ]
    (seek_datoms db Eavt ~e:1 ~a:"likes" ~v:(String "pizza") ());
  expect_triples
    (label ^ " datoms avet age exact")
    [ 3, "age", String "11" ]
    (datoms db Avet ~a:"age" ~v:(String "11") ());
  expect_triples
    (label ^ " rseek avet age attr")
    [ 2, "age", String "25"; 3, "age", String "11" ]
    (rseek_datoms db Avet ~a:"age" ());
  (match Seq.uncons (rseek_datoms db Avet ~a:"age" ()) with
   | Some (d, _) ->
       check_bool (label ^ " rseek age first is 25") true (d.e = 2 && d.v = String "25")
   | None -> failwith (label ^ " rseek age empty"));
  (match Seq.uncons (datoms db Eavt ~e:1 ()) with
   | Some (d, _) -> check_bool (label ^ " eavt entity first") true (d.e = 1)
   | None -> failwith (label ^ " eavt entity empty"))

let with_memory f =
  let db = db_with seed_ops (empty_db ~schema ()) in
  f "memory" db

let with_memory_storage f =
  let storage = memory_storage () in
  let db = db_with seed_ops (empty_db ~schema ~storage ()) in
  store db;
  match restore storage with
  | Some db -> f "memory_storage" db
  | None -> failwith "memory_storage restore failed"

let with_lmdb_file f =
  let path = temp_path "index-scan-lmdb" ".mdb" in
  let session = Datascript_lmdb.open_session path in
  Fun.protect
    ~finally:(fun () ->
      Datascript_lmdb.close session;
      if Sys.file_exists path then Sys.remove path;
      let lock = path ^ "-lock" in
      if Sys.file_exists lock then Sys.remove lock)
    (fun () ->
      let storage = storage_of_handle (Datascript_lmdb.storage session) in
      let db = db_with seed_ops (empty_db ~schema ~storage ()) in
      store db;
      match restore storage with
      | Some db -> f "lmdb_file" db
      | None -> failwith "lmdb restore failed")

let with_sqlite_file f =
  let path = temp_path "index-scan-sqlite" ".sqlite" in
  let session = Datascript_sqlite.open_session path in
  Fun.protect
    ~finally:(fun () ->
      Datascript_sqlite.close session;
      if Sys.file_exists path then Sys.remove path;
      List.iter
        (fun suffix ->
          let sibling = path ^ suffix in
          if Sys.file_exists sibling then Sys.remove sibling)
        [ "-wal"; "-shm" ])
    (fun () ->
      let storage = storage_of_handle (Datascript_sqlite.storage session) in
      let db = db_with seed_ops (empty_db ~schema ~storage ()) in
      store db;
      match restore storage with
      | Some db -> f "sqlite_file" db
      | None -> failwith "sqlite restore failed")

let test_memory () = with_memory assert_scan_suite
let test_memory_storage () = with_memory_storage assert_scan_suite
let test_lmdb_file () = with_lmdb_file assert_scan_suite
let test_sqlite_file () = with_sqlite_file assert_scan_suite

let () =
  run
    "index scan backends"
    [ ( "lazy seek/rseek/datoms"
      , [ test_case "memory (LMDB temp)" `Quick test_memory
        ; test_case "memory_storage" `Quick test_memory_storage
        ; test_case "lmdb file" `Quick test_lmdb_file
        ; test_case "sqlite file" `Quick test_sqlite_file
        ] )
    ]
