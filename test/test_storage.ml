open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms db index ?e ?a ?v ?tx () =
  datoms_seq db index ?e ?a ?v ?tx () |> List.of_seq

let assert_lmdb_addresses label addresses =
  if addresses <> [ "lmdb" ] then
    failf "%s: expected LMDB storage address [lmdb], got [%s]" label (String.concat "," addresses)

let assert_equal_triples label expected actual =
  let actual = List.map (fun d -> d.e, d.a, d.v) actual in
  if expected <> actual then failf "%s: unexpected datoms" label

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

let small_db ?storage () =
  empty_db ?storage ()
  |> db_with
       [ Add (Entity_id 1, "name", String "Ivan")
       ; Add (Entity_id 2, "name", String "Oleg")
       ; Add (Entity_id 3, "name", String "Petr")
       ]

let test_storage__test_basics () =
  let storage = memory_storage () in
  let db = small_db () in
  store ~storage db;
  assert_lmdb_addresses "store writes LMDB storage address" (storage_addresses storage);
  (match restore storage with
   | None -> failwith "restore should read stored db"
   | Some restored ->
     assert_equal_triples
       "restore returns stored facts"
       [ 1, "name", String "Ivan"; 2, "name", String "Oleg"; 3, "name", String "Petr" ]
       (datoms restored Eavt ());
     if List.assoc_opt "storage" (settings restored) <> Some (Bool true) then
       failwith "settings should expose storage attachment");
  let attached_storage = memory_storage () in
  let attached = empty_db ~schema:[ "name", indexed ] ~storage:attached_storage () in
  store attached;
  (match restore attached_storage with
   | None -> failwith "store should use db-attached storage"
   | Some restored ->
     if schema restored <> [ "name", indexed ] then failwith "restore should preserve schema")

let test_storage__test_restored_db_addresses () =
  let storage = memory_storage () in
  let db = small_db () in
  store ~storage db;
  let restored =
    match restore storage with
    | Some db -> db
    | None -> failwith "restore should read stored db"
  in
  assert_lmdb_addresses "addresses should include restored db live nodes" (addresses [ restored ])

let test_storage__test_conn () =
  let storage = memory_storage () in
  let conn = create_conn ~schema:[ "name", indexed ] ~storage () in
  assert_lmdb_addresses "storage-backed create_conn stores LMDB address" (storage_addresses storage);
  ignore (transact_conn conn [ Add (Entity_id 1, "name", String "Ivan") ]);
  ignore (transact_conn conn [ Add (Entity_id 2, "name", String "Oleg") ]);
  let restored =
    match restore_conn storage with
    | Some conn -> conn
    | None -> failwith "restore_conn should restore storage-backed conn"
  in
  assert_equal_triples
    "restore_conn returns stored facts"
    [ 1, "name", String "Ivan"; 2, "name", String "Oleg" ]
    (datoms (conn_db restored) Eavt ());
  ignore (transact_conn ~tx_meta:[ "skip-store?", Bool true ] restored [ Add (Entity_id 3, "name", String "Skipped") ]);
  (match restore storage with
   | None -> failwith "storage root should remain available"
   | Some restored_db ->
     assert_equal_triples
       "skip-store transaction is not persisted"
       [ 1, "name", String "Ivan"; 2, "name", String "Oleg" ]
       (datoms restored_db Eavt ()))

let test_storage__test_multi_tx_incremental_store () =
  let storage = memory_storage () in
  let db =
    empty_db ~schema:[ "name", indexed; "age", indexed ] ()
    |> db_with [ Add (Entity_id 1, "name", String "Alice"); Add (Entity_id 1, "age", Int 30) ]
  in
  let tx1 = basis_tx db in
  store ~storage db;
  let db = db_with [ Add (Entity_id 1, "age", Int 31) ] db in
  store ~storage db;
  let restored =
    match restore storage with
    | Some db -> db
    | None -> failwith "restore should read incrementally stored db"
  in
  let current_ages =
    datoms restored Eavt ~a:"age" ()
    |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  in
  if current_ages <> [ 31 ] then failf "restored db should see current age 31, got %S" (string_of_int (List.hd current_ages));
  let past = as_of tx1 restored in
  let past_ages =
    datoms past Eavt ~a:"age" ()
    |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
  in
  if past_ages <> [ 30 ] then failf "restored as_of should see historical age 30";
  let hist_ages =
    datoms (history restored) Eavt ~a:"age" ()
    |> List.filter (fun d -> d.added)
    |> List.map (fun d -> match d.v with Int n -> n | _ -> -1)
    |> List.sort compare
  in
  if hist_ages <> [ 30; 31 ] then failf "restored history should expose both age assertions"

let () =
  test_storage__test_basics ();
  test_storage__test_restored_db_addresses ();
  test_storage__test_conn ();
  test_storage__test_multi_tx_incremental_store ()
