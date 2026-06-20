open Datascript

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms db index ?e ?a ?v ?tx () =
  datoms_seq db index ?e ?a ?v ?tx () |> List.of_seq

let assert_equal_int label expected actual =
  if expected <> actual then failf "%s: expected %d, got %d" label expected actual

let assert_int_at_most label limit actual =
  if actual > limit then failf "%s: expected at most %d, got %d" label limit actual

let assert_upstream_storage_addresses label addresses =
  if List.mem "datascript/root" addresses || List.mem "datascript/tail" addresses then
    failf "%s: storage should not use OCaml snapshot address names" label;
  if not (List.mem "0" addresses) then failf "%s: storage should include upstream root address 0" label;
  if not (List.mem "1" addresses) then failf "%s: storage should include upstream tail address 1" label;
  if List.length addresses < 5 then
    failf
      "%s: storage should include root, tail, and separate index nodes, got [%s]"
      label
      (String.concat "," addresses)

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

let unique_identity = { indexed with unique = Some Identity }

let remove_dir_if_exists dir =
  if Sys.file_exists dir then begin
    Sys.readdir dir
    |> Array.iter (fun name -> Sys.remove (Filename.concat dir name));
    Unix.rmdir dir
  end

let small_db ?storage () =
  empty_db ?storage ()
  |> db_with
       [ Add (Entity_id 1, "name", String "Ivan")
       ; Add (Entity_id 2, "name", String "Oleg")
       ; Add (Entity_id 3, "name", String "Petr")
       ]

let large_db ?storage () =
  empty_db ?storage ()
  |> db_with
       (List.init 1000 (fun index ->
          let entity_id = index + 1 in
          Add (Entity_id entity_id, "str", String (string_of_int entity_id))))

let counting_storage () =
  let storage = memory_storage () in
  let writes = ref [] in
  let storage_store entries =
    writes := !writes @ List.map fst entries;
    storage.storage_store entries
  in
  { storage with storage_store }, writes

let reset_writes writes = writes := []

let test_storage__test_basics () =
  let storage = memory_storage () in
  let db = small_db () in
  store ~storage db;
  assert_upstream_storage_addresses "store writes upstream storage addresses" (storage_addresses storage);
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

let test_storage__test_upstream_wire_addresses () =
  let storage = memory_storage () in
  let db = small_db () in
  store ~storage db;
  let addresses = storage_addresses storage in
  if List.mem "datascript/root" addresses || List.mem "datascript/tail" addresses then
    failwith "storage should not use OCaml snapshot address names";
  (match storage.storage_restore "0", storage.storage_restore "1" with
   | Some _, Some (Storage_tail []) -> ()
   | None, _ -> failwith "storage should write upstream root address 0"
   | _, None -> failwith "storage should write upstream tail address 1"
   | _, Some _ -> failwith "storage tail address should contain the transaction tail");
  if List.length addresses < 5 then
    failf
      "storage should write root, tail, and separate index nodes, got [%s]"
      (String.concat "," addresses)

let test_storage__test_file_storage () =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      ("datascript_ocaml_storage_" ^ string_of_int (Random.bits ()))
  in
  remove_dir_if_exists dir;
  Fun.protect
    ~finally:(fun () -> remove_dir_if_exists dir)
    (fun () ->
      let storage = file_storage dir in
      let db = small_db () in
      store ~storage db;
      store_tail storage [ [ datom ~tx:(tx0 + 2) ~e:1 ~a:"name" ~v:(String "Alex") () ] ];
      let restored_storage = file_storage dir in
      assert_upstream_storage_addresses "file_storage lists persisted addresses" (storage_addresses restored_storage);
      match restore restored_storage with
      | None -> failwith "file_storage should restore stored db"
      | Some restored ->
        assert_equal_triples
          "file_storage restores root and replays persisted tail"
          [ 1, "name", String "Alex"; 2, "name", String "Oleg"; 3, "name", String "Petr" ]
          (datoms restored Eavt ()))

let test_storage__test_gc () =
  let storage = memory_storage () in
  let db = small_db () in
  store ~storage db;
  store_tail storage [ [ datom ~tx:(tx0 + 2) ~e:1 ~a:"name" ~v:(String "Alex") () ] ];
  storage.storage_store [ "stale/node", Storage_tail [] ];
  collect_garbage storage;
  assert_upstream_storage_addresses "collect_garbage keeps live storage addresses" (storage_addresses storage);
  match restore storage with
  | None -> failwith "restore should work after garbage collection"
  | Some restored ->
    assert_equal_triples
      "collect_garbage preserves restorable data"
      [ 1, "name", String "Alex"; 2, "name", String "Oleg"; 3, "name", String "Petr" ]
      (datoms restored Eavt ())

let test_storage__test_restored_db_addresses () =
  let storage = memory_storage () in
  let db = small_db () in
  store ~storage db;
  let restored =
    match restore storage with
    | Some db -> db
    | None -> failwith "restore should read stored db"
  in
  assert_upstream_storage_addresses "addresses should include restored db live nodes" (addresses [ restored ])

let test_storage__test_restored_incremental_store_reuses_index_nodes () =
  let storage, writes = counting_storage () in
  let db = large_db () in
  store ~storage db;
  let restored =
    match restore storage with
    | Some db -> db
    | None -> failwith "restore should read stored large db"
  in
  reset_writes writes;
  store ~storage restored;
  assert_int_at_most
    "storing an unchanged restored db should not rewrite index nodes"
    2
    (List.length !writes);
  reset_writes writes;
  let db_after =
    db_with [ Add (Entity_id 1001, "str", String "1001") ] restored
  in
  store ~storage db_after;
  assert_int_at_most
    "storing an incrementally changed restored db should write only changed index paths"
    8
    (List.length !writes);
  assert_equal_triples
    "incremental stored db remains restorable"
    [ 1001, "str", String "1001" ]
    (datoms db_after Eavt ~e:1001 ());
  reset_writes writes;
  let db_after_replacement =
    db_with [ Add (Entity_id 1, "str", String "changed") ] restored
  in
  store ~storage db_after_replacement;
  assert_int_at_most
    "storing a cardinality-one replacement should write only changed index paths"
    16
    (List.length !writes);
  assert_equal_triples
    "replacement stored db remains restorable"
    [ 1, "str", String "changed" ]
    (datoms db_after_replacement Eavt ~e:1 ())

let test_storage__test_conn () =
  let storage = memory_storage () in
  let conn = create_conn ~schema:[ "name", indexed ] ~storage () in
  assert_upstream_storage_addresses "storage-backed create_conn stores upstream addresses" (storage_addresses storage);
  ignore (transact_conn conn [ Add (Entity_id 1, "name", String "Ivan") ]);
  ignore (transact_conn conn [ Add (Entity_id 2, "name", String "Oleg") ]);
  let restored =
    match restore_conn storage with
    | Some conn -> conn
    | None -> failwith "restore_conn should restore storage-backed conn"
  in
  assert_equal_triples
    "restore_conn replays transaction tail"
    [ 1, "name", String "Ivan"; 2, "name", String "Oleg" ]
    (datoms (conn_db restored) Eavt ());
  ignore (transact_conn ~tx_meta:[ "skip-store?", Bool true ] restored [ Add (Entity_id 3, "name", String "Skipped") ]);
  (match restore storage with
   | None -> failwith "storage root should remain available"
   | Some restored_db ->
     assert_equal_triples
       "skip-store transaction is not persisted"
       [ 1, "name", String "Ivan"; 2, "name", String "Oleg" ]
       (datoms restored_db Eavt ()));
  ignore
    (transact_conn
       restored
       (List.init 34 (fun index ->
          let entity_id = index + 4 in
          Add (Entity_id entity_id, "name", String (string_of_int entity_id)))));
  (match storage.storage_restore "1" with
   | Some (Storage_tail []) -> ()
   | _ -> failwith "overflowing storage-backed conn tail should compact");
  let from_db_storage = memory_storage () in
  let from_db =
    empty_db ~schema:[ "name", indexed ] ~storage:from_db_storage ()
    |> db_with [ Add (Entity_id 1, "name", String "Ivan") ]
  in
  ignore (conn_from_db from_db);
  (match restore from_db_storage with
   | Some restored_db ->
     assert_equal_triples
       "conn_from_db stores the initial attached db root"
       [ 1, "name", String "Ivan" ]
       (datoms restored_db Eavt ())
   | None -> failwith "conn_from_db should store attached dbs");
  let from_datoms_storage = memory_storage () in
  ignore
    (conn_from_datoms
       ~schema:[ "name", indexed ]
       ~storage:from_datoms_storage
       [ datom ~e:3 ~a:"name" ~v:(String "Petr") () ]);
  match restore from_datoms_storage with
  | Some restored_db ->
    assert_equal_triples
      "conn_from_datoms stores the initial attached db root"
      [ 3, "name", String "Petr" ]
      (datoms restored_db Eavt ())
  | None -> failwith "conn_from_datoms should store attached datoms"

let test_storage__test_db_with_tail () =
  let db =
    empty_db ~schema:[ "block/updated-at", indexed; "block/uuid", unique_identity ] ()
    |> db_with [ Add (Entity_id 1, "block/updated-at", Int 2); Add (Entity_id 1, "block/uuid", String "u1") ]
  in
  let tail =
    [ [ datom ~tx:(tx0 + 3) ~e:1 ~a:"block/updated-at" ~v:(Int 1772979060646) () ]
    ; [ datom ~tx:(tx0 + 4) ~e:1 ~a:"block/updated-at" ~v:(Int 1772979061145) () ]
    ; [ datom ~tx:(tx0 + 5) ~e:2 ~a:"block/uuid" ~v:(String "u1") ()
      ; datom ~tx:(tx0 + 5) ~e:2 ~a:"block/title" ~v:(String "Rejected") ()
      ]
    ; [ datom ~tx:(tx0 + 6) ~e:3 ~a:"block/title" ~v:(String "Later") () ]
    ]
  in
  let restored = db_with_tail db tail in
  assert_equal_triples
    "db_with_tail retracts stale cardinality-one values"
    [ 1, "block/updated-at", Int 1772979061145 ]
    (datoms restored Avet ~a:"block/updated-at" ());
  assert_equal_triples
    "db_with_tail drops rejected unique-conflict tail groups"
    []
    (datoms restored Eavt ~e:2 ());
  assert_equal_triples
    "db_with_tail keeps later valid groups"
    [ 3, "block/title", String "Later" ]
    (datoms restored Eavt ~e:3 ());
  assert_equal_int "db_with_tail advances max tx" (tx0 + 6) restored.max_tx

let () =
  test_storage__test_basics ();
  test_storage__test_upstream_wire_addresses ();
  test_storage__test_file_storage ();
  test_storage__test_gc ();
  test_storage__test_restored_db_addresses ();
  test_storage__test_restored_incremental_store_reuses_index_nodes ();
  test_storage__test_conn ();
  test_storage__test_db_with_tail ()
