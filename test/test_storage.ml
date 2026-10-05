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

let restore_counting_storage storage =
  let reads = ref [] in
  let storage_restore address =
    reads := address :: !reads;
    storage.storage_restore address
  in
  { storage with storage_restore }, reads

let reset_writes writes = writes := []

let test_storage__test_basics () =
  let storage = memory_storage () in
  let db = small_db () in
  ignore (store ~storage db);
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
  ignore (store attached);
  (match restore attached_storage with
   | None -> failwith "store should use db-attached storage"
   | Some restored ->
     if schema restored <> [ "name", indexed ] then failwith "restore should preserve schema")

let test_storage__test_upstream_wire_addresses () =
  let storage = memory_storage () in
  let db = small_db () in
  ignore (store ~storage db);
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
      ignore (store ~storage db);
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
  ignore (store ~storage db);
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
  ignore (store ~storage db);
  let restored =
    match restore storage with
    | Some db -> db
    | None -> failwith "restore should read stored db"
  in
  assert_upstream_storage_addresses "addresses should include restored db live nodes" (addresses [ restored ])

let test_storage__test_restored_incremental_store_reuses_index_nodes () =
  let storage, writes = counting_storage () in
  let db = large_db () in
  ignore (store ~storage db);
  let restored =
    match restore storage with
    | Some db -> db
    | None -> failwith "restore should read stored large db"
  in
  reset_writes writes;
  ignore (store ~storage restored);
  assert_int_at_most
    "storing an unchanged restored db should not rewrite index nodes"
    2
    (List.length !writes);
  reset_writes writes;
  let db_after =
    db_with [ Add (Entity_id 1001, "str", String "1001") ] restored
  in
  ignore (store ~storage db_after);
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
  ignore (store ~storage db_after_replacement);
  assert_int_at_most
    "storing a cardinality-one replacement should write only changed index paths"
    16
    (List.length !writes);
  assert_equal_triples
    "replacement stored db remains restorable"
    [ 1, "str", String "changed" ]
    (datoms db_after_replacement Eavt ~e:1 ())

let test_storage__test_conn_repeated_transacts_store_incrementally () =
  let storage, writes = counting_storage () in
  let conn = create_conn ~schema:[ "name", indexed ] ~storage () in
  for i = 1 to 300 do
    ignore
      (transact_conn conn
         [ Add (Entity_id i, "name", String (string_of_int i)) ])
  done;
  if List.length !writes = 0 then
    failwith "storage-backed conn should compact its tail into index nodes";
  (* each compaction should only write the index nodes created since the
     previous store; rewriting the whole tree on every store made total
     writes grow quadratically with the number of transactions. At 300
     transactions the buggy code writes well over 3000 entries while the
     incremental store stays under 1000. *)
  assert_int_at_most
    "repeated transacts should not rewrite the whole index tree on every store"
    1500
    (List.length !writes);
  (match restore storage with
   | None -> failwith "storage-backed conn should restore"
   | Some restored ->
     assert_equal_int
       "all committed datoms remain restorable"
       300
       (List.length (datoms restored Eavt ())))

let test_storage__test_restore_is_lazy () =
  let storage = memory_storage () in
  large_db () |> store ~storage |> ignore;
  let address_count = List.length (storage_addresses storage) in
  if address_count < 20 then
    failf "large stored db should have many index nodes, got %d" address_count;
  let counted_storage, reads = restore_counting_storage storage in
  let restored =
    match restore counted_storage with
    | Some db -> db
    | None -> failwith "restore should read stored large db"
  in
  assert_int_at_most "restore should only read root and tail addresses" 2
    (List.length !reads);
  ignore (Seq.uncons (datoms_seq restored Eavt ()));
  let reads_after_first_datom = List.length !reads in
  if reads_after_first_datom <= 2 then
    failwith "reading the first datom should load the first index path";
  if reads_after_first_datom >= address_count then
    failf
      "reading the first datom should not restore every stored node: reads=%d addresses=%d"
      reads_after_first_datom address_count

let test_storage__test_restore_with_tail_is_lazy () =
  let storage = memory_storage () in
  large_db () |> store ~storage |> ignore;
  let address_count = List.length (storage_addresses storage) in
  store_tail storage
    [
      [
        datom ~tx:(tx0 + 2) ~e:1 ~a:"str" ~v:(String "1") ~added:false ();
        datom ~tx:(tx0 + 2) ~e:1 ~a:"str" ~v:(String "changed") ();
      ];
    ];
  let counted_storage, reads = restore_counting_storage storage in
  let restored =
    match restore counted_storage with
    | Some db -> db
    | None -> failwith "restore should read stored large db with tail"
  in
  if List.length !reads >= address_count then
    failf
      "restore tail replay should not restore every stored node: reads=%d addresses=%d"
      (List.length !reads) address_count;
  assert_equal_triples
    "tail replay should apply raw datoms"
    [ 1, "str", String "changed" ]
    (datoms restored Eavt ~e:1 ())

let test_storage__test_transact_after_restore_uses_index_slices () =
  let storage = memory_storage () in
  large_db () |> store ~storage |> ignore;
  let baseline_storage, baseline_reads = restore_counting_storage storage in
  let baseline =
    match restore baseline_storage with
    | Some db -> db
    | None -> failwith "restore should read stored large db for baseline"
  in
  ignore (Seq.uncons (datoms_seq baseline Eavt ~e:1 ()));
  let slice_read_count = List.length !baseline_reads in
  let counted_storage, reads = restore_counting_storage storage in
  let restored =
    match restore counted_storage with
    | Some db -> db
    | None -> failwith "restore should read stored large db"
  in
  let db_after =
    db_with [ Retract (Entity_id 1, "str", Some (String "1")) ] restored
  in
  if List.length !reads > slice_read_count + 8 then
    failf
      "transact after restore should use bounded index slices: reads=%d slice_reads=%d"
      (List.length !reads) slice_read_count;
  assert_equal_triples
    "restored db transaction should retract the targeted fact"
    []
    (datoms db_after Eavt ~e:1 ())

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

let tail_groups storage =
  match storage.storage_restore Storage.tail_address with
  | Some (Storage_tail groups) -> groups
  | _ -> failwith "storage tail address missing"

let flatten_tail_groups groups =
  List.map (fun group -> List.map (fun d -> d.e, d.a, d.v) group) groups

let test_storage__test_conn_tail_write_order () =
  let storage = memory_storage () in
  let conn = create_conn ~storage () in
  let assert_tail label conn expected =
    let stored = flatten_tail_groups (tail_groups storage) in
    if stored <> expected then
      failf "%s: stored tail order mismatch: %s" label
        (String.concat ";" (List.map (fun g -> string_of_int (List.length g)) stored));
    if flatten_tail_groups (Conn.storage_tail conn) <> expected then
      failwith (label ^ ": conn storage_tail accessor order mismatch")
  in
  ignore (transact_conn conn [ Add (Entity_id 1, "a", String "1") ]);
  ignore
    (transact_conn conn
       [ Add (Entity_id 2, "a", String "2"); Add (Entity_id 2, "b", String "3") ]);
  assert_tail "two txs" conn
    [ [ 1, "a", String "1" ]
    ; [ 2, "a", String "2"; 2, "b", String "3" ]
    ];
  (* a restored conn seeds its tail from disk and keeps appending in tx
     order; the on-disk order must survive the internal reversal *)
  let conn =
    match restore_conn storage with
    | Some conn -> conn
    | None -> failwith "restore_conn should return a conn"
  in
  ignore (transact_conn conn [ Add (Entity_id 3, "a", String "4") ]);
  assert_tail "restored conn" conn
    [ [ 1, "a", String "1" ]
    ; [ 2, "a", String "2"; 2, "b", String "3" ]
    ; [ 3, "a", String "4" ]
    ];
  (* crossing the compaction threshold clears the stored tail *)
  ignore
    (transact_conn conn
       (List.init 40 (fun i -> Add (Entity_id (i + 10), "a", String (string_of_int i)))));
  (match tail_groups storage with
   | [] -> ()
   | _ -> failwith "compaction should clear the stored tail");
  ignore (transact_conn conn [ Add (Entity_id 99, "a", String "9") ]);
  assert_tail "post-compaction" conn [ [ 99, "a", String "9" ] ];
  assert_equal_triples
    "all datoms queryable after compaction"
    [ 1, "a", String "1"; 2, "a", String "2"; 2, "b", String "3"
    ; 3, "a", String "4"; 99, "a", String "9" ]
    (datoms (conn_db conn) Eavt ~e:1 ()
     @ datoms (conn_db conn) Eavt ~e:2 ()
     @ datoms (conn_db conn) Eavt ~e:3 ()
     @ datoms (conn_db conn) Eavt ~e:99 ()
     |> List.sort compare)

let test_storage__test_tail_datom_count () =
  let d tx e = datom ~tx ~e ~a:"a" ~v:(String "v") () in
  assert_equal_int "tail_datom_count sums group sizes" 4
    (Storage.tail_datom_count [ [ d 1 1 ]; [ d 2 2; d 2 3; d 2 4 ]; [] ])

let test_storage__test_auto_gc () =
  let batches_of_txs conn tx_count batch_count =
    let entity = ref 0 in
    for _ = 1 to batch_count do
      ignore
        (transact_conn conn
           (List.init tx_count (fun _ ->
              incr entity;
              Add (Entity_id !entity, "name", String (string_of_int !entity)))))
    done
  in
  (* 35 > branching factor 32: every batch compacts, leaving orphaned index
     nodes behind. Default storage keeps them; an always-tripping policy
     collects after each compaction. *)
  let manual_storage = memory_storage () in
  let manual_conn = create_conn ~schema:[ "name", indexed ] ~storage:manual_storage () in
  batches_of_txs manual_conn 35 4;
  let auto_storage = memory_storage ~auto_gc:{ min_garbage = 1; garbage_fraction = 0.0 } () in
  let auto_conn = create_conn ~schema:[ "name", indexed ] ~storage:auto_storage () in
  batches_of_txs auto_conn 35 4;
  let auto_live = List.length (addresses [ conn_db auto_conn ]) in
  if List.length (storage_addresses auto_storage) <> auto_live then
    failf
      "auto-GC should leave only reachable addresses: stored=%d live=%d"
      (List.length (storage_addresses auto_storage))
      auto_live;
  if not (List.length (storage_addresses manual_storage) > auto_live) then
    failwith "manual storage should accumulate unreachable addresses";
  (* min_garbage is a floor: a large one keeps the storage manual *)
  let floored_storage =
    memory_storage ~auto_gc:{ min_garbage = 1_000_000; garbage_fraction = 0.0 } ()
  in
  let floored_conn = create_conn ~schema:[ "name", indexed ] ~storage:floored_storage () in
  batches_of_txs floored_conn 35 4;
  assert_equal_int
    "unreachable floor keeps auto-GC from deleting"
    (List.length (storage_addresses manual_storage))
    (List.length (storage_addresses floored_storage));
  (* maybe_collect_garbage is a no-op without a policy, and honours one *)
  let probe = memory_storage () in
  ignore (store ~storage:probe (small_db ()));
  probe.storage_store [ "stale/addr", Storage_tail [] ];
  maybe_collect_garbage probe;
  if not (List.mem "stale/addr" (storage_addresses probe)) then
    failwith "maybe_collect_garbage should be a no-op without a policy";
  auto_collect_garbage { min_garbage = 1; garbage_fraction = 0.0 } probe;
  if List.mem "stale/addr" (storage_addresses probe) then
    failwith "auto_collect_garbage should delete unreachable addresses";
  let opted = memory_storage ~auto_gc:{ min_garbage = 1; garbage_fraction = 0.0 } () in
  ignore (store ~storage:opted (small_db ()));
  opted.storage_store [ "stale/addr", Storage_tail [] ];
  maybe_collect_garbage opted;
  if List.mem "stale/addr" (storage_addresses opted) then
    failwith "maybe_collect_garbage should honour storage_auto_gc";
  if default_auto_gc.min_garbage <> 1024 || default_auto_gc.garbage_fraction <> 0.5 then
    failwith "default_auto_gc should keep the documented policy"

let test_storage__test_db_with_tail () =
  let db =
    empty_db ~schema:[ "block/updated-at", indexed; "block/uuid", unique_identity ] ()
    |> db_with [ Add (Entity_id 1, "block/updated-at", Int64 2L); Add (Entity_id 1, "block/uuid", String "u1") ]
  in
  let tail =
    [ [ datom ~tx:(tx0 + 3) ~e:1 ~a:"block/updated-at" ~v:(Int64 1772979060646L) () ]
    ; [ datom ~tx:(tx0 + 4) ~e:1 ~a:"block/updated-at" ~v:(Int64 1772979061145L) () ]
    ; [ datom ~tx:(tx0 + 5) ~e:2 ~a:"block/uuid" ~v:(String "u1") ()
      ; datom ~tx:(tx0 + 5) ~e:2 ~a:"block/title" ~v:(String "Rejected") ()
      ]
    ; [ datom ~tx:(tx0 + 6) ~e:3 ~a:"block/title" ~v:(String "Later") () ]
    ]
  in
  let restored = db_with_tail db tail in
  assert_equal_triples
    "db_with_tail retracts stale cardinality-one values"
    [ 1, "block/updated-at", Int64 1772979061145L ]
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

module PSet = Persistent_sorted_set

let count_fixture () =
  let storage = memory_storage () in
  let schema = [ "probe/indexed", indexed; "probe/plain", { indexed with indexed = false } ] in
  let datoms = List.init 4096 (fun i ->
    datom ~e:(i / 2 + 1)
      ~a:(if i mod 2 = 0 then "probe/indexed" else "probe/plain")
      ~v:(String (Printf.sprintf "v%04d" i)) ()) in
  ignore (store ~storage (init_db ~schema datoms));
  storage

let index_counts db =
  List.map PSet.count [ db.eavt_index; db.aevt_index; db.avet_index ]

let assert_index_counts label expected db =
  let actual = index_counts db in
  if actual <> expected then
    failf "%s: expected [%s], got [%s]" label
      (String.concat ";" (List.map string_of_int expected))
      (String.concat ";" (List.map string_of_int actual))

let check_cached_counts label expected db reads =
  reads := [];
  assert_index_counts label expected db;
  assert_index_counts (label ^ " repeated") expected db;
  assert_equal_int (label ^ " does not read storage") 0 (List.length !reads)

let map_storage_root storage f =
  match storage.storage_restore "0" with
  | Some (Storage_root root) -> storage.storage_store [ "0", Storage_root (f root) ]
  | _ -> failwith "expected stored root"

let test_storage__test_restore_keeps_snapshot_counts () =
  let storage = count_fixture () in
  (match storage.storage_restore "0" with
   | Some (Storage_root root) ->
     let counts = List.map (fun metadata -> (Option.get metadata).storage_index_count)
       [ root.storage_eavt_metadata; root.storage_aevt_metadata; root.storage_avet_metadata ] in
     if counts <> [4096;4096;2048] then failwith "root metadata count differs"
   | _ -> failwith "expected root metadata");
  let measured, reads = restore_counting_storage storage in
  let restored = Option.get (restore measured) in
  assert_equal_int "restore reads only root and tail" 2 (List.length !reads);
  check_cached_counts "cold snapshot counts" [4096;4096;2048] restored reads;
  let changed = db_with [ Add (Entity_id 2049, "probe/indexed", String "new") ] restored in
  check_cached_counts "successful add count" [4097;4097;2049] changed reads;
  let unchanged = db_with
    [ Add (Entity_id 2049, "probe/indexed", String "new")
    ; Retract (Entity_id 9999, "probe/indexed", Some (String "missing")) ] changed in
  check_cached_counts "no-op counts" [4097;4097;2049] unchanged reads;
  let removed = db_with
    [ Retract (Entity_id 1, "probe/indexed", Some (String "v0000")) ] unchanged in
  check_cached_counts "successful removal count" [4096;4096;2048] removed reads;
  check_cached_counts "persistent original count" [4096;4096;2048] restored reads

let test_storage__test_restore_counts_after_tail () =
  let storage = count_fixture () in
  store_tail storage
    [ [ datom ~e:2049 ~a:"probe/indexed" ~v:(String "new") ~tx:(tx0 + 1) () ]
    ; [ datom ~e:1 ~a:"probe/indexed" ~v:(String "v0000") ~added:false ~tx:(tx0 + 2) () ]
    ];
  let measured, reads = restore_counting_storage storage in
  let restored = Option.get (restore measured) in
  assert_int_at_most "tail replay does not scan all index nodes" 60 (List.length !reads);
  check_cached_counts "tail applies to snapshot count exactly once" [4096;4096;2048] restored reads;
  List.iter2 (fun index count ->
    assert_equal_int "tail cached count agrees with datoms" count
      (List.length (datoms restored index ())))
    [Eavt;Aevt;Avet] [4096;4096;2048];
  let conn = Option.get (restore_conn measured) in
  check_cached_counts "restore_conn counts after tail" [4096;4096;2048] (conn_db conn) reads;
  let tx = transact_conn conn [ Add (Entity_id 2050, "probe/indexed", String "later") ] in
  check_cached_counts "incremental conn transaction count" [4097;4097;2049] tx.db_after reads;
  ignore (store tx.db_after);
  let restored_again = Option.get (restore measured) in
  check_cached_counts "new snapshot count excludes cleared tail" [4097;4097;2049] restored_again reads

let test_storage__test_empty_and_missing_count_metadata () =
  let storage = memory_storage () in
  ignore (store ~storage (empty_db ()));
  let measured, reads = restore_counting_storage storage in
  let empty = Option.get (restore measured) in
  check_cached_counts "empty snapshot count" [0;0;0] empty reads;
  let changed = db_with [ Add (Entity_id 1, "plain", String "v") ] empty in
  check_cached_counts "empty snapshot edited count" [1;1;0] changed reads;
  let storage = count_fixture () in
  map_storage_root storage (fun root -> {root with
    storage_eavt_metadata = None; storage_aevt_metadata = None; storage_avet_metadata = None});
  let measured, reads = restore_counting_storage storage in
  let restored = Option.get (restore measured) in
  assert_equal_int "missing metadata restore stays lazy" 2 (List.length !reads);
  reads := [];
  assert_index_counts "missing metadata falls back to actual count" [4096;4096;2048] restored;
  if !reads = [] then failwith "missing metadata should load nodes for count";
  let storage = count_fixture () in
  map_storage_root storage (fun root -> {root with
    storage_eavt_metadata = Some {storage_index_count = -1; storage_index_shift = 2}});
  (match restore storage with
   | exception Invalid_argument _ -> ()
   | _ -> failwith "negative current snapshot count must be rejected")

let () =
  test_storage__test_restore_keeps_snapshot_counts ();
  test_storage__test_restore_counts_after_tail ();
  test_storage__test_empty_and_missing_count_metadata ();
  test_storage__test_basics ();
  test_storage__test_upstream_wire_addresses ();
  test_storage__test_file_storage ();
  test_storage__test_gc ();
  test_storage__test_restored_db_addresses ();
  test_storage__test_restored_incremental_store_reuses_index_nodes ();
  test_storage__test_restore_is_lazy ();
  test_storage__test_restore_with_tail_is_lazy ();
  test_storage__test_transact_after_restore_uses_index_slices ();
  test_storage__test_conn_repeated_transacts_store_incrementally ();
  test_storage__test_conn ();
  test_storage__test_conn_tail_write_order ();
  test_storage__test_tail_datom_count ();
  test_storage__test_auto_gc ();
  test_storage__test_db_with_tail ();
