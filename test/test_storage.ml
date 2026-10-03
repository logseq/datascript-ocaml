open Datascript
open Internal.Datascript_types

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq ?e ?a ?v ?tx db index () = Internal.datoms db index ?e ?a ?v ?tx ()

let datoms ?e ?a ?v ?tx db index =
  Internal.datoms db index ?e ?a ?v ?tx () |> List.of_seq

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
  { cardinality = One; unique = None; indexed = true; is_component = false; no_history = false; doc = None; value_type = None; tuple_attrs = None; tuple_types = None }

let unique_identity = { cardinality = (indexed.cardinality); unique = Some Identity; indexed = (indexed.indexed); is_component = (indexed.is_component); no_history = (indexed.no_history); doc = (indexed.doc); value_type = (indexed.value_type); tuple_attrs = indexed.tuple_attrs; tuple_types = indexed.tuple_types }

let remove_dir_if_exists dir =
  if Sys.file_exists dir then begin
    Sys.readdir dir
    |> Array.iter (fun name -> Sys.remove (Filename.concat dir name));
    Unix.rmdir dir
  end

let small_db ?storage () =
  Internal.empty_db ?storage ()
  |> Internal.db_with
       [ Add (Entity_id 1L, "name", String "Ivan")
       ; Add (Entity_id 2L, "name", String "Oleg")
       ; Add (Entity_id 3L, "name", String "Petr")
       ]

let large_db ?storage () =
  Internal.empty_db ?storage ()
  |> Internal.db_with
       (List.init 1000 (fun index ->
          let entity_id = index + 1 in
          Add (Entity_id (Int64.of_int entity_id), "str", String (Int64.to_string (Int64.of_int entity_id)))))

let counting_storage () =
  let storage = Internal.memory_storage () in
  let writes = ref [] in
  let storage_store entries =
    writes := !writes @ List.map fst entries;
    storage.storage_store entries
  in
  { storage_store; storage_restore = storage.storage_restore
  ; storage_list_addresses = storage.storage_list_addresses
  ; storage_delete = storage.storage_delete
  }, writes

let restore_counting_storage storage =
  let reads = ref [] in
  let storage_restore address =
    reads := address :: !reads;
    storage.storage_restore address
  in
  { storage_store = storage.storage_store; storage_restore
  ; storage_list_addresses = storage.storage_list_addresses
  ; storage_delete = storage.storage_delete
  }, reads

let reset_writes writes = writes := []

let test_storage__test_basics () =
  let storage = Internal.memory_storage () in
  let db = small_db () in
  ignore (Internal.store ~storage db);
  assert_upstream_storage_addresses "store writes upstream storage addresses" (Internal.storage_addresses storage);
  (match Internal.restore storage with
   | None -> failwith "restore should read stored db"
   | Some restored ->
     assert_equal_triples
       "restore returns stored facts"
       [ 1L, "name", String "Ivan"; 2L, "name", String "Oleg"; 3L, "name", String "Petr" ]
       (datoms restored Eavt);
     if List.assoc_opt "storage" (Internal.settings restored) <> Some (Bool true) then
       failwith "settings should expose storage attachment");
  let attached_storage = Internal.memory_storage () in
  let attached = Internal.empty_db ~schema:[ "name", indexed ] ~storage:attached_storage () in
  ignore (Internal.store attached);
  (match Internal.restore attached_storage with
   | None -> failwith "store should use db-attached storage"
   | Some restored ->
     if Internal.schema restored <> [ "name", indexed ] then failwith "restore should preserve schema")

let test_storage__test_upstream_wire_addresses () =
  let storage = Internal.memory_storage () in
  let db = small_db () in
  ignore (Internal.store ~storage db);
  let addresses = Internal.storage_addresses storage in
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
      let storage = Internal.file_storage dir in
      let db = small_db () in
      ignore (Internal.store ~storage db);
      Internal.store_tail storage [ [ Internal.datom ~tx:(Int64.add Internal.Db.tx0 2L) ~e:1L ~a:"name" ~v:(String "Alex") () ] ];
      let restored_storage = Internal.file_storage dir in
      assert_upstream_storage_addresses "file_storage lists persisted addresses" (Internal.storage_addresses restored_storage);
      match Internal.restore restored_storage with
      | None -> failwith "file_storage should restore stored db"
      | Some restored ->
        assert_equal_triples
          "file_storage restores root and replays persisted tail"
          [ 1L, "name", String "Alex"; 2L, "name", String "Oleg"; 3L, "name", String "Petr" ]
          (datoms restored Eavt))

let test_storage__test_gc () =
  let storage = Internal.memory_storage () in
  let db = small_db () in
  ignore (Internal.store ~storage db);
  Internal.store_tail storage [ [ Internal.datom ~tx:(Int64.add Internal.Db.tx0 2L) ~e:1L ~a:"name" ~v:(String "Alex") () ] ];
  storage.storage_store [ "stale/node", Storage_tail [] ];
  Internal.collect_garbage storage;
  assert_upstream_storage_addresses "collect_garbage keeps live storage addresses" (Internal.storage_addresses storage);
  match Internal.restore storage with
  | None -> failwith "restore should work after garbage collection"
  | Some restored ->
    assert_equal_triples
      "collect_garbage preserves restorable data"
      [ 1L, "name", String "Alex"; 2L, "name", String "Oleg"; 3L, "name", String "Petr" ]
      (datoms restored Eavt)

let test_storage__test_restored_db_addresses () =
  let storage = Internal.memory_storage () in
  let db = small_db () in
  ignore (Internal.store ~storage db);
  let restored =
    match Internal.restore storage with
    | Some db -> db
    | None -> failwith "restore should read stored db"
  in
  assert_upstream_storage_addresses "addresses should include restored db live nodes" (Internal.Storage.addresses [ restored ])

let test_storage__test_restored_incremental_store_reuses_index_nodes () =
  let storage, writes = counting_storage () in
  let db = large_db () in
  ignore (Internal.store ~storage db);
  let restored =
    match Internal.restore storage with
    | Some db -> db
    | None -> failwith "restore should read stored large db"
  in
  reset_writes writes;
  ignore (Internal.store ~storage restored);
  assert_int_at_most
    "storing an unchanged restored db should not rewrite index nodes"
    2
    (List.length !writes);
  reset_writes writes;
  let db_after =
    Internal.db_with [ Add (Entity_id 1001L, "str", String "1001") ] restored
  in
  ignore (Internal.store ~storage db_after);
  assert_int_at_most
    "storing an incrementally changed restored db should write only changed index paths"
    8
    (List.length !writes);
  assert_equal_triples
    "incremental stored db remains restorable"
    [ 1001L, "str", String "1001" ]
    (datoms ~e:1001L db_after Eavt);
  reset_writes writes;
  let db_after_replacement =
    Internal.db_with [ Add (Entity_id 1L, "str", String "changed") ] restored
  in
  ignore (Internal.store ~storage db_after_replacement);
  assert_int_at_most
    "storing a cardinality-one replacement should write only changed index paths"
    16
    (List.length !writes);
  assert_equal_triples
    "replacement stored db remains restorable"
    [ 1L, "str", String "changed" ]
    (datoms ~e:1L db_after_replacement Eavt)

let test_storage__test_conn_repeated_transacts_store_incrementally () =
  let storage, writes = counting_storage () in
  let conn = Internal.create_conn ~schema:[ "name", indexed ] ~storage () in
  for i = 1 to 300 do
    ignore
      (Internal.transact_conn conn
         [ Add (Entity_id (Int64.of_int i), "name", String (string_of_int i)) ])
  done;
  if List.length !writes = 0 then
    failwith "storage-backed conn should compact its tail into index nodes";
  (* each compaction should only write the index nodes created since the
     previous Internal.store; rewriting the whole tree on every Internal.store made total
     writes grow quadratically with the number of transactions. At 300
     transactions the buggy code writes well over 3000 entries while the
     incremental Internal.store stays under 1000. *)
  assert_int_at_most
    "repeated transacts should not rewrite the whole index tree on every store"
    1500
    (List.length !writes);
  (match Internal.restore storage with
   | None -> failwith "storage-backed conn should restore"
   | Some restored ->
     assert_equal_int
       "all committed datoms remain restorable"
       300
       (List.length (datoms restored Eavt)))

let test_storage__test_restore_is_lazy () =
  let storage = Internal.memory_storage () in
  large_db () |> Internal.store ~storage |> ignore;
  let address_count = List.length (Internal.storage_addresses storage) in
  if address_count < 20 then
    failf "large stored db should have many index nodes, got %d" address_count;
  let counted_storage, reads = restore_counting_storage storage in
  let restored =
    match Internal.restore counted_storage with
    | Some db -> db
    | None -> failwith "restore should read stored large db"
  in
  assert_int_at_most "restore should only read root and tail addresses" 2
    (List.length !reads);
  ignore (Seq.uncons (datoms_seq restored Eavt ()));
  let reads_after_first_datom = List.length !reads in
  if reads_after_first_datom <= 2 then
    failwith "reading the first datom ~e:(Int64.of_int should) ~a:load ~v:the () first index path";
  if reads_after_first_datom >= address_count then
    failf
      "reading the first datom ~e:(Int64.of_int should) ~a:not ~v:restore () every stored node: reads=%d addresses=%d"
      reads_after_first_datom address_count

let test_storage__test_restore_with_tail_is_lazy () =
  let storage = Internal.memory_storage () in
  large_db () |> Internal.store ~storage |> ignore;
  let address_count = List.length (Internal.storage_addresses storage) in
  Internal.store_tail storage
    [
      [
        Internal.datom ~tx:(Int64.add Internal.Db.tx0 2L) ~e:1L ~a:"str" ~v:(String "1") ~added:false ();
        Internal.datom ~tx:(Int64.add Internal.Db.tx0 2L) ~e:1L ~a:"str" ~v:(String "changed") ();
      ];
    ];
  let counted_storage, reads = restore_counting_storage storage in
  let restored =
    match Internal.restore counted_storage with
    | Some db -> db
    | None -> failwith "restore should read stored large db with tail"
  in
  if List.length !reads >= address_count then
    failf
      "restore tail replay should not restore every stored node: reads=%d addresses=%d"
      (List.length !reads) address_count;
  assert_equal_triples
    "tail replay should apply raw datoms"
    [ 1L, "str", String "changed" ]
    (datoms ~e:1L restored Eavt)

let test_storage__test_transact_after_restore_uses_index_slices () =
  let storage = Internal.memory_storage () in
  large_db () |> Internal.store ~storage |> ignore;
  let baseline_storage, baseline_reads = restore_counting_storage storage in
  let baseline =
    match Internal.restore baseline_storage with
    | Some db -> db
    | None -> failwith "restore should read stored large db for baseline"
  in
  ignore (Seq.uncons (datoms_seq ~e:1L baseline Eavt ()));
  let slice_read_count = List.length !baseline_reads in
  let counted_storage, reads = restore_counting_storage storage in
  let restored =
    match Internal.restore counted_storage with
    | Some db -> db
    | None -> failwith "restore should read stored large db"
  in
  let db_after =
    Internal.db_with [ Retract (Entity_id 1L, "str", Some (String "1")) ] restored
  in
  if List.length !reads > slice_read_count + 8 then
    failf
      "transact after restore should use bounded index slices: reads=%d slice_reads=%d"
      (List.length !reads) slice_read_count;
  assert_equal_triples
    "restored db transaction should retract the targeted fact"
    []
    (datoms ~e:1L db_after Eavt)

let test_storage__test_conn () =
  let storage = Internal.memory_storage () in
  let conn = Internal.create_conn ~schema:[ "name", indexed ] ~storage () in
  assert_upstream_storage_addresses "storage-backed create_conn stores upstream addresses" (Internal.storage_addresses storage);
  ignore (Internal.transact_conn conn [ Add (Entity_id 1L, "name", String "Ivan") ]);
  ignore (Internal.transact_conn conn [ Add (Entity_id 2L, "name", String "Oleg") ]);
  let restored =
    match Internal.restore_conn storage with
    | Some conn -> conn
    | None -> failwith "restore_conn should restore storage-backed conn"
  in
  assert_equal_triples
    "restore_conn replays transaction tail"
    [ 1L, "name", String "Ivan"; 2L, "name", String "Oleg" ]
    (datoms (Internal.conn_db restored) Eavt);
  ignore (Internal.transact_conn ~tx_meta:[ "skip-store?", Bool true ] restored [ Add (Entity_id 3L, "name", String "Skipped") ]);
  (match Internal.restore storage with
   | None -> failwith "storage root should remain available"
   | Some restored_db ->
     assert_equal_triples
       "skip-store transaction is not persisted"
       [ 1L, "name", String "Ivan"; 2L, "name", String "Oleg" ]
       (datoms restored_db Eavt));
  ignore
    (Internal.transact_conn
       restored
       (List.init 34 (fun index ->
          let entity_id = index + 4 in
          Add (Entity_id (Int64.of_int entity_id), "name", String (Int64.to_string (Int64.of_int entity_id))))));
  (match storage.storage_restore "1" with
   | Some (Storage_tail []) -> ()
   | _ -> failwith "overflowing storage-backed conn tail should compact");
  let from_db_storage = Internal.memory_storage () in
  let from_db =
    Internal.empty_db ~schema:[ "name", indexed ] ~storage:from_db_storage ()
    |> Internal.db_with [ Add (Entity_id 1L, "name", String "Ivan") ]
  in
  ignore (Internal.conn_from_db from_db);
  (match Internal.restore from_db_storage with
   | Some restored_db ->
     assert_equal_triples
       "conn_from_db stores the initial attached db root"
       [ 1L, "name", String "Ivan" ]
       (datoms restored_db Eavt)
   | None -> failwith "conn_from_db should store attached dbs");
  let from_datoms_storage = Internal.memory_storage () in
  ignore
    (Internal.conn_from_datoms
       ~schema:[ "name", indexed ]
       ~storage:from_datoms_storage
       [ Internal.datom ~e:(3L) ~a:"name" ~v:(String "Petr") () ]);
  match Internal.restore from_datoms_storage with
  | Some restored_db ->
    assert_equal_triples
      "conn_from_datoms stores the initial attached db root"
      [ 3L, "name", String "Petr" ]
      (datoms restored_db Eavt)
  | None -> failwith "conn_from_datoms should store attached datoms"

let test_storage__test_db_with_tail () =
  let db =
    Internal.empty_db ~schema:[ "block/updated-at", indexed; "block/uuid", unique_identity ] ()
    |> Internal.db_with [ Add (Entity_id 1L, "block/updated-at", Int64 2L); Add (Entity_id 1L, "block/uuid", String "u1") ]
  in
  let tail =
    [ [ Internal.datom ~tx:(Int64.add Internal.Db.tx0 3L) ~e:1L ~a:"block/updated-at" ~v:(Int64 1772979060646L) () ]
    ; [ Internal.datom ~tx:(Int64.add Internal.Db.tx0 4L) ~e:1L ~a:"block/updated-at" ~v:(Int64 1772979061145L) () ]
    ; [ Internal.datom ~tx:(Int64.add Internal.Db.tx0 5L) ~e:2L ~a:"block/uuid" ~v:(String "u1") ()
      ; Internal.datom ~tx:(Int64.add Internal.Db.tx0 5L) ~e:2L ~a:"block/title" ~v:(String "Rejected") ()
      ]
    ; [ Internal.datom ~tx:(Int64.add Internal.Db.tx0 6L) ~e:3L ~a:"block/title" ~v:(String "Later") () ]
    ]
  in
  let restored = Internal.db_with_tail db tail in
  assert_equal_triples
    "db_with_tail retracts stale cardinality-one values"
    [ 1L, "block/updated-at", Int64 1772979061145L ]
    (datoms ~a:"block/updated-at" restored Avet);
  assert_equal_triples
    "db_with_tail drops rejected unique-conflict tail groups"
    []
    (datoms ~e:2L restored Eavt);
  assert_equal_triples
    "db_with_tail keeps later valid groups"
    [ 3L, "block/title", String "Later" ]
    (datoms ~e:3L restored Eavt);
  assert_equal_int "db_with_tail advances max tx" (Int64.to_int (Int64.add Internal.Db.tx0 6L)) (Int64.to_int restored.max_tx)

module PSet = Persistent_sorted_set

let count_fixture () =
  let storage = Internal.memory_storage () in
  let schema = [ "probe/indexed", indexed; "probe/plain", { cardinality = (indexed.cardinality); unique = (indexed.unique); indexed = false; is_component = (indexed.is_component); no_history = (indexed.no_history); doc = (indexed.doc); value_type = (indexed.value_type); tuple_attrs = indexed.tuple_attrs; tuple_types = indexed.tuple_types } ] in
  let datoms = List.init 4096 (fun i ->
    Internal.datom ~e:(Int64.of_int (i / 2 + 1)) ~a:(if i mod 2 = 0 then "probe/indexed" else "probe/plain") ~v:(String (Printf.sprintf "v%04d" i)) ()) in
  ignore (Internal.store ~storage (Internal.init_db ~schema datoms));
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
  let restored = Option.get (Internal.restore measured) in
  assert_equal_int "restore reads only root and tail" 2 (List.length !reads);
  check_cached_counts "cold snapshot counts" [4096;4096;2048] restored reads;
  let changed = Internal.db_with [ Add (Entity_id 2049L, "probe/indexed", String "new") ] restored in
  check_cached_counts "successful add count" [4097;4097;2049] changed reads;
  let unchanged = Internal.db_with
    [ Add (Entity_id 2049L, "probe/indexed", String "new")
    ; Retract (Entity_id 9999L, "probe/indexed", Some (String "missing")) ] changed in
  check_cached_counts "no-op counts" [4097;4097;2049] unchanged reads;
  let removed = Internal.db_with
    [ Retract (Entity_id 1L, "probe/indexed", Some (String "v0000")) ] unchanged in
  check_cached_counts "successful removal count" [4096;4096;2048] removed reads;
  check_cached_counts "persistent original count" [4096;4096;2048] restored reads

let test_storage__test_restore_counts_after_tail () =
  let storage = count_fixture () in
  Internal.store_tail storage
    [ [ Internal.datom ~tx:(Int64.add Internal.Db.tx0 1L) ~e:2049L ~a:"probe/indexed" ~v:(String "new") () ]
    ; [ Internal.datom ~tx:(Int64.add Internal.Db.tx0 2L) ~e:1L ~a:"probe/indexed" ~v:(String "v0000") ~added:false () ]
    ];
  let measured, reads = restore_counting_storage storage in
  let restored = Option.get (Internal.restore measured) in
  assert_int_at_most "tail replay does not scan all index nodes" 60 (List.length !reads);
  check_cached_counts "tail applies to snapshot count exactly once" [4096;4096;2048] restored reads;
  List.iter2 (fun index count ->
    assert_equal_int "tail cached count agrees with datoms" count
      (List.length (datoms restored index)))
    [Eavt;Aevt;Avet] [4096;4096;2048];
  let conn = Option.get (Internal.restore_conn measured) in
  check_cached_counts "restore_conn counts after tail" [4096;4096;2048] (Internal.conn_db conn) reads;
  let tx = Internal.transact_conn conn [ Add (Entity_id 2050L, "probe/indexed", String "later") ] in
  check_cached_counts "incremental conn transaction count" [4097;4097;2049] tx.db_after reads;
  ignore (Internal.store tx.db_after);
  let restored_again = Option.get (Internal.restore measured) in
  check_cached_counts "new snapshot count excludes cleared tail" [4097;4097;2049] restored_again reads

let test_storage__test_empty_and_missing_count_metadata () =
  let storage = Internal.memory_storage () in
  ignore (Internal.store ~storage (Internal.empty_db ()));
  let measured, reads = restore_counting_storage storage in
  let empty = Option.get (Internal.restore measured) in
  check_cached_counts "empty snapshot count" [0;0;0] empty reads;
  let changed = Internal.db_with [ Add (Entity_id 1L, "plain", String "v") ] empty in
  check_cached_counts "empty snapshot edited count" [1;1;0] changed reads;
  let storage = count_fixture () in
  map_storage_root storage (fun root -> {root with
    storage_eavt_metadata = None; storage_aevt_metadata = None; storage_avet_metadata = None});
  let measured, reads = restore_counting_storage storage in
  let restored = Option.get (Internal.restore measured) in
  assert_equal_int "missing metadata restore stays lazy" 2 (List.length !reads);
  reads := [];
  assert_index_counts "missing metadata falls back to actual count" [4096;4096;2048] restored;
  if !reads = [] then failwith "missing metadata should load nodes for count";
  let storage = count_fixture () in
  map_storage_root storage (fun root -> {root with
    storage_eavt_metadata = Some {storage_index_count = -1; storage_index_shift = 2}});
  (match Internal.restore storage with
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
  test_storage__test_db_with_tail ();
