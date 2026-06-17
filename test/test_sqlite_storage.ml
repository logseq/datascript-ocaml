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

let assert_equal_query label expected actual =
  if expected <> actual then
    failf "%s: unexpected query result" label

let assert_equal_triples label expected actual =
  let triples = List.map (fun datom -> datom.e, datom.a, datom.v) actual in
  if expected <> triples then failf "%s: unexpected datoms" label

let many =
  { cardinality = Many
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

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

let unique_identity =
  { indexed with unique = Some Identity }

let ref_attr =
  { indexed with indexed = false; value_type = Some RefType }

let ref_many =
  { ref_attr with cardinality = Many }

let no_history =
  { indexed with no_history = true }

let assert_equal_tx_flags label expected actual =
  let values = List.map (fun datom -> datom.e, datom.a, datom.v, datom.added) actual in
  if expected <> values then failf "%s: unexpected tx flags" label

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

let test_sqlite_storage_backed_connections_query_and_transact_after_restore () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed query/transact test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let schema =
        [ "name", unique_identity
        ; "age", indexed
        ; "aka", many
        ; "friend", ref_attr
        ]
      in
      let conn = create_conn ~schema ~storage () in
      ignore
        (transact_conn
           conn
           [ Add (Entity_id 1, "name", String "Ivan")
           ; Add (Entity_id 1, "age", Int 15)
           ; Add (Entity_id 1, "aka", String "Devil")
           ; Add (Entity_id 1, "aka", String "Tupen")
           ; Add (Entity_id 1, "friend", Ref 2)
           ; Add (Entity_id 2, "name", String "Petr")
           ; Add (Entity_id 2, "age", Int 37)
           ]);
      let restored =
        match restore_conn storage with
        | Some conn -> conn
        | None -> failwith "SQLite storage should restore a connection after persisted transactions"
      in
      assert_equal_query
        "restored SQLite conn queries joins"
        [ [ Result_value (String "Petr") ] ]
        (q_string
           (conn_db restored)
           "[:find ?friend-name
             :where [?e :name \"Ivan\"]
                    [?e :friend ?friend]
                    [?friend :name ?friend-name]]");
      assert_equal_query
        "restored SQLite conn queries cardinality-many attrs"
        [ [ Result_value (String "Devil") ]; [ Result_value (String "Tupen") ] ]
        (q_string
           (conn_db restored)
           "[:find ?aka :where [1 :aka ?aka]]");
      assert_equal_query
        "restored SQLite conn queries transaction ids"
        [ [ Result_value (String "Ivan"); Result_entity (tx0 + 1) ] ]
        (q_string
           (conn_db restored)
           "[:find ?name ?tx :where [1 :name ?name ?tx]]");
      ignore
        (transact_conn
           restored
           [ Add (Lookup_ref ("name", String "Ivan"), "age", Int 16)
           ; Retract (Entity_id 1, "aka", Some (String "Devil"))
           ]);
      let restored_again =
        match restore storage with
        | Some db -> db
        | None -> failwith "SQLite storage should restore db after transact on restored conn"
      in
      assert_equal_query
        "SQLite storage persists lookup-ref transact after restore"
        [ [ Result_entity 1; Result_value (Int 16) ] ]
        (q_string
           restored_again
           "[:find ?e ?age
             :where [?e :name \"Ivan\"]
                    [?e :age ?age]]");
      assert_equal_query
        "SQLite storage persists retracts after restore"
        [ [ Result_value (String "Tupen") ] ]
        (q_string restored_again "[:find ?aka :where [1 :aka ?aka]]"))

let test_sqlite_storage_backed_connections_filter_entity_rules_and_repeated_transacts () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed filter/entity/rules test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let schema =
        [ "name", unique_identity
        ; "age", indexed
        ; "aka", many
        ; "tag", many
        ; "password", indexed
        ; "friend", ref_attr
        ]
      in
      let conn = create_conn ~schema ~storage () in
      ignore
        (transact_conn
           conn
           [ Add (Entity_id 1, "name", String "Ivan")
           ; Add (Entity_id 1, "age", Int 25)
           ; Add (Entity_id 1, "aka", String "Terrible")
           ; Add (Entity_id 1, "aka", String "IV")
           ; Add (Entity_id 1, "password", String "<PROTECTED>")
           ; Add (Entity_id 1, "friend", Ref 2)
           ; Add (Entity_id 2, "name", String "Petr")
           ; Add (Entity_id 2, "age", Int 37)
           ; Add (Entity_id 2, "password", String "<SECRET>")
           ; Add (Entity_id 3, "name", String "Nikolai")
           ; Add (Entity_id 3, "age", Int 7)
           ]);
      let restored =
        match restore_conn storage with
        | Some conn -> conn
        | None -> failwith "SQLite storage should restore conn for filter/entity/rules test"
      in
      let visible =
        filter (conn_db restored) (fun _ datom -> datom.a <> "password" && datom.e <> 3)
      in
      assert_equal_query
        "SQLite restored filtered db hides password attrs in queries"
        []
        (q_string visible "[:find ?password :where [_ :password ?password]]");
      assert_equal_query
        "SQLite restored filtered db hides filtered entities in joins"
        [ [ Result_value (String "Ivan") ]; [ Result_value (String "Petr") ] ]
        (q_string visible "[:find ?name :where [?e :name ?name]]");
      (match entity visible (Lookup_ref ("name", String "Ivan")) with
       | None -> failwith "SQLite restored filtered db should resolve visible lookup refs"
       | Some entity ->
         (match entity_attr entity "password" with
          | None -> ()
          | Some _ -> failwith "SQLite restored filtered entity should hide password attr");
         (match entity_attr entity "friend" with
          | Some (One_entity friend) when friend.db_id = Some (Entity_id 2) -> ()
          | _ -> failwith "SQLite restored filtered entity should navigate visible refs"));
      assert_equal_query
        "SQLite restored db supports structured rule queries"
        [ [ Result_value (String "Petr") ] ]
        (q
           (conn_db restored)
           { find = [ Find_var "friend_name" ]
           ; inputs = []
           ; with_vars = []
           ; rules =
               [ { rule_name = "friend-name"
                 ; rule_params = [ "e"; "friend_name" ]
                 ; rule_body =
                     [ Pattern (QVar "e", QAttr "friend", QVar "friend")
                     ; Pattern (QVar "friend", QAttr "name", QVar "friend_name")
                     ]
                 }
               ]
           ; where =
               [ Pattern (QVar "e", QAttr "name", QValue (String "Ivan"))
               ; Rule ("friend-name", [ QVar "e"; QVar "friend_name" ])
               ]
           });
      let friend_name_rules =
        [ { rule_name = "friend-name"
          ; rule_params = [ "e"; "friend_name" ]
          ; rule_body =
              [ Pattern (QVar "e", QAttr "friend", QVar "friend")
              ; Pattern (QVar "friend", QAttr "name", QVar "friend_name")
              ]
          }
        ]
      in
      assert_equal_query
        "SQLite restored db supports parsed rule inputs supplied through %"
        [ [ Result_value (String "Petr") ] ]
        (q_string
           ~inputs:[ Arg_rules friend_name_rules ]
           (conn_db restored)
           "[:find ?friend-name
             :in $ %
             :where [?e :name \"Ivan\"]
                    (friend-name ?e ?friend-name)]");
      ignore
        (transact_conn
           restored
           [ Add (Lookup_ref ("name", String "Ivan"), "tag", String "restored")
           ; Add (Entity_id 4, "name", String "Nina")
           ; Add (Entity_id 4, "age", Int 42)
           ; Add (Lookup_ref ("name", String "Ivan"), "friend", Ref 4)
           ]);
      let restored_again =
        match restore_conn storage with
        | Some conn -> conn
        | None -> failwith "SQLite storage should restore conn after repeated transacts"
      in
      assert_equal_query
        "SQLite storage persists repeated lookup-ref transacts"
        [ [ Result_value (String "restored") ] ]
        (q_string
           (conn_db restored_again)
           "[:find ?tag :where [?e :name \"Ivan\"] [?e :tag ?tag]]");
      assert_equal_query
        "SQLite storage persists cardinality-one ref replacement"
        [ [ Result_value (String "Nina") ] ]
        (q_string
           (conn_db restored_again)
           "[:find ?friend-name
             :where [?e :name \"Ivan\"]
                    [?e :friend ?friend]
                    [?friend :name ?friend-name]]"))

let test_sqlite_storage_backed_connections_index_query_and_transact_parity () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed index/query/transact test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let conn = create_conn ~schema:[ "name", indexed; "age", indexed; "path", indexed ] ~storage () in
      ignore
        (transact_conn
           conn
           [ Add (Entity_id 1, "name", String "Petr")
           ; Add (Entity_id 1, "age", Int 44)
           ; Add (Entity_id 1, "path", List [ Int 1; Int 2 ])
           ; Add (Entity_id 2, "name", String "Ivan")
           ; Add (Entity_id 2, "age", Int 25)
           ; Add (Entity_id 2, "path", List [ Int 1; Int 2; Int 3 ])
           ; Add (Entity_id 3, "name", String "Sergey")
           ; Add (Entity_id 3, "age", Int 11)
           ]);
      let restored =
        match restore_conn storage with
        | Some conn -> conn
        | None -> failwith "SQLite storage should restore conn for index/query/transact parity"
      in
      let restored_db = conn_db restored in
      assert_equal_triples
        "SQLite restored db preserves AEVT order"
        [ 1, "age", Int 44
        ; 2, "age", Int 25
        ; 3, "age", Int 11
        ; 1, "name", String "Petr"
        ; 2, "name", String "Ivan"
        ; 3, "name", String "Sergey"
        ; 1, "path", List [ Int 1; Int 2 ]
        ; 2, "path", List [ Int 1; Int 2; Int 3 ]
        ]
        (datoms restored_db Aevt ());
      assert_equal_triples
        "SQLite restored db supports AVET seek across attrs"
        [ 3, "age", Int 11
        ; 2, "age", Int 25
        ; 1, "age", Int 44
        ; 2, "name", String "Ivan"
        ; 1, "name", String "Petr"
        ; 3, "name", String "Sergey"
        ; 1, "path", List [ Int 1; Int 2 ]
        ; 2, "path", List [ Int 1; Int 2; Int 3 ]
        ]
        (seek_datoms restored_db Avet ~a:"age" ~v:(Int 10) ());
      assert_equal_triples
        "SQLite restored db supports AVET reverse seek"
        [ 1, "name", String "Petr"
        ; 2, "name", String "Ivan"
        ; 1, "age", Int 44
        ; 2, "age", Int 25
        ; 3, "age", Int 11
        ]
        (rseek_datoms restored_db Avet ~a:"name" ~v:(String "Petr") ());
      assert_equal_triples
        "SQLite restored db supports index ranges"
        [ 2, "name", String "Ivan"; 1, "name", String "Petr" ]
        (index_range restored_db "name" ~start:(String "I") ~stop:(String "Q") ());
      assert_equal_query
        "SQLite restored db query sees indexed list values exactly"
        [ [ Result_entity 1 ] ]
        (q_string restored_db "[:find ?e :where [?e :path [1 2]]]");
      ignore
        (transact_conn
           restored
           [ Add (Entity_id 4, "name", String "Nina")
           ; Add (Entity_id 4, "age", Int 42)
           ; Add (Entity_id 4, "path", List [ Int 2 ])
           ]);
      let restored_again =
        match restore storage with
        | Some db -> db
        | None -> failwith "SQLite storage should restore db after index parity transact"
      in
      assert_equal_query
        "SQLite storage persists later indexed transacts for queries"
        [ [ Result_value (String "Nina") ] ]
        (q_string restored_again "[:find ?name :where [?e :age 42] [?e :name ?name]]");
      assert_equal_triples
        "SQLite storage persists later indexed transacts for AVET"
        [ 4, "age", Int 42; 1, "age", Int 44 ]
        (index_range restored_again "age" ~start:(Int 42) ~stop:(Int 44) ()))

let test_sqlite_storage_backed_query_result_shapes_after_restore () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed query result-shape test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let conn = create_conn ~schema:[ "name", indexed; "age", indexed ] ~storage () in
      ignore
        (transact_conn
           conn
           [ Add (Entity_id 1, "name", String "Petr")
           ; Add (Entity_id 1, "age", Int 44)
           ; Add (Entity_id 2, "name", String "Ivan")
           ; Add (Entity_id 2, "age", Int 25)
           ; Add (Entity_id 3, "name", String "Sergey")
           ; Add (Entity_id 3, "age", Int 11)
           ]);
      let db =
        match restore_conn storage with
        | Some conn -> conn_db conn
        | None -> failwith "SQLite storage should restore conn for query result-shape test"
      in
      if
        q_return_string db "[:find [?name ...] :where [_ :name ?name]]"
        <> Query_collection
             [ Result_value (String "Ivan")
             ; Result_value (String "Petr")
             ; Result_value (String "Sergey")
             ]
      then failwith "SQLite restored db should support collection find specs";
      if
        q_return_string db "[:find (count ?name) . :where [_ :name ?name]]"
        <> Query_scalar (Some (Result_value (Int 3)))
      then failwith "SQLite restored db should support scalar aggregate find specs";
      if
        q_return_map_string
          db
          "[:find ?name ?age
            :keys n a
            :where [?e :name ?name]
                   [?e :age ?age]]"
        <> Query_relation_maps
             [ [ Keyword "a", Result_value (Int 25); Keyword "n", Result_value (String "Ivan") ]
             ; [ Keyword "a", Result_value (Int 44); Keyword "n", Result_value (String "Petr") ]
             ; [ Keyword "a", Result_value (Int 11); Keyword "n", Result_value (String "Sergey") ]
             ]
      then failwith "SQLite restored db should support relation return maps";
      if
        q_return_map_string
          db
          "[:find [?name ?age]
            :strs n a
            :where [?e :name ?name]
                   [(= ?name \"Ivan\")]
                   [?e :age ?age]]"
        <> Query_tuple_map (Some [ String "a", Result_value (Int 25); String "n", Result_value (String "Ivan") ])
      then failwith "SQLite restored db should support tuple return maps")

let test_sqlite_storage_backed_lookup_ref_transacts_after_restore () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed lookup-ref transact test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let conn =
        create_conn
          ~schema:[ "name", unique_identity; "email", unique_identity; "friend", ref_attr; "friends", ref_many; "age", indexed ]
          ~storage
          ()
      in
      ignore
        (transact_conn
           conn
           [ Add (Entity_id 1, "name", String "Ivan")
           ; Add (Entity_id 1, "email", String "ivan@example.com")
           ; Add (Entity_id 2, "name", String "Petr")
           ; Add (Entity_id 2, "email", String "petr@example.com")
           ; Add (Entity_id 3, "name", String "Oleg")
           ; Add (Entity_id 3, "email", String "oleg@example.com")
           ]);
      let restored =
        match restore_conn storage with
        | Some conn -> conn
        | None -> failwith "SQLite storage should restore conn for lookup-ref transact test"
      in
      ignore
        (transact_conn
           restored
           [ Add (Lookup_ref ("name", String "Ivan"), "age", Int 35)
           ; Add (Lookup_ref ("email", String "ivan@example.com"), "friend", Ref_to (Lookup_ref ("name", String "Petr")))
           ; Add (Lookup_ref ("name", String "Ivan"), "friends", Ref_to (Lookup_ref ("name", String "Petr")))
           ; Add (Lookup_ref ("name", String "Ivan"), "friends", Ref_to (Lookup_ref ("name", String "Oleg")))
           ]);
      let restored_again =
        match restore_conn storage with
        | Some conn -> conn
        | None -> failwith "SQLite storage should restore conn after lookup-ref transacts"
      in
      assert_equal_query
        "SQLite storage persists lookup-ref add entity ids"
        [ [ Result_value (Int 35) ] ]
        (q_string
           (conn_db restored_again)
           "[:find ?age :where [[:name \"Ivan\"] :age ?age]]");
      assert_equal_query
        "SQLite storage persists lookup-ref ref values"
        [ [ Result_value (String "Petr") ] ]
        (q_string
           (conn_db restored_again)
           "[:find ?name
             :where [[:name \"Ivan\"] :friend ?friend]
                    [?friend :name ?name]]");
      assert_equal_query
        "SQLite storage persists lookup-ref cardinality-many ref values"
        [ [ Result_value (String "Oleg") ]; [ Result_value (String "Petr") ] ]
        (q_string
           (conn_db restored_again)
           "[:find ?name
             :where [[:name \"Ivan\"] :friends ?friend]
                    [?friend :name ?name]]");
      ignore
        (transact_conn
           restored_again
           [ CompareAndSet
               ( Lookup_ref ("name", String "Ivan")
               , "friend"
               , Some (Ref_to (Lookup_ref ("name", String "Petr")))
               , Ref_to (Lookup_ref ("name", String "Oleg")) )
           ; Retract (Lookup_ref ("name", String "Ivan"), "age", Some (Int 35))
           ]);
      let final_db =
        match restore storage with
        | Some db -> db
        | None -> failwith "SQLite storage should restore final lookup-ref db"
      in
      assert_equal_query
        "SQLite storage persists lookup-ref CAS ref updates"
        [ [ Result_value (String "Oleg") ] ]
        (q_string
           final_db
           "[:find ?name
             :where [[:name \"Ivan\"] :friend ?friend]
                    [?friend :name ?name]]");
      assert_equal_query
        "SQLite storage persists lookup-ref retracts"
        []
        (q_string final_db "[:find ?age :where [[:name \"Ivan\"] :age ?age]]"))

let test_sqlite_storage_backed_not_or_queries_after_restore () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed not/or query test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let conn = create_conn ~schema:[ "name", indexed; "age", indexed ] ~storage () in
      ignore
        (transact_conn
           conn
           [ Add (Entity_id 1, "name", String "Ivan")
           ; Add (Entity_id 1, "age", Int 10)
           ; Add (Entity_id 2, "name", String "Ivan")
           ; Add (Entity_id 2, "age", Int 20)
           ; Add (Entity_id 3, "name", String "Oleg")
           ; Add (Entity_id 3, "age", Int 10)
           ; Add (Entity_id 4, "name", String "Oleg")
           ; Add (Entity_id 4, "age", Int 20)
           ]);
      let db =
        match restore storage with
        | Some db -> db
        | None -> failwith "SQLite storage should restore db for not/or queries"
      in
      assert_equal_query
        "SQLite restored db supports not query clauses"
        [ [ Result_entity 3 ]; [ Result_entity 4 ] ]
        (q_string db "[:find ?e :where [?e :name] (not [?e :name \"Ivan\"])]");
      assert_equal_query
        "SQLite restored db supports not-join query clauses"
        [ [ Result_entity 1; Result_value (Int 10) ]
        ; [ Result_entity 2; Result_value (Int 20) ]
        ]
        (q_string
           db
           "[:find ?e ?a
             :where [?e :name]
                    [?e :age ?a]
                    (not-join [?e]
                      [?e :name \"Oleg\"]
                      [?e :age ?a])]");
      assert_equal_query
        "SQLite restored db supports or query clauses"
        [ [ Result_entity 1 ]; [ Result_entity 3 ]; [ Result_entity 4 ] ]
        (q_string db "[:find ?e :where (or [?e :name \"Oleg\"] [?e :age 10])]");
      assert_equal_query
        "SQLite restored db supports or-join query clauses"
        [ [ Result_entity 1 ]; [ Result_entity 3 ]; [ Result_entity 4 ] ]
        (q_string
           db
           "[:find ?e
             :in $ ?a
             :where (or-join [?e ?a]
                      [?e :age ?a]
                      [?e :name \"Oleg\"])]"
           ~inputs:[ Arg_scalar (Result_value (Int 10)) ]))

let test_sqlite_storage_backed_transact_history_and_current_tx_parity () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed transact/history/current-tx test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let conn =
        create_conn
          ~schema:
            [ "name", unique_identity
            ; "created-at", ref_attr
            ; "source", indexed
            ; "secret", no_history
            ]
          ~storage
          ()
      in
      let report =
        transact_conn
          ~tx_meta:[ "source", String "sqlite-parity" ]
          conn
          [ Entity
              { db_id = Some (Temp_id "ivan")
              ; attrs =
                  [ "name", One_value (String "Ivan")
                  ; "created-at", One_value TxRef
                  ; "secret", One_value (String "alpha")
                  ]
              }
          ; Add (CurrentTx, "source", String "initial")
          ]
      in
      if report.tx_meta <> [ "source", String "sqlite-parity" ] then
        failwith "SQLite storage-backed transact should preserve tx metadata in reports";
      if resolve_tempid report.tempids "ivan" <> Some 1 then
        failwith "SQLite storage-backed transact should expose resolved entity tempids";
      if resolve_tempid report.tempids "db/current-tx" <> Some (tx0 + 1) then
        failwith "SQLite storage-backed transact should expose current tx tempid";
      let restored =
        match restore_conn storage with
        | Some conn -> conn
        | None -> failwith "SQLite storage should restore conn for transact/history/current-tx test"
      in
      assert_equal_query
        "SQLite restored db queries current-tx ref facts"
        [ [ Result_value (String "initial") ] ]
        (q_string
           (conn_db restored)
           "[:find ?source
             :where [?e :name \"Ivan\"]
                    [?e :created-at ?tx]
                    [?tx :source ?source]]");
      ignore
        (transact_conn
           restored
           [ Add (Lookup_ref ("name", String "Ivan"), "name", String "Petr")
           ; Add (Lookup_ref ("name", String "Petr"), "secret", String "beta")
           ]);
      let db =
        match restore storage with
        | Some db -> db
        | None -> failwith "SQLite storage should restore db after history transact"
      in
      assert_equal_query
        "SQLite storage persists cardinality-one replacement after restore"
        [ [ Result_value (String "Petr") ] ]
        (q_string db "[:find ?name :where [?e :name ?name]]");
      assert_equal_tx_flags
        "SQLite restored history keeps name additions and retractions"
        [ 1, "name", String "Ivan", true
        ; 1, "name", String "Ivan", false
        ; 1, "name", String "Petr", true
        ]
        (datoms (history db) Eavt ~a:"name" ());
      assert_equal_triples
        "SQLite restored history excludes no-history attrs"
        []
        (datoms (history db) Eavt ~a:"secret" ());
      assert_equal_query
        "SQLite restored active db keeps latest no-history value"
        [ [ Result_value (String "beta") ] ]
        (q_string db "[:find ?secret :where [?e :name \"Petr\"] [?e :secret ?secret]]"))

let test_sqlite_storage_backed_pull_sources_and_relation_inputs_after_restore () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed pull/source/relation query test: sqlite3 is not available"
  else
    with_temp_db (fun people_path ->
      with_temp_db (fun score_path ->
        let people_storage = Sqlite_storage.storage people_path in
        let score_storage = Sqlite_storage.storage score_path in
        let people_conn =
          create_conn
            ~schema:[ "email", unique_identity; "name", indexed; "friend", ref_attr ]
            ~storage:people_storage
            ()
        in
        let score_conn =
          create_conn
            ~schema:[ "email", unique_identity; "score", indexed ]
            ~storage:score_storage
            ()
        in
        ignore
          (transact_conn
             people_conn
             [ Add (Entity_id 1, "email", String "ivan@example.com")
             ; Add (Entity_id 1, "name", String "Ivan")
             ; Add (Entity_id 1, "friend", Ref 2)
             ; Add (Entity_id 2, "email", String "petr@example.com")
             ; Add (Entity_id 2, "name", String "Petr")
             ]);
        ignore
          (transact_conn
             score_conn
             [ Add (Entity_id 10, "email", String "ivan@example.com")
             ; Add (Entity_id 10, "score", Int 20)
             ; Add (Entity_id 11, "email", String "petr@example.com")
             ; Add (Entity_id 11, "score", Int 40)
             ]);
        let people =
          match restore people_storage with
          | Some db -> db
          | None -> failwith "SQLite storage should restore people db"
        in
        let scores =
          match restore score_storage with
          | Some db -> db
          | None -> failwith "SQLite storage should restore score db"
        in
        assert_equal_query
          "SQLite restored named sources join across persisted dbs"
          [ [ Result_value (String "Ivan"); Result_value (Int 20) ]
          ; [ Result_value (String "Petr"); Result_value (Int 40) ]
          ]
          (q_sources_string
             people
             [ "scores", Db_source scores ]
             "[:find ?name ?score
               :in $ $scores
               :where [?person :email ?email]
                      [?person :name ?name]
                      [$scores ?row :email ?email]
                      [$scores ?row :score ?score]]");
        assert_equal_query
          "SQLite restored db joins relation inputs after persistence"
          [ [ Result_value (String "Petr"); Result_value (String "friend") ] ]
          (q_sources_string
             people
             [ "labels", Relation_source [ [ Result_value (String "petr@example.com"); Result_value (String "friend") ] ] ]
             "[:find ?name ?label
               :in $ $labels
               :where [?e :email ?email]
                      [?e :name ?name]
                      [$labels ?email ?label]]");
        (match pull_string people "[:name {:friend [:name]}]" (Lookup_ref ("email", String "ivan@example.com")) with
         | Some pulled ->
           if
             pulled.pulled_attrs
             <> [ Keyword "friend",
                  Pulled_entity
                    { pulled_id = 2
                    ; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ]
                    }
                ; Keyword "name", Pulled_scalar (String "Ivan")
                ]
           then failwith "SQLite restored db should support pull with refs"
         | None -> failwith "SQLite restored db should pull lookup-ref entities");
        if
          q_return_string
            people
            "[:find (pull ?e [:name {:friend [:name]}]) .
              :where [?e :email \"ivan@example.com\"]]"
          <> Query_scalar
               (Some
                  (Result_pull
                     { pulled_id = 1
                     ; pulled_attrs =
                         [ Keyword "friend",
                           Pulled_entity
                             { pulled_id = 2
                             ; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Petr") ]
                             }
                         ; Keyword "name", Pulled_scalar (String "Ivan")
                         ]
                     }))
        then failwith "SQLite restored db should support pull find specs"))

let test_sqlite_storage_backed_reset_schema_and_compaction_parity () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed reset-schema/compaction test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let conn = create_conn ~schema:[ "name", indexed; "age", indexed ] ~storage () in
      ignore (transact_conn conn [ Add (Entity_id 1, "name", String "Ivan") ]);
      let restored =
        match restore_conn storage with
        | Some conn -> conn
        | None -> failwith "SQLite storage should restore conn for reset-schema test"
      in
      ignore (reset_schema restored [ "name", indexed; "email", unique_identity ]);
      ignore
        (transact_conn
           restored
           [ Add (Entity_id 1, "email", String "ivan@example.com")
           ; Add (Temp_id "same-email", "email", String "ivan@example.com")
           ; Add (Temp_id "same-email", "name", String "Ivan Upserted")
           ]);
      let db =
        match restore storage with
        | Some db -> db
        | None -> failwith "SQLite storage should restore db after reset-schema"
      in
      (match List.assoc_opt "age" (schema db) with
       | None -> ()
       | Some _ -> failwith "SQLite reset_schema should persist removed schema attrs");
      if List.assoc_opt "email" (schema db) <> Some unique_identity then
        failwith "SQLite reset_schema should persist added unique attrs";
      assert_equal_query
        "SQLite reset schema persists unique identity tempid upsert semantics"
        [ [ Result_entity 1; Result_value (String "ivan@example.com"); Result_value (String "Ivan Upserted") ] ]
        (q_string db "[:find ?e ?email ?name :where [?e :email ?email] [?e :name ?name]]");
      assert_equal
        "SQLite reset schema compacts stale tail"
        "datascript/root,datascript/tail"
        (String.concat "," (storage_addresses storage)))

let default_logseq_graph_db =
  "/Users/tiensonqin/logseq/graphs/demo/db.sqlite"

let logseq_graphs_dir =
  "/Users/tiensonqin/logseq/graphs"

let logseq_graph_dbs () =
  Sqlite_storage.graph_db_paths logseq_graphs_dir

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

let test_all_existing_logseq_graphs_are_recognized_read_only () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping all-graph Logseq inspection: sqlite3 is not available"
  else
    match logseq_graph_dbs () with
    | [] -> prerr_endline "Skipping all-graph Logseq inspection: no local graphs found"
    | db_paths ->
      List.iter
        (fun db_path ->
          let before = (Unix.stat db_path).Unix.st_mtime in
          let summary = Sqlite_storage.inspect ~read_only:true db_path in
          let after = (Unix.stat db_path).Unix.st_mtime in
          if not summary.has_kvs_table then failf "%s should contain a kvs table" db_path;
          if not summary.has_root then failf "%s should contain addr 0 root metadata" db_path;
          if summary.root_content_format <> Sqlite_storage.Logseq_transit then
            failf "%s root should be recognized as Transit JSON" db_path;
          if not (List.mem "schema" summary.root_keys) then
            failf "%s root should decode Transit schema metadata" db_path;
          if before <> after then failf "read-only inspection should not modify %s" db_path)
        db_paths

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

let assert_logseq_schema_query_and_transact db_path =
  let before = (Unix.stat db_path).Unix.st_mtime in
  let schema = Sqlite_storage.schema_of_logseq_graph ~read_only:true db_path in
  let after_schema = (Unix.stat db_path).Unix.st_mtime in
  let block_name_schema =
    match List.assoc_opt "block/name" schema with
    | Some schema -> schema
    | None -> failf "%s schema should expose :block/name" db_path
  in
  if not block_name_schema.indexed then failf "%s :block/name should be indexed" db_path;
  let db = empty_db ~schema () in
  let report =
    transact db [ Add (Entity_id 9_999_998, "block/name", String "from-logseq-schema") ]
  in
  assert_equal_int
    ("query synthetic datom with Logseq schema in " ^ db_path)
    1
    (List.length
       (q_string
          report.db_after
          "[:find ?e :where [?e :block/name \"from-logseq-schema\"]]"));
  if before <> after_schema then failf "read-only schema loading should not modify %s" db_path

let test_all_existing_logseq_graph_schemas_support_query_and_transact () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping all-graph Logseq schema smoke: sqlite3 is not available"
  else
    match logseq_graph_dbs () with
    | [] -> prerr_endline "Skipping all-graph Logseq schema smoke: no local graphs found"
    | db_paths -> List.iter assert_logseq_schema_query_and_transact db_paths

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

let query_for_datom datom =
  Printf.sprintf "[:find ?v :where [%d :%s ?v]]" datom.e datom.a

let assert_logseq_datoms_query_and_transact db_path =
  let before = (Unix.stat db_path).Unix.st_mtime in
  let schema = Sqlite_storage.schema_of_logseq_graph ~read_only:true db_path in
  let datoms = Sqlite_storage.datoms_of_logseq_graph ~read_only:true ~limit:1 db_path in
  let after = (Unix.stat db_path).Unix.st_mtime in
  let first_datom =
    match datoms with
    | first :: _ -> first
    | [] -> failf "%s should decode at least one datom from existing graph nodes" db_path
  in
  let db = init_db ~schema datoms in
  let query_results = q_string db (query_for_datom first_datom) in
  if
    not
      (List.exists
         (function
           | [ Result_value value ] -> value = first_datom.v
           | _ -> false)
         query_results)
  then
    failf "%s should query the first decoded Logseq datom" db_path;
  let report =
    transact db [ Add (Entity_id 9_999_999, "block/name", String "ocaml local graph smoke") ]
  in
  assert_equal_int
    ("transact against decoded Logseq graph schema in " ^ db_path)
    1
    (List.length
       (q_string
          report.db_after
          "[:find ?e :where [?e :block/name \"ocaml local graph smoke\"]]"));
  if before <> after then failf "read-only datom loading should not modify %s" db_path

let test_all_existing_logseq_graph_datoms_support_query_and_transact () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping all-graph Logseq datom smoke: sqlite3 is not available"
  else
    match logseq_graph_dbs () with
    | [] -> prerr_endline "Skipping all-graph Logseq datom smoke: no local graphs found"
    | db_paths -> List.iter assert_logseq_datoms_query_and_transact db_paths

let () =
  Random.self_init ();
  test_sqlite_storage_round_trips_ocaml_payloads ();
  test_sqlite_storage_backed_connections_query_and_transact_after_restore ();
  test_sqlite_storage_backed_connections_filter_entity_rules_and_repeated_transacts ();
  test_sqlite_storage_backed_connections_index_query_and_transact_parity ();
  test_sqlite_storage_backed_query_result_shapes_after_restore ();
  test_sqlite_storage_backed_lookup_ref_transacts_after_restore ();
  test_sqlite_storage_backed_not_or_queries_after_restore ();
  test_sqlite_storage_backed_transact_history_and_current_tx_parity ();
  test_sqlite_storage_backed_pull_sources_and_relation_inputs_after_restore ();
  test_sqlite_storage_backed_reset_schema_and_compaction_parity ();
  test_existing_logseq_graph_is_recognized_read_only ();
  test_all_existing_logseq_graphs_are_recognized_read_only ();
  test_existing_logseq_graph_schema_supports_query_and_transact ();
  test_all_existing_logseq_graph_schemas_support_query_and_transact ();
  test_existing_logseq_graph_datoms_support_query_and_transact ();
  test_all_existing_logseq_graph_datoms_support_query_and_transact ()
