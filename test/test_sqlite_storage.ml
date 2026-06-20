open Datascript

module Sqlite_storage = Logseq_sqlite_storage
module Transit = Logseq_sqlite_storage__Logseq_transit

let failf fmt = Printf.ksprintf failwith fmt

let datoms_seq = datoms

let datoms db index ?e ?a ?v ?tx () =
  datoms_seq db index ?e ?a ?v ?tx () |> List.of_seq

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

let sqlite3_available () = true

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

let select_single_string db_path sql =
  with_sqlite db_path (fun db ->
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> check_sql db sql (Sqlite3.finalize stmt))
      (fun () ->
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> Some (Sqlite3.column_text stmt 0)
        | Sqlite3.Rc.DONE -> None
        | rc ->
          check_sql db sql rc;
          None))

let select_single_int db_path sql =
  with_sqlite db_path (fun db ->
    let stmt = Sqlite3.prepare db sql in
    Fun.protect
      ~finally:(fun () -> check_sql db sql (Sqlite3.finalize stmt))
      (fun () ->
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW -> Sqlite3.column_int stmt 0
        | Sqlite3.Rc.DONE -> 0
        | rc ->
          check_sql db sql rc;
          0))

let sql_quote text =
  "'" ^ String.concat "''" (String.split_on_char '\'' text) ^ "'"

let ocaml_payload_prefix = "ocaml-marshal-hex:"

let starts_with prefix text =
  let prefix_len = String.length prefix in
  String.length text >= prefix_len && String.sub text 0 prefix_len = prefix

let assert_not_ocaml_marshal label content =
  if starts_with ocaml_payload_prefix content then
    failf "%s: SQLite content must be Transit, not OCaml marshal" label

let transit_of_sqlite_content label content =
  assert_not_ocaml_marshal label content;
  match Transit.of_string content with
  | value -> value
  | exception Transit.Decode_error message ->
    failf "%s: SQLite content is not decodable Transit: %s" label message
  | exception Yojson.Json_error message ->
    failf "%s: SQLite content is not JSON Transit: %s" label message

let transit_key = function
  | Transit.Keyword value | Transit.String value -> Some value
  | _ -> None

let transit_int = function
  | Transit.Int value -> Some value
  | Transit.Int64 value ->
    if value >= Int64.of_int min_int && value <= Int64.of_int max_int then
      Some (Int64.to_int value)
    else
      None
  | _ -> None

let transit_lookup key = function
  | Transit.Map entries ->
    List.find_map
      (fun (entry_key, value) ->
        match transit_key entry_key with
        | Some entry_key when entry_key = key -> Some value
        | _ -> None)
      entries
  | _ -> None

let expect_transit_map label = function
  | Transit.Map entries -> entries
  | _ -> failf "%s: expected a Transit map" label

let expect_transit_array label = function
  | Transit.Array values -> values
  | _ -> failf "%s: expected a Transit array" label

let expect_transit_int label value =
  match transit_int value with
  | Some value -> value
  | None -> failf "%s: expected a Transit integer" label

let assert_transit_has_key label key value =
  match transit_lookup key value with
  | Some _ -> ()
  | None -> failf "%s: missing Transit key :%s" label key

let json_quote text =
  let buffer = Buffer.create (String.length text + 2) in
  Buffer.add_char buffer '"';
  String.iter
    (function
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | ch -> Buffer.add_char buffer ch)
    text;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

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

let without_path f =
  let old_path = Sys.getenv_opt "PATH" in
  Fun.protect
    ~finally:(fun () ->
      match old_path with
      | Some path -> Unix.putenv "PATH" path
      | None -> Unix.putenv "PATH" "")
    (fun () ->
      Unix.putenv "PATH" "";
      f ())

let assert_equal label expected actual =
  if expected <> actual then failf "%s: expected %S, got %S" label expected actual

let assert_equal_int label expected actual =
  if expected <> actual then failf "%s: expected %d, got %d" label expected actual

let assert_equal_query label expected actual =
  if expected <> actual then
    failf "%s: unexpected query result" label

let rec string_of_value = function
  | Nil -> "nil"
  | Int value -> string_of_int value
  | Float value -> string_of_float value
  | String value -> Printf.sprintf "%S" value
  | Symbol value -> value
  | Bool value -> string_of_bool value
  | Keyword value -> ":" ^ value
  | Uuid value -> "#uuid " ^ Printf.sprintf "%S" value
  | Instant value -> string_of_int value
  | Regex value -> "#\"" ^ String.escaped value ^ "\""
  | Ref entity_id -> string_of_int entity_id
  | List values -> "[" ^ String.concat " " (List.map string_of_value values) ^ "]"
  | Vector values -> "#vector[" ^ String.concat " " (List.map string_of_value values) ^ "]"
  | Map entries ->
    "{"
    ^ (entries
       |> List.map (fun (key, value) -> string_of_value key ^ " " ^ string_of_value value)
       |> String.concat " ")
    ^ "}"
  | Set values -> "#{" ^ String.concat " " (List.map string_of_value values) ^ "}"
  | Tuple values ->
    "["
    ^ (values
       |> List.map (function None -> "nil" | Some value -> string_of_value value)
       |> String.concat " ")
    ^ "]"
  | TxRef -> ":db/current-tx"
  | Ref_to _ -> "#ref"

let string_of_triples triples =
  triples
  |> List.map (fun (e, a, v) -> Printf.sprintf "(%d :%s %s)" e a (string_of_value v))
  |> String.concat "; "

let assert_equal_triples label expected actual =
  let triples = List.map (fun datom -> datom.e, datom.a, datom.v) actual in
  if expected <> triples then
    failf
      "%s: expected [%s], got [%s]"
      label
      (string_of_triples expected)
      (string_of_triples triples)

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

let component =
  { ref_attr with is_component = true }

let no_history =
  { indexed with no_history = true }

let tuple_unique_identity attrs =
  { indexed with
    unique = Some Identity
  ; value_type = Some TupleType
  ; tuple_attrs = Some attrs
  }

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
        "CREATE TABLE kvs (addr INTEGER primary key, content TEXT, addresses JSON)"
        (Option.value
           ~default:""
           (select_single_string
              db_path
              "select sql from sqlite_master where type = 'table' and name = 'kvs';"));
      assert_equal_int "row count" 5 (Sqlite_storage.inspect db_path).row_count;
      assert_upstream_storage_addresses "storage addresses" (storage_addresses storage);
      match restore (Sqlite_storage.storage db_path) with
      | None -> failwith "SQLite storage should restore the stored db"
      | Some restored ->
        let names = datoms restored Avet ~a:"name" () in
        if List.map (fun datom -> datom.e, datom.a, datom.v) names <> [ 1, "name", String "Ada" ] then
          failwith "SQLite storage should preserve stored datoms")

let test_sqlite_storage_raw_layout_after_transact () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite raw storage layout test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let schema =
        [ "name", unique_identity
        ; "age", indexed
        ; "aka", many
        ; "friend", ref_attr
        ; "tag", many
        ]
      in
      let conn = create_conn ~schema ~storage () in
      ignore
        (transact_conn
           conn
           ([ Add (Entity_id 1, "name", String "Ivan")
            ; Add (Entity_id 1, "age", Int 15)
            ; Add (Entity_id 1, "aka", String "Devil")
            ; Add (Entity_id 1, "aka", String "Tupen")
            ; Add (Entity_id 1, "friend", Ref 2)
            ; Add (Entity_id 2, "name", String "Petr")
            ; Add (Entity_id 2, "age", Int 37)
            ]
            @ List.init 40 (fun index ->
              Add (Entity_id 1, "tag", String ("tag-" ^ string_of_int index)))));
      ignore
        (transact_conn
           conn
           [ Add (Entity_id 2, "aka", String "Czar")
           ; Add (Entity_id 3, "name", String "Nikolai")
           ; Add (Entity_id 1, "tag", String "tail-tag")
           ]);
      assert_equal_int
        "SQLite kvs should contain root and tail rows"
        1
        (select_single_int db_path "select count(*) from kvs where addr = 0;");
      assert_equal_int
        "SQLite kvs should contain transaction tail row"
        1
        (select_single_int db_path "select count(*) from kvs where addr = 1;");
      if select_single_int db_path "select count(*) from kvs where addresses is not null;" <= 0 then
        failwith "SQLite storage nodes should expose branch addresses in the Logseq addresses JSON column";
      assert_equal_int
        "SQLite storage should not write OCaml marshal payloads"
        0
        (select_single_int
           db_path
           ("select count(*) from kvs where content like " ^ sql_quote (ocaml_payload_prefix ^ "%") ^ ";"));
      let root_content =
        Option.value
          ~default:""
          (select_single_string db_path "select content from kvs where addr = 0;")
      in
      let tail_content =
        Option.value
          ~default:""
          (select_single_string db_path "select content from kvs where addr = 1;")
      in
      let root = transit_of_sqlite_content "root row" root_content in
      List.iter
        (fun key -> assert_transit_has_key "SQLite root Transit metadata" key root)
        [ "schema"
        ; "max-eid"
        ; "max-tx"
        ; "eavt"
        ; "aevt"
        ; "avet"
        ; "max-addr"
        ; "branching-factor"
        ; "ref-type"
        ];
      ignore (expect_transit_map "SQLite root row" root);
      let schema_value =
        match transit_lookup "schema" root with
        | Some schema -> schema
        | None -> failwith "SQLite root Transit metadata should include :schema"
      in
      List.iter
        (fun attr -> assert_transit_has_key "SQLite root Transit schema" attr schema_value)
        [ "name"; "age"; "aka"; "friend"; "tag" ];
      let eavt_address =
        expect_transit_int
          "SQLite root :eavt"
          (Option.value ~default:Transit.Null (transit_lookup "eavt" root))
      in
      let aevt_address =
        expect_transit_int
          "SQLite root :aevt"
          (Option.value ~default:Transit.Null (transit_lookup "aevt" root))
      in
      let avet_address =
        expect_transit_int
          "SQLite root :avet"
          (Option.value ~default:Transit.Null (transit_lookup "avet" root))
      in
      if eavt_address = aevt_address || eavt_address = avet_address || aevt_address = avet_address then
        failwith "SQLite root row should point at three distinct index roots";
      List.iter
        (fun (label, address) ->
          match
            select_single_string
              db_path
              ("select content from kvs where addr = " ^ string_of_int address ^ ";")
          with
          | Some content ->
            let node = transit_of_sqlite_content (label ^ " index root row") content in
            assert_transit_has_key (label ^ " index root row") "keys" node
          | None -> failf "%s index root address is missing from SQLite: %d" label address)
        [ "EAVT", eavt_address; "AEVT", aevt_address; "AVET", avet_address ];
      let tail = transit_of_sqlite_content "tail row" tail_content in
      let tail_groups = expect_transit_array "SQLite tail row" tail in
      let has_fact e a v =
        List.exists
          (List.exists
             (function
               | Transit.Array [ entity; attr; value; _tx ] ->
                 Some e = transit_int entity && transit_key attr = Some a && value = v
               | _ -> false))
          (List.map (expect_transit_array "SQLite tail transaction group") tail_groups)
      in
      assert_equal_int "SQLite tail should contain one transaction group" 1 (List.length tail_groups);
      if not (has_fact 2 "aka" (Transit.String "Czar")) then
        failwith "SQLite tail should contain Petr aka datom";
      if not (has_fact 3 "name" (Transit.String "Nikolai")) then
        failwith "SQLite tail should contain Nikolai name datom";
      if not (has_fact 1 "tag" (Transit.String "tail-tag")) then
        failwith "SQLite tail should contain the latest tag datom";
      match restore_conn (Sqlite_storage.storage db_path) with
      | None -> failwith "SQLite storage should restore raw-layout test db"
      | Some restored ->
        assert_equal_query
          "SQLite restored db should replay raw tail data"
          [ [ Result_value (String "Petr") ] ]
          (q_string
             (conn_db restored)
             "[:find ?friend-name
               :where [?e :name \"Ivan\"]
                      [?e :friend ?friend]
                      [?friend :name ?friend-name]]"))

let test_sqlite_storage_does_not_require_sqlite3_binary () =
  with_temp_db (fun db_path ->
    without_path (fun () ->
      let storage = Sqlite_storage.storage db_path in
      storage.storage_store [ "2", Storage_tail [] ];
      match storage.storage_restore "2" with
      | Some (Storage_tail []) -> ()
      | Some _ -> failwith "SQLite storage should keep payloads without sqlite3 binary"
      | None -> failwith "SQLite storage should not require sqlite3 binary"))

let test_sqlite_storage_store_and_delete_are_separate () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage explicit delete test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      storage.storage_store [ "2", Storage_tail [] ];
      storage.storage_store [ "3", Storage_tail [] ];
      (match storage.storage_restore "2" with
       | Some (Storage_tail []) -> ()
       | Some _ -> failwith "SQLite storage should keep the original payload"
       | None -> failwith "SQLite storage store should not delete addresses");
      assert_equal
        "storage addresses before explicit delete"
        "2,3"
        (String.concat "," (storage_addresses storage));
      storage.storage_delete [ "2" ];
      assert_equal
        "storage addresses after explicit delete"
        "3"
        (String.concat "," (storage_addresses storage)))

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
        (q_string restored_db "[:find ?e :where [?e :path (1 2)]]");
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

let test_sqlite_storage_backed_composite_values_after_restore () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed composite value test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let profile =
        Map
          [ Keyword "tags", Vector [ String "alpha"; String "beta" ]
          ; Keyword "prefs", Map [ Keyword "theme", String "dark"; Keyword "pins", Vector [ Int 1; Int 2 ] ]
          ]
      in
      let conn = create_conn ~schema:[ "profile", indexed ] ~storage () in
      ignore (transact_conn conn [ Add (Entity_id 1, "profile", profile) ]);
      let restored =
        match restore_conn storage with
        | Some conn -> conn
        | None -> failwith "SQLite storage should restore conn for composite value test"
      in
      let restored_db = conn_db restored in
      assert_equal_query
        "SQLite restored db queries map-of-vector datom values by structural equality"
        [ [ Result_entity 1 ] ]
        (q_string
           restored_db
           "[:find ?e :where [?e :profile {:tags [\"alpha\" \"beta\"] :prefs {:pins [1 2] :theme \"dark\"}}]]");
      assert_equal_query
        "SQLite restored db reads nested vector values out of map datom values"
        [ [ Result_value (Vector [ Int 1; Int 2 ]) ] ]
        (q_string
           restored_db
           "[:find ?pins :where [?e :profile ?profile] [(get ?profile :prefs) ?prefs] [(get ?prefs :pins) ?pins]]");
      assert_equal_query
        "SQLite restored db uses map datom values in AVET lookups"
        [ [ Result_entity 1 ] ]
        (q_string
           restored_db
           "[:find ?e :where [?e :profile {:prefs {:theme \"dark\" :pins [1 2]} :tags [\"alpha\" \"beta\"]}]]"))

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
        "SQLite restored db exposes current name facts"
        [ 1, "name", String "Petr", true ]
        (datoms db Eavt ~a:"name" ());
      assert_equal_triples
        "SQLite restored db exposes current no-history facts"
        [ 1, "secret", String "beta" ]
        (datoms db Eavt ~a:"secret" ());
      assert_equal_query
        "SQLite restored active db keeps latest no-history value"
        [ [ Result_value (String "beta") ] ]
        (q_string db "[:find ?secret :where [?e :name \"Petr\"] [?e :secret ?secret]]"))

let test_sqlite_storage_backed_transact_cljc_batch_after_restore () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed transact.cljc batch: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let conn =
        create_conn
          ~schema:
            [ "name", unique_identity
            ; "age", indexed
            ; "aka", many
            ; "friend", ref_attr
            ; "created-at", ref_attr
            ; "tx/source", indexed
            ; "label", many
            ]
          ~storage
          ()
      in
      ignore
        (transact_conn
           conn
           [ Entity
               { db_id = Some (Entity_id 1)
               ; attrs =
                   [ "name", One_value (String "Ivan")
                   ; "age", One_value (Int 15)
                   ; "aka", Many_values [ String "Devil"; String "Tupen" ]
                   ; "friend", One_value (Ref 2)
                   ; "created-at", One_value TxRef
                   ]
               }
           ; Entity
               { db_id = Some (Entity_id 2)
               ; attrs = [ "name", One_value (String "Petr"); "age", One_value (Int 37) ]
               }
           ; Add (CurrentTx, "tx/source", String "initial")
           ; Call (fun _ -> [ Entity { db_id = None; attrs = [ "name", One_value (String "Generated") ] } ])
           ]);
      ignore
        (transact_conn
           conn
           [ CompareAndSet (Entity_id 1, "age", Some (Int 15), Int 16)
           ; CompareAndSet (Entity_id 1, "label", None, String "fresh")
           ; Retract (Entity_id 1, "aka", Some (String "Devil"))
           ]);
      let restored =
        match restore_conn storage with
        | Some conn -> conn
        | None -> failwith "SQLite storage should restore transact.cljc batch connection"
      in
      assert_equal_query
        "SQLite restored db keeps cardinality-one replacement and CAS results"
        [ [ Result_value (String "Ivan"); Result_value (Int 16); Result_value (String "fresh") ] ]
        (q_string
           (conn_db restored)
           "[:find ?name ?age ?label
             :where [1 :name ?name]
                    [1 :age ?age]
                    [1 :label ?label]]");
      assert_equal_query
        "SQLite restored db keeps cardinality-many retraction results"
        [ [ Result_value (String "Tupen") ] ]
        (q_string (conn_db restored) "[:find ?aka :where [1 :aka ?aka]]");
      assert_equal_query
        "SQLite restored db can query current tx facts from transacted refs"
        [ [ Result_value (String "initial") ] ]
        (q_string
           (conn_db restored)
           "[:find ?source
             :where [1 :created-at ?tx]
                    [?tx :tx/source ?source]]");
      assert_equal_query
        "SQLite restored db persists transaction function entity output"
        [ [ Result_entity 3 ] ]
        (q_string (conn_db restored) "[:find ?e :where [?e :name \"Generated\"]]");
      let second_report =
        transact_conn
          restored
          [ RetractAttr (Entity_id 1, "aka")
          ; RetractEntity (Entity_id 2)
          ; Entity
              { db_id = Some (Temp_id "oleg")
              ; attrs =
                  [ "name", One_value (String "Oleg")
                  ; "created-at", One_value TxRef
                  ]
              }
          ; Add (CurrentTx, "tx/source", String "second")
          ]
      in
      let oleg_id =
        match resolve_tempid second_report.tempids "oleg" with
        | Some entity_id -> entity_id
        | None -> failwith "SQLite storage-backed transact should expose tempids after restore"
      in
      let db =
        match restore storage with
        | Some db -> db
        | None -> failwith "SQLite storage should restore transact.cljc batch db"
      in
      assert_equal_query
        "SQLite second restore persists retractAttribute and retractEntity effects"
        [ [ Result_entity 1 ]; [ Result_entity 3 ]; [ Result_entity oleg_id ] ]
        (q_string db "[:find ?e :where [?e :name]]");
      assert_equal_triples
        "SQLite second restore removes incoming refs to retracted entities"
        []
        (datoms db Eavt ~e:1 ~a:"friend" ());
      assert_equal_query
        "SQLite second restore queries tempid entity current-tx facts"
        [ [ Result_value (String "second") ] ]
        (q_string
           db
           "[:find ?source
             :where [?e :name \"Oleg\"]
                    [?e :created-at ?tx]
                    [?tx :tx/source ?source]]"))

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
      assert_upstream_storage_addresses
        "SQLite reset schema compacts stale tail"
        (storage_addresses storage))

let test_sqlite_storage_backed_aggregates_and_upserts_after_restore () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed aggregate/upsert test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let conn =
        create_conn
          ~schema:
            [ "name", unique_identity
            ; "email", unique_identity
            ; "slug", unique_identity
            ; "group", indexed
            ; "score", indexed
            ; "name+email", tuple_unique_identity [ "name"; "email" ]
            ]
          ~storage
          ()
      in
      ignore
        (transact_conn
           conn
           [ Entity
               { db_id = None
               ; attrs =
                   [ "name", One_value (String "Ivan")
                   ; "email", One_value (String "ivan@example.com")
                   ; "slug", One_value (String "ivan")
                   ; "group", One_value (String "red")
                   ; "score", One_value (Int 10)
                   ]
               }
           ; Entity
               { db_id = None
               ; attrs =
                   [ "name", One_value (String "Petr")
                   ; "email", One_value (String "petr@example.com")
                   ; "slug", One_value (String "petr")
                   ; "group", One_value (String "red")
                   ; "score", One_value (Int 20)
                   ]
               }
           ; Entity
               { db_id = None
               ; attrs =
                   [ "name", One_value (String "Oleg")
                   ; "email", One_value (String "oleg@example.com")
                   ; "slug", One_value (String "oleg")
                   ; "group", One_value (String "blue")
                   ; "score", One_value (Int 5)
                   ]
               }
           ]);
      let restored =
        match restore_conn storage with
        | Some conn -> conn
        | None -> failwith "SQLite storage should restore conn for aggregate/upsert test"
      in
      assert_equal_query
        "SQLite restored db supports grouped aggregate queries"
        [ [ Result_value (String "blue"); Result_value (Int 1); Result_value (Int 5) ]
        ; [ Result_value (String "red"); Result_value (Int 2); Result_value (Int 30) ]
        ]
        (q_string
           (conn_db restored)
           "[:find ?group (count ?e) (sum ?score)
             :where [?e :group ?group]
                    [?e :score ?score]]");
      ignore
        (transact_conn
           restored
           [ Entity
               { db_id = None
               ; attrs =
                   [ "name", One_value (String "Ivan")
                   ; "email", One_value (String "ivan+updated@example.com")
                   ; "score", One_value (Int 15)
                   ]
               }
           ; Add (Temp_id "petr", "name", String "Petr")
           ; Add (Temp_id "petr", "score", Int 25)
           ; Add (Temp_id "oleg", "name", String "Oleg")
           ; Add (Temp_id "oleg", "email", String "oleg@example.com")
           ; Add (Temp_id "oleg", "group", String "green")
           ]);
      let db =
        match restore storage with
        | Some db -> db
        | None -> failwith "SQLite storage should restore aggregate/upsert db after transact"
      in
      assert_equal_query
        "SQLite restored db persists unique identity and tempid upserts"
        [ [ Result_entity 1
          ; Result_value (String "Ivan")
          ; Result_value (String "ivan+updated@example.com")
          ; Result_value (String "red")
          ; Result_value (Int 15)
          ]
        ; [ Result_entity 2
          ; Result_value (String "Petr")
          ; Result_value (String "petr@example.com")
          ; Result_value (String "red")
          ; Result_value (Int 25)
          ]
        ; [ Result_entity 3
          ; Result_value (String "Oleg")
          ; Result_value (String "oleg@example.com")
          ; Result_value (String "green")
          ; Result_value (Int 5)
          ]
        ]
        (q_string
           db
           "[:find ?e ?name ?email ?group ?score
             :where [?e :name ?name]
                    [?e :email ?email]
                    [?e :group ?group]
                    [?e :score ?score]]");
      assert_equal_triples
        "SQLite restored db persists tuple identity datoms after upserts"
        [ 1, "name+email", Tuple [ Some (String "Ivan"); Some (String "ivan+updated@example.com") ]
        ; 2, "name+email", Tuple [ Some (String "Petr"); Some (String "petr@example.com") ]
        ; 3, "name+email", Tuple [ Some (String "Oleg"); Some (String "oleg@example.com") ]
        ]
        (datoms db Eavt ~a:"name+email" ()))

let test_sqlite_storage_backed_parsed_transact_and_query_pull_parity () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed parsed transact/query-pull test: sqlite3 is not available"
  else
    with_temp_db (fun people_path ->
      with_temp_db (fun score_path ->
        let people_storage = Sqlite_storage.storage people_path in
        let score_storage = Sqlite_storage.storage score_path in
        let people_conn =
          create_conn
            ~schema:
              [ "name", unique_identity
              ; "email", unique_identity
              ; "age", indexed
              ; "aka", many
              ; "friend", ref_attr
              ; "friends", ref_many
              ; "profile", component
              ; "bio", indexed
              ; "name+email", tuple_unique_identity [ "name"; "email" ]
              ]
            ~storage:people_storage
            ()
        in
        let score_conn =
          create_conn ~schema:[ "email", unique_identity; "score", indexed ] ~storage:score_storage ()
        in
        ignore
          (transact_conn_string
             people_conn
             "[{:db/id -1
                :name \"Ivan\"
                :email \"ivan@example.com\"
                :age 25
                :aka [\"Vanya\" \"IV\"]
                :friend -2
                :profile {:bio \"engineer\"}}
               {:db/id -2
                :name \"Petr\"
                :email \"petr@example.com\"
                :age 44}
               {:db/id -3
                :name \"Oleg\"
                :email \"oleg@example.com\"
                :age 11
                :friends [-1 -2]}
               [:db/add datomic.tx :source \"parsed\"]
               {:db/id datascript.tx :kind \"datascript\"}]");
        ignore
          (transact_conn_string
             score_conn
             "[{:db/id 10 :email \"ivan@example.com\" :score 20}
               {:db/id 11 :email \"petr@example.com\" :score 40}
               {:db/id 12 :email \"oleg@example.com\" :score 5}]");
        assert_equal_triples
          "SQLite live parsed transacts derive tuple attrs before persistence"
          [ 1, "name+email", Tuple [ Some (String "Ivan"); Some (String "ivan@example.com") ]
          ; 2, "name+email", Tuple [ Some (String "Petr"); Some (String "petr@example.com") ]
          ; 4, "name+email", Tuple [ Some (String "Oleg"); Some (String "oleg@example.com") ]
          ]
          (datoms (conn_db people_conn) Eavt ~a:"name+email" ());
        let people =
          match restore people_storage with
          | Some db -> db
          | None -> failwith "SQLite storage should restore parsed people transactions"
        in
        let scores =
          match restore score_storage with
          | Some db -> db
          | None -> failwith "SQLite storage should restore parsed score transactions"
        in
        assert_equal_triples
          "SQLite parsed transacts persist derived tuple attrs"
          [ 1, "name+email", Tuple [ Some (String "Ivan"); Some (String "ivan@example.com") ]
          ; 2, "name+email", Tuple [ Some (String "Petr"); Some (String "petr@example.com") ]
          ; 4, "name+email", Tuple [ Some (String "Oleg"); Some (String "oleg@example.com") ]
          ]
          (datoms people Eavt ~a:"name+email" ());
        assert_equal_query
          "SQLite restored db queries derived tuple attrs with tuple function output"
          [ [ Result_value (String "Ivan") ] ]
          (q_string
             people
             "[:find ?name
               :where [(tuple \"Ivan\" \"ivan@example.com\") ?lookup]
                      [?e :name+email ?lookup]
                      [?e :name ?name]]");
        assert_equal_query
          "SQLite parsed transacts persist nested component maps"
          [ [ Result_value (String "engineer") ] ]
          (q_string
             people
             "[:find ?bio
               :where [?e :name \"Ivan\"]
                      [?e :profile ?profile]
                      [?profile :bio ?bio]]");
        assert_equal_query
          "SQLite parsed transacts resolve current-tx aliases"
          [ [ Result_value (String "parsed"); Result_value (String "datascript") ] ]
          (q_string
             people
             "[:find ?source ?kind
               :where [?tx :source ?source]
                      [?tx :kind ?kind]]");
        assert_equal_query
          "SQLite restored db supports relation input bindings after parsed transact"
          [ [ Result_value (String "Ivan"); Result_value (Int 25) ]
          ; [ Result_value (String "Petr"); Result_value (Int 44) ]
          ]
          (q_string
             ~inputs:
               [ Arg_relation
                   [ [ Result_value (String "Ivan"); Result_value (Int 18) ]
                   ; [ Result_value (String "Petr"); Result_value (Int 18) ]
                   ; [ Result_value (String "Oleg"); Result_value (Int 18) ]
                   ]
               ]
             people
             "[:find ?name ?age
               :in $ [[?name ?min-age]]
               :where [?e :name ?name]
                      [?e :age ?age]
                      [(>= ?age ?min-age)]]");
        if
          q_return_string
            ~inputs:[ Arg_scalar (Result_value (List [ Keyword "name" ])) ]
            people
            "[:find (pull ?e ?pattern) .
              :in $ ?pattern
              :where [?e :email \"ivan@example.com\"]]"
          <> Query_scalar
               (Some
                  (Result_pull
                     { pulled_id = 1
                     ; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Ivan") ]
                     }))
        then failwith "SQLite restored db should support pull find specs with pattern inputs";
        if
          q_return_string
            ~inputs:[ Arg_scalar (Result_value (List [ Keyword "name" ])) ]
            people
            "[:find (pull ?e pattern) .
              :in $ pattern
              :where [(ground 1) ?e]]"
          <> Query_scalar
               (Some
                  (Result_pull
                     { pulled_id = 1
                     ; pulled_attrs = [ Keyword "name", Pulled_scalar (String "Ivan") ]
                     }))
        then failwith "SQLite restored db should support symbolic pull pattern inputs";
        assert_equal_query
          "SQLite restored db supports pull with lookup-ref collection inputs"
          [ [ Result_value (Ref_to (Lookup_ref ("name", String "Ivan")))
            ; Result_value (Int 25)
            ; Result_pull
                { pulled_id = 1
                ; pulled_attrs =
                    [ Keyword "db/id", Pulled_scalar (Int 1)
                    ; Keyword "name", Pulled_scalar (String "Ivan")
                    ]
                }
            ]
          ; [ Result_value (Ref_to (Lookup_ref ("name", String "Petr")))
            ; Result_value (Int 44)
            ; Result_pull
                { pulled_id = 2
                ; pulled_attrs =
                    [ Keyword "db/id", Pulled_scalar (Int 2)
                    ; Keyword "name", Pulled_scalar (String "Petr")
                    ]
                }
            ]
          ]
          (q_string
             ~inputs:
               [ Arg_collection
                   [ Result_value (Ref_to (Lookup_ref ("name", String "Ivan")))
                   ; Result_value (Ref_to (Lookup_ref ("name", String "Oleg")))
                   ; Result_value (Ref_to (Lookup_ref ("name", String "Petr")))
                   ]
               ]
             people
             "[:find ?ref ?age (pull ?ref [:db/id :name])
               :in $ [?ref ...]
               :where [?ref :age ?age]
                      [(>= ?age 18)]]");
        assert_equal_query
          "SQLite restored named sources use source-specific pull contexts"
          [ [ Result_value (String "Ivan")
            ; Result_pull
                { pulled_id = 10
                ; pulled_attrs = [ Keyword "score", Pulled_scalar (Int 20) ]
                }
            ]
          ; [ Result_value (String "Petr")
            ; Result_pull
                { pulled_id = 11
                ; pulled_attrs = [ Keyword "score", Pulled_scalar (Int 40) ]
                }
            ]
          ]
          (q_sources_string
             people
             [ "scores", Db_source scores ]
             "[:find ?name (pull $scores ?row [:score])
               :in $ $scores
               :where [?person :email ?email]
                      [?person :name ?name]
                      [$scores ?row :email ?email]
                      [$scores ?row :score ?score]
                      [(>= ?score 20)]]")))

let test_sqlite_storage_backed_query_input_maps_after_restore () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping SQLite storage-backed query input map test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let storage = Sqlite_storage.storage db_path in
      let conn =
        create_conn
          ~schema:[ "name", unique_identity; "age", indexed; "score", indexed ]
          ~storage
          ()
      in
      ignore
        (transact_conn
           conn
           [ Add (Entity_id 1, "name", String "Ivan")
           ; Add (Entity_id 1, "age", Int 25)
           ; Add (Entity_id 1, "score", Int 4)
           ; Add (Entity_id 2, "name", String "Petr")
           ; Add (Entity_id 2, "age", Int 44)
           ; Add (Entity_id 2, "score", Int 7)
           ; Add (Entity_id 3, "name", String "Oleg")
           ; Add (Entity_id 3, "age", Int 11)
           ; Add (Entity_id 3, "score", Int 2)
           ]);
      let restored =
        match restore_conn storage with
        | Some conn -> conn
        | None -> failwith "SQLite storage should restore conn for query input map test"
      in
      ignore (transact_conn restored [ Add (Lookup_ref ("name", String "Oleg"), "age", Int 18) ]);
      let db =
        match restore storage with
        | Some db -> db
        | None -> failwith "SQLite storage should restore db after query input map transact"
      in
      assert_equal_query
        "SQLite restored db joins plain map relation inputs after transact"
        [ [ Result_value (String "Ivan"); Result_value (Int 25) ]
        ; [ Result_value (String "Oleg"); Result_value (Int 18) ]
        ; [ Result_value (String "Petr"); Result_value (Int 44) ]
        ]
        (q_string
           ~inputs:
             [ Arg_scalar
                 (Result_value
                    (Map
                       [ String "Ivan", Int 18
                       ; String "Oleg", Int 18
                       ; String "Petr", Int 18
                       ]))
             ]
           db
           "[:find ?name ?age
             :in $ [[?name ?min-age] ...]
             :where [?e :name ?name]
                    [?e :age ?age]
                    [(>= ?age ?min-age)]]");
      let minmax = function
        | [ Result_value (List values) ] ->
          (match values with
           | [] -> None
           | first :: rest ->
             let min_value, max_value =
               List.fold_left
                 (fun (min_value, max_value) -> function
                    | Int value -> min min_value value, max max_value value
                    | _ -> min_value, max_value)
                 (match first with
                  | Int value -> value, value
                  | _ -> 0, 0)
                 rest
             in
             Some [ Result_value (Int min_value); Result_value (Int max_value) ])
        | _ -> None
      in
      assert_equal_query
        "SQLite restored db joins map relation rows through dynamic tuple outputs"
        [ [ Result_value (String "Ivan"); Result_value (Int 1); Result_value (Int 4) ]
        ; [ Result_value (String "Petr"); Result_value (Int 5); Result_value (Int 7) ]
        ]
        (q_string
           ~inputs:
             [ Arg_scalar
                 (Result_value
                    (Map
                       [ String "Ivan", List [ Int 1; Int 4 ]
                       ; String "Petr", List [ Int 5; Int 7 ]
                       ; String "Oleg", List [ Int 2; Int 2 ]
                       ]))
             ; Arg_function minmax
             ]
           db
           "[:find ?name ?min ?max
             :in $ [[?name ?scores] ...] ?minmax
             :where [?e :name ?name]
                    [?e :score ?score]
                    [(?minmax ?scores) [?min ?max]]
                    [(= ?score ?max)]
                    [(> ?max ?min)]]");
      let range_values = function
        | [ Result_value (Int min_value); Result_value (Int max_value) ] ->
          let rec collect value acc =
            if value >= max_value then List.rev acc
            else collect (value + 1) (Int value :: acc)
          in
          Some [ Result_value (List (collect min_value [])) ]
        | _ -> None
      in
      assert_equal_query
        "SQLite restored db joins nested map relation rows through dynamic collection outputs"
        [ [ Result_value (String "Ivan"); Result_value (Int 2) ]
        ; [ Result_value (String "Ivan"); Result_value (Int 4) ]
        ; [ Result_value (String "Petr"); Result_value (Int 6) ]
        ]
        (q_string
           ~inputs:
             [ Arg_scalar
                 (Result_value
                    (Map
                       [ String "Ivan", List [ Int 1; Int 5 ]
                       ; String "Petr", List [ Int 6; Int 8 ]
                       ; String "Oleg", List [ Int 3; Int 4 ]
                       ]))
             ; Arg_function range_values
             ]
           db
           "[:find ?name ?candidate
             :in $ [[?name [?min ?max]] ...] ?range
             :where [?e :name ?name]
                    [?e :age ?age]
                    [(?range ?min ?max) [?candidate ...]]
                    [(even? ?candidate)]
                    [(< ?candidate ?age)]]");
      assert_equal_query
        "SQLite restored db accepts input-only queries with no db source"
        [ [ Result_value (Int 10); Result_value (Int 20) ] ]
        (q_string
           ~inputs:[ Arg_scalar (Result_value (Int 10)); Arg_scalar (Result_value (Int 20)) ]
           db
           "[:find ?a ?b :in ?a ?b]"))

let test_logseq_sqlite_import_preserves_clojure_collection_values () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping Logseq SQLite collection value import test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let content =
        {|["^ ","~:keys",[[101,"~:item/vector",[1,2],536870913],[102,"~:item/list",["~#list",[1,2]],536870913],[103,"~:item/profile",["^ ","~:tags",["alpha","beta"],"~:prefs",["^ ","~:pins",[1,2]]],536870913]]]|}
      in
      ignore
        (run_sql
           db_path
           ("create table kvs (addr INTEGER primary key, content TEXT, addresses JSON);\n"
            ^ "insert into kvs (addr, content, addresses) values (2, "
            ^ sql_quote content
            ^ ", '[]');"));
      let datoms = Sqlite_storage.datoms_of_logseq_graph ~read_only:true db_path in
      assert_equal_triples
        "Logseq SQLite import preserves vector/list/map value shapes"
        [ 101, "item/vector", Vector [ Int 1; Int 2 ]
        ; 102, "item/list", List [ Int 1; Int 2 ]
        ; ( 103
          , "item/profile"
          , Map
              [ Keyword "tags", Vector [ String "alpha"; String "beta" ]
              ; Keyword "prefs", Map [ Keyword "pins", Vector [ Int 1; Int 2 ] ]
              ] )
        ]
        datoms;
      let db = init_db ~schema:[ "item/vector", indexed; "item/profile", indexed ] datoms in
      assert_equal_query
        "Logseq SQLite imported vectors query as Clojure vectors"
        [ [ Result_entity 101 ] ]
        (q_string db "[:find ?e :where [?e :item/vector [1 2]]]");
      assert_equal_query
        "Logseq SQLite imported nested map vectors query structurally"
        [ [ Result_value (Vector [ Int 1; Int 2 ]) ] ]
        (q_string
           db
           "[:find ?pins :where [?e :item/profile ?profile] [(get ?profile :prefs) ?prefs] [(get ?prefs :pins) ?pins]]"))

let test_logseq_sqlite_datom_cache_ignores_uuid_values () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping Logseq SQLite datom cache test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let content =
        {|["^ ","~:keys",[[95,"~:block/updated-at",1778143747441,536870913],[95,"~:block/uuid","~u00000002-2073-3937-9700-000000000000",536870913],[95,"~:db/ident","~:logseq.property.repeat/recur-unit.month",536870913],[95,"~:logseq.property/built-in?",true,536870913],[95,"~:logseq.property/created-from-property",90,536870913],[96,"~:block/closed-value-property",90,536870913],[96,"~:block/created-at",1778143747441,536870913],[96,"~:block/order","b0N",536870913],[96,"~:block/page",90,536870913],[96,"~:block/parent",90,536870913],[96,"~:block/title","Year",536870913],[96,"^1",1778143747441,536870913],[96,"^2","~u00000002-1520-4385-2400-000000000000",536870913],[96,"^3","~:logseq.property.repeat/recur-unit.year",536870913],[96,"^5",true,536870913],[96,"^6",90,536870913],[97,"^8",1778143747442,536870913],[97,"~:block/name","node repeats?",536870913]]]|}
      in
      ignore
        (run_sql
           db_path
           ("create table kvs (addr INTEGER primary key, content TEXT, addresses JSON);\n"
            ^ "insert into kvs (addr, content, addresses) values (2, "
            ^ sql_quote content
            ^ ", '[]');"));
      let datoms = Sqlite_storage.datoms_of_logseq_graph ~read_only:true db_path in
      assert_equal_triples
        "Logseq datom cache codes should not be shifted by UUID values"
        [ 97, "block/created-at", Int 1778143747442 ]
        (List.filter (fun datom -> datom.e = 97 && datom.v = Int 1778143747442) datoms))

let test_logseq_sqlite_datom_cache_ignores_transit_tag_values () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping Logseq SQLite datom tag cache test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let content =
        {|["^ ","~:keys",[[1,"~:prop/set",["~#set",["~:alpha"]],536870913],[2,"~:block/created-at",1000,536870913],[3,"^3",2000,536870913]]]|}
      in
      ignore
        (run_sql
           db_path
           ("create table kvs (addr INTEGER primary key, content TEXT, addresses JSON);\n"
            ^ "insert into kvs (addr, content, addresses) values (2, "
            ^ sql_quote content
            ^ ", '[]');"));
      assert_equal_triples
        "Logseq datom cache codes should not be shifted by Transit tags"
        [ 2, "block/created-at", Int 1000; 3, "block/created-at", Int 2000 ]
        (Sqlite_storage.datoms_of_logseq_graph ~read_only:true db_path
         |> List.filter (fun datom -> datom.a = "block/created-at")))

let test_logseq_sqlite_datom_cache_spans_ordered_rows () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping Logseq SQLite datom row cache test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let first_row =
        {|["^ ","~:keys",[[1,"~:block/created-at",1000,536870913]]]|}
      in
      let second_row =
        {|["^ ","^0",[[2,"^1",2000,536870913]]]|}
      in
      ignore
        (run_sql
           db_path
           ("create table kvs (addr INTEGER primary key, content TEXT, addresses JSON);\n"
            ^ "insert into kvs (addr, content, addresses) values (2, "
            ^ sql_quote first_row
            ^ ", '[]');\n"
            ^ "insert into kvs (addr, content, addresses) values (3, "
            ^ sql_quote second_row
            ^ ", '[]');"));
      assert_equal_triples
        "Logseq datom cache codes should carry across SQLite rows in addr order"
        [ 1, "block/created-at", Int 1000; 2, "block/created-at", Int 2000 ]
        (Sqlite_storage.datoms_of_logseq_graph ~read_only:true db_path))

let test_logseq_sqlite_query_loads_matching_nodes_without_full_materialization () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping Logseq SQLite direct query test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let root_content =
        {|["^ ","~:schema",["^ ","~:block/name",["^ ","~:db/index",true],"~:block/title",["^ "],"~:block/created-at",["^ ","~:db/index",true]]] |}
      in
      let broken_unrelated_node = {|["^ ","~:keys",|} in
      let page_node =
        {|["^ ","~:keys",[[101,"~:block/name","alpha",536870913],[101,"~:block/title","Alpha",536870913],[101,"~:block/created-at",1000,536870913],[102,"~:block/name","beta",536870913],[102,"~:block/title","Beta",536870913],[102,"~:block/created-at",2000,536870913]]]|}
      in
      run_sql
        db_path
        ("create table kvs (addr INTEGER primary key, content TEXT, addresses JSON);\n"
         ^ "insert into kvs (addr, content, addresses) values (0, "
         ^ sql_quote root_content
         ^ ", '[]');\n"
         ^ "insert into kvs (addr, content, addresses) values (2, "
         ^ sql_quote broken_unrelated_node
         ^ ", '[]');\n"
         ^ "insert into kvs (addr, content, addresses) values (3, "
         ^ sql_quote page_node
         ^ ", '[]');");
      assert_equal_query
        "direct Logseq SQLite query should load only matching graph nodes"
        [ [ Result_value (Int 1000); Result_value (String "Alpha") ]
        ; [ Result_value (Int 2000); Result_value (String "Beta") ]
        ]
        (match
           Sqlite_storage.query_logseq_graph
             ~read_only:true
             db_path
             "[:find ?created ?title
               :where [?p :block/name ?name]
                      [?p :block/title ?title]
                      [?p :block/created-at ?created]]"
         with
         | Query_relation rows -> rows
         | _ -> failwith "direct Logseq SQLite query should return relation rows"))

let test_logseq_sqlite_query_treats_timestamp_attrs_as_scalars_when_schema_marks_refs () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping Logseq SQLite timestamp schema query test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let root_content =
        {|["^ ","~:schema",["^ ","~:block/name",["^ ","~:db/index",true],"~:block/title",["^ "],"~:block/created-at",["^ ","~:db/index",true,"~:db/valueType","~:db.type/ref"],"~:block/updated-at",["^ ","^2",true,"^5","^6"],"~:logseq.kv/graph-created-at",["^ ","^2",true,"^5","^6"]]] |}
      in
      let page_node =
        {|["^ ","~:keys",[[101,"~:block/name","lambda",536870913],[101,"~:block/title","Lambda",536870913],[101,"~:block/created-at",1743432598614,536870913],[101,"~:block/updated-at",1743432616414,536870913],[101,"~:logseq.kv/graph-created-at",1747740706964,536870913]]]|}
      in
      run_sql
        db_path
        ("create table kvs (addr INTEGER primary key, content TEXT, addresses JSON);\n"
         ^ "insert into kvs (addr, content, addresses) values (0, "
         ^ sql_quote root_content
         ^ ", '[]');\n"
         ^ "insert into kvs (addr, content, addresses) values (2, "
         ^ sql_quote page_node
         ^ ", '[]');");
      assert_equal_query
        "direct Logseq SQLite query should keep timestamp attrs as scalar values"
        [ [ Result_value (String "Lambda")
          ; Result_value (Int 1743432598614)
          ; Result_value (Int 1743432616414)
          ; Result_value (Int 1747740706964)
          ]
        ]
        (match
           Sqlite_storage.query_logseq_graph
             ~read_only:true
             db_path
             "[:find ?title ?created ?updated ?graph-created
               :where [?p :block/name ?name]
                      [?p :block/title ?title]
                      [?p :block/created-at ?created]
                      [?p :block/updated-at ?updated]
                      [?p :logseq.kv/graph-created-at ?graph-created]]"
         with
         | Query_relation rows -> rows
         | _ -> failwith "direct Logseq SQLite query should return relation rows"))

let logseq_schema_attr_json attr schema =
  let props =
    [ Some ("~:db/cardinality", (match schema.cardinality with Many -> "~:db.cardinality/many" | One -> "~:db.cardinality/one"))
    ; (match schema.unique with
       | Some Identity -> Some ("~:db/unique", "~:db.unique/identity")
       | Some Value -> Some ("~:db/unique", "~:db.unique/value")
       | None -> None)
    ; if schema.indexed then Some ("~:db/index", "true") else None
    ; if schema.is_component then Some ("~:db/isComponent", "true") else None
    ; if schema.no_history then Some ("~:db/noHistory", "true") else None
    ; (match schema.value_type with
       | Some RefType -> Some ("~:db/valueType", "~:db.type/ref")
       | Some StringType -> Some ("~:db/valueType", "~:db.type/string")
       | Some KeywordType -> Some ("~:db/valueType", "~:db.type/keyword")
       | Some NumberType -> Some ("~:db/valueType", "~:db.type/number")
       | Some UuidType -> Some ("~:db/valueType", "~:db.type/uuid")
       | Some InstantType -> Some ("~:db/valueType", "~:db.type/instant")
       | Some TupleType -> Some ("~:db/valueType", "~:db.type/tuple")
       | None -> None)
    ]
    |> List.filter_map Fun.id
    |> List.concat_map (fun (key, value) ->
      [ json_quote key; if value = "true" then value else json_quote value ])
  in
  [ json_quote ("~:" ^ attr); "[" ^ String.concat "," (json_quote "^ " :: props) ^ "]" ]

let logseq_root_content schema =
  let schema_entries = List.concat_map (fun (attr, schema) -> logseq_schema_attr_json attr schema) schema in
  "["
  ^ String.concat
      ","
      [ json_quote "^ "
      ; json_quote "~:schema"
      ; "[" ^ String.concat "," (json_quote "^ " :: schema_entries) ^ "]"
      ; json_quote "~:max-eid"
      ; "1000"
      ; json_quote "~:max-tx"
      ; "536870913"
      ; json_quote "~:eavt"
      ; "2"
      ; json_quote "~:aevt"
      ; "3"
      ; json_quote "~:avet"
      ; "4"
      ]
  ^ "]"

let logseq_json_of_value = function
  | String value -> json_quote value
  | Int value -> string_of_int value
  | Bool value -> if value then "true" else "false"
  | Keyword value -> json_quote ("~:" ^ value)
  | Ref entity_id -> string_of_int entity_id
  | value -> failf "unsupported Logseq test value: %s" (string_of_value value)

let logseq_row_content datoms =
  let datom_json datom =
    "["
    ^ String.concat
        ","
        [ string_of_int datom.e
        ; json_quote ("~:" ^ datom.a)
        ; logseq_json_of_value datom.v
        ; string_of_int datom.tx
        ]
    ^ "]"
  in
  "["
  ^ String.concat
      ","
      [ json_quote "^ "
      ; json_quote "~:keys"
      ; "[" ^ String.concat "," (List.map datom_json datoms) ^ "]"
      ]
  ^ "]"

let insert_logseq_rows db_path rows =
  run_sql
    db_path
    ("create table kvs (addr INTEGER primary key, content TEXT, addresses JSON);\n"
     ^ (rows
        |> List.map (fun (addr, content) ->
          Printf.sprintf
            "insert into kvs (addr, content, addresses) values (%d, %s, '[]');\n"
            addr
            (sql_quote content))
        |> String.concat ""))

let test_logseq_sqlite_generated_graph_queries_transacted_properties_and_blocks () =
  if not (sqlite3_available ()) then
    prerr_endline "Skipping generated Logseq SQLite query test: sqlite3 is not available"
  else
    with_temp_db (fun db_path ->
      let schema =
        [ "db/ident", unique_identity
        ; "block/name", unique_identity
        ; "block/title", indexed
        ; "block/tags", ref_many
        ; "block/page", ref_attr
        ; "block/created-at", indexed
        ; "block/updated-at", indexed
        ; "block/order", indexed
        ; "logseq.property/type", indexed
        ; "logseq.property/public?", indexed
        ; "user/priority", indexed
        ]
      in
      let report =
        transact
          (empty_db ~schema ())
          [ Add (Entity_id 100, "db/ident", Keyword "logseq.class/Property")
          ; Add (Entity_id 100, "block/title", String "Property")
          ; Add (Entity_id 100, "block/name", String "property")
          ; Add (Entity_id 101, "db/ident", Keyword "logseq.class/Page")
          ; Add (Entity_id 101, "block/title", String "Page")
          ; Add (Entity_id 101, "block/name", String "page")
          ; Add (Entity_id 200, "db/ident", Keyword "user/priority")
          ; Add (Entity_id 200, "block/title", String "Priority")
          ; Add (Entity_id 200, "block/name", String "priority")
          ; Add (Entity_id 200, "block/tags", Keyword "logseq.class/Property")
          ; Add (Entity_id 200, "logseq.property/type", Keyword "default")
          ; Add (Entity_id 200, "logseq.property/public?", Bool true)
          ; Add (Entity_id 300, "block/title", String "Project Alpha")
          ; Add (Entity_id 300, "block/name", String "project alpha")
          ; Add (Entity_id 300, "block/tags", Keyword "logseq.class/Page")
          ; Add (Entity_id 400, "block/title", String "Ship generated sqlite")
          ; Add (Entity_id 400, "block/page", Ref 300)
          ; Add (Entity_id 400, "block/order", String "a0")
          ; Add (Entity_id 400, "block/created-at", Int 1781829000000)
          ; Add (Entity_id 400, "block/updated-at", Int 1781829297990)
          ; Add (Entity_id 400, "user/priority", String "high")
          ]
      in
      insert_logseq_rows
        db_path
        [ 0, logseq_root_content schema
        ; 1, "[]"
        ; 2, logseq_row_content report.tx_data
        ];
      assert_equal_query
        "generated Logseq sqlite should query property pages"
        [ [ Result_value (String "Priority")
          ; Result_value (Keyword "user/priority")
          ; Result_value (Keyword "default")
          ]
        ]
        (match
           Sqlite_storage.query_logseq_graph
             ~read_only:true
             db_path
             "[:find ?title ?ident ?type
               :where [?p :block/tags :logseq.class/Property]
                      [?p :block/title ?title]
                      [?p :db/ident ?ident]
                      [?p :logseq.property/type ?type]]"
         with
         | Query_relation rows -> rows
         | _ -> failwith "generated Logseq property query should return relation rows");
      assert_equal_query
        "generated Logseq sqlite should query transacted blocks with custom properties"
        [ [ Result_value (String "Ship generated sqlite")
          ; Result_value (String "high")
          ; Result_value (Int 1781829297990)
          ]
        ]
        (match
           Sqlite_storage.query_logseq_graph
             ~read_only:true
             db_path
             "[:find ?title ?priority ?updated
               :where [?b :block/title ?title]
                      [?b :user/priority ?priority]
                      [?b :block/updated-at ?updated]]"
         with
         | Query_relation rows -> rows
         | _ -> failwith "generated Logseq block query should return relation rows"))

let rec find_repo_root dir =
  if Sys.file_exists (Filename.concat dir "dune-project") then dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then failf "could not find repo root from %s" (Sys.getcwd ())
    else find_repo_root parent

let repo_root =
  find_repo_root (Sys.getcwd ())

let default_logseq_graph_db =
  match Sys.getenv_opt "LOGSEQ_GRAPH_DB" with
  | Some path when path <> "" -> path
  | _ -> Filename.concat repo_root "db.sqlite"

let logseq_graphs_dir =
  Sys.getenv_opt "LOGSEQ_GRAPHS_DIR"

let logseq_graph_dbs () =
  match logseq_graphs_dir with
  | Some dir when dir <> "" -> Sqlite_storage.graph_db_paths dir
  | _ -> if Sys.file_exists default_logseq_graph_db then [ default_logseq_graph_db ] else []

let test_default_logseq_graph_db_uses_portable_default () =
  match Sys.getenv_opt "LOGSEQ_GRAPH_DB" with
  | Some path when path <> "" -> assert_equal "configured Logseq graph db" path default_logseq_graph_db
  | _ ->
    assert_equal "default Logseq graph db file name" "db.sqlite" (Filename.basename default_logseq_graph_db);
    if not (Sys.file_exists (Filename.concat (Filename.dirname default_logseq_graph_db) "dune-project")) then
      failf "default Logseq graph db should live in the repo root: %s" default_logseq_graph_db

let test_logseq_graph_dbs_uses_portable_default () =
  match logseq_graphs_dir with
  | Some dir when dir <> "" ->
    ignore (Sqlite_storage.graph_db_paths dir : string list)
  | _ ->
    if Sys.file_exists default_logseq_graph_db then
      match logseq_graph_dbs () with
      | [ db_path ] -> assert_equal "Logseq graph db path" default_logseq_graph_db db_path
      | db_paths ->
        failf
          "Logseq graph dbs should contain only repo-root db.sqlite, got %d paths"
          (List.length db_paths)

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

let assert_logseq_timestamp_attrs_are_not_refs schema =
  List.iter
    (fun attr ->
      match List.assoc_opt attr schema with
      | Some { value_type = Some RefType; _ } ->
        failf "%s should not decode as a ref schema attr" attr
      | Some _ -> ()
      | None -> failf "Logseq graph schema should expose :%s" attr)
    [ "block/created-at"; "block/updated-at" ]

let max_supported_entity_id = 2_147_483_647

let unsupported_entity_id entity_id =
  entity_id < 0 || entity_id > max_supported_entity_id

let datom_has_unsupported_entity_id schema datom =
  unsupported_entity_id datom.e
  ||
  match List.assoc_opt datom.a schema, datom.v with
  | _, Ref entity_id -> unsupported_entity_id entity_id
  | Some { value_type = Some RefType; _ }, Int _
  | _ -> false

let find_unsupported_entity_id_datoms schema datoms =
  List.find_opt (datom_has_unsupported_entity_id schema) datoms

let test_existing_logseq_graph_full_datoms_support_query () =
  if (not (sqlite3_available ())) || not (Sys.file_exists default_logseq_graph_db) then
    prerr_endline "Skipping full Logseq graph datom query smoke: sqlite3 or demo graph is unavailable"
  else
    let before = (Unix.stat default_logseq_graph_db).Unix.st_mtime in
    let schema = Sqlite_storage.schema_of_logseq_graph ~read_only:true default_logseq_graph_db in
    assert_logseq_timestamp_attrs_are_not_refs schema;
    let datoms = Sqlite_storage.datoms_of_logseq_graph ~read_only:true default_logseq_graph_db in
    let after = (Unix.stat default_logseq_graph_db).Unix.st_mtime in
    (match find_unsupported_entity_id_datoms schema datoms with
     | Some datom ->
       Printf.eprintf
         "Skipping full Logseq graph datom query smoke: datom entity id %d or ref value exceeds supported max %d\n"
         datom.e
         max_supported_entity_id
     | None ->
       let db = init_db ~schema datoms in
       assert_equal_int
         "query decoded full Logseq graph datoms"
         1
         (List.length (q_string db "[:find ?e :where [?e :block/name \"root tag\"]]")));
    if before <> after then failwith "read-only full datom loading should not modify the graph file"

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
  test_sqlite_storage_raw_layout_after_transact ();
  test_sqlite_storage_does_not_require_sqlite3_binary ();
  test_sqlite_storage_store_and_delete_are_separate ();
  test_sqlite_storage_backed_connections_query_and_transact_after_restore ();
  test_sqlite_storage_backed_connections_filter_entity_rules_and_repeated_transacts ();
  test_sqlite_storage_backed_connections_index_query_and_transact_parity ();
  test_sqlite_storage_backed_composite_values_after_restore ();
  test_sqlite_storage_backed_query_result_shapes_after_restore ();
  test_sqlite_storage_backed_lookup_ref_transacts_after_restore ();
  test_sqlite_storage_backed_not_or_queries_after_restore ();
  test_sqlite_storage_backed_transact_history_and_current_tx_parity ();
  test_sqlite_storage_backed_transact_cljc_batch_after_restore ();
  test_sqlite_storage_backed_pull_sources_and_relation_inputs_after_restore ();
  test_sqlite_storage_backed_reset_schema_and_compaction_parity ();
  test_sqlite_storage_backed_aggregates_and_upserts_after_restore ();
  test_sqlite_storage_backed_parsed_transact_and_query_pull_parity ();
  test_sqlite_storage_backed_query_input_maps_after_restore ();
  test_logseq_sqlite_import_preserves_clojure_collection_values ();
  test_logseq_sqlite_datom_cache_ignores_uuid_values ();
  test_logseq_sqlite_datom_cache_ignores_transit_tag_values ();
  test_logseq_sqlite_datom_cache_spans_ordered_rows ();
  test_logseq_sqlite_query_loads_matching_nodes_without_full_materialization ();
  test_logseq_sqlite_query_treats_timestamp_attrs_as_scalars_when_schema_marks_refs ();
  test_logseq_sqlite_generated_graph_queries_transacted_properties_and_blocks ();
  test_default_logseq_graph_db_uses_portable_default ();
  test_logseq_graph_dbs_uses_portable_default ();
  test_existing_logseq_graph_is_recognized_read_only ();
  test_all_existing_logseq_graphs_are_recognized_read_only ();
  test_existing_logseq_graph_schema_supports_query_and_transact ();
  test_all_existing_logseq_graph_schemas_support_query_and_transact ();
  test_existing_logseq_graph_datoms_support_query_and_transact ();
  test_existing_logseq_graph_full_datoms_support_query ();
  test_all_existing_logseq_graph_datoms_support_query_and_transact ()
