module D = Datascript
module A = Datascript_lg
module E = Lg_edn_backend

let check condition message = if not condition then failwith message

let ok = function
  | Ok value -> value
  | Error _ -> failwith "unexpected decoding error"

let error = function
  | Error _ -> ()
  | Ok _ -> failwith "expected decoding error"

let form = D.read_edn

let test_edn_boundary () =
  let input =
    E.Map
      [|
        (E.Keyword "shape/id", E.String "one");
        (E.Keyword "points", E.Vector [| E.Small_int 1; E.Float 2. |]);
      |]
  in
  check
    (ok (A.of_edn input) = form "{:shape/id \"one\" :points [1 2.0]}")
    "nested EDN";
  check (ok (A.of_edn (E.Int 42L)) = D.QueryFormInt 42L) "integer conversion";
  check (ok (A.of_edn (E.Int Int64.max_int)) = D.QueryFormInt Int64.max_int) "int64 max";
  check (ok (A.of_edn (E.Int Int64.min_int)) = D.QueryFormInt Int64.min_int) "int64 min";
  error (A.of_edn (E.Int4_array ([| 1 |], [||], [||], [||])));
  check
    (ok (A.of_edn (E.Tagged ("uuid", E.String "value")))
    = D.QueryFormTagged ("uuid", D.QueryFormString "value"))
    "tag preservation";
  check (ok (A.of_edn (E.Int_vector [| 1; 2 |])) = form "[1 2]") "packed vector";
  check
    (ok (A.of_edn (E.Seq (List.to_seq [ E.Bool true; E.Nil ])))
    = form "(true nil)")
    "sequence";
  check (ok (A.of_edn (E.Set [| E.String "a" |])) = form "#{\"a\"}") "set"

let test_typed_application () =
  let id =
    A.Attribute.make "shape/id" A.Codec.string
      (A.schema_spec (form "{:db/unique :db.unique/identity}"))
  in
  let doc =
    A.Attribute.make "shape/doc" A.Codec.string (A.schema_spec (form "{}"))
  in
  let x =
    A.Attribute.make "shape/x" A.Codec.float (A.schema_spec (form "{}"))
  in
  let conn =
    D.create_conn
      ~schema:
        (List.map Fun.id
           [
             A.Attribute.schema id; A.Attribute.schema doc; A.Attribute.schema x;
           ])
      ()
  in
  let tx name document coordinate =
    A.entity (Some (D.Temp_id name))
      [
        A.Attribute.entry id name;
        A.Attribute.entry doc document;
        A.Attribute.entry x coordinate;
      ]
  in
  ignore (D.transact_conn conn [ tx "a" "first" 1.; tx "b" "second" 2. ]);
  let query =
    A.prepare_query
      (A.Projection.column 0 A.Codec.string)
      (form
         "[:find ?id :in $ ?doc :where [?e :shape/doc ?doc] [?e :shape/id ?id]]")
  in
  check
    (ok (A.run_query query (D.db conn) [ A.input A.Codec.string "first" ])
    = [ "a" ])
    "query input one";
  check
    (ok (A.run_query query (D.db conn) [ A.input A.Codec.string "second" ])
    = [ "b" ])
    "query input reuse";
  check
    (ok (A.run_query query (D.db conn) [ A.input A.Codec.string "none" ]) = [])
    "empty query";
  let reference = D.Lookup_ref ("shape/id", D.String "a") in
  let entity = Option.get (D.entity (D.db conn) reference) in
  check (ok (A.Attribute.read_one x entity) = Some 1.) "typed entity";
  let absent =
    A.Attribute.make "missing" A.Codec.int (A.schema_spec (form "{}"))
  in
  check (ok (A.Attribute.read_one absent entity) = None) "absent entity value";
  let wrong =
    A.Attribute.make "shape/id" A.Codec.int (A.schema_spec (form "{}"))
  in
  error (A.Attribute.read_one wrong entity);
  let pair =
    A.prepare_query
      (A.Projection.pair
         (A.Projection.column 0 A.Codec.string)
         (A.Projection.column 1 A.Codec.float))
      (form "[:find ?id ?x :where [?e :shape/id ?id] [?e :shape/x ?x]]")
  in
  check
    (List.sort compare (ok (A.run_query pair (D.db conn) []))
    = [ ("a", 1.); ("b", 2.) ])
    "typed columns";
  let bad_query =
    A.prepare_query
      (A.Projection.column 0 A.Codec.int)
      (form "[:find ?id :where [?e :shape/id ?id]]")
  in
  error (A.run_query bad_query (D.db conn) []);
  error
    (A.Projection.run
       (A.Projection.column 1 A.Codec.string)
       [ D.Result_value (D.String "a") ]);
  let pulled =
    A.pull (D.db conn) [ D.Pull_attr "shape/x" ] reference (A.Pull.field x)
    |> ok
  in
  check (pulled = Some (Some 1.)) "typed pull";
  ignore (A.transact conn (form "[[:db/add [:shape/id \"a\"] :shape/x 3.0]]"));
  check
    (ok (A.Attribute.read_one x (Option.get (D.entity (D.db conn) reference)))
    = Some 3.)
    "lookup ref transaction";
  ignore (A.transact conn (form "[[:db/retractEntity [:shape/id \"b\"]]]"));
  check
    (ok (A.run_query query (D.db conn) [ A.input A.Codec.string "second" ]) = [])
    "retract";
  error (A.Codec.decode A.Codec.int (D.Float 1.5));
  check
    (ok (A.Codec.decode A.Codec.float (D.Int64 2L)) = 2.)
    "legacy integer coordinates"

let test_cardinality_and_pull_errors () =
  let many =
    A.Attribute.make "tags" A.Codec.string
      (A.schema_spec (form "{:db/cardinality :db.cardinality/many}"))
  in
  let conn = D.create_conn ~schema:[ A.Attribute.schema many ] () in
  ignore
    (D.transact_conn conn
       [
         A.entity (Some (D.Entity_id 1))
           [ A.Attribute.entries many [ "a"; "b" ] ];
       ]);
  let entity = Option.get (D.entity (D.db conn) (D.Entity_id 1)) in
  check
    (List.sort compare (ok (A.Attribute.read_many many entity)) = [ "a"; "b" ])
    "many values";
  error (A.Attribute.read_one many entity);
  error
    (A.pull (D.db conn) [ D.Pull_attr "tags" ] (D.Entity_id 1)
       (A.Pull.field many));
  let rejected =
    try
      ignore (A.Attribute.entry many "c");
      false
    with Invalid_argument _ -> true
  in
  check rejected "wrong cardinality write";
  let ids =
    A.prepare_query
      (A.Projection.column 0 A.Codec.entity_id)
      (form "[:find ?e :where [?e :tags \"a\"]]")
  in
  let actual_ids = ok (A.run_query ids (D.db conn) []) in
  check (actual_ids = [ 1 ]) "entity ID projection";
  let absent =
    A.Attribute.make "unknown" A.Codec.string (A.schema_spec (form "{}"))
  in
  check
    (ok
       (A.pull_form (D.db conn) (form "[:tags :unknown]") (D.Entity_id 1)
          (A.Pull.field absent))
    = Some None)
    "absent pull field";
  check
    (ok
       (A.pull_form (D.db conn) (form "[:unknown]") (D.Entity_id 99)
          (A.Pull.field absent))
    = None)
    "absent pull entity"

let () =
  test_edn_boundary ();
  test_typed_application ();
  test_cardinality_and_pull_errors ()
