open Datascript

let datoms_seq = datoms

let datoms db index ?e ?a ?v ?tx () =
  datoms_seq db index ?e ?a ?v ?tx () |> List.of_seq

let () =
  let db =
    empty_db ()
    |> db_with [ Add (Entity_id 1, "name", String "Ivan") ]
  in
  (match q_string db "[:find ?name :where [1 :name ?name]]" with
   | [ [ Result_value (String "Ivan") ] ] -> ()
   | _ -> failwith "JavaScript smoke query returned an unexpected result");
  let db =
    db
    |> db_with
         [ Entity
             { db_id = Some (Entity_id 2)
             ; attrs = [ "db/ident", One_value (Keyword "touch") ]
             }
         ; InstallTxFn
             ( Ident "touch"
             , fun _ args ->
                 match args with
                 | [ Int64 entity_id ] -> [ Add (Entity_id (Util.int64_to_int_exn "entity id" entity_id), "touched", Bool true) ]
                 | _ -> invalid_arg "touch expects one entity id" )
         ]
    |> db_with [ CallIdent (Ident "touch", [ Int64 1L ]) ]
  in
  match datoms db Eavt ~e:1 ~a:"touched" () with
  | [ { v = Bool true; _ } ] -> ()
  | _ -> failwith "JavaScript smoke transaction function returned an unexpected result"

let () =
  let base = memory_storage () in
  let reads = ref 0 in
  let storage = { base with storage_restore = (fun address ->
    incr reads; base.storage_restore address) } in
  let schema = schema_of_edn_string "{:name {:db/index true}}" in
  let db = init_db ~schema [datom ~e:1 ~a:"name" ~v:(String "v") ()] in
  store ~storage db;
  let restored = Option.get (restore storage) in
  reads := 0;
  List.iter (fun set ->
    if Persistent_sorted_set.count set <> 1 || Persistent_sorted_set.count set <> 1 then
      failwith "restored smoke count differs")
    [restored.eavt_index;restored.aevt_index;restored.avet_index];
  if !reads <> 0 then failwith "restored smoke count loaded storage"
