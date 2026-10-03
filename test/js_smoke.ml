open Datascript

let datoms_seq = datoms

let datoms ?e ?a ?v ?tx db index =
  datoms_seq ?e ?a ?v ?tx db index |> List.of_seq

let () =
  let db =
    empty_db ()
    |> db_with [ Add (Entity_id (eid 1L), "name", String "Ivan") ]
  in
  (match q_string db "[:find ?name :where [1 :name ?name]]" with
   | [ [ Result_value (String "Ivan") ] ] -> ()
   | _ -> failwith "JavaScript smoke query returned an unexpected result");
  let db =
    db
    |> db_with
         [ Entity
             { db_id = Some (Entity_id (eid 2L))
             ; attrs = [ "db/ident", One_value (Keyword "touch") ]
             }
         ; InstallTxFn
             ( Ident "touch"
             , fun _ args ->
                 match args with
                 | [ Int64 entity_id ] -> [ Add (Entity_id (eid entity_id), "touched", Bool true) ]
                 | _ -> invalid_arg "touch expects one entity id" )
         ]
    |> db_with [ CallIdent (Ident "touch", [ Int64 1L ]) ]
  in
  match datoms ~e:(eid 1L) ~a:"touched" db Eavt with
  | [ { v = Bool true; _ } ] -> ()
  | _ -> failwith "JavaScript smoke transaction function returned an unexpected result"

let () =
  let base = Internal.memory_storage () in
  let reads = ref 0 in
  let storage =
    { base with
      Internal.Datascript_types.storage_restore =
        (fun address -> incr reads; base.Internal.Datascript_types.storage_restore address)
    }
  in
  let schema =
    [ "name", { Internal.Datascript_types.cardinality = Internal.Datascript_types.One
              ; unique = None; indexed = true; is_component = false; no_history = false
              ; doc = None; value_type = None; tuple_attrs = None; tuple_types = None } ]
  in
  let db =
    Internal.init_db ~schema
      [ Internal.datom ~e:1L ~a:"name" ~v:(Internal.Datascript_types.String "v") () ]
  in
  ignore (Internal.store ~storage db);
  let restored = Option.get (Internal.restore storage) in
  reads := 0;
  List.iter (fun set ->
    if Persistent_sorted_set.count set <> 1 || Persistent_sorted_set.count set <> 1 then
      failwith "restored smoke count differs")
    [restored.eavt_index;restored.aevt_index;restored.avet_index];
  if !reads <> 0 then failwith "restored smoke count loaded storage"
