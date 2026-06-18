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
                 | [ Int entity_id ] -> [ Add (Entity_id entity_id, "touched", Bool true) ]
                 | _ -> invalid_arg "touch expects one entity id" )
         ]
    |> db_with [ CallIdent (Ident "touch", [ Int 1 ]) ]
  in
  match datoms db Eavt ~e:1 ~a:"touched" () with
  | [ { v = Bool true; _ } ] -> ()
  | _ -> failwith "JavaScript smoke transaction function returned an unexpected result"
