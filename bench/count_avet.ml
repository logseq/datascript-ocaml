open Datascript

let indexed =
  {
    cardinality = One
  ; unique = None
  ; indexed = true
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let many = { indexed with cardinality = Many; indexed = false }

let rng = ref 1

let next_int bound =
  rng := (!rng * 1_664_525 + 1_013_904_223) land 0x7fffffff;
  !rng mod bound

let names = [| "Ivan"; "Petr"; "Sergey"; "Oleg"; "Yuri"; "Dmitry"; "Fedor"; "Denis" |]
let last_names = [| "Ivanov"; "Petrov"; "Sidorov"; "Kovalev"; "Kuznetsov"; "Voronoi" |]
let aliases = [| "A. C. Q. W."; "A. J. Finn"; "A.A. Fair"; "Aapeli"; "Aaron Wolfe" |]

let random_man i =
  let name = names.(i mod Array.length names) in
  let last_name = last_names.(i mod Array.length last_names) in
  let alias_count = 1 + next_int 10 in
  let alias_values = List.init alias_count (fun _ -> String aliases.(next_int (Array.length aliases))) in
  Entity
    {
      db_id = Some (Temp_id (string_of_int (i + 1)))
    ; attrs =
        [ "name", One_value (String name)
        ; "last-name", One_value (String last_name)
        ; "full-name", One_value (String (name ^ " " ^ last_name))
        ; "alias", Many_values alias_values
        ; "sex", One_value (Keyword (if next_int 2 = 0 then "male" else "female"))
        ; "age", One_value (Int (next_int 100))
        ; "salary", One_value (Int (next_int 100_000))
        ]
    }

let minimal_schema = [ "salary", indexed ]

let full_schema =
  [ "name", indexed; "last-name", indexed; "age", indexed; "salary", indexed; "alias", many ]

let build_db schema size =
  let entities =
    match schema with
    | "minimal" ->
        List.init size (fun index ->
            Entity
              {
                db_id = Some (Temp_id (string_of_int (index + 1)))
              ; attrs = [ "salary", One_value (Int (next_int 100_000)) ]
              })
    | _ -> List.init size random_man
  in
  let schema = if schema = "minimal" then minimal_schema else full_schema in
  db_with entities (empty_db ~schema ())

let time_ms iterations f =
  let start = Sys.time () in
  for _ = 1 to iterations do
    ignore (f ())
  done;
  (Sys.time () -. start) *. 1000. /. float iterations

let seq_len seq =
  Seq.fold_left (fun count _ -> count + 1) 0 seq

let bench label db =
  let q () =
    q_string db "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]" |> List.length
  in
  let seq () = seq_len (index_range db "salary" ~start:(Int 50001) ()) in
  let seq_one () = seq_len (index_range db "salary" ~start:(Int 1) ~stop:(Int 1) ()) in
  Printf.printf "%s count=%d one=%d ms_q=%.4f ms_seq=%.4f ms_one=%.4f max_e=%d\n" label (q ())
    (seq_one ())
    (time_ms 200 q)
    (time_ms 200 seq)
    (time_ms 200 seq_one)
    db.max_datom_e

let () =
  rng := 1;
  bench "minimal" (build_db "minimal" 2000);
  rng := 1;
  bench "full" (build_db "full" 2000)
