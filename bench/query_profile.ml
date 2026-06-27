open Datascript

let now_ms () =
  Unix.gettimeofday () *. 1000.

let measure name iterations f =
  let start = now_ms () in
  let total = ref 0 in
  for _ = 1 to iterations do
    total := (!total + f ()) land 0x3fffffff
  done;
  let elapsed = now_ms () -. start in
  Printf.printf "%s\t%.5f\t%d\n%!" name (elapsed /. float_of_int iterations) !total

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

let many =
  { indexed with cardinality = Many; indexed = false }

let schema =
  [ "id", unique_identity
  ; "name", indexed
  ; "age", indexed
  ; "salary", indexed
  ; "friend", ref_attr
  ; "alias", many
  ]

let names = [| "Ivan"; "Petr"; "Sergey"; "Oleg"; "Yuri"; "Dmitry"; "Fedor"; "Denis" |]
let last_names = [| "Ivanov"; "Petrov"; "Sidorov"; "Kovalev"; "Kuznetsov"; "Voronoi" |]
let aliases =
  [| "A. C. Q. W."
   ; "A. J. Finn"
   ; "A.A. Fair"
   ; "Aapeli"
   ; "Aaron Wolfe"
   ; "Abigail Van Buren"
   ; "Jeanne Phillips"
   ; "Abram Tertz"
   ; "Abu Nuwas"
   ; "Acton Bell"
   ; "Adunis"
  |]

type rng = { mutable state : int32 }

let rng seed = { state = Int32.of_int seed }

let next_int rng bound =
  rng.state <- Int32.add (Int32.mul rng.state 1_664_525l) 1_013_904_223l;
  Int32.(to_int (rem (logand (shift_right_logical rng.state 1) 0x3fffffffl) (of_int bound)))

let rand_nth rng values =
  values.(next_int rng (Array.length values))

let random_man rng i =
  let name = rand_nth rng names in
  let last_name = rand_nth rng last_names in
  let alias_count = next_int rng 10 in
  let alias_values = List.init alias_count (fun _ -> String (rand_nth rng aliases)) in
  Entity
    { db_id = Some (Temp_id (string_of_int i))
    ; attrs =
        [ "name", One_value (String name)
        ; "last-name", One_value (String last_name)
        ; "full-name", One_value (String (name ^ " " ^ last_name))
        ; "alias", Many_values alias_values
        ; "sex", One_value (Keyword (if next_int rng 2 = 0 then "male" else "female"))
        ; "age", One_value (Int (next_int rng 100))
        ; "salary", One_value (Int (next_int rng 100_000))
        ]
    }

let people size =
  let rng = rng 1 in
  List.init size (fun i -> random_man rng (i + 1))

let seq_len seq =
  Seq.fold_left (fun count _ -> count + 1) 0 seq

let rows_len rows =
  List.length rows

let q_len db query =
  rows_len (q_string db query)

let q_len_inputs db inputs query =
  rows_len (q_string ~inputs db query)

let direct_q3 db =
  let male = Bytes.make (1001) '\000' in
  datoms db Aevt ~a:"sex" ()
  |> Seq.iter (fun datom ->
    if datom.v = Keyword "male" && datom.e >= 0 && datom.e < Bytes.length male then
      Bytes.set male datom.e '\001');
  datoms db Avet ~a:"name" ~v:(String "Ivan") ()
  |> Seq.fold_left
       (fun count datom ->
         if datom.e >= 0 && datom.e < Bytes.length male && Bytes.get male datom.e = '\001' then
           count + seq_len (datoms db Eavt ~e:datom.e ~a:"age" ())
         else
           count)
       0

let name_entities db =
  datoms db Avet ~a:"name" ~v:(String "Ivan") ()
  |> Seq.map (fun datom -> datom.e)
  |> List.of_seq

let age_rows db =
  datoms db Aevt ~a:"age" ()
  |> Seq.map (fun datom -> datom.e, datom.v)
  |> List.of_seq

let male_entities db =
  datoms db Aevt ~a:"sex" ~v:(Keyword "male") ()
  |> Seq.map (fun datom -> datom.e)
  |> List.of_seq

let manual_q2 db =
  let names = Hashtbl.create 256 in
  name_entities db |> List.iter (fun entity_id -> Hashtbl.replace names entity_id ());
  age_rows db
  |> List.fold_left
       (fun count (entity_id, _) ->
         if Hashtbl.mem names entity_id then count + 1 else count)
       0

let manual_q3 db =
  let names = Hashtbl.create 256 in
  name_entities db |> List.iter (fun entity_id -> Hashtbl.replace names entity_id ());
  let q2_rows =
    age_rows db
    |> List.filter_map (fun (entity_id, value) ->
      if Hashtbl.mem names entity_id then Some (entity_id, value) else None)
  in
  let male = Hashtbl.create 512 in
  male_entities db |> List.iter (fun entity_id -> Hashtbl.replace male entity_id ());
  q2_rows
  |> List.fold_left
       (fun count (entity_id, _) ->
         if Hashtbl.mem male entity_id then count + 1 else count)
       0

let add_one_by_one size =
  let single_datom_attrs = [ "name"; "last-name"; "sex"; "age"; "salary" ] in
  let add_entity db entity =
    match entity with
    | Entity { db_id = Some entity_ref; attrs; _ } ->
      List.fold_left
        (fun db (attr, value) ->
          if List.mem attr single_datom_attrs then
            match value with
            | One_value value -> db_with [ Add (entity_ref, attr, value) ] db
            | Many_values _ | One_entity _ | Many_entities _ -> db
          else
            db)
        db
        attrs
    | _ -> db_with [ entity ] db
  in
  List.fold_left add_entity (empty_db ~schema ()) (people size)

let add_five size =
  db_with (people size) (empty_db ~schema ())

let explicit_people size =
  people size
  |> List.mapi (fun index -> function
    | Entity entity -> Entity { entity with db_id = Some (Entity_id (index + 1)) }
    | tx_op -> tx_op)

let people_without_alias size =
  people size
  |> List.map (function
    | Entity entity ->
      Entity { entity with attrs = List.filter (fun (attr, _) -> attr <> "alias") entity.attrs }
    | tx_op -> tx_op)

let people_without_alias_explicit size =
  people_without_alias size
  |> List.mapi (fun index -> function
    | Entity entity -> Entity { entity with db_id = Some (Entity_id (index + 1)) }
    | tx_op -> tx_op)

let bulk_explicit size =
  db_with (explicit_people size) (empty_db ~schema ())

let bulk_without_alias size =
  db_with (people_without_alias size) (empty_db ~schema ())

let bulk_without_alias_explicit size =
  db_with (people_without_alias_explicit size) (empty_db ~schema ())

let add_ops size =
  explicit_people size
  |> List.concat_map (function
    | Entity { db_id = Some entity_ref; attrs } ->
      attrs
      |> List.concat_map (fun (attr, tx_value) ->
        match tx_value with
        | One_value value -> [ Add (entity_ref, attr, value) ]
        | Many_values values -> List.map (fun value -> Add (entity_ref, attr, value)) values
        | One_entity _ | Many_entities _ -> [])
    | _ -> [])

let bulk_add_ops size =
  db_with (add_ops size) (empty_db ~schema ())

let () =
  let size, iterations =
    match Sys.argv |> Array.to_list |> List.tl with
    | [ size; iterations ] -> int_of_string size, int_of_string iterations
    | [ size ] -> int_of_string size, 1000
    | [] -> 1000, 1000
    | _ -> invalid_arg "usage: query_profile.exe [size] [iterations]"
  in
  let db = db_with (people size) (empty_db ~schema ()) in
  let qpred2_query =
    parse_query_string "[:find ?e ?s :in $ ?min-s :where [?e :salary ?s] [(> ?s ?min-s)]]"
  in
  Printf.printf "case\tms\tblackhole\n%!";
  measure "datoms-name-avet" iterations (fun () -> seq_len (datoms db Avet ~a:"name" ~v:(String "Ivan") ()));
  measure "datoms-sex-aevt" iterations (fun () -> seq_len (datoms db Aevt ~a:"sex" ()));
  measure "datoms-sex-aevt-value" iterations (fun () -> seq_len (datoms db Aevt ~a:"sex" ~v:(Keyword "male") ()));
  measure "datoms-age-aevt" iterations (fun () -> seq_len (datoms db Aevt ~a:"age" ()));
  measure "profile-name-list" iterations (fun () -> List.length (name_entities db));
  measure "profile-age-list" iterations (fun () -> List.length (age_rows db));
  measure "profile-sex-male-list" iterations (fun () -> List.length (male_entities db));
  measure "profile-manual-q2" iterations (fun () -> manual_q2 db);
  measure "profile-manual-q3" iterations (fun () -> manual_q3 db);
  measure "q-name" iterations (fun () -> q_len db "[:find ?e :where [?e :name \"Ivan\"]]");
  measure "q-name-age" iterations (fun () -> q_len db "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]");
  measure "q-name-sex" iterations (fun () -> q_len db "[:find ?e :where [?e :name \"Ivan\"] [?e :sex :male]]");
  measure "q-sex-name" iterations (fun () -> q_len db "[:find ?e :where [?e :sex :male] [?e :name \"Ivan\"]]");
  measure "q-name-age-sex" iterations (fun () -> q_len db "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e :sex :male]]");
  measure "q-sex-name-age" iterations (fun () -> q_len db "[:find ?e ?a :where [?e :sex :male] [?e :name \"Ivan\"] [?e :age ?a]]");
  measure "q-name-last-age-sex" iterations (fun () -> q_len db "[:find ?e ?l ?a :where [?e :name \"Ivan\"] [?e :last-name ?l] [?e :age ?a] [?e :sex :male]]");
  measure "qpred1" iterations (fun () -> q_len db "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]");
  measure
    "qpred2"
    iterations
    (fun () ->
       q_len_inputs
         db
         [ Arg_scalar (Result_value (Int 50000)) ]
         "[:find ?e ?s :in $ ?min-s :where [?e :salary ?s] [(> ?s ?min-s)]]");
  measure
    "qpred2-parsed"
    iterations
    (fun () ->
       q db ~inputs:[ Arg_scalar (Result_value (Int 50000)) ] qpred2_query
       |> rows_len);
  measure "direct-q3-index-join" iterations (fun () -> direct_q3 db)
  ;
  let tx_iterations = max 1 (iterations / 100) in
  measure "add-1" tx_iterations (fun () -> add_one_by_one size |> fun db -> seq_len (datoms db Eavt ()));
  measure "add-5" tx_iterations (fun () -> add_five size |> fun db -> seq_len (datoms db Eavt ()));
  measure "bulk-explicit" tx_iterations (fun () -> bulk_explicit size |> fun db -> seq_len (datoms db Eavt ()));
  measure "bulk-no-alias" tx_iterations (fun () -> bulk_without_alias size |> fun db -> seq_len (datoms db Eavt ()));
  measure "bulk-no-alias-explicit" tx_iterations (fun () -> bulk_without_alias_explicit size |> fun db -> seq_len (datoms db Eavt ()));
  measure "bulk-add-ops" tx_iterations (fun () -> bulk_add_ops size |> fun db -> seq_len (datoms db Eavt ()))
