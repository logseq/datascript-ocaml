open Datascript

type config =
  { size : int
  ; warmup_ms : float
  ; sample_ms : float
  ; samples : int
  ; semantic_only : bool
  }

let default_config = { size = 200; warmup_ms = 200.; sample_ms = 500.; samples = 5; semantic_only = false }

let parse_args () =
  let config = ref default_config in
  let set_size value = config := { !config with size = int_of_string value } in
  let set_warmup value = config := { !config with warmup_ms = float_of_string value } in
  let set_sample_ms value = config := { !config with sample_ms = float_of_string value } in
  let set_samples value = config := { !config with samples = int_of_string value } in
  let rec loop = function
    | [] -> !config
    | "--semantic-only" :: rest ->
      config := { !config with semantic_only = true };
      loop rest
    | "--size" :: value :: rest ->
      set_size value;
      loop rest
    | "--warmup-ms" :: value :: rest ->
      set_warmup value;
      loop rest
    | "--sample-ms" :: value :: rest ->
      set_sample_ms value;
      loop rest
    | "--samples" :: value :: rest ->
      set_samples value;
      loop rest
    | arg :: _ -> invalid_arg ("unknown benchmark argument: " ^ arg)
  in
  Sys.argv |> Array.to_list |> List.tl |> loop

let now_ms () =
  Unix.gettimeofday () *. 1000.

let median values =
  let sorted = List.sort Float.compare values in
  List.nth sorted (List.length sorted / 2)

let format_ms value =
  if value > 1. then Printf.sprintf "%.2f" value else Printf.sprintf "%.5f" value

let blackhole = ref 0

let consume_int value =
  blackhole := (!blackhole + value) land 0x3fffffff

let seq_length seq =
  let rec loop count seq =
    match seq () with
    | Seq.Nil -> count
    | Seq.Cons (_, rest) -> loop (count + 1) rest
  in
  loop 0 seq

let consume_db db =
  consume_int (seq_length (datoms db Eavt ()))

let consume_rows rows =
  consume_int (List.length rows)

let consume_pull = function
  | Some entity -> consume_int (List.length entity.pulled_attrs)
  | None -> consume_int 0

let run_for duration_ms f =
  let start = now_ms () in
  let deadline = start +. duration_ms in
  let rec loop iterations =
    f ();
    let iterations = iterations + 1 in
    if now_ms () < deadline then loop iterations else iterations, now_ms () -. start
  in
  loop 0

let bench config name f =
  Gc.full_major ();
  ignore (run_for config.warmup_ms f);
  Gc.full_major ();
  let samples =
    List.init config.samples (fun _ ->
      let iterations, elapsed = run_for config.sample_ms f in
      elapsed /. float_of_int iterations)
  in
  Printf.printf "%s\t%s\n%!" name (format_ms (median samples))

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
  { cardinality = One
  ; unique = Some Identity
  ; indexed = true
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = None
  ; tuple_attrs = None
  ; tuple_types = None
  }

let ref_attr =
  { cardinality = One
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = Some RefType
  ; tuple_attrs = None
  ; tuple_types = None
  }

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
  (* Explicit draws preserve the native fixture across compiler evaluation orders. *)
  let salary = next_int rng 100_000 in
  let age = next_int rng 100 in
  let sex = if next_int rng 2 = 0 then "male" else "female" in
  Entity
    { db_id = Some (Entity_id i)
    ; attrs =
        [ "name", One_value (String name)
        ; "last-name", One_value (String last_name)
        ; "full-name", One_value (String (name ^ " " ^ last_name))
        ; "alias", Many_values alias_values
        ; "sex", One_value (Keyword sex)
        ; "age", One_value (Int64 (Int64.of_int age))
        ; "salary", One_value (Int64 (Int64.of_int salary))
        ]
    }

let people size =
  let rng = rng 1 in
  List.init size (fun index -> random_man rng (index + 1))

let build_db input =
  db_with input (empty_db ~schema ())

(* Logseq get-page-data scenario — mirrors
   src/main/logseq/api/db_based/tools.cljs (get-page-data -> get-page-blocks ->
   otree/blocks->vec-tree): page lookup via avet :block/name, block listing via
   avet :block/page, full entity materialization, then nesting by :block/parent
   sorted on :block/order. *)

let plain = { indexed with indexed = false }

let logseq_schema =
  [ "block/name", indexed
  ; "block/title", indexed
  ; "block/uuid", unique_identity
  ; "block/page", { ref_attr with indexed = true }
  ; "block/parent", { ref_attr with indexed = true }
  ; "block/order", indexed
  ; "block/journal-day", indexed
  ; "block/refs", { ref_attr with cardinality = Many }
  ; "block/tags", { ref_attr with cardinality = Many }
  ; "block/created-at", plain
  ; "block/updated-at", plain
  ]

let logseq_blocks_per_page = 100

let logseq_page_name i = Printf.sprintf "page-%06d" i

let logseq_block_title rng i =
  let suffix = next_int rng 0xffff in
  let prefix = next_int rng 0x7fff_ffff in
  Printf.sprintf
    "block %d content [[some page]] #tag and a ref to ((%08x-%04x))"
    i
    prefix
    suffix

(* UUID-shaped placeholders are strings in both runtimes, not UUID objects. *)
let logseq_data size =
  let rng = rng 1 in
  let num_pages = max 1 (size / logseq_blocks_per_page) in
  let blocks_per_page = size / num_pages in
  let next_id = ref 0 in
  let fresh_id () =
    incr next_id;
    string_of_int !next_id
  in
  let tag_ids = List.init 5 (fun _ -> fresh_id ()) in
  let tags =
    List.map
      (fun id ->
        Entity
          { db_id = Some (Entity_id (int_of_string id))
          ; attrs =
              [ "block/title", One_value (String ("tag-" ^ id))
              ; "block/uuid", One_value (String ("tag-uuid-" ^ id))
              ]
          })
      tag_ids
  in
  let pages =
    List.init num_pages (fun p ->
      let page_id = fresh_id () in
      let page_ref = Entity_id (int_of_string page_id) in
      let page_attrs =
        [ "block/name", One_value (String (logseq_page_name p))
        ; "block/title", One_value (String (logseq_page_name p))
        ; "block/uuid", One_value (String (Printf.sprintf "page-uuid-%06d" p))
        ; "block/created-at", One_value (Int64 1_700_000_000_000L)
        ; "block/updated-at", One_value (Int64 1_700_000_100_000L)
        ]
        |> fun attrs ->
        if p mod 2 = 0 then ("block/journal-day", One_value (Int64 20_260_101L)) :: attrs else attrs
      in
      let sibling_counts = Hashtbl.create 16 in
      let next_order parent =
        let n = Option.value ~default:0 (Hashtbl.find_opt sibling_counts parent) in
        Hashtbl.replace sibling_counts parent (n + 1);
        Printf.sprintf "%08d" n
      in
      let blocks =
        List.init blocks_per_page (fun b ->
          let block_id = fresh_id () in
          (* ~40% top-level, rest nested under a random earlier block *)
          let parent =
            if b = 0 || b mod 5 < 2 then page_ref
            else Entity_id (int_of_string page_id + 1 + next_int rng b)
          in
          let attrs =
            [ "block/uuid", One_value (String (Printf.sprintf "block-uuid-%s" block_id))
            ; "block/title", One_value (String (logseq_block_title rng b))
            ; "block/page", One_value (Ref_to page_ref)
            ; "block/parent", One_value (Ref_to parent)
            ; "block/order", One_value (String (next_order parent))
            ; "block/created-at", One_value (Int64 1_700_000_000_000L)
            ; "block/updated-at", One_value (Int64 1_700_000_100_000L)
            ]
            |> fun attrs ->
            let attrs =
              if b mod 10 = 0 then
                ( "block/tags"
                , Many_values [ Ref_to (Entity_id (int_of_string (rand_nth rng (Array.of_list tag_ids)))) ] )
                :: attrs
              else attrs
            in
            if b mod 7 = 0 then
              ("block/refs", Many_values [ Ref_to (Entity_id (6 + next_int rng num_pages)) ])
              :: attrs
            else attrs
          in
          Entity { db_id = Some (Entity_id (int_of_string block_id)); attrs })
      in
      Entity { db_id = Some page_ref; attrs = page_attrs } :: blocks)
  in
  tags @ List.flatten pages

let build_logseq_db input =
  db_with input (empty_db ~schema:logseq_schema ())

let logseq_get_page_data db name =
  (* ldb/get-page via get-first-page-by-name: first avet :block/name hit *)
  let page_id =
    match datoms db Avet ~a:"block/name" ~v:(String name) () |> Seq.uncons with
    | Some (d, _) -> Some d.e
    | None -> None
  in
  match page_id with
  | None -> consume_int 0
  | Some page_id ->
    (match entity db (Entity_id page_id) with
     | None -> ()
     | Some page -> consume_int (1 + List.length (entity_attrs page)));
    let block_eids =
      datoms db Avet ~a:"block/page" ~v:(Ref page_id) ()
      |> Seq.fold_left (fun acc d -> d.e :: acc) []
      |> List.rev
    in
    let blocks =
      List.filter_map
        (fun eid ->
          match entity db (Entity_id eid) with
          | Some entity -> Some (eid, entity_attrs entity)
          | None -> None)
        block_eids
    in
    let attr_value attrs attr =
      List.find_map
        (fun (a, v) -> if String.equal a attr then Some v else None)
        attrs
    in
    let parent_id attrs =
      match attr_value attrs "block/parent" with
      | Some (One_value (Ref parent)) -> parent
      | Some (One_entity { db_id = Some (Entity_id parent); _ }) -> parent
      | _ -> page_id
    in
    let order_key attrs =
      match attr_value attrs "block/order" with
      | Some (One_value (String order)) -> order
      | _ -> ""
    in
    let children_of = Hashtbl.create 64 in
    List.iter
      (fun ((_, attrs) as block) ->
        let parent = parent_id attrs in
        let siblings = Option.value ~default:[] (Hashtbl.find_opt children_of parent) in
        Hashtbl.replace children_of parent (block :: siblings))
      blocks;
    Hashtbl.iter
      (fun parent siblings ->
        Hashtbl.replace
          children_of
          parent
          (List.sort
             (fun (_, a) (_, b) -> String.compare (order_key a) (order_key b))
             siblings))
      children_of;
    let rec count_subtree parent =
      match Hashtbl.find_opt children_of parent with
      | None -> 0
      | Some siblings ->
        List.fold_left
          (fun acc (eid, attrs) -> acc + 2 + List.length attrs + count_subtree eid)
          0
          siblings
    in
    consume_int (count_subtree page_id)

let build_storage_db input =
  let storage = memory_storage () in
  let db = db_with input (empty_db ~schema ~storage ()) in
  store db;
  match restore storage with
  | Some db -> db
  | None -> failwith "storage-backed benchmark db should restore"

let add_one_by_one input =
  List.fold_left
    (fun db entity -> db_with [ entity ] db)
    (empty_db ~schema ())
    input

let add_one_datom_per_tx input =
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
  List.fold_left add_entity (empty_db ~schema ()) input

module Json = Yojson.Basic

let sort_json values =
  List.sort
    (fun left right ->
      String.compare (Json.to_string left) (Json.to_string right))
    values

let json_int64 value =
  (* OCaml ints are 32-bit under js_of_ocaml; retain exact JS-safe integers. *)
  if value < -9_007_199_254_740_991L || value > 9_007_199_254_740_991L then
    invalid_arg "benchmark integer exceeds the shared exact range";
  `Float (Int64.to_float value)

let json_value = function
  | String s -> `Assoc [ ("kind", `String "string"); ("value", `String s) ]
  | Keyword s -> `Assoc [ ("kind", `String "keyword"); ("value", `String s) ]
  | Uuid s -> `Assoc [ ("kind", `String "uuid"); ("value", `String s) ]
  | Int64 n -> `Assoc [ ("kind", `String "number"); ("value", json_int64 n) ]
  | Ref n -> `Assoc [ ("kind", `String "ref"); ("value", `Int n) ]
  | Bool b -> `Assoc [ ("kind", `String "boolean"); ("value", `Bool b) ]
  | _ -> invalid_arg "unsupported benchmark value"

let json_datoms ?a db =
  fold_datoms
    (fun facts d ->
      `List [ `Int d.e; json_value (Keyword d.a); json_value d.v ] :: facts)
    [] db Eavt ?a ()
  |> sort_json
  |> fun facts -> `List facts

let json_result = function
  | Result_entity n -> `Int n
  | Result_value (String s) -> `String s
  | Result_value (Int64 n) -> json_int64 n
  | _ -> invalid_arg "unsupported benchmark result"

let json_rows rows =
  rows |> List.map (fun row -> `List (List.map json_result row)) |> sort_json
  |> fun rows -> `List rows

let json_pull_key = function
  | Keyword name -> ":" ^ name
  | String name -> name
  | _ -> invalid_arg "unsupported benchmark pull key"

let rec json_pulled_value = function
  | Pulled_scalar (String s) -> `String s
  | Pulled_scalar (Int64 n) -> json_int64 n
  | Pulled_scalar (Ref n) -> `Assoc [ (":db/id", `Int n) ]
  | Pulled_many values -> `List (sort_json (List.map json_pulled_value values))
  | Pulled_entity entity -> json_pulled_entity entity
  | _ -> invalid_arg "unsupported benchmark pull value"

and json_pulled_entity entity =
  `Assoc
    (List.map
       (fun (key, value) -> (json_pull_key key, json_pulled_value value))
       entity.pulled_attrs)

let benchmark_queries =
  [
    ("q1", "[:find ?e :where [?e :name \"Ivan\"]]", []);
    ("q2", "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]", []);
    ( "q3",
      "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e :sex :male]]",
      [] );
    ( "q4",
      "[:find ?e ?l ?a :where [?e :name \"Ivan\"] [?e :last-name ?l] [?e :age \
       ?a] [?e :sex :male]]",
      [] );
    ( "q5-shortcircuit",
      "[:find ?e ?n ?l ?a ?s ?al :in $ ?n ?a :where [?e :name ?n] [?e :age ?a] \
       [?e :last-name ?l] [?e :sex ?s] [?e :alias ?al]]",
      [ String "Anastasia"; Int64 35L ] );
    ("qpred1", "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]", []);
    ( "qpred2",
      "[:find ?e ?s :in $ ?min-s :where [?e :salary ?s] [(> ?s ?min-s)]]",
      [ Int64 50000L ] );
    ( "q2pred",
      "[:find ?e ?s :where [?e :name \"Ivan\"] [?e :salary ?s] [(> ?s 50000)]]",
      [] );
  ]

let query_inputs values =
  List.map (fun value -> Arg_scalar (Result_value value)) values

let benchmark_pull =
  [
    Pull_attr "name";
    Pull_attr "age";
    Pull_ref ("friend", [ Pull_attr "name"; Pull_attr "age" ]);
  ]

let json_schema schema =
  schema
  |> List.map (fun (attr, spec) ->
      `List
        [
          json_value (Keyword attr);
          `Assoc
            [
              ( "cardinality",
                `String (if spec.cardinality = Many then "many" else "one") );
              ("indexed", `Bool (spec.indexed || spec.unique <> None));
              ( "unique",
                match spec.unique with
                | None -> `Null
                | Some Identity -> `String "identity"
                | Some Value -> `String "value" );
              ( "valueType",
                match spec.value_type with
                | None -> `Null
                | Some RefType -> `String "ref"
                | _ -> invalid_arg "unsupported benchmark schema" );
            ];
        ])
  |> sort_json
  |> fun values -> `List values

let json_page db name =
  match datoms db Avet ~a:"block/name" ~v:(String name) () |> Seq.uncons with
  | None -> `Null
  | Some (page, _) ->
      let blocks =
        datoms db Avet ~a:"block/page" ~v:(Ref page.e) () |> List.of_seq
      in
      let entity eid =
        match pull db [ Pull_wildcard ] (Entity_id eid) with
        | Some entity -> json_pulled_entity entity
        | None -> invalid_arg "missing benchmark entity"
      in
      let children = Hashtbl.create 16 in
      List.iter
        (fun d ->
          let parent =
            match datoms db Eavt ~e:d.e ~a:"block/parent" () |> Seq.uncons with
            | Some ({ v = Ref parent; _ }, _) -> parent
            | _ -> page.e
          in
          let order =
            match datoms db Eavt ~e:d.e ~a:"block/order" () |> Seq.uncons with
            | Some ({ v = String order; _ }, _) -> order
            | _ -> ""
          in
          Hashtbl.replace children parent
            ((order, d.e)
            :: Option.value ~default:[] (Hashtbl.find_opt children parent)))
        blocks;
      let rec tree parent =
        Option.value ~default:[] (Hashtbl.find_opt children parent)
        |> List.sort compare
        |> List.map (fun (_, eid) ->
            `Assoc [ ("id", `Int eid); ("children", tree eid) ])
        |> fun values -> `List values
      in
      let before = !blackhole in
      logseq_get_page_data db name;
      let count = (!blackhole - before) land 0x3fffffff in
      `Assoc
        [
          ("page", entity page.e);
          ( "blocks",
            `List
              (sort_json
                 (List.map (fun d -> `List [ `Int d.e; entity d.e ]) blocks)) );
          ("tree", tree page.e);
          ("count", `Int count);
        ]

let semantic_export config input logseq_input =
  let db = build_db input in
  let logseq_db = build_logseq_db logseq_input in
  let rows =
    List.map
      (fun (name, query, inputs) ->
        let rows =
          q_string ~inputs:(query_inputs inputs) db query |> json_rows
        in
        ( name,
          `Assoc
            [
              ("query", `String query);
              ("inputs", `List (List.map json_value inputs));
              ("rows", rows);
            ] ))
      benchmark_queries
  in
  let pull_result =
    match pull db benchmark_pull (Entity_id 1) with
    | None -> `Null
    | Some entity -> json_pulled_entity entity
  in
  let num_pages = max 1 (config.size / logseq_blocks_per_page) in
  let cases =
    [
      ("add-1", json_datoms (add_one_datom_per_tx input));
      ("add-5", json_datoms (add_one_by_one input));
      ("add-all", json_datoms db);
      ("datoms-name", json_datoms ~a:"name" db);
      ("pull-one", pull_result);
      ("get-page-data", json_page logseq_db (logseq_page_name (num_pages / 2)));
    ]
    @ List.map (fun (name, value) -> (name, Json.Util.member "rows" value)) rows
  in
  if json_datoms (build_storage_db input) <> json_datoms db then
    failwith "storage-roundtrip semantic mismatch";
  `Assoc
    [
      ("size", `Int config.size);
      ( "schemas",
        `Assoc
          [
            ("people", json_schema schema); ("logseq", json_schema logseq_schema);
          ] );
      ( "fixtures",
        `Assoc [ ("people", json_datoms db); ("logseq", json_datoms logseq_db) ]
      );
      ("queries", `Assoc rows);
      ("cases", `Assoc cases);
    ]

let main () =
  let config = parse_args () in
  let input = people config.size in
  let logseq_input = logseq_data config.size in
  if config.semantic_only then
    print_endline (Json.to_string (semantic_export config input logseq_input))
  else
    let runtime_label =
      match Sys.getenv_opt "BENCH_RUNTIME_LABEL" with
      | Some label -> label
      | None -> "ocaml"
    in
    Printf.printf "runtime\t%s\n" runtime_label;
    Printf.printf "size\t%d\n" config.size;
    let db = lazy (build_db input) in
    let logseq_db = lazy (build_logseq_db logseq_input) in
    let logseq_mid_page =
      let num_pages = max 1 (config.size / logseq_blocks_per_page) in
      logseq_page_name (num_pages / 2)
    in
    bench config "add-1" (fun () -> consume_db (add_one_datom_per_tx input));
    bench config "add-5" (fun () -> consume_db (add_one_by_one input));
    bench config "add-all" (fun () -> consume_db (build_db input));
    bench config "datoms-name" (fun () ->
        consume_int
          (fold_datoms
             (fun count _ -> count + 1)
             0 (Lazy.force db) Aevt ~a:"name" ()));
    List.iter
      (fun (name, query, inputs) ->
        bench config name (fun () ->
            consume_rows
              (q_string ~inputs:(query_inputs inputs) (Lazy.force db) query)))
      benchmark_queries;
    bench config "pull-one" (fun () ->
        consume_pull (pull (Lazy.force db) benchmark_pull (Entity_id 1)));
    bench config "storage-roundtrip" (fun () ->
        consume_db (build_storage_db input));
    bench config "get-page-data" (fun () ->
        logseq_get_page_data (Lazy.force logseq_db) logseq_mid_page);
    Printf.eprintf "blackhole=%d\n%!" !blackhole

let () = main ()
