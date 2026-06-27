open Datascript

type config =
  { size : int
  ; warmup_ms : float
  ; sample_ms : float
  ; samples : int
  }

let default_config = { size = 5000; warmup_ms = 300.; sample_ms = 700.; samples = 7 }

let parse_args () =
  let config = ref default_config in
  let rec loop = function
    | [] -> !config
    | "--size" :: value :: rest ->
      config := { !config with size = int_of_string value };
      loop rest
    | "--warmup-ms" :: value :: rest ->
      config := { !config with warmup_ms = float_of_string value };
      loop rest
    | "--sample-ms" :: value :: rest ->
      config := { !config with sample_ms = float_of_string value };
      loop rest
    | "--samples" :: value :: rest ->
      config := { !config with samples = int_of_string value };
      loop rest
    | arg :: _ -> invalid_arg ("unknown outliner insert benchmark argument: " ^ arg)
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
  Seq.fold_left (fun count _ -> count + 1) 0 seq

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

let unique_identity = { indexed with unique = Some Identity }
let ref_attr = { indexed with indexed = false; value_type = Some RefType }

let schema =
  [ "block/id", unique_identity
  ; "block/journal-day", indexed
  ; "block/content", indexed
  ; "block/order", indexed
  ; "block/collapsed", indexed
  ; "block/parent", ref_attr
  ]

let block_tx ?id ?parent_id index =
  let id =
    match id with
    | Some id -> id
    | None -> Printf.sprintf "block-%05d" index
  in
  let entity = Temp_id id in
  let parent_ops =
    match parent_id with
    | None -> []
    | Some parent_id -> [ Add (entity, "block/parent", Ref_to (Lookup_ref ("block/id", String parent_id))) ]
  in
  [ Add (entity, "block/id", String id)
  ; Add (entity, "block/journal-day", String "2026-06-27")
  ; Add (entity, "block/content", String ("Block " ^ string_of_int index))
  ; Add (entity, "block/order", Float (float_of_int index))
  ; Add (entity, "block/collapsed", Bool false)
  ]
  @ parent_ops

let build_db size =
  let datoms =
    List.concat (List.init size (fun index -> block_tx (index + 1)))
  in
  db_with datoms (empty_db ~schema ())

let make_insert_txs size count =
  Array.init count (fun index ->
    let block_index = size + index + 1 in
    let id = Printf.sprintf "block-new-%05d" index in
    id, block_tx ~id ~parent_id:"block-00001" block_index)

let run_for duration_ms f =
  let start = now_ms () in
  let deadline = start +. duration_ms in
  let rec loop iterations =
    f iterations;
    let iterations = iterations + 1 in
    if now_ms () < deadline then loop iterations else iterations, now_ms () -. start
  in
  loop 0

let bench config name f =
  ignore (run_for config.warmup_ms f);
  let samples =
    List.init config.samples (fun _ ->
      let iterations, elapsed = run_for config.sample_ms f in
      elapsed /. float_of_int iterations)
  in
  Printf.printf "%s\t%s\n%!" name (format_ms (median samples))

let main () =
  let config = parse_args () in
  let runtime =
    match Sys.getenv_opt "BENCH_RUNTIME_LABEL" with
    | Some label -> label
    | None -> "ocaml-native"
  in
  Printf.printf "runtime\t%s\n" runtime;
  Printf.printf "size\t%d\n" config.size;
  let db = build_db config.size in
  let txs = make_insert_txs config.size 4096 in
  bench config "insert-one-block" (fun iteration ->
    let id, tx = txs.(iteration land (Array.length txs - 1)) in
    let next_db = db_with tx db in
    consume_int (seq_length (datoms next_db Avet ~a:"block/id" ~v:(String id) ())));
  Printf.eprintf "blackhole=%d\n%!" !blackhole

let () = main ()
