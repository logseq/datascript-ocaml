open Datascript

type timing = { label : string; elapsed_ms : float }

let now_ms () = Unix.gettimeofday () *. 1000.

let time label f =
  let start = now_ms () in
  let result = f () in
  ({ label; elapsed_ms = now_ms () -. start }, result)

let print_timing { label; elapsed_ms } =
  Printf.printf "%s\t%.2f\n%!" label elapsed_ms

let indexed =
  {
    cardinality = One;
    unique = None;
    indexed = true;
    is_component = false;
    no_history = false;
    doc = None;
    value_type = None;
    tuple_attrs = None;
    tuple_types = None;
  }

let unique_identity = { indexed with unique = Some Identity }

let many =
  {
    cardinality = Many;
    unique = None;
    indexed = false;
    is_component = false;
    no_history = false;
    doc = None;
    value_type = None;
    tuple_attrs = None;
    tuple_types = None;
  }

let schema =
  [ ("id", unique_identity)
  ; ("name", indexed)
  ; ("age", indexed)
  ; ("salary", indexed)
  ; ("alias", many)
  ]

let names = [| "Ivan"; "Petr"; "Sergey"; "Oleg"; "Yuri"; "Dmitry"; "Fedor"; "Denis" |]
let last_names = [| "Ivanov"; "Petrov"; "Sidorov"; "Kovalev"; "Kuznetsov"; "Voronoi" |]

type rng = { mutable state : int32 }

let rng seed = { state = Int32.of_int seed }

let next_int rng bound =
  rng.state <- Int32.add (Int32.mul rng.state 1_664_525l) 1_013_904_223l;
  Int32.(to_int (rem (logand (shift_right_logical rng.state 1) 0x3fffffffl) (of_int bound)))

let rand_nth rng values = values.(next_int rng (Array.length values))

let datoms_for size =
  let rng = rng 1 in
  List.init size (fun index ->
      let i = index + 1 in
      let name = rand_nth rng names in
      let last_name = rand_nth rng last_names in
      [
        { e = i; a = "name"; v = String name; tx = 0x20000001; added = true }
      ; { e = i; a = "last-name"; v = String last_name; tx = 0x20000001; added = true }
      ; { e = i; a = "age"; v = Int (next_int rng 100); tx = 0x20000001; added = true }
      ; { e = i; a = "salary"; v = Int (next_int rng 100_000); tx = 0x20000001; added = true }
      ])
  |> List.concat

let entity_count db = Seq.length (datoms db Eavt ())

let parse_size () =
  match Sys.argv with
  | [| _; size |] -> int_of_string size
  | _ -> 20_000

let main () =
  let size = parse_size () in
  let runtime_label =
    match Sys.getenv_opt "BENCH_RUNTIME_LABEL" with
    | Some label -> label
    | None -> "ocaml"
  in
  Printf.printf "runtime\t%s\n%!" runtime_label;
  Printf.printf "size\t%d\n%!" size;
  Printf.printf "datoms\t%d\n%!" (size * 4);
  let datoms = datoms_for size in
  let build_all, db =
    time "build-all-init" (fun () -> init_db ~schema datoms)
  in
  print_timing build_all;
  Printf.printf "datom-count\t%d\n%!" (entity_count db);
  let scan_name, count =
    time "scan-aevt-name" (fun () ->
      fold_datoms (fun count _ -> count + 1) 0 db Aevt ~a:"name" ())
  in
  print_timing scan_name;
  Printf.printf "scan-aevt-name-count\t%d\n%!" count;
  let find_name, rows =
    time "query-name-ivan" (fun () ->
      q_string db "[:find ?e :where [?e :name \"Ivan\"]]")
  in
  print_timing find_name;
  Printf.printf "query-name-ivan-count\t%d\n%!" (List.length rows);
  let add_one, db =
    time "add-one-tx" (fun () ->
      db_with [ Add (Entity_id 1, "nickname", String "Vanya") ] db)
  in
  print_timing add_one;
  ignore db;
  let storage, restored =
    time "storage-roundtrip" (fun () ->
      let storage = memory_storage () in
      let db = init_db ~schema ~storage datoms in
      store db;
      match restore storage with
      | Some db -> db
      | None -> failwith "restore failed")
  in
  print_timing storage;
  Printf.printf "restored-datom-count\t%d\n%!" (entity_count restored)

let () = main ()
