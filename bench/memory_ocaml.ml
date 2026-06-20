module Scenario = Memory_bench_common.Memory_scenario

let rss_bytes () =
  let command = Printf.sprintf "ps -o rss= -p %d" (Unix.getpid ()) in
  let channel = Unix.open_process_in command in
  let line =
    try input_line channel with
    | End_of_file -> "0"
  in
  ignore (Unix.close_process_in channel);
  line |> String.trim |> int_of_string |> fun kb -> kb * 1024

let heap_bytes () =
  let stat = Gc.stat () in
  stat.live_words * (Sys.word_size / 8)

let report runtime scenario =
  Scenario.report runtime scenario (rss_bytes ()) (heap_bytes ())

let main () =
  let config = Scenario.parse_args () in
  let runtime =
    match Sys.getenv_opt "MEMORY_RUNTIME_LABEL" with
    | Some label -> label
    | None -> "ocaml-native"
  in
  let db = Scenario.build_db config.size in
  report runtime "initial-open";
  let db = Scenario.run_scenario config db in
  report runtime "after-transact-query";
  Gc.full_major ();
  Gc.compact ();
  report runtime "after-gc";
  Scenario.finish db

let () = main ()
