open Js_of_ocaml

module Scenario = Memory_bench_common.Memory_scenario

let js_number_to_int value =
  int_of_float (Obj.magic value)

let node_memory_usage () =
  let process = Js.Unsafe.get Js.Unsafe.global "process" in
  Js.Unsafe.meth_call process "memoryUsage" [||]

let memory_property name =
  node_memory_usage ()
  |> fun memory -> Js.Unsafe.get memory name
  |> js_number_to_int

let rss_bytes () =
  memory_property "rss"

let heap_bytes () =
  memory_property "heapUsed"

let maybe_gc () =
  let global_gc = Js.Unsafe.get Js.Unsafe.global "gc" in
  if Js.to_string (Js.typeof global_gc) = "function" then
    for _ = 1 to 4 do
      ignore (Js.Unsafe.fun_call global_gc [||])
    done

let report runtime scenario =
  Scenario.report runtime scenario (rss_bytes ()) (heap_bytes ())

let trace_enabled () =
  match Sys.getenv_opt "MEM_BENCH_TRACE" with
  | Some "1" | Some "true" -> true
  | _ -> false

let trace label =
  if trace_enabled () then
    Printf.eprintf "trace\t%s\t%d\t%d\n%!" label (rss_bytes ()) (heap_bytes ())

let main () =
  let config = Scenario.parse_args () in
  let runtime =
    match Sys.getenv_opt "MEMORY_RUNTIME_LABEL" with
    | Some label -> label
    | None -> "js_of_ocaml"
  in
  let db = Scenario.build_db config.size in
  report runtime "initial-open";
  let db = Scenario.run_scenario ~probe:trace config db in
  report runtime "after-transact-query";
  Gc.full_major ();
  maybe_gc ();
  report runtime "after-gc";
  Scenario.finish db

let () = main ()
