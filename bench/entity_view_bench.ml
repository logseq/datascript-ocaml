(* Micro-benchmark for the lazy entity view.

   Builds a db with one wide entity (N distinct cardinality-one attrs) plus a
   tail of unrelated datoms, then times `entity` + one `entity_attr` call — the
   "read a single attribute" path. Usage: entity_view_bench.exe <attrs> <iters> *)

open Datascript

let now_ms () = Unix.gettimeofday () *. 1000.

let () =
  let wide = int_of_string Sys.argv.(1) in
  let iters = int_of_string Sys.argv.(2) in
  let db =
    empty_db ()
    |> db_with
         (List.init wide (fun i -> Add (Entity_id 1, "attr" ^ string_of_int i, Int64 (Int64.of_int i)))
          @ List.init 2000 (fun i -> Add (Entity_id (i + 2), "other", Int64 (Int64.of_int i))))
  in
  (* entity construction alone *)
  let t0 = now_ms () in
  for _ = 1 to iters do
    ignore (entity db (Entity_id 1))
  done;
  let construct_ms = (now_ms () -. t0) *. 1000. /. float_of_int iters in
  (* single forward attr read on a fresh entity each iteration *)
  let t0 = now_ms () in
  for i = 1 to iters do
    match entity db (Entity_id 1) with
    | None -> failwith "expected entity"
    | Some e ->
      let target = "attr" ^ string_of_int (i mod wide) in
      ignore (entity_attr e target)
  done;
  let single_attr_us = (now_ms () -. t0) *. 1000. /. float_of_int iters in
  (* full materialization *)
  let t0 = now_ms () in
  for _ = 1 to iters do
    match entity db (Entity_id 1) with
    | None -> failwith "expected entity"
    | Some e -> ignore (entity_attrs e)
  done;
  let attrs_ms = (now_ms () -. t0) /. float_of_int iters in
  Printf.printf "wide=%d iters=%d\n" wide iters;
  Printf.printf "entity construct:        %8.2f us/call\n" construct_ms;
  Printf.printf "entity + single attr:    %8.2f us/call\n" single_attr_us;
  Printf.printf "entity_attrs (all):      %8.3f ms/call\n" attrs_ms
