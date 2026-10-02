(* Byte-parity check: decode cljs-written kvs rows, re-encode with our
   codec, and compare bytes. Usage: codec_parity <db.sqlite> [max-rows] *)

let () =
  let db_path = Sys.argv.(1) in
  let max_rows = if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 50 in
  let db = Sqlite3.db_open db_path in
  let rows = ref [] in
  (match
     Sqlite3.exec db ~cb:(fun row _headers ->
       rows := (Option.value ~default:"" row.(0), Option.value ~default:"" row.(1)) :: !rows)
     "select addr, content from kvs order by rowid"
   with
   | Sqlite3.Rc.OK -> ()
   | rc -> Printf.eprintf "sqlite error: %s\n" (Sqlite3.Rc.to_string rc); exit 1);
  let rows = List.rev !rows in
  let total = ref 0 and identical = ref 0 and diffs = ref [] in
  List.iteri
    (fun i (addr, content) ->
      if i < max_rows then (
        incr total;
        match Datascript_sqlite_codec.decode content with
        | payload ->
          let re = Datascript_sqlite_codec.encode payload in
          if String.equal re content then incr identical
          else
            diffs := (addr, String.length content, String.length re, content, re) :: !diffs
        | exception exn ->
          diffs := (addr, -1, -1, content, Printexc.to_string exn) :: !diffs))
    rows;
  Printf.printf "%d/%d rows byte-identical\n" !identical !total;
  List.iter
    (fun (addr, clen, rlen, c, r) ->
      Printf.printf "\n--- addr %s (cljs %dB, ours %dB)\ncljs: %s\nours: %s\n" addr clen
        rlen
        (if String.length c > 300 then String.sub c 0 300 ^ "..." else c)
        (if String.length r > 300 then String.sub r 0 300 ^ "..." else r))
    (List.rev (if List.length !diffs > 5 then List.filteri (fun i _ -> i < 5) !diffs else !diffs))
