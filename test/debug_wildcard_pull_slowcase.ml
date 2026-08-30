open Datascript

let ref_many =
  { cardinality = Many
  ; unique = None
  ; indexed = false
  ; is_component = false
  ; no_history = false
  ; doc = None
  ; value_type = Some RefType
  ; tuple_attrs = None
  ; tuple_types = None
  }

let ref_one = { ref_many with cardinality = One }

let one = { ref_many with cardinality = One; value_type = None }

let test_wildcard_pull_page_missing () =
  let page_count = 20 in
  let noise_count = 500_000 in
  let page_datoms =
    List.concat
      (List.init page_count (fun index ->
         let page = 1_000 + index in
         let block = 10_000 + index in
         [ datom ~e:page ~a:"block/name" ~v:(String (Printf.sprintf "page-%d" index)) ()
         ; datom ~e:page ~a:"block/title" ~v:(String (Printf.sprintf "Page %d" index)) ()
         ; datom ~e:block ~a:"block/title" ~v:(String (Printf.sprintf "Block %d" index)) ()
         ; datom ~e:block ~a:"block/page" ~v:(Ref page) ()
         ]))
  in
  let noise_datoms =
    List.init noise_count (fun index ->
      datom ~e:(100_000 + index) ~a:"noise/value" ~v:(String (Printf.sprintf "noise-%d" index)) ())
  in
  let db =
    init_db
      ~schema:
        [ "block/name", one
        ; "block/title", one
        ; "block/page", ref_one
        ; "logseq.property/built-in?", one
        ; "noise/value", one
        ]
      (page_datoms @ noise_datoms)
  in
  Printf.eprintf "[repro] db max_e=%d\n%!" db.max_datom_e;
  let started = Unix.gettimeofday () in
  let result =
    q_return_string
      db
      "[:find (pull ?p [*]) :where [?b :block/title] [?b :block/page ?p] [(missing? $ ?p :logseq.property/built-in?)]]"
  in
  let elapsed = Unix.gettimeofday () -. started in
  match result with
  | Query_relation rows ->
    Printf.eprintf "[repro] elapsed=%.3fs rows=%d\n%!" elapsed (List.length rows);
    if elapsed > 3.0 then (
      Printf.eprintf "[repro] FAIL: exceeded 3s threshold\n%!";
      exit 1)
  | _ ->
    Printf.eprintf "[repro] unexpected result\n%!";
    exit 1

let () = test_wildcard_pull_page_missing ()
