(* Logseq-shaped query microbench.
   Patterns mirror deps/db/.../initial_data.cljs hot paths and common
   d/entity + AVET lookups — not the people/follows shared suite. *)

open Datascript

type storage_backend =
  | Memory_lmdb_nosync
  | Lmdb_file
  | Sqlite_file

type config =
  { size : int
  ; pages : int
  ; warmup_ms : float
  ; sample_ms : float
  ; repeats : int
  ; step : int
  ; jit_warmup : int
  ; query : string option
  ; storages : storage_backend list
  ; data_dir : string
  }

let default_config =
  { size = 20_000
  ; pages = 2_000
  ; warmup_ms = 200.
  ; sample_ms = 200.
  ; repeats = 3
  ; step = 5
  ; jit_warmup = 50
  ; query = None
  ; storages = [ Memory_lmdb_nosync; Lmdb_file; Sqlite_file ]
  ; data_dir = Filename.get_temp_dir_name ()
  }

let storage_label = function
  | Memory_lmdb_nosync -> "memory-lmdb-nosync"
  | Lmdb_file -> "lmdb"
  | Sqlite_file -> "sqlite"

let parse_storage_list value =
  value
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> List.map (function
    | "memory-lmdb-nosync" | "memory" -> Memory_lmdb_nosync
    | "lmdb" -> Lmdb_file
    | "sqlite" -> Sqlite_file
    | other ->
      invalid_arg
        ("unknown storage "
        ^ other
        ^ " (expected: memory-lmdb-nosync|lmdb|sqlite, comma-separated)"))

let int_from_env name default =
  match Sys.getenv_opt name with
  | Some value -> int_of_string value
  | None -> default

let float_from_env name default =
  match Sys.getenv_opt name with
  | Some value -> float_of_string value
  | None -> default

let config_from_env base =
  { base with
    warmup_ms = float_from_env "BENCH_WARMUP_MS" base.warmup_ms
  ; sample_ms = float_from_env "BENCH_SAMPLE_MS" base.sample_ms
  ; repeats = int_from_env "BENCH_REPEATS" base.repeats
  ; jit_warmup = int_from_env "BENCH_JIT_WARMUP" base.jit_warmup
  ; size = int_from_env "BENCH_SIZE" base.size
  ; pages = int_from_env "BENCH_PAGES" base.pages
  }

let parse_args () =
  let config = ref (config_from_env default_config) in
  let debug_profile = ref false in
  let set_size value = config := { !config with size = int_of_string value } in
  let set_pages value = config := { !config with pages = int_of_string value } in
  let set_warmup value = config := { !config with warmup_ms = float_of_string value } in
  let set_sample_ms value = config := { !config with sample_ms = float_of_string value } in
  let set_repeats value = config := { !config with repeats = int_of_string value } in
  let set_jit_warmup value = config := { !config with jit_warmup = int_of_string value } in
  let set_query value = config := { !config with query = Some value } in
  let set_storage value =
    config :=
      { !config with
        storages =
          (match value with
           | "all" -> [ Memory_lmdb_nosync; Lmdb_file; Sqlite_file ]
           | "compare" -> [ Lmdb_file; Sqlite_file ]
           | other -> parse_storage_list other)
      }
  in
  let set_data_dir value = config := { !config with data_dir = value } in
  let rec loop = function
    | [] -> !config, !debug_profile
    | "--size" :: value :: rest ->
      set_size value;
      loop rest
    | "--pages" :: value :: rest ->
      set_pages value;
      loop rest
    | "--warmup-ms" :: value :: rest ->
      set_warmup value;
      loop rest
    | "--sample-ms" :: value :: rest ->
      set_sample_ms value;
      loop rest
    | "--repeats" :: value :: rest ->
      set_repeats value;
      loop rest
    | "--jit-warmup" :: value :: rest ->
      set_jit_warmup value;
      loop rest
    | "--query" :: value :: rest ->
      set_query value;
      loop rest
    | "--storage" :: value :: rest ->
      set_storage value;
      loop rest
    | "--data-dir" :: value :: rest ->
      set_data_dir value;
      loop rest
    | "--debug-profile" :: rest ->
      debug_profile := true;
      loop rest
    | arg :: _ -> invalid_arg ("unknown benchmark argument: " ^ arg)
  in
  Sys.argv |> Array.to_list |> List.tl |> loop

let now_ms () = Unix.gettimeofday () *. 1000.

let median values =
  let sorted = List.sort Float.compare values in
  List.nth sorted (List.length sorted / 2)

let format_ms value =
  if value > 1. then Printf.sprintf "%.2f" value
  else if value > 0.01 then Printf.sprintf "%.3f" value
  else Printf.sprintf "%.4f" value

let blackhole = ref 0

let bump n = blackhole := (!blackhole + n) land 0x3fffffff

let consume_seq seq = bump (Seq.fold_left (fun n _ -> n + 1) 0 seq)

let consume_rows rows =
  match rows with
  | [] -> ()
  | first :: rest ->
    bump (List.length first + if rest == [] then 0 else 1)

let keep_take n pred seq =
  let rec loop i seq acc =
    if i <= 0 then List.rev acc
    else
      match seq () with
      | Seq.Nil -> List.rev acc
      | Seq.Cons (x, xs) ->
        if pred x then loop (i - 1) xs (x :: acc) else loop i xs acc
  in
  loop n seq []

let dotime duration_ms step f =
  let start = now_ms () in
  let deadline = start +. duration_ms in
  let rec loop iterations =
    for _ = 1 to step do
      f ()
    done;
    let iterations = iterations + step in
    if now_ms () < deadline then loop iterations else (now_ms () -. start) /. float iterations
  in
  loop step

let bench config f =
  ignore (dotime config.warmup_ms config.step f);
  let samples = List.init config.repeats (fun _ -> dotime config.sample_ms config.step f) in
  median samples

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
let ref_one = { indexed with value_type = Some RefType }
let ref_many = { ref_one with cardinality = Many }

(* Minimal Logseq-like attrs used by initial_data / common lookups. *)
let schema =
  [ "block/uuid", unique_identity
  ; "block/title", indexed
  ; "block/name", indexed
  ; "block/updated-at", indexed
  ; "block/created-at", indexed
  ; "block/journal-day", indexed
  ; "block/parent", ref_one
  ; "block/page", ref_one
  ; "block/tags", ref_many
  ; "block/refs", ref_many
  ; "block/content", indexed
  ]

let uuid_of i = Printf.sprintf "00000000-0000-4000-8000-%012d" i

(* Day integers like Logseq journal-day (YYYYMMDD-ish packed ints). *)
let journal_day_of i = 202_501_01 + (i mod 400)

let build_logseq_graph ~size ~pages =
  let pages = max 1 (min pages size) in
  let base_ms = 1_700_000_000_000 in
  let day_ms = 86_400_000 in
  let tag_count = min 32 pages in
  (* Pages sit at the high end of updated-at so rseek finds them quickly —
     matching “recently edited pages” rather than burying them under blocks. *)
  let page_updated e = base_ms + (10 * day_ms) + (e * 1_000) in
  let block_updated e = base_ms + (e * 30) in
  let page_entity e =
    let updated = page_updated e in
    let is_journal = e mod 5 = 0 in
    let attrs =
      [ "block/uuid", One_value (String (uuid_of e))
      ; "block/title", One_value (String (Printf.sprintf "Page %d" e))
      ; "block/name", One_value (String (Printf.sprintf "page-%d" e))
      ; "block/updated-at", One_value (Int updated)
      ; "block/created-at", One_value (Int (updated - day_ms))
      ; "block/content", One_value (String (Printf.sprintf "page body %d" e))
      ]
      @ (if is_journal then [ "block/journal-day", One_value (Int (journal_day_of e)) ] else [])
      @
      if e mod 7 = 0 then
        [ "block/tags", Many_values [ Ref ((e mod tag_count) + 1) ] ]
      else
        []
    in
    Entity { db_id = Some (Entity_id e); attrs }
  in
  let block_entity index =
    let e = pages + index + 1 in
    let page = 1 + (index mod pages) in
    let parent = if index = 0 || index mod 3 = 0 then page else e - 1 in
    let updated = block_updated e in
    let attrs =
      [ "block/uuid", One_value (String (uuid_of e))
      ; "block/title", One_value (String (Printf.sprintf "Block %d" e))
      ; "block/updated-at", One_value (Int updated)
      ; "block/created-at", One_value (Int (updated - 60_000))
      ; "block/parent", One_value (Ref parent)
      ; "block/page", One_value (Ref page)
      ; "block/content", One_value (String (Printf.sprintf "block body %d" e))
      ]
      @
      if e mod 11 = 0 then
        [ "block/tags", Many_values [ Ref ((e mod tag_count) + 1) ]
        ; "block/refs", Many_values [ Ref page ]
        ]
      else
        []
    in
    Entity { db_id = Some (Entity_id e); attrs }
  in
  let ops =
    Array.init size (fun index ->
        if index < pages then page_entity (index + 1) else block_entity (index - pages))
  in
  ops, pages, base_ms

type prepared =
  { label : string
  ; db : db
  ; pages : int
  ; size : int
  ; base_ms : int
  ; mid_tx : tx
  ; sample_uuid : string
  ; sample_page : entity_id
  ; sample_tag : entity_id
  ; build_ms : float
  ; restore_ms : float
  ; path : string option
  ; cleanup : unit -> unit
  }

let remove_path path =
  if Sys.file_exists path then Sys.remove path;
  List.iter
    (fun suffix ->
      let sibling = path ^ suffix in
      if Sys.file_exists sibling then Sys.remove sibling)
    [ "-wal"; "-shm"; "-lock" ]

let file_size path =
  if Sys.file_exists path then (Unix.stat path).st_size else 0

let disk_footprint path =
  List.fold_left
    (fun total suffix -> total + file_size (if suffix = "" then path else path ^ suffix))
    0
    [ ""; "-wal"; "-shm"; "-lock" ]

let build_db ~storage ~persist ~size ~pages =
  set_tave_retention_days 30;
  let ops, pages, base_ms = build_logseq_graph ~size ~pages in
  let started = now_ms () in
  (* Spread entities across txs so since/TAVE has a meaningful window. *)
  let batch = 250 in
  let rec loop db i =
    if i >= Array.length ops then db
    else
      let hi = min (Array.length ops) (i + batch) in
      let chunk = Array.to_list (Array.sub ops i (hi - i)) in
      let instant = base_ms + (i * 1_000) in
      let r = transact ~tx_meta:[ "db/txInstant", Instant instant ] db chunk in
      loop r.db_after hi
  in
  let db = loop (empty_db ~schema ~storage ()) 0 in
  let db = refresh_db_indexes db in
  let build_ms = now_ms () -. started in
  let mid_tx = db.max_tx / 2 in
  let sample_page = 1 in
  let sample_tag = 1 in
  let sample_uuid = uuid_of (max 1 (pages / 2)) in
  if not persist then
    db, pages, base_ms, mid_tx, sample_uuid, sample_page, sample_tag, build_ms, 0.
  else
    let store_started = now_ms () in
    store db;
    collect_garbage storage;
    let restored =
      match restore storage with
      | Some db -> db
      | None -> failwith "storage-backed logseq bench should restore"
    in
    let restore_ms = now_ms () -. store_started in
    restored, pages, base_ms, mid_tx, sample_uuid, sample_page, sample_tag, build_ms, restore_ms

let prepare_backend ~data_dir backend ~size ~pages =
  match backend with
  | Memory_lmdb_nosync ->
    let storage = benchmark_memory_storage () in
    let db, pages, base_ms, mid_tx, sample_uuid, sample_page, sample_tag, build_ms, restore_ms =
      build_db ~storage ~persist:false ~size ~pages
    in
    { label = storage_label backend
    ; db
    ; pages
    ; size
    ; base_ms
    ; mid_tx
    ; sample_uuid
    ; sample_page
    ; sample_tag
    ; build_ms
    ; restore_ms
    ; path = None
    ; cleanup = Fun.id
    }
  | Lmdb_file ->
    let path =
      Filename.concat data_dir (Printf.sprintf "logseq-query-bench-lmdb-%d.mdb" size)
    in
    remove_path path;
    let session = Datascript_lmdb.open_session path in
    let storage = storage_of_handle (Datascript_lmdb.storage session) in
    let db, pages, base_ms, mid_tx, sample_uuid, sample_page, sample_tag, build_ms, restore_ms =
      build_db ~storage ~persist:true ~size ~pages
    in
    { label = storage_label backend
    ; db
    ; pages
    ; size
    ; base_ms
    ; mid_tx
    ; sample_uuid
    ; sample_page
    ; sample_tag
    ; build_ms
    ; restore_ms
    ; path = Some path
    ; cleanup =
        (fun () ->
          Datascript_lmdb.close session;
          remove_path path)
    }
  | Sqlite_file ->
    let path =
      Filename.concat data_dir
        (Printf.sprintf "logseq-query-bench-sqlite-%d.sqlite3" size)
    in
    remove_path path;
    let session = Datascript_sqlite.open_session path in
    let storage = storage_of_handle (Datascript_sqlite.storage session) in
    let db, pages, base_ms, mid_tx, sample_uuid, sample_page, sample_tag, build_ms, restore_ms =
      build_db ~storage ~persist:true ~size ~pages
    in
    { label = storage_label backend
    ; db
    ; pages
    ; size
    ; base_ms
    ; mid_tx
    ; sample_uuid
    ; sample_page
    ; sample_tag
    ; build_ms
    ; restore_ms
    ; path = Some path
    ; cleanup =
        (fun () ->
          Datascript_sqlite.close session;
          remove_path path)
    }

type query_case =
  { name : string
  ; run : prepared -> unit
  }

(* Selective forward attrs — Logseq page?/title checks are lazy, not pull [*]. *)
let hydrate_forward e =
  List.iter
    (fun attr -> match entity_attr e attr with Some _ -> bump 1 | None -> ())
    [ "block/uuid"; "block/title"; "block/name"; "block/updated-at"; "block/journal-day" ]

(* Logseq: (rseq (d/datoms db :avet attr)) — exact attr, then reverse.
   Not rseek-datoms (which continues into earlier attrs per DataScript). *)
let avet_attr_rseq db attr =
  datoms db Avet ~a:attr () |> List.of_seq |> List.rev |> List.to_seq

(* Mirror get-recent-updated-pages: reverse AVET updated-at, keep pages, take 15. *)
let recent_pages prepared =
  let db = prepared.db in
  let is_page d =
    match Seq.uncons (datoms db Eavt ~e:d.e ~a:"block/page" ()) with
    | Some _ -> false
    | None -> (
      match Seq.uncons (datoms db Eavt ~e:d.e ~a:"block/title" ()) with
      | Some (t, _) -> (match t.v with String s -> String.trim s <> "" | _ -> false)
      | None -> false)
  in
  let pages = keep_take 15 is_page (avet_attr_rseq db "block/updated-at") in
  List.iter
    (fun d ->
      match entity db (Entity_id d.e) with
      | Some e -> hydrate_forward e
      | None -> ())
    pages

(* Mirror get-latest-journals: reverse journal-day AVET, take 10 journals. *)
let latest_journals prepared =
  let today = journal_day_of prepared.pages in
  let kept =
    keep_take 10
      (fun d -> match d.v with Int day -> day <= today | _ -> false)
      (avet_attr_rseq prepared.db "block/journal-day")
  in
  List.iter
    (fun d ->
      match entity prepared.db (Entity_id d.e) with
      | Some e -> hydrate_forward e
      | None -> ())
    kept

let uuid_lookup prepared =
  match entity prepared.db (Lookup_ref ("block/uuid", String prepared.sample_uuid)) with
  | Some e -> hydrate_forward e
  | None -> bump 0

let title_lookup prepared =
  let title = Printf.sprintf "Page %d" (prepared.pages / 2) in
  consume_seq (datoms prepared.db Avet ~a:"block/title" ~v:(String title) ())

let children_by_parent prepared =
  consume_seq
    (datoms prepared.db Avet ~a:"block/parent" ~v:(Ref prepared.sample_page) ())

let blocks_by_page prepared =
  consume_seq (datoms prepared.db Avet ~a:"block/page" ~v:(Ref prepared.sample_page) ())

let tags_scan prepared =
  consume_seq (datoms prepared.db Avet ~a:"block/tags" ~v:(Ref prepared.sample_tag) ())

let eavt_entity prepared =
  consume_seq (datoms prepared.db Eavt ~e:prepared.sample_page ())

let entity_hydrate prepared =
  match entity prepared.db (Entity_id prepared.sample_page) with
  | Some e -> hydrate_forward e
  | None -> bump 0

(* Full materialize including reverse refs (all_datoms scan) — expensive; kept for contrast. *)
let entity_attrs_full prepared =
  match entity prepared.db (Entity_id prepared.sample_page) with
  | Some e -> bump (List.length (entity_attrs e))
  | None -> bump 0

(* DSL-like between on updated-at (Logseq (between …) expands similarly). *)
let q_updated_at_between prepared =
  let lo = prepared.base_ms + 3_600_000 in
  let hi = prepared.base_ms + 86_400_000 in
  let query =
    "[:find ?e ?t :in $ ?lo ?hi :where [?e :block/updated-at ?t] [(>= ?t ?lo)] [(<= ?t ?hi)]]"
  in
  consume_rows
    (q_string
       ~inputs:
         [ Arg_scalar (Result_value (Int lo)); Arg_scalar (Result_value (Int hi)) ]
       prepared.db query)

let q_journal_pages prepared =
  consume_rows
    (q_string prepared.db
       "[:find ?e ?d :where [?e :block/journal-day ?d] [?e :block/title ?t]]")

let q_page_by_name prepared =
  let name = Printf.sprintf "page-%d" (prepared.pages / 3) in
  consume_rows
    (q_string
       ~inputs:[ Arg_scalar (Result_value (String name)) ]
       prepared.db
       "[:find ?e :in $ ?n :where [?e :block/name ?n]]")

(* Engine path Logseq does not use today: since + attr AEVT (TAVE when clean). *)
let since_attr_aevt prepared =
  consume_seq (datoms (since prepared.mid_tx prepared.db) Aevt ~a:"block/updated-at" ())

let full_attr_aevt prepared =
  consume_seq (datoms prepared.db Aevt ~a:"block/updated-at" ())

let since_attr_avet prepared =
  consume_seq (datoms (since prepared.mid_tx prepared.db) Avet ~a:"block/updated-at" ())

let queries =
  [ { name = "recent-pages"; run = recent_pages }
  ; { name = "latest-journals"; run = latest_journals }
  ; { name = "uuid-lookup"; run = uuid_lookup }
  ; { name = "title-lookup"; run = title_lookup }
  ; { name = "children-by-parent"; run = children_by_parent }
  ; { name = "blocks-by-page"; run = blocks_by_page }
  ; { name = "tags-scan"; run = tags_scan }
  ; { name = "eavt-entity"; run = eavt_entity }
  ; { name = "entity-hydrate"; run = entity_hydrate }
  ; { name = "entity-attrs-full"; run = entity_attrs_full }
  ; { name = "q-updated-at-between"; run = q_updated_at_between }
  ; { name = "q-journal-pages"; run = q_journal_pages }
  ; { name = "q-page-by-name"; run = q_page_by_name }
  ; { name = "since-attr-aevt"; run = since_attr_aevt }
  ; { name = "full-attr-aevt"; run = full_attr_aevt }
  ; { name = "since-attr-avet"; run = since_attr_avet }
  ]

let query_names = List.map (fun q -> q.name) queries

let select_queries = function
  | None -> queries
  | Some name -> (
    match List.find_opt (fun q -> q.name = name) queries with
    | Some q -> [ q ]
    | None ->
      invalid_arg
        (Printf.sprintf "unknown query %S (available: %s)" name (String.concat ", " query_names)))

let warmup_queries jit_warmup selected prepared =
  if jit_warmup <= 0 then ()
  else
    List.iter
      (fun query ->
        for _ = 1 to jit_warmup do
          query.run prepared
        done)
      selected

let run_backend config selected prepared =
  Printf.printf "storage\t%s\n%!" prepared.label;
  (match prepared.path with
   | Some path ->
     Printf.printf "path\t%s\n%!" path;
     Printf.printf "disk-bytes\t%d\n%!" (disk_footprint path)
   | None -> Printf.printf "path\tmemory\n%!");
  Printf.printf "pages\t%d\n%!" prepared.pages;
  Printf.printf "entities\t%d\n%!" prepared.size;
  Printf.printf "mid-tx\t%d\n%!" prepared.mid_tx;
  Printf.printf "build-ms\t%s\n%!" (format_ms prepared.build_ms);
  if prepared.restore_ms > 0. then
    Printf.printf "store-restore-ms\t%s\n%!" (format_ms prepared.restore_ms);
  Printf.eprintf
    "[%s] JIT pre-warmup (%d/query)...\n%!"
    prepared.label
    config.jit_warmup;
  warmup_queries config.jit_warmup selected prepared;
  Printf.eprintf "[%s] Running %d logseq query cases...\n%!" prepared.label (List.length selected);
  List.iter
    (fun query ->
      let ms = bench config (fun () -> query.run prepared) in
      Printf.printf "%s\t%s\n%!" query.name (format_ms ms))
    selected

let ensure_dir path =
  let rec loop dir =
    if dir = "" || dir = Filename.current_dir_name || Sys.file_exists dir then ()
    else (
      loop (Filename.dirname dir);
      try Unix.mkdir dir 0o755 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  loop path

let debug_ms label t0 =
  let elapsed = now_ms () -. t0 in
  Printf.printf "debug\t%s\t%.3f\n%!" label elapsed;
  elapsed

(* Phase-level instrumentation for recent-pages / entity — evidence only, no behavior change. *)
let debug_profile_recent prepared =
  let db = prepared.db in
  Printf.printf "debug\tstorage\t%s\n%!" prepared.label;
  Printf.printf "debug\tentities\t%d\n%!" prepared.size;
  Printf.printf "debug\tpages\t%d\n%!" prepared.pages;
  (* 1) Force full AVET updated-at via datoms (ascending). *)
  let t0 = now_ms () in
  let avet_count = Seq.fold_left (fun n _ -> n + 1) 0 (datoms db Avet ~a:"block/updated-at" ()) in
  ignore (debug_ms "datoms-avet-updated-at-full-count" t0);
  Printf.printf "debug\tavet-updated-at-count\t%d\n%!" avet_count;
  (* 2) Force full rseek of same attr. *)
  let t0 = now_ms () in
  let rseek_count =
    Seq.fold_left (fun n _ -> n + 1) 0 (rseek_datoms db Avet ~a:"block/updated-at" ())
  in
  ignore (debug_ms "rseek-avet-updated-at-full-count" t0);
  Printf.printf "debug\trseek-updated-at-count\t%d\n%!" rseek_count;
  (* 3) Take first 15 from rseek with no filter. *)
  let t0 = now_ms () in
  let first15 =
    let rec loop i seq acc =
      if i <= 0 then List.rev acc
      else
        match seq () with
        | Seq.Nil -> List.rev acc
        | Seq.Cons (x, xs) -> loop (i - 1) xs (x :: acc)
    in
    loop 15 (rseek_datoms db Avet ~a:"block/updated-at" ()) []
  in
  ignore (debug_ms "rseek-take-15-nofilter" t0);
  Printf.printf "debug\trseek-first15-e\t%s\n%!"
    (String.concat "," (List.map (fun d -> string_of_int d.e) first15));
  Printf.printf "debug\trseek-first15-a\t%s\n%!"
    (String.concat "," (List.map (fun d -> d.a) first15));
  (* Logseq path: exact attr datoms then reverse *)
  let t0 = now_ms () in
  let exact_rev =
    datoms db Avet ~a:"block/updated-at" ()
    |> List.of_seq
    |> List.rev
  in
  ignore (debug_ms "datoms-avet-attr-then-list-rev" t0);
  Printf.printf "debug\texact-rev-count\t%d\n%!" (List.length exact_rev);
  let exact15 = List.filteri (fun i _ -> i < 15) exact_rev in
  Printf.printf "debug\texact-rev-first15-e\t%s\n%!"
    (String.concat "," (List.map (fun d -> string_of_int d.e) exact15));
  let t0 = now_ms () in
  let _ =
    keep_take 15
      (fun d ->
        match Seq.uncons (datoms db Eavt ~e:d.e ~a:"block/page" ()) with
        | Some _ -> false
        | None -> true)
      (List.to_seq exact_rev)
  in
  ignore (debug_ms "logseq-style-keep-take-15-on-exact-rev" t0);
  (* 4) keep_take 15 with is_page on Logseq-style exact-attr reverse. *)
  let visited = ref 0 in
  let page_hits = ref 0 in
  let filter_ms = ref 0. in
  let is_page d =
    let t = now_ms () in
    incr visited;
    let ok =
      match Seq.uncons (datoms db Eavt ~e:d.e ~a:"block/page" ()) with
      | Some _ -> false
      | None -> (
        match Seq.uncons (datoms db Eavt ~e:d.e ~a:"block/title" ()) with
        | Some (t, _) -> (match t.v with String s -> String.trim s <> "" | _ -> false)
        | None -> false)
    in
    if ok then incr page_hits;
    filter_ms := !filter_ms +. (now_ms () -. t);
    ok
  in
  let t0 = now_ms () in
  let pages = keep_take 15 is_page (avet_attr_rseq db "block/updated-at") in
  let keep_ms = debug_ms "keep-take-15-is-page-logseq-style" t0 in
  Printf.printf "debug\tkeep-visited\t%d\n%!" !visited;
  Printf.printf "debug\tkeep-page-hits\t%d\n%!" !page_hits;
  Printf.printf "debug\tkeep-filter-ms-sum\t%.3f\n%!" !filter_ms;
  Printf.printf "debug\tkeep-scan-overhead-ms\t%.3f\n%!" (keep_ms -. !filter_ms);
  Printf.printf "debug\tkeep-page-e\t%s\n%!"
    (String.concat "," (List.map (fun d -> string_of_int d.e) pages));
  (* 5) Hydrate costs: entity only, entity_attr x5, entity_attrs full. *)
  let sample_e =
    match pages with
    | d :: _ -> d.e
    | [] -> prepared.sample_page
  in
  let t0 = now_ms () in
  let ent = entity db (Entity_id sample_e) in
  ignore (debug_ms "entity-only" t0);
  (match ent with
   | None -> Printf.printf "debug\tentity-missing\t%d\n%!" sample_e
   | Some e ->
     let t0 = now_ms () in
     List.iter
       (fun attr ->
         let t = now_ms () in
         let v = entity_attr e attr in
         Printf.printf "debug\tentity-attr\t%s\t%.3f\tpresent=%b\n%!" attr (now_ms () -. t)
           (Option.is_some v))
       [ "block/uuid"; "block/title"; "block/name"; "block/updated-at"; "block/journal-day" ];
     ignore (debug_ms "hydrate-forward-5attrs" t0);
     let t0 = now_ms () in
     let n = List.length (entity_attrs e) in
     ignore (debug_ms "entity-attrs-full" t0);
     Printf.printf "debug\tentity-attrs-count\t%d\n%!" n);
  (* 6) Repeat full recent_pages once timed. *)
  let t0 = now_ms () in
  recent_pages prepared;
  ignore (debug_ms "recent-pages-once" t0);
  (* 7) Contrast: datoms Eavt for one entity. *)
  let t0 = now_ms () in
  let n = Seq.fold_left (fun n _ -> n + 1) 0 (datoms db Eavt ~e:sample_e ()) in
  ignore (debug_ms "datoms-eavt-one-entity" t0);
  Printf.printf "debug\teavt-one-entity-count\t%d\n%!" n;
  (* 8) Full EAVT count (entity_attrs reverse path scans this). *)
  let t0 = now_ms () in
  let n = Seq.fold_left (fun n _ -> n + 1) 0 (datoms db Eavt ()) in
  ignore (debug_ms "datoms-eavt-full" t0);
  Printf.printf "debug\teavt-full-count\t%d\n%!" n

let main () =
  let config, do_debug = parse_args () in
  let selected = select_queries config.query in
  ensure_dir config.data_dir;
  Printf.printf "runtime\tocaml\n%!";
  Printf.printf "suite\tlogseq-queries\n%!";
  Printf.printf "size\t%d\n%!" config.size;
  Printf.printf "pages\t%d\n%!" config.pages;
  Printf.printf "warmup-ms\t%.0f\n%!" config.warmup_ms;
  Printf.printf "sample-ms\t%.0f\n%!" config.sample_ms;
  Printf.printf "repeats\t%d\n%!" config.repeats;
  Printf.printf "jit-warmup\t%d\n%!" config.jit_warmup;
  Printf.printf "data-dir\t%s\n%!" config.data_dir;
  Printf.printf "query-cases\t%d\n%!" (List.length selected);
  Printf.printf "debug-profile\t%b\n%!" do_debug;
  (match config.query with
   | Some name -> Printf.printf "query\t%s\n%!" name
   | None -> ());
  List.iter
    (fun backend ->
      Printf.eprintf
        "Building logseq-shaped db (entities=%d pages=%d storage=%s)...\n%!"
        config.size config.pages (storage_label backend);
      let prepared =
        prepare_backend ~data_dir:config.data_dir backend ~size:config.size ~pages:config.pages
      in
      Fun.protect ~finally:prepared.cleanup (fun () ->
          if do_debug then debug_profile_recent prepared
          else run_backend config selected prepared))
    config.storages;
  Printf.eprintf "blackhole=%d\n%!" !blackhole

let () =
  if Array.mem "--list-queries" Sys.argv then (
    List.iter (fun q -> Printf.printf "%s\n%!" q.name) queries;
    exit 0);
  main ()
