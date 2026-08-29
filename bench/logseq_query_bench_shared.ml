(* Logseq-shaped shared query suite for cross-runtime comparison.
   Durable backend: SQLite (store + restore) — same shape as the nbb CLJS harness. *)

open Datascript

type config =
  { size : int
  ; pages : int
  ; warmup_ms : float
  ; sample_ms : float
  ; repeats : int
  ; step : int
  ; jit_warmup : int
  ; query : string option
  ; sqlite_path : string option
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
  ; sqlite_path = None
  }

let int_from_env name default =
  match Sys.getenv_opt name with
  | Some value -> int_of_string value
  | None -> default

let float_from_env name default =
  match Sys.getenv_opt name with
  | Some value -> float_of_string value
  | None -> default

let parse_args () =
  let config =
    ref
      { default_config with
        warmup_ms = float_from_env "BENCH_WARMUP_MS" default_config.warmup_ms
      ; sample_ms = float_from_env "BENCH_SAMPLE_MS" default_config.sample_ms
      ; repeats = int_from_env "BENCH_REPEATS" default_config.repeats
      ; jit_warmup = int_from_env "BENCH_JIT_WARMUP" default_config.jit_warmup
      ; size = int_from_env "BENCH_SIZE" default_config.size
      ; pages = int_from_env "BENCH_PAGES" default_config.pages
      }
  in
  let rec loop = function
    | [] -> !config
    | "--size" :: v :: rest ->
      config := { !config with size = int_of_string v };
      loop rest
    | "--pages" :: v :: rest ->
      config := { !config with pages = int_of_string v };
      loop rest
    | "--warmup-ms" :: v :: rest ->
      config := { !config with warmup_ms = float_of_string v };
      loop rest
    | "--sample-ms" :: v :: rest ->
      config := { !config with sample_ms = float_of_string v };
      loop rest
    | "--repeats" :: v :: rest ->
      config := { !config with repeats = int_of_string v };
      loop rest
    | "--jit-warmup" :: v :: rest ->
      config := { !config with jit_warmup = int_of_string v };
      loop rest
    | "--query" :: v :: rest ->
      config := { !config with query = Some v };
      loop rest
    | "--sqlite" :: v :: rest ->
      config := { !config with sqlite_path = Some v };
      loop rest
    | arg :: _ -> invalid_arg ("unknown argument: " ^ arg)
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
  | first :: rest -> bump (List.length first + if rest == [] then 0 else 1)

let keep_take n pred seq =
  let rec loop i seq acc =
    if i <= 0 then List.rev acc
    else
      match seq () with
      | Seq.Nil -> List.rev acc
      | Seq.Cons (x, xs) -> if pred x then loop (i - 1) xs (x :: acc) else loop i xs acc
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
let journal_day_of i = 202_501_01 + (i mod 400)

let build_graph ~size ~pages =
  let pages = max 1 (min pages size) in
  let base_ms = 1_700_000_000_000 in
  let day_ms = 86_400_000 in
  let tag_count = min 32 pages in
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
      if e mod 7 = 0 then [ "block/tags", Many_values [ Ref ((e mod tag_count) + 1) ] ] else []
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
  Array.to_list ops, pages, base_ms

let remove_path path =
  if Sys.file_exists path then Sys.remove path;
  List.iter
    (fun suffix ->
      let sibling = path ^ suffix in
      if Sys.file_exists sibling then Sys.remove sibling)
    [ "-wal"; "-shm"; "-lock" ]

let file_size path =
  if Sys.file_exists path then (Unix.stat path).st_size else 0

type prepared =
  { db : db
  ; pages : int
  ; base_ms : int
  ; sample_uuid : string
  ; sample_page : entity_id
  ; sample_tag : entity_id
  ; build_ms : float
  ; restore_ms : float
  ; sqlite_path : string
  ; cleanup : unit -> unit
  }

let build ~size ~pages ~sqlite_path =
  let ops, pages, base_ms = build_graph ~size ~pages in
  let path =
    match sqlite_path with
    | Some p -> p
    | None -> Filename.temp_file "logseq-query-bench-shared-" ".sqlite3"
  in
  remove_path path;
  let started = now_ms () in
  let session = Datascript_sqlite.open_session path in
  (* Current branch: sqlite plugin returns Datascript_types.storage (Storage_handle).
     origin/main returns Datascript.storage directly — compare script strips
     storage_of_handle / refresh_db_indexes when copying into the main worktree. *)
  let storage = storage_of_handle (Datascript_sqlite.storage session) in
  let db = db_with ops (empty_db ~schema ~storage ()) in
  let db = refresh_db_indexes db in
  store db;
  collect_garbage storage;
  let build_ms = now_ms () -. started in
  let restore_started = now_ms () in
  let restored =
    match restore storage with
    | Some db -> db
    | None -> failwith "sqlite restore failed"
  in
  let restore_ms = now_ms () -. restore_started in
  { db = restored
  ; pages
  ; base_ms
  ; sample_uuid = uuid_of (max 1 (pages / 2))
  ; sample_page = 1
  ; sample_tag = 1
  ; build_ms
  ; restore_ms
  ; sqlite_path = path
  ; cleanup =
      (fun () ->
        Datascript_sqlite.close session;
        remove_path path)
  }

let hydrate_forward e =
  List.iter
    (fun attr -> match entity_attr e attr with Some _ -> bump 1 | None -> ())
    [ "block/uuid"; "block/title"; "block/name"; "block/updated-at"; "block/journal-day" ]

let avet_attr_rseq db attr =
  (* Match CLJS `(rseq (datoms :avet attr))` — reverse scan of one attr only. *)
  rseek_datoms db Avet ~a:attr ()

let is_page db d =
  match Seq.uncons (datoms db Eavt ~e:d.e ~a:"block/page" ()) with
  | Some _ -> false
  | None -> (
    match Seq.uncons (datoms db Eavt ~e:d.e ~a:"block/title" ()) with
    | Some (t, _) -> (match t.v with String s -> String.trim s <> "" | _ -> false)
    | None -> false)

let recent_page_datoms p =
  keep_take 15 (is_page p.db) (avet_attr_rseq p.db "block/updated-at")

let journal_day_value = function
  | Int day -> Some day
  | Float f when float_of_int (int_of_float f) = f -> Some (int_of_float f)
  | _ -> None

let latest_journal_datoms p =
  let today = journal_day_of p.pages in
  keep_take 10
    (fun d -> match journal_day_value d.v with Some day -> day <= today | None -> false)
    (avet_attr_rseq p.db "block/journal-day")

(* Canonical EDN strings for cross-runtime result equality (match Clojure pr-str). *)
let edn_of_value = function
  | Nil -> "nil"
  | Bool true -> "true"
  | Bool false -> "false"
  | Int i -> string_of_int i
  | Float f when float_of_int (int_of_float f) = f -> string_of_int (int_of_float f)
  | Float f ->
    (* Match JS/Clojure number print for this workload (ints + small floats). *)
    let s = Printf.sprintf "%.15g" f in
    if String.contains s '.' || String.contains s 'e' || String.contains s 'E' then s
    else s ^ ".0"
  | String s -> "\"" ^ String.escaped s ^ "\""
  | Keyword k -> ":" ^ k
  | Symbol s -> s
  | Uuid u -> "#uuid \"" ^ u ^ "\""
  | Instant i -> string_of_int i
  | Ref e -> string_of_int e
  | other ->
    (* Fallback: keep type visible without inventing EDN readers. *)
    match other with
    | Regex r -> "#\"" ^ String.escaped r ^ "\""
    | _ -> "nil"

let edn_vector items = "[" ^ String.concat " " items ^ "]"

let edn_of_tx_value = function
  | One_value v -> edn_of_value v
  | Many_values vs ->
    vs
    |> List.map edn_of_value
    |> List.sort String.compare
    |> edn_vector
  | One_entity _ | Many_entities _ -> "nil"

let edn_of_result_cell = function
  | Result_value v -> edn_of_value v
  | Result_entity e -> string_of_int e
  | Result_attr a -> ":" ^ a
  | Result_db _ -> "$"
  | Result_pull _ -> "nil"

let edn_of_q_rows rows =
  rows
  |> List.map (fun row -> edn_vector (List.map edn_of_result_cell row))
  |> List.sort String.compare
  |> edn_vector

let hydrate_edn_map e =
  [ "block/uuid"; "block/title"; "block/name"; "block/updated-at"; "block/journal-day" ]
  |> List.filter_map (fun attr ->
    match entity_attr e attr with
    | None -> None
    | Some tv -> Some (edn_vector [ ":" ^ attr; edn_of_tx_value tv ]))
  |> List.sort String.compare
  |> edn_vector

(* Pre-parse once — matches CLJS/Datahike quoted queries (no per-op string parse). *)
let q_updated_at_between_query =
  parse_query_string
    "[:find ?e ?t :in $ ?lo ?hi :where [?e :block/updated-at ?t] [(>= ?t ?lo)] [(<= ?t ?hi)]]"

let q_journal_pages_query =
  parse_query_string "[:find ?e ?d :where [?e :block/journal-day ?d] [?e :block/title ?t]]"

let q_page_by_name_query =
  parse_query_string "[:find ?e :in $ ?n :where [?e :block/name ?n]]"

let result_edn p name =
  match name with
  | "recent-pages" ->
    recent_page_datoms p |> List.map (fun d -> string_of_int d.e) |> edn_vector
  | "latest-journals" ->
    latest_journal_datoms p |> List.map (fun d -> string_of_int d.e) |> edn_vector
  | "uuid-lookup" -> (
    match entity p.db (Lookup_ref ("block/uuid", String p.sample_uuid)) with
    | None -> "nil"
    | Some e -> edn_vector [ string_of_int e.id; hydrate_edn_map e ])
  | "title-lookup" ->
    let title = Printf.sprintf "Page %d" (p.pages / 2) in
    datoms p.db Avet ~a:"block/title" ~v:(String title) ()
    |> List.of_seq
    |> List.map (fun d -> d.e)
    |> List.sort compare
    |> List.map string_of_int
    |> edn_vector
  | "children-by-parent" ->
    datoms p.db Avet ~a:"block/parent" ~v:(Ref p.sample_page) ()
    |> List.of_seq
    |> List.map (fun d -> d.e)
    |> List.sort compare
    |> List.map string_of_int
    |> edn_vector
  | "blocks-by-page" ->
    datoms p.db Avet ~a:"block/page" ~v:(Ref p.sample_page) ()
    |> List.of_seq
    |> List.map (fun d -> d.e)
    |> List.sort compare
    |> List.map string_of_int
    |> edn_vector
  | "tags-scan" ->
    datoms p.db Avet ~a:"block/tags" ~v:(Ref p.sample_tag) ()
    |> List.of_seq
    |> List.map (fun d -> d.e)
    |> List.sort compare
    |> List.map string_of_int
    |> edn_vector
  | "eavt-entity" ->
    datoms p.db Eavt ~e:p.sample_page ()
    |> List.of_seq
    |> List.map (fun d -> edn_vector [ ":" ^ d.a; edn_of_value d.v ])
    |> List.sort String.compare
    |> edn_vector
  | "entity-hydrate" -> (
    match entity p.db (Entity_id p.sample_page) with
    | None -> "nil"
    | Some e -> edn_vector [ string_of_int e.id; hydrate_edn_map e ])
  | "q-updated-at-between" ->
    let lo = p.base_ms + 3_600_000 in
    let hi = p.base_ms + 86_400_000 in
    edn_of_q_rows
      (q
         ~inputs:
           [ Arg_scalar (Result_value (Int lo)); Arg_scalar (Result_value (Int hi)) ]
         p.db q_updated_at_between_query)
  | "q-journal-pages" ->
    edn_of_q_rows (q p.db q_journal_pages_query)
  | "q-page-by-name" ->
    let name = Printf.sprintf "page-%d" (p.pages / 3) in
    edn_of_q_rows
      (q ~inputs:[ Arg_scalar (Result_value (String name)) ] p.db q_page_by_name_query)
  | other -> invalid_arg ("unknown result-edn query: " ^ other)

let recent_pages p =
  List.iter
    (fun d -> match entity p.db (Entity_id d.e) with Some e -> hydrate_forward e | None -> ())
    (recent_page_datoms p)

let latest_journals p =
  List.iter
    (fun d -> match entity p.db (Entity_id d.e) with Some e -> hydrate_forward e | None -> ())
    (latest_journal_datoms p)

let uuid_lookup p =
  match entity p.db (Lookup_ref ("block/uuid", String p.sample_uuid)) with
  | Some e -> hydrate_forward e
  | None -> bump 0

let title_lookup p =
  let title = Printf.sprintf "Page %d" (p.pages / 2) in
  consume_seq (datoms p.db Avet ~a:"block/title" ~v:(String title) ())

let children_by_parent p =
  consume_seq (datoms p.db Avet ~a:"block/parent" ~v:(Ref p.sample_page) ())

let blocks_by_page p =
  consume_seq (datoms p.db Avet ~a:"block/page" ~v:(Ref p.sample_page) ())

let tags_scan p =
  consume_seq (datoms p.db Avet ~a:"block/tags" ~v:(Ref p.sample_tag) ())

let eavt_entity p = consume_seq (datoms p.db Eavt ~e:p.sample_page ())

let entity_hydrate p =
  match entity p.db (Entity_id p.sample_page) with
  | Some e -> hydrate_forward e
  | None -> bump 0

let q_updated_at_between p =
  let lo = p.base_ms + 3_600_000 in
  let hi = p.base_ms + 86_400_000 in
  consume_rows
    (q
       ~inputs:
         [ Arg_scalar (Result_value (Int lo)); Arg_scalar (Result_value (Int hi)) ]
       p.db q_updated_at_between_query)

let q_journal_pages p =
  consume_rows (q p.db q_journal_pages_query)

let q_page_by_name p =
  let name = Printf.sprintf "page-%d" (p.pages / 3) in
  consume_rows
    (q ~inputs:[ Arg_scalar (Result_value (String name)) ] p.db q_page_by_name_query)

type query_case = { name : string; run : prepared -> unit }

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
  ; { name = "q-updated-at-between"; run = q_updated_at_between }
  ; { name = "q-journal-pages"; run = q_journal_pages }
  ; { name = "q-page-by-name"; run = q_page_by_name }
  ]

let select_queries = function
  | None -> queries
  | Some name -> (
    match List.find_opt (fun q -> q.name = name) queries with
    | Some q -> [ q ]
    | None ->
      invalid_arg
        (Printf.sprintf "unknown query %S (available: %s)" name
           (String.concat ", " (List.map (fun q -> q.name) queries))))

let () =
  if Array.mem "--list-queries" Sys.argv then (
    List.iter (fun q -> Printf.printf "%s\n%!" q.name) queries;
    exit 0);
  let config = parse_args () in
  let selected = select_queries config.query in
  let label =
    match Sys.getenv_opt "BENCH_RUNTIME_LABEL" with
    | Some l -> l
    | None -> "ocaml"
  in
  Printf.printf "runtime\t%s\n%!" label;
  Printf.printf "suite\tlogseq-queries-shared\n%!";
  Printf.printf "backend\tsqlite\n%!";
  Printf.printf "size\t%d\n%!" config.size;
  Printf.printf "pages\t%d\n%!" config.pages;
  Printf.printf "warmup-ms\t%.0f\n%!" config.warmup_ms;
  Printf.printf "sample-ms\t%.0f\n%!" config.sample_ms;
  Printf.printf "repeats\t%d\n%!" config.repeats;
  Printf.printf "jit-warmup\t%d\n%!" config.jit_warmup;
  Printf.printf "query-cases\t%d\n%!" (List.length selected);
  Printf.eprintf
    "Building sqlite logseq graph (entities=%d pages=%d)...\n%!" config.size config.pages;
  let prepared = build ~size:config.size ~pages:config.pages ~sqlite_path:config.sqlite_path in
  Fun.protect ~finally:prepared.cleanup (fun () ->
      Printf.printf "sqlite-path\t%s\n%!" prepared.sqlite_path;
      Printf.printf "disk-bytes\t%d\n%!" (file_size prepared.sqlite_path);
      Printf.printf "build-ms\t%s\n%!" (format_ms prepared.build_ms);
      Printf.printf "restore-ms\t%s\n%!" (format_ms prepared.restore_ms);
      List.iter
        (fun q ->
          Printf.printf "result-edn\t%s\t%s\n%!" q.name (result_edn prepared q.name))
        selected;
      if config.jit_warmup > 0 then
        List.iter
          (fun q ->
            for _ = 1 to config.jit_warmup do
              q.run prepared
            done)
          selected;
      List.iter
        (fun q ->
          let ms = bench config (fun () -> q.run prepared) in
          Printf.printf "%s\t%s\n%!" q.name (format_ms ms))
        selected;
      Printf.eprintf "blackhole=%d\n%!" !blackhole)
