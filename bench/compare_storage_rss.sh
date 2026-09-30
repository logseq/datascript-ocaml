#!/usr/bin/env bash
# Compare process RSS after common 50k ops for memory / LMDB / SQLite on this
# branch, plus main's in-memory path (main has no Share_index_db SQLite/LMDB file
# package comparable to this branch).
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

if command -v opam >/dev/null 2>&1; then
  eval "$(opam env --switch=5.5 2>/dev/null || opam env 2>/dev/null || true)"
fi

SIZE="${SIZE:-50000}"
TX_SIZE="${TX_SIZE:-200}"
DATA_DIR="${DATA_DIR:-$repo_root/_bench_data/storage-rss}"
BACKENDS="${BACKENDS:-memory,lmdb,sqlite}"
OUT="${OUT:-/opt/cursor/artifacts/bench-storage-rss-50k.txt}"
MAIN_WORKTREE="${MAIN_WORKTREE:-/tmp/datascript-ocaml-main-rss}"
mkdir -p "$DATA_DIR" "$(dirname "$OUT")"

RAW="$(mktemp)"
trap 'rm -f "$RAW"' EXIT

echo "=== storage RSS compare (size=${SIZE}) ===" | tee "$OUT"
echo "branch=$(git rev-parse --abbrev-ref HEAD) tip=$(git rev-parse --short HEAD)" | tee -a "$OUT"
echo | tee -a "$OUT"

echo "--- PR branch backends (one process each): ${BACKENDS} ---" | tee -a "$OUT"
dune build bench/storage_rss_bench.exe
IFS=',' read -r -a backend_arr <<< "$BACKENDS"
for backend in "${backend_arr[@]}"; do
  backend="$(echo "$backend" | xargs)"
  [[ -z "$backend" ]] && continue
  echo | tee -a "$OUT"
  echo ">>> backend=${backend}" | tee -a "$OUT"
  dune exec bench/storage_rss_bench.exe -- \
    --size "$SIZE" \
    --tx-size "$TX_SIZE" \
    --data-dir "$DATA_DIR" \
    --backends "$backend" \
    | tee -a "$RAW" | tee -a "$OUT"
done

echo | tee -a "$OUT"
echo "--- main memory backend ---" | tee -a "$OUT"

if [[ ! -d "$MAIN_WORKTREE/.git" && ! -f "$MAIN_WORKTREE/.git" ]]; then
  rm -rf "$MAIN_WORKTREE"
  git fetch origin main
  git worktree add --detach "$MAIN_WORKTREE" origin/main
fi

# Memory-only probe for main (same light people schema / phases as storage_rss_bench).
cat > "$MAIN_WORKTREE/bench/memory_rss_probe.ml" <<'ML'
open Datascript

let rss_bytes () =
  let channel = Unix.open_process_in (Printf.sprintf "ps -o rss= -p %d" (Unix.getpid ())) in
  let line = try input_line channel with End_of_file -> "0" in
  ignore (Unix.close_process_in channel);
  line |> String.trim |> int_of_string |> fun kb -> kb * 1024

let heap_bytes () =
  let stat = Gc.stat () in
  stat.live_words * (Sys.word_size / 8)

let settle () = Gc.full_major (); Unix.sleepf 0.05

let report phase =
  settle ();
  Printf.printf "backend\tmemory\n%!";
  Printf.printf "phase\t%s\n%!" phase;
  Printf.printf "rss-bytes\t%d\n%!" (rss_bytes ());
  Printf.printf "heap-bytes\t%d\n%!" (heap_bytes ())

let indexed =
  { cardinality = One; unique = None; indexed = true; is_component = false
  ; no_history = false; doc = None; value_type = None; tuple_attrs = None; tuple_types = None }
let schema =
  [ "name", indexed; "last-name", indexed; "sex", indexed; "age", indexed; "salary", indexed ]
let names = [| "Ivan"; "Petr"; "Sergei"; "Oleg"; "Yuri"; "Dmitry"; "Fedor"; "Denis" |]
let last_names = [| "Ivanov"; "Petrov"; "Sidorov"; "Kovalev"; "Kuznetsov"; "Voronoi" |]
let sexes = [| "male"; "female" |]
type rng = { mutable state : int32 }
let rng seed = { state = Int32.of_int seed }
let next_int rng bound =
  rng.state <- Int32.add (Int32.mul rng.state 1_664_525l) 1_013_904_223l;
  Int32.(to_int (rem (logand (shift_right_logical rng.state 1) 0x3fffffffl) (of_int bound)))
let rand_nth rng values = values.(next_int rng (Array.length values))
let rand_sex rng = sexes.(next_int rng 997 mod Array.length sexes)
let person rng i =
  Entity
    { db_id = Some (Temp_id (string_of_int i))
    ; attrs =
        [ "name", One_value (String (rand_nth rng names))
        ; "last-name", One_value (String (rand_nth rng last_names))
        ; "sex", One_value (Keyword (rand_sex rng))
        ; "age", One_value (Int (next_int rng 100))
        ; "salary", One_value (Int (next_int rng 100_000))
        ]
    }
let chunk = 10000
let build_db size =
  let r = rng 1 in
  let rec loop i db =
    if i > size then db
    else
      let hi = min size (i + chunk - 1) in
      let tx = List.init (hi - i + 1) (fun k -> person r (i + k)) in
      Printf.eprintf "built\t%d/%d\trss=%d\n%!" hi size (rss_bytes ());
      loop (hi + 1) (db_with tx db)
  in
  loop 1 (empty_db ~schema ())
let update_person rng i =
  Entity
    { db_id = Some (Entity_id (i + 1))
    ; attrs =
        [ "age", One_value (Int (next_int rng 100))
        ; "salary", One_value (Int (next_int rng 100_000))
        ]
    }
let blackhole = ref 0
let consume n = blackhole := (!blackhole + n) land 0x3fffffff
let q_name = lazy (parse_query_string "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a]]")
let q_sal = lazy (parse_query_string "[:find ?e ?s :where [?e :salary ?s] [(> ?s 50000)]]")
let q_sex = lazy (parse_query_string "[:find ?e ?a :where [?e :name \"Ivan\"] [?e :age ?a] [?e :sex :male]]")
let run_queries db =
  consume (Seq.fold_left (fun n _ -> n + 1) 0 (datoms db Aevt ~a:"name" ()));
  consume (List.length (q db (Lazy.force q_name)));
  consume (List.length (q db (Lazy.force q_sal)));
  consume (List.length (q db (Lazy.force q_sex)));
  for entity_id = 1 to 100 do
    match pull db [ Pull_attr "name"; Pull_attr "age"; Pull_attr "salary" ] (Entity_id entity_id) with
    | None -> consume 0
    | Some e -> consume (List.length e.pulled_attrs)
  done
let size = try int_of_string Sys.argv.(1) with _ -> 50000
let tx_size = try int_of_string Sys.argv.(2) with _ -> 200
let () =
  Printf.printf "runtime\tOCaml\n%!";
  Printf.printf "size\t%d\n%!" size;
  Printf.printf "tx-size\t%d\n%!" tx_size;
  Printf.printf "bench\tstorage-rss\n%!";
  Printf.printf "label\tmain\n%!";
  Printf.printf "storage\tmemory\n%!";
  report "baseline";
  let t0 = Unix.gettimeofday () in
  let db = ref (build_db size) in
  Printf.printf "build-ms\t%.1f\n%!" ((Unix.gettimeofday () -. t0) *. 1000.);
  Printf.printf "disk-bytes\t0\n%!";
  report "after-build";
  run_queries !db;
  report "after-queries";
  let r = rng 99 in
  db := db_with (List.init tx_size (fun i -> update_person r i)) !db;
  report "after-tx";
  run_queries !db;
  report "after-queries-2";
  report "after-gc-full-major";
  Gc.compact (); Unix.sleepf 0.05;
  report "after-gc-compact";
  db := empty_db ();
  settle (); Gc.compact (); Unix.sleepf 0.1;
  report "after-drop-db";
  settle (); Gc.compact (); Unix.sleepf 0.1;
  report "after-close";
  Printf.printf "blackhole\t%d\n%!" !blackhole
ML

# Ensure dune stanza exists for the probe on main.
if ! grep -q 'memory_rss_probe' "$MAIN_WORKTREE/bench/dune"; then
  cat >> "$MAIN_WORKTREE/bench/dune" <<'DUNE'

(executable
 (name memory_rss_probe)
 (modules memory_rss_probe)
 (modes exe)
 (libraries datascript-ocaml-native unix))
DUNE
fi

(
  cd "$MAIN_WORKTREE"
  if command -v opam >/dev/null 2>&1; then
    eval "$(opam env --switch=5.5 2>/dev/null || opam env 2>/dev/null || true)"
  fi
  echo "main tip=$(git rev-parse --short HEAD)" | tee -a "$OUT"
  dune build bench/memory_rss_probe.exe
  dune exec bench/memory_rss_probe.exe -- "$SIZE" "$TX_SIZE" | tee -a "$RAW" | tee -a "$OUT"
)

python3 - "$RAW" <<'PY' | tee -a "$OUT"
import sys
from collections import defaultdict

by = defaultdict(dict)  # backend -> phase -> rss
order_backends = []
order_phases = []
backend = None
phase = None
meta = {}

with open(sys.argv[1]) as f:
    for line in f:
        line = line.rstrip("\n")
        if "\t" not in line:
            continue
        k, v = line.split("\t", 1)
        if k in {"runtime", "size", "tx-size", "bench", "data-dir", "label", "build-ms", "disk-bytes", "blackhole"}:
            if k == "label":
                meta["label"] = v
            continue
        if k == "storage":
            backend = v
            if meta.get("label") == "main" and v == "memory":
                backend = "main-memory"
            if backend not in order_backends:
                order_backends.append(backend)
            phase = None
            continue
        if k == "label":
            meta["label"] = v
            continue
        if k == "phase":
            phase = v
            if phase not in order_phases:
                order_phases.append(phase)
            continue
        if k == "rss-bytes" and backend and phase:
            by[backend][phase] = int(v)
        if k == "heap-bytes" and backend and phase:
            by[backend][phase + ":heap"] = int(v)

def fmt(n):
    if n is None:
        return "?"
    if n >= 1024 * 1024 * 1024:
        return f"{n / (1024**3):.2f} GiB"
    if n >= 1024 * 1024:
        return f"{n / (1024**2):.1f} MiB"
    if n >= 1024:
        return f"{n / 1024:.1f} KiB"
    return f"{n} B"

print()
print("=== RSS by phase (process resident) ===")
header = f"{'phase':<22}" + "".join(f"{b:>14}" for b in order_backends)
print(header)
print("-" * len(header))
for phase in order_phases:
    row = f"{phase:<22}"
    for b in order_backends:
        row += f"{fmt(by[b].get(phase)):>14}"
    print(row)

print()
print("=== release deltas (after-queries-2 → later) ===")
for b in order_backends:
    base = by[b].get("after-queries-2")
    if base is None:
        continue
    print(f"[{b}] after-queries-2 = {fmt(base)}")
    for phase in ("after-gc-full-major", "after-gc-compact", "after-drop-db", "after-close"):
        cur = by[b].get(phase)
        if cur is None:
            continue
        delta = cur - base
        sign = "+" if delta >= 0 else ""
        print(f"  {phase:<22} {fmt(cur):>10}  ({sign}{fmt(abs(delta))})")
PY

echo
echo "wrote $OUT"
