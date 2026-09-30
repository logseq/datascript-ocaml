#!/usr/bin/env bash
# Compare LMDB vs SQLite on the full shared query suite.
# STORAGE=compare uses on-disk LMDB and SQLite files only (no in-memory).
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

if command -v opam >/dev/null 2>&1; then
  eval "$(opam env --switch=5.5 2>/dev/null || opam env 2>/dev/null || true)"
fi

SIZE="${SIZE:-20000}"
WARMUP_MS="${WARMUP_MS:-200}"
SAMPLE_MS="${SAMPLE_MS:-200}"
REPEATS="${REPEATS:-2}"
JIT_WARMUP="${JIT_WARMUP:-100}"
STORAGE="${STORAGE:-compare}" # compare => lmdb + sqlite on disk
DATA_DIR="${DATA_DIR:-$repo_root/_bench_data/queries}"
mkdir -p "$DATA_DIR"

echo "=== LMDB vs SQLite shared query suite (disk-backed) ==="
echo "size=${SIZE} warmup=${WARMUP_MS}ms sample=${SAMPLE_MS}ms repeats=${REPEATS} jit=${JIT_WARMUP}"
echo "storage=${STORAGE}"
echo "data-dir=${DATA_DIR}"
echo

RAW="$(mktemp)"
trap 'rm -f "$RAW"' EXIT

dune build bench/shared_query_bench.exe
dune exec bench/shared_query_bench.exe -- \
  --size "$SIZE" \
  --warmup-ms "$WARMUP_MS" \
  --sample-ms "$SAMPLE_MS" \
  --repeats "$REPEATS" \
  --jit-warmup "$JIT_WARMUP" \
  --storage "$STORAGE" \
  --data-dir "$DATA_DIR" \
  | tee "$RAW"

python3 - "$RAW" <<'PY'
import sys
from collections import defaultdict

path = sys.argv[1]
current = None
by_storage = defaultdict(dict)
meta = {}
order = []

with open(path) as f:
    for line in f:
        line = line.rstrip("\n")
        if not line or "\t" not in line:
            continue
        k, v = line.split("\t", 1)
        if k in {"runtime", "size", "warmup-ms", "sample-ms", "repeats", "jit-warmup", "db-mode", "query-cases", "query", "data-dir"}:
            meta[k] = v
            continue
        if k == "storage":
            current = v
            if v not in order:
                order.append(v)
            continue
        if current is None:
            continue
        by_storage[current][k] = v

setup = ["path", "disk-bytes", "build-ms", "store-restore-ms"]
queries = [
    "q1", "q2", "q2-switch", "q3", "q4", "q5",
    "qpred1", "qpred2", "q-or", "q-not", "q-or-join", "q-not-join",
    "q-pred-range", "q-5-merge", "q-rule",
]

def ratio(a, b):
    try:
        fa, fb = float(a), float(b)
        if fa == 0:
            return "?"
        return f"{fb / fa:.2f}x"
    except Exception:
        return "?"

print()
print(f"data-dir\t{meta.get('data-dir', '?')}")
print(f"=== comparison (ms; ratio = second/first; storages={','.join(order)}) ===")
if len(order) < 2:
    print("Need at least two storages for a ratio table.")
    for s in order:
        print(f"\n[{s}]")
        for k in setup + queries:
            if k in by_storage[s]:
                print(f"  {k}\t{by_storage[s][k]}")
    raise SystemExit(0)

left, right = order[0], order[1]
print(f"{'metric':<22} {left:>12} {right:>12} {'ratio':>10}")
print("-" * 60)
for metric in setup + queries:
    lv = by_storage[left].get(metric, "?")
    rv = by_storage[right].get(metric, "?")
    if metric in {"path"}:
        print(f"{metric:<22} {lv:>12} {rv:>12} {'':>10}")
    else:
        print(f"{metric:<22} {lv:>12} {rv:>12} {ratio(lv, rv):>10}")
print()
print(f"query-cases\t{meta.get('query-cases', '?')}")
print(f"size\t{meta.get('size', '?')}")
PY
