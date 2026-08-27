#!/usr/bin/env bash
# Compare LMDB vs SQLite on narrow Index scan microbenchmarks (disk-backed).
# Measures cold open/restore + cold/hot point/prefix/range/full scans — not the
# shared query evaluator.
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

if command -v opam >/dev/null 2>&1; then
  eval "$(opam env --switch=5.5 2>/dev/null || opam env 2>/dev/null || true)"
fi

SIZE="${SIZE:-50000}"
WARMUP="${WARMUP:-20}"
REPEATS="${REPEATS:-5}"
DATA_DIR="${DATA_DIR:-$repo_root/_bench_data/index-scan}"
DROP_CACHES="${DROP_CACHES:-0}"
BACKENDS="${BACKENDS:-lmdb,sqlite}"
mkdir -p "$DATA_DIR"

echo "=== LMDB vs SQLite index-scan microbench (disk) ==="
echo "size=${SIZE} warmup=${WARMUP} repeats=${REPEATS}"
echo "data-dir=${DATA_DIR} backends=${BACKENDS} drop-caches=${DROP_CACHES}"
echo

RAW="$(mktemp)"
trap 'rm -f "$RAW"' EXIT

EXTRA=()
if [[ "$DROP_CACHES" == "1" ]]; then
  EXTRA+=(--drop-caches)
fi

dune build bench/index_scan_bench.exe
dune exec bench/index_scan_bench.exe -- \
  --size "$SIZE" \
  --data-dir "$DATA_DIR" \
  --warmup "$WARMUP" \
  --repeats "$REPEATS" \
  --backends "$BACKENDS" \
  "${EXTRA[@]}" \
  | tee "$RAW"

python3 - "$RAW" <<'PY'
import sys
from collections import defaultdict

path = sys.argv[1]
current = None
by = defaultdict(dict)
meta = {}
order = []

with open(path) as f:
    for line in f:
        line = line.rstrip("\n")
        if not line or "\t" not in line:
            continue
        k, v = line.split("\t", 1)
        if k in {"runtime", "size", "data-dir", "warmup", "repeats", "bench", "drop-caches"}:
            meta[k] = v
            continue
        if k == "storage":
            current = v
            if v not in order:
                order.append(v)
            continue
        if current is None:
            continue
        by[current][k] = v

metrics = [
    "disk-bytes",
    "build-ms",
    "cold-open-restore-ms",
    "cold-point-eavt-entity-ms",
    "hot-point-eavt-entity-ms",
    "cold-prefix-aevt-name-ms",
    "hot-prefix-aevt-name-ms",
    "cold-exact-avet-name-ivan-ms",
    "hot-exact-avet-name-ivan-ms",
    "cold-range-avet-salary-50k-60k-ms",
    "hot-range-avet-salary-50k-60k-ms",
    "cold-seek-eavt-mid-take-100-ms",
    "hot-seek-eavt-mid-take-100-ms",
    "cold-scan-eavt-all-ms",
    "hot-scan-eavt-all-ms",
]

def ratio(a, b):
    try:
        fa, fb = float(a), float(b)
        if fa == 0:
            return "?"
        return f"{fb / fa:.2f}x"
    except Exception:
        return "?"

def fmt_bytes(n):
    try:
        n = int(n)
    except Exception:
        return n
    if n >= 1024 * 1024:
        return f"{n / (1024 * 1024):.2f} MiB"
    if n >= 1024:
        return f"{n / 1024:.1f} KiB"
    return f"{n} B"

print()
print(f"size\t{meta.get('size', '?')}")
print(f"data-dir\t{meta.get('data-dir', '?')}")
print(f"drop-caches\t{meta.get('drop-caches', '?')}")
if len(order) < 2:
    for s in order:
        print(f"\n[{s}]")
        for m in metrics:
            if m in by[s]:
                val = by[s][m]
                if m == "disk-bytes":
                    val = fmt_bytes(val)
                print(f"  {m}\t{val}")
    raise SystemExit(0)

left, right = order[0], order[1]
print(f"=== comparison (ratio = {right}/{left}) ===")
print(f"{'metric':<36} {left:>14} {right:>14} {'ratio':>10}")
print("-" * 78)
for m in metrics:
    lv = by[left].get(m, "?")
    rv = by[right].get(m, "?")
    if m == "disk-bytes":
        print(f"{m:<36} {fmt_bytes(lv):>14} {fmt_bytes(rv):>14} {ratio(lv, rv):>10}")
    else:
        print(f"{m:<36} {lv:>14} {rv:>14} {ratio(lv, rv):>10}")
PY
