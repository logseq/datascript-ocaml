#!/usr/bin/env bash
# Compare LMDB vs SQLite on narrow Index scan microbenchmarks (disk-backed).
# Measures cold open/restore + cold/hot point/prefix/range/full scans — not the
# shared query evaluator.
#
# Default sizes: 200k and 500k (large-index stress without a full 1M build).
#   SIZES=50000 bash bench/compare_lmdb_sqlite_index_scan.sh
#   SIZES=200000,500000 WARMUP=5 REPEATS=3 bash bench/compare_lmdb_sqlite_index_scan.sh
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

if command -v opam >/dev/null 2>&1; then
  eval "$(opam env --switch=5.5 2>/dev/null || opam env 2>/dev/null || true)"
fi

# Prefer SIZES; SIZE remains as a single-size override for convenience.
if [[ -n "${SIZE:-}" && -z "${SIZES:-}" ]]; then
  SIZES="$SIZE"
fi
SIZES="${SIZES:-200000,500000}"
# Defaults tuned for multi-hundred-k sizes; override upward for tighter medians.
WARMUP="${WARMUP:-5}"
REPEATS="${REPEATS:-3}"
DATA_DIR="${DATA_DIR:-$repo_root/_bench_data/index-scan}"
DROP_CACHES="${DROP_CACHES:-0}"
BACKENDS="${BACKENDS:-lmdb,sqlite}"
mkdir -p "$DATA_DIR"

echo "=== LMDB vs SQLite index-scan microbench (disk) ==="
echo "sizes=${SIZES} warmup=${WARMUP} repeats=${REPEATS}"
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
  --sizes "$SIZES" \
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
current_size = None
current_storage = None
# by[size][storage][metric] = value
by = defaultdict(lambda: defaultdict(dict))
meta = {}
sizes = []
order = []

with open(path) as f:
    for line in f:
        line = line.rstrip("\n")
        if not line or "\t" not in line:
            continue
        k, v = line.split("\t", 1)
        if k in {"runtime", "data-dir", "warmup", "repeats", "bench", "drop-caches"}:
            meta[k] = v
            continue
        if k == "size":
            current_size = int(v)
            if current_size not in sizes:
                sizes.append(current_size)
            current_storage = None
            continue
        if k == "storage":
            current_storage = v
            if v not in order:
                order.append(v)
            continue
        if current_size is None or current_storage is None:
            continue
        by[current_size][current_storage][k] = v

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
    if n >= 1024 * 1024 * 1024:
        return f"{n / (1024 * 1024 * 1024):.2f} GiB"
    if n >= 1024 * 1024:
        return f"{n / (1024 * 1024):.2f} MiB"
    if n >= 1024:
        return f"{n / 1024:.1f} KiB"
    return f"{n} B"

print()
print(f"data-dir\t{meta.get('data-dir', '?')}")
print(f"drop-caches\t{meta.get('drop-caches', '?')}")
print(f"sizes\t{','.join(str(s) for s in sizes)}")

for size in sizes:
    print()
    print(f"=== size {size} (ratio = sqlite/lmdb) ===")
    if len(order) < 2:
        for s in order:
            print(f"[{s}]")
            for m in metrics:
                if m in by[size][s]:
                    val = by[size][s][m]
                    if m == "disk-bytes":
                        val = fmt_bytes(val)
                    print(f"  {m}\t{val}")
        continue
    left, right = order[0], order[1]
    print(f"{'metric':<36} {left:>14} {right:>14} {'ratio':>10}")
    print("-" * 78)
    for m in metrics:
        lv = by[size][left].get(m, "?")
        rv = by[size][right].get(m, "?")
        if m == "disk-bytes":
            print(f"{m:<36} {fmt_bytes(lv):>14} {fmt_bytes(rv):>14} {ratio(lv, rv):>10}")
        else:
            print(f"{m:<36} {lv:>14} {rv:>14} {ratio(lv, rv):>10}")
PY
