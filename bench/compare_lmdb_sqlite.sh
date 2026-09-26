#!/usr/bin/env bash
# Compare LMDB vs SQLite persistent storage benchmarks side-by-side.
# Both backends write durable files under DATA_DIR (default: repo _bench_data).
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

if command -v opam >/dev/null 2>&1; then
  eval "$(opam env --switch=5.5 2>/dev/null || opam env 2>/dev/null || true)"
fi

SIZES="${SIZES:-100,1000,5000}"
RAW_ONLY="${RAW_ONLY:-0}"
INPUT_FILE="${INPUT_FILE:-}"
DATA_DIR="${DATA_DIR:-$repo_root/_bench_data/persistent}"
mkdir -p "$DATA_DIR"

echo "=== LMDB vs SQLite persistent storage bench (disk only) ==="
echo "sizes=${SIZES}"
echo "data-dir=${DATA_DIR}"
echo

RAW="$(mktemp)"
trap 'rm -f "$RAW"' EXIT

if [[ -n "$INPUT_FILE" ]]; then
  cat "$INPUT_FILE" > "$RAW"
elif [[ "$RAW_ONLY" == "1" ]]; then
  cat > "$RAW"
else
  dune build bench/persistent_storage_bench.exe
  dune exec bench/persistent_storage_bench.exe -- \
    --disk-only \
    --data-dir "$DATA_DIR" \
    --sizes "$SIZES" | tee "$RAW"
fi

python3 - "$RAW" <<'PY'
import sys
from collections import defaultdict

path = sys.argv[1]
sqlite = defaultdict(dict)
lmdb = defaultdict(dict)
sizes = []
size = None
meta = {}

with open(path) as f:
    for line in f:
        line = line.rstrip("\n")
        if not line or "\t" not in line:
            continue
        key, val = line.split("\t", 1)
        if key in {"data-dir", "disk-only"}:
            meta[key] = val
            continue
        if key == "size":
            size = int(val)
            sizes.append(size)
            continue
        if size is None:
            continue
        if key.startswith("sqlite-"):
            sqlite[size][key[len("sqlite-"):]] = val
        elif key.startswith("lmdb-"):
            lmdb[size][key[len("lmdb-"):]] = val

timing_metrics = [
    "snapshot-build-and-store",
    "snapshot-restore",
    "snapshot-add-one-and-store-after-restore",
    "snapshot-update-one-and-store-after-add",
    "conn-build",
    "conn-restore",
    "conn-add-one-after-restore",
    "conn-update-one-after-add",
]
size_metrics = [
    "snapshot-file-size-after-build",
    "snapshot-file-size-after-update",
    "conn-file-size-after-build",
    "conn-file-size-after-update",
]

def ratio(sv, lv):
    try:
        s = float(sv)
        l = float(lv)
    except Exception:
        return "?"
    if s == 0:
        return "?"
    return f"{l / s:.2f}x"

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

print(f"data-dir\t{meta.get('data-dir', '?')}")
print(f"disk-only\t{meta.get('disk-only', '?')}")
print()
print("=== timing (ms; ratio = lmdb/sqlite; <1x means LMDB faster) ===")
print(f"{'size':<8} {'metric':<42} {'sqlite':>12} {'lmdb':>12} {'ratio':>10}")
print("-" * 88)
for s in sizes:
    for metric in timing_metrics:
        sv = sqlite[s].get(metric, "?")
        lv = lmdb[s].get(metric, "?")
        print(f"{s:<8} {metric:<42} {sv:>12} {lv:>12} {ratio(sv, lv):>10}")
    print()

print("=== on-disk footprint (ratio = lmdb/sqlite; <1x means LMDB smaller) ===")
print(f"{'size':<8} {'metric':<42} {'sqlite':>14} {'lmdb':>14} {'ratio':>10}")
print("-" * 92)
for s in sizes:
    for metric in size_metrics:
        sv = sqlite[s].get(metric, "?")
        lv = lmdb[s].get(metric, "?")
        print(
            f"{s:<8} {metric:<42} {fmt_bytes(sv):>14} {fmt_bytes(lv):>14} {ratio(sv, lv):>10}"
        )
    print()
PY
