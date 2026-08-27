#!/usr/bin/env bash
# Compare LMDB vs SQLite persistent storage benchmarks side-by-side.
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

if command -v opam >/dev/null 2>&1; then
  eval "$(opam env --switch=5.5 2>/dev/null || opam env 2>/dev/null || true)"
fi

SIZES="${SIZES:-100,1000,5000}"
RAW_ONLY="${RAW_ONLY:-0}"
INPUT_FILE="${INPUT_FILE:-}"

echo "=== LMDB vs SQLite persistent storage bench ==="
echo "sizes=${SIZES}"
echo

RAW="$(mktemp)"
trap 'rm -f "$RAW"' EXIT

if [[ -n "$INPUT_FILE" ]]; then
  cat "$INPUT_FILE" > "$RAW"
elif [[ "$RAW_ONLY" == "1" ]]; then
  cat > "$RAW"
else
  dune build bench/persistent_storage_bench.exe
  dune exec bench/persistent_storage_bench.exe -- --sizes "$SIZES" | tee "$RAW"
fi

python3 - "$RAW" <<'PY'
import sys
from collections import defaultdict

path = sys.argv[1]
sqlite = defaultdict(dict)
lmdb = defaultdict(dict)
sizes = []
size = None

with open(path) as f:
    for line in f:
        line = line.rstrip("\n")
        if not line or "\t" not in line:
            continue
        key, val = line.split("\t", 1)
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
        if s == 0:
            return "?"
        return f"{l / s:.2f}x"
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
print("=== timing (ms; ratio = lmdb/sqlite; <1x means LMDB faster) ===")
print(f"{'size':<8} {'metric':<42} {'sqlite':>12} {'lmdb':>12} {'ratio':>10}")
print("-" * 88)
for s in sizes:
    for metric in timing_metrics:
        sv = sqlite[s].get(metric, "?")
        lv = lmdb[s].get(metric, "?")
        print(f"{s:<8} {metric:<42} {sv:>12} {lv:>12} {ratio(sv, lv):>10}")
    print()

print("=== file size (ratio = lmdb/sqlite; <1x means LMDB smaller) ===")
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
