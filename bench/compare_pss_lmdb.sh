#!/usr/bin/env bash
set -euo pipefail

SIZE="${1:-20000}"
WARMUP_MS="${2:-100}"
SAMPLE_MS="${3:-200}"
SAMPLES="${4:-3}"

bench_args=(--size "$SIZE" --warmup-ms "$WARMUP_MS" --sample-ms "$SAMPLE_MS" --samples "$SAMPLES")

run_branch_bench() {
  local label="$1"
  local repo="$2"
  (
    cd "$repo"
    dune build bench/bench_ocaml.exe >/dev/null
    BENCH_RUNTIME_LABEL="$label" dune exec bench/bench_ocaml.exe -- "${bench_args[@]}" 2>/dev/null
  )
}

echo "=== PSS vs LMDB benchmark (${SIZE} entities) ==="
echo "warmup=${WARMUP_MS}ms sample=${SAMPLE_MS}ms samples=${SAMPLES}"
echo

PSS_OUT="$(run_branch_bench pss /tmp/bench-pss-main)"
LMDB_OUT="$(run_branch_bench lmdb /workspace)"

printf "%-22s %12s %12s %12s\n" "benchmark" "pss(ms)" "lmdb(ms)" "lmdb/pss"
echo "----------------------------------------------------------------"

while IFS=$'\t' read -r name pss_ms; do
  [[ "$name" == runtime* || "$name" == size* || -z "$name" ]] && continue
  lmdb_ms="$(printf '%s\n' "$LMDB_OUT" | awk -F'\t' -v n="$name" '$1 == n { print $2; exit }')"
  if [[ -z "$lmdb_ms" ]]; then
    printf "%-22s %12s %12s %12s\n" "$name" "$pss_ms" "?" "?"
    continue
  fi
  ratio="$(awk -v l="$lmdb_ms" -v p="$pss_ms" 'BEGIN { if (p + 0 == 0) print "?"; else printf "%.2fx", l / p }')"
  printf "%-22s %12s %12s %12s\n" "$name" "$pss_ms" "$lmdb_ms" "$ratio"
done <<< "$PSS_OUT"

echo
echo "=== raw: pss ==="
printf '%s\n' "$PSS_OUT"
echo
echo "=== raw: lmdb ==="
printf '%s\n' "$LMDB_OUT"
