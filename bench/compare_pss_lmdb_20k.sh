#!/usr/bin/env bash
set -euo pipefail

SIZE="${1:-20000}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PSS_REPO="${PSS_REPO:-/tmp/bench-pss-main}"

run_branch() {
  local label="$1"
  local repo="$2"
  (
    cd "$repo"
    dune build bench/index_compare_20k.exe >/dev/null
    BENCH_RUNTIME_LABEL="$label" dune exec bench/index_compare_20k.exe -- "$SIZE" 2>/dev/null
  )
}

if [[ ! -e "$PSS_REPO/.git" ]]; then
  echo "PSS worktree missing at $PSS_REPO; run: git worktree add $PSS_REPO main" >&2
  exit 1
fi

echo "=== PSS vs LMDB index benchmark (${SIZE} entities / ~${SIZE}0 datoms) ==="
echo

PSS_OUT="$(run_branch pss "$PSS_REPO")"
LMDB_OUT="$(run_branch lmdb "$REPO_ROOT")"

printf "%-24s %12s %12s %12s\n" "benchmark" "pss(ms)" "lmdb(ms)" "lmdb/pss"
echo "------------------------------------------------------------------------"

while IFS=$'\t' read -r name pss_ms; do
  [[ "$name" == runtime* || "$name" == size* || "$name" == datoms || "$name" == *count* || -z "$name" ]] && continue
  lmdb_ms="$(printf '%s\n' "$LMDB_OUT" | awk -F'\t' -v n="$name" '$1 == n { print $2; exit }')"
  if [[ -z "$lmdb_ms" ]]; then
    printf "%-24s %12s %12s %12s\n" "$name" "$pss_ms" "?" "?"
    continue
  fi
  ratio="$(awk -v l="$lmdb_ms" -v p="$pss_ms" 'BEGIN { if (p + 0 == 0) print "?"; else printf "%.2fx", l / p }')"
  printf "%-24s %12s %12s %12s\n" "$name" "$pss_ms" "$lmdb_ms" "$ratio"
done <<< "$PSS_OUT"

echo
echo "=== raw: pss ==="
printf '%s\n' "$PSS_OUT"
echo
echo "=== raw: lmdb ==="
printf '%s\n' "$LMDB_OUT"
