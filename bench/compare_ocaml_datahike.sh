#!/usr/bin/env bash
set -euo pipefail

SIZE="${1:-20000}"
WARMUP_MS="${WARMUP_MS:-2000}"
SAMPLE_MS="${SAMPLE_MS:-2000}"
REPEATS="${REPEATS:-5}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DATAHIKE_REPO="${DATAHIKE_REPO:-/tmp/bench-datahike}"

ensure_datahike_java() {
  if [[ ! -e "$DATAHIKE_REPO/deps.edn" ]]; then
    git clone --depth 1 https://github.com/replikativ/datahike.git "$DATAHIKE_REPO"
  fi
  (
    cd "$DATAHIKE_REPO"
    mkdir -p target/classes
    local cp
    cp="$(clojure -Spath -M:bench)"
    if [[ ! -f target/classes/datahike/java/QueryResult.class ]]; then
      javac -cp "$cp:target/classes" -d target/classes \
        java/src/datahike/java/IEntity.java \
        java/src/datahike/java/Util.java \
        java/src/datahike/java/QueryResult.java
    fi
  )
}

run_datahike() {
  (
    cd "$DATAHIKE_REPO"
    DATAHIKE_QUERY_PLANNER=true clojure -M:bench -e \
      "(load-file \"${REPO_ROOT}/bench/datahike_shared_bench.clj\")" \
      2>/dev/null
  )
}

run_ocaml() {
  (
    cd "$REPO_ROOT"
    dune build --profile release bench/datahike_compare.exe >/dev/null
    BENCH_RUNTIME_LABEL=ocaml dune exec bench/datahike_compare.exe -- \
      --size "$SIZE" --warmup-ms "$WARMUP_MS" --sample-ms "$SAMPLE_MS" --repeats "$REPEATS" 2>/dev/null
  )
}

parse_dh_row() {
  local name="$1"
  awk -v n="$name" '$1 == n { print $2; exit }'
}

parse_ocaml_row() {
  local name="$1"
  awk -F'\t' -v n="$name" '$1 == n { print $2; exit }'
}

ratio_cell() {
  awk -v o="$1" -v d="$2" 'BEGIN {
    if (o + 0 == 0 || d + 0 == 0) print "?";
    else printf "%.2fx", o / d
  }'
}

ensure_datahike_java

echo "=== OCaml vs Datahike query benchmark (${SIZE} entities) ==="
echo "Protocol: warmup=${WARMUP_MS}ms sample=${SAMPLE_MS}ms repeats=${REPEATS}, shared-db (both sides)"
echo "Storage:  datahike=memory+persistent-set  ocaml=memory LMDB index (nosync, see storage row in raw output)"
echo

echo "Running Datahike..."
DH_OUT="$(run_datahike)"
echo "Running OCaml..."
OCAML_OUT="$(run_ocaml)"

QUERY_ORDER=(
  q1 q2 q2-switch q3 q4 q5 qpred1 qpred2
  q-or q-not q-or-join q-not-join q-pred-range q-5-merge q-rule
)

printf "%-14s %12s %12s %12s\n" "benchmark" "datahike(ms)" "ocaml(ms)" "ocaml/dh"
echo "------------------------------------------------------------"

for name in "${QUERY_ORDER[@]}"; do
  dh_ms="$(printf '%s\n' "$DH_OUT" | parse_dh_row "$name")"
  ocaml_ms="$(printf '%s\n' "$OCAML_OUT" | parse_ocaml_row "$name")"
  if [[ -z "$dh_ms" || -z "$ocaml_ms" ]]; then
    printf "%-14s %12s %12s %12s\n" "$name" "${dh_ms:-?}" "${ocaml_ms:-?}" "?"
    continue
  fi
  ratio="$(ratio_cell "$ocaml_ms" "$dh_ms")"
  printf "%-14s %12s %12s %12s\n" "$name" "$dh_ms" "$ocaml_ms" "$ratio"
done

echo
echo "=== raw: datahike ==="
printf '%s\n' "$DH_OUT" | awk '/^(q|Setting|Query planner|Done)/ || /^[[:space:]]*q/ || /^Benchmark/ || /^---/ { print }'
echo
echo "=== raw: ocaml ==="
printf '%s\n' "$OCAML_OUT"
