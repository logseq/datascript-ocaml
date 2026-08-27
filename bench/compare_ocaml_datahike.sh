#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: compare_ocaml_datahike.sh [SIZE] [QUERY]

Run OCaml vs Datahike shared query benchmarks.

  SIZE    entity count (default: 2000)
  QUERY   optional single query name, e.g. q3, qpred1, q-rule

Environment:
  BENCH_QUERY      same as QUERY positional arg
  BENCH_WARMUP_MS  warmup duration per benchmark (default: 200, 2000 when FULL=1)
  BENCH_SAMPLE_MS  sample duration per benchmark (default: 200, 2000 when FULL=1)
  BENCH_REPEATS    median sample count (default: 2)
  BENCH_JIT_WARMUP JIT iterations per query before timing (default: 100)
  FULL=1           use publication timing (2000ms warmup/sample)

Examples:
  ./compare_ocaml_datahike.sh 2000 q3
  BENCH_QUERY=qpred1 ./compare_ocaml_datahike.sh
  dune exec --release bench/datahike_compare.exe -- --size 2000 --query q3 --list-queries
EOF
}

SIZE="${1:-2000}"
QUERY="${BENCH_QUERY:-}"

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

if [[ -n "${2:-}" ]]; then
  QUERY="$2"
fi

if [[ "$SIZE" == "--help" || "$SIZE" == "-h" ]]; then
  usage
  exit 0
fi

if [[ "${FULL:-0}" == "1" ]]; then
  WARMUP_MS="${WARMUP_MS:-2000}"
  SAMPLE_MS="${SAMPLE_MS:-2000}"
  REPEATS="${REPEATS:-2}"
  JIT_WARMUP="${JIT_WARMUP:-100}"
else
  WARMUP_MS="${WARMUP_MS:-200}"
  SAMPLE_MS="${SAMPLE_MS:-200}"
  REPEATS="${REPEATS:-2}"
  JIT_WARMUP="${JIT_WARMUP:-100}"
fi

export BENCH_SIZE="$SIZE"
export BENCH_WARMUP_MS="$WARMUP_MS"
export BENCH_SAMPLE_MS="$SAMPLE_MS"
export BENCH_REPEATS="$REPEATS"
export BENCH_JIT_WARMUP="$JIT_WARMUP"
if [[ -n "$QUERY" ]]; then
  export BENCH_QUERY="$QUERY"
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DATAHIKE_REPO="${DATAHIKE_REPO:-/tmp/bench-datahike}"
OCAML_BENCH="${REPO_ROOT}/_build/default/bench/datahike_compare.exe"

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
  local ocaml_args=(
    --size "$SIZE"
    --warmup-ms "$WARMUP_MS"
    --sample-ms "$SAMPLE_MS"
    --repeats "$REPEATS"
    --jit-warmup "$JIT_WARMUP"
  )
  if [[ -n "$QUERY" ]]; then
    ocaml_args+=(--query "$QUERY")
  fi
  (
    cd "$REPO_ROOT"
    dune build --profile release bench/datahike_compare.exe >/dev/null
    BENCH_RUNTIME_LABEL=ocaml "$OCAML_BENCH" "${ocaml_args[@]}" 2>/dev/null
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

if [[ -n "$QUERY" ]]; then
  echo "=== OCaml vs Datahike query benchmark (${SIZE} entities, query=${QUERY}) ==="
else
  echo "=== OCaml vs Datahike query benchmark (${SIZE} entities) ==="
fi
if [[ "${FULL:-0}" == "1" ]]; then
  echo "Protocol: FULL warmup=${WARMUP_MS}ms sample=${SAMPLE_MS}ms repeats=${REPEATS} jit=${JIT_WARMUP} (set FULL=1)"
else
  echo "Protocol: fast warmup=${WARMUP_MS}ms sample=${SAMPLE_MS}ms repeats=${REPEATS} jit=${JIT_WARMUP} (use FULL=1 for publication timing)"
fi
echo "Storage:  datahike=memory+persistent-set  ocaml=memory LMDB index (nosync, see storage row in raw output)"
echo

START=$(date +%s)
echo "Running Datahike (JVM cold start may take ~30-60s)..."
DH_OUT="$(run_datahike)"
DH_SEC=$(( $(date +%s) - START ))
echo "Running OCaml (${DH_SEC}s for Datahike side)..."
OCAML_START=$(date +%s)
OCAML_OUT="$(run_ocaml)"
OCAML_SEC=$(( $(date +%s) - OCAML_START ))
TOTAL_SEC=$(( $(date +%s) - START ))

QUERY_ORDER=(
  q1 q2 q2-switch q3 q4 q5 qpred1 qpred2
  q-or q-not q-or-join q-not-join q-pred-range q-5-merge q-rule
)

if [[ -n "$QUERY" ]]; then
  QUERY_ORDER=("$QUERY")
fi

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
echo "Timing: datahike=${DH_SEC}s ocaml=${OCAML_SEC}s total=${TOTAL_SEC}s"
echo
echo "=== raw: datahike ==="
printf '%s\n' "$DH_OUT" | awk '/^(q|runtime|size|warmup|sample|repeats|jit|Setting|Query planner|Done)/ || /^[[:space:]]*q/ || /^Benchmark/ || /^---/ { print }'
echo
echo "=== raw: ocaml ==="
printf '%s\n' "$OCAML_OUT"
