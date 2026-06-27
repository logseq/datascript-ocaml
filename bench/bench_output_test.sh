#!/usr/bin/env bash
set -euo pipefail

bench_exe="$1"
if [[ "$bench_exe" != */* ]]; then
  bench_exe="./$bench_exe"
fi
output="$("$bench_exe" --size 20 --warmup-ms 1 --sample-ms 1 --samples 1 2>/dev/null)"

for name in add-1 add-5 add-all q1 q2 q3 q4 q5-shortcircuit qpred1 qpred2 pull-one; do
  if ! grep -q "^${name}[[:space:]]" <<<"$output"; then
    echo "missing benchmark: $name" >&2
    echo "$output" >&2
    exit 1
  fi
done
