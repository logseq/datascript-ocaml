#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"

output="$(
  BENCH_SKIP_BUILD="${BENCH_SKIP_BUILD:-1}" \
  UPSTREAM_DATASCRIPT_JS="${UPSTREAM_DATASCRIPT_JS:-$repo_root/_deps/datascript/release-js/datascript.js}" \
  "$repo_root/script/benchmark_vs_cljs.sh"
)"

printf '%s\n' "$output"

printf '%s\n' "$output" | node "$repo_root/script/benchmark_gate_vs_cljs_check.js"
