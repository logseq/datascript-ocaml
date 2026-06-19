#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
size="${BENCH_SIZE:-200}"
warmup_ms="${BENCH_WARMUP_MS:-200}"
sample_ms="${BENCH_SAMPLE_MS:-500}"
samples="${BENCH_SAMPLES:-5}"
upstream_datascript_js="${UPSTREAM_DATASCRIPT_JS:-$repo_root/_deps/datascript/release-js/datascript.js}"
ocaml_native="${BENCH_OCAML_NATIVE:-$repo_root/_build/default/bench/bench_ocaml.exe}"
ocaml_js="${BENCH_OCAML_JS:-$repo_root/_build/default/bench/bench_ocaml.bc.js}"

if [ ! -f "$upstream_datascript_js" ]; then
  echo "Upstream DataScript JS bundle not found: $upstream_datascript_js" >&2
  exit 2
fi

if [ "${BENCH_SKIP_BUILD:-0}" != "1" ]; then
  dune build --profile release bench/bench_ocaml.exe bench/bench_ocaml.bc.js
fi

args=(--size "$size" --warmup-ms "$warmup_ms" --sample-ms "$sample_ms" --samples "$samples")

run() {
  local label="$1"
  shift
  echo "== $label =="
  "$@" "${args[@]}"
  echo
}

run "ocaml-native" env BENCH_RUNTIME_LABEL="ocaml-native" "$ocaml_native"
run "js_of_ocaml" env BENCH_RUNTIME_LABEL="js_of_ocaml" node "$ocaml_js"
run "upstream-cljs-js" env UPSTREAM_DATASCRIPT_JS="$upstream_datascript_js" node "$repo_root/bench/bench_upstream.js"
