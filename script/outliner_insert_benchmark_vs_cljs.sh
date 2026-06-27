#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
size="${OUTLINER_BENCH_SIZE:-5000}"
warmup_ms="${OUTLINER_BENCH_WARMUP_MS:-300}"
sample_ms="${OUTLINER_BENCH_SAMPLE_MS:-700}"
samples="${OUTLINER_BENCH_SAMPLES:-7}"
upstream_datascript_js="${UPSTREAM_DATASCRIPT_JS:-$repo_root/../datascript/release-js/datascript.js}"
ocaml_native="${OUTLINER_BENCH_OCAML_NATIVE:-$repo_root/_build/default/bench/outliner_insert_ocaml.exe}"
ocaml_js="${OUTLINER_BENCH_OCAML_JS:-$repo_root/_build/default/bench/outliner_insert_ocaml.bc.js}"

if [ ! -f "$upstream_datascript_js" ]; then
  echo "Set UPSTREAM_DATASCRIPT_JS to the upstream DataScript JS bundle." >&2
  exit 2
fi

if [ "${OUTLINER_BENCH_SKIP_BUILD:-0}" != "1" ]; then
  dune build --profile release bench/outliner_insert_ocaml.exe bench/outliner_insert_ocaml.bc.js
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
run "upstream-cljs-js" env UPSTREAM_DATASCRIPT_JS="$upstream_datascript_js" node "$repo_root/bench/outliner_insert_upstream.js"
