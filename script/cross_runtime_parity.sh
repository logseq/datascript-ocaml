#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
ocaml_runner="${1:-$repo_root/_build/default/test/cross_runtime_ocaml.exe}"
upstream_runner="${2:-$repo_root/script/cross_runtime_upstream.js}"
upstream_fuzz_runner="${3:-$repo_root/script/cross_runtime/upstream_fuzz.cljs}"
upstream_datascript_js="${UPSTREAM_DATASCRIPT_JS:-/Users/tiensonqin/Codes/projects/datascript/release-js/datascript.js}"
upstream_datascript_repo="${UPSTREAM_DATASCRIPT_REPO:-/Users/tiensonqin/Codes/projects/datascript}"

if [ ! -f "$upstream_datascript_js" ]; then
  echo "Upstream DataScript JS bundle not found: $upstream_datascript_js" >&2
  exit 2
fi

if [ ! -d "$upstream_datascript_repo" ]; then
  echo "Upstream DataScript repo not found: $upstream_datascript_repo" >&2
  exit 2
fi

if [ ! -f "$upstream_fuzz_runner" ]; then
  echo "Upstream fuzz ClojureScript runner not found: $upstream_fuzz_runner" >&2
  exit 2
fi

tmp_dir="$(mktemp -d)"
cljs_runner_dir="$upstream_datascript_repo/target/datascript-ocaml-cross-runtime"
trap 'rm -rf "$tmp_dir" "$cljs_runner_dir"' EXIT

upstream_out="$tmp_dir/upstream.jsonl"
ocaml_out="$tmp_dir/ocaml.jsonl"
cljs_out="$tmp_dir/upstream-fuzz.js"
mkdir -p "$cljs_runner_dir/cross_runtime"
cp "$upstream_fuzz_runner" "$cljs_runner_dir/cross_runtime/upstream_fuzz.cljs"

UPSTREAM_DATASCRIPT_JS="$upstream_datascript_js" node "$upstream_runner" > "$upstream_out"
(
  cd "$upstream_datascript_repo"
  clojure \
    -Sdeps '{:paths ["src" "test" "target/datascript-ocaml-cross-runtime"]}' \
    -M:cljs \
    -m cljs.main \
    --target node \
    --output-to "$cljs_out" \
    --compile cross-runtime.upstream-fuzz >/dev/null
  node "$cljs_out"
) >> "$upstream_out"
"$ocaml_runner" > "$ocaml_out"

diff -u "$upstream_out" "$ocaml_out"
