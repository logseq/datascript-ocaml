#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
ocaml_runner="${1:-$repo_root/_build/default/test/cross_runtime_ocaml.exe}"
upstream_runner="${2:-$repo_root/script/cross_runtime_upstream.js}"
upstream_datascript_js="${UPSTREAM_DATASCRIPT_JS:-/Users/tiensonqin/Codes/projects/datascript/release-js/datascript.js}"

if [ ! -f "$upstream_datascript_js" ]; then
  echo "Upstream DataScript JS bundle not found: $upstream_datascript_js" >&2
  exit 2
fi

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT

upstream_out="$tmp_dir/upstream.jsonl"
ocaml_out="$tmp_dir/ocaml.jsonl"

UPSTREAM_DATASCRIPT_JS="$upstream_datascript_js" node "$upstream_runner" > "$upstream_out"
"$ocaml_runner" > "$ocaml_out"

diff -u "$upstream_out" "$ocaml_out"
