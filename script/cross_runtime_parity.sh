#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
ocaml_runner="${1:-$repo_root/_build/default/test/cross_runtime_ocaml.exe}"
upstream_runner="${2:-$repo_root/script/cross_runtime_upstream.js}"
upstream_fuzz_runner="${3:-$repo_root/script/cross_runtime/upstream_fuzz.cljs}"
upstream_datascript_js="${UPSTREAM_DATASCRIPT_JS:-$repo_root/_deps/datascript/release-js/datascript.js}"
upstream_datascript_repo="${UPSTREAM_DATASCRIPT_REPO:-$repo_root/_deps/datascript}"

if [ -z "$upstream_datascript_js" ]; then
  echo "Set UPSTREAM_DATASCRIPT_JS or build the upstream DataScript JS bundle at $repo_root/_deps/datascript/release-js/datascript.js." >&2
  exit 2
fi

if [ -z "$upstream_datascript_repo" ]; then
  echo "Set UPSTREAM_DATASCRIPT_REPO or clone upstream DataScript at $repo_root/_deps/datascript." >&2
  exit 2
fi

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
trap 'rm -rf "$tmp_dir"' EXIT

upstream_out="$tmp_dir/upstream.jsonl"
ocaml_out="$tmp_dir/ocaml.jsonl"
patched_src="$tmp_dir/upstream-src"
cljs_out="$cljs_runner_dir/upstream-fuzz.js"
cljs_cache_key="$cljs_runner_dir/cache.key"
mkdir -p "$cljs_runner_dir/cross_runtime"
cp "$upstream_fuzz_runner" "$cljs_runner_dir/cross_runtime/upstream_fuzz.cljs"
cp -R "$upstream_datascript_repo/src" "$patched_src"
perl -0pi -e 's/\[me\.tonsky\.\s+persistent-sorted-set :as set :refer \[BTSet Node Leaf\]\]/[me.tonsky.persistent-sorted-set :as set :refer [BTSet Node Leaf]]/g' \
  "$patched_src/datascript/storage.cljs"

cljs_source_key="$(
  {
    shasum "$upstream_fuzz_runner"
    (
      cd "$patched_src"
      find . -type f -print0 | sort -z | xargs -0 shasum
    )
    (
      cd "$upstream_datascript_repo"
      git ls-files -z deps.edn test/node_test_runner.cljs 2>/dev/null | xargs -0 shasum
    )
  } | shasum | awk '{print $1}'
)"

UPSTREAM_DATASCRIPT_JS="$upstream_datascript_js" node "$upstream_runner" > "$upstream_out"
(
  cd "$upstream_datascript_repo"
  if [ ! -f "$cljs_out" ] || [ ! -f "$cljs_cache_key" ] || [ "$(cat "$cljs_cache_key")" != "$cljs_source_key" ]; then
    clojure \
      -Sdeps "{:paths [\"$patched_src\" \"test\" \"$cljs_runner_dir\"]}" \
      -M:cljs \
      -m cljs.main \
      --target node \
      --output-to "$cljs_out" \
      --compile cross-runtime.upstream-fuzz >/dev/null
    printf '%s\n' "$cljs_source_key" > "$cljs_cache_key"
  fi
  node "$cljs_out"
) >> "$upstream_out"
"$ocaml_runner" > "$ocaml_out"

diff -u "$upstream_out" "$ocaml_out"
