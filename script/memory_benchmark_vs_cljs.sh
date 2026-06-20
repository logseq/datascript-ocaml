#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
size="${MEM_BENCH_SIZE:-5000}"
tx_size="${MEM_BENCH_TX_SIZE:-500}"
upstream_datascript_js="${UPSTREAM_DATASCRIPT_JS:-$repo_root/_deps/datascript/release-js/datascript.js}"
ocaml_native="${MEM_BENCH_OCAML_NATIVE:-$repo_root/_build/default/bench/memory_ocaml.exe}"

if [ ! -f "$upstream_datascript_js" ] && [ -f "$repo_root/../datascript/release-js/datascript.js" ]; then
  upstream_datascript_js="$repo_root/../datascript/release-js/datascript.js"
fi

if [ ! -f "$upstream_datascript_js" ]; then
  echo "Upstream DataScript JS bundle not found: $upstream_datascript_js" >&2
  exit 2
fi

if [ "${MEM_BENCH_SKIP_BUILD:-0}" != "1" ]; then
  dune build --profile release bench/memory_ocaml.exe
fi

args=(--size "$size" --tx-size "$tx_size")
ocaml_output="$(mktemp)"
upstream_output="$(mktemp)"
combined_output="$(mktemp)"
trap 'rm -f "$ocaml_output" "$upstream_output" "$combined_output"' EXIT

echo "size=$size"
echo "tx_size=$tx_size"
echo "upstream=$upstream_datascript_js"
echo

env MEMORY_RUNTIME_LABEL="ocaml-native" "$ocaml_native" "${args[@]}" > "$ocaml_output"
env UPSTREAM_DATASCRIPT_JS="$upstream_datascript_js" MEMORY_RUNTIME_LABEL="upstream-cljs-js" \
  node --expose-gc "$repo_root/bench/memory_upstream.js" "${args[@]}" > "$upstream_output"

cat "$ocaml_output" "$upstream_output" > "$combined_output"

echo -e "runtime\tscenario\trss_mb\theap_mb\trss_bytes\theap_bytes"
awk -F '\t' '
  {
    printf "%s\t%s\t%.2f\t%.2f\t%s\t%s\n", $1, $2, $3 / 1048576, $4 / 1048576, $3, $4
  }
' "$combined_output"

echo
echo -e "scenario\tocaml_rss_mb\tupstream_rss_mb\tdelta_mb\tstatus"
awk -F '\t' '
  $1 == "ocaml-native" { ocaml[$2] = $3 }
  $1 == "upstream-cljs-js" { upstream[$2] = $3 }
  END {
    failed = 0
    scenarios[1] = "initial-open"
    scenarios[2] = "after-transact-query"
    scenarios[3] = "after-gc"
    for (i = 1; i <= 3; i++) {
      scenario = scenarios[i]
      delta = ocaml[scenario] - upstream[scenario]
      status = delta < 0 ? "PASS" : "FAIL"
      if (status == "FAIL") failed = 1
      printf "%s\t%.2f\t%.2f\t%.2f\t%s\n", scenario, ocaml[scenario] / 1048576, upstream[scenario] / 1048576, delta / 1048576, status
    }
    exit failed
  }
' "$combined_output"
