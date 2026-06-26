#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
size="${MEM_BENCH_SIZE:-1000}"
tx_size="${MEM_BENCH_TX_SIZE:-20}"
upstream_datascript_js="${UPSTREAM_DATASCRIPT_JS:-}"
ocaml_native="${MEM_BENCH_OCAML_NATIVE:-$repo_root/_build/default/bench/memory_ocaml.exe}"
ocaml_js="${MEM_BENCH_OCAML_JS:-$repo_root/_build/default/bench/memory_ocaml_js.bc.js}"

if [ -z "$upstream_datascript_js" ]; then
  echo "Set UPSTREAM_DATASCRIPT_JS to the upstream DataScript JS bundle." >&2
  exit 2
fi

if [ ! -f "$upstream_datascript_js" ]; then
  echo "Upstream DataScript JS bundle not found: $upstream_datascript_js" >&2
  exit 2
fi

if [ "${MEM_BENCH_SKIP_BUILD:-0}" != "1" ]; then
  dune build --profile release bench/memory_ocaml.exe bench/memory_ocaml_js.bc.js
fi

args=(--size "$size" --tx-size "$tx_size")
ocaml_output="$(mktemp)"
ocaml_js_output="$(mktemp)"
upstream_output="$(mktemp)"
combined_output="$(mktemp)"
ocaml_verify="$(mktemp)"
ocaml_js_verify="$(mktemp)"
upstream_verify="$(mktemp)"
trap 'rm -f "$ocaml_output" "$ocaml_js_output" "$upstream_output" "$combined_output" "$ocaml_verify" "$ocaml_js_verify" "$upstream_verify"' EXIT

echo "size=$size"
echo "tx_size=$tx_size"
echo "upstream=$upstream_datascript_js"
echo

env MEMORY_RUNTIME_LABEL="ocaml-native" MEMORY_VERIFY_FILE="$ocaml_verify" \
  "$ocaml_native" "${args[@]}" > "$ocaml_output"
env MEMORY_RUNTIME_LABEL="js_of_ocaml" MEMORY_VERIFY_FILE="$ocaml_js_verify" \
  node --expose-gc "$ocaml_js" "${args[@]}" > "$ocaml_js_output"
env UPSTREAM_DATASCRIPT_JS="$upstream_datascript_js" MEMORY_RUNTIME_LABEL="upstream-cljs-js" \
  MEMORY_VERIFY_FILE="$upstream_verify" \
  node --expose-gc "$repo_root/bench/memory_upstream.js" "${args[@]}" > "$upstream_output"

cat "$ocaml_output" "$ocaml_js_output" "$upstream_output" > "$combined_output"

verify_final_data() {
  local runtime="$1"
  local actual="$2"
  local diff_output
  diff_output="$(mktemp)"
  if diff -u "$upstream_verify" "$actual" > "$diff_output"; then
    local lines
    local sha
    lines="$(wc -l < "$actual" | tr -d ' ')"
    sha="$(shasum -a 256 "$actual" | awk '{print $1}')"
    printf 'final-data\t%s\tPASS\tlines=%s\tsha256=%s\n' "$runtime" "$lines" "$sha"
    rm -f "$diff_output"
  else
    echo "final-data	$runtime	FAIL"
    cat "$diff_output"
    rm -f "$diff_output"
    return 1
  fi
}

verify_final_data "ocaml-native" "$ocaml_verify"
verify_final_data "js_of_ocaml" "$ocaml_js_verify"
echo

echo -e "runtime\tscenario\trss_mb\theap_mb\trss_bytes\theap_bytes"
awk -F '\t' '
  {
    printf "%s\t%s\t%.2f\t%.2f\t%s\t%s\n", $1, $2, $3 / 1048576, $4 / 1048576, $3, $4
  }
' "$combined_output"

echo
echo -e "runtime\tscenario\truntime_rss_mb\tupstream_rss_mb\tdelta_mb\tstatus"
awk -F '\t' '
  $1 == "ocaml-native" { runtime[$1, $2] = $3 }
  $1 == "js_of_ocaml" { runtime[$1, $2] = $3 }
  $1 == "upstream-cljs-js" { upstream[$2] = $3 }
  END {
    runtimes[1] = "ocaml-native"
    runtimes[2] = "js_of_ocaml"
    scenarios[1] = "initial-open"
    scenarios[2] = "after-transact-query"
    scenarios[3] = "after-gc"
    for (r = 1; r <= 2; r++) {
      runtime_name = runtimes[r]
      for (i = 1; i <= 3; i++) {
        scenario = scenarios[i]
        delta = runtime[runtime_name, scenario] - upstream[scenario]
        status = delta < 0 ? "PASS" : "FAIL"
        printf "%s\t%s\t%.2f\t%.2f\t%.2f\t%s\n", runtime_name, scenario, runtime[runtime_name, scenario] / 1048576, upstream[scenario] / 1048576, delta / 1048576, status
      }
    }
  }
' "$combined_output"

echo
echo -e "runtime\tscenario\truntime_heap_mb\tupstream_heap_mb\tdelta_mb\tstatus"
awk -F '\t' '
  $1 == "ocaml-native" { heap[$1, $2] = $4 }
  $1 == "js_of_ocaml" { heap[$1, $2] = $4 }
  $1 == "upstream-cljs-js" { upstream[$2] = $4 }
  END {
    failed = 0
    runtimes[1] = "ocaml-native"
    runtimes[2] = "js_of_ocaml"
    scenarios[1] = "initial-open"
    scenarios[2] = "after-transact-query"
    scenarios[3] = "after-gc"
    for (r = 1; r <= 2; r++) {
      runtime_name = runtimes[r]
      for (i = 1; i <= 3; i++) {
        scenario = scenarios[i]
        delta = heap[runtime_name, scenario] - upstream[scenario]
        status = delta < 0 ? "PASS" : "FAIL"
        if (status == "FAIL") failed = 1
        printf "%s\t%s\t%.2f\t%.2f\t%.2f\t%s\n", runtime_name, scenario, heap[runtime_name, scenario] / 1048576, upstream[scenario] / 1048576, delta / 1048576, status
      }
    }
    exit failed
  }
' "$combined_output"
