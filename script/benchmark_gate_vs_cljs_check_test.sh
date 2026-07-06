#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
checker="$repo_root/script/benchmark_gate_vs_cljs_check.js"

passing_output="$(cat <<'EOF'
runtime	ocaml-native
size	1000
add-1	1.00
new-query	2.00

runtime	js_of_ocaml
size	1000
add-1	1.50
new-query	2.50

runtime	upstream-cljs-js
size	1000
add-1	3.00
new-query	4.00
EOF
)"

printf '%s\n' "$passing_output" | node "$checker"

failing_output="$(cat <<'EOF'
runtime	ocaml-native
size	1000
add-1	1.00

runtime	js_of_ocaml
size	1000
add-1	1.50
new-query	2.50

runtime	upstream-cljs-js
size	1000
add-1	3.00
new-query	4.00
EOF
)"

if printf '%s\n' "$failing_output" | node "$checker" 2>/tmp/benchmark-gate-check-test.err; then
  echo "expected native OCaml to fail when a comparable upstream case is missing" >&2
  exit 1
fi

if ! grep -q "missing ocaml-native new-query" /tmp/benchmark-gate-check-test.err; then
  cat /tmp/benchmark-gate-check-test.err >&2
  exit 1
fi
