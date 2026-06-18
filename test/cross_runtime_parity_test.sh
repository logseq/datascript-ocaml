#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
ocaml_runner="${1:-$repo_root/_build/default/test/cross_runtime_ocaml.exe}"
upstream_runner="${2:-$repo_root/script/cross_runtime_upstream.js}"

case "$ocaml_runner" in
  */*) ;;
  *) ocaml_runner="./$ocaml_runner" ;;
esac

bash "$repo_root/script/cross_runtime_parity.sh" "$ocaml_runner" "$upstream_runner"
