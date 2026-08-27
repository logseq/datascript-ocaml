#!/usr/bin/env bash
# Run comprehensive history + Datahike-compatible excise (purge) tests.
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

if command -v opam >/dev/null 2>&1; then
  eval "$(opam env --switch=5.5 2>/dev/null || opam env 2>/dev/null || true)"
fi

echo "==> history+excise (integration)"
dune exec test/test_history_excise.exe

echo "==> tx history suite"
dune exec test/test_tx_history.exe

echo "==> purge suite"
dune exec test/test_purge.exe

echo "All history/excise tests passed."
