#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

eval "$(opam env --switch=5.5)"

export OPAMYES=1
opam install . --deps-only --with-test -y
dune build
