#!/usr/bin/env bash
# Idempotent Cloud Agent bootstrap: ensure OCaml 5.5 switch, install project
# opam dependencies, and build installable packages.
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

if ! command -v opam >/dev/null 2>&1; then
  echo "opam is required but was not found on PATH" >&2
  exit 1
fi

export OPAMYES=1
export OPAMCOLOR=never

if ! opam switch list --short 2>/dev/null | grep -qx '5.5'; then
  opam switch create 5.5 ocaml-base-compiler.5.5.0
fi

eval "$(opam env --switch=5.5)"

opam install . --deps-only --with-test -y
dune build @install
