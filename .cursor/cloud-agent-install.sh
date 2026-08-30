#!/usr/bin/env bash
# Idempotent Cloud Agent bootstrap.
# Works both when the Dockerfile already provides opam/OCaml 5.5 and when a
# Personal/DB-managed base image does not (install must self-bootstrap).
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

export DEBIAN_FRONTEND=noninteractive
export OPAMYES=1
export OPAMCOLOR=never

need_sudo() {
  if [ "$(id -u)" -eq 0 ]; then
    "$@"
  elif command -v sudo >/dev/null 2>&1; then
    sudo DEBIAN_FRONTEND=noninteractive "$@"
  else
    echo "Need root or sudo to install system packages: $*" >&2
    exit 1
  fi
}

ensure_system_packages() {
  local missing=0
  for pkg in opam pkg-config libsqlite3-dev liblmdb-dev build-essential bubblewrap curl ca-certificates git; do
    if ! dpkg -s "$pkg" >/dev/null 2>&1; then
      missing=1
      break
    fi
  done
  if [ "$missing" -eq 0 ] && command -v opam >/dev/null 2>&1; then
    return 0
  fi
  need_sudo apt-get update
  need_sudo apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    git \
    build-essential \
    pkg-config \
    bubblewrap \
    opam \
    libsqlite3-dev \
    liblmdb-dev
}

ensure_system_packages

if ! command -v opam >/dev/null 2>&1; then
  echo "opam is still missing after apt install" >&2
  exit 1
fi

if [ ! -d "${HOME}/.opam" ]; then
  opam init --disable-sandboxing -a -y
fi

if ! opam switch list --short 2>/dev/null | grep -qx '5.5'; then
  opam switch create 5.5 ocaml-base-compiler.5.5.0
fi

eval "$(opam env --switch=5.5)"

opam install . --deps-only --with-test -y
dune build @install
