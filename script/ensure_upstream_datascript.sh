#!/usr/bin/env bash
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
upstream_repo="${UPSTREAM_DATASCRIPT_REPO:-$repo_root/_deps/datascript}"
upstream_js="${UPSTREAM_DATASCRIPT_JS:-$upstream_repo/release-js/datascript.js}"
upstream_git_url="${UPSTREAM_DATASCRIPT_GIT_URL:-https://github.com/logseq/datascript.git}"

if [ ! -d "$upstream_repo" ]; then
  mkdir -p "$(dirname "$upstream_repo")"
  git clone "$upstream_git_url" "$upstream_repo"
fi

if [ ! -f "$upstream_js" ]; then
  if ! command -v lein >/dev/null 2>&1; then
    echo "lein is required to build the upstream DataScript JS bundle at $upstream_js." >&2
    exit 2
  fi
  (
    cd "$upstream_repo"
    lein with-profile test cljsbuild once release
  )
fi

if [ ! -f "$upstream_js" ]; then
  echo "Upstream DataScript JS bundle not found after build: $upstream_js" >&2
  exit 2
fi
