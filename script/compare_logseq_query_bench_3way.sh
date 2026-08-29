#!/usr/bin/env bash
# Three-way Logseq shared-query bench (SQLite backend throughout):
#   1) Logseq-forked DataScript via nbb + node:sqlite
#   2) OCaml on origin/main (existing ref via detached worktree; no new branch)
#   3) OCaml on the current checkout
set -euo pipefail

repo_root="$(git -C "$(dirname "$0")/.." rev-parse --show-toplevel)"
cd "$repo_root"

size="${BENCH_SIZE:-20000}"
pages="${BENCH_PAGES:-2000}"
warmup_ms="${BENCH_WARMUP_MS:-200}"
sample_ms="${BENCH_SAMPLE_MS:-200}"
repeats="${BENCH_REPEATS:-3}"
jit_warmup="${BENCH_JIT_WARMUP:-20}"
out_dir="${BENCH_OUT_DIR:-/opt/cursor/artifacts}"
upstream_js="${UPSTREAM_DATASCRIPT_JS:-$repo_root/_deps/datascript/release-js/datascript.js}"
main_ref="${BENCH_MAIN_REF:-origin/main}"
worktree="${BENCH_MAIN_WORKTREE:-/tmp/datascript-ocaml-main-bench}"
nbb_bin="${NBB_BIN:-}"

mkdir -p "$out_dir"

if [ -z "$nbb_bin" ]; then
  if [ -x /tmp/nbb-local/node_modules/.bin/nbb ]; then
    nbb_bin=/tmp/nbb-local/node_modules/.bin/nbb
  elif command -v nbb >/dev/null 2>&1; then
    nbb_bin="$(command -v nbb)"
  else
    npm install nbb --no-save --prefix /tmp/nbb-local
    nbb_bin=/tmp/nbb-local/node_modules/.bin/nbb
  fi
fi

if [ ! -f "$upstream_js" ]; then
  echo "Missing upstream JS bundle: $upstream_js" >&2
  echo "Run: script/ensure_upstream_datascript.sh" >&2
  exit 2
fi

args=(--size "$size" --pages "$pages" --warmup-ms "$warmup_ms" --sample-ms "$sample_ms" --repeats "$repeats" --jit-warmup "$jit_warmup")

echo "== building current-branch shared sqlite bench =="
dune build bench/logseq_query_bench_shared.exe

echo "== 1/3 logseq-forked cljs via nbb + node:sqlite =="
BENCH_RUNTIME_LABEL="logseq-forked-cljs-nbb" \
  UPSTREAM_DATASCRIPT_JS="$upstream_js" \
  "$nbb_bin" "$repo_root/bench/logseq_query_bench_upstream.cljs" \
    "${args[@]}" --sqlite "$out_dir/logseq-bench-cljs-$size.sqlite3" \
  | tee "$out_dir/bench-logseq-shared-cljs-nbb.txt"

echo "== preparing origin/main worktree (no new branch) =="
git fetch origin main
if [ -d "$worktree" ]; then
  git -C "$worktree" fetch origin main 2>/dev/null || true
  git -C "$worktree" checkout -f --detach "$main_ref"
  git -C "$worktree" reset --hard "$main_ref"
else
  git worktree add --detach "$worktree" "$main_ref"
fi

mkdir -p "$worktree/bench"
cp "$repo_root/bench/logseq_query_bench_shared.ml" "$worktree/bench/logseq_query_bench_shared.ml"
if ! grep -q 'logseq_query_bench_shared' "$worktree/bench/dune"; then
  cat >> "$worktree/bench/dune" <<'EOF'

(executable
 (name logseq_query_bench_shared)
 (modules logseq_query_bench_shared)
 (modes exe)
 (libraries datascript-ocaml-native datascript_sqlite unix sqlite3))
EOF
fi

echo "== 2/3 ocaml origin/main (sqlite) =="
(
  cd "$worktree"
  dune build bench/logseq_query_bench_shared.exe
  BENCH_RUNTIME_LABEL="ocaml-main" \
    dune exec -- bench/logseq_query_bench_shared.exe \
      "${args[@]}" --sqlite "$out_dir/logseq-bench-ocaml-main-$size.sqlite3"
) | tee "$out_dir/bench-logseq-shared-ocaml-main.txt"

echo "== 3/3 ocaml current branch (sqlite) =="
BENCH_RUNTIME_LABEL="ocaml-current" \
  dune exec -- bench/logseq_query_bench_shared.exe \
    "${args[@]}" --sqlite "$out_dir/logseq-bench-ocaml-current-$size.sqlite3" \
  | tee "$out_dir/bench-logseq-shared-ocaml-current.txt"

python3 - <<'PY' "$out_dir" "$size" "$pages"
import sys
from pathlib import Path

out_dir = Path(sys.argv[1])
size, pages = sys.argv[2], sys.argv[3]

def parse(path):
    rows = {}
    for line in path.read_text().splitlines():
        if "\t" not in line:
            continue
        k, v = line.split("\t", 1)
        rows[k] = v
    return rows

paths = {
    "logseq-forked-cljs-nbb": out_dir / "bench-logseq-shared-cljs-nbb.txt",
    "ocaml-main": out_dir / "bench-logseq-shared-ocaml-main.txt",
    "ocaml-current": out_dir / "bench-logseq-shared-ocaml-current.txt",
}
parsed = {label: parse(path) for label, path in paths.items()}
queries = [
    "build-ms", "restore-ms",
    "recent-pages", "latest-journals", "uuid-lookup", "title-lookup",
    "children-by-parent", "blocks-by-page", "tags-scan", "eavt-entity",
    "entity-hydrate", "q-updated-at-between", "q-journal-pages", "q-page-by-name",
]
labels = list(paths.keys())

md = []
md.append("# Logseq shared query bench (3-way, SQLite)")
md.append("")
md.append(f"- Size: {size} entities / {pages} pages")
md.append("- Backend: **SQLite** for all runtimes")
md.append("  - CLJS: nbb + `node:sqlite` storing `serializable` / `from_serializable`")
md.append("  - OCaml: `datascript_sqlite` index store + restore")
md.append("- Workload: Logseq `initial_data` hot paths")
md.append("")
md.append("| query | " + " | ".join(labels) + " |")
md.append("| --- | " + " | ".join(["---:"] * len(labels)) + " |")
for q in queries:
    cells = [parsed[l].get(q, "—") for l in labels]
    md.append(f"| `{q}` | " + " | ".join(cells) + " |")
md.append("")
md.append("Times are median ms/op.")
md.append("")
md.append("Artifacts:")
for label, path in paths.items():
    md.append(f"- `{path.name}` ({label})")

report = out_dir / "bench-logseq-shared-3way.md"
report.write_text("\n".join(md) + "\n")
print(report.read_text())
print(f"wrote {report}", file=sys.stderr)
PY
