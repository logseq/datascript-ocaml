#!/usr/bin/env bash
# Five-way Logseq shared-query bench (fair size, EDN string equality):
#   1) CLJS via @logseq/nbb-logseq — PSS indexes + SQLite kvs IStorage
#   2) OCaml on origin/main — PSS + SQLite blob kvs (restore → memory indexes)
#   3) OCaml on the current checkout — non-PSS durable SQLite Share indexes
#   4) Datahike — PSS + SQLite JDBC (konserve)
#   5) Datalevin — LMDB
set -euo pipefail

repo_root="$(git -C "$(dirname "$0")/.." rev-parse --show-toplevel)"
cd "$repo_root"

size="${BENCH_SIZE:-5000}"
pages="${BENCH_PAGES:-500}"
warmup_ms="${BENCH_WARMUP_MS:-200}"
sample_ms="${BENCH_SAMPLE_MS:-200}"
repeats="${BENCH_REPEATS:-3}"
jit_warmup="${BENCH_JIT_WARMUP:-20}"
out_dir="${BENCH_OUT_DIR:-/opt/cursor/artifacts}"
main_ref="${BENCH_MAIN_REF:-origin/main}"
worktree="${BENCH_MAIN_WORKTREE:-/tmp/datascript-ocaml-main-bench}"
nbb_bin="${NBB_LOGSEQ_BIN:-}"
nbb_prefix="${NBB_LOGSEQ_PREFIX:-/tmp/nbb-logseq-db}"
nbb_pkg="${NBB_LOGSEQ_PKG:-github:logseq/nbb-logseq#feat-db-v34}"
skip_main="${BENCH_SKIP_MAIN:-0}"
skip_cljs="${BENCH_SKIP_CLJS:-0}"

mkdir -p "$out_dir"

if [ -z "$nbb_bin" ] && [ "$skip_cljs" != "1" ]; then
  if [ -x "$nbb_prefix/node_modules/.bin/nbb-logseq" ]; then
    nbb_bin="$nbb_prefix/node_modules/.bin/nbb-logseq"
  elif command -v nbb-logseq >/dev/null 2>&1; then
    nbb_bin="$(command -v nbb-logseq)"
  else
    mkdir -p "$nbb_prefix"
    npm install "$nbb_pkg" --no-save --prefix "$nbb_prefix"
    nbb_bin="$nbb_prefix/node_modules/.bin/nbb-logseq"
  fi
fi

args=(--size "$size" --pages "$pages" --warmup-ms "$warmup_ms" --sample-ms "$sample_ms" --repeats "$repeats" --jit-warmup "$jit_warmup")

echo "== building current-branch shared sqlite bench =="
dune build bench/logseq_query_bench_shared.exe

if [ "$skip_cljs" != "1" ]; then
  echo "== 1/5 cljs via nbb-logseq (PSS + sqlite kvs) =="
  BENCH_RUNTIME_LABEL="cljs-nbb-logseq-pss" \
    "$nbb_bin" "$repo_root/bench/logseq_query_bench_upstream.cljs" \
      "${args[@]}" --sqlite "$out_dir/logseq-bench-cljs-$size.sqlite3" \
    | tee "$out_dir/bench-logseq-shared-cljs-nbb.txt"
else
  echo "== 1/5 cljs skipped =="
fi

if [ "$skip_main" != "1" ]; then
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
  python3 - <<'PY' "$worktree/bench/logseq_query_bench_shared.ml"
from pathlib import Path
import sys
path = Path(sys.argv[1])
text = path.read_text()
text = text.replace(
    "let storage = storage_of_handle (Datascript_sqlite.storage session) in",
    "let storage = Datascript_sqlite.storage session in",
)
text = text.replace("  let db = refresh_db_indexes db in\n", "")
path.write_text(text)
PY
  if ! grep -q 'logseq_query_bench_shared' "$worktree/bench/dune"; then
    cat >> "$worktree/bench/dune" <<'EOF'

(executable
 (name logseq_query_bench_shared)
 (modules logseq_query_bench_shared)
 (modes exe)
 (libraries datascript-ocaml-native datascript_sqlite unix sqlite3))
EOF
  fi

  echo "== 2/5 ocaml origin/main (PSS + sqlite kvs) =="
  (
    cd "$worktree"
    dune build bench/logseq_query_bench_shared.exe
    BENCH_RUNTIME_LABEL="ocaml-main-pss" \
      dune exec -- bench/logseq_query_bench_shared.exe \
        "${args[@]}" --sqlite "$out_dir/logseq-bench-ocaml-main-$size.sqlite3"
  ) | tee "$out_dir/bench-logseq-shared-ocaml-main.txt"
else
  echo "== 2/5 ocaml main skipped =="
fi

echo "== 3/5 ocaml current branch (non-PSS sqlite Share indexes) =="
BENCH_RUNTIME_LABEL="ocaml-current-non-pss" \
  dune exec -- bench/logseq_query_bench_shared.exe \
    "${args[@]}" --sqlite "$out_dir/logseq-bench-ocaml-current-$size.sqlite3" \
  | tee "$out_dir/bench-logseq-shared-ocaml-current.txt"

echo "== 4/5 datahike (PSS + sqlite JDBC) =="
(
  cd "$repo_root/bench/external"
  BENCH_RUNTIME_LABEL="datahike-pss-sqlite" \
    clojure -M:run --runtime datahike \
      "${args[@]}" --sqlite "$out_dir/logseq-bench-datahike-$size.sqlite3"
) 2>"$out_dir/bench-logseq-shared-datahike.err" \
  | tee "$out_dir/bench-logseq-shared-datahike.txt"

echo "== 5/5 datalevin (LMDB) =="
(
  cd "$repo_root/bench/external"
  BENCH_RUNTIME_LABEL="datalevin-lmdb" \
    clojure -M:run --runtime datalevin \
      "${args[@]}" --sqlite "$out_dir/logseq-bench-datalevin-$size.sqlite3"
) 2>"$out_dir/bench-logseq-shared-datalevin.err" \
  | tee "$out_dir/bench-logseq-shared-datalevin.txt"

python3 - <<'PY' "$out_dir" "$size" "$pages" "$skip_cljs" "$skip_main"
import sys
from pathlib import Path

out_dir = Path(sys.argv[1])
size, pages = sys.argv[2], sys.argv[3]
skip_cljs = sys.argv[4] == "1"
skip_main = sys.argv[5] == "1"

paths = {}
if not skip_cljs:
    paths["cljs-nbb-logseq-pss"] = out_dir / "bench-logseq-shared-cljs-nbb.txt"
if not skip_main:
    paths["ocaml-main-pss"] = out_dir / "bench-logseq-shared-ocaml-main.txt"
paths["ocaml-current-non-pss"] = out_dir / "bench-logseq-shared-ocaml-current.txt"
paths["datahike-pss-sqlite"] = out_dir / "bench-logseq-shared-datahike.txt"
paths["datalevin-lmdb"] = out_dir / "bench-logseq-shared-datalevin.txt"

def parse_full(path):
    rows, edns = {}, {}
    for line in path.read_text().splitlines():
        if "\t" not in line:
            continue
        parts = line.split("\t", 2)
        if len(parts) == 3 and parts[0] == "result-edn":
            edns[parts[1]] = parts[2]
        elif len(parts) >= 2:
            rows[parts[0]] = parts[1]
    return rows, edns

parsed_full = {label: parse_full(path) for label, path in paths.items()}
parsed = {label: rows for label, (rows, _edns) in parsed_full.items()}
edns = {label: edn for label, (_rows, edn) in parsed_full.items()}
queries = [
    "build-ms", "restore-ms", "disk-bytes",
    "recent-pages", "latest-journals", "uuid-lookup", "title-lookup",
    "children-by-parent", "blocks-by-page", "tags-scan", "eavt-entity",
    "entity-hydrate", "q-updated-at-between", "q-journal-pages", "q-page-by-name",
]
query_only = [q for q in queries if q not in ("build-ms", "restore-ms", "disk-bytes")]
labels = list(paths.keys())

# Required EDN equality: CLJS vs OCaml current (when CLJS present).
# Also compare Datahike/Datalevin vs OCaml current.
ref_pairs = []
if "cljs-nbb-logseq-pss" in edns:
    ref_pairs.append(("cljs-nbb-logseq-pss", "ocaml-current-non-pss"))
ref_pairs.append(("ocaml-current-non-pss", "datahike-pss-sqlite"))
ref_pairs.append(("ocaml-current-non-pss", "datalevin-lmdb"))

mismatches = []
for left_l, right_l in ref_pairs:
    for q in query_only:
        left = edns[left_l].get(q)
        right = edns[right_l].get(q)
        if left is None or right is None:
            mismatches.append(
                f"{left_l} vs {right_l} / {q}: missing edn "
                f"(left={left is not None}, right={right is not None})"
            )
        elif left != right:
            mismatches.append(
                f"{left_l} vs {right_l} / {q}:\n  left:  {left}\n  right: {right}"
            )

# Performance gate: every Logseq query must be faster on ocaml-current than Datahike.
cur = "ocaml-current-non-pss"
dh = "datahike-pss-sqlite"
slower = []
for q in query_only:
    try:
        cv = float(parsed[cur][q])
        dv = float(parsed[dh][q])
    except (KeyError, ValueError):
        slower.append(f"{q}: missing timing")
        continue
    if cv >= dv:
        slower.append(f"{q}: ocaml-current={cv} >= datahike={dv}")

md = []
md.append("# Logseq shared query bench (5-way)")
md.append("")
md.append(f"- Size: {size} entities / {pages} pages")
md.append("- CLJS: `@logseq/nbb-logseq#feat-db-v34` — PSS + SQLite `kvs`")
md.append("- OCaml main: PSS + SQLite blob kvs")
md.append("- OCaml current: non-PSS durable SQLite Share indexes")
md.append("- Datahike: PSS + SQLite JDBC")
md.append("- Datalevin: LMDB")
md.append("- Result equality: `result-edn` strings")
md.append("")
if mismatches:
    md.append("## Result EDN mismatches")
    md.append("")
    for m in mismatches:
        md.append(f"- {m}")
    md.append("")
else:
    md.append("## Result EDN")
    md.append("")
    md.append("All compared `result-edn` strings match.")
    md.append("")

if slower:
    md.append("## OCaml current vs Datahike (expected: current faster)")
    md.append("")
    for s in slower:
        md.append(f"- {s}")
    md.append("")
else:
    md.append("## OCaml current vs Datahike")
    md.append("")
    md.append("Every Logseq query timing is faster on OCaml current than Datahike.")
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

report = out_dir / "bench-logseq-shared-5way.md"
report.write_text("\n".join(md) + "\n")
print(report.read_text())
print(f"wrote {report}", file=sys.stderr)
rc = 0
if mismatches:
    print("RESULT EDN MISMATCH", file=sys.stderr)
    rc = 1
if slower:
    print("OCAML NOT FASTER THAN DATAHIKE", file=sys.stderr)
    rc = 1
sys.exit(rc)
PY
