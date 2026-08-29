#!/usr/bin/env bash
# Three-way shared people-query suite (q1/q2/q3/…):
#   1) OCaml current — non-PSS durable SQLite Share indexes
#   2) Datahike — PSS + SQLite JDBC
#   3) Datalevin — durable LMDB
# Compares canonical result-edn strings; reports timings.
set -euo pipefail

repo_root="$(git -C "$(dirname "$0")/.." rev-parse --show-toplevel)"
cd "$repo_root"

if command -v opam >/dev/null 2>&1; then
  eval "$(opam env --switch=5.5 2>/dev/null || opam env 2>/dev/null || true)"
fi

size="${BENCH_SIZE:-20000}"
warmup_ms="${BENCH_WARMUP_MS:-200}"
sample_ms="${BENCH_SAMPLE_MS:-200}"
repeats="${BENCH_REPEATS:-2}"
jit_warmup="${BENCH_JIT_WARMUP:-100}"
out_dir="${BENCH_OUT_DIR:-/opt/cursor/artifacts}"
query_filter="${BENCH_QUERY:-}"

mkdir -p "$out_dir"

args=(--size "$size" --warmup-ms "$warmup_ms" --sample-ms "$sample_ms" --repeats "$repeats" --jit-warmup "$jit_warmup")
if [ -n "$query_filter" ]; then
  args+=(--query "$query_filter")
fi

echo "== building ocaml shared_query_bench =="
dune build bench/shared_query_bench.exe

echo "== 1/3 ocaml current (sqlite Share) =="
BENCH_RUNTIME_LABEL="ocaml-current-non-pss" \
  dune exec -- bench/shared_query_bench.exe \
    "${args[@]}" --storage sqlite \
    --data-dir "$out_dir/shared-query-ocaml-$size" \
  | tee "$out_dir/bench-shared-people-ocaml-current.txt"

echo "== 2/3 datahike (PSS + sqlite JDBC) =="
(
  cd "$repo_root/bench/external"
  BENCH_RUNTIME_LABEL="datahike-pss-sqlite" \
    clojure -M:shared --runtime datahike \
      "${args[@]}" --sqlite "$out_dir/shared-query-datahike-$size.sqlite3"
) 2>"$out_dir/bench-shared-people-datahike.err" \
  | tee "$out_dir/bench-shared-people-datahike.txt"

echo "== 3/3 datalevin (LMDB) =="
(
  cd "$repo_root/bench/external"
  BENCH_RUNTIME_LABEL="datalevin-lmdb" \
    clojure -M:shared --runtime datalevin \
      "${args[@]}" --sqlite "$out_dir/shared-query-datalevin-$size.sqlite3"
) 2>"$out_dir/bench-shared-people-datalevin.err" \
  | tee "$out_dir/bench-shared-people-datalevin.txt"

python3 - <<'PY' "$out_dir" "$size"
import sys
from pathlib import Path

out_dir = Path(sys.argv[1])
size = sys.argv[2]

paths = {
    "ocaml-current-non-pss": out_dir / "bench-shared-people-ocaml-current.txt",
    "datahike-pss-sqlite": out_dir / "bench-shared-people-datahike.txt",
    "datalevin-lmdb": out_dir / "bench-shared-people-datalevin.txt",
}

def parse_full(path):
    rows, edns = {}, {}
    for line in path.read_text(errors="replace").splitlines():
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

# Prefer queries present on OCaml result-edn; fall back to known suite order.
default_order = [
    "q1", "q2", "q2-switch", "q3", "q4", "q5",
    "qpred1", "qpred2", "q-or", "q-not", "q-or-join", "q-not-join",
    "q-pred-range", "q-5-merge", "q-rule",
]
ocaml_edns = edns["ocaml-current-non-pss"]
query_only = [q for q in default_order if q in ocaml_edns] or sorted(ocaml_edns)
setup = ["build-ms", "restore-ms", "disk-bytes", "store-restore-ms"]
labels = list(paths.keys())

ref_pairs = [
    ("ocaml-current-non-pss", "datahike-pss-sqlite"),
    ("ocaml-current-non-pss", "datalevin-lmdb"),
]

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
            # Keep mismatch short in the report.
            mismatches.append(
                f"{left_l} vs {right_l} / {q}: edn mismatch "
                f"(left_len={len(left)}, right_len={len(right)})"
            )

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
md.append("# Shared people query bench (3-way)")
md.append("")
md.append(f"- Size: {size} entities")
md.append("- OCaml current: non-PSS durable SQLite Share indexes")
md.append("- Datahike: PSS + SQLite JDBC")
md.append("- Datalevin: durable LMDB")
md.append("- Result equality: canonical `result-edn` strings")
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
    md.append("All compared `result-edn` strings match (OCaml current, Datahike, Datalevin).")
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
    md.append("Every shared query timing is faster on OCaml current than Datahike.")
    md.append("")

md.append("| query | " + " | ".join(labels) + " |")
md.append("| --- | " + " | ".join(["---:"] * len(labels)) + " |")
for q in setup + query_only:
    cells = [parsed[l].get(q, "—") for l in labels]
    if all(c == "—" for c in cells):
        continue
    md.append(f"| `{q}` | " + " | ".join(cells) + " |")
md.append("")
md.append("Times are median ms/op.")
md.append("")
md.append("Artifacts:")
for label, path in paths.items():
    md.append(f"- `{path.name}` ({label})")

report = out_dir / "bench-shared-people-3way.md"
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
