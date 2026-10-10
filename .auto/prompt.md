# Autoresearch: datascript-ocaml transact + query performance

Objective (from Tienson, 2026-10-10): keep datascript-ocaml CORRECT, improve
transact and query performance, prefer Logseq's real usage scenarios, keep the
code simple and readable — no complexity without measurable payoff.

Hard constraints
- Correctness first: `dune runtest` must stay green (34+ suites incl.
  cross-runtime parity vs upstream cljs).
- Storage format MUST NOT change: transit codec bytes, node serialization,
  storage_root/tail row layout all frozen. Only the compute paths may change.
- No `Obj.magic`, no disabling compiler warnings (AGENTS.md).
- Prefer simple implementations; upstream DataScript semantics are ground truth.
- Perf bar: must not trail the cljs fork on Logseq-shaped workloads.

Primary metrics (Logseq scenarios; ocaml-native, this box, median of runs)
- `bench/outliner_insert_ocaml.exe --size 5000` → `insert-one-block` (block
  edits, the dominant chat/outliner op)
- `bench/bench_ocaml.exe --size 1000` → `get-page-data`, `add-1`, `add-5`,
  `add-all`, `storage-roundtrip` (page load + transact + persistence)
- `bench/entity_view_bench.exe 1000 500` → entity construct / single attr /
  entity_attrs (UI render views)
- `bench/query_profile.exe 1000 2000` → datoms seeks + q-* (query engine guts)
- `test/logseq_queries.edn` real Logseq queries via test_logseq_query_*
- Secondary: generic q1–q5/qpred benches; js_of_ocaml + upstream-cljs columns
  for parity tracking (script/benchmark_vs_cljs.sh, needs
  _deps/datascript/release-js/datascript.js built per .auto/upstream_bundle.md)

Baselines (this box, main @ 0561660):
  insert-one-block 0.0373ms | get-page-data 0.68–0.99ms
  entity: construct 1.07us / attr 3.91us / attrs-all 0.927ms
  add-1 21.4ms | add-5 24.1ms | add-all 26.6ms
  bulk-explicit 61.2ms | bulk-add-ops 58.4ms | bulk-no-alias 17.5ms
  q1 0.029 | q2 0.096 | q3 0.152 | q4 0.221 | qpred1 0.053 | qpred2 0.061–0.106
  datoms-name-avet 0.0066 | datoms-sex-aevt 0.018 | aevt-fold 0.0136
  storage-roundtrip 36–48ms

Loop protocol (pi-autoresearch style; per user 2026-10-10: run IN-SESSION,
no child sessions)
1. Pick next experiment from `ideas.md` (bounded scope each round).
2. Implement ONE focused change on a branch off integration branch
   `autoresearch/perf`; measure before/after on this box.
3. Verify: build + affected benches + test gate + diff simplicity review →
   commit onto `autoresearch/perf` (keep) or `git checkout .` (revert).
4. Append to `.auto/log.jsonl`; update `best.md`.

Keep rule: target metric improves ≥3% and no other bench regresses >5%,
tests green, diff is simple. Revert rule: anything failing that, or
complexity out of proportion to the gain.

Budget: ~48h wall clock; stop early on sustained plateau.
