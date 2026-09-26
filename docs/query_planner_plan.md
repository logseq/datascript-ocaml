# Query Planner Implementation Plan

See also `docs/query_implementation_comparison.md` for a side-by-side analysis of
a reference compiled executor versus the current OCaml interpreter (lists, bindings,
allocation patterns, and per-query-shape gaps).

This plan implements the decision in `docs/adr/query-planner.md`. It is ordered by
risk and benchmark impact. Each phase has explicit parity and performance gates.

## Current baseline (interpreter + fast paths)

| Area | Location | Behavior |
| --- | --- | --- |
| Relation evaluator | `impl/query_where.ml` | Shape-gated `{ attrs; rows }` relations, hash joins |
| Same-entity fusion | `relation_of_same_entity_patterns` | Bitset intersection + value scan |
| AVET predicates | `relation_of_avet_value_comparisons` | Index range + direct row collection |
| Find projection | `impl/query_api.ml` | Skip binding maps when `:find` vars match attrs |
| Index access | `impl/db.ml` | AVET slice, lazy seq, temporal filter pred |

### Benchmark gaps to close (20k shared DB, target: native OCaml ≤ competitor on all 15 cases)

| Query | Issue | Root cause |
| --- | --- | --- |
| qpred1/2, q-pred-range | 20–30× slower | Full row materialization per iteration; loose AVET bounds |
| q3, q4 | ~2.5× slower | Same-entity path builds rows via per-entity probes vs fused merge |
| q5, q-or, q-not | ~1.5–2.8× slower | Hash join + binding round-trips |
| q-rule | ~5× slower | Rule invocation overhead; no relation fast path for rule body |

## Phase 0 — Hot-path fixes (in progress)

**Goal:** Remove avoidable allocation and redundant filters without a full planner.

1. Precompute direct pattern row slots; collect with rev accumulator (done).
2. Tighten AVET bounds for strict Int inequalities; skip post-filter when exact.
3. Extend `eval_relation_rows` to simple non-recursive rule heads whose body is
   relation-only (e.g. `(follow ?e1 ?e2)` → `[?e1 :follows ?e2]`).
4. Expand golden tests in `test/test_shared_queries.ml` to all 15 benchmark
   queries at size=2000, seed=1.

**Gate:** `opam exec -- dune runtest`; `shared_query_bench.exe --size 2000`; qpred ≤ 0.5 ms
at 2000; no result count regressions.

## Phase 1 — Logical plan IR and analysis

**Goal:** Compile supported queries to a stable logical tree; still execute via
existing operators initially.

1. Add `impl/query_plan.ml`:
   - types: `logical_node`, `plan`, `bound`, `index_choice`
   - `analyze : db -> query -> plan option`
2. Recognize plan shapes equivalent to current fast paths (scan, range, merge, join).
3. Unit tests: analyze-only fixtures mirroring benchmark queries.

**Gate:** 100% of Phase 0 benchmark queries produce a plan; unsupported shapes return
`None` and use interpreter fallback.

## Phase 2 — Cost-based join ordering

**Goal:** Order clauses by estimated cost, not source order.

1. Cardinality hints: AVET slice width, constant lookup count, `max_datom_e`.
2. Selinger DP for ≤ 8 logical nodes; left-deep preference when costs tie.
3. Verify q2-switch and reordered q3/q4 pick the same or better plans.

**Gate:** q3/q4 at 20k ≤ competitor; no ordering-sensitive test regressions.

## Phase 3 — Streaming physical operators

**Goal:** Execute plans without materializing full relations.

1. `RangeScan` iterator — wrap `index_range`, emit column tuple per datom.
2. `MergeScan` iterator — synchronized seek on same-entity legs (EAVT/AEVT/AVET).
3. `HashProbe` — open-address entity map; build from smaller side.
4. Pipe iterators through `relation_rows_for_find` for `:find` projection.

**Gate:** qpred at 20k ≤ competitor; native memory churn reduced (fewer major heap
words in benchmark loop).

## Phase 4 — Unified execution and fallback shrink

**Goal:** One primary executor; delete redundant interpreter branches.

1. Route `Query_api.q_sources_raw` through compile → execute.
2. Keep interpreter only for recursive rules, unsupported callables, exotic `not-join`.
3. Document remaining interpreter-only shapes in `docs/query_planner.md`.

**Gate:** full `dune runtest`; all 15 benchmark cases native ≤ competitor; js_of_ocaml
≥ upstream DataScript on standard `bench_ocaml` suite.

## Phase 5 — Temporal and write benchmarks

**Goal:** Extend parity coverage beyond read-only people benchmark.

1. Golden tests for `as_of` / `since` / history queries (tx-filter gate tests).
2. Write + query microbench if competitor suite includes writes.
3. Planner must respect `source_context` filters on all iterators.

**Gate:** tx-filter gate tests pass; temporal query plans use same IR nodes with
filtered index access.

## Testing strategy

| Layer | Tool |
| --- | --- |
| Result parity | `test/test_shared_queries.ml` — counts per query |
| Semantic parity | existing `dune runtest` query fixtures |
| Performance | `bench/shared_query_bench.ml`, `script/benchmark_vs_cljs.sh` |
| Planner internals | new `test/test_query_plan.ml` (analyze/lower only) |

## File ownership (target end state)

```
impl/query_plan.ml       — analyze, cost, optimize
impl/query_exec.ml       — physical operators, streaming
impl/query_where.ml      — shrink to fallback interpreter + shared helpers
impl/query_api.ml        — compile hook, find projection
docs/adr/query-planner.md — architecture decision (this ADR)
docs/query_planner_plan.md — this plan
```

## Principles

- No new public APIs.
- Observable behavior matches upstream DataScript.
- Prefer deleting special cases once the generic plan shape covers them.
- Do not disable compiler warnings; no magic type casts.
