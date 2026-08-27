# ADR: Compiled Query Planner for Datalog Pattern Queries

## Status

Accepted

## Context

The query engine today evaluates `:where` clauses through a hybrid interpreter in
`impl/query_where.ml`. Simple shapes already take dedicated fast paths — same-entity
pattern fusion, AVET range scans for value predicates, hash-join for cross-entity
patterns, and direct relation-to-find projection in `impl/query_api.ml`. This works
well for many upstream DataScript queries and keeps semantics aligned with the public
Clojure/ClojureScript engine.

However, the interpreter model has structural limits:

1. **No compile/execute split.** Each query re-derives index choices and clause
   ordering from scratch. There is no reusable plan for repeated execution inside
   benchmarks, reactive queries, or application hot loops.

2. **Shape-gated fast paths.** Optimizations are tied to specific clause sequences.
   Equivalent queries with reordered clauses or slightly different surface syntax can
   miss the fast path and fall back to binding-based evaluation.

3. **Intermediate materialization.** Even when index access is narrow, many paths
   build full `{ attrs; rows }` relations before projection. For large selective
   scans (predicate/range queries over indexed attributes), row construction and
   list allocation dominate runtime.

4. **No cost model.** Clause order follows source order or ad hoc heuristics. A
   constant lookup followed by a wide scan can be chosen when the reverse order would
   probe far fewer datoms.

5. **Rule and join overhead.** Non-recursive rules and multi-clause joins still
   round-trip through binding lists even when the rule body is a single indexed
   pattern.

Industry Datalog engines that compile queries to index plans share a common shape:
analyze clauses into a logical plan, estimate access cost, order joins, lower to
physical operators (range scan, merge scan, hash probe), and stream results without
materializing full binding maps. The OCaml port should converge on that architecture
while preserving DataScript semantics and the existing public query API (`q`, `q`,
inputs, rules, temporal views).

Performance is a hard requirement: native OCaml must lead tracked benchmark suites,
and `js_of_ocaml` must stay at least on par with upstream DataScript JavaScript.
Planner work is incomplete if it regresses those targets.

## Decision

Introduce a **compiled query planner** behind the existing query entry points. The
planner will not add new public APIs. Parsed queries will optionally compile to a
small logical plan IR, optimize clause order, lower to physical operators, and
execute with streaming index access.

### Logical plan IR

Represent `:where` clauses as a tree of logical nodes:

| Node | Meaning |
| --- | --- |
| `Scan` | Single pattern on one index (EAVT, AEVT, AVET, or VAET-equivalent path) |
| `RangeScan` | AVET slice with optional open/closed bounds on value |
| `MergeScan` | Same-entity multi-pattern intersection via synchronized cursors |
| `HashJoin` | Cross-entity or cross-variable join on shared keys |
| `Filter` | Comparison, equality, or callable predicate on bound columns |
| `AntiJoin` | `not` / `not-join` exclusion |
| `Union` | `or` / `or-join` branches |
| `RuleExpand` | Inline non-recursive rule heads |

Each node carries:

- bound and free variables
- chosen index and prefix fields (e, a, v, tx)
- estimated row count (cardinality hint)
- source (`$` or named DB)

### Analysis phase

1. **Constant propagation** — substitute single-value bindings from inputs and prior
   nodes (same as upstream `substitute-constants`).
2. **Index selection** — for each pattern, pick the narrowest index: AVET when attr
   and value bounds exist; AEVT when only attr is ground; EAVT when entity is
   ground; reverse-ref via VAET path.
3. **Predicate pushdown** — move comparison clauses onto `RangeScan` bounds when the
   compared variable is the pattern value and the attribute is AVET-indexed.
4. **Same-entity detection** — collapse consecutive same-entity patterns into one
   `MergeScan` node instead of sequential hash joins.

### Optimization phase

Use dynamic programming (Selinger-style) over join ordering for up to a small fixed
number of logical nodes (typically ≤ 8, matching practical DataScript query size):

- **Cost estimates** from index cardinality hints: schema `:db/cardinality`, AVET
  slice width, constant lookup size, and `max_datom_e` fallbacks.
- **Join algorithm choice**: entity-key merge for same-entity; hash probe for
  cross-entity when build side is smaller.
- **Left-deep bias** for selective scans, mirroring upstream `query_v3` behavior.

Keep the current fast paths as **recognized plan shapes** during a transition period
so behavior and performance do not regress while the generic planner matures.

### Physical execution

Lower logical nodes to streaming operators:

1. **Range scan iterator** — walk AVET/AEVT slice; apply tight bounds (strict `>` /
   `<` on integers uses `n±1` bounds to avoid post-filters).
2. **Merge scan iterator** — seekGE + step for each same-entity leg; intersect on
   entity id without building entity bitsets when all legs are direct indexed attrs.
3. **Hash probe join** — build side from smaller relation; probe with entity or value
   keys; reuse open-addressing tables keyed by `int` entity ids where possible.
4. **Direct find projection** — when `:find` variables match scan column order, emit
   result rows without `(var . result)` binding lists.

Results flow as lazy `Seq.t` until the final `:find` projection; materialize only
when deduplication, sorting, or aggregates require it.

### Integration

- **Entry**: `Query_api.q_sources_raw` tries `compile_and_execute` first; on
  unsupported shapes, fall back to the current interpreter (no behavior change).
- **Temporal views**: planner receives the same `source_context` as today (`as_of`,
  `since`, filtered DBs) so index iterators read through existing `fold_datoms` /
  `index_range` hooks.
- **Rules**: non-recursive rules compile to `RuleExpand` + body subplan; recursive
  rules stay on the interpreter until a fixed-point operator is added.
- **Tests**: golden result counts per benchmark query at fixed seed/size; no
  observable difference from interpreter path.

## Consequences

### Positive

- Repeated queries amortize analysis cost; benchmarks and app hot loops benefit.
- Predicate and same-entity queries stream from index cursors with minimal
  allocation.
- Clause reordering becomes cost-driven instead of source-order dependent.
- A single execution model replaces growing special-case branches in
  `query_where.ml`.

### Negative / risks

- Two execution paths until fallback coverage is complete; must keep parity tests
  strict.
- Planner bugs can be subtle (wrong join order, missed pushdown); need exhaustive
  query fixtures.
- `js_of_ocaml` code size may grow slightly; monitor bundle size.

### Non-goals (initial phases)

- SQL-style cost hints or user-provided plan overrides.
- Parallel index scans.
- New public planner or EXPLAIN APIs.

## Implementation phases

See `docs/query_planner_plan.md` for the step-by-step rollout, benchmarks gates,
and file-level ownership.

## References

- `docs/query_planner.md` — upstream DataScript v3 planner notes and current OCaml
  relation evaluator status.
- `impl/query_where.ml` — current interpreter and shape-gated fast paths.
- `impl/query_api.ml` — relation-to-find direct projection.
- Upstream `query_v3.cljc` — logical plan and collapse-rels model.
