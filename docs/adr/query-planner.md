# ADR: Compiled Query Planner for Datalog Pattern Queries

## Status

Accepted — Datahike-aligned logical/physical IR is live; relational interpreter
remains the permanent fallback. `datascript.ml` shape-gate `simple_*` bypasses
were removed.

## Context

The query engine evaluates `:where` clauses through a hybrid interpreter in
`impl/query_where.ml` (relation `{ attrs; rows }` path, then binding path).
That interpreter is the Datahike-style **relational fallback**: it must remain
for ineligible shapes (recursive rules, exotic callables, multi-source cases,
DataScript error-order for `not`, etc.).

Previously, `Datascript.q` also tried six benchmark-shaped `simple_*` gates that
duplicated `query_where` logic and drifted from it. Those gates are removed.

The planner mirrors Datahike's pipeline at the IR level:

```
classify / build logical → lower (cost + readiness) → execute via relation ops
  ↳ on ineligible / non-executable plans: relational interpreter
```

Observable **results** stay DataScript-compatible. **Execution architecture**
follows Datahike (explicit divergence from “implementation details match
DataScript”).

Performance remains a hard requirement: native OCaml must lead tracked
benchmarks; `js_of_ocaml` must stay at least on par with upstream DataScript JS.

## Decision

### Logical IR (`impl/query_plan.ml`)

| Node | Meaning |
| --- | --- |
| `LScan` | Single pattern |
| `LEntityJoin` | Same-entity scans + foldable anti-scans + attached filters |
| `LFilter` | Comparison / equality predicates |
| `LUnion` | `or` / `or-join` |
| `LAntiJoin` | `not` / `not-join` not folded into an entity group |
| `LRuleExpand` | Non-recursive rule (usually inlined before lower) |
| `LPassthrough` | Force relational fallback for that clause |

### Lowering

- Index preference from ground components (EAVT / AEVT / AVET).
- Entity-group legs ordered cheapest-first.
- Filters attached when their vars ⊆ one entity group.
- Readiness-aware schedule among physical ops.
- Queries containing any `not` / `not-join` **keep source clause order** so
  DataScript unbound-var errors remain observable.

### Execution

Physical ops lower back to ordered clauses consumed by existing operators:

- `relation_of_same_entity_patterns`, `relation_of_pattern`, AVET range helpers
- `hash_join`, `anti_join`, `union_relations`
- Find projection in `query_api.ml`

Entry: `Datascript.q` → `Query_impl.q` → `q_sources_raw` → `eval_relation_rows`
(with `plan_ordered_clauses`) → binding interpreter if needed.

### Non-goals (deferred)

- Full fused cursor pipelines / Selinger DP / count-slice cardinality
- Semi-naive recursive fixpoint / stratum aggregates / prepared-query cache
- Deleting the relational interpreter

## Consequences

### Positive

- One planner + one fallback (Datahike shape); no duplicate `simple_*` gates.
- Logical entity grouping matches Datahike’s `LEntityJoin` model.
- Dead stub IR (`left_deep_join` discarded, unused `RangeScan` constructors) replaced.

### Risks

- Planner reordering must not change DataScript error surfaces (mitigated by
  source-order preserve on NOT).
- Cost estimates are still heuristic until real slice counts land.

## References

- Datahike `doc/query-engine.md`, `src/datahike/query/*`
- `docs/datahike-query-alignment.md`
- `docs/query_implementation_comparison.md`
- `impl/query_plan.ml`, `impl/query_where.ml`
