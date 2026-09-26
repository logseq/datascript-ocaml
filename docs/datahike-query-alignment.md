# Datahike-Aligned Query Rewrite Plan

Work branch: `logseq/shared-api-parity-fe5d` (PR #5).

Observable query **results** remain DataScript-compatible. **Execution architecture**
aligns with Datahike's compiled planner (explicit user divergence from
"implementation details match DataScript").

## Reference

Datahike pipeline (`replikativ/datahike`):

```
classify (analyze) → LogicalPlan (entity groups) → lower (cost + physical ops)
  → execute (fused scan/merge / hash-probe) → find project
  ↳ on ineligible shapes: relational engine (permanent fallback)
```

## End state

1. **One primary path:** `q` / `q_sources_raw` → compile plan → execute → project.
2. **One fallback:** existing relation / binding interpreter in `query_where.ml`
   (Datahike's "relational engine is permanent fallback").
3. **No** `datascript.ml` `simple_*` shape gates on `q`.
4. **No** dead IR (`left_deep_join` discarded, `RangeScan` never built, `analyze`
   unused at runtime).
5. **No** mid-eval fake `order_where_clauses` with `max_e/8` ratios without a plan;
   ordering happens once in lower, with readiness constraints.
6. Docs (`adr/query-planner.md`, `query_planner_plan.md`,
   `query_implementation_comparison.md`) match the code.

## Phases

### P0 — Remove Datascript query hacks

- Delete `simple_avet_predicate_rows`, `simple_same_entity_constant_rows`,
  `simple_cross_entity_value_join_rows`, `simple_or_join_constant_rows`,
  `simple_not_join_constant_rows`, `simple_single_pattern_rule_rows` and helpers
  only used by them.
- `Datascript.q` → `Query_impl.q` only (plus string parsing wrappers).
- Keep pull/collection helpers for a later pass unless they block simplification.

### P1 — Logical IR (Datahike-shaped)

Replace stub `query_plan.ml` types with:

| Node | Meaning |
| --- | --- |
| `LScan` | Single pattern |
| `LEntityJoin` | Same-entity scans + foldable anti-scans |
| `LFilter` | Comparison / equality / predicate |
| `LBind` | Function binding (passthrough / fallback if unsupported) |
| `LUnion` | `or` / `or-join` |
| `LAntiJoin` | `not` / `not-join` not folded into entity group |
| `LRuleExpand` | Non-recursive rule head → body plan |
| `LPassthrough` | Force relational fallback for that clause |

`build_logical_plan`: classify clauses → group by entity var → fold simple NOT
into anti-scans when non-entity vars are local (Datahike `foldable-not?`).

### P2 — Lower + cost

- Index choice: EAVT / AEVT / AVET from ground components (same rules as Datahike).
- Predicate pushdown onto AVET bounds for comparisons on value vars.
- Within `LEntityJoin`: order legs cheapest-first; driving scan = narrowest.
- Between groups: left-deep order by estimated cardinality.
- Estimates: prefer real index slice width when cheap; else schema/cardinality
  heuristics — **not** fixed `max_datom_e/8` alone.
- Hard readiness: never schedule a filter/join before its vars are produced.
- Ineligible plans return `None` → interpreter fallback.

### P3 — Execute

Physical ops reuse existing operators (no parallel micro-implementations):

- Entity group → `relation_of_same_entity_patterns` / pattern + AVET helpers
- Cross-group → `hash_join`
- OR → `union_relations`
- NOT → `anti_join`
- Non-recursive rules → inline then execute body plan
- Project via `relation_rows_for_find`

Stream where already possible; avoid new `(max_e+1)` value arrays.

### P4 — Wire entry + delete mid-eval reorder

- `Query_api.q_sources_raw`: try `Query_exec.run` first when eligible
  (no aggregates / `:with` / callables / multi-db disjoint / recursive rules).
- Remove `Query_plan.order_where_clauses` calls from
  `eval_relation_from_*` once planner owns ordering.
- Export a small public surface: `analyze` / `explain`-style plan for tests.

### P5 — Review and simplify

- Delete unused IR constructors and duplicated AVET helpers left after P0.
- Collapse mirrored `eval_relation_from_empty` / `_from_relation` if safe.
- Update ADR + comparison docs; drop stale 50k-gate claims.
- Parity: `test_shared_queries`, `test_query_plan`, `dune runtest` query suites.
- Manual/bench spot-check shared suite sizes used in PR #5.

## Non-goals (this pass)

- Full Datahike semi-naive fixpoint / stratum aggregates / SIP / prepared-query cache.
- Porting `execute.cljc` line-for-line (~6k LOC); we match **architecture and
  operator shapes**, implementing execute via OCaml relation primitives.
- Changing public `q` / `q_string` / inputs / rules API.

## Success criteria

- [x] No `simple_*` on `Datascript.q` path
- [x] Planner produces entity-group plans for shared-suite shapes (q1–q-rule)
- [x] Unsupported / NOT-heavy → interpreter source order; results match golden counts
- [x] No dead `left_deep_join` / unused stub `RangeScan` constructors
- [x] Docs describe the live pipeline
- [x] Review pass removed redundant shape gates (~1k LOC in `datascript.ml`)
