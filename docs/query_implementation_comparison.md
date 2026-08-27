# OCaml vs Datahike Query Implementation Comparison

This document compares how the shared Datahike benchmark queries are executed in
Datahike (compiled planner) versus this OCaml port (interpreter + shape gates).
It explains **structural** differences—not per-query fast paths—and lists
allocation and algorithm gaps to close in the general executor.

## Architecture

| | Datahike | OCaml (this repo) |
|---|---|---|
| Default path | Compile → logical plan → cost-based order → fused execute | `eval_clauses` / `eval_relation_rows` interpreter |
| Shape recognition | Generic planner (entity group, OR, hash-probe) | Ad hoc gates in `impl/datascript.ml` + `relation_of_*` in `impl/query_where.ml` |
| Hot-loop output | `ArrayList`, `object[]` tuples, PSS cursors | `query_result list list`, `(string × query_result) list` bindings |
| Index walk | Cursor `lookupGE` / prefix slice, no full relation | `Seq.t` / `List.t`, often `List.of_seq` materialization |
| Cost model | `count-slice` + Selinger DP | Source order / smallest-constant heuristic |

Reference: Datahike `doc/query-engine.md`, `execute.cljc`, `plan.cljc`.

## Same-entity multi-attr (q-5-merge, q3, q4)

**Query shape:** `[?e :name ?n] … [?e :sex :male]` — one entity var, mix of free vars and constants.

### Datahike

1. Groups clauses into one `:entity-group` on `?e`.
2. Picks driving scan by cost (e.g. `:sex :male` ~50% selectivity).
3. For each surviving entity: **in-index `lookupGE`** on EAVT/AEVT for each remaining attr.
4. Emits tuples directly into pre-sized arrays; no `{attrs; rows}` relation.

### OCaml today

Two overlapping implementations:

1. **`simple_same_entity_constant_rows`** (`impl/datascript.ml`) — bypasses `Query_impl.q` when
   `max_datom_e ≤ 50_000` and `:in`/rules empty.
2. **`relation_of_same_entity_patterns`** (`impl/query_where.ml`) — relation fast path inside
   `eval_relation_rows`.

Both use entity bitsets for constant intersection. Same-entity queries with constants use the
Datahike entity-group pattern: constant slice → candidate entities → in-index lookup per
value attr. No `(max_e+1)` value arrays.

- **`simple_same_entity_constant_rows`:** caches `aevt_by_attr` arrays, then
  `find_entity_in_aevt_array` (binary search on entity id) for each candidate × value attr.
- **`find_datom` / `find_primary_aevt_entity_attr`:** fast Aevt `~e ~a` point reads without
  Seq materialization.
- **`relation_of_same_entity_patterns`:** driver scan + lookup when multiple value patterns
  and constants (general-path fallback).

**Gap:** Driver attr in the relation path is still the first value pattern, not cost-based.

## OR / NOT (q-or, q-not)

### Datahike

- `(or …)` → `:or` op; each branch is an independent sub-plan.
- Union at **relation** level (`rel/sum-rel`); `limit-context` avoids Cartesian growth.

### OCaml

- `eval_relation_rows` / `eval_relation_from_empty` union OR branches via `union_relations`
  (relation-level, Datahike `sum-rel` style).
- `eval_clauses` on embedded `(Or branches)` still uses binding `List.concat_map` for non-relation
  query shapes.

**Gap:** OR inside larger clause lists (not Or-only relation queries) still round-trips bindings.

## Cross-entity / value join (q5)

### Datahike

- Hash-probe between entity groups; producer builds probe-set of join values; consumer
  scan filtered during iteration.

### OCaml

- Sequential `hash_join` on materialized `{attrs; rows}` relations.
- `hash_join` copies rows (`left_row @ right_row`), uses `List.mem` for attr intersection.

**Gap:** Full relation materialization before join; row copying on every match.

## Predicates / AVET range (qpred*, q-pred-range)

### Datahike

- Comparison pushdown to AVET encoded bounds; strict int ranges skip post-filter.

### OCaml

- `relation_of_avet_value_comparisons` + fast path in `simple_avet_predicate_rows`.
- General path may still materialize all range datoms then filter.

**Gap:** Per-iteration full row lists in benchmark loop (documented in `query_planner_plan.md`).

## Rules (q-rule)

### Datahike

- Non-recursive rule heads expanded at plan time → single pattern scan on rule body.

### OCaml

- Runtime `rule_invocation_binding` + body re-eval through `eval_clauses`.
- Recent shortcut in `simple_follow_rule_rows` duplicates planner inlining for one shape only.

**Target fix (Phase 0 plan):** Inline non-recursive rule bodies into relation clauses in
`eval_relation_rows`, not only in `datascript.q` fast paths.

## Bindings and lists (all queries)

| Pattern | Location | Cost |
|---|---|---|
| `(string × query_result) list` bindings | `impl/query.ml` `bind_var` | O(n) `List.assoc_opt` per match |
| `List.concat_map` sequential clauses | `eval_sequential` | New list per clause × binding count |
| `List.of_seq` on every pattern match | `match_query_source_pattern` | Full materialization of index slice |
| `List.sort_uniq compare` on results | `query_api.ml` `q_sources_raw` | Even when rows already unique / ordered |
| `group_by_key` | `impl/query.ml` | O(n²) via `List.remove_assoc` |
| `hash_join` attr overlap | `List.mem` on attr names | Quadratic in attr count per join |

**Target fixes (general executor):**

1. Fast `bind_var` when `left = right` before `query_results_equivalent`.
2. Propagate `unique_rows` from relation eval to skip final sort.
3. `Hashtbl` for `group_by_key` and join attr sets.
4. Fold-based pattern matching API to avoid `List.of_seq` in sequential eval.

## Fast paths vs general path

Current `datascript.q` tries six shape gates before `Query_impl.q`. These are useful for
parity work but **do not replace** a compiled executor:

- Large DBs no longer bypass the same-entity fast path solely on `max_datom_e`; lookup
  strategy avoids `(max_e+1)` arrays when the graph is large.
- Duplicated logic between `datascript.ml` and `query_where.ml` drifts (e.g. value tables).
- Benchmark wins on q5/q-or/q-rule came from bypassing the interpreter, not fixing it.

Roadmap: `docs/query_planner_plan.md` (Phases 0–4). Phase 0 = allocation + bounds fixes;
Phases 1–3 = plan IR, cost ordering, streaming operators matching Datahike's entity-group
and OR union semantics.

## Verification

| Check | Command |
|---|---|
| Result parity | `dune runtest test/test_datahike_queries.ml` |
| vs Datahike timing | `./bench/compare_ocaml_datahike.sh 2000 [QUERY]` |
| General path only | Temporarily disable fast paths or use `max_datom_e > 50_000` test DB |

When optimizing, measure both **single-query** compare and **full suite**, and confirm
counts match Datahike golden values (size=2000, seed=1).
