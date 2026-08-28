# Datahike vs OCaml Query Implementation Comparison

Reference clone: `_deps/datahike` (replikativ/datahike, shallow clone for local diff).
Upstream doc: `_deps/datahike/doc/query-engine.md`.

Observable **results** must stay DataScript-compatible. **Execution architecture**
should follow Datahike's compiled planner + permanent relational fallback.

## Pipeline mapping

| Phase | Datahike | OCaml (this repo) | Gap |
| --- | --- | --- | --- |
| Entry | `datahike/query.cljc` → `q` / `execute-planned-direct` | `datascript.ml` → `Query_impl.q` → `query_api.ml` `q_sources_raw` | OK (no `simple_*` on `q`) |
| Classify | `query/analyze.cljc` `classify-clause` | Inline in `query_plan.ml` / `query_where.ml` pattern parsing | No dedicated analyze module |
| Logical IR | `query/logical.cljc` `build-logical-plan` | `query_plan.ml` `build_logical_plan` | Same node shapes (`LEntityJoin`, `LScan`, …) |
| Lower | `query/lower.cljc` + `query/plan.cljc` | `query_plan.ml` `lower` / `compile` | **Major**: DH uses DP merge + pipeline DSL; OCaml flattens to clause list |
| Execute | `query/execute.cljc` fused scan+merge, probe-map joins | **Missing** `query_exec.ml`; execution lives in `query_where.ml` | **Major**: no cursor merge, no `PPipeline` |
| Fallback | `query/relation.cljc` + `query.cljc` `execute-legacy` | `query_where.ml` relation interpreter | Permanent fallback — correct role, but also hosts fast paths |
| Project | find projection in execute / query | `query_api.ml` `relation_rows_for_find` | OK |

Datahike end-to-end:

```
analyze → logical.cljc → lower.cljc → execute.cljc → find project
                              ↳ ineligible → relation.cljc (legacy)
```

OCaml today:

```
query_plan.compile → clauses_of_plan → query_where (fused kernels + interpreter)
         ↳ try_fast_empty_relation_rows (pre-planner bypass)
         ↳ relation_of_same_entity_patterns (dense AEVT gather)
         ↳ eval_relation_from_empty (hash_join chain)
```

The planner IR **matches** Datahike; the **execute layer does not**.

## Module-by-module notes

### `analyze.cljc` (Datahike)

- Classifies each clause: `:pattern`, `:predicate`, `:function`, `:not`, `:or`, …
- Extracts vars, checks fn args, handles quote forms.
- **OCaml**: scattered across `Query.pattern_scan`, `query_plan.pattern_scan`, `query_where` clause walks. No single classify API.

### `logical.cljc` (Datahike)

Key behaviors (see `build-logical-plan`):

1. Classify all clauses → `LScan` / `LFilter` / `LBind` / …
2. Group scans by `[entity-var, source]` → `LEntityJoin`
3. **Foldable NOT** (`foldable-not?`): single-pattern NOT on grouped entity, non-entity vars local to negation → **anti-scan inside entity group**
4. Remaining NOT → `LAntiJoin`
5. OR / rules → `LUnion` / `LRuleCall` / `LFixpoint`

**OCaml** (`query_plan.ml` `build_logical_plan`):

- Same grouping and foldable-NOT idea (`foldable_not_scan`).
- Extra constraint: fold only if positive scan **earlier in source order** (DataScript outer-binding errors).
- Does **not** tag nodes with `:source-idx` for bound-var-card propagation (Datahike lower uses this).

### `plan.cljc` + `lower.cljc` (Datahike)

Physical planning primitives:

| Primitive | Purpose |
| --- | --- |
| `plan-pattern-op` | Index choice (EAVT/AEVT/AVET) + pushdown bounds |
| `dp-order-fuse-ops` | Optimal scan + merge order within entity group |
| `assemble-entity-group` | `:entity-group` op + `build-pipeline` |
| `detect-inter-group-joins` | Shared value vars → hash-probe plan |
| `dp-order-groups` / `order-plan-ops` | Inter-group order + readiness |

Lower produces ops like:

```clojure
{:op :entity-group
 :scan-op {... :index :aevt ...}
 :merge-ops [{:join-method :lookup ...} ...]
 :pipeline {:path :sorted-merge :steps [...]}}
```

**OCaml** (`query_plan.ml`):

- `OpEntityGroup { clauses; estimated_rows }` — **only clause list**, no scan/merge split, no pipeline.
- `lower` schedules ops by heuristic cost; `clauses_of_plan` **discards physical structure**.
- Index choice exists (`choose_index`) but is not consumed by a fused executor.

### `execute.cljc` (Datahike)

Core execution paths:

1. **`execute-group-direct`** — entity group fused scan:
   - Pick driving scan (lowest cardinality after DP)
   - Walk index slice; for each datom, **seekGE** merge lookups (no intermediate relations)
   - Paths: `:scan-only`, `:sorted-merge`, `:per-cursor-merge`, `:card-many-merge`
2. **Anti-merge** — during merge loop, skip entities matching anti-scan attr/value
3. **Multi-group** — producer probe-set / probe-map → consumer filtered scan
4. **Post-filter / post-apply** — wide tuples then project to find-vars

**OCaml** (`query_where.ml`):

- `relation_of_same_entity_patterns` — materializes `{ attrs; rows }` lists
- Dense AEVT gather (`try_same_entity_constant_dense_rows`, `try_fast_empty_relation_rows`) — **ad hoc**, not driven by `OpEntityGroup` / pipeline
- `hash_join` on relations — correct fallback shape, not cursor merge
- NOT: bitset exclusion scan OR `anti_join` on relations

### `relation.cljc` (Datahike fallback)

- Tuple relations, `hash-join`, `sum-rel`, `subtract-rel`
- Used when planner ineligible or `*disable-planner*`

**OCaml**: same concepts in `query_where.ml` (`hash_join`, `anti_join`, `union_relations`).

## Shared bench queries — shape-by-shape

Queries from `bench/shared_query_bench.ml`.

### q1 — `[:find ?e :where [?e :name "Ivan"]]`

| | Datahike | OCaml |
| --- | --- | --- |
| Logical | `LScan` (ground value → AVET) | `LScan` → `OpScan` or single-pattern group |
| Execute | AVET slice or EAVT seek; **no relation alloc** | AVET ids or AEVT scan → relation rows |
| Gap | Direct emit to result set | Extra `{attrs;rows}` wrapper |

### q2 — `[:find ?e ?a :where [?e :name "Ivan"] [?e :age ?a]]`

| | Datahike | OCaml |
| --- | --- | --- |
| Logical | `LEntityJoin` with 2 scans | Same |
| Lower | `assemble-entity-group`: DP picks scan (`:name` selective) + merge `:age` via **lookupGE** | `OpEntityGroup` → flat clauses → `try_fast_*` or `relation_of_same_entity_patterns` |
| Execute | **Fused sorted-merge** — one pass, no hash join | Dense AEVT index gather OR hash_join two relations |
| Perf | ~0.6 ms (20k entities, DH bench doc) | ~0.009 ms (2k entities) vs **0.004 ms** pre-removal gate |

Root cause of OCaml gap: execution still **materializes row lists** and duplicates kernel logic outside the planner op stream.

### q-5-merge — five attrs + `[?e :sex :male]`

| | Datahike | OCaml |
| --- | --- | --- |
| Logical | `LEntityJoin` 5 scans + constant on `:sex` | Same |
| Execute | DP order: selective constant/attr as scan, merges via cursor | Const-first aligned AEVT gather (4 value vars) |
| DH doc | "5-clause entity merge" **2.4 ms** @ 20k | **0.046 ms** @ 2k vs **0.030 ms** baseline |

Datahike uses **merge ordering + seekGE**, not "all arrays aligned then index by entity id".

### q-not / q-not-join — `[?e :age ?a] (not [?e :sex :male])`

| | Datahike | OCaml |
| --- | --- | --- |
| Logical | Foldable NOT → **anti-scan** inside `LEntityJoin` on `?e` | Same fold in `build_logical_plan` |
| Execute | Anti-merge during fused scan (skip excluded entities) | `try_not_single_value_aevt_scan` / bitset + full AEVT walk |
| Planner | NOT present → still plans positive leg | **`plan_ordered_clauses` skips compile when any NOT** — source order only |
| DH doc | NOT **3.8 ms** @ 20k | **0.025 ms** @ 2k vs **0.023 ms** baseline |

OCaml NOT path never uses planner ordering; anti-scan is reimplemented in fallback, not as merge op.

### q-or-join, q-rule

| Query | Datahike | OCaml |
| --- | --- | --- |
| q-or-join | `LUnion` → branch execute → combine | `eval_or_branch_relations` / union |
| q-rule | `LRuleCall` → expand → plan body | `try_single_pattern_rule_rows` + inline rules |

## What is wrong with current `query_where.ml` complexity

These are **execute-layer** concerns implemented inside the **fallback module**:

| Mechanism | Lines (approx) | Datahike equivalent |
| --- | --- | --- |
| `try_fast_empty_relation_rows` | ~350 | Should not exist — `execute.cljc` `execute-group-direct` |
| `try_same_entity_constant_dense_rows` | ~150 | `assemble-entity-group` + `execute-sorted-merge` |
| `rows_from_dense_aevt_gather` | ~200 | Pipeline `PIndexScan` → `PSortedMerge` → `PEmitTuple` |
| `same_entity_fused_relation` | wrapper | `OpEntityGroup` execution |
| `relation_of_same_entity_patterns` | ~1300 | Split: lower produces ops, execute consumes ops |

Adding more special cases in `query_where` **diverges further** from Datahike. The alignment doc (`docs/datahike-query-alignment.md` P3–P4) already says physical ops should drive execution.

## Recommended refactor (Datahike-faithful)

### 1. Add `impl/query_exec.ml` (execute layer)

```ocaml
val run :
  db -> physical_plan -> query_source -> bindings ->
  (string list * query_result list list * bool) option
```

Implement op dispatch matching Datahike:

- `OpEntityGroup` → fused entity-group execute (port `execute-group-direct` / `execute-sorted-merge` using existing `aevt_attr_array`, `entity_ids_array_by_attr_value`, index seeks)
- `OpScan` → single pattern scan
- `OpUnion` → `union_relations`
- `OpAntiJoin` → `anti_join`
- `OpFilter` → filter relation or in-group attached pred
- `OpPassthrough` → return `None` (fallback)

Move dense gather / bitset NOT / aligned multi-attr logic **into** entity-group execute keyed by pipeline path — delete `try_fast_*`.

### 2. Extend physical IR (minimal)

Extend `OpEntityGroup` to carry what lower already knows:

```ocaml
| OpEntityGroup of {
    entity_var : string;
    scan : l_scan;           (* driving pattern *)
    merges : l_scan list;    (* DP-ordered *)
    anti_scans : l_scan list;
    filters : query_clause list;
    index : index_choice;
    ...
  }
```

Stop flattening to `clauses` in `clauses_of_plan` for execution (keep flatten for tests/explain only).

### 3. Wire entry (`query_api.ml`)

```ocaml
match Query_plan.compile db.max_datom_e [] [] where with
| Some plan when Query_plan.plan_is_executable plan ->
  (match Query_exec.run db plan default_source bindings with
   | Some result -> ...
   | None -> fallback interpreter)
| None -> fallback interpreter
```

Remove `try_fast_empty_relation_rows` bypass from `eval_relation_rows`.

### 4. Keep `query_where.ml` as fallback only

- `eval_relation_from_empty` / binding interpreter
- `hash_join`, `anti_join`, `union_relations`, `relation_of_pattern`
- Source-order NOT for DataScript error parity
- **No** bench-shaped dense kernels at module top level

### 5. Port planning primitives incrementally

Priority for bench perf:

1. `dp-order-fuse-ops` (scan + merge order within group) — `plan.cljc:531`
2. `assemble-entity-group` + `build-pipeline` — `plan.cljc:831`, `:617`
3. `execute-sorted-merge` — `execute.cljc:1375` (card-one attrs, dense DBs)
4. Anti-merge in merge loop — NOT as separate full-DB bitset scan
5. Count-slice estimates — `estimate.cljc` (replace `max_e/8` heuristics)

## File reference index (Datahike)

| File | LOC (approx) | Read first |
| --- | --- | --- |
| `doc/query-engine.md` | 523 | Architecture overview |
| `src/datahike/query/ir.cljc` | 172 | IR + pipeline record defs |
| `src/datahike/query/analyze.cljc` | large | Clause classification |
| `src/datahike/query/logical.cljc` | 453 | `build-logical-plan`, NOT fold |
| `src/datahike/query/plan.cljc` | 1860 | DP merge, entity group, ordering |
| `src/datahike/query/lower.cljc` | medium | Logical → physical |
| `src/datahike/query/execute.cljc` | 6500+ | Fused scan, probe joins |
| `src/datahike/query/relation.cljc` | 300 | Fallback relations |
| `src/datahike/query.cljc` | 5300+ | Entry, planner eligibility |

## Immediate action items

1. **Stop expanding** `try_fast_empty_relation_rows` / `relation_of_same_entity_patterns` special cases.
2. **Implement** `query_exec.ml` with `OpEntityGroup` fused path for q1/q2/q-5-merge shapes.
3. **Extend** `query_plan.ml` `OpEntityGroup` to retain scan/merge structure (mirror `assemble-entity-group`).
4. **Delete** redundant dense kernels once execute path covers bench suite.
5. **Verify**: `test_shared_queries` + `shared_query_bench --size 2000` vs `3547876` baselines.

## Local clone usage

```bash
# Already cloned (gitignored)
ls _deps/datahike/src/datahike/query/

# Diff logical IR grouping
diff -u \
  <(rg -n 'LEntityJoin|foldable-not' _deps/datahike/src/datahike/query/logical.cljc) \
  <(rg -n 'LEntityJoin|foldable_not' impl/query_plan.ml)
```
