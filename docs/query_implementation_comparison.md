# OCaml vs Datahike Query Implementation Comparison

This document compares the OCaml query path to Datahike's compiled planner
(architecture target) and notes the permanent relational fallback.

## Architecture (current)

| | Datahike | OCaml (this repo) |
|---|---|---|
| Primary path | analyze → logical IR → lower → fused execute | `Query_plan.compile` → ordered clauses → relation interpreter operators |
| Logical IR | `LEntityJoin`, `LScan`, `LFilter`, `LUnion`, `LAntiJoin`, … | Same shape in `impl/query_plan.ml` (`LEntityJoin`, `LScan`, …) |
| Fallback | Relational engine (permanent) | `query_where` relation / binding interpreter (permanent) |
| Shape gates on `q` | None (planner or fallback) | **Removed** — `Datascript.q` → `Query_impl.q` only |
| Hot-loop output | Dense tuples / cursors | `{attrs; rows}` relations, then find projection |
| Cost / order | count-slice + DP; readiness constraints | Ground-component estimates + readiness-aware schedule; **source order kept when any `not` is present** (DataScript error parity) |

## Same-entity multi-attr

**Datahike:** entity group + DP merge + index cursors.

**OCaml:** `LEntityJoin` in the planner; execution via `relation_of_same_entity_patterns`
(bitset / lookup). No parallel `simple_same_entity_*` bypass in `datascript.ml`.

## OR / NOT

**Datahike:** `LUnion` / `LAntiJoin`; foldable NOT → anti-scan when safe.

**OCaml:** planner builds `LUnion` / `LAntiJoin`; foldable NOT only when a positive
same-entity scan appears earlier in source order. Queries containing any NOT keep
source clause order so unbound-var errors match DataScript.

## Cross-entity / predicates / rules

- Cross-entity: `hash_join` on relations (no `(max_e+1)` value-array gate).
- Predicates: AVET helpers in `query_where` (`relation_of_avet_value_comparisons`).
- Non-recursive rules: inlined in `Query_plan.compile` and/or `expand_inline_rules`.

## Verification

| Check | Command |
|---|---|
| Result parity | `dune exec -- test/test_shared_queries.exe` |
| Category parity | `dune exec -- test/test_shared_api_parity.exe` |
| Planner unit tests | `dune exec -- test/test_query_plan.exe` |
| DataScript suite | `dune exec -- test/test_datascript.exe` |

See also `docs/datahike-query-alignment.md` and `docs/adr/query-planner.md`.
