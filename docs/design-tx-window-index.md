# Tx-Window Index: Narrow Scans with `t`

Branch: `logseq/tx-window-index-fe5d`  
Builds on: Share_index_db (LMDB / SQLite) + tx-visibility (`as_of` / `since` / `history`)

## Problem

Every datom carries `tx` (`t`), and larger `tx` means a later transaction. Today
`since` / `as_of` only **filter** after a cursor already walks EAVT / AEVT / AVET.

Those indexes sort as:

| Index | Key order |
| --- | --- |
| EAVT | `e → a → v → tx → added` |
| AEVT | `a → e → v → tx → added` |
| AVET | `a → v → e → tx → added` |

`tx` is last. There is **no** seek that means “only datoms with `tx > T`”.
On a large graph, “last N days” still pays full-attr / full-entity scan cost.

Wall-clock time is not `tx` itself: `:db/txInstant` lives on the **transaction
entity** (`e = tx`). N days → a tx lower bound needs that mapping first.

## Goal

Use `t` to **physically bound** how much index we touch, so a time window’s cost
tracks **window size**, not full graph size — for the access paths that opt in.

Non-goals for v1:

- Make every arbitrary Datalog shape automatically fast on a huge live window.
- Replace EAVT/AEVT/AVET.
- Change observable DataScript results for unscoped queries.

## Semantics (pick explicitly)

Two useful meanings of “recent”; the design supports both from one structure.

| Mode | Meaning | Typical product use |
| --- | --- | --- |
| **A. Tx-datom window** | Datoms whose `tx` is in `(tx_lo, tx_hi]` | History / activity / “what changed” |
| **B. Touched-entity window** | Distinct `e` that appear in any datom with `tx` in range, then read **current** facts for those entities from EAVT | “Pages/blocks edited in last N days” |

Mode A is the raw temporal slice. Mode B is usually what Logseq-style UIs want.
`since` today is closer to A applied as a filter on every scan, not B.

## Design

### 1. Resolve wall time → tx range

Keep using `:db/txInstant` on tx entities (already written on `transact` when
`tx_meta` supplies it).

```text
instant_lo  →  smallest tx with txInstant >= instant_lo   (or next tx after)
instant_hi  →  max_tx  (or as_of bound)
```

Implementation options (v1 can be simple):

1. **AVET on `:db/txInstant`** (already indexed if marked `:db/index`) — range
   scan values in `[lo, hi]` → list of tx entity ids. Fine while tx count is
   modest vs datom count.
2. Later: compact **tx-time side table** `instant → tx` if AVET over all txs
   becomes hot.

Cache `(instant_lo → tx_lo)` per db basis (`max_tx`) in the session.

### 2. Fourth index: TEAV (tx-ordered posting)

Add a Share_index DBI / SQLite table alongside EAVT/AEVT/AVET:

```text
TEAV key:  tx | e | a | v | added
TEAV value: same payload as other indexes (or empty if value is fully in key)
```

Codec: same order-preserving components as today; only component order changes.
LMDB and SQLite Share_index both grow one map/table (`ds_teav`).

Write path (same txn as the three indexes):

- On each assert/retract in `tx_data`, also `put` TEAV.
- Purge / remove must delete TEAV keys too.
- `sync_append_since_tx` copies TEAV deltas like the others.

Read API:

```ocaml
val fold_teav_range :
  db -> from_tx:tx -> ?to_tx:tx -> (datom -> unit) -> unit
```

Ascending `tx` order → cheap “everything after `tx_lo`”.

Optional covering variants later (not v1):

- `T-A-E-V` if “recent datoms of attr A” dominates.
- Posting list `tx → [e]` only (smaller) if Mode B is the only consumer.

### 3. Window handle on `db`

Extend the temporal view (compatible with existing fields):

```ocaml
(* conceptual *)
type tx_window = { tx_lo : tx; tx_hi : tx; mode : Tx_datoms | Touched_entities }

val since_window : ?mode:_ -> tx_lo:tx -> db -> db
val since_instant : ?mode:_ -> Instant.t -> db -> db
```

- `since_instant` = resolve instant → `tx_lo`, then `since_window`.
- Existing `since tx` stays; window APIs are additive and document Mode A vs B.

### 4. How queries use the window

Planner / `datoms` do **not** rewrite every clause. They change the **seed set**:

```text
if db has tx_window:
  candidates = TEAV.range(tx_lo, tx_hi)     (* Mode A: datoms *)
            or unique e from that range     (* Mode B: entities *)
  evaluate where-clauses constrained to candidates
else:
  current Share_index path
```

Concrete wiring (v1):

1. **`datoms` / index scans with empty `e`** under Mode A: iterate TEAV range,
   then apply `a`/`v` predicates in memory (or seek within TEAV if we add T-A…).
2. **Mode B**: build `e` bitset/hashset from TEAV range once per query (or cache
   on the windowed `db` value); pattern resolution requires `e ∈ candidates`
   (entity-group / EAVT-by-e stays selective).
3. **Already-bound `e`**: keep EAVT; only check `tx` if Mode A. No TEAV needed.
4. **Full unbound scans** (`[?e :attr ?v]` with no selective const) under a
   window: **prefer TEAV → filter attr** when `|window| << |attr|`; else keep
   AEVT and filter tx (today). Cost model: compare window datom count vs attr
   cardinality estimate.

This is how “use `t` to narrow” becomes real: **unbounded attribute scans become
window scans when a window is set.**

### 5. What stays slow (honest bound)

| Situation | Still expensive? |
| --- | --- |
| Window spans most of the graph | Yes — window ≈ full DB |
| Query result itself is huge inside the window | Yes — output-bound |
| No window set | Same as today |
| Mode B + need all attrs of touched entities | Pays EAVT-per-e (good if few touched ids) |
| Rules / or that escape the candidate set | Must keep candidates threaded; leaks = full scan |

Guarantee we **can** claim:

> With a tx window set, scans that would have been full-index are rewritten to
> TEAV `[tx_lo, tx_hi]` (or entity-restricted EAVT). Cost scales with
> **datoms/entities touched in that tx range**, not total graph size.

Guarantee we **cannot** claim:

> Any Datalog string is always fast for any N and any graph.

## Storage cost

- Extra index ≈ one more copy of every historied datom key (same order as
  append-only growth). Roughly **+25–35%** index bytes vs three indexes only
  (codec-dependent).
- SQLite: one more `WITHOUT ROWID` BLOB table; same write txn.
- Compaction: `purge_history_before` / window eviction must drop TEAV too.

## Phased delivery

| Phase | Deliverable | Gate |
| --- | --- | --- |
| 0 | This design; agree Mode A vs B default for Logseq | Review |
| 1 | TEAV codec + write on LMDB & SQLite Share_index; roundtrip tests | `dune runtest` |
| 2 | `fold_teav_range`; `since_instant` → `tx_lo`; Mode A `datoms` path under window | Temporal + sqlite tests |
| 3 | Mode B candidate entity set; wire into entity-group / unbound attr scans | Bench: 200k graph, 1-day window ≪ full AEVT |
| 4 | Query planner cost hint: choose TEAV vs AEVT when window set | Shared query suite, no result regressions |
| 5 | Optional `T-A-E-V` or tx→e posting if Mode B profiles hot | Microbench |

## Bench protocol (must prove narrowing)

Fixed large DB (e.g. 200k–500k entities), measure:

1. Unscoped `datoms` / AEVT attr scan (baseline).
2. Same op under `since_instant` **without** TEAV (filter-only) — today’s cost.
3. Same op **with** TEAV window rewrite.

Success: (3) ≈ O(window), (2) ≈ O(graph); (3) ≪ (2) when window ≪ graph.
Also track write amp and disk (+TEAV).

## Alternatives considered

| Option | Why not as v1 |
| --- | --- |
| Only improve `since` filter | No seek; does not shrink IO |
| Time-partitioned EAVT files | Strong isolation, heavy ops/rebalance; later |
| Hot DB copy of last N days | Great product UX; duplication + sync lag; can sit **on top** of TEAV |
| Rely on `:block/updated-at` AVET alone | App-level, misses tx semantics / history; complements, does not replace TEAV |

## Compatibility

- Observable unscoped behavior unchanged.
- Existing `since` / `as_of` / `history` remain; window APIs are additive.
- Melange/jsoo: TEAV native-first; JS backends follow or stay filter-only until ported.
- Upstream DataScript has no TEAV; document as dbval/Logseq extension (same class as history).

## API stability: callers must not change

**Constraint (Logseq / app):** keep using `d/q`, `d/datoms`, `d/entity` as today.
No required `since_instant`, Mode B handle, or query rewrite at the call site.

Faster paths are chosen **inside** the engine from the query / db view:

```text
parse where
  → recognize selective shapes (AVET value, AVET time range, bound e, …)
  → pick index + physical op
  → optional: if db already has since_tx / as_of, prefer TAVE only when
    that beats AEVT/AVET for the clause set
```

### Auto-selection rules (aligned with Logseq survey)

| Query / datoms shape (unchanged API) | Prefer | Why |
| --- | --- | --- |
| `[?e :block/uuid u]` / lookup-ref | AVET unique → EAVT | already selective |
| `[?e :block/updated-at ?t] [(>= ?t lo)] …` | **AVET range** on updated-at | Logseq’s real “recent” |
| `[?p :block/journal-day ?d] [(>= ?d lo)]` | **AVET range** on journal-day | journals / between |
| `(between …)` / DSL timestamp between | same AVET ranges (via rules expansion) | query_dsl today |
| Bound `?e` then attrs | EAVT | entity hydrate |
| `[?e :attr]` full presence, huge attr | AEVT (status quo) | no free lunch |
| Db view with `since` / history + unbound attr scan | **TAVE** `tx\|a\|…` when estimated cheaper than AEVT+filter | engine-only; apps need not opt in |
| `d/datoms db :avet :block/updated-at` (+ rseq) | keep AVET | `get-recent-updated-pages` |

Planner cost hint (same as above): for each clause, estimate `|AVET(attr,range)|` vs
`|AEVT(attr)|` vs `|TAVE(tx_lo,attr)|` when a tx lower bound is in scope; pick min.
Wrong choice only affects speed, not results.

### What this means for TAVE

- **Not** a new public “window API” for Logseq UI queries.
- **Yes** a storage/access path the executor may use when:
  1. the db value already carries `since_tx` / history semantics, **or**
  2. future internal sync/history ops need tx-ordered scans,
  and AVET cannot express the bound (no denormalized time attr in the clause).

For shipping Logseq shapes, auto-fast means **recognize AVET time ranges and
bound-entity plans** — that is the win without app changes. TAVE is backup for
tx-scoped views, not a replacement for `:block/updated-at`.

### Non-goals under this constraint

- Requiring apps to call `since_instant` / pass Mode B.
- Changing EDN query syntax for “recent”.
- Silently changing `since` result semantics to Mode B (touched-entity current facts).

Optional later: *internal* connection defaults (e.g. always maintain TAVE) remain
invisible to `d/q` callers.

## Index selection (EAVT / AVET stay primary)

TEAV does **not** retire the other indexes. Selection is per clause / seed:

| Clause shape (under window) | Preferred index | Role of TEAV |
| --- | --- | --- |
| Bound `e` (`[e :a ?v]`, pull, entity) | **EAVT** | None (Mode A: optional tx check) |
| Bound `a` + bound `v` (unique / lookup) | **AVET** | None, unless needing “only if written in window” |
| Bound `a`, unbound `e`/`v`, **no window** | **AEVT** | — |
| Bound `a`, unbound `e`, **window**, `\|TEAV\| ≪ \|A\|` | **TEAV** then filter `a` | Seed / narrow |
| Bound `a`, unbound `e`, **window**, attr tiny | **AEVT** + tx filter | Skip TEAV |
| Mode B after candidate `e` set built | **EAVT** per `e` | Only to build the `e` set once |
| `txInstant` → `tx_lo` | **AVET** on `:db/txInstant` | Bootstrap only |

Invariant: **current-fact identity** always comes from EAVT/AEVT/AVET + existing
tx-visibility/`datoms_filter`. TEAV never answers “what is true now?” alone in
Mode B; it only answers “who/what was touched in this tx range?”.

```text
                    ┌── selective e/v ──► EAVT / AVET (unchanged)
 query + window ────┤
                    └── unselective scan ──► TEAV[tx_lo..] ──► filter / rejoin EAVT
```

## Layering on `since` / `as_of` / `history`

| Mechanism | What it does today | Relation to TEAV window |
| --- | --- | --- |
| `as_of` | Upper bound `tx <= T` on every datom | Still applied; TEAV range uses `to_tx = min(window_hi, as_of)` |
| `since` | Drop `tx <= since_tx` after scan | Window’s `tx_lo` should **subsume** filter when TEAV path is used; filter remains as safety on EAVT paths |
| `history` | Skip cancel of add/retract | Mode A natural fit; Mode B should use **non-history** EAVT for “current entity state” |
| `filter` pred | Extra datom predicate | Unchanged; runs after index choice |

Recommended stacking:

```text
db0 = conn.db
db1 = as_of? tx_hi db0          (* optional snapshot *)
db2 = since_instant ~mode:B t0 db1
q query db2                     (* planner sees window on db2 *)
```

Do **not** silently reinterpret plain `since` as Mode B — that would change
results (Mode B returns current attrs of touched entities, including attrs last
written *before* the window). Mode A ≈ today’s `since` semantics with a faster
physical path; Mode B is a **different product API**.

## Candidate discipline (Mode B)

Once `E_touched = unique e in TEAV[tx_lo, tx_hi]` (excluding pure tx entities if
desired):

1. Every pattern that produces new `?e` must intersect `E_touched`, **or** be a
   join from an already-bound var that itself descends from that set.
2. Rules / `or` / `or-join`: expand inside the same candidate constraint; if a
   branch cannot honor it, either reject planning for window (fallback warning)
   or evaluate branch then intersect — never full-DB seed.
3. Reverse refs (`[_ :block/refs e]`): still AEVT/AVET, but result `e` filtered to
   candidates when the query is window-scoped “touched pages” style.
4. Cache `E_touched` on the windowed `db` value (O(window) build once per basis).

Leakage = accidental full AEVT scan = design bug, not “planner best effort”.

## Product-shaped API (sketch only)

```text
;; Mode B — default for “recent work”
(d/q query (d/since-instant #inst "..." db))

;; Mode A — activity / history slice (same facts as filtered since, faster path)
(d/q query (d/since-instant db #inst "..." {:mode :tx-datoms}))

;; Explicit tx bound when caller already mapped time → tx
(d/since-tx-window db tx-lo {:mode :touched-entities})
```

Logseq layer can keep `:block/updated-at` for UX sorting; TEAV window is the
**engine** bound. App attr indexes remain complementary, not a substitute for
transaction-scoped narrowing.

## Cost model (planner hint)

Rough compare for “scan attr `A` under window”:

```text
cost_aevt  ≈ |datoms(A)|           // + cheap tx filter if since set
cost_teav  ≈ |datoms in window|    // + filter a = A
pick min
```

Estimates: maintain `max_tx`, optional running `datoms_in_recent_tx` sketch, or
sample TEAV first page. Wrong choice only hurts constant factors when sizes are
close; correctness unchanged.

## Open decisions (need agreement before code)

1. **Default mode** for `since_instant`: B (product) vs A (semantic continuity with `since`).
2. **TEAV fullness**: full datom keys vs `tx → e` posting only (B-optimized, A needs extra joins).
3. **Tx entities in Mode B**: include `e = tx` rows or strip them from `E_touched`.
4. **Mandatory txInstant**: require stamp on every tx for wall-clock windows, or allow tx-only API.
5. **SQLite + LMDB together in v1** vs native LMDB first.
6. Whether plain `since` gains TEAV acceleration (Mode A path) in the same phase as Mode B API.

## Logseq production query survey (2026-08-29)

Shallow clone of `logseq/logseq` (`3e85583`). Scope: `deps/db`, `deps/outliner`,
`src/main` production code (not tests).

### Access mix (approx)

| Mechanism | ~count | Index fit |
| --- | ---: | --- |
| `d/entity` | 600+ | EAVT (bound entity) |
| Lookup-ref `[:block/uuid …]` | ~250 | unique → EAVT |
| `d/datoms :avet` | ~100+ | AVET |
| `d/datoms :eavt` | ~50–60 | EAVT |
| `d/q` | ~50 | mixed |
| `d/datoms :aevt` | handful | AEVT (rare) |

Hot attrs (schema-indexed): `:block/uuid`, `:block/tags`, `:block/name` /
`:block/title`, `:block/parent`, `:block/page`, `:block/refs`, `:block/alias`,
`:block/journal-day`, `:block/created-at`, `:block/updated-at`.

### How “recent” is done today (not via `tx`)

| Intent | Mechanism | File |
| --- | --- | --- |
| Recent pages | `d/datoms :avet :block/updated-at` + `rseq` + take 15 | `deps/db/.../initial_data.cljs` `get-recent-updated-pages` |
| Latest journals | `d/datoms :avet :block/journal-day` + `rseq` | same file `get-latest-journals` |
| Journal by day | AVET exact `:block/journal-day` | `deps/db/src/logseq/db.cljs` |
| DSL `(between …)` | journal-day rule **or** `created-at`/`updated-at` range | `query_dsl.cljs`, `rules.cljc` `:between` |
| Doing last 14d / Todo next 7d | `(task …)` + journal-day window | `frontend/state.cljs` default journal queries |
| View sort | default `:block/updated-at` desc via AVET | `common/view.cljs` |

**Not found** in production Datalog: `:db/txInstant`, DataScript `since`, or
“datoms since tx T” shapes. Sync `:since` is protocol-level, not an index scan.

### Implications for TAVE

| Workload | Needs TAVE? |
| --- | --- |
| Recent pages / journals / `(between updated-at)` | **No** — denormalized time on **AVET** already |
| UUID / name / tags / parent / page / refs | **No** — unique / AVET / EAVT |
| Bound hydrate (`d/entity`) | **No** — EAVT |
| “All assertions of attr A in tx window” / audit without `updated-at` | **Yes** — only if product moves to tx-log time |
| Sync/history deltas by transaction | **Yes** (future) |

**Conclusion for Logseq as it ships today:** keep investing in **AVET** (time attrs,
tags, journal-day) and **EAVT** entity paths. TAVE is justified for engine-level
tx-window scans and history, not for replacing `:block/updated-at` recent-page UX.

If implementing TAVE anyway, prioritize engine/sync use cases; do not expect
Logseq’s current “recent N days” queries to switch to it without an app rewrite.
