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

## Decision defaults (proposed)

1. Ship **TEAV** as the mechanism.
2. Public helper **`since_instant`** defaults to **Mode B** for app queries; Mode A
   available for history/activity.
3. Keep EAVT/AEVT/AVET authoritative for current-fact identity; TEAV is a
   **covering access path**, not a second source of truth.
