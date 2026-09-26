# Tx-Filter Index Design (dbval-style)

Branch: `logseq/tx-filter-history-fe5d`  
Builds on: `logseq/non-pss-lmdb-fe5d` (LMDB overlay optimizations, PR #2)

## Goal

Remove the LMDB in-memory overlay (`additions` / `removals` / `bulk`) and replace it with an
append-only datom store plus **transaction visibility filters** on read, matching the dbval model.
Expose dbval-compatible `history`, `as_of`, `since`, `basis_tx`, `as_of_t`, `since_t`, and `temporal_view` on the public API.

## Current model (to remove)

```
Index.t = LMDB + overlay lists
  add/remove  → mutate overlay (O(1))
  read        → merge LMDB cursor + overlay hashtables
  snapshot_db → O(1) handle copy (no overlay)
  store       → append tx batch + meta update (sync_append_since_tx for delta copy)
```

## Target model

```
Index.t = LMDB append-only (keys include tx + added flag in value)
  add/remove  → append assert/retract datoms at new tx (no key delete)
  read        → cursor scan + tx-visibility + datoms-filter
  snapshot_db → O(1) handle copy (overlay copy until append-only migration)
  store       → append tx batch + meta update (no full rewrite)
```

Reference: dbval `tx-visibility-xform` and `datoms-filter` in `dbval.db`.

## DB view fields

Extend `db` with dbval-compatible view fields:

| Field | dbval equivalent | Meaning |
| --- | --- | --- |
| `max_tx` | `max-tx` | Basis: upper bound for reads (`tx <= max_tx`) |
| `store_max_tx` | store `q-max-tx` | Committed store high water (for `as_of` validation) |
| `as_of_tx` | `as-of-tx` | Set by `as_of`; marks temporal view |
| `since_tx` | `since-tx` | Set by `since`; lower bound (`tx > since_tx`) |
| `history` | `history?` | Skip `datoms-filter` when true |

Public API (matches dbval.core):

- `basis_tx db` → `max_tx`
- `as_of tx db` → `{ max_tx = tx; as_of_tx = Some tx }`
- `as_of_t db` → `as_of_tx`
- `since tx db` → `{ since_tx = Some tx }`
- `since_t db` → `since_tx`
- `history db` → `{ history = true }`
- `temporal_view db` → read-only guard (as-of / since / history)
- `history_datoms_since checkpoint db` → TAVE history log with `tx > checkpoint`
  (retractions included). Composition of the view APIs:
  `datoms (history (since checkpoint db)) Tave`. This is the sync/delta helper;
  callers should not re-query `history` with application tx metadata.

Transact rejects temporal views with dbval-compatible error message.

## Purge (compatible excise)

Physical removal of datoms from current **and** history (GDPR-style), unlike retract:

| Op | EDN | Effect |
| --- | --- | --- |
| `Purge` | `[:db/purge e a v]` | Remove one fact from all indices |
| `PurgeAttr` | `[:db.purge/attribute e a]` | Remove all values of attr on entity |
| `PurgeEntity` | `[:db.purge/entity e]` | Remove entity + incoming refs + components |

Implementation searches the history view (`history = true`, no `datoms-filter`), then
`Index.remove` deletes keys from EAVT/AEVT/AVET. Persistent storage sync uses
`sync_removals_to_storage` on `transact`. Purge does not append to `tx_data`.

## Read pipeline

For ascending index scans:

1. LMDB cursor over key range
2. Decode datom; drop if `d.tx > max_tx` (basis)
3. Drop if `since_tx` set and `d.tx <= since_tx`
4. Unless `history`, run `datoms_filter` (cancel add/retract pairs in stream order)
5. Apply `filter_pred` if set
6. Apply query component filters (`?e`, `?a`, …)

`datoms_filter` follows dbval semantics: consecutive datoms with same `[e,a,v]` cancel
when a retract follows an add; same-tx add/retract pairs cancel; orphaned retracts are dropped.
LMDB keys include `[tx, added]` after index components so add/retract pairs for the same fact
sort adjacently (assert before retract at the same tx).

## Write pipeline

### transact

1. `db_before = snapshot_db db` → `{ db with view_tx = db.max_tx }` (no index copy)
2. Apply tx ops; collect `tx_data` (full assert/retract log)
3. `db_after`: append all `tx_data` to three indexes; bump `max_tx`; refresh attr caches
4. `persist_transact`: append-only store write

Reject transact on temporal views (`temporal_view` / as-of / since / history).

### init / bulk load

Single-tx bulk append (`of_bulk` → direct LMDB write batch). No overlay staging.

## Storage

  - **store**: append new datoms for the tx + update meta (`max_tx`, `max_eid`, schema)
  - **restore**: open LMDB env, read meta, rebuild attr caches from filtered scan at `max_tx`
  - Storage sync uses `sync_append_since_tx` for delta copy when session and storage envs differ

PSS tail replay (`impl/storage_pss.ml`) is the closest in-repo precedent for append-only persistence.

## Phased migration

| Phase | Deliverable |
| --- | --- |
| 1 | Design doc, `db` view fields, `tx_visibility` module, public API stubs, unit tests for filter |
| 2 | Wire visibility filter into `datoms` / `fold_datoms` read paths |
| 3 | Append-only index writes; delete overlay types and merge logic |
| 4 | O(1) `snapshot_db`; transact/store append-only |
| 5 | Full `history` / multi-tx storage roundtrip tests |
| 6 | Benchmark regression check; melange/jsoo sync |

## Risks

- **Performance**: per-read `datoms-filter` cost vs current overlay merge; mitigate with current-fact
  projection cache or lazy filter on slices.
- **Storage growth**: append-only history requires compaction strategy (future work).
- **Attr caches**: `aevt_by_attr` / AVET entity-id maps must be rebuilt or incrementally updated
  from filtered current facts, not raw index contents.
- **Melange**: native index changes must be mirrored in `lmdb/melange/`.

## Compatibility

- `:db/noHistory` schema attrs: retractions still append; filter rules discard prior asserts.
- `?tx:` on `datoms`: exact-tx filter within the resolved stream.
- Upstream DataScript has no full `history` in the checked-out revision; we implement dbval-grade
  time travel as an extension documented here.
