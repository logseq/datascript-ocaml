# Plan: SQLite Index Without LMDB

Branch: `logseq/sqlite-without-lmdb-fe5d`  
Builds on: `logseq/shared-api-parity-fe5d` (non-PSS Index API, storage protocol, SQLite BLOB tables)  
References: current LMDB Index (`lmdb/native/`), `docs/design-non-pss.md`, `docs/design-tx-filter-history.md`, vendor dbval `ITupleStore` / `dbval.store.sqlite`

## Goal

Make the SQLite backend a **first-class index engine**: open a SQLite file, transact, query, and restore **without linking or loading LMDB**, and without a temp LMDB mirror for runtime reads.

Today SQLite is only a durable mirror of LMDB-codec keys; the live `db` indexes always run on a temporary LMDB env (`Separate_index_db`). That coupling is the root problem this plan removes.

## Current state

### Runtime path (SQLite)

```
open SQLite file
  → register Separate_index_db backend
  → Index.create_lmdb / create_index_db allocates temp LMDB
  → load_indexes_from_storage: copy_indexes_to_lmdb (full table scan → LMDB put)
  → queries/writes hit LMDB Index
  → sync_indexes_to_storage: sync_append_since_tx (LMDB fold → SQLite REPLACE)
  → sync_removals_to_storage: DELETE keys in SQLite
```

Relevant code:

| Layer | Role today |
| --- | --- |
| `sqlite/datascript_sqlite_db.ml` | Tables `ds_eavt` / `ds_aevt` / `ds_avet` / `ds_meta`; BLOB PK + value; fold / prefix / range helpers |
| `sqlite/datascript_storage_sqlite.ml` | Meta + `copy_indexes_to_lmdb` + `sync_append_since_tx` + removals |
| `sqlite/datascript_storage_sqlite_plugin.ml` | `index_db = Separate_index_db`; sync/load callbacks typed on LMDB |
| `storage/.../datascript_storage_protocol.mli` | `Share_index_db of Datascript_lmdb_db.t` \| `Separate_index_db`; sync/load take `Datascript_lmdb_*` |
| `impl/index.mli` | Abstract API, but names/`type lmdb` and `create_lmdb` assume LMDB |
| `lmdb/native/datascript_lmdb_index.ml` | Full Index implementation (write txn, slice, rslice, seek, …) |
| `lmdb/datascript_lmdb_codec.ml` | Shared order-preserving key/value encoding (already used by SQLite) |
| `sqlite/dune` | `datascript-ocaml-native-sqlite` links `lmdb_*` + `datascript_lmdb_codec` |

### What already works for a SQLite Index

- Same codec as LMDB (`Datascript_lmdb_codec`) → byte order matches `Util.compare_datom`.
- Per-index BLOB tables with PK on key (ordered scans via `ORDER BY key`).
- Write txn (`BEGIN IMMEDIATE` / `COMMIT`), put/remove, meta store/restore.
- Forward range/prefix fold (`fold_index_range_until`, `fold_index_prefix`).

### Gaps vs LMDB Index

- No `Datascript_sqlite_index` implementing `Index` (slice / seq / rslice / seek / append_tx_data / …).
- No reverse range SQL (`ORDER BY key DESC`) — required for `rslice_seq`.
- Storage protocol hard-codes LMDB types in `Share_index_db`, `sync_*`, `load_*`, `create_index_db`, `db_for_storage`.
- Package and core native path still assume LMDB exists even for “SQLite-only” use.

## dbval reference (what to take / what not to copy)

dbval’s store is a thin ordered KV API (`vendor/dbval/src/dbval/store.clj`):

```clojure
(defprotocol ITupleStore
  (-scan [store begin end reverse?])   ; begin <= k < end, unsigned byte order
  (-commit! [store keys blobs])        ; atomic batch insert (append-only)
  (-get-blob [store hash])
  (-close! [store]))
```

SQLite impl (`dbval.store.sqlite`):

- Single key table `dbval (k BLOB PRIMARY KEY) WITHOUT ROWID` (+ blob table for deref attrs).
- Autocommit reads; `-commit!` wraps batch inserts in one transaction.
- Scan supports `reverse?` via `ORDER BY k DESC`.
- Engine (not the store) owns tx-visibility / datoms-filter overlays.

**Take from dbval:**

1. Store is the source of truth for committed keys; no second engine for query.
2. Range scan primitive with optional reverse.
3. Atomic batch commit for a tx’s keys.
4. Visibility / history filtering stays above the store (already planned in `design-tx-filter-history.md`).

**Do not copy blindly (divergence, keep DataScript/LMDB layout):**

| dbval | This repo (keep) |
| --- | --- |
| One table, all indexes encoded into one key space | Three tables `ds_eavt` / `ds_aevt` / `ds_avet` (matches LMDB DBIs) |
| Keys only (values in blob store for deref) | Key + value BLOB (`added` + codec value), same as LMDB Index |
| Append-only `INSERT OR IGNORE` | Current put/replace + remove; append-only purge model follows tx-filter work |
| JDBC + WITHOUT ROWID | ocaml-sqlite3; optional WITHOUT ROWID later as optimization |
| Content-hash blobs | Out of scope unless/until deref attrs land |

Observable DataScript behavior and the existing LMDB codec remain authoritative.

## Design options

### Option A — SQLite implements the Index API (recommended first)

Treat `Datascript_sqlite_db.t` like `Datascript_lmdb_db.t`: one shared handle, three logical indexes. Add `datascript_sqlite_index.ml` mirroring `datascript_lmdb_index.ml`, reusing the same codec.

Promote SQLite plugin to **share** its db handle (same role as LMDB `Share_index_db`):

```
open SQLite
  → Share_index_db sqlite_db
  → Index.empty Eavt|Aevt|Avet on that handle
  → no copy_indexes_to_lmdb, no temp LMDB
  → writes go to SQLite Index; sync_* become no-ops or meta-only
```

**Pros:** Smallest path to “SQLite without LMDB”; reuses Index call sites in `impl/db.ml`; keeps three-table layout and codec parity with LMDB for benches/tests.  
**Cons:** Still two Index implementations until a deeper shared KV layer exists.

### Option B — Introduce a dbval-like `Tuple_store` first

Define an OCaml `Tuple_store` (scan / commit / close) with LMDB and SQLite adapters; rewrite `Index` once on top of `Tuple_store`.

**Pros:** Matches dbval layering; one Index implementation.  
**Cons:** Larger refactor of LMDB path before SQLite becomes independent; higher risk to current LMDB parity.

### Decision

**Ship Option A first.** Optionally extract a shared internal KV scan/commit helper later (Option B lite) once both backends are green and duplication is obvious.

Keep Option B as a follow-up ADR if we want one Index over pluggable stores.

## Target architecture (Option A)

```
                    ┌─────────────────────────┐
                    │  impl/db.ml (Index API) │
                    └───────────┬─────────────┘
                                │
              ┌─────────────────┴─────────────────┐
              ▼                                   ▼
   Datascript_lmdb_index                 Datascript_sqlite_index
              │                                   │
   Datascript_lmdb_db                    Datascript_sqlite_db
              │                                   │
         LMDB env                          SQLite file
              │                                   │
              └──────── Datascript_*_codec ───────┘
                     (shared order-preserving keys)
```

Storage protocol (conceptual):

```ocaml
type storage_index_db =
  | Share_index_db of index_db   (* opaque: LMDB or SQLite handle *)
  | Separate_index_db            (* reserved for true external mirrors *)

(* Prefer: backend owns the shared handle; create_index_db returns that handle
   when Share_index_db, else allocates a backend-default temp index db. *)
```

Concrete typing options (pick one in Phase 1):

1. **Variant handle** — `index_db = Lmdb of Datascript_lmdb_db.t | Sqlite of Datascript_sqlite_db.t` in protocol/native only; Index module dispatches.
2. **First-class modules / functor** — Index ops parameterized by a `Db` signature (`with_write_txn`, `put`, `fold_range`, …). Cleaner long-term; more churn.
3. **Virtual library** — Dune virtual Index already exists per platform; add a native “sqlite index” product or select backend at link/register time.

Recommendation: **(1) for the first PR series**, keep call sites simple; revisit (2) if dispatch noise grows.

Rename for honesty (can be gradual):

- `type lmdb` → `type index_db` (or keep alias)
- `create_lmdb` → `create_index_db` (protocol already has this name; Index wrapper should match)
- `Index.lmdb_of` / `db_of` → `index_db_of`

## Phased work

### Phase 0 — Scope lock (this doc)

- [x] Document current Separate_index mirror and LMDB hard deps.
- [x] Choose Option A; document dbval takeaways and non-goals.
- [x] Agree: SQLite package must build/link **without** direct `lmdb_*` libraries
      (transitive LMDB via `datascript-ocaml-native` for the default memory engine remains until a later optional-backend split).

### Phase 1 — Decouple protocol types from concrete LMDB

1. [x] Introduce `index_db = Lmdb | Sqlite` in the native storage protocol.
2. [x] Keep LMDB behavior for memory/file Share path (cross-env sync lives in Index).
3. [x] Rename Index entry points (`create_index_db`, `index_db`, aliases kept).
4. [x] `empty_db` / `init_db` / `restore` pass storage into `create_index_db`.

**Exit:** native + tests green.

### Phase 2 — `Datascript_sqlite_index`

- [x] Implement Index surface over `Datascript_sqlite_db` (codec reused).
- [x] Wire native Index dispatch `Lmdb | Sqlite`.
- [x] Rename codec package to neutral `datascript_index_codec`.
- [x] SQL `ORDER BY key DESC` for rslice via `fold_index_range_desc_until`.

**Exit:** SQLite Index round-trips without constructing an LMDB mirror for Share sessions.

### Phase 3 — SQLite plugin becomes Share_index_db

1. [x] Plugin: `Share_index_db (Sqlite sqlite)`; sync/load no-ops for shared handle.
2. [x] Drop `copy_indexes_to_lmdb` / LMDB-typed mirror helpers from sqlite storage.
3. [x] Drop direct `lmdb_db_native` / `lmdb_index_native` from `sqlite/dune`.
4. [x] Tests assert `db_shares_storage_index` for empty_db and restore.

**Exit:** sqlite package has no direct `lmdb_*` dune deps; Share path verified.

### Phase 4 — Hardening & parity

1. [x] Tx-filter / history / as-of / since on SQLite Share path (same read pipeline; covered by sqlite temporal tests).
2. [x] WAL / synchronous pragmas: open `WAL` + `NORMAL`; `Storage.sync` → FULL + wal_checkpoint.
3. [x] `WITHOUT ROWID` on index/meta tables (dbval-style).
4. [x] Reverse scan via `fold_index_range_desc_until` for `rslice_seq`.
5. [x] Document operator choice: `docs/lmdb-vs-sqlite.md`.
6. [x] Codec rename: `Datascript_index_codec` / `datascript-ocaml-native.index-codec`.
7. [x] `open_path` no longer deletes existing files (reopen persistence).

### Phase 5 (optional) — Tuple_store extraction

Deferred: LMDB and SQLite Index duplication is acceptable for now; revisit if a third backend lands.

## Package / dependency matrix (target)

| Package / use | LMDB | SQLite | Codec |
| --- | --- | --- | --- |
| `datascript-ocaml-native` (memory default) | yes (until optional) | no | yes |
| `datascript-ocaml-native-lmdb` | yes | no | yes |
| `datascript-ocaml-native-sqlite` | **no** | yes | yes |
| Benches comparing both | yes | yes | yes |

## Testing

1. Existing sqlite storage / restore tests: switch expectation from “copy into LMDB” to “share sqlite index”; same public API results.
2. Shared query suite: `sqlite` storage mode must not open LMDB (assert via link or runtime probe in debug builds if useful).
3. Parity: LMDB file vs SQLite file on shared query + temporal views once Phase 4 lands.
4. Regression: memory LMDB path unchanged.

## Non-goals (this plan)

- Logseq legacy PSS `db.sqlite` / kvs format compatibility.
- Replacing LMDB as the default in-memory engine in the first series of PRs.
- Adopting dbval’s single-table key layout or content-addressed blob store.
- Melange / js_of_ocaml SQLite (native-only unless a separate effort).
- Making PostgreSQL / other backends Share_index in the same change set (protocol should allow it later).

## Risks

| Risk | Mitigation |
| --- | --- |
| `rslice_seq` / seek semantics differ | Golden tests vs LMDB Index on same datom sets |
| Perf regression vs LMDB mirror-in-RAM | Accept for v1; WAL + prepared stmts + WITHOUT ROWID experiments in Phase 4 |
| Core still pulls LMDB so “sqlite-only install” fails | Phase 3 dependency split; optional later default-backend flag |
| Protocol variant explodes call sites | Keep dispatch inside Index / protocol; db.ml stays on Index API |
| Codec package name implies LMDB | Rename to neutral module in Phase 2 |

## Success criteria

1. Documented plan reviewed (this file).
2. Application can depend on sqlite storage, run queries, and never load LMDB.
3. Observable query/tx results match LMDB backend for the shared suite.
4. SQLite dune library does not list `lmdb` / `lmdb_*` deps.

## Suggested PR slice

1. **Plan only** (this document).
2. Protocol / Index rename + `index_db` abstraction (LMDB-only behavior).
3. `datascript_sqlite_index` + codec rename + unit tests.
4. Plugin Share path + drop LMDB from sqlite package + bench/CI.
5. Tx-visibility / pragma / streaming follow-ups.

## Open questions

1. Should default `empty_db` (no storage) stay LMDB forever, or allow a compile/link-time sqlite default for sqlite-only products?
2. Keep three BLOB tables permanently, or migrate toward one dbval-style table after Index is shared? (Recommendation: keep three.)
3. Is renaming `Datascript_lmdb_codec` → `Datascript_index_codec` acceptable in the same PR as sqlite Index, or a pure move PR first?
