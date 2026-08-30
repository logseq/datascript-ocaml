# LMDB vs SQLite operator choice

Native DataScript can persist indexes with either LMDB or SQLite. Both use the same
order-preserving index codec (`Datascript_index_codec`) and the same Index /
tx-visibility read pipeline. Prefer one based on ops and access pattern, not API.

## Prefer LMDB when

- You want memory-mapped range scans and the lowest query latency on large indexes.
- The deployment already depends on LMDB / can ship `liblmdb`.
- Writers and readers share a durable env and you are comfortable with LMDB sizing /
  map growth (`mapsize`) and multi-process locking rules.

## Prefer SQLite when

- You want a **single portable file** (plus WAL/SHM sidecars in WAL mode) that ops
  already know how to backup, inspect, and migrate.
- You want SQL tooling (`sqlite3` CLI) for debugging meta / key tables.
- Embedding constraints favor SQLite over LMDB, or you already link `libsqlite3`.

## Shared behavior

| Concern | Behavior |
| --- | --- |
| Live indexes | `Share_index_db` — queries/writes hit the storage file/env directly |
| Key layout | Three BLOB tables/DBIs: EAVT / AEVT / AVET (+ meta) |
| Temporal views | `as_of` / `since` / `history` use the same visibility filter above the store |
| Durability | LMDB `sync`; SQLite open defaults `WAL` + `synchronous=NORMAL`, `Storage.sync` forces FULL + WAL checkpoint |
| Reopen | Both `open_path` / `open_session` reopen existing files without deleting; callers wipe via `remove_path` when they want a fresh env |

## Package map

| Package | Backend |
| --- | --- |
| `datascript-ocaml-native` | Default in-memory indexes via LMDB temp env |
| `datascript-ocaml-native-lmdb` | File LMDB sessions |
| `datascript-ocaml-native-sqlite` | File SQLite sessions (no direct `lmdb_*` dune deps) |

Benches: `bench/compare_lmdb_sqlite.sh` (persistent store/restore),
`bench/compare_lmdb_sqlite_queries.sh` (shared query suite), and
`bench/compare_lmdb_sqlite_index_scan.sh` (cold open + narrow Index scans;
default sizes `200000,500000`).
