---
name: performance
description: Performance rules and known hot paths for this codebase. Use before touching impl/query_*.ml, impl/transact.ml, impl/db.ml, impl/storage.ml, impl/pull_api.ml, impl/entity.ml, or any code that loops over datoms, query rows, or index contents.
---

# Performance

Measured profile (large Logseq graphs): queries returning 30k–70k rows dominate;
GC ~26%, List ops ~10%. The rules below keep new code off the quadratic paths and
keep hot paths allocation-light.

## Index access discipline

- Never walk a whole index (`datoms db Eavt ()`, unbounded `Avet ~a` slices) or
  materialize `entity`/`ent_of_id` inside loops when a bounded seek suffices.
  Prefer `~e`/`~a`/`~v`-constrained `datoms` queries and datom-level checks.
- Whole-index work belongs only in inherently whole-db operations: `diff`,
  export, serialize/restore, GC (`collect_garbage`), checksum/validate.
- `index_datoms_seq`/`reverse_index_datoms_seq` stream without materializing;
  `visible_index_datoms`/`raw_index_datoms_list` copy the full index — use the
  seq variants inside loops.

## List anti-patterns banned on hot paths

Per-datom, per-row, per-block loops must not contain:

- `xs @ [x]`, `List.append`, `List.concat` inside folds/loops — O(n²). Cons onto
  a reversed accumulator, or use `Hashtbl`-keyed groups / `Queue` / `Rrbvec`,
  then `List.rev`/`concat` once at the end.
- `List.mem` / `List.assoc` / `List.find` / `List.exists` / `List.nth` inside a
  loop over a growing or large collection — build a `Hashtbl`/Set once, then
  O(1) lookups.
- `List.length` recomputed per iteration — hoist it or thread a counter.
- `List.filter`/`map` chains that re-traverse large data when one fold suffices.
- `List.sort`/`List.sort_uniq` where a `Hashtbl` dedup or a min-fold applies
  (e.g. sorting only to take `List.hd`).
- `List.combine`/`List.split`/`Array.to_list`/`List.of_seq`/`List.of_array` per
  row — keep arrays positional, hash the key→index map once.
- Non-tail recursion over unbounded lists (stack depth risk at 100k+).

Cheap-constant exceptions: fixed small lists like `Schema.schema_fields`
(10 elements) — `List.mem` on them is fine.

## Known hot paths (audit Oct 2026)

- `impl/query_where.ml` — join/scan loops; `relation.rows` is
  `query_result array list` with typed-hash dedup already — keep it that way.
- `impl/query_api.ml:295` — `collect_find_specs` + `List.combine`/`Array.to_list`
  per result row; precompute a `Hashtbl attr→index` per query instead.
- `impl/db.ml` — `diff` (whole-DB; pairwise scans now `(e,a)`→values
  Hashtbls), `eavt_datoms`, `remove_facts_from_duplicate_tables`/
  `refresh_indexes_with_tx_data` (retract rebuilds now batched per tx —
  keep them batched), `find_active_datom_by_fact` (min-fold, keep it).
- `impl/transact.ml:754` — `dedupe_facts` now Hashtbl buckets on
  upsert-merge; order must stay first-occurrence.
- `impl/storage.ml` — `collect_garbage` (live set is a Hashtbl),
  `tail_datom_count` (concat-to-count).
- `impl/entity.ml:48` — `raw_forward_entity_attrs` assoc/remove_assoc grouping
  per datom → O(D²) on wide entities.
- `impl/pull_api.ml:492/513/604` — `List.mem` ancestry check per recursion level.
- `impl/conn.ml:150,178` — `storage_tail @ [report.tx_data]` per tx.

## Checklist before submitting

- For each `List.` call inside a loop: what is the input size at this call site,
  and who reaches it from a public endpoint (`transact`, `q`, `pull`, `entity`,
  `datoms`, `diff`, storage GC)?
- Whole-db scans only in diff/export/restore/GC/checksum.
- Allocations: avoid `map|>filter|>map` chains and intermediate lists on
  >1k-item data; fuse into a single fold.
- Verify no `Seq` was forced into a `List` where streaming suffices.
