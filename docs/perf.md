# Performance Notes

This document records the performance work done while comparing this OCaml port
against upstream DataScript's ClojureScript/JavaScript build. Semantic parity is
the first constraint: optimizations must preserve public behavior, especially
lazy `datoms` access.

## Benchmark Harness

The cross-runtime benchmark entrypoint is:

```sh
BENCH_SIZE=200 BENCH_WARMUP_MS=100 BENCH_SAMPLE_MS=300 BENCH_SAMPLES=5 \
  script/benchmark_vs_cljs.sh
```

The harness compares:

- native OCaml benchmark executable
- `js_of_ocaml` output for the same benchmark
- upstream DataScript ClojureScript/JavaScript benchmark

The benchmark script supports these knobs:

- `BENCH_SIZE`
- `BENCH_WARMUP_MS`
- `BENCH_SAMPLE_MS`
- `BENCH_SAMPLES`
- `UPSTREAM_DATASCRIPT_JS`

## Latest Verified Results

Verified on 2026-06-19.

Configuration:

```text
BENCH_WARMUP_MS=200
BENCH_SAMPLE_MS=500
BENCH_SAMPLES=5
UPSTREAM_DATASCRIPT_JS=/Users/tiensonqin/Codes/projects/datascript/release-js/datascript.js
```

Lower is better.

### Size 200

| Benchmark | OCaml native | js_of_ocaml | upstream CLJS/JS |
| --- | ---: | ---: | ---: |
| add-all | 3.05 | 6.72 | 14.25 |
| add-one-by-one | 3.22 | 7.80 | 14.35 |
| datoms-name | 0.01654 | 0.05378 | 0.00431 |
| query-name-age | 0.02199 | 0.05820 | 0.06224 |
| query-salary-pred | 0.00985 | 0.02585 | 0.15149 |
| pull-one | 0.00313 | 0.00950 | 0.01040 |

### Size 1000

| Benchmark | OCaml native | js_of_ocaml | upstream CLJS/JS |
| --- | ---: | ---: | ---: |
| add-all | 17.44 | 40.15 | 81.42 |
| add-one-by-one | 22.76 | 61.44 | 80.87 |
| datoms-name | 0.07618 | 0.25536 | 0.01825 |
| query-name-age | 0.12468 | 0.31969 | 0.19548 |
| query-salary-pred | 0.04302 | 0.11631 | 0.63592 |
| pull-one | 0.00329 | 0.01021 | 0.01051 |

### Size 10000

| Benchmark | OCaml native | js_of_ocaml | upstream CLJS/JS |
| --- | ---: | ---: | ---: |
| add-all | 209.66 | 496.00 | 999.61 |
| add-one-by-one | 727.86 | 2906.00 | 999.67 |
| datoms-name | 0.76836 | 2.75 | 0.19502 |
| query-name-age | 1.94 | 5.19 | 1.71 |
| query-salary-pred | 0.65311 | 1.44 | 6.43 |
| pull-one | 0.00327 | 0.01041 | 0.01058 |

Current status:

- The sequential explicit-id transaction regression is fixed for native OCaml:
  size 10000 `add-one-by-one` dropped from 10212.84 ms to 727.86 ms.
- `js_of_ocaml` also improved on the same path: size 10000
  `add-one-by-one` dropped from 11707.00 ms to 2906.00 ms.
- Native OCaml remains faster than upstream CLJS/JS on bulk add, small and
  medium one-by-one add, salary predicate query, and pull.
- Upstream CLJS/JS remains faster for `datoms-name`, and at size 10000 it is
  still faster for `query-name-age` and one-by-one add. Those are tracked as
  remaining gaps, not hidden by local API changes.

## Optimizations Applied

### Lazy Public Datoms

Public `datoms` APIs now return `datom Seq.t` instead of eagerly materializing
lists. This matches upstream DataScript's lazy datoms behavior and prevents a
plain datoms call from reading and returning the full index immediately.

Tests that need a list explicitly materialize the sequence at the compatibility
boundary. Added coverage checks that:

- public `datoms` returns a lazy sequence
- bounded datoms slicing happens before filtered-db predicate checks

### Persistent Sorted Indexes

The DB stores EAVT, AEVT, AVET, and VAET as `Persistent_sorted_set.t` values.
This matches upstream DataScript's persistent sorted set model better than
whole-index arrays because transactions must preserve the old immutable DB while
producing a new DB with updated indexes.

2026-06-19 root-cause fix: `persistent-sorted-set-ocaml` now exposes
`to_seq : 'a seq -> 'a Seq.t`, and tree-backed `seq`/`slice_seq` sources stream
from the persistent sorted tree instead of first materializing a list. This
matches upstream `me.tonsky.persistent-sorted-set`, where `slice` returns an
iterator over B+ tree paths. DataScript's `datoms` path consumes
`PSet.seq |> PSet.to_seq` and `PSet.slice_seq |> PSet.to_seq`, keeping public
index reads lazy without adding array-backed duplicate indexes.

Bulk construction sorts datoms once and builds each persistent sorted set from
the sorted array. Incremental safe-add paths update the persistent sorted sets
with structural sharing instead of marking whole indexes stale.

Exact prefix reads such as `datoms db Aevt ~a`, lower-bound reads such as
`seek_datoms db Avet ~a ~v`, reverse reads such as `rseek_datoms`, and
`index_range` use persistent sorted set `slice`/`rslice` bounds. Non-prefix
named-argument combinations continue to fall back to ordered filtering for
compatibility.

### Incremental Index Refresh

Bulk construction uses sorted-array PSS builders. For safe incremental writes,
the write path adds new datoms into each relevant persistent sorted set. The old
DB keeps its previous set roots, and the new DB shares unchanged tree structure
with them.

A regression test covers incremental writes followed by public EAVT, AEVT,
AVET, and VAET reads.

### Query Candidate Narrowing

Query pattern execution substitutes already-bound variables into later clauses
before selecting candidate datoms. This allows later clauses to use narrower
index slices instead of scanning broad candidates.

The pattern datoms path also uses AVET when both attribute and scalar/ref value
are constants and the attribute is AVET-accessible. Complex values such as
tuples, lists, maps, sets, nil, and tx values avoid this fast path so matcher
semantics stay unchanged.

### Query String Cache

Top-level query-string execution caches parsed non-pull query strings. Pull query
strings still use the normal parse path because parsing can depend on pull
context.

### Pull Fast Path

Pull has a forward-only fast path for selectors that do not use wildcard or
reverse attributes. It builds a lightweight entity from the EAVT entity slice
instead of constructing a full entity view.

Reverse attributes and wildcard selectors still use the full entity path to
preserve behavior. Recursive forward pull is included in the fast path.

### Transaction Fast Paths

Safe add paths were added for explicit-id simple entity maps under conservative
conditions:

- no tempids
- no nested entities
- no reverse attributes
- no db/schema attributes
- no tuple attributes
- no complex refs
- no duplicate cardinality-one attrs in the same entity map

For supported fast-path transactions, strict schema recomputation is skipped
because schema-changing attributes are excluded.

2026-06-19 root-cause fix: `Transact.apply_tx` no longer materializes the full
EAVT index before trying the explicit-id entity fast path. Full active datoms
are forced only when the fallback transaction interpreter or old-entity conflict
checks need them. For new explicit entity ids, the fast path returns only the
added facts and lets `refresh_indexes_with_added_datoms` update the PSS roots,
matching upstream's incremental persistent-index behavior instead of rebuilding
or scanning the whole DB for every small transaction.

### Transaction Staging Lists

Transaction code still uses local datom lists while applying a batch. Those
lists are staging values only; the resulting DB stores current facts in PSS
indexes instead of retaining a duplicate active datom list.

### Existing Fact and Unique Checks

The DB tracks `max_datom_e`, which allows existing-fact and cardinality scans to
skip checks for facts whose entity id is greater than every existing datom entity
id.

The DB also keeps a lightweight `unique_index` so unique conflict checks do not
need to scan all datoms.

## Semantics Constraints

These constraints must remain true for future performance work:

- Public `datoms` must stay lazy.
- Datoms slicing must happen before filtered-db predicate checks.
- Optimizations must not change datom ordering.
- Query fast paths must preserve matcher coercion and special value behavior.
- Pull fast paths must fall back for reverse attributes and wildcard selectors.
- Transaction fast paths must reject unsupported cases instead of approximating
  behavior.

## Remaining Work

The current deterministic benchmark goal is satisfied. Next useful work is to
validate larger workloads and mixed read/write patterns against the persistent
sorted set implementation. DB index fields are still present in the public record
type, so future API cleanup should either make those internals private or keep
their PSS representation documented.

## Verification

Latest feasible native test command in the current environment:

```sh
dune runtest test/test_datascript.exe test/test_lru.exe test/test_conn.exe \
  test/test_core.exe test/test_db.exe test/test_data_readers.exe \
  test/test_built_ins.exe test/test_issues.exe test/test_entity.exe \
  test/test_listen.exe test/test_lookup_refs.exe test/test_parser.exe \
  test/test_parser_find.exe test/test_parser_query.exe \
  test/test_parser_return_map.exe test/test_parser_rules.exe \
  test/test_parser_where.exe test/test_pull_api.exe test/test_pull_parser.exe \
  test/test_query_pull.exe test/test_query_namespace.exe test/test_tuples.exe \
  test/test_serialize.exe test/test_storage.exe test/test_upsert.exe \
  test/test_util.exe
```

It passed after the persistent sorted set index change. Full `dune runtest`
currently depends on Dune/Findlib resolving the `sqlite3` library for the SQLite
storage tests.

Latest cross-runtime benchmark command:

```sh
BENCH_SIZE=200 BENCH_WARMUP_MS=200 BENCH_SAMPLE_MS=500 BENCH_SAMPLES=5 \
  UPSTREAM_DATASCRIPT_JS=/Users/tiensonqin/Codes/projects/datascript/release-js/datascript.js \
  script/benchmark_vs_cljs.sh
```

The same command was repeated with `BENCH_SIZE=1000` and `BENCH_SIZE=10000` for
the 2026-06-19 tables above.
