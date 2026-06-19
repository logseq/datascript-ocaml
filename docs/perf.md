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

Configuration:

```text
BENCH_SIZE=200
BENCH_WARMUP_MS=200
BENCH_SAMPLE_MS=500
BENCH_SAMPLES=5
```

Lower is better.

| Benchmark | OCaml native | js_of_ocaml | upstream CLJS/JS |
| --- | ---: | ---: | ---: |
| add-all | 2.23 | 5.13 | 14.27 |
| add-one-by-one | 1.56 | 4.20 | 14.31 |
| datoms-name | 0.00085 | 0.00384 | 0.00429 |
| query-name-age | 0.01490 | 0.03514 | 0.06272 |
| query-salary-pred | 0.04028 | 0.09610 | 0.15142 |
| pull-one | 0.00164 | 0.00443 | 0.01035 |

Current status:

- Native OCaml is faster than upstream CLJS/JS on every benchmark listed above.
- `js_of_ocaml` is faster than upstream CLJS/JS on every benchmark listed above.
- The biggest remaining item is broader validation on larger and more varied
  workloads; the current deterministic harness goal is satisfied.

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
BENCH_SIZE=200 BENCH_WARMUP_MS=100 BENCH_SAMPLE_MS=300 BENCH_SAMPLES=5 \
  script/benchmark_vs_cljs.sh
```
