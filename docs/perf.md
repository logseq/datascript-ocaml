# Performance Notes

This document records the performance work done while comparing this OCaml port
against upstream DataScript's ClojureScript/JavaScript build. Semantic parity is
the first constraint: optimizations must preserve public behavior, especially
lazy `datoms` access.

## Benchmark Harness

The cross-runtime benchmark entrypoint is:

```sh
BENCH_SIZE=200 BENCH_WARMUP_MS=50 BENCH_SAMPLE_MS=100 BENCH_SAMPLES=3 \
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
BENCH_WARMUP_MS=50
BENCH_SAMPLE_MS=100
BENCH_SAMPLES=3
```

Lower is better.

| Benchmark | OCaml native | js_of_ocaml | upstream CLJS/JS |
| --- | ---: | ---: | ---: |
| add-all | 2.27 | 6.12 | 15.64 |
| add-one-by-one | 13.76 | 27.75 | 14.95 |
| datoms-name | 0.00114 | 0.00489 | 0.00433 |
| query-name-age | 0.01491 | 0.03588 | 0.07258 |
| query-salary-pred | 0.04117 | 0.10471 | 0.15547 |
| pull-one | 0.00150 | 0.00429 | 0.01087 |

Current status:

- Native OCaml is faster than upstream CLJS/JS on every benchmark listed above.
- `js_of_ocaml` is faster on most read/query/pull benchmarks.
- `js_of_ocaml add-one-by-one` is still slower than upstream CLJS/JS.
- `js_of_ocaml datoms-name` is close to upstream, but still slightly slower in
  this sample.

## Optimizations Applied

### Lazy Public Datoms

Public `datoms` APIs now return `datom Seq.t` instead of eagerly materializing
lists. This matches upstream DataScript's lazy datoms behavior and prevents a
plain datoms call from reading and returning the full index immediately.

Tests that need a list explicitly materialize the sequence at the compatibility
boundary. Added coverage checks that:

- public `datoms` returns a lazy sequence
- bounded datoms slicing happens before filtered-db predicate checks

### Bounded Index Iteration

The DB stores sorted array snapshots for EAVT, AEVT, AVET, and VAET when index
arrays are valid. Datoms lookup can then binary-search a bounded range and expose
that range as a lazy sequence.

This avoids full-index filtering for common component-constrained accesses such
as entity or attribute slices.

### Incremental Index Refresh

Bulk transaction paths can merge newly added datoms into existing sorted indexes
instead of rebuilding everything from scratch. Initial bulk loads keep array
snapshots valid; later incremental changes currently invalidate array snapshots
when a full array rebuild is not performed.

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

### History Append Avoidance

Fast-path transaction history uses prepend-oriented accumulation instead of
repeated append copying. This avoids quadratic list copying in add-heavy
workloads.

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

The main remaining benchmark gap is `js_of_ocaml add-one-by-one`. The next likely
optimization is delaying maintenance or array rebuilding for secondary indexes
that are not needed by the write-heavy path, while keeping public `datoms`
correct when those indexes are later accessed.

Any delayed-index approach needs careful handling because DB index fields are
part of the public representation today. Prefer a simple validity model over a
large rewrite unless profiling shows the extra complexity is justified.

## Verification

Latest full test command:

```sh
dune runtest
```

It passed after the current optimization set. The run emitted linker warnings
about a missing `/opt/homebrew/opt/node@22/lib` search path, but no test failure.
