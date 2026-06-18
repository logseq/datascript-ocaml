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
BENCH_WARMUP_MS=100
BENCH_SAMPLE_MS=300
BENCH_SAMPLES=5
```

Lower is better.

| Benchmark | OCaml native | js_of_ocaml | upstream CLJS/JS |
| --- | ---: | ---: | ---: |
| add-all | 2.24 | 5.10 | 16.01 |
| add-one-by-one | 1.58 | 4.23 | 14.55 |
| datoms-name | 0.00089 | 0.00385 | 0.00426 |
| query-name-age | 0.01522 | 0.03586 | 0.06415 |
| query-salary-pred | 0.04021 | 0.09810 | 0.15102 |
| pull-one | 0.00167 | 0.00446 | 0.01055 |

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

### Bounded Index Iteration

The DB stores sorted array snapshots for EAVT, AEVT, AVET, and VAET when index
arrays are valid. Datoms lookup can then binary-search a bounded range and expose
that range as a lazy sequence.

This avoids full-index filtering for common component-constrained accesses such
as entity or attribute slices.

Exact prefix slices such as `datoms db Aevt ~a` now compute both start and stop
bounds up front and then lazily walk the array range by index. This avoids a
per-datom predicate check inside the hot range.

### Incremental Index Refresh

Bulk transaction paths can merge newly added datoms into sorted indexes for
initial loads. For later safe incremental writes into a non-empty DB, the write
path updates active datoms and metadata immediately, but marks stored index lists
and arrays stale instead of maintaining all four sorted indexes on every write.

Public `datoms` still returns correct results. If stored indexes are stale, the
read path builds the requested sorted index from current datoms and applies the
requested component filters. A regression test covers incremental writes followed
by public EAVT, AEVT, AVET, and VAET reads.

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

The current deterministic benchmark goal is satisfied. Next useful work is to
validate larger workloads, mixed read/write patterns after stale incremental
indexes, and public behavior around direct DB record access. DB index fields are
still present in the public record type, so future API cleanup should either make
those internals private or clearly document their validity flags.

## Verification

Latest full test command:

```sh
dune runtest
```

It passed after the current optimization set. The run emitted linker warnings
about a missing `/opt/homebrew/opt/node@22/lib` search path, but no test failure.

Latest cross-runtime benchmark command:

```sh
BENCH_SIZE=200 BENCH_WARMUP_MS=100 BENCH_SAMPLE_MS=300 BENCH_SAMPLES=5 \
  script/benchmark_vs_cljs.sh
```
