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

## Semantic preflight and comparable inputs

`script/benchmark_vs_cljs.sh` first launches untimed semantic exports from all
three runtimes. It compares typed fixture facts, schema options, exact query
strings and scalar inputs, complete query results, transaction outputs, pulls,
and the selected page's materialized entities and ordered tree. Any difference
stops the script before timing begins. `storage-roundtrip` has no CLJS timing
counterpart; its restored facts are verified against the native fixture instead.

Random draws use an explicit shared order. Entity IDs are positive integers so
reference targets do not depend on temporary-ID allocation. Attributes and sex
values are genuine keywords; placeholder `block/uuid` values are strings in all
runtimes. The CLJS harness obtains typed literals using the pinned bundle's own
public EDN query reader and function input, then passes them to unchanged public
transaction/query APIs. It does not use the OCaml JS facade's implicit string
conversion. Typed input construction runs once before timing in both harnesses;
the `add-*` cases now measure database transactions over prepared input, rather
than entity generation or EDN parsing. Page result consumption includes `db/id`
in both runtimes. Historical measurements below predate these changes and must
be rerun before using them as current comparisons.

The semantic regression test covers sizes 0, 1, 20 and 1000, and deliberately
injects a wrong row with an unchanged row count and a string/keyword mismatch.
With `UPSTREAM_DATASCRIPT_JS` pointing to the pinned bundle, run:

```sh
opam exec -- dune runtest bench --profile release
```

The comparison script prints timings; the separate strict speed gate is:

```sh
opam exec -- script/benchmark_gate_vs_cljs.sh
```

The gate still requires each OCaml runtime's printed median to be strictly lower
than CLJS on every comparable case, with no tolerance. Empty/non-finite reference
results now fail. CI currently invokes the comparison script, so a successful CI
comparison confirms semantic agreement and execution, not a passing strict
speed gate. Native and Node measurements are separate runtimes and should not
be mixed into a same-runtime performance claim.

## Latest Verified Results

Verified on 2026-09-27.

Configuration:

```text
BENCH_WARMUP_MS=200
BENCH_SAMPLE_MS=500
BENCH_SAMPLES=5
UPSTREAM_DATASCRIPT_JS=_deps/datascript/release-js/datascript.js
```

Milliseconds, lower is better.

### Size 200

| Benchmark | OCaml native | js_of_ocaml | upstream CLJS/JS |
| --- | ---: | ---: | ---: |
| add-1 | 2.18 | 5.28 | 6.33 |
| add-5 | 3.85 | 6.13 | 13.89 |
| add-all | 3.33 | 7.00 | 13.59 |
| datoms-name | 0.00199 | 0.00391 | 0.00445 |
| q1 | 0.00407 | 0.01049 | 0.02987 |
| q2 | 0.00485 | 0.01415 | 0.06474 |
| q3 | 0.01688 | 0.02748 | 0.06801 |
| q4 | 0.02074 | 0.02817 | 0.10196 |
| q5-shortcircuit | 0.00496 | 0.01108 | 0.03359 |
| qpred1 | 0.01126 | 0.04702 | 0.12986 |
| qpred2 | 0.01719 | 0.04581 | 0.16120 |
| q2pred | 0.00557 | 0.01416 | 0.07445 |
| pull-one | 0.00130 | 0.00277 | 0.00666 |
| storage-roundtrip | 3.45 | 9.49 | n/a |
| get-page-data | 0.30675 | 0.89606 | 1.17 |

### Size 1000

| Benchmark | OCaml native | js_of_ocaml | upstream CLJS/JS |
| --- | ---: | ---: | ---: |
| add-1 | 13.82 | 27.21 | 42.30 |
| add-5 | 16.51 | 42.17 | 77.41 |
| add-all | 22.26 | 64.25 | 81.16 |
| datoms-name | 0.00699 | 0.01761 | 0.01886 |
| q1 | 0.01939 | 0.06015 | 0.08590 |
| q2 | 0.05351 | 0.06251 | 0.21758 |
| q3 | 0.10196 | 0.12168 | 0.13764 |
| q4 | 0.12178 | 0.12953 | 0.20986 |
| q5-shortcircuit | 0.00572 | 0.01084 | 0.03235 |
| qpred1 | 0.07263 | 0.26954 | 0.58958 |
| qpred2 | 0.07048 | 0.25773 | 0.74998 |
| q2pred | 0.02273 | 0.06615 | 0.21568 |
| pull-one | 0.00130 | 0.00316 | 0.00700 |
| storage-roundtrip | 33.71 | 84.50 | n/a |
| get-page-data | 0.35314 | 0.84459 | 1.19 |

### Size 10000

| Benchmark | OCaml native | js_of_ocaml | upstream CLJS/JS |
| --- | ---: | ---: | ---: |
| add-1 | 174.13 | 306.50 | 456.11 |
| add-5 | 218.88 | 417.00 | 845.32 |
| add-all | 742.30 | 2105.00 | 941.57 |
| datoms-name | 0.06288 | 0.14988 | 0.20549 |
| q1 | 0.25882 | 0.64516 | 0.81310 |
| q2 | 0.78567 | 0.82372 | 2.02 |
| q3 | 1.35 | 1.61 | 1.31 |
| q4 | 1.87 | 2.07 | 2.26 |
| q5-shortcircuit | 0.00923 | 0.02294 | 0.03312 |
| qpred1 | 1.95 | 4.43 | 6.53 |
| qpred2 | 1.90 | 4.39 | 6.64 |
| q2pred | 0.29155 | 0.62344 | 1.71 |
| pull-one | 0.00129 | 0.00294 | 0.00639 |
| storage-roundtrip | 1321.55 | 4051.00 | n/a |
| get-page-data | 0.40625 | 0.93110 | 1.22 |

Current status:

- Native OCaml is faster than upstream CLJS/JS on every benchmark except `q3`
  at size 10000, where the two are effectively tied (1.35 ms vs 1.31 ms).
- `js_of_ocaml` is faster than upstream on nearly all cases; exceptions are
  `add-all` at size 10000 (2105 ms vs 941.57 ms) and `q3` at sizes 1000 and
  10000 (1.61 ms vs 1.31 ms at 10000).
- `js_of_ocaml` overflows the default Node.js stack at size 10000
  (`RangeError: Maximum call stack size exceeded` during `add-all`). The
  numbers above were measured with `node --stack-size=8000`; this is a known
  js_of_ocaml recursion limitation, not a benchmark artifact.
- `storage-roundtrip` has no upstream equivalent (upstream bundle is in-memory
  only), so it is reported without a comparison.
- `get-page-data` models Logseq's `logseq.api.db-based.tools/get-page-data`:
  look up a page by `avet :block/name`, list its ~100 blocks via
  `avet :block/page`, materialize each entity (`entity_attrs` /
  `(into {} (d/entity ...))`), then nest them by `:block/parent` sorted on
  `:block/order` (`otree/blocks->vec-tree`). The fixture DB has ~100 blocks per
  page so `size` scales page count, not per-page cost — which is why timings
  are nearly flat. It exercises indexed-ref datoms slicing plus entity
  materialization; pure string work in the real path
  (`recur-replace-uuid-in-block-title`, `remove-hidden-properties`) is not
  datascript work and is not modeled.
- Since the 2026-06-19 run the benchmark set was expanded (now `add-1`,
  `add-5`, `q1`–`q5-shortcircuit`, `qpred1`, `qpred2`, `q2pred`,
  `storage-roundtrip`, `get-page-data`). The earlier gaps — upstream being faster on
  `datoms-name` and the size-10000 one-by-one add — are now closed: native
  `datoms-name` at size 10000 improved from 0.77 ms to 0.06 ms, and
  one-by-one add (`add-5`) from 727.86 ms to 218.88 ms.

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

Unique conflict checks use the `AVET` index, matching upstream DataScript's
unique validation path.

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
  UPSTREAM_DATASCRIPT_JS=_deps/datascript/release-js/datascript.js \
  script/benchmark_vs_cljs.sh
```

The same command was repeated with `BENCH_SIZE=1000` and `BENCH_SIZE=10000` for
the 2026-06-19 tables above.
