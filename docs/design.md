# DataScript OCaml Design

This document records the current DB/index design and the parity contract with
upstream DataScript's Clojure/ClojureScript implementation.

Primary reference:

- `/Users/tiensonqin/Codes/projects/datascript/src/datascript/db.cljc`

The goal is semantic compatibility first. Performance work is acceptable only
when it preserves DataScript's immutable DB value model, datom ordering, lazy
public access, and comparator-bound index behavior.

## DB Value Model

DataScript DB values are persistent immutable values. A transaction must keep
the old DB usable and produce a new DB with updated indexes. In upstream
DataScript this is handled by persistent sorted set indexes:

- `:eavt`
- `:aevt`
- `:avet`

The OCaml port follows that model with:

- `eavt_index : datom Persistent_sorted_set.t`
- `aevt_index : datom Persistent_sorted_set.t`
- `avet_index : datom Persistent_sorted_set.t`
- `vaet_index : datom Persistent_sorted_set.t`

`VAET` is explicit in the OCaml port because the public API exposes it directly
for reverse-reference style access. Upstream derives reverse-reference behavior
from indexed ref attrs; the OCaml port keeps a dedicated value-attribute-entity
index for that public surface.

The DB does not keep a separate active `datoms` list. The active fact set is the
`EAVT` persistent sorted set, matching upstream DataScript's DB shape. Code that
needs a list view, such as transaction staging or serialization, derives it from
`EAVT` at the boundary.

## Index Construction

Bulk DB construction sorts datoms with the same index comparators used by the
public access paths, then builds a persistent sorted set:

```ocaml
PSet.of_sorted_array_by (Util.compare_datom index) items
```

This matches upstream `set/from-sorted-array` in `init-db`.

Empty DB construction creates empty persistent sorted sets with the same
comparators. Deserialization reconstructs indexes from serialized datoms through
the normal refresh path; serialized data remains plain schema/datoms data, not
serialized index internals.

## Transaction Updates

For append-only fast paths, the OCaml port updates each persistent sorted set
with `Persistent_sorted_set.add`. This gives the same structural-sharing model
as upstream `set/conj`: the old DB keeps the old root, and the new DB points to
updated roots that share unchanged tree structure.

The port still computes `unique_index` and transaction reports separately:

- `unique_index` is an auxiliary lookup cache for uniqueness checks, not one of
  DataScript's ordered datom indexes.
- transaction reports preserve `db_before`, `db_after`, and `tx_data` values as
  immutable snapshots.

## Index Order

The OCaml datom comparators mirror upstream:

| Index | Order |
| --- | --- |
| `Eavt` | entity, attribute, value, tx |
| `Aevt` | attribute, entity, value, tx |
| `Avet` | attribute, value, entity, tx |
| `Vaet` | value, attribute, entity, tx |

The value comparator is DataScript-aware. Numeric values compare numerically, so
`Int 1` and `Float 1.0` are comparator-equal for index bounds. Exact AVET
lookups must therefore return both facts when the only difference is numeric
representation, matching upstream DataScript.

## Index Access

Upstream `IIndexAccess` uses `set/slice` and `set/rslice` for public index
operations. The OCaml port follows the same shape:

- `datoms` uses exact prefix slices when arguments form an index prefix.
- `seek_datoms` uses a lower-bound slice for prefix-compatible bounds.
- `rseek_datoms` uses a reverse upper-bound slice for prefix-compatible bounds.
- `index_range` slices `AVET` from an attribute/value lower bound to an
  attribute/value upper bound.

Non-prefix combinations are still supported because the OCaml public API uses
named optional arguments. When callers provide a combination that is not an
index prefix, the implementation falls back to ordered index iteration plus a
component filter. This preserves compatibility instead of pretending every
named-argument combination is a native sorted-set range.

Filtered DBs apply the filter predicate after slicing. This matches upstream
`FilteredDB`, where the filter wraps `-datoms`, `-seek-datoms`,
`-rseek-datoms`, and `-index-range`.

## Bound Datoms

Upstream bound datoms can contain `nil` components as wildcard-like comparator
markers. OCaml `datom` fields are not optional, so the port uses synthetic bound
datoms plus a bound-field mask. The custom slice comparator compares only the
fields that participate in the requested bound.

This is required for behavior such as:

- `datoms db Aevt ~a`
- `datoms db Eavt ~e ~a`
- `datoms db Avet ~a ~v`
- `seek_datoms db Avet ~a ~v`
- `rseek_datoms db Vaet ~v ~a`
- `index_range db attr ?start ?stop`

The actual datoms stored in the persistent sorted sets are always normal
datoms. Bound masks affect only the temporary comparator used by a slice.

## Public Laziness

Public `datoms` returns `Seq.t`. The implementation must not eagerly
materialize more than required at API boundaries. The current persistent sorted
set slice API returns lists, so a prefix slice materializes that bounded slice
before converting it to `Seq.t`. This is still materially different from
whole-index materialization, and it preserves the public lazy sequence contract
for callers. If `Persistent_sorted_set` grows streaming slice cursors, the OCaml
DB access layer should switch to them.

## Schema and AVET Accessibility

`AVET` contains datoms whose attributes are accessible through value lookup:

- `:db/index true`
- `:db/unique`
- ref-valued attributes, matching the port's schema rules
- tuple attributes that are installed as indexed tuple attrs

The access layer validates `AVET` attribute access and raises the upstream-style
message:

```text
Attribute :<attr> should be marked as :db/index true
```

Schema changes rebuild or update indexes through the same DB refresh/update
paths, so access reflects the DB value produced by that transaction.

## What Should Not Use PSS

Not every value near the DB should become an ordered persistent index.

- `unique_index` remains a lightweight uniqueness helper. It is not an
  upstream PSS index and does not provide ordered public access.
- transaction-local datom lists are temporary staging values, not DB fields.
- query rows, pull results, transaction reports, schema data, and storage
  payloads remain plain OCaml values.

Moving these to PSS would not improve parity with upstream DataScript and would
make serialization and equality behavior more complex.

## Local Dependency

The repo uses the sibling project:

```text
persistent-sorted-set-ocaml/lib -> ../../persistent-sorted-set-ocaml/lib
```

The bridge contains its own `dune-project` so Dune sees
`persistent_sorted_set_ocaml` as a package while importing only the sibling
library. It intentionally does not import the sibling tests or benchmarks into
this workspace.

## Verification Coverage

Important tests covering this design:

- `test_db__test_indexes_use_persistent_sorted_set`
- `test_db__test_index_lookup_matches_upstream_numeric_comparator_bounds`
- `test_datoms_returns_lazy_sequence`
- `test_datoms_slices_before_filtered_predicate`
- `test_vaet_index_returns_ref_datoms_by_value`
- `test_incremental_writes_keep_public_datoms_indexes_correct`
- `test_seek_datoms_scans_forward_from_index_tuple`
- `test_rseek_datoms_scans_backward_from_index_tuple`
- `test_seek_datoms_continues_across_avet_attributes`
- `test_rseek_datoms_continues_across_avet_attributes`
- tuple AVET lookup and range tests in `test_tuples.ml`

Full `dune runtest` is the verification gate for this design. It includes the
native test suite, SQLite storage tests when the opam environment resolves
`sqlite3`, the JS smoke test, and cross-runtime parity checks against the sibling
DataScript checkout.

## Maintenance Rules

- Keep `eavt/aevt/avet/vaet` backed by `Persistent_sorted_set.t`.
- Use the index comparator for all index membership, bounds, and slices.
- Preserve old DB values after every transaction.
- Apply filtered DB predicates after index slicing.
- Prefer bounded PSS slices over whole-index iteration whenever arguments form
  an index prefix.
- Keep non-prefix named-argument combinations correct, even if they require a
  filtered ordered scan.
- Do not serialize PSS internals as part of DB snapshots.
