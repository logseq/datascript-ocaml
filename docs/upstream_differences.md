# Upstream Differences

Date: 2026-06-19

This report compares the current OCaml DataScript implementation with the local
upstream DataScript checkout, and compares the current OCaml persistent sorted
set implementation with the local upstream persistent-sorted-set checkout.

The compatibility goal is exact upstream behavior. The current automated gates
are strong, but they do not prove exactness across every upstream surface.

## Source Snapshot

| Project | Path | Revision / state |
| --- | --- | --- |
| DataScript OCaml | repository root | `5c7d2a07114ee292808c3718f369031880e9ed97` before this report |
| upstream DataScript | `<upstream-datascript>` | `3f141af97b70e1f14c65eaa119acd822ebece37e`; untracked `.lsp/` |
| persistent-sorted-set OCaml | `<persistent-sorted-set-ocaml>` | `f4ecbe720de97e08210a538c122bfdd59b98163d` |
| upstream persistent-sorted-set | `<upstream-persistent-sorted-set>` | `efc3add9af192abc64711bb58c3d1d2352097129`; `project.clj` version changed from `0.1.1` to `0.1.2`, plus untracked editor/cache dirs |

## Verification Run

These commands passed:

```sh
UPSTREAM_TEST_DIR=<upstream-datascript>/test/datascript/test \
  bash script/diff_upstream_tests.sh

UPSTREAM_DATASCRIPT_REPO=<upstream-datascript> \
UPSTREAM_DATASCRIPT_JS=<upstream-datascript>/release-js/datascript.js \
  dune runtest

cd <persistent-sorted-set-ocaml>
bash script/diff_upstream_tests.sh
dune runtest
```

Coverage script results:

| Area | Upstream tests | Covered by exact name or alias | Missing name coverage | Stale aliases |
| --- | ---: | ---: | ---: | ---: |
| DataScript | 170 | 170 | 0 | 0 |
| persistent-sorted-set | 15 | 15 | 0 | 0 |

## Confirmed Aligned Under Current Tests

DataScript OCaml currently passes the local unit tests, JS smoke tests, and
cross-runtime parity against the configured upstream DataScript checkout. The
covered upstream areas include transaction behavior, upserts, lookup refs,
components/explode, entity access, pull, parser/query forms, return maps, tuple
attributes, index APIs, filters, serialization, storage round trips, and common
validation messages.

The core DB shape is intentionally close to upstream: `EAVT`, `AEVT`, and `AVET`
are backed by persistent sorted sets, and there is no separate `VAET` index.
Index order, bounded access, AVET accessibility, filtered DB slicing, and lazy
public `datoms` behavior are covered by tests and documented in `docs/design.md`.

Persistent-sorted-set OCaml passes the upstream test-name coverage check and its
native/js_of_ocaml test suite. Covered areas include sorted order, uniqueness,
slice/rslice boundaries, seekable sequences, stress cases, restored storage
laziness, stable address reuse, and walk-addresses behavior.

## Exactness Blockers

These are current differences from upstream surfaces or implementation details.
They should be treated as follow-up work if exact upstream parity is required.

### DataScript: Upstream `datafy` Is Not Implemented

Upstream has `src/datascript/datafy.cljc`, which extends Clojure
`Datafiable`/`nav` behavior for DataScript entities. This repo has no
corresponding implementation; `rg datafy` only finds the upstream-test alias
file and comments/tests around query-v3 validation.

The alias file explicitly marks `datafy.cljc test-navigation` as `-`, which
means it is an intentional non-goal today. For exact upstream surface parity,
this needs either an OCaml equivalent or an explicitly documented permanent
runtime divergence.

### DataScript: `query_v3` Is Only Partially Represented

Upstream has `src/datascript/query_v3.cljc`, including native relation
implementations and a no-doc query engine surface. The OCaml repo does not have
a `query_v3` module. The upstream alias maps only `query_v3.cljc
test-validation` to `test_q_input_arity_matches_upstream_validation_messages`.

The tested validation behavior matches, but the full upstream `query_v3`
implementation is not ported as a separate surface.

### DataScript: Entity Construction Is Not Lazy

Upstream DataScript entities behave as lazy entity views: constructing an entity
does not require building the full attribute map, and attributes are resolved
when requested.

The OCaml `entity` type currently stores a concrete `attrs` list. Calling
`entity db entity_ref` resolves the entity id, materializes forward attributes
from the EAVT entity slice, scans visible datoms for reverse attributes, sorts
the resulting attributes, and then returns the entity record. Referenced entity
values are still expanded later by `entity_attr`, but the base entity attribute
map is already materialized.

The current behavior can still match observable entity results, but it is an
implementation divergence from upstream laziness and can affect performance for
call sites that only need one attribute from an entity. Exact upstream parity
should move entity values back toward lazy views and make `entity_attr` perform
bounded per-attribute index access.

### DataScript: Seek And Range APIs Materialize Lists

Public `datoms` and `datoms_ref` return `Seq.t` and preserve lazy index access
for normal datom reads. The adjacent seek and range APIs are still eager:

- `seek_datoms` / `seek_datoms_ref`
- `rseek_datoms` / `rseek_datoms_ref`
- `index_range`

Those functions return `datom list` and force the requested range before
returning. Upstream exposes these reads as sequence/iterator-style results, so
callers can consume only the prefix they need. Exact upstream laziness would
change these OCaml APIs, or add lazy variants and route compatibility layers
through them.

### DataScript: Direct Lookup Refs Scan Visible Datoms

Some unique-value checks already use the `AVET` index, including transaction
conflict checks. Direct `entid` and lookup-ref resolution still go through
`visible_datoms` and then scan a list for the matching unique attribute/value.

That preserves result behavior, but it is an eager path where upstream can use
indexed lookup. Exact parity should route direct lookup-ref resolution through a
bounded `AVET` lookup, while keeping transaction-local datom-list paths for
staged transaction state.

### DataScript: Public Options Are Narrower Than Upstream

Upstream `empty-db`, `init-db`, `create-conn`, and restore paths accept option
maps with storage and persistent-set settings such as `:branching-factor` and
`:ref-type`. The OCaml public API exposes `?storage` but not general upstream
option maps:

- `empty_db : ?schema:schema -> ?storage:storage -> unit -> db`
- `init_db : ?schema:schema -> ?storage:storage -> datom list -> db`
- `create_conn : ?schema:schema -> ?storage:storage -> unit -> conn`

`Storage.settings` returns the active index settings for `"branching-factor"`
and `"ref-type"`, but the public constructors still do not expose the full
upstream option map.

### DataScript: Storage Wire Format Is Different

Upstream DataScript storage stores persistent-sorted-set node roots and metadata
under integer root/tail addresses (`0` and `1`) and keeps index roots separately.
The OCaml core storage layer stores:

- `"datascript/root"` -> `Storage_db serializable_db`
- `"datascript/tail"` -> `Storage_tail datom list list`

That means the OCaml storage API round-trips DB behavior, but the core storage
payload shape is not upstream-compatible. The Logseq SQLite storage example has
additional compatibility logic, but the main `Datascript.Storage` abstraction is
not the same as upstream node-level storage.

### DataScript: Serialization Hooks Are Narrower Than Upstream

Upstream `serializable` and `from-serializable` accept freeze/thaw options for
non-primitive values. The OCaml API serializes the typed `serializable_db`
record. This is a reasonable typed-port boundary, but it is not an exact match
for upstream's arbitrary Clojure value serialization hooks.

### DataScript: JS Facade Is Not an Upstream API Match

Upstream has `src/datascript/js.cljs`, exporting JavaScript functions such as
`empty_db`, `init_db`, `q`, `pull`, `db_with`, `transact`, `datoms`, and
`index_range` with JS data conversion. This repo supports `js_of_ocaml`, but it
does not provide an equivalent `datascript.js` facade with the same argument and
conversion behavior.

If JS consumers need exact upstream API parity, this should be tracked as a
separate compatibility layer rather than as an internal OCaml implementation
detail.

### Persistent Sorted Set: Clojure Collection Semantics Are Not Exact

Upstream persistent-sorted-set is a near drop-in replacement for Clojure sorted
sets. It supports Clojure collection protocols, metadata, `seq`/`rseq`, reduce,
transients, and mutation-during-iteration checks.

The OCaml port exposes a typed API instead. It has no Clojure metadata or
transient collection API. The upstream `iter-over-transient` test is explicitly
marked as `-` in the OCaml alias file, so this is a known non-goal today.

### Persistent Sorted Set: Slice API Shape Differs

Upstream `slice` and `rslice` return efficient iterator/sequence objects that
support normal Clojure sequence operations and efficient `rseq`. The OCaml API
has:

- `slice` / `rslice` returning lists
- `slice_seq` / `rslice_seq` returning the custom lazy sequence type

DataScript OCaml can use the sequence APIs to preserve lazy index behavior, but
the public PSS API is not an exact upstream API match.

### Persistent Sorted Set: Restored Sequence And Count Paths Can Materialize

Upstream restored sorted sets keep enough iterator and metadata state to support
lazy traversal and cheap count when restore metadata is available. The OCaml PSS
port has lazy restored-path coverage for several range/list operations, but some
paths still force more than upstream:

- `to_seq` over restored/deferred sources materializes the restored tree before
  yielding the first element.
- `count` traverses or materializes restored sets because the OCaml storage
  shape does not keep upstream-style count/depth metadata.

This can affect DataScript storage-backed indexes when a path converts a
restored PSS sequence to `Seq.t` or counts a restored index. Exact upstream
parity would keep restored iteration cursor-based and preserve count metadata
through restore.

### Persistent Sorted Set: Settings And Storage Shape Differ

Upstream settings include at least `:branching-factor` and `:ref-type`
(`:strong`, `:soft`, `:weak`). The OCaml PSS settings type models both fields;
OCaml treats `Soft` as reclaimable like `Weak` because OCaml does not expose JVM
soft-reference pressure heuristics.

The storage APIs also differ. Upstream `store` returns a root address and works
with Clojure/JVM or CLJS storage protocols. OCaml `store` returns
`string * 'a t`, uses an OCaml `stored_node` shape, and exposes storage as:

```ocaml
type 'a stored_node =
  | Leaf of 'a list
  | Branch of 'a list * string list
```

This is behaviorally covered for OCaml storage tests, but it is not the same
wire/API surface as upstream.

## Not Currently Classified As Differences

These areas are intentionally not listed as blockers because the current
evidence shows parity or there is no upstream surface in the checked-out source:

- Historical DB behavior: the current upstream checkout does not expose a
  `history` implementation; the OCaml API keeps compatibility functions where
  `history` returns current facts and `is_history` is false.
- Performance differences: benchmark numbers differ by runtime and operation,
  but performance is not observable DataScript semantics unless it changes
  laziness, ordering, storage, or mutation behavior.
- Internal module naming: OCaml modules are not expected to mirror upstream
  namespaces exactly where the public behavior and data model are covered.

## Recommended Next Work

1. Decide whether "exact" includes upstream runtime-specific APIs (`datafy`,
   `query_v3`, JS facade, Clojure collection protocols). If yes, add explicit
   tracking tests for each currently ignored surface instead of leaving them as
   aliases to `-`.
2. Bring storage closer to upstream if storage-file compatibility matters:
   model root/tail addresses and PSS node payloads rather than whole-DB
   snapshots in the core storage API.
3. Add upstream option-map support, especially branching factor and ref type, or
   document them as permanent typed-OCaml divergences.
4. For PSS, decide whether public `slice`/`rslice` should return lazy sequence
   values by default to match upstream API shape more closely.
5. Add lazy seek/range APIs for `seek_datoms`, `rseek_datoms`, and
   `index_range`, then route compatibility layers through those APIs.
6. Route direct lookup-ref resolution through bounded `AVET` lookup instead of
   scanning `visible_datoms`, while preserving transaction-local datom-list
   behavior.
7. Rework entity values into lazy views so constructing an entity does not
   materialize all forward and reverse attributes before `entity_attr` access.
8. Keep restored PSS `to_seq` and `count` lazy/metadata-backed enough for
   storage-backed DataScript indexes.
9. Keep the existing coverage scripts and cross-runtime parity gate as required
   checks, but treat them as necessary rather than sufficient for exactness.
