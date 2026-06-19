# Type and Implementation Improvements

This report describes how to make the implementation more idiomatic OCaml while
keeping upstream DataScript behavior as the compatibility contract. The goal is
not to make the code more abstract. The goal is to let types describe the real
domain boundaries, remove accidental flexibility, and make hot paths cheaper.

## Current Friction

The port currently keeps many upstream concepts in broad structural types:

- `entity_id`, `tx`, `attr`, and `storage_address` are aliases of `int` or
  `string`.
- `value` represents user values, query literals, storage values, tempid refs,
  and internal sentinels in one variant.
- public records expose implementation details such as DB indexes, storage
  functions, and schema fields.
- query, transaction, pull, serialization, storage, and JS interop all reuse the
  same core types even when they need different guarantees.

This makes porting fast, but it weakens OCaml's type checker. Many invalid
states are representable, so validation moves into runtime branches and hot-path
defensive checks.

## Design Principles

- Keep upstream behavior exact at public compatibility boundaries.
- Use OCaml types to express internal invariants after parsing/validation.
- Prefer small concrete modules over large shared variants.
- Avoid phantom types and GADTs unless they remove a real class of runtime
  checks.
- Keep interop formats at the edges. Core DB code should not carry JS, EDN, or
  storage concerns.
- Optimize data representation only after the type boundary is clear.

## Recommended Direction

### 1. Hide Core Records Behind Modules

Make `db`, `schema_attr`, `storage`, `entity`, and `conn` abstract in public
interfaces. Expose constructors and accessors instead of record fields.

Why:

- callers cannot construct invalid DB values;
- future internal layout changes do not break every consumer;
- storage capability rules become enforceable by interface, not convention.

Start with `db` and `storage`. They have the highest risk because invalid values
can break immutability, restore, or index consistency.

### 2. Split Core Values From Interop Values

Keep the current `value` shape for public API compatibility, but introduce
smaller internal types where the domain is narrower:

- `datom_value` for values that can be stored in indexes;
- `query_value` or `query_atom` for query inputs/results;
- `edn_value` for parsed EDN before validation;
- `js_value` conversion helpers in the JS facade only.

Why:

- transaction code can reject `TxRef`, `Ref_to`, function values, and sentinels
  before index insertion;
- serializers can avoid carrying impossible cases;
- query code can distinguish DB sources, pull results, attrs, refs, and scalar
  values without repeatedly pattern matching the full public value space.

Do this incrementally. A good first cut is to add conversion functions:

```ocaml
val datom_value_of_value : value -> datom_value
val value_of_datom_value : datom_value -> value
```

Then move storage/index code to `datom_value` once tests prove behavior is
unchanged.

### 3. Replace Association Lists On Hot Paths

Several central structures are association lists:

- `schema : (attr * schema_attr) list`
- map values in `value`
- transaction tempid maps
- query bindings

Lists are simple and fine for small compatibility boundaries, but repeated
lookup in transaction/query code is expensive and noisy.

Recommended replacements:

- `StringMap` for schema and query bindings keyed by attr/variable name;
- `IntMap` for entity-oriented transient state;
- keep public results as lists where ordering is observable.

This keeps external behavior stable while giving internal code predictable
lookup costs and clearer intent.

### 4. Make Attribute Names A Real Type

`attr = string` makes these easy to mix:

- raw EDN keyword text;
- internal attr names without leading `:`;
- reverse attrs such as `_friend`;
- namespaced attrs;
- built-in schema attrs.

Introduce an `Attr` module:

```ocaml
module Attr : sig
  type t
  val of_keyword_string : string -> t
  val of_name : string -> t
  val name : t -> string
  val keyword : t -> string
  val is_reverse : t -> bool
  val reverse : t -> t
end
```

Keep `type attr = string` at the public edge at first if compatibility requires
it, but convert to `Attr.t` internally in schema, query planning, and indexes.

### 5. Separate Parsed Transaction Input From Applied Operations

`tx_op` currently mixes public input, already-normalized datoms, transaction
functions, and internal forms:

- `Add`, `Retract`, `Entity` are public input forms.
- `Raw_datom` is an internal restore/tail path.
- `InstallTxFn`, `CallIdent`, and `Call` carry executable OCaml functions.

Split this into:

- `tx_input` for public API and parsers;
- `resolved_tx_op` for validated entity ids and normalized values;
- `stored_tx_datom` for tail/storage replay.

Why:

- storage and JS facade cannot accidentally accept function-carrying operations;
- transaction application can assume refs/tempids are resolved after one phase;
- performance fast paths can pattern match smaller variants.

### 6. Make Storage Capabilities Explicit

The current `storage` record is already simpler after separating write and
delete. The next step is capability-oriented modules:

```ocaml
module Storage : sig
  type t
  type writer
  type gc

  val writer : t -> writer
  val gc : t -> gc
  val store_entries : writer -> storage_entry list -> unit
  val delete_addresses : gc -> storage_address list -> unit
end
```

This is especially useful once `db` is abstract. Normal DB store code should
receive only the write capability. GC receives the delete capability.

### 7. Type Index Prefixes Instead Of Optional Arguments Internally

The public API uses optional arguments:

```ocaml
datoms db Eavt ?e ?a ?v ?tx ()
```

That is ergonomic, but internally it permits non-prefix combinations that cannot
use efficient index slices. Keep the public API, but translate once into an
internal request type:

```ocaml
type eavt_request =
  | Eavt_all
  | Eavt_e of entity_id
  | Eavt_ea of entity_id * attr
  | Eavt_eav of entity_id * attr * value
  | Eavt_scan_filter of datom_filter
```

Do the same for `Aevt` and `Avet`. This makes the fast path obvious and keeps
fallback scans explicit.

### 8. Keep JS/EDN/Storage At The Boundary

The JS facade should convert JS values to typed OCaml transaction/schema/query
input immediately. EDN readers should produce typed input after validation.
Storage should store typed storage payloads, not public transaction forms.

This prevents interop-specific values from leaking into the core and lets the
compiler enforce simpler assumptions.

## Performance Opportunities

### Query

- Represent query bindings with maps or compact arrays after query planning.
- Assign stable variable ids during parse/planning so runtime lookup is integer
  indexed.
- Keep source/DB bindings distinct from scalar bindings in the type system.
- Cache parsed query plans, not just parsed query strings.

### Transactions

- Split validation, resolution, and index update into typed phases.
- Use transient maps/sets for tempids, duplicate facts, and schema changes.
- Keep fast paths small: explicit-id add, tempid allocation, and schema-changing
  transactions should have separate code paths.
- Avoid materializing EAVT unless a fallback path actually needs all current
  facts.

### Schema

- Store schema internally as `AttrMap.t`.
- Precompute per-attr flags needed by transaction and index code:
  cardinality, uniqueness, AVET accessibility, value type, component flag.
- Keep ordered list conversion only for public display/serialization.

### Pull

- Compile pull selectors into an internal plan with attr ids/flags resolved.
- Keep forward-only pull on EAVT slices.
- Keep reverse/wildcard paths separate so the common path does not carry their
  checks.

### Storage

- Keep the upstream root/tail/node shape, but make payload constructors private
  to storage.
- Consider address generation as a storage-owned state instead of a module-level
  global.
- Keep storage node payloads separate from DB snapshots and transaction input.

## Suggested Migration Order

1. Make `db` and `storage` abstract in public interfaces.
2. Add `Attr` and convert schema/query/index internals to use it.
3. Replace internal schema association lists with `AttrMap`.
4. Split `tx_op` into public input and resolved operation phases.
5. Translate public optional index arguments into internal request types.
6. Split `value` internally into datom/query/interop value subsets where useful.
7. Move query bindings to planned variable ids.
8. Tighten storage payload visibility and address ownership.

Each step should preserve public behavior and include focused tests. Avoid
combining type changes with semantic fixes; the compiler will already create
enough call-site work for each step.

## Non-Goals

- Do not mirror upstream Clojure implementation details where OCaml has a
  clearer type boundary.
- Do not add a large framework of phantom types before simpler modules and
  private types have been tried.
- Do not expose internal maps or compiled plans as public API.
- Do not optimize by changing lazy index access, datom ordering, equality,
  storage layout, or transaction report behavior.

