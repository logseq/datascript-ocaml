# Non-PSS LMDB Index Design

Branch: `feat/non-pss`

This document records the architecture for replacing `Persistent_sorted_set` indexes
with native LMDB indexes. Queries and writes go directly to LMDB. Datoms keep `tx`,
`added`, and DataScript `value` types (not Datalevin AVG/aid encoding).

## Type strategy: Scheme A

Replace PSS field types in `db` with an explicit `Index.t`. Do not alias
`Persistent_sorted_set` to LMDB.

### New public types (`type/datascript_types.ml`)

```ocaml
module Index : sig
  type t
  type 'a seq
  val to_seq : 'a seq -> 'a Seq.t
end

type index = Index.t

and db = {
  db_uid : int;
  schema : schema;
  eavt_index : index;
  aevt_index : index;
  avet_index : index;
  (* caches and duplicate side tables unchanged *)
  ...
  storage_ref : storage option;
  ...
}
```

`Index.t` is abstract at the type level. Native builds use LMDB; the type does not
mention PSS or LMDB in `datascript_types`.

### Index module API (`lmdb/datascript_lmdb_index.mli`)

Surface area required by `impl/db.ml` (PSS-free subset):

```ocaml
type t
type 'a seq

val empty : index -> t
val of_sorted_list : index -> datom list -> t

val add : datom -> t -> t
val remove : datom -> t -> t

val to_list : t -> datom list
val fold : (acc -> datom -> acc) -> acc -> t -> acc

val slice : ?from_:datom -> ?to_:datom -> ?cmp:(datom -> datom -> int) -> t -> datom list
val slice_seq : ?from_:datom -> ?to_:datom -> ?cmp:(datom -> datom -> int) -> t -> datom seq
val rslice_seq : ?from_:datom -> ?to_:datom -> ?cmp:(datom -> datom -> int) -> t -> datom seq
val seq : t -> datom seq
val seq_to_list : datom seq -> datom list
val fold_seq : (acc -> datom -> acc) -> acc -> datom seq -> acc
val to_seq : datom seq -> datom Seq.t
```

Notes:

- `index` parameter is `Eavt | Aevt | Avet` (`Datascript_types.index`).
- Default comparator is `Util.compare_datom index`; bound slices pass custom `cmp`
  (same as PSS today).
- `t` holds a reference to the shared LMDB environment and the DBI name for that
  logical index (`ds/eavt`, `ds/aevt`, `ds/avet`).

### Shared LMDB database handle

Introduce `Lmdb_db.t` (one env per storage session):

```ocaml
type t = {
  env : Lmdb.Env.t;
  eavt : (bytes, bytes, [`Uni]) Lmdb.Map.t;
  aevt : (bytes, bytes, [`Uni]) Lmdb.Map.t;
  avet : (bytes, bytes, [`Uni]) Lmdb.Map.t;
  meta : (string, bytes, [`Uni]) Lmdb.Map.t;
  path : string;
}
```

Each `Index.t` is `{ db : Lmdb_db.t; which : index }` or three dedicated handles
created at `empty_db` / `restore`.

`storage` on `feat/non-pss` simplifies to wrapping `Lmdb_db.t`:

```ocaml
type storage = Lmdb_db.t
```

Remove PSS snapshot payloads (`Storage_root`, `Storage_node`, tail groups) from the
native non-PSS path. Logseq SQLite reader code stays on other branches.

## LMDB key/value encoding

Keys are order-preserving binary tuples matching `Util.compare_datom`:

| Index | Key field order |
| --- | --- |
| EAVT | e, a, v, tx |
| AEVT | a, e, v, tx |
| AVET | a, v, e, tx |

- `v` uses a typed order-preserving encoder aligned with `Util.compare_value`.
- `tx` is included in the key (unlike Datalevin).
- Value payload stores at least `added : bool` and may mirror `v` for simpler decode.

Duplicate numeric facts (`Int 1` vs `Float 1.0` comparator-equal but distinct) stay
in side tables (`duplicate_*` fields on `db`) with merge at read time, same as today.

## Code migration map (Scheme A)

| Area | Change |
| --- | --- |
| `type/datascript_types.ml` | `eavt_index` etc. become `Index.t`; drop PSS from `db` |
| `impl/db.ml` | `module PSet = ...` removed; call `Lmdb_index.*` |
| `impl/datascript.ml` | Replace direct `PSet.*` on `db.*_index` |
| `impl/serialize.ml` | Iterate LMDB or `to_list`; no PSS builders |
| `impl/storage.ml` | Open/sync `Lmdb_db`; delete PSS store/restore/tail |
| `impl/conn.ml` | Commit LMDB txn per transact; no tail compaction |
| `lmdb/*` | `datascript_lmdb_codec.ml`, `datascript_lmdb_index.ml`, `datascript_lmdb_db.ml` |
| `impl/dune` | Native links `datascript_lmdb_index`; drop `persistent_sorted_set_ocaml` on this branch |
| `test/test_db.ml` | Replace `assert_uses_persistent_sorted_set` with LMDB index checks |
| `bench/*` | Update labels; expect no snapshot full-tree rewrite |

## What stays unchanged

- Public query semantics: lazy `datoms`, bound slices, filtered DB order.
- `datom` record including `tx` and `added`.
- `value` algebra and `Util.compare_value` rules.
- Three indexes: EAVT, AEVT, AVET.

## Implementation phases

1. **Codec + Index unit tests** — key order matches `compare_datom`.
2. **`Lmdb_db` lifecycle** — temp env for `empty_db ()`, path-backed for conn.
3. **Rewire `db.ml`** — Scheme A types throughout.
4. **Storage + conn** — remove PSS snapshot/tail.
5. **Tests + bench** — parity vs PSS branch on small fixtures; benchmark tables.

## Explicit non-goals (this branch)

- js_of_ocaml / melange non-PSS.
- Logseq PSS `db.sqlite` compatibility.
- Datalevin-style aid/AVG encoding or dropping `tx` from storage.
