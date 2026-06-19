# Compatibility

This port aims to match upstream DataScript behavior for database semantics while
using OCaml-specific APIs where upstream exposes runtime-specific surfaces.

## Storage Compatibility

The OCaml storage layout follows upstream DataScript's root/tail/index-node
shape:

- `"0"` stores root metadata, including schema, max entity/tx values, and EAVT,
  AEVT, and AVET root addresses.
- `"1"` stores persisted transaction tail groups.
- generated numeric addresses store persistent-sorted-set leaf and branch nodes.

The storage API remains typed for OCaml callers, and stores use the upstream
root/tail/index-node layout.

## JS Facade Compatibility

The js_of_ocaml facade exports the same JavaScript-oriented entry points as
upstream `datascript/js.cljs`, including `empty_db`, `init_db`, `q`, `pull`,
`pull_many`, `db_with`, `create_conn`, `transact`, `datoms`, `seek_datoms`, and
`index_range`.

The facade accepts JavaScript schema objects and transaction entity maps, keeps
DB and connection values as opaque handles between calls, and converts query,
pull, datom, and transaction report results back to JavaScript values.
