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

The js_of_ocaml build is not an upstream-compatible JavaScript facade.

Upstream DataScript exposes `datascript/js.cljs`, which converts JavaScript data
to ClojureScript values and exports JavaScript-oriented functions such as
`empty_db`, `init_db`, `q`, `pull`, `db_with`, `transact`, `datoms`, and
`index_range`.

This repository supports compiling the OCaml implementation with js_of_ocaml and
tests that runtime path, but it does not expose the upstream JavaScript API
shape or conversion behavior. JavaScript consumers should treat the generated
js_of_ocaml output as an OCaml runtime artifact, not as a drop-in replacement for
upstream `datascript.js`.
