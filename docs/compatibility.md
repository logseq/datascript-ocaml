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
