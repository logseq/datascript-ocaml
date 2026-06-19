# Identity Collection Output Rules

## Case

q66 and q67 use the Logseq `tags` rule. That rule destructures a runtime tag collection with an `identity` function output:

```clojure
[(identity ?tags) [?spec ...]]
```

Upstream returned `[]` for both queries, while OCaml failed before evaluation with:

```text
expected query variable symbol: ...
```

## Root Cause

The OCaml parser recognized collection output markers such as `[?spec ...]` only for dynamic function inputs and `ground`. Plain/core function calls with vector or list outputs were sent directly to tuple-output parsing.

For `identity`, `[?spec ...]` was therefore parsed as a tuple output vector containing `?spec` and the literal `...`. The tuple parser requires every output element to be a query variable or `_`, so it rejected the marker.

## Fix

`impl/parser.ml` now checks collection and relation output bindings before falling back to tuple output parsing for plain/core function calls. `identity` with a collection output maps to `GroundTermCollection`, matching the existing runtime path used by dynamic `ground` collection outputs.

The same dispatch also handles relation output markers before tuple parsing, so `identity` follows upstream binding semantics for collection-shaped and relation-shaped outputs.

## Upstream Comparison

Upstream DataScript treats `identity` as a value function and applies the output binding shape after the function result is available. With `[?spec ...]`, it iterates the returned collection and binds each value to `?spec`.

The OCaml fix keeps the existing `identity` evaluation model but parses the output binding in the same order: collection and relation markers first, tuple outputs only after marker forms are ruled out.

## Verification

Regression coverage was added for parser-level `identity` collection outputs and for runtime rules that bind values through `[(identity ?tags) [?tag ...]]`.

The q66/q67 comparison slice completed with 0 mismatches.
