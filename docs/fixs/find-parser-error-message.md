# Find Parser Error Message

## Case

q57 is a non-query map form extracted from Logseq parser code:

```clojure
{:error :parser/query
 :return-map (cons (:type (:qreturn-map q)) return-symbols)
 :find find-elements
 :form form}
```

Upstream reports:

```text
Cannot parse :find, expected: (find-rel | find-coll | find-tuple | find-scalar)
```

OCaml reported:

```text
expected query variable symbol: find-elements
```

## Root Cause

OCaml reused the general query-variable parser for bare `:find` symbols. That parser reports the lower-level reason a symbol is not a query var.

Upstream DataScript reports malformed `:find` shapes through the higher-level find parser, so the observable error message is the generic `Cannot parse :find...` message.

## Fix

`impl/parser.ml` now uses a find-specific variable parser for bare find symbols. It still accepts normal query vars such as `?e`, but non-query symbols in `:find` raise the upstream-compatible `Cannot parse :find...` message.

## Upstream Comparison

Upstream parses find return shapes as `find-rel`, `find-coll`, `find-tuple`, or `find-scalar`. A bare non-query symbol does not match any valid find shape, so upstream reports the generic find parse failure instead of a variable-symbol detail.

## Verification

Regression coverage was added for malformed map-form `:find`. q57 now matches upstream, and the runnable batch `20 20` completes with 0 mismatches.
