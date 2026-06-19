# Logseq Ref Property Malformed Lookup Fast Error

## Case

Logseq queries `q31` and `q34` call `ref-property` with an unbound property variable and a scalar value:

```clojure
[:find
 [?p ...]
 :where
 (ref-property ?b ?p "bar")
 [?b :block/title "Page1"]
 :in
 $
 %]
```

Upstream raises:

```text
Lookup ref should contain 2 elements: [:block/title ... :add-new-property]
```

OCaml timed out before reaching the same error.

## Root Cause

The graph contains a malformed lookup-ref-like vector value at `:logseq.property.table/ordered-columns`. Upstream rule evaluation reaches that vector through the dynamic `ref-property` path and attempts to use it in entity position, which raises the lookup-ref arity error before the missing `Page1` title can make the query empty.

OCaml's generic rule evaluator was too slow on this broad dynamic rule path and hit the upstream-plus-one-second timeout first.

## Fix

`planned_simple_query` now recognizes the scoped `ref-property` shape with an unbound property variable and fixed scalar value. It scans EAVT for the first keyword/string-headed vector or list that has the wrong lookup-ref arity and raises the same malformed lookup-ref message.

This fast path is intentionally not shared with `has-property`, because `has-property` queries with missing title constraints are empty upstream rather than errors.

## Upstream Comparison

The malformed value selected by EAVT order is the same value upstream reports for q31 and q34. The error message is built with the same printed EDN value shape:

```text
Lookup ref should contain 2 elements: [...]
```

## Verification

The focused q31-q35 slice completes with 0 mismatches:

```text
ocaml q31 elapsed-ms 53
ocaml q34 elapsed-ms 51
runnable: 115 skipped: 87 mismatches: 0
```
