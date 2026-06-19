# Malformed Lookup Ref Entity Values

## Case

Logseq `ref-property` rules can bind a vector value and later use it in entity position:

```clojure
(ref->val ?pv ?val)
;; expands to
[?pv :block/title ?val]
```

For q45/q48, upstream reports:

```text
Lookup ref should contain 2 elements: [:block/title ... :add-new-property]
```

## Root Cause

OCaml previously treated malformed list/vector values in entity position as non-matching values and returned no rows. After fixing dynamic attr joins, OCaml reached the same rule path but raised a different error first because it validated large integer values as entity ids during query search.

Upstream distinguishes these cases:

- malformed sequential entity values are lookup-ref syntax errors;
- plain numeric entity pattern values are used as search keys and do not raise just because they exceed the normal entity id range.

## Fix

`impl/query.ml` now raises the upstream-compatible lookup-ref arity message when a keyword/string-headed list or vector with the wrong length is resolved as an entity value.

`impl/datascript.ml` now treats `QValue (Int eid)` in pattern entity search as a search key, not as a validated entity id. This lets broad rule scans skip timestamp-like values and reach the same malformed vector that upstream reports.

## Upstream Comparison

Upstream `datascript/db.cljc` raises `"Lookup ref should contain 2 elements"` from `entid` for malformed sequential lookup refs. Upstream `datascript/query.cljc` only calls `entid-strict` for lookup refs or attr keywords in `resolve-pattern-lookup-refs`; numeric entity pattern values remain raw search values.

## Verification

Regression tests cover malformed lookup refs reached through direct and nested rule entity matching. q45 and q48 single-query comparisons now both report 0 mismatches.
