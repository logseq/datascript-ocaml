# Dynamic Attribute Keyword Joins

## Case

Queries can bind an attribute keyword from `:db/ident` and reuse that variable in attribute position:

```clojure
[:find ?b
 :where
 [?prop-e :db/ident ?prop]
 [?b ?prop "bar"]]
```

## Root Cause

OCaml has separate internal result constructors for datom attributes (`Result_attr`) and ordinary values (`Result_value (Keyword ...)`). Upstream DataScript represents attributes as keywords in query relations, so the join key for `?prop` is the same value in both clauses.

The OCaml fast relation hash join used structural equality for row keys. It treated `Result_value (Keyword "user.property/foo")` from `:db/ident` and `Result_attr "user.property/foo"` from datom attribute position as different, dropping valid joins.

## Fix

`impl/query.ml` now treats keyword attr values and attr results as equivalent. `impl/query.ml` also converts bound keyword attr values to `QAttr` when planning attribute-position index scans. `impl/query_where.ml` normalizes `Result_attr attr` to the corresponding keyword value only for relation join keys.

## Upstream Comparison

In upstream `datascript/query.cljc`, pattern relations use datom tuples where the attr column is the keyword attr itself. Joining a `:db/ident` keyword with an attr column is therefore ordinary value equality. The OCaml fix preserves its internal `Result_attr` representation but normalizes equality at the same observable boundary.

## Verification

Regression coverage was added for keyword attr values reused as attribute variables and for lower-level pattern matching. `test/test_datascript.exe` and `test/test_query_namespace.exe` pass.
