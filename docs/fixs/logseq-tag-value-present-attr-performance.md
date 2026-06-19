# Logseq Tag Value Present Attr Performance

## Case

Logseq query `q191`:

```clojure
[:find
 [?e ...]
 :where
 [?e :block/tags :logseq.class/Tag]
 [?e :block/uuid]]
```

timed out in OCaml under the upstream elapsed time plus one second budget.

## Root Cause

This query has a fixed value pattern followed by an attr-present pattern for the same entity. Upstream can solve it by using the indexed `:block/tags` value lookup first, then checking `:block/uuid` only for the small matching entity set.

OCaml had planners for two fixed value patterns and for some pull-specific attr-present shapes, but not for this fixed-value plus attr-present shape. It fell back to the generic evaluator, which was too slow on the Logseq graph.

## Fix

`planned_simple_query` now recognizes both clause orders:

```clojure
[?e :block/tags :logseq.class/Tag]
[?e :block/uuid]
```

and:

```clojure
[?e :block/uuid]
[?e :block/tags :logseq.class/Tag]
```

The new planner resolves the fixed value through the normal attr-aware value resolution path, then checks present-attr existence for each matched entity.

## Upstream Comparison

The fixed tag value uses the same value resolution path as other indexed value planners, so `:logseq.class/Tag` resolves to the referenced ident entity for the `:block/tags` ref attribute, matching upstream DataScript query semantics.

## Verification

Added focused coverage in `test/test_logseq_query_planners.ml` for the q191 shape.

The `100 20` runnable batch now completes with 0 mismatches:

```text
ocaml q191 elapsed-ms 1534
runnable: 115 skipped: 87 mismatches: 0
```
