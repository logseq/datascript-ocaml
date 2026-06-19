# Logseq Tag Value Not Attr Performance

## Case

Logseq queries `q15` and `q16` use a fixed tag value and exclude entities that have an attribute:

```clojure
[:find
 [?db-ident ...]
 :where
 [?p :db/ident ?db-ident]
 [?p :block/tags :logseq.class/Property]
 (not [?p :logseq.property/built-in?])]
```

```clojure
[:find
 [?class ...]
 :where
 [?class :block/tags :logseq.class/Tag]
 (not [?class :logseq.property/built-in?])]
```

Both timed out in OCaml under the upstream elapsed time plus one second budget.

## Root Cause

OCaml already had a planner for a fixed value pattern, a projected value attr, and a missing attr exclusion, but only for one clause order. It did not recognize the q15 order where the projected value attr appears before the fixed tag value.

OCaml also did not have a planner for the simpler q16 shape: fixed value pattern plus `not` attr-present exclusion returning the entity itself. That fell back to the generic evaluator.

## Fix

`planned_simple_query` now:

- accepts the reversed clause order for fixed value + projected value + excluded attr;
- adds an indexed difference planner for fixed value + excluded attr.

Both planners resolve the fixed tag value through the normal attr-aware value resolution path, so class keywords resolve to their referenced ident entities for `:block/tags`.

## Upstream Comparison

Upstream evaluates the fixed tag value as a selective indexed relation and applies `not` against that smaller relation context. The OCaml planner mirrors that strategy by building the excluded entity set once and filtering the indexed fixed-value candidates.

## Verification

Added focused coverage in `test/test_logseq_query_planners.ml`.

In the `0 20` runnable batch, q15 and q16 now return before their upstream-derived timeout budgets:

```text
ocaml q15 elapsed-ms 265
ocaml q16 elapsed-ms 277
```
