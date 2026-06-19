# Rule Prefix Relation Performance

## Case

q42 uses the Logseq `property` rule:

```clojure
[:find [?p ...]
 :where
 (property ?b ?p "bar")
 [?b :block/title "Page1"]
 :in $ %]
```

After rule inputs were enabled, OCaml timed out while upstream returned `[]`. The same batch also exposed timing failures for adjacent `property` queries, including q45 and q46.

## Root Cause

Inside the `property` rule body, OCaml evaluated positive rule prefixes and `or` branches one binding at a time while rule expansion was active:

```clojure
[?prop-e :db/ident ?prop]
[?prop-e :block/tags :logseq.class/Property]
(or
 [(missing? $ ?prop-e :logseq.property/public?)]
 [?prop-e :logseq.property/public? true])
[?b ?prop ?pv]
```

That repeatedly rebuilt branch work for each binding and prevented the hash-join relation path from seeing the whole active binding set. Upstream DataScript keeps the current context as relations, solves each `or` branch against that relation context, hash-joins branch relations, then unions them with `sum-rel`.

## Fix

`impl/query_where.ml` now:

- evaluates relation-compatible positive prefixes during active rule expansion with the relation/hash-join path;
- batches active-rule `or` branch evaluation across the current binding set instead of evaluating each binding separately;
- uses known bound dynamic attribute values as indexed pattern inputs, so `[?b ?prop ?pv]` can use attr-specific scans when `?prop` is already bound.

The optimization remains limited to active rule contexts. A broader top-level rewrite changed error ordering and regressed ordinary query behavior, so it was removed.

## Upstream Comparison

Upstream `datascript/query.cljc` expands rules with `solve-rule`. It solves rule prefixes with `-resolve-clause`, collapses relations with `hash-join`, evaluates `or` branches as relation contexts, and combines branch results with `sum-rel`. The OCaml fix follows that behavior for active rule contexts without reordering top-level query clauses.

## Verification

The focused q36-q43 slice completes with 0 mismatches. The full runnable batch `20 20` also completes with 0 mismatches:

```text
runnable: 115 skipped: 87 mismatches: 0
```
