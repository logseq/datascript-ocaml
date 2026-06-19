# Logseq Tagged Page Ref Join Performance

## Case

Logseq query `q6`:

```clojure
[:find
 ?p
 ?ref-page
 :where
 [?block :block/page ?p]
 [?p :block/tags]
 (not [?p :block/tags :logseq.class/Journal])
 [?block :block/refs ?ref-page]]
```

timed out in OCaml under the upstream elapsed time plus one second budget.

## Root Cause

The query joins `:block/page` and `:block/refs`, but only for pages that have any `:block/tags` and do not have the Journal tag.

OCaml had no planner for this shape, so it fell back to the generic evaluator. The first planner attempt still used a per-block exact slice for `:block/refs`; that matched the small regression test but remained too slow on the real Logseq graph because the current OCaml slice path is expensive when repeated many times.

## Fix

`planned_simple_query` now recognizes the q6 shape and evaluates it as indexed set joins:

1. Resolve the excluded Journal tag through the normal attr-aware value path.
2. Build a set of tagged non-Journal pages.
3. Scan `:block/refs` once into `refs_by_block`.
4. Scan `:block/page` once and emit `[?p ?ref-page]` rows for matching pages.

## Upstream Comparison

Upstream DataScript evaluates the selective relations without repeatedly re-scanning the whole graph for each block. The OCaml planner follows the same relation-oriented strategy by batching the refs relation before joining it with page datoms.

## Verification

Added focused coverage in `test/test_logseq_query_planners.ml`.

The q6 isolated Logseq replay against the saved graph EDN completes with 0 mismatches:

```text
upstream q6 elapsed-ms 17913
ocaml q6 elapsed-ms 303
runnable: 115 skipped: 87 mismatches: 0
```
