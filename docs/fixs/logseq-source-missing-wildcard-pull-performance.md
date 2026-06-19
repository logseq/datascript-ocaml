# Logseq Source Missing Wildcard Pull Performance

## Case

Logseq query `q132`:

```clojure
[:find
 (pull ?p [*])
 :where
 [?b :block/title]
 [?b :block/page ?p]
 [(missing? $ ?p :logseq.property/built-in?)]]
```

timed out in OCaml while upstream DataScript completed within the allowed budget.

## Root Cause

The optimized page-pull planner only matched the source-less internal clause:

```ocaml
Missing (QVar missing_var, QAttr missing_attr)
```

The Logseq query spells the database source explicitly as `(missing? $ ...)`, which the parser represents as:

```ocaml
SourceMissing ("$", QVar missing_var, QAttr missing_attr)
```

Upstream treats `$` as the default database source for this query. OCaml did not recognize that equivalent parsed shape, so q132 fell back to the generic query evaluator and took more than 60 seconds on the saved Logseq graph.

## Fix

`planned_simple_query` now accepts both `Missing` and `SourceMissing "$"` for the page-pull-with-missing planner. The optimized planner can therefore run for both clause orders:

```clojure
[?b :block/title]
[?b :block/page ?p]
[(missing? $ ?p :logseq.property/built-in?)]
```

and:

```clojure
[?b :block/page ?p]
[?b :block/title]
[(missing? $ ?p :logseq.property/built-in?)]
```

The runner also parses each query once and evaluates the parsed query directly, avoiding duplicate pull-query parsing.

## Upstream Comparison

Upstream `datascript.built-ins/-missing?` checks `(de/entity db e)` for the supplied source database. For the default `$` source, the explicit-source form is semantically the same as the source-less planner input OCaml already optimized.

## Verification

Added focused coverage in `test/test_logseq_query_planners.ml` for the q132 shape with explicit `$` in `missing?`.

The q132 isolated Logseq replay against the saved graph EDN completes with 0 mismatches:

```text
upstream q132 elapsed-ms 3093
ocaml q132 elapsed-ms 129
runnable: 115 skipped: 87 mismatches: 0
```
