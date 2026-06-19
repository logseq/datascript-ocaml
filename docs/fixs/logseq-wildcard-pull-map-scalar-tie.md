# Logseq Wildcard Pull Map Scalar Tie

## Case

Logseq query `q131`:

```clojure
[:find (pull ?b [*]) :where [?b :block/name]]
```

returned a different `:logseq.property/icon` value for a page entity that had duplicate cardinality-one icon datoms at the same transaction. Upstream returned the map value shaped like `{:id "robot_face" :emoji :type}`, while OCaml returned `{:id "robot_face" :type :emoji}`.

## Root Cause

Upstream DataScript compares unequal Clojure maps through its non-comparable value fallback. In `datascript.db/value-compare`, maps are not sequential or comparable, so unequal maps fall through to `clojure.lang.Util/hasheq`.

The OCaml port compared map values structurally. That reversed the EAVT ordering for the two icon map values. Wildcard pull then selected the wrong scalar value for a cardinality-one attribute with multiple datoms.

The optimized wildcard pull path also sorted scalar candidates only by tx. Upstream pull walks EAVT order, so scalar replacement must follow `(value, tx)` EAVT ordering, with the last candidate winning.

## Fix

`Util.compare_value` now uses a Clojure-compatible hash fallback for same-type `Map` and `Set` values instead of structural ordering. The fallback covers the value kinds present in the Logseq corpus, including keywords, symbols, ordered and unordered collections, maps, UUIDs, refs, and instants.

`fast_wildcard_pull_rows` now selects cardinality-one scalar values by the last candidate in EAVT `(value, tx)` order, matching upstream wildcard pull iteration.

## Upstream Comparison

Checked against the local upstream DataScript implementation:

```clojure
(db/value-compare {:id "robot_face" :type :emoji}
                  {:id "robot_face" :emoji :type})
;; => -1
```

Upstream sorts the normal icon map before the inverted icon map, so wildcard pull keeps the inverted map as the final scalar value.

## Verification

Added focused coverage:

- `test/test_util.ml` asserts map value ordering matches the upstream comparison for the icon maps.
- `test/test_logseq_query_planners.ml` asserts the q131-style wildcard pull keeps the upstream scalar value when duplicate cardinality-one map datoms tie on tx.
- `test/test_datascript.ml` updates map datom ordering expectations to upstream map hash ordering.

Commands run:

```sh
rtk dune exec -- test/test_util.exe
rtk dune exec -- test/test_logseq_query_planners.exe
rtk dune exec -- test/test_pull_api.exe
rtk dune exec -- test/test_query_pull.exe
rtk gtimeout 180s dune exec -- test/test_datascript.exe
```

The q131 isolated Logseq replay against the saved graph EDN also completes with 0 mismatches.
