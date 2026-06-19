# Logseq Has Property Empty Rule Shortcuts

## Case

Logseq queries `q26`, `q27`, and `q28` use `has-property` rules in cases that are deterministically empty:

```clojure
[:find
 (pull ?b [:block/title])
 :where
 (has-property ?b :user.property/foo)
 :in
 $
 %]
```

```clojure
[:find
 [?p ...]
 :where
 (has-property ?b ?p)
 [?b :block/title "Page1"]
 :in
 $
 %]
```

OCaml timed out under the upstream elapsed time plus one second budget.

## Root Cause

For q26 and q27, the constant property idents do not exist in the graph. The `has-property` rule therefore cannot match, but OCaml expanded and evaluated the full runtime rule body anyway.

For q28, the fixed `:block/title` value `"Page1"` does not exist in the graph. The query is therefore empty, but the generic rule evaluator still did expensive rule work before discovering that.

## Fix

`planned_simple_query` now short-circuits two `has-property` cases:

- constant property ident is missing;
- `has-property` is constrained by a fixed attr value that is missing.

The fixed attr-value shortcut is scoped to `has-property`. It is not applied to `ref-property`, because q31 and q34 show that upstream reports malformed lookup-ref errors before the missing-title filter can turn the query into an empty result.

## Upstream Comparison

The shortcuts preserve upstream observable behavior for these cases: the rules are pure relation predicates, and a missing required property ident or missing fixed title relation makes the result empty.

## Verification

Added focused coverage in `test/test_logseq_query_planners.ml`.

Isolated Logseq replays against the saved graph EDN now complete with 0 mismatches:

```text
ocaml q26 elapsed-ms 265
ocaml q27 elapsed-ms 257
ocaml q28 elapsed-ms 17
```
