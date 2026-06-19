# Logseq Input Arity Error Message

## Case

Logseq query `q178`:

```clojure
[:find
 (pull ?b [*])
 :in
 $
 %
 :where
 [?b :block/title "block with empty rules input"]]
```

was expected to fail because no rules input was supplied. Upstream and OCaml both returned errors, but the messages differed.

## Root Cause

OCaml reported input arity errors with an older v3-style message:

```text
Wrong number of arguments for bindings [$ %], 2 required, 1 provided
```

The upstream implementation used by Logseq raises current query input errors from `datascript.query/resolve-ins`, which distinguishes too few from extra inputs:

```text
Too few inputs passed, expected: [$ %], got: 1
```

The behavior was semantically correct, but the observable error shape/message did not match upstream.

## Fix

`Query.query_input_arity_error` now emits upstream-shaped messages:

- `Too few inputs passed, expected: [...], got: n`
- `Extra inputs passed, expected: [...], got: n`

The existing input arity tests were updated to the upstream message format.

## Upstream Comparison

Upstream `datascript.query/resolve-ins` compares the number of declared input bindings with supplied values and raises either `Too few inputs passed` or `Extra inputs passed`, including the expected binding labels and actual count.

## Verification

The q178 batch replay now matches upstream:

```text
ocaml q178 elapsed-ms 1
runnable: 115 skipped: 87 mismatches: 0
```
