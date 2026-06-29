# Logseq Runtime Rules Inputs

## Case

Logseq queries that call rules, for example `(has-property ?b ?p)` and `(ref-property ?b ?p "bar")`, were previously skipped because they need `%` runtime rule inputs.

## Root Cause

The comparator only treated queries with no runtime inputs, or only `$`, as runnable. Extracted Logseq rule definitions were not passed to either upstream DataScript or the OCaml runner, so rule queries could not be compared.

## Fix

`script/extract_logseq_runtime_inputs.clj` extracts Logseq rule forms into `test/logseq_runtime_inputs.edn`. `script/compare_logseq_queries.clj` now:

- detects queries that call extracted rules;
- adds `:in $ %` when the query omitted rule inputs;
- sends only the selected rule closure to upstream and OCaml;
- includes rules in the JSONL payload consumed by `examples/logseq_query_runner.ml`.

## Upstream Comparison

Upstream DataScript accepts rules through `%` in `:in`; `datascript/query.cljc` parses rule inputs with `parse-rules` and stores them in the query context. The OCaml comparator now mirrors that observable input path by passing `Arg_rules` to the runner instead of skipping rule queries.

## Verification

The first rule-enabled batch now runs with 196 runnable queries instead of 190, and batch `0 20` completes with 0 mismatches.
