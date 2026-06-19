# Rule Relation Lookup Key Normalization

## Case

q38 uses Logseq's `ref-property` rule:

```clojure
[:find ?p ?val
 :where
 (ref-property ?b ?p ?val)
 [?b :block/title "Page1"]
 :in $ %]
```

Upstream raises:

```text
Lookup ref should contain 2 elements: [:block/title ... :add-new-property]
```

OCaml previously returned `[]`.

## Root Cause

The rule expands to a nested `ref->val` pattern that uses a previously bound value as an entity:

```clojure
[?pv :block/title ?val]
```

Upstream DataScript marks entity-position variables as dynamic lookup candidates for relation joins. When a join key is a sequential value, upstream tries to resolve it as a lookup ref. Malformed keyword-headed vectors therefore raise the lookup-ref arity error while building the join key.

OCaml's relation hash join normalized only attribute keyword values. It did not normalize entity-position join keys through lookup-ref resolution, so malformed vectors were treated as ordinary non-matching values and silently filtered out.

## Fix

`impl/query_where.ml` now tracks relation variables that appear in entity, tx, or ref-value positions. When those variables are common join keys, the hash join normalizes list/vector keys through the existing query entity resolver. Malformed lookup-ref vectors therefore raise the same message as upstream.

The normalization is scoped to dynamic lookup-key positions. Plain scalar integers are not validated as entity ids in this path, matching upstream's relation-key behavior.

## Upstream Comparison

Upstream `datascript/query.cljc` uses `dynamic-lookup-attrs` around pattern lookup and `getter-fn` during relation joins. `getter-fn` resolves sequential dynamic lookup-key values with `db/entid`, which raises the malformed lookup-ref message for keyword-headed vectors with the wrong arity.

## Verification

Regression coverage was added for malformed lookup refs reached through relation-joined nested dynamic property rules. q38 now matches upstream, and the runnable batch `20 20` completes with 0 mismatches.
