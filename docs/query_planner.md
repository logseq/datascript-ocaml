# Query Planner and Evaluator Notes

This document describes how upstream DataScript plans and evaluates queries, and
how the OCaml implementation should move toward the same model while remaining
faster than upstream in native benchmarks and at least on par with upstream in
JavaScript benchmarks.

The compatibility target is not just equivalent query results. Query execution
should use the same upstream ideas: push known values into later clauses, resolve
patterns through the narrowest available index path, join relations incrementally,
and avoid constructing full binding maps when a compact relation is enough.

Performance is a hard requirement. Native OCaml must stay ahead of upstream
ClojureScript/JavaScript on the tracked benchmark cases, and `js_of_ocaml` must
stay at least on par with upstream. If an upstream-shaped rewrite loses those
properties, it is incomplete.

## Upstream Engines

The upstream checkout contains two relevant query engines:

- `src/datascript/query.cljc`
- `src/datascript/query_v3.cljc`

`query.cljc` is the public engine used by DataScript today. `query_v3.cljc` is a
newer no-doc engine that makes the intended planning model more explicit. The
OCaml implementation should use both as references: `query.cljc` for current
behavior and edge cases, `query_v3.cljc` for the cleaner planner/evaluator
shape.

## Upstream `query.cljc`

The public upstream evaluator uses a context that carries relations and known
sources. Clauses are processed left to right, but each clause is resolved against
the current context before it produces a relation.

Important pieces:

- `substitute-constants`
- `resolve-pattern-lookup-refs`
- `lookup-pattern-db`
- `lookup-pattern`
- `collapse-rels`
- `hash-join`
- `resolve-clause`
- `-q`

### Context

The context stores a list of relations. Each relation has:

- `:attrs`: a map from query variable to tuple index
- `:tuples`: tuples containing query values

The evaluator does not need every clause to immediately produce a full binding
map. It keeps compact relation tuples and only collects final rows at the end.

### Constant Substitution

Before looking up a DB pattern, upstream substitutes variables that are already
known to have exactly one value.

The logic is:

1. Find a relation in the context containing the variable.
2. If that relation has exactly one tuple, read the variable value from that
   tuple.
3. Replace the variable in the pattern with that value.

This makes a later clause like:

```clojure
[?e :age ?a]
```

become an indexed lookup when `?e` has already been reduced to one entity. It
also makes scalar input values behave like constants after binding.

### Pattern Resolution

For database sources, upstream builds a search pattern where constants remain
values and free variables become `nil`. It then calls `db/-search`.

For collection sources, upstream filters tuples by constant positions.

The important property is that the planner resolves the pattern after constant
substitution. Clause order still matters, but known values from previous clauses
are pushed into the next index lookup.

### Relation Collapse

After resolving a pattern to a relation, upstream does not simply append it to
the context. It calls `collapse-rels`:

1. Find existing relations with overlapping attributes.
2. Hash-join them with the new relation.
3. Keep unrelated relations separate.

This keeps the context compact and avoids carrying avoidable intermediate
bindings.

### Predicates and Functions

Predicates first check that their variable arguments are bound. Then they filter
the relation or product of relations that contains those variables.

This is why an input-bound predicate should be equivalent to a constant
predicate. Once the input is known, it should be inserted into the predicate
arguments instead of forcing a generic slow path.

### Rules, `or`, and `not`

Rules and branching clauses are still expressed in relation terms:

- rules solve branches into relations
- `or` sums compatible branch relations
- `not` subtracts relations
- `or-join` and `not-join` project the context to the listed vars before
  evaluating the branch

The same relation and hash-join machinery is used after expansion.

## Upstream `query_v3.cljc`

`query_v3.cljc` is a clearer model for an OCaml rewrite because it separates the
concepts more explicitly.

Important pieces:

- `resolve-ins`
- `resolve-in`
- `rel->consts`
- `substitute-constants`
- `resolve-pattern-db`
- `resolve-pattern`
- `hash-join-rel`
- `resolve-predicate`
- `resolve-clauses`
- `collect-to`

### Context Shape

The v3 context carries:

- `:rels`: non-scalar relations
- `:consts`: vars known to have a single value
- `:sources`: input DB or collection sources
- `:rules`: rule definitions
- `:default-source-symbol`

This distinction matters. A scalar input does not need to become a one-row
relation forever. It can be promoted to `:consts`, and later clauses can
substitute it directly.

### Input Resolution

`resolve-ins` binds `:in` forms before resolving `:where`.

If an input binding produces exactly one row, v3 stores those values in
`:consts`. If it produces multiple rows, it stores a relation in `:rels`.

This gives the planner a uniform answer to this question: is this variable known
as a single value right now?

### Pattern Resolution

`resolve-pattern` does three things:

1. `substitute-constants` walks the clause and replaces variables found in
   `:consts` with `Constant` nodes.
2. `resolve-pattern-db` calls `db/-search` using only constant positions.
3. `hash-join-rel` joins the produced relation with any existing related
   relations.

This is the generic version of the local benchmark fixes for q2, q3, q4, and
input-bound predicates.

### Hash Join

`hash-join-rel` extracts related relations from the context, products them when
needed, hashes the related side by common symbols, joins the new relation, and
then returns either:

- an empty context for empty results
- new constants for one-row results
- a relation for multi-row results

This feedback loop is important: a join that collapses to one row can turn more
variables into constants for the next clause.

### Predicate Resolution

`resolve-predicate` copies constants into an argument array, determines which
variable arguments remain, checks that they are bound, and filters only the
relation/product that contains those variables.

This avoids constructing full bindings for every row before testing a predicate.

## Current OCaml Shape

The OCaml implementation now routes `Query.q` through the relation-oriented
query evaluator:

- `impl/query_where.ml` implements relation-oriented clause evaluation.
- `impl/query_api.ml` projects relation rows into find results.
- `impl/datascript.ml` provides DB/query source operations but no longer has a
  `planned_simple_query` shape dispatcher.

The relation evaluator already has useful pieces:

- direct pattern relations
- row-based hash joins
- comparison filtering
- relation-row projection through `Query_api.q_sources_raw`, including `pull`
  find specs
- bound input handling for some cases
- single-row input constant substitution for relation rows
- bounded comparison candidates for indexed DB pattern values
- same-entity pattern chains for direct database attrs
- uniqueness metadata for relation rows that are provably distinct

There are no remaining `Query.q` shape guards. Former Logseq-specific
shortcuts have been moved into generic relation, projection, pull, and rule
evaluation behavior.

The literal and input-bound comparison cases are now handled by the generic
relation evaluator instead of a `Query.q`-only fast path: single-row `:in`
bindings are substituted into relation clauses, and adjacent indexed
pattern/comparison clauses can ask the DB source for bounded AVET candidates
before applying the same predicate filter. The relation evaluator also pushes
that predicate down to the datom stream before row materialization.

The q1/q2/q3/q4 cases now run through the generic relation evaluator rather
than `Query.q` shape guards. The evaluator builds constant entity sets,
consumes the value attr index as a sequence, uses direct row slots for direct
attrs, avoids ref coercion for non-ref attrs, and marks rows as unique only when
cardinality-one attrs and the current DB state prove that final projection can
skip distinct sorting safely.

Simple same-entity `not` clauses and source-qualified simple `not` clauses also
run inside the relation evaluator now. The evaluator preserves upstream's
clause-order binding check, builds excluded entity sets for same-entity `not`
patterns, and uses relation anti-join for source-qualified `not`. This removes
the older `Query.q` guards for constant/value patterns with a missing attribute
without keeping top-level query shape guards.

Ref child-by-parent joins now also rely on the relation evaluator instead of a
`Query.q` shape guard. Source-qualified and default-source forms use the same
pattern relation plus hash-join path.

Namespace/value joins no longer have a `Query.q` shape guard either. The
generic evaluator handles the source pattern, `namespace` function, equality
predicate, and value pattern sequence through the normal query pipeline.

Incoming-ref plus missing-attr filters now also use the relation evaluator.
The source-qualified and default-source forms share the same pattern relation,
hash-join, and relation anti-join path.

Tagged page-ref pair queries with journal-tag exclusion now also use the same
generic relation path. The default-source and source-qualified forms both
resolve block/page, page tag presence, journal tag exclusion, and block refs
through pattern relations, hash joins, and relation anti-join.

Pull-heavy simple patterns, joined refs, and missing-attr page pulls no longer
have `Query.q` shape guards. The relation evaluator produces rows, and
`Query_api.q_sources_raw` collects `pull` find specs from those rows without
re-running clause evaluation. Wildcard pull now uses forward-only entity
materialization unless a reverse selector is explicitly requested, so generic
pull projection does not force a full EAVT scan.

The Logseq `has-property` and `property` empty-result shortcuts have also moved
out of top-level query dispatch. Nonrecursive rule invocation now checks bound
rule-body DB patterns with the same bounded pattern lookup used by the relation
evaluator. If a bound attr/value slice is empty, the rule branch is empty
before unrelated rule-body prefixes are evaluated. This matches upstream's
rule-context empty-branch outcome without preserving a Logseq-specific query
shape.

The `task` plus `page-ref` literal-string empty case also runs through the same
rule precheck. Literal values on ref attrs are tested against the bounded attr
slice without lookup-ref coercion, so a raw string that cannot match
`:block/refs` makes the rule branch empty before noisy earlier rules run.

The `ref-property` malformed lookup-ref error-ordering case also no longer has
a top-level query guard. The generic rule evaluator reaches the malformed value
through the dynamic property rule path and raises the same lookup-ref arity
error before the later missing title relation can turn the query empty.

Top-level `or` and source-qualified `or` now reuse the same relation-context
batching that active rule expansion uses. A relation prefix before `or` is
evaluated once into relation bindings, and branch patterns with many bound
outer rows use bounded relation-prefix evaluation instead of running branch
work one binding at a time. Top-level `or-join` and source-qualified `or-join`
also batch many outer bindings by projecting the listed join vars once,
evaluating each branch against that projected relation, and merging branch rows
back into the outer bindings by the same join vars. Single-binding `or-join`
still keeps the older path where it preserves upstream constant-substitution
semantics. Required-vars `or-join` forms use the same batching, but project the
union of required vars and listed vars. Required vars participate in matching
branch rows back to the correct outer binding, while the listed vars remain the
only branch vars that can add new values to the outer binding.

Top-level `not-join` and source-qualified `not-join` now also stay in the
relation evaluator for supported clause shapes. The evaluator projects the
current relation to the listed join vars, evaluates the nested clauses from
that projected relation, projects the nested result back to the join vars, and
anti-joins it against the outer relation. This matches upstream's context
projection model without materializing one binding map per outer row before the
nested query.

Nonrecursive rule calls and source-qualified nonrecursive rule calls now batch
many outer bindings through the same grouped rule-pair model. The evaluator
builds initial rule bindings for the outer rows, evaluates the rule body from a
relation initial context, and then propagates matching rule results back to the
outer bindings. Source-qualified rule bodies use the invocation source as their
default source while preserving an explicit `$` source for the original root
DB, matching other source-qualified composite clauses. Recursive rules still
use the existing guarded path. Batched nonrecursive calls do not run the
single-binding rule-body no-match probe for every outer row; upstream's context
model evaluates the body once from the relation of initial rule bindings.

Relation source patterns also participate in relation evaluation now. A clause
such as `[$labels ?value ?label]` is converted to a relation once and hash-joined
with the current relation, instead of scanning the relation source separately
for each outer binding. The conversion still uses the normal relation-source row
matching semantics, so constants, wildcards, repeated vars, and arity errors
stay aligned with the old evaluator and upstream behavior.

Source-qualified wrapper clauses also run inside the relation evaluator for
supported wrapped clauses. A wrapper such as a source-qualified predicate first
keeps the current relation rows intact, switches the default source for the
wrapped clause, evaluates that wrapped clause against the whole relation, and
then continues with the outer source context. This avoids materializing one
binding map per row just to enter the wrapper.

## Why No-Planner Is Slow

A no-planner experiment, run before the current relation-evaluator migration,
disabled:

- the old `Query.planned_simple_query` dispatcher
- relation-row projection in `Query_api.q_sources_raw`

With transaction, DB, and persistent sorted set fixes still enabled, the JS
benchmark showed that non-query paths stayed fast while multi-clause queries
regressed:

| Case | no planner OCaml JS | upstream CLJS JS |
| --- | ---: | ---: |
| `datoms-name` | 0.107 ms | 0.110 ms |
| `add-all` | 238 ms | 464 ms |
| `q2` | 3.13 ms | 1.02 ms |
| `q3` | 5.42 ms | 0.72 ms |
| `q4` | 9.02 ms | 1.30 ms |
| `qpred2` | 5.41 ms | 3.41 ms |

This isolates the remaining gap to query planning/evaluation. It is not caused
by EAVT materialization, persistent sorted set traversal, or transaction
application.

## Alignment Targets

The OCaml planner/evaluator should move toward the v3 model:

1. Represent evaluation context with separate constants and relations.
2. Bind `:in` values into constants when the input has exactly one row.
3. Substitute constants into every pattern and predicate before resolution.
4. Resolve DB patterns by calling bounded index access with constant positions.
5. Hash-join new relations with existing related relations immediately.
6. Convert one-row relation results back into constants when safe.
7. Filter predicates against the smallest relation/product containing the
   predicate variables.
8. Delay final row/binding materialization until projection/aggregation.
9. Keep existing rule, `or`, `not`, source, pull, and dynamic callable semantics.

## OCaml Data Structures

The upstream ClojureScript implementation uses mutable/transient arrays and
maps internally. A direct list-heavy OCaml port is likely to lose on
`js_of_ocaml` even when the algorithm is equivalent.

The OCaml implementation should use data structures that compile well to both
native and JavaScript:

- arrays for relation rows
- integer indexes for symbols inside a relation
- hash tables or maps only at join/build boundaries
- compact row projection for `:find`
- lists only at API boundaries or for small fixed arities

The current `query_result list list` and `(string * query_result) list` binding
forms are useful compatibility types, but the hot planner should not use them as
its primary internal row representation.

## Performance Gates

Every planner/evaluator alignment step must pass semantic tests and performance
checks.

Required semantic checks:

```sh
opam exec -- dune runtest
```

Required benchmark checks for query work:

```sh
opam exec -- dune build bench/bench_ocaml.exe bench/bench_ocaml.bc.js
_build/default/bench/bench_ocaml.exe --size 5000 --warmup-ms 200 --sample-ms 400 --samples 5
node _build/default/bench/bench_ocaml.bc.js --size 5000 --warmup-ms 200 --sample-ms 400 --samples 5
UPSTREAM_DATASCRIPT_JS=_deps/datascript/release-js/datascript.js \
  node bench/bench_upstream.js --size 5000 --warmup-ms 200 --sample-ms 400 --samples 5
```

Acceptance criteria:

- native OCaml must beat upstream CLJS/JS on every tracked benchmark case
- `js_of_ocaml` must be at least on par with upstream CLJS/JS on every tracked
  benchmark case
- no planner rewrite may regress current wins without replacing them with a
  more general upstream-shaped path that preserves the native lead and keeps
  `js_of_ocaml` at least on par with upstream

Current standard benchmark evidence with `size=1000`, `warmup=500ms`,
`sample=1000ms`, and `samples=7`, using upstream DataScript
`3f141af97b70e1f14c65eaa119acd822ebece37e`:

| Case | native OCaml | js_of_ocaml | upstream CLJS/JS |
| --- | ---: | ---: | ---: |
| `add-1` | 12.09 ms | 50.90 ms | 66.55 ms |
| `add-5` | 26.39 ms | 83.92 ms | 112.22 ms |
| `add-all` | 25.37 ms | 63.75 ms | 125.05 ms |
| `storage-roundtrip` | 32.27 ms | 108.70 ms | n/a |
| `datoms-name` | 0.01095 ms | 0.02753 ms | 0.03966 ms |
| `q1` | 0.01726 ms | 0.07218 ms | 0.17383 ms |
| `q2` | 0.03247 ms | 0.09787 ms | 0.40041 ms |
| `q3` | 0.08150 ms | 0.19550 ms | 0.25324 ms |
| `q4` | 0.17874 ms | 0.29472 ms | 0.47448 ms |
| `q5-shortcircuit` | 0.01309 ms | 0.02944 ms | 0.07878 ms |
| `qpred1` | 0.09715 ms | 0.47755 ms | 1.28 ms |
| `qpred2` | 0.10778 ms | 0.44307 ms | 1.43 ms |
| `q2pred` | 0.08380 ms | 0.20713 ms | 0.37083 ms |
| `pull-one` | 0.00245 ms | 0.00691 ms | 0.01260 ms |

Native OCaml is ahead on every tracked standard benchmark case in this run.
`js_of_ocaml` is also ahead of upstream CLJS/JS on every tracked case in this
run, including q3.

The `datoms-name` case uses `fold_datoms`, which preserves the lazy `datoms`
API while giving callers an upstream-shaped reducible path like upstream BTSet
`Iter`'s `IReduce` implementation. Same-entity pattern evaluation also uses
`fold_pattern_datoms` when it builds constant entity bitsets for value-variable
joins. That keeps the public `datoms` API lazy and avoids materializing
intermediate attr/value datom lists in the q3-style hot path.

## Migration Plan

1. Introduce a planner context with constants, relations, sources, and rules.
2. Route simple pattern and predicate clauses through the new context.
3. Keep q2/q3/q4, qpred2, same-entity `not`, source-qualified simple `not`,
   ref child-by-parent joins, namespace/value joins, incoming-ref missing-attr
   filters, tagged page-ref pair joins, pull find projection, and
   `has-property`/`property` empty rule branches, top-level `or`, relation
   source joins, and supported `not-join` forms on generic relation behavior
   and broaden that same machinery to more clause combinations.
4. Replace list bindings in hot relation joins with array rows.
5. Continue moving `Query_api.q_sources_raw` projection onto relation rows
   without materializing bindings first.
6. Port remaining rule, `or-join`, and source behavior onto the same context
   model.
7. Keep deleting compatibility-only evaluator paths only after benchmarks prove
   the generic path preserves the native lead, keeps `js_of_ocaml` at least on
   par with upstream, and tests prove semantics still match.

The end state should be smaller and more upstream-shaped than the current mix of
generic relation evaluation plus specialized query cases, while keeping the
absolute performance requirement.
