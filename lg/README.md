# Typed LG application API

`datascript-ocaml-lg` is an optional package. It depends on the shared DataScript
interface and LG's closed EDN backend; applications select the Native or Melange
DataScript implementation. The query and transaction engines remain unchanged.

Compile `stdlib/lg/literal.cljc` from the installed `lg` package, followed by
`sources/datascript/api.cljc` from `datascript-ocaml-lg`, before application
sources. These are ordinary reusable LG macros and typed values. Dune consumers
can name the files as `%{lib:lg:stdlib/lg/literal.cljc}` and
`%{lib:datascript-ocaml-lg:sources/datascript/api.cljc}`. A standard LG compiled
state supplies the core macro environment.

For Native linking with `ocamlfind`, list the selected implementation before
this facade, for example `-package datascript-ocaml-native,datascript-ocaml-lg`.
Dune resolves the virtual implementation through the application libraries.

## Structured literals and reusable queries

```clojure
(ns drawing.db
  (:require [datascript.api :as d]
            [ocaml.Datascript :as ds]
            [ocaml.Datascript_lg :as api]))

(def drawing-schema
  (d/schema {:shape/id {:db/unique :db.unique/identity}
             :shape/doc {:db/index true}}))

(d/defattr shape-id :shape/id :string {})

(defn document-shape-ids [db doc-id]
  (vec (d/q [:find ?id :in $ ?doc
             :where [?e :shape/doc ?doc] [?e shape-id ?id]]
         db (api/input d/string-codec doc-id))))

(defn add-shape! [conn id doc-id x]
  (d/transact! conn
    [{:shape/id (unquote :string id)
      :shape/doc (unquote :string doc-id)
      :shape/x (unquote :float x)}]))
```

`d/form` constructs the closed `Datascript.query_form` directly; it does not
build an LG EDN tree or parse a string. It supports nil, booleans, integers,
floats, strings, keywords, symbols, lists, vectors, sets, and maps. Symbols
inside the literal are data. `(unquote expression)` embeds an existing form;
`(unquote :kind expression)` calls that scalar constructor with a statically
checked payload. Expressions are evaluated once in source order.

`d/schema`, `d/tx`, and `d/transact!` reuse the upstream readers and validation.

Queries do not require a declaration. Pass a literal directly to `d/q`:

```clojure
(d/q [:find ?id :where [?e shape-id ?id]] db)

;; Keep keyword EDN by associating keywords with typed attributes.
(d/q '[:find ?id :where [?e :shape/id ?id]] db
  {:attributes {:shape/id shape-id}})

;; A codec can supply evidence when there is no typed attribute declaration.
(d/q [:find ?id :where [?e :shape/id ?id]] db
  {:types {?id d/string-codec}})
```

`db` is always passed to `d/q`, which executes the query. The optional map after
`db` is available for literal queries; subsequent arguments are upstream query
inputs, as in `(d/q literal db options (api/input d/string-codec doc-id))`.
Quoted and unquoted literal vectors are supported. Opaque runtime query data
requires the explicit `d/query` projection API below.

`d/defquery` is optional: it parses a reusable query at definition time and
infers the same projections. It accepts the options map before its literal:
`(d/defquery ids {:attributes {:shape/id shape-id}} [:find ?id :where ...])`.
Direct literal queries prepare on each evaluation; no hidden global cache or
attribute registry is involved. Bind reusable queries outside hot loops:

```clojure
(d/defquery ids-by-document
  [:find ?id :in $ ?doc
   :where [?e :shape/doc ?doc] [?e shape-id ?id]])

(d/q ids-by-document db (api/input d/string-codec doc-id))
```

A value variable inherits the codec of its typed attribute. Entity positions
infer entity IDs, attribute positions infer keywords, and transaction positions
infer integers. Repeated variables and explicit hints must agree statically.
Attribute bindings can come from another namespace. Keyword mappings validate
that the declared attribute actually has the given keyword name. Inferred types
still decode and validate actual DB results: malformed external data is an
error, not an unchecked cast.

One `:find` variable yields an OCaml list of values; multiple variables yield a
list of flat tuples, in `:find` order. Wrap the result in `vec` for an LG vector.
This inference supports relation queries with datom patterns, optional source
prefixes, `:in`, and `:with`. Predicate/function clauses are preserved; function
outputs require explicit `:types` evidence. Aggregates, pull expressions,
scalar/collection find syntax, rules, and logical clauses require the existing
explicit projection API. Unknown types produce a compile-time error instead
of guessing from a keyword name. Codec expressions are evaluated once when a
query is prepared.

`d/query` remains the low-level preparation API for existing callers:
`(d/query (Datascript_lg.Projection.column 0 d/string-codec) query-literal)`.
It does not execute a query or take a DB. `d/q prepared-query db inputs...`
executes it. Projection mismatches raise `Datascript_lg.Invalid_data`;
`run_query` is the result-returning API.

## Typed attributes and direct transactions

```clojure
(d/defattr shape-id :shape/id :string {:db/unique :db.unique/identity})
(d/defattr shape-x :shape/x :float {})

(d/transact-ops! conn
  (d/entity nil {shape-id "first" shape-x 12.5}))

(d/pull db [:shape/x]
  (ds/Lookup_ref "shape/id" (ds/String "first"))
  (Datascript_lg.Pull.field shape-x))
```

The typed entity path emits `tx_op` values directly, with no query-form/EDN
intermediate. A field value must match its attribute codec. `d/entity` receives
an optional entity reference (`nil` for an anonymous entity, or `Some` of a
DataScript entity reference). It handles cardinality-one attributes; use
`Attribute.entries` with an explicit typed entity for cardinality-many data.
`d/transact-ops!` evaluates its connection and operations once in order.

`Attribute.schema` returns the declaration's schema entry when building a
schema from typed attributes. Declaring a codec does not mutate an existing DB
or silently install additional `valueType` restrictions. Configure the actual
schema explicitly. `Attribute.read_one` and `read_many` validate the returned
values. `read_one codec name entity` is convenient when the attribute name is
selected at runtime and avoids allocating a temporary attribute declaration.

Absence and malformed values are distinct. `read_one` returns `Ok None` for a
missing attribute and `Error` for a wrong value/cardinality. `Pull.field` returns
an optional field, while `pull`/`d/pull` additionally preserves the upstream
optional entity result. Defaults belong at application call sites. The float
codec accepts integer coordinates; the int codec never truncates floats.

`Projection.pair`/`map` and `Pull.pair`/`map` compose explicit typed results.
The literal query API generates these projections from static attribute/codec
evidence. Raw upstream APIs remain available for queries outside that subset.

## External EDN

`of_edn` returns a checked conversion from `Lg_edn_backend.t`; `of_edn_exn` is
the explicitly raising variant. Nested paths appear in conversion errors.
Integers outside the target's DataScript `int` range and inconsistent packed
arrays are rejected. Tags and numeric tagged forms retain their representation
for upstream validation. `read` directly uses the DataScript reader for input
strings, avoiding a redundant LG EDN tree. No unsafe cast or universal dynamic
value is used.

## Verification

- `dune exec test/test_lg_api.exe` checks the OCaml boundary against real DBs.
- `dune exec test/lg_api_body.exe` compiles and executes the LG API.
- `dune build @lg-api-melange` builds the same source for JavaScript.
- `dune runtest` runs the above runtime tests, both-target static rejections,
  and the existing DataScript suite.
