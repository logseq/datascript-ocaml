# DataScript OCaml

An OCaml 5 rewrite of DataScript, focused on matching the behavior of the ClojureScript implementation while providing a native OCaml API and js_of_ocaml build support.

This project is still under active development. Compatibility with upstream DataScript is verified through OCaml tests and cross-runtime parity checks against the ClojureScript implementation.

## Goals

- Preserve DataScript semantics for transactions, datoms, indexes, entities, pull, and query.
- Keep public APIs close enough to upstream behavior that Logseq can share the same data model assumptions.
- Support native OCaml and js_of_ocaml runtimes.
- Prefer clear OCaml implementations over shortcuts that change observable behavior.

## Development

Requirements:

- OCaml 5.2.1 or newer
- Dune 3.17 or newer
- Node.js for js_of_ocaml smoke tests and cross-runtime checks

Common commands:

```sh
dune build
dune runtest
```

Cross-runtime parity checks compare this implementation with the upstream ClojureScript DataScript behavior:

```sh
git clone https://github.com/logseq/datascript.git _deps/datascript
cd _deps/datascript && lein with-profile test cljsbuild once release && cd -
dune build test/cross_runtime_ocaml.exe
bash test/cross_runtime_parity_test.sh _build/default/test/cross_runtime_ocaml.exe script/cross_runtime_upstream.js
```

By default, the parity script expects an upstream DataScript checkout at
`_deps/datascript`. Set `UPSTREAM_DATASCRIPT_REPO` or `UPSTREAM_DATASCRIPT_JS`
to use a different checkout or compiled JS bundle.

## Repository Layout

- `type/`: shared public type definitions
- `impl/`: implementation modules
- `test/`: unit, integration, js_of_ocaml, and cross-runtime tests
- `examples/`: small executable examples
- `bench/`: benchmark entry points
- `script/`: parity and benchmark helper scripts

## Credits

This project is a port of DataScript's ideas and behavior to OCaml.

Primary credit goes to the upstream DataScript ClojureScript project:

- https://github.com/tonsky/datascript

The OCaml implementation is written independently, but upstream DataScript remains the semantic reference for compatibility work.
