#!/usr/bin/env sh
set -eu

tmp="${TMPDIR:-/tmp}/datascript_ocaml_test"
mkdir -p "$tmp"
rm -f "$tmp"/*

ocamlc -I type -c type/datascript_types.mli -o "$tmp/datascript_types.cmi"
ocamlc -I "$tmp" -I type -c type/datascript_types.ml -o "$tmp/datascript_types.cmo"
ocamlc -I "$tmp" -I type -I impl -c impl/datascript.mli -o "$tmp/datascript.cmi"
ocamlc -I +unix -I +str -I "$tmp" -I type -I impl -c impl/datascript.ml -o "$tmp/datascript.cmo"
ocamlc -I +unix -I +str -I "$tmp" -I type -I impl -c test/test_datascript.ml -o "$tmp/test_datascript.cmo"
ocamlc \
  unix.cma \
  str.cma \
  -I +unix \
  -I +str \
  -I "$tmp" \
  "$tmp/datascript_types.cmo" \
  "$tmp/datascript.cmo" \
  "$tmp/test_datascript.cmo" \
  -o "$tmp/test_datascript"

"$tmp/test_datascript"
