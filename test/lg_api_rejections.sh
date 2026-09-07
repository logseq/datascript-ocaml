#!/usr/bin/env bash
set -euo pipefail
compiler=$1
literal=$2
api=$3
native_state=$4
melange_state=$5
test_dir=$(mktemp -d)
trap 'rm -rf "$test_dir"' EXIT

expect_rejected() {
  local name=$1 expected=$2 source=$3
  printf '%s\n' "$source" > "$test_dir/$name.cljc"
  if "$compiler" --target "$target" --compile-files-from "$state" \
      "$literal" "$api" "$test_dir/$name.cljc" -o "$test_dir/$name.ml" \
      > "$test_dir/$name.log" 2>&1; then
    echo "expected rejection: $target/$name" >&2
    exit 1
  fi
  if ! grep -Fq "$expected" "$test_dir/$name.log"; then
    cat "$test_dir/$name.log" >&2
    exit 1
  fi
}

for target in native melange; do
  if [[ "$target" == native ]]; then state=$native_state; else state=$melange_state; fi
  expect_rejected wrong_attribute_value string '
(ns bad (:require [datascript.api :as d]))
(d/defattr id :shape/id :string {})
(def wrong (Datascript_lg.Attribute.entry id 42))'
  expect_rejected wrong_entity_value string '
(ns bad (:require [datascript.api :as d]))
(d/defattr id :shape/id :string {})
(def wrong (d/entity nil {id 42}))'
  expect_rejected wrong_literal_value string '
(ns bad (:require [datascript.api :as d]))
(def wrong (d/form [(unquote :string 42)]))'
  expect_rejected unsupported_attribute "unsupported attribute codec" '
(ns bad (:require [datascript.api :as d]))
(d/defattr id :shape/id :anything {})'
  expect_rejected unsupported_literal "unsupported literal form" '
(ns bad (:require [datascript.api :as d]))
(def wrong (d/form #"regex"))'
  expect_rejected missing_query_type "cannot infer query variable ?x" '
(ns bad (:require [datascript.api :as d]))
(d/defquery ids [:find ?x :where [?e :item/id ?x]])'
  expect_rejected conflicting_query_type "string" '
(ns bad (:require [datascript.api :as d]))
(d/defattr id :item/id :string {})
(d/defattr count :item/count :int {})
(d/defquery ids [:find ?x :where [?e id ?x] [?e count ?x]])'
  expect_rejected wrong_query_hint "string" '
(ns bad (:require [datascript.api :as d]))
(d/defattr id :item/id :string {})
(d/defquery ids {:types {?x d/int-codec}} [:find ?x :where [?e id ?x]])'
  expect_rejected unsupported_find "defquery expects relation :find variables" '
(ns bad (:require [datascript.api :as d]))
(d/defquery ids [:find (count ?e) :where [?e :item/id ?x]])'
  expect_rejected missing_where "defquery requires :find and :where" '
(ns bad (:require [datascript.api :as d]))
(d/defquery ids [:find ?x])'
  expect_rejected unsupported_query_clause "unsupported defquery clause" '
(ns bad (:require [datascript.api :as d]))
(d/defquery ids [:find ?e :where (or [?e :x 1] [?e :x 2])])'

  expect_rejected function_result_needs_hint "cannot infer query variable ?y" '
(ns bad (:require [datascript.api :as d]))
(d/defattr id :item/id :int {})
(d/defquery ids [:find ?y :where [?e id ?x] [(identity ?x) ?y]])'
  expect_rejected invalid_attribute_mapping "defquery :attributes must map keywords to attribute symbols" '
(ns bad (:require [datascript.api :as d]))
(d/q [:find ?e :where [?e :item/id ?x]] db {:attributes []})'

done
