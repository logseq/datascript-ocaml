(ns test-logseq-query-filters
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [clojure.test :refer [deftest is run-tests testing]]))

(def test-root "tmp/logseq-query-filter-test")

(defn recreate-dir [path]
  (let [file (io/file path)]
    (when (.exists file)
      (doseq [child (reverse (file-seq file))]
        (.delete child)))
    (.mkdirs file)))

(def extract-deps
  "{:deps {org.clojure/tools.reader {:mvn/version \"1.5.2\"}}}")

(def compare-deps
  "{:deps {cheshire/cheshire {:mvn/version \"6.0.0\"} datascript/datascript {:git/url \"https://github.com/logseq/datascript\" :git/sha \"3f141af97b70e1f14c65eaa119acd822ebece37e\"}}}")

(declare fake-synth-input-runner-script)

(defn write-file [path content]
  (.mkdirs (.getParentFile (io/file path)))
  (spit path content))

(deftest extractor-ignores-nbb-cache-queries-and-dedupes-query-forms
  (let [root (str test-root "/extract-src")
        out (str test-root "/queries.edn")]
    (recreate-dir test-root)
    (write-file
      (str root "/src/a-query.cljs")
      "(def q '[:find ?e :where [?e :name]])")
    (write-file
      (str root "/src/z-duplicate.cljs")
      "(def q '[:find ?e :where [?e :name]])")
    (write-file
      (str root "/.nbb/.cache/cache-key/query.cljs")
      "(def q '[:find ?e :where [?e :cached]])")
    (let [{:keys [exit err]} (shell/sh "clojure" "-Sdeps" extract-deps "-M" "script/extract_logseq_queries.clj" root out)
          entries (edn/read-string (slurp out))]
      (is (zero? exit) err)
      (is (= ["src/a-query.cljs"] (mapv :file entries)))
      (is (= ["q1"] (mapv :id entries))))))

(deftest extractor-normalizes-quoted-map-query-sections
  (let [root (str test-root "/extract-quoted-map")
        out (str test-root "/quoted-map-queries.edn")]
    (recreate-dir test-root)
    (write-file
      (str root "/src/query.cljs")
      "(defn find-page [repo page-name selector]\n  (transport/invoke nil :thread-api/q\n    [repo\n     [{:find [[(list 'pull '?e selector) '...]]\n       :in '[$ ?name]\n       :where '[[?e :block/name ?name]]}\n      page-name]]))")
    (let [{:keys [exit err]} (shell/sh "clojure" "-Sdeps" extract-deps "-M" "script/extract_logseq_queries.clj" root out)
          [entry] (edn/read-string (slurp out))
          query (:query entry)]
      (is (zero? exit) err)
      (is (= '[$ ?name ?selector] (:in query)))
      (is (= '[[(pull ?e ?selector) ...]] (:find query)))
      (is (= '[[?e :block/name ?name]] (:where query)))
      (is (not (contains? query '[$ ?name]))))))

(deftest comparator-synthesizes-missing-runtime-inputs
  (let [queries-path (str test-root "/synth-input-queries.edn")
        runner-path (str test-root "/fake-synth-input-runner.sh")
        report-path (str test-root "/synth-input-report.md")]
    (recreate-dir test-root)
    (spit
      queries-path
      (pr-str
        [{:id "synth-input-query"
          :file "src/query.cljs"
          :line 1
          :query '[:find ?e :in $ ?name [?tag-ident ...] :where [?e :block/name ?name] [?tag :db/ident ?tag-ident]]}]))
    (write-file runner-path (fake-synth-input-runner-script))
    (.setExecutable (io/file runner-path) true)
    (let [{:keys [exit err]} (shell/sh
                               "clojure"
                               "-Sdeps"
                               compare-deps
                               "-M"
                               "script/compare_logseq_queries.clj"
                               queries-path
                               "unused.sqlite"
                               runner-path
                               report-path
                               "0"
                               "1"
                               "missing-runtime-inputs.edn")
          report (if (.exists (io/file report-path)) (slurp report-path) "")]
      (is (zero? exit) err)
      (is (str/includes? report "- Runnable queries: 1"))
      (is (str/includes? report "- Batch query ids: synth-input-query"))
      (is (str/includes? report "- Mismatches: 0")))))

(defn fake-runner-script []
  (str "#!/bin/sh\n"
       "set -eu\n"
       "if [ \"$1\" = \"dump-graph\" ]; then\n"
       "  printf '{:schema {}, :datoms []}' > \"$3\"\n"
       "  exit 0\n"
       "fi\n"
       "if [ \"$1\" = \"run\" ]; then\n"
       "  while IFS= read -r line; do\n"
       "    id=$(printf '%s' \"$line\" | sed 's/.*\"id\":\"\\([^\"]*\\)\".*/\\1/')\n"
       "    printf '{\"id\":\"%s\",\"status\":\"ok\",\"value\":\"[]\"}\\n' \"$id\"\n"
       "  done < \"$3\"\n"
       "  exit 0\n"
       "fi\n"
       "exit 1\n"))

(defn fake-ready-runner-script []
  (str "#!/bin/sh\n"
       "set -eu\n"
       "if [ \"$1\" = \"dump-graph\" ]; then\n"
       "  printf '{:schema {}, :datoms []}' > \"$3\"\n"
       "  exit 0\n"
       "fi\n"
       "if [ \"$1\" = \"run\" ]; then\n"
       "  sleep 2\n"
       "  printf '{\"status\":\"ready\"}\\n'\n"
       "  while IFS= read -r line; do\n"
       "    id=$(printf '%s' \"$line\" | sed 's/.*\"id\":\"\\([^\"]*\\)\".*/\\1/')\n"
       "    printf '{\"id\":\"%s\",\"status\":\"ok\",\"value\":\"[[42]]\"}\\n' \"$id\"\n"
       "  done < \"$3\"\n"
       "  exit 0\n"
       "fi\n"
       "exit 1\n"))

(defn fake-input-runner-script []
  (str "#!/bin/sh\n"
       "set -eu\n"
       "if [ \"$1\" = \"dump-graph\" ]; then\n"
       "  printf '{:schema [[:name {}]], :datoms [[1 :name \"Alice\" 536870913 true]]}' > \"$3\"\n"
       "  exit 0\n"
       "fi\n"
       "if [ \"$1\" = \"run\" ]; then\n"
       "  printf '{\"status\":\"ready\"}\\n'\n"
       "  while IFS= read -r line; do\n"
       "    id=$(printf '%s' \"$line\" | sed 's/.*\"id\":\"\\([^\"]*\\)\".*/\\1/')\n"
       "    case \"$line\" in\n"
       "      *'\"inputs\":[\"\\\"Alice\\\"\"]'*) printf '{\"id\":\"%s\",\"status\":\"ok\",\"value\":\"[[1]]\"}\\n' \"$id\" ;;\n"
       "      *) printf '{\"id\":\"%s\",\"status\":\"error\",\"message\":\"missing inputs\"}\\n' \"$id\" ;;\n"
       "    esac\n"
       "  done < \"$3\"\n"
       "  exit 0\n"
       "fi\n"
       "exit 1\n"))

(defn fake-synth-input-runner-script []
  (str "#!/bin/sh\n"
       "set -eu\n"
       "if [ \"$1\" = \"dump-graph\" ]; then\n"
       "  printf '{:schema [], :datoms []}' > \"$3\"\n"
       "  exit 0\n"
       "fi\n"
       "if [ \"$1\" = \"run\" ]; then\n"
       "  printf '{\"status\":\"ready\"}\\n'\n"
       "  while IFS= read -r line; do\n"
       "    id=$(printf '%s' \"$line\" | sed 's/.*\"id\":\"\\([^\"]*\\)\".*/\\1/')\n"
       "    case \"$line\" in\n"
       "      *'\"inputs\":['*) printf '{\"id\":\"%s\",\"status\":\"ok\",\"value\":\"[]\"}\\n' \"$id\" ;;\n"
       "      *) printf '{\"id\":\"%s\",\"status\":\"error\",\"message\":\"missing synthesized inputs\"}\\n' \"$id\" ;;\n"
       "    esac\n"
       "  done < \"$3\"\n"
       "  exit 0\n"
       "fi\n"
       "exit 1\n"))

(defn fake-collection-input-runner-script []
  (str "#!/bin/sh\n"
       "set -eu\n"
       "if [ \"$1\" = \"dump-graph\" ]; then\n"
       "  printf '{:schema [[:name {}]], :datoms [[1 :name \"Alice\" 536870913 true]]}' > \"$3\"\n"
       "  exit 0\n"
       "fi\n"
       "if [ \"$1\" = \"run\" ]; then\n"
       "  printf '{\"status\":\"ready\"}\\n'\n"
       "  while IFS= read -r line; do\n"
       "    id=$(printf '%s' \"$line\" | sed 's/.*\"id\":\"\\([^\"]*\\)\".*/\\1/')\n"
       "    case \"$line\" in\n"
       "      *'\"inputs\":[\"\\\"Alice\\\"\"]'*) printf '{\"id\":\"%s\",\"status\":\"ok\",\"value\":\"[1]\"}\\n' \"$id\" ;;\n"
       "      *) printf '{\"id\":\"%s\",\"status\":\"error\",\"message\":\"missing inputs\"}\\n' \"$id\" ;;\n"
       "    esac\n"
       "  done < \"$3\"\n"
       "  exit 0\n"
       "fi\n"
       "exit 1\n"))

(defn report-ocaml-elapsed-ms [report]
  (some->> (re-find #"- OCaml elapsed ms: ([0-9]+)" report)
           second
           Long/parseLong))

(deftest comparator-ignores-existing-nbb-cache-entries-and-dedupes-query-forms
  (let [queries-path (str test-root "/compare-queries.edn")
        runner-path (str test-root "/fake-runner.sh")
        report-path (str test-root "/report.md")]
    (recreate-dir test-root)
    (spit
      queries-path
      (pr-str
        [{:id "cache"
          :file ".nbb/.cache/cache-key/query.cljs"
          :line 1
          :query '[:find ?e :where [?e]]}
         {:id "normal"
          :file "src/query.cljs"
          :line 1
          :query '[:find ?e :where [?e]]}
         {:id "duplicate"
          :file "src/duplicate.cljs"
          :line 2
          :query '[:find ?e :where [?e]]}]))
    (write-file runner-path (fake-runner-script))
    (.setExecutable (io/file runner-path) true)
    (let [{:keys [exit err]} (shell/sh
                               "clojure"
                               "-Sdeps"
                               compare-deps
                               "-M"
                               "script/compare_logseq_queries.clj"
                               queries-path
                               "unused.sqlite"
                               runner-path
                               report-path
                               "0"
                               "10"
                               "missing-runtime-inputs.edn")
          report (slurp report-path)]
      (is (zero? exit) err)
      (is (str/includes? report "- Extracted queries: 1"))
      (is (str/includes? report "- Batch query ids: normal"))
      (is (not (str/includes? report "- Batch query ids: cache")))
      (is (not (str/includes? report "- Batch query ids: duplicate"))))))

(deftest comparator-starts-query-timing-after-ocaml-ready-line
  (let [queries-path (str test-root "/ready-queries.edn")
        runner-path (str test-root "/fake-ready-runner.sh")
        report-path (str test-root "/ready-report.md")]
    (recreate-dir test-root)
    (spit
      queries-path
      (pr-str
        [{:id "normal"
          :file "src/query.cljs"
          :line 1
          :query '[:find ?e :where [?e]]}]))
    (write-file runner-path (fake-ready-runner-script))
    (.setExecutable (io/file runner-path) true)
    (let [{:keys [exit err]} (shell/sh
                               "clojure"
                               "-Sdeps"
                               compare-deps
                               "-M"
                               "script/compare_logseq_queries.clj"
                               queries-path
                               "unused.sqlite"
                               runner-path
                               report-path
                               "0"
                               "1"
                               "missing-runtime-inputs.edn")
          report (if (.exists (io/file report-path)) (slurp report-path) "")]
      (is (zero? exit) err)
      (is (str/includes? report "- Mismatches: 1"))
      (is (< (or (report-ocaml-elapsed-ms report) Long/MAX_VALUE) 1000)))))

(deftest comparator-runs-queries-with-scalar-inputs
  (let [queries-path (str test-root "/input-queries.edn")
        runner-path (str test-root "/fake-input-runner.sh")
        report-path (str test-root "/input-report.md")]
    (recreate-dir test-root)
    (spit
      queries-path
      (pr-str
        [{:id "input-query"
          :file "src/query.cljs"
          :line 1
          :query '[:find ?e :in $ ?name :where [?e :name ?name]]
          :inputs ["\"Alice\""]}]))
    (write-file runner-path (fake-synth-input-runner-script))
    (.setExecutable (io/file runner-path) true)
    (let [{:keys [exit err]} (shell/sh
                               "clojure"
                               "-Sdeps"
                               compare-deps
                               "-M"
                               "script/compare_logseq_queries.clj"
                               queries-path
                               "unused.sqlite"
                               runner-path
                               report-path
                               "0"
                               "1"
                               "missing-runtime-inputs.edn")
          report (if (.exists (io/file report-path)) (slurp report-path) "")]
      (is (zero? exit) err)
      (is (str/includes? report "- Runnable queries: 1"))
      (is (str/includes? report "- Batch query ids: input-query"))
      (is (str/includes? report "- Mismatches: 0")))))

(deftest comparator-passes-rules-in-declared-input-order
  (let [queries-path (str test-root "/ordered-rule-input-queries.edn")
        runtime-inputs-path (str test-root "/ordered-rule-inputs.edn")
        runner-path (str test-root "/fake-ordered-rule-runner.sh")
        report-path (str test-root "/ordered-rule-report.md")]
    (recreate-dir test-root)
    (spit
      queries-path
      (pr-str
        [{:id "ordered-rule-query"
          :file "src/query.cljs"
          :line 1
          :query '[:find [?e ...] :in $ ?name % :where (named ?name ?e)]
          :inputs ["\"Alice\""]}]))
    (spit runtime-inputs-path (pr-str {:rules '[[(named ?name ?e) [?e :name ?name]]]}))
    (write-file runner-path (fake-collection-input-runner-script))
    (.setExecutable (io/file runner-path) true)
    (let [{:keys [exit err]} (shell/sh
                               "clojure"
                               "-Sdeps"
                               compare-deps
                               "-M"
                               "script/compare_logseq_queries.clj"
                               queries-path
                               "unused.sqlite"
                               runner-path
                               report-path
                               "0"
                               "1"
                               runtime-inputs-path)
          report (if (.exists (io/file report-path)) (slurp report-path) "")]
      (is (zero? exit) err)
      (is (str/includes? report "- Mismatches: 0")))))

(deftest comparator-passes-empty-rules-for-declared-rules-input
  (let [queries-path (str test-root "/empty-rule-input-queries.edn")
        runtime-inputs-path (str test-root "/empty-rule-inputs.edn")
        runner-path (str test-root "/fake-empty-rule-runner.sh")
        report-path (str test-root "/empty-rule-report.md")]
    (recreate-dir test-root)
    (spit
      queries-path
      (pr-str
        [{:id "empty-rule-query"
          :file "src/query.cljs"
          :line 1
          :query '[:find ?b :in $ % ?start ?end :where [?b :block/name] [(>= ?start 0)] [(<= ?end 0)]]
          :inputs ["0" "0"]}]))
    (spit runtime-inputs-path (pr-str {:rules []}))
    (write-file runner-path (fake-synth-input-runner-script))
    (.setExecutable (io/file runner-path) true)
    (let [{:keys [exit err]} (shell/sh
                               "clojure"
                               "-Sdeps"
                               compare-deps
                               "-M"
                               "script/compare_logseq_queries.clj"
                               queries-path
                               "unused.sqlite"
                               runner-path
                               report-path
                               "0"
                               "1"
                               runtime-inputs-path)
          report (if (.exists (io/file report-path)) (slurp report-path) "")]
      (is (zero? exit) err)
      (is (str/includes? report "- Mismatches: 0")))))

(let [{:keys [fail error]} (run-tests)]
  (shutdown-agents)
  (when (pos? (+ fail error))
    (System/exit 1)))
