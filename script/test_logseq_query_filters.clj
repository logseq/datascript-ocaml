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

(let [{:keys [fail error]} (run-tests)]
  (shutdown-agents)
  (when (pos? (+ fail error))
    (System/exit 1)))
