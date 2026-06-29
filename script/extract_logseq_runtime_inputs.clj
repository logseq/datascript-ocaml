(ns extract-logseq-runtime-inputs
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as rt]))

(defn read-file-forms [file]
  (with-open [r (rt/indexing-push-back-reader (slurp file))]
    (loop [forms []]
      (let [form (reader/read {:eof ::eof
                               :read-cond :allow
                               :features #{:clj :cljs}
                               :default (fn [tag value] (list 'tagged-literal tag value))}
                              r)]
        (if (= ::eof form)
          forms
          (recur (conj forms form)))))))

(defn quoted-value [form]
  (if (and (seq? form) (= 'quote (first form)))
    (second form)
    form))

(defn def-form [forms name]
  (some (fn [form]
          (when (and (seq? form) (= 'def (first form)) (= name (second form)))
            (last form)))
        forms))

(defn merge-rules-form [base-rules form]
  (let [form (quoted-value form)]
    (cond
      (map? form) form
      (and (seq? form) (= 'merge (first form)))
      (reduce
       (fn [acc part]
         (let [part (quoted-value part)]
           (cond
             (= 'rules part) (merge acc base-rules)
             (map? part) (merge acc part)
             :else acc)))
       {}
       (rest form))
      :else {})))

(defn flatten-rules [rules-map]
  (->> rules-map
       vals
       (mapcat (fn [rule]
                 (let [rule (quoted-value rule)]
                   (if (vector? (first rule)) rule [rule]))))
       vec))

(defn -main [& [logseq-root out]]
  (when-not logseq-root
    (binding [*out* *err*]
      (println "Usage: clojure -M script/extract_logseq_runtime_inputs.clj LOGSEQ_ROOT [OUT]"))
    (System/exit 2))
  (let [out (or out "test/logseq_runtime_inputs.edn")
        rules-file (io/file logseq-root "deps/db/src/logseq/db/frontend/rules.cljc")
        forms (read-file-forms rules-file)
        base-rules (merge-rules-form {} (def-form forms 'rules))
        db-query-dsl-rules (merge-rules-form base-rules (def-form forms 'db-query-dsl-rules))
        result {:rules (flatten-rules db-query-dsl-rules)}]
    (spit out (with-out-str (pprint/pprint result)))
    (binding [*out* *err*]
      (println "wrote runtime inputs to" out)
      (println "rules:" (count (:rules result))))))

(apply -main *command-line-args*)
