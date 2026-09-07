(ns datascript.api
  (:require [ocaml.package/datascript-ocaml-lg]
            [ocaml.Datascript_lg :as api]
            [ocaml.Datascript_lg.Codec :as codec]
            [lg.literal :as literal]))

(def string-codec codec/string)
(def int-codec codec/int)
(def float-codec codec/float)
(def bool-codec codec/bool)
(def keyword-codec codec/keyword)
(def entity-id-codec codec/entity_id)

(defmacro form [value]
  (list 'lg.literal/build
    {:nil 'Datascript.QueryFormNil
     :bool 'Datascript.QueryFormBool
     :int 'Datascript.QueryFormInt
     :float 'Datascript.QueryFormFloat
     :string 'Datascript.QueryFormString
     :keyword 'Datascript.QueryFormKeyword
     :symbol 'Datascript.QueryFormSymbol
     :vector 'Datascript.QueryFormVector
     :list 'Datascript.QueryFormList
     :set 'Datascript.QueryFormSet
     :map 'Datascript.QueryFormMap}
    value))

(defmacro schema [value]
  (list 'Datascript_lg.schema (list 'datascript.api/form value)))

(defmacro tx [value]
  (list 'Datascript_lg.tx (list 'datascript.api/form value)))

(defmacro transact! [conn value]
  (list 'Datascript_lg.transact conn (list 'datascript.api/form value)))

(defmacro query [projection value]
  (let [value (if (seq? value)
                (if (= 'quote (first value)) (second value) value)
                value)]
    (list 'Datascript_lg.prepare_query projection (list 'datascript.api/form value))))

(defmacro pull [db selector reference projection]
  (list 'Datascript_lg.or_raise
    (list 'Datascript_lg.pull_form db (list 'datascript.api/form selector)
      reference projection)))

(defmacro q [query db & inputs]
  (let [source (if (and (seq? query) (= 'quote (first query))) (second query) query)
        literal? (vector? source)
        options? (and literal? (map? (first inputs)))
        prepared (if literal?
                   (if options?
                     (list 'datascript.api/query-value (first inputs) source)
                     (list 'datascript.api/query-value source))
                   query)
        inputs (if options? (next inputs) inputs)]
    (list 'Datascript_lg.or_raise
      (list 'Datascript_lg.run_query prepared db (cons 'list inputs)))))

(defmacro defattr [binding attr kind options]
  (assert (keyword? attr) "defattr expects an attribute keyword")
  (let [codec (case kind
                :string 'datascript.api/string-codec
                :int 'datascript.api/int-codec
                :float 'datascript.api/float-codec
                :bool 'datascript.api/bool-codec
                :keyword 'datascript.api/keyword-codec
                nil)]
    (assert (if (nil? codec) false true) "unsupported attribute codec")
    (list 'def binding
      (list 'Datascript_lg.Attribute.make
        (if (namespace attr) (str (namespace attr) "/" (name attr)) (name attr))
        codec
        (list 'Datascript_lg.schema_spec (list 'datascript.api/form options))))))

(defmacro entity [reference attributes]
  (assert (map? attributes) "entity expects a map of typed attributes to values")
  (let [reference-name (gensym "entity_ref_")
        bindings (volatile! [reference-name reference])
        entries (map (fn [pair]
                       (let [attribute (first pair)
                             value-name (gensym "entity_value_")]
                         (assert (symbol? attribute) "entity keys must name typed attributes")
                         (IVolatile/-vreset! bindings
                           (conj (conj (deref bindings) value-name) (second pair)))
                         (list 'Datascript_lg.Attribute.entry attribute value-name)))
                     attributes)]
    (list 'let (deref bindings)
      (list 'Datascript_lg.entity reference-name (cons 'list entries)))))

(defmacro transact-ops! [conn & operations]
  (let [conn-name (gensym "tx_conn_")
        bindings (volatile! [conn-name conn])
        values (map (fn [operation]
                      (let [name (gensym "tx_op_")]
                        (IVolatile/-vreset! bindings
                          (conj (conj (deref bindings) name) operation))
                        name)) operations)]
    (list 'let (deref bindings)
      (list 'Datascript.transact_conn conn-name (cons 'list values)))))

(defmacro query-value [& args]
  (assert (or (= 1 (count args)) (= 2 (count args)))
    "defquery expects an optional {:types {?variable codec}} and a query")
  (let [not (fn [value] (if value false true))
        get (fn [entries key]
              (second (first (filter (fn [entry] (= key (first entry))) entries))))
        options (if (= 2 (count args)) (first args) {})
        source (if (= 2 (count args)) (second args) (first args))
        source (if (and (seq? source) (= 'quote (first source))) (second source) source)
        variable? (fn [value]
                    (and (symbol? value) (= "?" (subs (str value) 0 1))))
        sections (volatile! {})
        evidence (volatile! {})
        checks (volatile! [])
        bindings (volatile! [])
        add-type! (fn [variable codec]
                    (when (variable? variable)
                      (let [previous (get (deref evidence) variable)
                            codec-name (symbol (str "_query_codec_" (gensym)))]
                        (IVolatile/-vreset! bindings
                          (conj (conj (deref bindings) codec-name) codec))
                        (if (nil? previous)
                          (IVolatile/-vreset! evidence (assoc (deref evidence) variable codec-name))
                          (IVolatile/-vreset! checks
                            (conj (deref checks)
                              (list 'Datascript_lg.Codec.agree previous codec-name)))))))]
    (assert (map? options) "defquery options must be a map")
    (map (fn [option]
           (assert (or (= :types (first option)) (= :attributes (first option))) "unsupported defquery option")) options)
    (let [attributes (get options :attributes)]
      (when (not (nil? attributes))
        (assert (map? attributes) "defquery :attributes must map keywords to attribute symbols")
        (map (fn [entry]
               (assert (and (keyword? (first entry)) (symbol? (second entry)))
                 "defquery :attributes must map keywords to attribute symbols")) attributes)))
    (assert (vector? source) "defquery expects a query vector")
    (let [parse (fn parse [remaining section]
       (when (not (empty? remaining))
         (let [value (first remaining)]
           (if (keyword? value)
             (do
               (assert (or (= :find value) (= :where value) (= :in value) (= :with value))
                 "unsupported defquery section")
               (assert (nil? (get (deref sections) value)) "duplicate defquery section")
               (IVolatile/-vreset! sections (assoc (deref sections) value []))
               (parse (next remaining) value))
             (do
               (assert (not (nil? section)) "defquery expects section keywords")
               (IVolatile/-vreset! sections
                 (assoc (deref sections) section (conj (get (deref sections) section) value)))
               (parse (next remaining) section))))))]
      (parse source nil))
    (assert (and (not (empty? (get (deref sections) :find))) (not (empty? (get (deref sections) :where))))
      "defquery requires :find and :where")
    (let [hints (get options :types)]
      (when (not (nil? hints))
        (assert (map? hints) "defquery :types must map variables to codecs")
        (map (fn [hint]
               (assert (variable? (first hint)) "defquery :types keys must be variables")
               (add-type! (first hint) (second hint))) hints)))
    (let [clauses
          (map (fn [clause]
                 (assert (vector? clause) "unsupported defquery clause; use an explicit query projection")
                 (if (seq? (first clause))
                   (do
                     (assert (or (= 1 (count clause)) (= 2 (count clause)))
                       "unsupported defquery function clause")
                     clause)
                   (let [source? (and (symbol? (first clause))
                                      (= "$" (subs (str (first clause)) 0 1)))
                         parts (if source? (next clause) clause)
                         size (count parts)]
                     (assert (or (= size 2) (= size 3) (= size 4)) "unsupported defquery datom pattern")
                     (let [entity (first parts)
                           attribute (second parts)
                           declared (get (get options :attributes) attribute)
                           typed? (and (symbol? attribute)
                                       (not (variable? attribute)) (not (= '_ attribute)))
                           value (first (drop 2 parts))]
                       (add-type! entity 'datascript.api/entity-id-codec)
                       (add-type! attribute 'datascript.api/keyword-codec)
                       (when typed?
                         (add-type! value (list 'Datascript_lg.Attribute.codec attribute)))
                       (when (not (nil? declared))
                         (assert (keyword? attribute) "defquery attribute mappings require keywords")
                         (add-type! value
                           (list 'Datascript_lg.Attribute.codec_for
                             (if (namespace attribute)
                               (str (namespace attribute) "/" (name attribute)) (name attribute))
                             declared)))
                       (when (= size 4)
                         (add-type! (first (drop 3 parts)) 'datascript.api/entity-id-codec))
                       (if typed?
                         (vec (concat (if source? [(first clause)] [])
                           [entity (list 'unquote :keyword (list 'Datascript_lg.Attribute.name attribute))]
                           (drop 2 parts)))
                         clause)))))
               (get (deref sections) :where))
          finds (get (deref sections) :find)
          visited (volatile! [])
          projections (map (fn [variable]
                             (assert (variable? variable) "defquery expects relation :find variables")
                             (let [codec (get (deref evidence) variable)
                                   index (count (deref visited))]
                               (assert (not (nil? codec))
                                 (str "cannot infer query variable " variable "; supply :types"))
                               (IVolatile/-vreset! visited (conj (deref visited) variable))
                               (list 'Datascript_lg.Projection.column index codec))) finds)
          pair (fn pair [values constructor]
                 (if (= 1 (count values)) (first values)
                   (list constructor (first values) (pair (next values) constructor))))
          projection (pair projections 'Datascript_lg.Projection.pair)
          names (map (fn [_] (gensym "column_")) finds)
          projection (if (not (or (= (count finds) 1) (= (count finds) 2)))
                       (let [row (gensym "row_")]
                         (list 'Datascript_lg.Projection.map
                           (list 'fn [row]
                             (list 'match row (pair names 'tuple) (cons 'tuple names)))
                           projection))
                       projection)
          query (concat [:find] finds
                  (if (not (empty? (get (deref sections) :in)))
                    (cons :in (get (deref sections) :in)) [])
                  (if (not (empty? (get (deref sections) :with)))
                    (cons :with (get (deref sections) :with)) [])
                  [:where] clauses)]
      (list 'let (deref bindings)
        (cons 'do
          (concat (deref checks)
            [(list 'Datascript_lg.prepare_query projection
               (list 'datascript.api/form (vec query)))]))))))

(defmacro defquery [binding & args]
  (assert (symbol? binding) "defquery expects a binding name")
  (list 'def binding (cons 'datascript.api/query-value args)))
