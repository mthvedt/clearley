(ns clearley.grammar
  "The base lib for the Clearley intermediate representation."
  (require [uncore.throw :as t])
  (use uncore.core))

; === Grammar building ===

; Resolves a symbol
(defn- lookup-symbol [thesym thens theenv]
  (if-let [resolved (ns-resolve thens theenv thesym)]
    @resolved
    (t/IAE "Cannot resolve rule: " thesym)))

(defn rule-type
  "Gets the rule type for an unnormalized rule. The rule types are
  ::tagged-clause, ::rule, ::symbol, ::token."
  [rule]
  (cond (sequential? rule) ::tagged-clause
        (map? rule) ::rule
        (symbol? rule) ::symbol
        true ::token))

(defrecord ^{:doc "An IFn record that matches a token. Is a record
                  to support =."}
  TokenAction [token]
  clojure.lang.IFn
  (invoke [_] token)
  (applyTo [_ args] token))

(defmulti default-action
  "Gets the default action for a (nonnormalized) rule."
  (fn [tag _] tag))
(defmethod default-action :default [& _] identity)
(defmethod default-action :star [& _] list-identity)
(defmethod default-action :seq [& _] list-identity)
(defmethod default-action :token [_ {[token] :value}] (TokenAction. token))

; TODO unify similar items?
(declare map-normalize)

(defn normalize
  "Turns a rule into a 'normalized' rule map with :tag, :value, :action,
  :name and :original. The :values will also be normalized. :action and :name
  may be auto-populated. :original will point to the original, unnormalized rule."
  [rule candidate-name]
  (case (rule-type rule)
    ::tagged-clause (let [[tag & rest] rule]
                      {:tag tag, :value (map-normalize rest candidate-name tag),
                       :action (default-action tag rest), :name candidate-name,
                       :original rule})
    ::rule (let [name (if (:name rule) (:name rule) candidate-name)
                 value (map-normalize (:value rule) name (:tag rule))]
             (merge rule {:name name, :value value, :original rule}))
    ::symbol {:name (str rule), :tag :symbol, :value [rule], :action identity,
              :original rule}
    ::token {:name (pr-str rule), :tag :token, :value [rule],
             :action (TokenAction. rule), :original rule}))

(defn- map-normalize [rules parent-name parent-tag]
  (if (contains? #{:token :scanner} parent-tag) ; exempt from normalization
    ; TODO why?
    rules
    (vec (map normalize rules (map #(str parent-name "." %)
                              (range))))))

(defn- deps [{:keys [tag value]}]
  (cond (= tag :token) []
        (= tag :scanner) []
        (= tag :symbol) value
        true (mapcat deps value)))

; seed: a seqable of syms
(defn- build-grammar-helper [seed thens theenv]
  (loop [syms seed ; syms to resolve
         grammar {}] ; grammar: maps symbols to rules
    (if-let [sym (first syms)]
      (if (contains? grammar sym) ; have we already seen this?
        (recur (rest syms) grammar)
        (let [resolved (normalize (lookup-symbol sym thens theenv)
                                  (str sym))]
          (recur (doall (concat (rest syms) (deps resolved)))
                 (assoc grammar sym resolved))))
      grammar)))

; In the future, we might bind &env to theenv
; The form of &env is not fixed by Clojure authors so don't do it now
(defn build-grammar-with-ns
  "Builds a grammar in the given ns from the given goal rule."
  [goal thens]
  (build-grammar-helper [goal] thens {}))

(defmacro build-grammar
  "Builds a grammar in *ns* from the given goal rule.
  A grammar maps (usually unqualified) symbols to normalized rules.
  Returns the grammar."
  [goal]
  `(build-grammar-with-ns '~goal *ns*))
