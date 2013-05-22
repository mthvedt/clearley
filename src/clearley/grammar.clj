(ns clearley.grammar
  "The base lib for creating Clearley grammars. A Clearley grammar maps
  symbols to full-fledged rule maps, called 'normalized' rules.
  
  Unless you are doing advanced stuff, you probably don't need this namespace."
  (require [uncore.throw :as t]
           backtick)
  (use uncore.core))

; === Grammar building ===

(definline ^{:doc "An inlinable identity fn for use in parsers."}
  fast-identity [x] x)

; Resolves a symbol
(defn- lookup-symbol [thesym thens theenv]
  (if (symbol? thesym)
    (if-let [resolved (ns-resolve thens theenv thesym)]
      @resolved
      (t/IAE "Cannot resolve rule: " thesym))
    (t/IAE thesym " is not a symbol")))

(defn rule-type
  "Gets the rule type for an unnormalized rule. The rule types are
  ::tagged-clause, ::rule, ::symbol, ::token."
  [rule]
  (cond (sequential? rule) ::tagged-clause
        (map? rule) ::rule
        (symbol? rule) ::symbol
        true ::token))

(defmulti default-action
  "Gets the default action for a (nonnormalized) rule."
  (fn [tag _] tag))
(defmethod default-action :default [& _] `fast-identity)
(defmethod default-action :star [& _] list-identity)
(defmethod default-action :seq [& _] list-identity)
(defmethod default-action :token [_ {[token] :value}] `fast-identity)

(declare map-normalize)

(defn normalize
  "Turns a rule into a 'normalized' rule map with :tag, :value, :action,
  :name and :original. The :values will also be normalized, unless
  it is a token or scanner. :action and :name may be auto-populated.
  Symbols in rules will be qualified in the namespace bound to *ns*.
  (If a symbol cannot be qualified, that is an error.)
  :original will point to the original, unnormalized rule."
  [rule candidate-name]
  (case (rule-type rule)
    ::tagged-clause (let [[tag & rest] rule]
                      {:tag tag, :value (map-normalize rest candidate-name tag),
                       :action (default-action tag rest), :name candidate-name,
                       :original rule})
    ::rule (let [name (if (:name rule) (:name rule) candidate-name)
                 value (map-normalize (:value rule) name (:tag rule))]
             (merge rule {:name name, :value value, :original rule}))
    ::symbol {:name (name rule), :tag :symbol,
              :value [(backtick/resolve-symbol rule)],
              :action `fast-identity, :original rule}
    ; TODO fix: chars instead of longs
    ::token {:name (pr-str rule), :tag :token, :value [rule],
             :action `fast-identity, :original rule}))

(defn- map-normalize [rules parent-name parent-tag]
  (if (contains? #{:token :scanner} parent-tag)
    rules
    (vec (map normalize rules (map #(str parent-name "-" %)
                              (range))))))

(defn- deps [{:keys [tag value]}]
  (cond (= tag :token) []
        (= tag :scanner) []
        (= tag :symbol) value
        true (mapcat deps value)))

; seed: a seqable of syms
; TODO make this stack-based so we can tell for what rule a normalization fails
(defn- build-grammar-helper [seed thens theenv]
  (loop [syms (map backtick/resolve-symbol seed) ; syms to resolve
         grammar {}] ; grammar: maps symbols to rules
    (if-let [sym (first syms)]
      (if (contains? grammar sym) ; have we already seen this?
        (recur (rest syms) grammar)
        (let [resolved (normalize (lookup-symbol sym thens theenv)
                                  (name sym))]
          (recur (doall (concat (rest syms) (deps resolved)))
                 (assoc grammar sym resolved))))
      grammar)))

(defn build-grammar-with-ns
  "Builds a grammar in the given ns from the given goal symbol."
  [goal thens]
  (binding [*ns* thens]
    (build-grammar-helper [goal] thens {})))

(defmacro build-grammar
  "Builds a grammar in *ns* from the given goal symbol.
  A grammar maps qualified symbols to normalized rules. Returns the grammar."
  [goal]
  `(build-grammar-with-ns '~goal *ns*))
