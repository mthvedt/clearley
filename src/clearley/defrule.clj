(ns clearley.defrule
  "Tools for parsing and processing linear input.
  The central abstraction is the context-free grammar, where one match rule
  maps to arbitrary sequences of sub-rules.
  Emphasis is on completeness, modularity, and ease of use.
  A functional API and a macro DSL are provided.
 
  See the high-level docs for a further background and overview." 
  (require [clojure string pprint]
           [uncore.throw :as t])
  (use uncore.core clearley.rules))
; TODO put grammar building in different ns

; === Basic stuff ===

(defmacro defrulefn [sym doc arg1 default-action & full-body]
  `(defn ~sym ~doc
     ([~arg1] (~sym nil ~arg1 ~default-action))
     ([~arg1 ~'action] (~sym nil ~arg1 ~'action))
     ([~'name ~arg1 ~'action] ~@full-body)))

; TODO include default actions?
; TODO have value always be a vector?
; TODO expose to-rule, normalize
; TODO arguments-checking
(defrulefn rule
  "Creates a context-free grammar rule. A rule has a required seq of clauses,
  an optional name, and an optional action.
  If not supplied, the default action bundles the args into a list."
  clauses list-identity
  {:name name, :tag :seq, :value (vec clauses), :action action})

(defrulefn scanner
  "Creates a rule that accepts input tokens. For a token t, if (scanner-fn t)
  is logical true, this rule matches that token.
  The default action returns the token."
  scanner-fn identity
  {:name name, :tag :scanner, :action action, :value [scanner-fn]})

(defrulefn token
  "Creates a rule that matches a token. The default action returns the token."
  token (fn [] token)
  {name name, :tag :token, :value [token], :action action})

(defrulefn symbol-rule
  "Creates a rule that points to some other rule, identified by the given symbol.
  The default action is the identity."
  a-symbol identity
  {name name, :tag :symbol, :value [a-symbol], :action action})

; === Grammar building ===

; Resolves a symbol
(defn- lookup-symbol [thesym thens theenv]
  (if-let [resolved (ns-resolve thens theenv thesym)]
    (if [vector? @resolved]
      @resolved
      (t/IAE "Clause symbol " thesym " must point to a vector")) ;TODO WTF?
    (t/IAE "Cannot resolve rule: " thesym)))

(defn rule-type [rule]
  ; Allowed rules: symbol, literal object (token), map, tagged clause
  (cond (sequential? rule) ::tagged-clause
        (map? rule) ::rule
        (symbol? rule) ::symbol
        true ::token))

(defmulti default-action (fn [tag _] tag))
(defmethod default-action :default [& _] identity)
(defmethod default-action :star [& _] list-identity)
(defmethod default-action :seq [& _] list-identity)
(defmethod default-action :token [_ {[token] :value}] (fn [] token))

; TODO unify similar items
(declare map-normalize)

(defn normalize [rule candidate-name]
  (case (rule-type rule)
    ::tagged-clause (let [[tag & rest] rule]
                      {:tag tag, :value (vec (map-normalize rest candidate-name tag)),
                       :action (default-action tag rest), :name candidate-name})
    ::rule (let [name (if (:name rule) (:name rule) candidate-name)
                 value (vec (map-normalize (:value rule) name (:tag rule)))]
             (merge rule {:name name :value value}))
    ::symbol {:name (str rule), :tag :symbol, :value [rule], :action identity}
    ::token {:name (pr-str rule), :tag :token, :value [rule], :action (fn [] rule)}))

(defn- map-normalize [rules parent-name parent-tag]
  (if (contains? #{:token :scanner} parent-tag) ; exempt from normalization
    rules
    (map normalize rules (map #(str (name parent-tag) "@" parent-name "." %)
                              (range)))))

(defn deps [{:keys [tag value]}]
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

; === Defrule ===

; Macro helper fn for def rule. Returns a pair of
; [appropriate-symbol-for-action-body, rule-or-rulename]
(defn- process-nonlist-clause [clause]
  (cond (list? clause) (assert false)
        (symbol? clause) [clause `'~clause]
        (keyword? clause) [(symbol (name clause)) clause]
        (string? clause) [(symbol clause) clause]
        true ['_ clause])) ; can't be an arg in a fn

; Macro helper fn for def rule. Returns a pair of
; [appropriate-symbol-for-action-body, rule-or-rulename-or-uneval'd-form]
(defn- process-clause [clause]
  (if (list? clause)
    (if-let [thename (first clause)]
      (let [therule (second clause)]
        [thename (if (list? therule) ; A form to evaluate
                   ; Return it; will be eval'd when defrule called
                   therule
                   ; See what process-nonlist-clause has to say
                   (second (process-nonlist-clause therule)))])
      (t/IAE "Not a valid subrule: " clause))
    (process-nonlist-clause clause)))

; TODO: eliminate most of the logic here.
; Macro helper fn. Builds the `(rule ...) bodies for defrule.
; Head: a symbol. impls: seq of (bindings bodies+) forms.
(defn- build-defrule-rule-bodies [head impls]
  (vec (map (fn [impl]
              (let [clauses (first impl)]
                (if (seq clauses)
                  (let [processed-clauses (map process-clause clauses)]
                    `(rule '~head [~@(map second processed-clauses)]
                           (fn [~@(map first processed-clauses)] ~@(rest impl))))
                  (t/IAE "Rule clauses must be seqable"))))
            impls)))

; Macro helper fn. Builds the body for defrule and related macros.
; Head: a symbol. impl-or-impls: (clauses bodies+) or ((clauses bodies+)+).
(defn- build-defrule-bodies [head impl-or-impls]
  (let [first-form (first impl-or-impls)]
    (cond (or (vector? first-form) (string? first-form))
          (build-defrule-rule-bodies head [(apply list first-form
                                                  (rest impl-or-impls))])
          (seq? first-form) (build-defrule-rule-bodies head impl-or-impls)
          true (t/IAE "Not a valid defrule; "
                     "expected clause vector, string, or clause-body pairs"))))

(defmacro defrule
  "Defs a parser rule or seq of parser rules. See the docs for
  the full defrule syntax.
  
  Usage:
  (defrule symbol [clauses] action?)
  (defrule (symbol [clauses] action?)+)

  The clauses may be any seq, so you can do something like
  (defrule true-rule \"true\" true)

  A clause may be any of the following:
  any rule object
  a symbol pointing to a seq of rules
  [rule or rule-symbol]+
  (rule-alias-symbol rule-symbol)
  (rule-alias-symbol [rule or rule-symbol]+)
  
  Defines one or more rules and binds them in a seq to the given var.
  The rule's name will be the given (unqualified) symbol by default.
  The optional action form defines a parse action, where the symbols will be bound
  to the results of the actions of the correspoinding subrules.

  Example:

  (defrule sum [num \\+ (num2 num)] (+ num num2))
  
  The above rule matches two nums (one of which is aliased as num2)
  and adds them together. If a parse action is not provided, a default
  will be used which bundles its args into a list. The rule will be bound
  to 'sum in the current namespace.
  
  Symbols in the defrule bodies do not become qualified."
  [sym & impl-or-impls]
  `(def ~sym (list :or ~@(build-defrule-bodies sym impl-or-impls))))

; In the future, we might bind &env to theenv
; The form of &env is not fixed by Clojure authors so don't do it now
(defn build-grammar-with-ns
  "Builds a grammar in the given ns from the given goal clause.
  Symbols in the grammar will be unqualified."
  [goal thens]
  (build-grammar-helper [goal] thens {}))

(defmacro build-grammar
  "Builds a grammar in *ns* from the given goal clause.
  A grammar is a map from symbols to seqs of rules.
  Symbols in the grammar are unqualified."
  [goal]
  `(build-grammar-with-ns '~goal *ns*))

; ===
; Convenient fns
; ===

(load "core_stdlib")
