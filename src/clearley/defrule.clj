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

; === Grammar building ===

; Resolves a symbol
(defn- lookup-symbol [thesym thens theenv]
  (if-let [resolved (ns-resolve thens theenv thesym)]
    (if [vector? @resolved]
      @resolved
      (t/IAE "Clause symbol " thesym " must point to a vector"))
    (t/IAE "Cannot resolve rule: " thesym)))

(defn- deps [rule]
  (cond (map? rule) (deps (:value rule))
        (seq? rule) (mapcat deps (rest rule))
        (symbol? rule) [rule]
        true []))

; seed: a seqable of syms
(defn- build-grammar-helper [seed thens theenv]
  (loop [syms seed ; syms to resolve
         grammar {}] ; grammar: maps symbols to rules
    (if-let [sym (first syms)]
      (if (contains? grammar sym) ; have we already seen this?
        (recur (rest syms) grammar)
        (let [resolved (lookup-symbol sym thens theenv)]
          (recur (concat (rest syms) (deps resolved))
                 (assoc grammar sym resolved))))
      grammar)))

; === Basic stuff ===

; TODO Figure out what to do with "double named" rules
(defn rule
  "Creates a context-free grammar rule. A rule has a required seq of clauses,
  an optional name, and an optional action.
  If not supplied, the default action bundles the args into a list."
  ([clauses] (rule nil clauses nil))
  ([clauses action] (rule nil clauses action))
  ([name clauses action] {:name name
                          :value (cons :seq clauses)
                          :action action}))

(defn scanner
  "Creates a rule that accepts input tokens. For a token t, if (scanner-fn t)
  is logical true, this rule matches that token.
  The default action returns the token."
  ([scanner-fn] (scanner scanner-fn identity))
  ([scanner-fn action]
   {:action action, :value `(:scanner ~scanner-fn)}))

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
  `(def ~sym (list :defrule ~@(build-defrule-bodies sym impl-or-impls))))

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
