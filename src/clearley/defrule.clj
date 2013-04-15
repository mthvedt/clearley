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

; TODO: have tagged :choice as rule symbols?
;
; TODO: allow tagged :choice in defrule? sort of messes up the bindings thing.
; Defrule only accepts symbols or token literals. If it's not a symbol
; it's a token literal. End of defrule.
; Maybe defmatch not defrule.
;
; let-match
;
; Being able to def a rule literal would be a feature.

; ===
; Grammar building
; ===

; Resolves a symbol to a seq of clauses
(defn lookup-symbol [thesym thens theenv]
  ; TODO kill ns-resolve and qualify syms
  (if-let [resolved (ns-resolve thens theenv thesym)]
    (if [vector? @resolved]
      @resolved
      (t/IAE "Clause symbol " thesym " must point to a vector"))
    (t/IAE "Cannot resolve rule: " thesym)))

(defn clause-deps [clause]
  (cond (seq? clause) (mapcat clause-deps (rest clause))
        ; TODO what to do here?
        (symbol? clause) [clause]
        true []))

; TODO tag rule values?
(defn deps [{:keys [value] :as rule}]
  (mapcat clause-deps (rest value)))

; goal: a seq of syms
(defn build-grammar-1 [goal thens theenv]
  (loop [syms goal ; syms to resolve
         grammar {}] ; grammar: maps symbols to rules
    (if-let [sym (first syms)]
      (if (contains? grammar sym) ; have we already seen this?
        (recur (rest syms) grammar)
        (let [rulevec (lookup-symbol sym thens theenv)]
          (recur (apply concat (rest syms) (map deps rulevec))
                 (assoc grammar sym rulevec))))
      grammar)))

; ===
; Defrule
; ===

(defn rule
  "Creates a context-free grammar rule. A rule has a required seq of clauses,
  an optional name, and an optional action.
  If not supplied, the default action bundles the args into a list."
  ; TODO eliminate name?
  ([clauses] (rule nil clauses nil))
  ([clauses action] (rule nil clauses action))
  ([name clauses action] {:name name
                          :value (cons :seq clauses)
                          :action action}))

#_(defn token
  "Creates a rule that matches a single given token.
  Its action returns the given value if supplied, or the token if not."
  ([a-token] (token a-token a-token))
  ([a-token value] (rule nil [a-token] (fn [_] value))))

#_(defrecord OneOrMoreImpl [subrule done]
  RuleKernel
  (predict [self] [(rule nil [subrule] vector)
                   (rule nil [self subrule] conj)])
  (rule-deps [self] [subrule])
  (scan [_ _] [])
  (is-complete? [_] done)
  (advance [self] (assoc self :done true))
  (rule-str [_] (str (clause-str subrule) (if done " *" ""))))

#_(defn one-or-more
  "Creates a rule that matches one or more of a clause. Its action returns a vector
  of the matches."
  ([clause]
   (one-or-more (str (clause-str clause) "+") clause))
  ([name clause]
   (merge (OneOrMoreImpl. clause false) {:name name, :action identity})))

#_(defrecord Scanner [rulefn scanned]
  RuleKernel
  (rule-deps [_] [])
  (predict [self] [])
  (scan [self input-token]
    (if (and (not (is-complete? self)) (rulefn input-token))
      [(advance self)]
      []))
  (is-complete? [_] scanned)
  (advance [self] (assoc self :scanned true))
  (rule-str [_] (str (clause-str rulefn) (if scanned " *" ""))))

#_(defn scanner
  "Creates a rule that accepts input tokens. For a token t, if (scanner-fn t)
  is logical true, this rule matches that token. The default action returns the token."
  ([scanner-fn] (scanner scanner-fn identity))
  ([scanner-fn action]
   (wrap-kernel (Scanner. scanner-fn false) nil action)))

#_(defn char-range
  "Creates a rule that accepts any one character within a given range
  given by min and max, inclusive. min and max should be chars. The default
  action is the identity."
  ([min max]
   (char-range min max identity))
  ([min max action]
  (if (not (and (char? min) (char? max)))
    (t/IAE "min and max should be chars"))
  (let [intmin (int min)
        intmax (int max)]
    (scanner #(let [intx (int %)] (and (<= intx intmax) (>= intx intmin)))
             action))))

; ===
; Defrule
; ===

#_(defn qualify [sym]
  (if (-> sym str (clojure.string/split #"/") (= 1)) ; unqualified
    (symbol *ns* sym)
    sym))

; Macro helper fn for def rule. Returns a pair of
; [appropriate-symbol-for-action-body, rule-or-rulename]
(defn- process-nonlist-clause [clause]
  (cond (list? clause) (assert false)
        (symbol? clause) [(symbol (name clause)) `'~clause]
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

  Valid clauses:
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

(defmacro extend-rule
  ; TODO re-doc
  "Like defrule, but for an existing symbol with some rules bound to it,
  such as one defined by defrule."
  [sym & impl-or-impls]
  `(def ~sym (concat ~sym (list ~@(build-defrule-bodies sym impl-or-impls)))))

(defmacro add-rules
  ; TODO re-doc
  "Adds some amount of additional rules to a symbol with rules bound to it,
  such as one defined by defrule. The given rules must be rule objects, not
  defrule-style definitions."
  [sym & rules]
  `(def ~sym (vec (concat ~sym [~@rules]))))

; In the future, we might bind &env to theenv
; The form of &env is not fixed by Clojure authors so don't do it now
(defn build-grammar-with-ns
  "Builds a grammar in the given ns from the given goal clause.
  Symbols in the grammar will be unqualified."
  [goal thens]
  (build-grammar-1 [goal] thens {}))

; TODO qualify symbols?
(defmacro build-grammar
  "Builds a grammar in the current ns from the given goal clause.
  A grammar is a map from symbols to seqs of rules.
  Symbols in the grammar are unqualified."
  [goal]
  `(build-grammar-with-ns '~goal *ns*))
