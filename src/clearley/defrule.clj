(ns clearley.defrule
  "Fns and macros to define context-free grammars.
  Emphasis is on power, flexibility, and ease of use."
  (require [uncore.throw :as t])
  (use uncore.core))

(defmacro defrulefn [sym doc arg1 default-action & full-body]
  `(defn ~sym ~doc
     ([~arg1] (~sym nil ~arg1 ~default-action))
     ([~arg1 ~'action] (~sym nil ~arg1 ~'action))
     ([~'name ~arg1 ~'action] ~@full-body)))

(defrulefn rule
  "Creates a context-free grammar rule. A rule has a required seq of clauses,
  an optional name, and an optional action.
  If not supplied, the default action bundles the args into a list."
  clauses list-identity
  {:name name, :tag :seq, :value (vec clauses), :action action})

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
                   ; Return it; will be eval'd by the reader
                   therule
                   ; See what process-nonlist-clause has to say
                   (second (process-nonlist-clause therule)))])
      (t/IAE "Not a valid match clause: " clause))
    (process-nonlist-clause clause)))

(defmacro match
  "Defines a rule together with an action. If a subrule is a symbol,
  it can be bound to symbols in the action body. You can also supply a
  renaming binding symbol.
  
  Examples:
  
  (match [\\- num (- num))
  (match [(num1 num) \\+ (num2 num)] (+ num1 num2))"
  [clauses & body]
  (if (vector? clauses)
    (let [processed-clauses (map process-clause clauses)]
      `(rule [~@(map second processed-clauses)]
             (fn [~@(map first processed-clauses)] ~@body)))
    (t/IAE "Match clauses must be a vector")))

(defn- build-multi-match [form]
  (cond (list? form) (let [[f1 & rest] form]
                       (cond (vector? f1) `(match ~@form)
                             (string? f1) `(match ~(vec f1) ~@rest)
                             true (t/IAE "Not a valid start to a defrule body: "
                                         form ", expect vector or string")))
        (symbol? form) `'~form
        true (t/IAE "Not a valid defrule body: " form
                    ", expected list or symbol")))

(defmacro defrule
  "Defines a rule and an action together. This macro is intended to be
  the primary way to def rules.

  Usage:
  (defrule symbol [subrules] action-body?)
  (defrule symbol ([subrules] action-body? | symbol)+)
  Subrules may be a vector or a string. It can contain any rule,
  but if it's a symbol, this can bind symbols in the action body.

  Examples:

  (defrule true-token \"true\" true)
  This matches the string \"true\" and returns true.

  (defrule sum [num \\+ (num2 num)] (+ num num2)
               [num \\- (num2 num)] (- num num2))
  This matches any num, followed by + or -, followed by another num,
  and returns the value of adding or subtracting the two respectively.

  (defrule num ([\\- posnum] (- posnum))
                posnum)
  This matches any posnum preceded by -, returning the negation of posnum.
  Or it can match a posnum directly, and return the posnum (default).

  You can embed rules in named subrules, viz:
  (defrule digit [(x (char-range 0 9))] x)

  Symbols in the defrule bodies do not become qualified."
  [sym & impl-or-impls]
  `(def ~sym ~(let [[first-form & rest] impl-or-impls]
                (cond (vector? first-form)
                      `(match ~@impl-or-impls)
                      (string? first-form)
                      `(match ~(vec first-form) ~@rest)
                      true
                      `(list :or ~@(map build-multi-match impl-or-impls))))))

(defmacro bind
  "A format for defining a rule and an action that binds to some subrules.
  Syntax is as in let, except the binding values are rules.

  Example:
  (letrule [a rule1
            b `(:or rule2-1 rule2-2)
            [c d] rule3]
    (str rule1 rule2 rule3))"
  [binding-forms & body]
  (let [pairs (partition 2 binding-forms)
        rules (map (fn [key] (if (symbol? key) `'~key key))
                   (map second pairs))
        val-gensyms (repeatedly (count pairs) #(gensym "bind_"))]
    (if (some #(not (= 2 %)) (map count pairs))
      (t/IAE "Binding forms must come in pairs"))
    `(rule ~(vec rules)
           (fn [~@val-gensyms]
             (let [~@(interleave (map first pairs) val-gensyms)]
               ~@body)))))

(defmacro defbind
  "Like bind but defs a variable."
  [sym & body]
  `(def ~sym (assoc (bind ~@body) :name '~sym)))

(load "core_stdlib")
