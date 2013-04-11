(ns clearley.rules
  "Back-end stuff for Clearley. Work in progress with unstable API."
  (require clojure.string
           [uncore.throw :as t]
           [uncore.str :as s]))

; TODO merge into core? simplify rule kernel?
(defrecord Match [rule submatches])

(defn match [rule submatches]
  (Match. rule submatches))

; rule-deps: rule dependencies
; predict: predicts a single clause that must be matched
; scan: scans a single input
; is-complete?: is this rule fully matched
; advance: called when a predicted clause is matched
; rule-str: a short useful string representation of this rule
;
; TODO ELIMINATE ALL?
;
; Our problems are:
; * Define self-referential rule builders.
; * Allow custom rules.
; * Yet keep things simple.
; * An almost fanatical devotion to the pope, and nice red uniforms.
;
; Plan:
; * Prefix lists. choice, &c
; * Still some kind of prediction?
(defprotocol RuleKernel
  (rule-deps [self])
  (predict [self])
  (scan [self input-token])
  (is-complete? [self])
  (advance [self])
  (rule-str [self]))

(declare context-free-rule)

(defn rule? [x]
  (instance? clearley.rules.RuleKernel x))

(defn rule-action [rule]
  (if (rule? rule)
    ; work around :key -> nil maps in defrecords
    (if-let [r (:action rule)]
      r
      (fn [& xs] (vec xs)))
    (fn [] rule)))

; ===
; Rule clauses
; ===

(def cmap
  {\newline "\\n"
   \tab "\\t"
   \backspace "\\b"
   \formfeed "\\f"
   \return "\\r"})

; A short string representation of a clause.
(defn clause-str [clause]
  (cond
    (and (rule? clause) (:name clause)) (:name clause)
    (symbol? clause) (str clause)
    (string? clause) (clojure.string/escape (str \" clause \") cmap)
    (char? clause) (clojure.string/escape (str \' clause \') cmap)
    (keyword? clause) (str clause)
    (or (vector? clause)
        (seq? clause)) (str "["
                            (s/separate-str ", " (map clause-str clause))
                            "]")
    true (clojure.string/escape (str clause) cmap)))

; Resolves a symbol to a seq of clauses
(defn lookup-symbol [thesym thens theenv]
  (if-let [resolved (ns-resolve thens theenv thesym)]
    (let [resolved @resolved]
      (if (or (vector? resolved) (seq? resolved))
        resolved
        [resolved]))
    (t/IAE "Cannot resolve rule for head: " thesym)))

; Rule-ifies the given clause, wrapping it in a one-clause rule if neccesary.
(defn to-rule [clause]
  (if (rule? clause)
    clause
    (context-free-rule (str "Anon@" (hash clause)) [clause] identity)))

; Processes the clause re. a grammar. If clause is a symbol,
; looks up the symbol, maps to-rule to the result, and adds it to the grammar.
(defn update-grammar [grammar clause thens theenv]
  (if (symbol? clause)
    (assoc grammar clause (map to-rule (lookup-symbol clause thens theenv)))
    grammar))

; Gets a seq of subrules from a clause
(defn predict-clause [clause grammar]
  (cond
    (rule? clause) [clause]
    (seq? clause) (map to-rule clause)
    (vector? clause) (map to-rule clause)
    true (get grammar clause [])))

; All things this clause might point to, that we must resolve at grammar build time
(defn clause-deps [x grammar]
  (cond (rule? x) (rule-deps x)
        true (predict-clause x grammar)))

; ===
; Here be dragons
; ===

(defn build-grammar-1 [goal thens theenv]
  (loop [stack [goal] ; stack: clauses to resolve
         breadcrumbs #{}
         grammar {}] ; grammar: maps keyword clauses to rules
    (if-let [current-clause (first stack)]
      (if (contains? breadcrumbs current-clause)
        ; have we already seen this? skip it entirely
        (recur (rest stack) breadcrumbs grammar)
        ; otherwise, process it
        (let [grammar (update-grammar grammar current-clause thens theenv)
              predictions (clause-deps current-clause grammar)] ; order is important
          (recur (concat predictions (rest stack))
                 (conj breadcrumbs current-clause)
                 grammar)))
      grammar)))

; ===
; Rules
; ===

(defrecord RuleImpl [kernel name action]
  RuleKernel
  (predict [self] (predict kernel))
  (rule-deps [_] (rule-deps kernel))
  (scan [self input-token] (map #(assoc self :kernel %) (scan kernel input-token)))
  (is-complete? [_] (is-complete? kernel))
  (advance [self] (assoc self :kernel (advance kernel)))
  (rule-str [_] (rule-str kernel)))

(defn wrap-kernel [kernel name action]
  (RuleImpl. kernel name action))

(defn cfg-rule-str [rule-deps dot]
  (let [clause-strs (map clause-str rule-deps)]
    (s/separate-str " " (if (zero? dot)
                        clause-strs
                        (concat (take dot clause-strs) ["*"]
                                (drop dot clause-strs))))))

(defrecord CfgRule [clauses dot]
  RuleKernel
  (rule-deps [_] clauses)
  (predict [self]
    (if (is-complete? self)
      []
      (get clauses dot)))
  (scan [self input-token]
    (if (and (not (is-complete? self)) (= (get clauses dot) input-token))
      [(advance self)]
      []))
  (is-complete? [_]
    (= dot (count clauses)))
  (advance [self] (assoc self :dot (inc dot)))
  (rule-str [_] (cfg-rule-str clauses dot)))

(defn context-free-rule [name clauses action]
  (if (= (count clauses) 0)
    (t/IAE "Clauses cannot be empty")
    (wrap-kernel (CfgRule. (vec clauses) 0) name action)))
