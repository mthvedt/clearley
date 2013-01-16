(ns clearley.rules
  "Back-end stuff for Clearley. Work in progress with unstable API."
  (use clearley.utils)
  (require clojure.string))

; TODO merge into core? simplify rule kernel(?)
(defrecord Match [rule submatches])

(defn match [rule submatches]
  (Match. rule submatches))

; rule-deps: rule dependencies
; predict: predicts a single clause that must be matched
; scan: scans a single input
; is-complete?: is this rule fully matched
; advance: called when a predicted clause is matched
; rule-str: a short useful string representation of this rule
(defprotocol RuleKernel
  (rule-deps [self])
  (predict [self])
  (scan [self input-token])
  (is-complete? [self])
  (advance [self])
  (rule-str [self]))

(declare context-free-rule)

; ===
; Rule clauses
; ===

(defn rule-name [rule]
  (if (instance? clearley.rules.RuleKernel rule)
    (:name rule)
    nil))

(def cmap
  {\newline "\\n"
   \tab "\\t"
   \backspace "\\b"
   \formfeed "\\f"
   \return "\\r"})

(defn clause-str [clause]
  (clojure.string/escape
    (if (instance? clearley.rules.RuleKernel clause)
      (rule-str clause)
      (str clause))
    cmap))

(defn clause-name [clause]
  (cond
    (:name clause) (:name clause)
    (symbol? clause) (str clause)
    (string? clause) (str \" clause \")
    (keyword? clause) (str clause)
    true "anon"))

(defn rule-action [rule]
  (if (instance? clearley.rules.RuleKernel rule)
    ; work around :key -> nil maps in defrecords
    (if-let [r (:action rule)]
      r
      (fn [& xs] (vec xs)))
    (fn [] rule)))

; Resolves a symbol to a seq of clauses
(defn lookup-symbol [thesym thens theenv]
  (if-let [resolved (ns-resolve thens theenv thesym)]
    (let [resolved @resolved]
      (if (or (vector? resolved) (seq? resolved))
        resolved
        [resolved]))
    (TIAE "Cannot resolve rule for head: " thesym)))

; Rule-ifies the given clause, wrapping it in a one-clause rule if neccesary.
(defn to-rule [clause]
  (if (instance? clearley.rules.RuleKernel clause)
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
    (instance? clearley.rules.RuleKernel clause) [clause]
    (seq? clause) (map to-rule clause)
    (vector? clause) (map to-rule clause)
    true (get grammar clause [])))

; All things this clause might point to, that we must resolve at grammar build time
(defn clause-deps [x grammar]
  (cond (instance? clearley.rules.RuleKernel x) (rule-deps x)
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

; TODO rule attrs/metadata?
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
    (separate-str " " (if (zero? dot)
                        rule-deps
                        (concat (take dot rule-deps) ["*"] (drop dot rule-deps)))))

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
  (wrap-kernel (CfgRule. (vec clauses) 0) name action))
