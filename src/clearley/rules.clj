(ns clearley.rules
  "Back-end stuff for Clearley. Work in progress with unstable API."
  (require clojure.string
           [uncore.throw :as t]
           [uncore.str :as s]))

(defn resolve-clause
  "Resolves a clause to a uniform format, (tag & rules)."
  [clause grammar]
  (if (symbol? clause)
    (cons :or (get grammar clause))
    clause))

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

; Gets a seq of subrules from a clause
#_(defn predict-clause [clause grammar]
  (cond
    (rule? clause) [clause]
    (seq? clause) (map to-rule clause)
    (vector? clause) (map to-rule clause)
    true (get grammar clause [])))

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
