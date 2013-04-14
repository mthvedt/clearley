(ns clearley.rules
  "Back-end stuff for Clearley. Work in progress with unstable API."
  (require clojure.string
           [uncore.throw :as t]
           [uncore.str :as s])
  (use uncore.core))

; TODO maybe we can nuke this entirely; move it into GLR

(declare predict-clause)

; TODO merge into core?
(defrecord Match [rule submatches])

(defn match [rule submatches]
  (Match. rule submatches))

; predict: predicts a single clause that must be matched
; scanner: returns a fn that accepts input, or nil
; is-complete?: is this rule fully matched
; advance: called when a predicted clause is matched
; rule-str: a short useful string representation of this rule
;
; Our problems are:
; * Define self-referential rule builders.
; * Allow custom rules.
; * Yet keep things simple.
; * An almost fanatical devotion to the pope, and nice red uniforms.

(def rule-action nil) ; TODO

(defprotocol CfgRule
  (predict [self])
  (scanner [self]) ; Returns a fn that accepts/rejects input token
  (is-complete? [self])
  (advance [self])
  (rule-name [self])
  (rule-str [self]))

; TODO move grammar to glr?
; TODO: rename name to head?
; TODO: rule names?
;
; TODO !!!!! make this even simpler--remove special treatment
; for symbol lookups?
(defrecord BasicCfgRule [name clauses dot grammar]
  CfgRule
  (predict [self]
    (if (is-complete? self)
      []
      (predict-clause (str name "." dot) (get clauses dot) grammar)))
  (scanner [self] ; TODO change?
    (when-not (is-complete? self)
      #(= (get clauses dot) %)))
  (is-complete? [_]
    (= dot (count clauses)))
  (advance [self] (assoc self :dot (inc dot)))
  (rule-name [_] name)
  (rule-str [self]
    (let [clause-strs (map pr-str clauses)]
      (s/separate-str " " (concat [(rule-name self) "->"]
                                  (take dot clause-strs)
                                  (if (zero? dot) [] ["*"])
                                  (drop dot clause-strs))))))

(defn basic-cfg-rule [name rules grammar]
  (BasicCfgRule. name rules 0 grammar))

(defn goal-rule [sym grammar]
  (BasicCfgRule. ::goal [sym] 0 grammar))

; Turns a clause into some seq of anonymous rules
(defn predict-tagged-clause [potential-name tagged-clause grammar]
  (let [tag (first tagged-clause)
        clauses (rest tagged-clause)]
  (case tag
    :and [(basic-cfg-rule potential-name clauses grammar)]
    :or (map #(predict-clause potential-name % grammar) clauses))))

(defn defrule-to-cfgrule [head-sym {:keys [name value action] :as rule} grammar]
  (if (= (first value) :seq)
    (BasicCfgRule. head-sym (vec (rest value)) 0 grammar)
    (BasicCfgRule. head-sym [value] 0 grammar)))

(defn predict-symbol [sym grammar]
  (map #(defrule-to-cfgrule sym % grammar) (get grammar sym)))

; Clause -> seq of rules
(defn predict-clause [potential-name clause grammar]
  (cond (symbol? clause) (predict-symbol clause grammar)
        (list? clause) (predict-tagged-clause potential-name clause grammar)
        true []))
