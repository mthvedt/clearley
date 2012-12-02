(ns clearley.rules
  (use clearley.utils))

(defprotocol Rule
  (rulename [self])
  (clauses [self])
  (action [self])
  (rule-str [self]))

(extend-protocol Rule
  Object
  (rulename [self] nil)
  (clauses [self] [self])
  (action [self] (fn [& _] self))
  (rule-str [self] (str self))) ; TODO: rule-str?

; TODO: eliminiate need for this--head should only be used when building grammars
(defn rename-rule [new-name rule]
  (reify Rule
    (rulename [_] new-name)
    (clauses [_] (clauses rule))
    (action [_] (action rule))
    (rule-str [_] (rule-str rule))))

(defmulti rule-builder)

(defn request-for-symbol [sym])

; (defn request-for-key [thekey])

; TODO s/rule/protorule and s/earleyrule/rule?
#_(defprotocol Protorule
  (requires [self]) ; symbols required ot be in the grammar
  (to-rule [self])) ; Rule for now... soon EarleyRule

; A clause is something which, given a grammar, yields a category of subrules.
; Fundemental part of CFGs.
#_(defrecord Clause
  (clause-name [self])
  (clause-dependencies [self])
  (clause-predictions [self]))

(defn lookup-symbol [thesym thens theenv]
  ; TODO return a clause
  (if-let [resolved (ns-resolve thens theenv thesym)]
    (let [resolved @resolved]
      (if (or (vector? resolved) (seq? resolved))
        resolved
        [resolved]))
    (TIAE "Cannot resolve rule for head: " thesym)))

;(defn predict-clause-1 [clause grammar]
;  (cond
;    (vector? clause) clause
;    (seq? clause) clause
;    (

(defn resolve-all-clauses [goal thens theenv]
  (loop [stack [goal] ; stack: clauses to resolve
         breadcrumbs #{}
         grammar {}] ; grammar: maps keyword clauses to rules
    (if-let [current-clause (first stack)]
      (if (contains? breadcrumbs current-clause)
        ; have we already seen this? skip it entirely
        (recur (rest stack) breadcrumbs grammar)
        (let [predictions (if (or (vector? current-clause) (seq? current-clause))
                            current-clause
                            (clauses current-clause))
              predictions (if (symbol? current-clause)
                            ; lookup rule if it is a symbol
                            (lookup-symbol current-clause thens theenv)
                            predictions)
              rulename (if (symbol? current-clause)
                         current-clause
                         nil)]
          (recur (concat predictions (rest stack))
                 (conj breadcrumbs current-clause)
                 (if rulename
                   (assoc grammar current-clause
                          (map #(rename-rule current-clause %) predictions))
                   grammar))))
      grammar)))
