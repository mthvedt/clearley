(ns clearley.rules
  "Back-end stuff for Clearley. Work in progress with unstable API."
  (use clearley.utils))

; ===
; Stuff about Rules
; ===

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
  (rule-str [self] (str self))) ; TODO work on this

(defrecord RuleImpl [name clauses action]
  Rule
  (rulename [_] name)
  (clauses [_] clauses)
  (action [_] action)
  (rule-str [_]
    (separate-str clauses " ")))

; ===
; Stuff about rule clauses
; ===

; Resolves a symbol to a seq of clauses.
(defn lookup-symbol [thesym thens theenv]
  (if-let [resolved (ns-resolve thens theenv thesym)]
    (let [resolved @resolved]
      (if (or (vector? resolved) (seq? resolved))
        resolved
        [resolved]))
    (TIAE "Cannot resolve rule for head: " thesym)))

; Rule-ifies the given clause, wrapping it in a one-clause rule if neccesary.
(defn to-rule [clause]
  (if (instance? clearley.rules.Rule clause)
    clause
    (RuleImpl. (str "Anon@" (hash clause)) [clause] identity)))

; Processes the clause re. a grammar. If clause is a symbol,
; looks up the symbol, maps to-rule to the result, and adds it to the grammar.
(defn update-grammar [grammar clause thens theenv]
  (if (symbol? clause)
    (assoc grammar clause (map to-rule (lookup-symbol clause thens theenv)))
    grammar))

; Gets a seq of subrules from a clause
(defn predict-clause [clause grammar]
  (cond
    (instance? clearley.rules.Rule clause) [clause]
    (seq? clause) (map to-rule clause)
    (vector? clause) (map to-rule clause)
    true (get grammar clause [])))

; All things this clause might point to, that we must resolve at grammar build time
(defn clause-deps [x grammar]
  (cond (instance? clearley.rules.Rule x) (clauses x)
        true (predict-clause x grammar)))

; ===
; Here be dragons
; ===

(defn resolve-all-clauses [goal thens theenv]
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
