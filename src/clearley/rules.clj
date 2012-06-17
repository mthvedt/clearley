(ns clearley.rules)

(defprotocol Rule
  (head [self])
  (clauses [self])
  (action [self])
  (rule-str [self]))

(extend-protocol Rule
  Object
  (head [self] nil)
  (clauses [self] [self])
  (action [self] (fn [& _] self))
  (rule-str [self] (str self))) ; TODO: rule-str?

; TODO: eliminiate need for this--head should only be used when building grammars
(defn rehead-rule [head rule]
  (reify Rule
    (head [_] head)
    (clauses [_] (clauses rule))
    (action [_] (action rule))
    (rule-str [_] (rule-str rule))))
