; Standard library, included in clearley.defrule
; TODO include defrule in core

(def ^{:doc "The empty rule. Returns nil."}
  empty-rule {:name "empty" :tag :seq :value [] :action (fn [] nil)})

; TODO include default actions?
; TODO arguments-checking
(defrulefn scanner
  "Creates a rule that accepts input tokens. For a token t, if (scanner-fn t)
  is logical true, this rule matches that token.
  The default action returns the token."
  scanner-fn identity
  {:name name, :tag :scanner, :value [scanner-fn], :action action})

(defrulefn token
  "Creates a rule that matches a token. The default action returns the token."
  token (fn [] token)
  {name name, :tag :token, :value [token], :action action})

(defrulefn symbol-rule
  "Creates a rule that points to some other rule, identified by the given symbol.
  The default action is the identity."
  a-symbol identity
  {name name, :tag :symbol, :value [a-symbol], :action action})

(defrulefn or-rule
  "Creates a rule that matches one of some number of given rules. The default
  action is the identity."
  rules identity
  {name name, :tag :or, :value (vec rules), :action action})

(defn plus
  "Creates a rule that matches one or more of some subrule.
  The defalt action returns a seq of the args."
  ([a-rule] (plus a-rule (fn [& args] args)))
  ([a-rule action]
   (rule [a-rule [:star a-rule]] (fn [f r] (apply action f r)))))

(defn opt
  "Creates a rule that matches a subrule, or nothing. The action will be passed
  the subrule's value, or nil. The default action is the identity."
  ([a-rule] (opt a-rule identity))
  ([a-rule action]
   {:tag :or :value [a-rule empty-rule] :action action}))

(defn char-range
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

(defn string-rule
  "Creates a rule that matches some string. The default action returns the string."
  ([str] (string-rule str (fn [] str)))
  ([str action]
   {:tag :seq, :action action, :value (vec str)}))

(defn separate-rule [a-rule separator-rule action]
  "Creates a rule that matches 1 or more of a given subrule, separated
  by the given separator rule which is invisible to the parse action."
  (let [subrule (rule [separator-rule a-rule] (fn [_ x] x))]
    (rule [a-rule `(:star ~subrule)] (fn [a b] (apply action a b)))))
