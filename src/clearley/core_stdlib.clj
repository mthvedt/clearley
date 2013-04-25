; Standard library, included in clearley.core

(def ^{:doc "The empty rule. Returns nil."}
  EMPTY {:value '(:seq) :action (fn [] nil)})

(defn plus
  "Creates a rule that matches one or more of some subrule.
  The defalt action returns a seq of the args."
  ([a-rule] (plus a-rule (fn [& args] args)))
  ([a-rule action]
   (rule [a-rule `(:star ~a-rule)] (fn [f r] (apply action f r)))))

(defn opt
  "Creates a rule that matches a subrule, or nothing. The action will be passed
  the subrule's value, or nil. The default action is the identity."
  ([a-rule] (opt a-rule identity))
  ([a-rule action]
   {:value `(:or ~a-rule EMPTY) :action action}))

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
   {:action action, :value `(:seq ~@str)}))
