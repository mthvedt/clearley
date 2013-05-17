(ns clearley.lib
  "More fns and macros for context-free grammars."
  (require [uncore.throw :as t])
  (use clearley.match))

(defrulefn token
  "Creates a rule that matches a token. The default action returns the token."
  a-token (fn [_] a-token)
  {:name name, :tag :token, :value [a-token], :action action})

(defrulefn symbol-rule
  "Creates a rule that points to some other rule, identified by the given symbol.
  The default action is the identity."
  a-symbol identity
  {:name name, :tag :symbol, :value [a-symbol], :action action})

(defrulefn or-rule
  "Creates a rule that matches one of some number of given rules. The default
  action is the identity."
  rules identity
  {:name name, :tag :or, :value (vec rules), :action action})

#_(defrulefn star
    "Creates a rule that matches a subrule zero or more times.
    The default action returns a seq of the matches."
    subrule identity
    {:name name, :tag :star, :value [subrule], :action action})

#_(defn plus
    "Creates a rule that matches a subrule one or more times.
    The defalt action returns a seq of the matches"
    ([subrule] (plus subrule (fn [& args] args)))
    ([subrule action]
     (rule [subrule [:star subrule]] (fn [f r] (apply action f r)))))

(defmacro defplus
  "Creates a rule that matches one or more of a given rule. Because
  CFGs don't support star rules directly, actually defs a recursive
  rule (that's why it needs to be a macro, you can't make self-referential plain
  old data structures). The rule is left-recursive
  and the action should have 1 or 2 args, the 1-arity for the non-recursive case,
  the 2-arity for the left-recursive case applied a la core/reduce.
  
  Example:
  (defplus add-a-bunch-of-numbers 'number
    (fn ([num] (Integer/parseInt num))
        ([num1 num2] (+ num1 (Integer/parseInt num2)))))"

  [name subrule action]
  `(let [action# ~action]
     (def ~name (or-rule [(rule [~subrule] action#)
                          (rule ['~name ~subrule] action#)]))))

(defmacro defstar
  "Creates a rule that matches zero or more of a given rule. Because
  CFGs don't support star rules directly, actually defs a pair of mutually recursive
  rules (that's why it needs to be a macro)
  and the action should have 0, 1, or 2 args. Arities 0 and 1 are for the non-recursive
  case, arity 2 for the left-recursive case as in core/reduce.
  
  Example:
  (defplus make-array 'array-value
    (fn ([] [])
        ([val] [val])
        ([arr val] (conj arr val))))"
  [name subrule action]
  (let [name-plus (symbol (str name "-plus"))]
    `(do
       (let [action# ~action]
         (def ~name-plus (or-rule [~subrule
                                     (rule ['~name-plus ~subrule] action#)]))
           (def ~name (or-rule [(rule [] action#) ~name-plus]))))))

(defmacro def-unrolled
  "Helper for def-unrolled-star and def-unrolled-plus."
  [name subrule action reducer-action zero?]
  (let [name-1 (symbol (str name "-1"))
        name-2 (symbol (str name "-2"))
        name-3 (symbol (str name "-3"))
        name-4 (symbol (str name "-4"))
        name-plus (symbol (str name "-plus"))
        action-sym (gensym "action")]
    `(do
       (let [~action-sym ~action
             reducer# ~reducer-action]
         (def ~name-1 (rule [~subrule] ~action-sym))
         (def ~name-2 (rule [~subrule ~subrule] ~action-sym))
         (def ~name-3 (rule [~subrule ~subrule ~subrule] ~action-sym))
         ;(def ~name-4 (rule [~subrule ~subrule ~subrule ~subrule] action#))
         (def ~name-plus (or-rule ['~name-3
                                   (rule ['~name-plus ~subrule] reducer#)
                                   (rule ['~name-plus ~subrule ~subrule] reducer#)
                                   (rule ['~name-plus ~subrule ~subrule ~subrule]
                                         reducer#)]))
         (def ~name (or-rule [~@(if zero? `((rule [] ~action-sym)) ())
                             '~name-1 '~name-2 '~name-3 '~name-plus]))))))

(defmacro def-unrolled-star
  "Like defstar, but unrolls the rule a few times.
  Please only use this if you need the speed. Unrolls up to 3 times.
  You should supply two actions: one for the base case, with arities 0, 1, 2, or 3;
  and one with for the reducing case, with arities 2, 3, or 4. For the latter,
  the rule is left-recursive, so the first arg will be the recursive case.

  This is primarily to make the parser faster, so feel free to use rest args
  unless you care about that last few % of performance."
  [name subrule action reducer-action]
  `(def-unrolled ~name ~subrule ~action ~reducer-action true))

(defmacro def-unrolled-plus
  "Like def-unrolled-star but does not match the empty rule."
  [name subrule action reducer-action]
  `(def-unrolled ~name ~subrule ~action ~reducer-action false))

(defn opt
  "Creates a rule that matches a subrule, or nothing. The action will be passed
  the subrule's value, or a default value (nil if unspecified).
  The default action is the identity."
  ([subrule] (opt subrule identity))
  ([subrule action] (opt nil subrule identity))
  ([name subrule action]
   {:name name, :tag :or :value [subrule empty-rule] :action action})
  ([name subrule action default]
   {:name name, :tag :or :value [subrule (assoc empty-rule :action (fn [] default))],
    :action action}))

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

(defmacro defdelimit
  "Creates a rule that matches 1 or more of a given subrule, separated
  by the given delimiter which is invisible to the parse action.
  Trailing delimiters don't get matched. The parse action should take
  1 or 2 arguments, as with defplus."
  [name a-rule delimiter action]
  `(let [delimited_action# ~action]
     (def ~name (or-rule [(rule [~a-rule] delimited_action#)
                          (rule ['~name ~delimiter ~a-rule]
                                (fn [a# ~'_ b#] (delimited_action# a# b#)))]))))

(defn char-to-num
  "Returns a fn that maps chars to numbers linearally. Default maps \\0 to 0.
  This behavior can be overriden by providing char-start and num-start,
  such that char-start -> numstart (for example \\a -> 10)."
  ([] (char-to-num \0 0))
  ([char-start num-start]
   (let [char-start (long char-start)
         num-start (long num-start)]
     (binding [*unchecked-math* true]
       (fn [^long c] (+ num-start (- c char-start)))))))

(def ^{:doc "A rule that matches an input digit, and returns a number for it."}
  digit (char-range \0 \9 (char-to-num)))

(def ^{:doc "Like digit but matches 1 through 9."}
  digit1-9 (char-range \1 \9 (char-to-num)))

(def ^{:doc "Matches \\0 and returns 0."}
  zero (rule [\0] 0))

(def ^{:doc "A rule that matches an input hex digit, case insensitive, and returns
            a number for it."}
  hex-digit
  `(:or digit
        ~(char-range \a \f (char-to-num \a 10))
        ~(char-range \A \F (char-to-num \A 10))))

#_(defn make-num
  "Turns a bunch of digits into a number.
  An optional radix can be supplied (default 10).
  Useful for plugging into parse actions."
  ([digits] (make-num digits 10))
  ([digits radix]
   (reduce #(+ (* radix %) %2) 0 digits)))

(defn num-reducer
  ; TODO better doc
  ; TODO codox linking source?
  "Creates a (0,1,2)-ary num reducer useful in plus/star rules."
  ([] (num-reducer 10))
  ([radix] (fn ([] 0)
             ([^long x] x)
             ([^long x ^long y] (+ (* x radix) y)))))

(defplus
  ^{:doc "Matches a bunch of digits. Returns an ArrayList of digits. The usual
         Java collections caveates apply..."}
  digits-ar
  digit (fn ([x] (doto (java.util.ArrayList.) (.add x)))
          ([^java.util.ArrayList l x] (doto l (.add x)))))

(defplus
  ^{:doc "Matches a positive number, returning the number. Any number of leading
         zeroes is allowed."}
  natnum digit (num-reducer))

(defmatch ^{:doc "Matches a positive number with no leading zeroes,
                 returning the number. (0 is a matching number.)"}
  canonical-natnum
  ([\0] 0)
  digit1-9
  ([digit1-9 digits-ar] (reduce (num-reducer) digit1-9 digits-ar)))

(defn surround 
  "Surrounds a rule with one or two other rules.
  Returns the result of the center rule.
  Useful for warpping in quotes, braces, brackets, &c."
  ([r a-rule]
    (surround r a-rule r))
  ([r1 a-rule r2]
   (surround r1 a-rule r2 identity))
  ([r1 a-rule r2 action]
   (surround nil r1 a-rule r2 action))
  ([name r1 a-rule r2 action]
   (rule name [r1 a-rule r2] (fn [_ x _] (action x)))))

(defmacro defsurrounder [sym in-what open close]
  `(defrulefn ~sym
     ~(str "Surround a rule in " in-what ".")
     ~'a-rule identity
     (surround ~'name ~open ~'a-rule ~close ~'action)))

(defsurrounder quotes "quotes" \" \")
(defsurrounder brackets "brackets: []" \[ \])
(defsurrounder braces "braces: {}" \{ \})
(defsurrounder parens "parens: ()" \( \))
(defsurrounder angle-brackets "angle brackets: <>" \< \>)
(defsurrounder single-quotes "single quotes" \' \')
