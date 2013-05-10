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
  and the action should have 1 or 2 args, and is applied a la core/reduce."
  [name subrule action]
  `(let [action# ~action]
     (def ~name (or-rule [(rule [~subrule] action#)
                          (rule ['~name ~subrule] action#)]))))

(defmacro defstar
  "Creates a rule that matches zero or more of a given rule. Because
  CFGs don't support star rules directly, actually defs a pair of mutually recursive
  rules (that's why it needs to be a macro). The rule is left-recursive
  and the action should have 0, 1, or 2 args, and is applied a la core/reduce."
  [name subrule action]
  (let [name-plus (symbol (str name "-plus"))]
    `(do
       (let [action# ~action]
         (def ~name-plus (or-rule [(rule [~subrule] action#)
                                     (rule ['~name ~subrule] action#)]))
           (def ~name (or-rule [(rule [] action#) ~name-plus]))))))

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
  `(let [action# ~action]
     (def ~name (or-rule [(rule [~a-rule] action#)
                          (rule ['~name ~delimiter ~a-rule]
                                (fn [x# _# y#] (action# x# y#)))]))))

(defn char-to-num
  "Maps chars to numbers linearally. Default maps \\0 to 0.
  This behavior can be overriden by providing char-start and num-start,
  such that char-start -> numstart (for example \\a -> 10)."
  ([char] (char-to-num char \0 0))
  ([char char-start num-start]
   (let [char-start-int (int char-start)]
     (+ num-start (- (int char) char-start-int)))))

(def ^{:doc "A rule that matches an input digit, and returns a number for it."}
  digit (char-range \0 \9 char-to-num))

(def ^{:doc "Like digit but matches 1 through 9."}
  digit1-9 (char-range \1 \9 char-to-num))

(def ^{:doc "Matches \\0 and returns 0."}
  zero (rule [\0] 0))

(def ^{:doc "A rule that matches an input hex digit, case insensitive, and returns
            a number for it."}
  hex-digit
  `(:or digit
        ~(char-range \a \f #(char-to-num % \a 10))
        ~(char-range \A \F #(char-to-num % \A 10))))

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
          ([l x] (doto l (.add x)))))

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
