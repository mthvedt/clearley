(ns clearley.core
  "Tools for general context-free grammar parsing and syntax tree processing.
  The goal of Clearley is to make parsing as easy as any other programming task.
  Emphasis is on completeness, modularity, and ease of use.
  Can be used in a vanilla functional style or the provided macro language.
  
  Please be sure to see the docs for a high-level overview.
  Clearley docs assume familiarity with the library's high level concepts,
  for succintness."
  (require [clojure string]
           [clojure.pprint])
  (use [clearley utils rules earley]))
; All of the Clearley core library goes here.
; Because I like short files, other stuff is shuffled into various files.

(defn rule
  "Creates a rule associated with a parse action that can be called
  after matching. A rule has a required vector of clauses,
  a head (optional, since Rules can also be embedded in other rules),
  and an optional action (the default action bundles the args into a list)."
  ([clauses] (rule nil clauses nil))
  ([clauses action] (rule nil clauses action))
  ([name clauses action] (context-free-rule name clauses action)))

(defn token
  "Returns a rule that matches a single given token.
  Its action returns the given value."
  ([a-token value] (rule nil [a-token] (fn [_] value))))

(defrecord OneOrMoreImpl [subrule dot]
  RuleKernel
  (predict [self] [(rule nil [subrule] vector)
                   (rule nil [self subrule] conj)])
  (rule-deps [self] [subrule])
  (scan [_ _] [])
  (is-complete? [_] (= dot 1))
  (advance [self] (assoc self :dot 1))
  (rule-str [_] (if (not (zero? dot))
                  (str (clause-str subrule) " *")
                  subrule)))

(defn one-or-more
  "Creates a rule that matches one or more of a subrule. Returns a vector
  of the matches."
  ([subrule]
   (one-or-more (str (rule-name subrule) "+") subrule))
  ([name subrule]
   (merge (OneOrMoreImpl. subrule 0) {:name name, :action identity})))

(defrecord Scanner [rulefn dot]
  RuleKernel
  (rule-deps [_] [])
  (predict [self] [])
  (scan [self input-token]
    (if (and (not (is-complete? self)) (rulefn input-token))
      [(advance self)]
      []))
  (is-complete? [_] (= dot 1))
  (advance [self] (assoc self :dot 1))
  (rule-str [_] (if (zero? dot)
                  (clause-str rulefn)
                  (str (clause-str rulefn) " *"))))

(defn scanner
  "Creates a rule that accepts input tokens. For a token t, if (scanner-fn t)
  is logical true, this rule matches that token."
  ([scanner-fn action]
   (wrap-kernel (Scanner. scanner-fn 0) nil action)))

(defn char-range
  "Creates a rule that accepts any one character within a given range
  given by min and max, inclusive. min and max should be chars."
  ([min max]
   (char-range min max identity))
  ([min max action]
  (if (not (and (char? min) (char? max)))
    (TIAE "min and max should be chars"))
  (let [intmin (int min)
        intmax (int max)]
    (scanner (fn [x]
               (let [intx (int x)]
                 (and (<= intx intmax) (>= intx intmin))))
             action))))

; Don't need to expose parser protocol... only 'parse' fn
(defprotocol ^:private Parser
  (parse [parser input] "Parse the given input with the given parser,
                        yielding a match tree (a tree of the form
                        [rule leaves] where leaves is a seq).")
  ; charts is not yet usable by external users
  (^:private charts [parser input]))

(defn earley-parser
  "Constructs an Earley parser given a map of rules,
  a goal symbol, and an optional tokenizer."
  ([goal rules]
   (earley-parser goal identity rules))
  ([goal tokenizer rules]
   (reify Parser
     (parse [parser input]
       ; For now, only return first match
       (first (scan-goal (peek (charts parser input)))))
     (charts [parser input]
       (parse-charts input rules tokenizer goal)))))

(defn parse-tree
  "Parses the given input with the given parser, yielding an abstract
  syntax tree with no rule nodes."
  [parser input]
  (if-let [match (parse parser input)]
    ((fn f [match] ; This fn will recursively reduce the match tree
       (if-let [submatches (seq (rest match))]
         (vec (map f submatches))
         (first match)))
       match)))

(defn print-charts
  "For a given parser and input, prints a multi-line representation of its charts to
  *out*. The representation might change in the future. For more about
  parse charts, see http://www.wikipedia.org/wiki/Earley_parser. Useful for debugging."
  [parser input]
  (dorun (for [chart (charts parser input)]
           (println (pstr chart)))))

(defn take-action
  "Executes the parse actions for a parser match."
  [match]
  (if (nil? match)
    (throw (RuntimeException. "Failure to parse"))
    (let [subactions (map take-action (rest match))
          rule (first match)
          action (rule-action rule)]
      (try
        (apply action subactions)
        (catch clojure.lang.ArityException e
          (throw (RuntimeException. (str "Wrong # of params taking action for rule "
                                         (rule-str rule) ", "
                                         "was given " (count subactions))
                                    e)))))))

(defn execute
  "Parses some input and executes the parse actions."
  [parser input]
  (take-action (parse parser input)))

; Defrule begins here.
; --In the future, might be able to replace all this nonsense with a parser.

; Macro helper fn. Dequalifies a stringable qualified sym or keyword
(defn- dequalify [strable]
  (let [dequalified (clojure.string/split (str strable) #"/")]
    (symbol (nth dequalified (dec (count dequalified))))))

; Macro helper fn for def rule. Returns a pair of
; [appropriate-symbol-for-action-body, rule-or-rulename]
(defn- process-nonlist-clause [clause]
  (cond (list? clause) (assert false)
        (symbol? clause) [(dequalify clause) `'~clause]
        (keyword? clause) [(dequalify clause) clause]
        (= java.lang.String (type clause)) [(symbol (str clause)) clause]
        true ['_ clause])) ; can't be an arg in a fn

; Macro helper fn for def rule. Returns a pair of
; [appropriate-symbol-for-action-body, rule-or-rulename-or-uneval'd-form]
(defn- process-clause [clause]
  (if (list? clause)
    (if-let [thename (first clause)]
      (let [therule (second clause)]
        [thename (if (list? therule) ; A form to evaluate
                   ; Return it; will be eval'd when defrule called
                   therule
                   ; See what process-nonlist-clause has to say
                   (second (process-nonlist-clause therule)))])
      (TIAE "Not a valid subrule: " clause))
    (process-nonlist-clause clause)))

; Macro helper fn. Builds the `(rule ...) bodies for defrule.
; Head: a symbol. impls: seq of (bindings bodies+) forms.
(defn- build-defrule-rule-bodies [head impls]
  (vec (map (fn [impl]
              (let [clauses (first impl)]
                (if (seq clauses)
                  (let [processed-clauses (map process-clause clauses)]
                    `(rule '~head [~@(map second processed-clauses)]
                           (fn [~@(map first processed-clauses)] ~@(rest impl))))
                  (TIAE "Rule clauses must be seqable"))))
            impls)))

; Macro helper fn. Builds the body for defrule and related macros.
; Head: a symbol. impl-or-impls: (clauses bodies+) or ((clauses bodies+)+).
(defn- build-defrule-bodies [head impl-or-impls]
  (let [first-form (first impl-or-impls)]
    (cond (or (vector? first-form) (string? first-form))
          (build-defrule-rule-bodies head [(apply list first-form
                                                  (rest impl-or-impls))])
          (seq? first-form) (build-defrule-rule-bodies head impl-or-impls)
          true (TIAE "Not a valid defrule; "
                     "expected clause vector, string, or clause-body pairs"))))

(defmacro defrule
  "Defs a parser rule or seq of parser rules. See the docs for
  the full defrule syntax.
  
  Usage:
  (defrule rule-name [clauses] action?)
  (defrule (rule-name [clauses] action?)+)

  Valid clauses:
  a rule
  a symbol pointing to a seq of rules
  [rule or rule symbol+]
  (rule-alias-symbol rule-symbol)
  (rule-alias-symbol [rule or rule symbol+])
  
  Defines one or more rules and binds them (possibly in a seq) to a given var.
  The optional action form defines a parse action, where the symbols will be bound
  to the results of the actions of the correspoinding subrules. For example:

  (defrule sum [num \\+ (num2 num)] (+ num num2))
  
  The above rule matches two nums (one of which is aliased as num2)
  and adds them together. If a parse action is not provided, a default
  will be used which bundles its args into a list. The rule's head is
  'sum and will be bound to *ns*/sum."
  ; TODO qualify syms?
  [head & impl-or-impls]
  `(def ~head ~(build-defrule-bodies head impl-or-impls)))

(defmacro extend-rule
  "Like defrule, but for an existing symbol with some rules bound to it."
  [head & impl-or-impls]
  `(def ~head (vec (concat ~head ~(build-defrule-bodies head impl-or-impls)))))

(defmacro add-rules
  "Adds some amount of additional rule objects to a symbol with rules bound to it."
  [head & rules]
  `(def ~head (vec (concat ~head [~@rules]))))

; In the future, we might bind &env to theenv
; The form of &env is not fixed by Clojure authors so don't do it now
(defn build-grammar-with-ns
  "Builds a grammar in the given ns from the given goal rule."
  [goal thens]
  (resolve-all-rule-deps goal thens {}))

; TODO test
(defmacro build-grammar
  "Builds a grammar in the current ns from the given goal rule.
  A grammar is implemented as a seq of rules."
  [goal]
  `(build-grammar-with-ns '~goal {} *ns* {}))

(defmacro build-parser
  "Build a parser in the current ns from the given goal rule and an
  optional tokenizer."
  ([goal]
   `(build-parser ~goal identity))
  ([goal tokenizer]
   `(build-parser-with-ns '~goal ~tokenizer *ns*)))

(defn build-parser-with-ns
  "Build a parser in a given ns from the given goal rule and tokenizer."
  [goal tokenizer thens]
  (earley-parser goal tokenizer (build-grammar-with-ns goal thens)))
