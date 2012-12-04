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
  (import [clearley.rules RuleImpl])
  (use [clearley utils rules]))
; All of the Clearley core library goes here.
; Because I like short files, other stuff is shuffled into various files.

; TODO: empty rule?

; TODO: get rid of this protocol?
(defprotocol ^:private PStrable
  (^:private pstr [obj] "pstr stands for \"pretty-string\".
                        Returns a shorthand str of this item."))

; TODO: make rule similar to defrule, rulefn fn?
(defn rule
  "Creates a rule associated with a parse action that can be called
  after matching. A rule has a required vector of clauses,
  a head (optional, since Rules can also be embedded in other rules),
  and an optional action (the default action bundles the args into a list)."
  ([clauses] (rule nil clauses nil))
  ([clauses action] (rule nil clauses action))
  ([name clauses action]
   (RuleImpl. name (vec clauses) action)))

(defn token
  "Returns a rule that matches a single object (the token)."
  ([a-token value] (rule nil [a-token] (fn [_] value))))

; TODO test in core tests
; TODO presents an argument for some type of Clause protocol.
(defn one-or-more
  "Creates a rule that matches one or more of a subrule. Returns a vector
  of the matches."
  ([subrule]
   (one-or-more (str (rulename subrule) "+") subrule))
  ([name subrule]
   (reify Rule
     (rulename [_] name)
     (clauses [self] [[(rule nil [subrule] vector)
                       (rule nil [self subrule] conj)]])
     (action [_] identity)
     (rule-str [self] (str subrule)))))

; TODO work on this
(defn scanner
  "Defines a rule that scans one token of input with the given scanner function.
  The scanner function is used by the parser to match tokens. If this rule is invoked
  on a token, and the scanner returns logcial true, the rule matches the token."
  ([scanner-fn action]
   (rule nil [{::scanner scanner-fn}] action)))

(defn token-range
  "Creates a rule that accepts all characters within a range. The given min and max
  should be chars."
  ([min max]
   (token-range min max identity))
  ([min max action]
  (if (not (and (char? min) (char? max)))
    (TIAE "min and max should be chars"))
  (let [intmin (int min)
        intmax (int max)]
    (scanner (fn [x]
               (let [intx (int x)]
                 (and (<= intx intmax) (>= intx intmin))))
             action))))

(defn- token-match [token] [token])

(defn- rulehead-clause [clause]
  (cond
    (symbol? clause) (str clause)
    (string? clause) (str \" clause \")
    (keyword? clause) (str clause)
    true "anon"))

(defprotocol ^:private EarleyItem
  (^:private predict [self index])
  (^:private escan [self input-token])
  (^:private is-complete? [self])
  (^:private advance [self]))

(defrecord ^:private REarleyItem [rulehead rule dot index grammar]
  EarleyItem
  (predict [self pos]
    (if (not (is-complete? self))
      (let [clause (get (clauses rule) dot)]
        (map (fn [prediction]
               (REarleyItem. (rulehead-clause clause) prediction 0 pos grammar))
             (predict-clause clause grammar)))))
  (escan [self input-token]
    (cond
      (::scanner (get (clauses rule) dot))
      (if ((::scanner (get (clauses rule) dot)) input-token)
        [(advance self)]
        [])
      (and (not (is-complete? self)) (= (get (clauses rule) dot) input-token))
      [(advance self)]
      true
      []))
  (is-complete? [_]
    (= dot (count (clauses rule))))
  (advance [self]
    (REarleyItem. rulehead rule (inc dot) index grammar))
  PStrable
  (pstr [_]
    ; TODO: stop the below from stack overflowing on self-referential rules
    (separate-str (concat [rulehead "->"]
                          (take dot (clauses rule)) ["*"]
                          (drop dot (clauses rule)) [(str "@" index)])
                  " ")))

(defprotocol ^:private ChartItem
  (^:private cpredict [self pos])
  (^:private cscan [self input-token input])
  (^:private emerge [self other-item]))

; Builds a rule match from the output stack and pushes the match to the top
; (think of a Forth operator reducing the top of a stack)
; Right now, we build the stack as we parse instead of emitting an output stream...
; this may change in the future e.g. to support pull parsing
(defn- reduce-ostack [ostack rule]
  (let [thecount (count (clauses rule))]
    (cons (vec (cons rule (reverse (take thecount ostack)))) (drop thecount ostack))))

; earley-item: duh, rstack: @map<item rstack>, ostack: the output "stream"
; TODO: eliminate need to wrap rstacks in atoms
(defrecord ^:private RChartItem [earley-item rstack ostack]
  ChartItem
  (cpredict [self pos]
    (if (is-complete? earley-item)
      (map (fn [[item rstack]]
             (RChartItem. (advance item) rstack
                          (reduce-ostack ostack (:rule earley-item)))) @rstack)
      (map (fn [item]
             (RChartItem. item (atom {earley-item rstack}) ostack))
           (predict earley-item pos))))
  (cscan [self input-token input]
    (map (fn [item]
           (RChartItem. item rstack (cons [input] ostack)))
         (escan earley-item input-token)))
  ; Merges the stacks of this and the other-item. True if there was anything to merge.
  (emerge [self other-item]
    (swap! rstack #(merge % @(:rstack other-item))))
  PStrable
  (pstr [_] (str (pstr earley-item) " | "
                 (separate-str (map (comp pstr first) @rstack) ", "))))

; creates an initial earley item with dot and pos 0
(defn- chart-item [head-sym rule grammar]
  (RChartItem. (REarleyItem. head-sym rule 0 0 grammar) (atom {}) '()))

; TODO: nuke this protocol, have data object chart
; data object charts can also serve as prototypes of parsing NDFA states
(defprotocol Chart
  (^:private add [self item])
  (^:private cfirst [self])
  (^:private crest [self])
  (^:private reset [self])
  (^:private chart-seq [self]))

(defrecord RChart [chartvec chartmap dot]
  Chart
  (add [self item]
    (let [ikey (:earley-item item)]
      (if-let [previndex (get chartmap ikey)]
        (do (emerge (get chartvec previndex) item)
          self)
        (RChart. (conj chartvec item)
                 (assoc chartmap ikey (count chartvec)) dot))))
  (cfirst [self]
    (if (not (= dot (count chartvec)))
      (get chartvec dot)))
  (crest [self]
    (RChart. chartvec chartmap (inc dot)))
  (reset [self] (RChart. chartvec chartmap 0))
  (chart-seq [self] chartvec)
  PStrable
  (pstr [self]
    (if (= dot (count chartvec))
      (apply str (map #(str (pstr %) "\n") chartvec))
      (apply str (update-in (vec (map #(str (pstr %) "\n") chartvec))
                            [dot] #(str "* " %))))))

(def ^:private new-chart (RChart. [] {} 0))

; scans an input character, seeding a new chart
(defn- scan-chart [chart input-token input]
  (reduce add new-chart (mapcat #(cscan % input-token input)
                                (chart-seq chart))))

; process completions and predictions for a single chart
(defn- parse-chart [chart pos]
  (loop [c chart]
    (if-let [item (cfirst c)]
      (recur (crest (reduce add c (cpredict item pos))))
     c)))

(defn- parse-charts [inputstr grammar tokenizer goal-rule]
  (loop [pos 0
         thestr inputstr
         ; TODO actual goal symbol
         current-chart (add new-chart (chart-item ::goal goal-rule grammar))
         charts []]
    (if-let [thechar (first thestr)]
      (let [thetoken (tokenizer thechar)
            parsed-chart (parse-chart current-chart pos)
            next-chart (scan-chart parsed-chart thetoken thechar)
            next-charts (conj charts parsed-chart)]
        (if (cfirst next-chart)
          (recur (inc pos) (rest thestr) next-chart next-charts)
          ; early termination on failure, incl. failed chart
          (conj next-charts next-chart)))
      ; end step
      (conj charts (parse-chart current-chart (inc pos))))))

; Don't need to expose parser protocol... only 'parse' fn
(defprotocol ^:private Parser
  (parse [parser input] "Parse the given input with the given parser,
                        yielding a match tree (a tree of the form
                        [rule leaves] where leaves is a seq).")
  ; charts is not yet usable by external users
  (charts [parser input]))

; Searches a chart for completed parse of the goal rule, returning all matches
(defn- scan-goal [chart]
  (mapcat :ostack (filter (comp (partial = ::goal) :rulehead :earley-item)
                          (chart-seq chart))))

(defn earley-parser
  "Constructs an Earley parser given a map of rules,
  a goal symbol, and an optional tokenizer."
  ([goal rules]
   (earley-parser goal identity rules))
  ([goal tokenizer rules]
   (let [goal-rule (rule ::goal [goal] identity)]
     (reify Parser
       (parse [parser input]
         ; For now, only retunr first match
         (first (scan-goal (peek (charts parser input)))))
       (charts [parser input]
         (parse-charts input rules tokenizer goal-rule))))))

(defn parse-tree
  "Parses the given input with the given parser, yielding an abstract
  syntax tree with no rules (effectively, the same as stripping the first elements
  (the rules) from a match tree)."
  [parser input]
  (if-let [match (parse parser input)]
    ((fn f [match] ; This fun will recursively reduce the match tree
       (if-let [submatches (seq (rest match))]
         (vec (map f submatches))
         (first match)))
       match)))

(defn print-charts
  "For a given parser and input, prints a multi-line representation of its charts to
  *out*. The representation might change in the future. For more about
  parse charts, see http://www.wikipedia.org/wiki/Earley_parser. Primarily
  useful for debugging."
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
          ; Below is the default action--return args if not empty, otherwise return rule
          action (action rule)]
      (try
        (apply action subactions)
        (catch clojure.lang.ArityException e
          (throw (RuntimeException. (str "Wrong # of params taking action for rule "
                                         (if (rulename (first match))
                                           (rulename (first match))
                                           (first match)) ", "
                                         "was given " (count subactions))
                                    e)))))))

(defn execute
  "Parses some input and executes the parse actions."
  [parser input]
  (take-action (parse parser input)))

; Defrule begins here.
; TODO: experiment with using a parser for defrule
; instead of all these macro helpers... would make a convincing POC for earley parsing!

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
  ; TODO: qualify syms?
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
  (resolve-all-clauses goal thens {}))

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
  ; TODO: test
  [goal tokenizer thens]
  (earley-parser goal tokenizer (build-grammar-with-ns goal thens)))

; TODO execute fn?
