(ns clearley.core
  "A generalized context-free grammar parser. It will
  accept any seq of inputs, not just text, and parse any context-free grammar.
  Emphasis is on ease of use, versatility, and dynamic/exploratory programming."
  (require [clojure string]
           [clojure.pprint :as pp])
  (use [clearley utils rules]))
; TODO: empty rule?

; TODO: get rid of this protocol?
(defprotocol ^:private PStrable
  (^:private pstr [obj] "pstr stands for \"pretty-string\".
                        Returns a shorthand str of this item."))

(defrecord ^:private RuleImpl [head clauses action]
  Rule
  (head [_] head)
  (clauses [_] clauses)
  (action [_] action)
  (rule-str [_]
    (separate-str clauses " ")))

#_(defmethod clojure.core/print-method clearley.core.RuleImpl [rule writer]
  (.write writer (rule-str rule)))

(defmethod clojure.pprint/simple-dispatch clearley.rules.Rule [rule]
  (clojure.pprint/write-out (rule-str rule)))

(prefer-method clojure.pprint/simple-dispatch clearley.rules.Rule clojure.lang.IPersistentMap)

; TODO: rule macro, rulefn fn
(defn rule
  "Creates a rule associated with a parse action that can be called
  after matching. A rule has a required vector of clauses,
  an head (optional, since Rules can also be embedded in other rules),
  and an optional action (the default action bundles the args into a list).
  A clause can be a rule, a symbol referring to one or more rules,
  an atom containing a rule,
  or a vector or seq of one or more rules (anonymous rules)."
  ; TODO: clauses action, not head clauses
  ([clauses] (rule nil clauses nil))
  ([head clauses] (rule head clauses nil))
  ([head clauses action]
   (RuleImpl. head (vec clauses) action)))

(defn token
  "Returns a rule that matches a single object (the token). Its action by default
  returns the token but can also return some specified value."
  ([a-token] (rule nil [a-token] (fn [_] a-token)))
  ([a-token value] (rule nil [a-token] (fn [_] value))))

; TODO test in core tests
(defn one-or-more
  "Creates a rule that matches one or more of a subrule. Returns a vector
  of the matches."
  ([subrule]
   (one-or-more (str (head subrule) "+") subrule))
  ([head subrule]
   (reify Rule
     (head [_] head)
     (clauses [self] [[(rule nil [subrule] vector)
                       (rule nil [self subrule] conj)]])
     (action [_] identity)
     (rule-str [self] (str subrule)))))

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

; A grammar maps rule heads to rules. nil never maps to anything.
(defn- grammar [rules]
  (dissoc (group-by head rules) nil))

(defn- token-match [token] [token])

; Gets a seq of subrules from a clause
(defn- predict-clause [clause grammar]
  (cond
    (sequential? clause) clause
    ; This is just hideous, we can't even use satisfies?...
    ; Clearley needs a unified clause model, stat
    (instance? clearley.rules.Rule clause) [clause]
    ;(instance? clearley.core.RuleImpl clause) [clause]
    true (get grammar clause [])))

(defprotocol ^:private EarleyItem
  (^:private predict [self index])
  (^:private escan [self input-token])
  (^:private is-complete? [self])
  (^:private advance [self]))

(defrecord ^:private REarleyItem [rule dot index grammar]
  EarleyItem
  (predict [self pos]
    (if (not (is-complete? self))
      (map (fn [prediction]
             (REarleyItem. prediction 0 pos grammar))
           (predict-clause (get (clauses rule) dot) grammar))))
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
    (REarleyItem. rule (inc dot) index grammar))
  PStrable
  (pstr [_]
    ; TODO: stop the below from stack overflowing on self-referential rules
    (separate-str (concat [(head rule) "->"]
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
(defn- chart-item [rule grammar]
  (RChartItem. (REarleyItem. rule 0 0 grammar) (atom {}) '()))

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
         current-chart (add new-chart (chart-item goal-rule grammar))
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
(defn scan-goal [chart]
  (mapcat :ostack (filter (comp (partial = ::goal) head :rule :earley-item)
                          (chart-seq chart))))

(defn earley-parser
  "Constructs an Earley parser, provided with a seq of rules and a predefined
  goal symbol. The parser will attempt to match the given input to the goal symbol,
  given the rules provided. The optional tokenizer can be used to map inputs
  to the terminal rules of your grammar (the parse tree will contiain inputs
  as its leaves, not the terminal symbols)."
  ([goal rules]
   (earley-parser goal identity rules))
  ([goal tokenizer rules]
   (let [goal-rule (rule ::goal [goal] identity)
         grammar (grammar rules)]
     (reify Parser
       (parse [parser input]
         ; For now, only retunr first match
         (first (scan-goal (peek (charts parser input)))))
       (charts [parser input]
         (parse-charts input grammar tokenizer goal-rule))))))

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
                                         (if (head (first match))
                                           (head (first match))
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
  "Defs a parser rule or seq of parser rules.
  
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
  ; TODO: test all above cases
  ; TODO: qualify syms
  [head & impl-or-impls]
  `(def ~head ~(build-defrule-bodies head impl-or-impls)))

(defmacro extend-rule
  "Like defrule, but extends an existing rule."
  [head & impl-or-impls]
  `(def ~head (vec (concat ~head ~(build-defrule-bodies head impl-or-impls)))))

; TODO: better name than add-rules
(defmacro add-rules
  [head & rules]
  `(def ~head (vec (concat ~head [~@rules]))))

; TODO: reformalize clauses.

; Assumed to represent another clause
(defn- clause-symbol? [clause]
  (or (symbol? clause) (keyword? clause)))

(defn- gmv-mapcat-helper [head thefn theseq]
  (try
    (mapcat thefn theseq)
    (catch Exception e
      (throw (RuntimeException.
               (str "Problem processing rule " head)
               e)))))

(defn- gmv-assoc-helper [head]
  (fn [rule]
    (try
      ;(assoc rule head head)
      (rehead-rule head rule)
      (catch Exception e
        (throw (RuntimeException.
                 (str "Problem processing rule " head)
                 e))))))

; resolves all clauses in a grammar seeded by the given goal,
; populating rule heads (overriding possibly)
(defn- resolve-all-clauses [goal thens theenv]
  (loop [stack [goal] ; stack: clauses to resolve
         breadcrumbs #{}
         return {}] ; grammar: maps keyword clauses to rules
    (if-let [current-head (first stack)]
      (let [predictions (if (or (vector? current-head) (seq? current-head))
                          current-head
                          (clauses current-head))]
        (cond
          ; have we already seen this? skip it entirely
          (contains? breadcrumbs current-head) (recur (rest stack) breadcrumbs return)
          ; do we have to look it up? if not skip it but keep track of predictions
          (not (symbol? current-head)) (recur (concat predictions (rest stack))
                                              (conj breadcrumbs current-head) return)
          true ; rule is a symbol--look it up
          (if-let [resolved (ns-resolve thens theenv current-head)]
            (let [resolved @resolved]
              (if (or (vector? resolved) (seq? resolved))
                ; assume it is a seq of rules
                (recur (concat (gmv-mapcat-helper current-head
                                                  clauses resolved) stack)
                       (conj breadcrumbs current-head)
                       (assoc return current-head
                              (map (gmv-assoc-helper current-head) resolved)))
                ; assume it is a rule
                (recur (cons resolved stack)
                       (conj breadcrumbs current-head)
                       (assoc return current-head
                              [((gmv-assoc-helper current-head) resolved)]))))
            (TIAE "Cannot resolve rule for head: " current-head))))
      return))) ; stack is empty--we are done

; IN the future, we might bind &env to theenv
; Currently this may cause a print-dup-not-defined error (because &env
; is a map of symbol -> LocalBinding)
(defn- build-grammar-in-env
  [goal grammar thens theenv]
  (apply concat (vals (resolve-all-clauses goal thens theenv))))

(defmacro build-parser
  "Builds an earley parser with the given goal rule. build-parser will
  crawl the tree of sub-rules, looking up any symbols in the current namespace
  and figuring out what rules or seqs of rules they map to. If any symbol
  could not be looked up, that is an error. After all the rules are looked up,
  the parser is built."
  ([goal]
   `(build-parser ~goal identity))
  ([goal tokenizer]
   `(build-parser-in-env '~goal ~tokenizer *ns*)))

(defn build-parser-in-env
  "Fn version of build-parser if you want to provide your own *ns*."
  ; TODO: test
  [goal tokenizer thens]
  (earley-parser goal tokenizer (build-grammar-in-env goal {} thens {})))
