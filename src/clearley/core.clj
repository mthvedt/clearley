(ns clearley.core
  "A generalized context-free grammar parser. It will
  accept any seq of inputs, not just text, and parse any context-free grammar.
  Emphasis is on ease of use, versatility, and dynamic/exploratory programming."
  (:require (clojure string))
  (:use clearley.utils))
; TODO: test compositability/extensibility

(defprotocol ^:private PStrable
  (^:private pstr [obj] "pstr stands for \"pretty-string\".
                        Returns a shorthand str of this item."))

(defrecord ^:private Rule [head clauses action])

(defn rule
  "Creates a rule associated with a parse action that can be called
  after matching. A rule has a required vector of clauses,
  an head (optional, since Rules can also be embedded in other rules),
  and an optional action (the default action bundles the args into a list).
  A clause can be a rule head referring to one or more rules,
  or a seq of one or more rules (anonymous rules)."
  ([clauses] (rule nil clauses nil))
  ([head clauses] (rule head clauses nil))
  ([head clauses action] (Rule. head (vec clauses) action)))

(defn pstr-rule [rule]
  (str (:head rule) " -> " (separate-str (:clauses rule) " ")))

; TODO: better token fns
(defn token
  "A rule that matches a single object (the token) and returns whatever it matches."
  ([a-token] (rule nil [a-token] (fn [_] a-token)))
  ([a-token value] (rule nil [a-token] (fn [_] value))))

(defn scanner
  "Defines a rule that scans one token of input with the given scanner function.
  The scanner function is used by the parser to match tokens. If this rule is invoked
  on a token, and the scanner returns logcial true, the rule matches the token."
  [scanner-fn action]
  ; TODO: test scanners
  ; TODO: a hack here: the below clause is highly unlikely to match anything
  (assoc (Rule. nil [(str "Scanner<" scanner-fn ">")] action) :scanner scanner-fn))

; A grammar maps rule heads to rules. nil never maps to anything.
(defn- grammar [rules]
  (dissoc (group-by :head rules) nil))

(defn- token-match [token] [token])

; Gets a seq of subrules from a clause
(defn- predict-clause [clause grammar]
  (if (sequential? clause)
    clause
    (get grammar clause [])))

; TODO: polymorphism. EarleyItem can be much faster.
; But the parser automaton should come first, since parser automata are big perf win
; and whatever polymorphism EarleyItme has should be tailored to that.
;
; what do i want this to look like...
; (scanner (fn token -> result))
; (scanner scanner-fn action) is better
(defprotocol ^:private EarleyItem
  (predict [self index])
  (escan [self input-token])
  (is-complete? [self])
  (advance [self]))

(defrecord ^:private REarleyItem [rule dot index grammar]
  EarleyItem
  (predict [self pos]
    (if (not (is-complete? self))
      (map (fn [prediction]
             (REarleyItem. prediction 0 pos grammar))
           (predict-clause (get (:clauses rule) dot) grammar))))
  (escan [self input-token]
    (cond
      (:scanner rule)
      (if ((:scanner rule) input-token)
        [(advance self)]
        [])
      (and (not (is-complete? self)) (= (get (:clauses rule) dot) input-token))
      [(advance self)]
      true
      []))
  (is-complete? [_]
    (= dot (count (:clauses rule))))
  (advance [self]
    (REarleyItem. rule (inc dot) index grammar))
  PStrable
  (pstr [_]
    (separate-str (concat [(:head rule) "->"]
                          (take dot (:clauses rule)) ["*"]
                          (drop dot (:clauses rule)) [(str "@" index)])
                  " ")))

(defprotocol ^:private ChartItem
  (cpredict [self pos])
  (cscan [self input-token input])
  (emerge [self other-item]))

; Builds a rule match from the output stack and pushes the match to the top
; (think of a Forth operator reducing the top of a stack)
; Right now, we build the stack as we parse instead of emitting an output stream...
; this may change in the future e.g. to support pull parsing
(defn- reduce-ostack [ostack rule]
  (let [thecount (count (:clauses rule))]
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

; TODO: nuke this protocol
(defprotocol Chart
  (add [self item])
  (cfirst [self])
  (crest [self])
  (reset [self])
  (chart-seq [self]))

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

(def new-chart (RChart. [] {} 0))

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

(defprotocol Parser
  (parse [parser input] "Parse the given input with the given parser,
                        yielding a match tree (a tree of the form
                        [rule leaves] where leaves is a seq).")
  ; charts is not yet usable by external users, so private
  (^:private charts [parser input]))

; Searches a chart for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (mapcat :ostack (filter (comp (partial = ::goal) :head :rule :earley-item)
                          (chart-seq chart))))

(defn earley-parser
  "Constructs an Earley parser, provided with a seq of rules and a predefined
  goal symbol. The parser will attempt to match the given input to the goal symbol,
  given the rules provided. The tokenizer should be a fn that maps input objects
  to the input tokens used in your grammar."
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
  "For a givne parser and input, prints a string representation of its charts to
  *out*."
  [parser input]
  (dorun (for [chart (charts parser input)]
           (println (pstr chart)))))

(defn take-action
  "Executes the parse actions for a parser match."
  [match]
  (if (nil? match)
    (throw (RuntimeException. "Failure to parse"))
    (let [subactions (map take-action (rest match))
          action1 (:action (first match))
          action (if (nil? action1) (fn [& args] args) action1)] ; default action
      (try
        (apply action subactions)
        (catch clojure.lang.ArityException e
          (throw (RuntimeException. (str "Wrong # of params taking action for rule "
                                         (:head (first match)) ", "
                                         "was given " (count subactions))
                                    e)))))))

; Defrule begins here.
; TODO: experiment with using a parser for defrule
; instead of all these macro helpers... would make a convincing POC for earley parsing!

; Macro helper fn. Dequalifies a stringable qualified sym or keyword
(defn- dequalify [strable]
  (let [dequalified (clojure.string/split (str strable) #"/")]
    (symbol (nth dequalified (dec (count dequalified))))))

; Macro helper fn for def rule. Returns a pair of
; [appropriate-symbol-for-action-body, rule-or-rulename]
(defn- process-nonseq-clause [clause]
  (cond (seq? clause) (assert false)
        (symbol? clause) [(dequalify clause) `'~clause]
        (keyword? clause) [(dequalify clause) clause]
        (= java.lang.String (type clause)) [(symbol (str clause)) clause]
        true ['_ clause])) ; can't be an arg in a fn

; Macro helper fn for def rule. Returns a pair of
; [appropriate-symbol-for-action-body, rule-or-rulename-or-uneval'd-form]
(defn- process-clause [clause]
  (if (seq? clause)
    (if-let [thename (first clause)]
      (let [therule (second clause)]
        [thename (if (seq? therule) ; A form to evaluate
                   ; Return it; will be eval'd when defrule called
                   therule
                   ; See what process-nonseq-clause has to say
                   (second (process-nonseq-clause therule)))])
      (TIAE "Not a valid subrule: " clause))
    (process-nonseq-clause clause)))

; Macro helper fn. Builds the `(rule ...) bodies for defrule.
; Head: a symbol. impls: seq of (bindings bodies+) forms.
(defn- build-defrule-rule-bodies [head impls]
  (vec (map (fn [impl]
              (let [clauses (first impl)]
                (if (vector? clauses)
                  (let [processed-clauses (map process-clause clauses)]
                    `(rule '~head [~@(map second processed-clauses)]
                           (fn [~@(map first processed-clauses)] ~@(rest impl))))
                  (TIAE "rule clauses must be a vector"))))
            impls)))

; Macro helper fn. Builds the body for defrule and related macros.
; Head: a symbol. impl-or-impls: (bindings bodies+) or ((bindings bodies+)+).
(defn- build-defrule-bodies [head impl-or-impls]
  (let [first-form (first impl-or-impls)]
    (cond (vector? first-form)
          (build-defrule-rule-bodies head [(apply list first-form
                                                  (rest impl-or-impls))])
          (seq? first-form) (build-defrule-rule-bodies head impl-or-impls)
          true (TIAE "Not a valid defrule; "
                     "expected clause vector or clause-body pairs"))))

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

; resolves all clauses in a grammar seeded by the given goal,
; populating rule :heads (overriding possibly)
(defn- grammar-map-env [goal grammar thens theenv]
  (loop [stack [goal]
         rgrammar grammar]
    (let [current-head (first stack)]
      (cond (nil? current-head) rgrammar ; stack is empty--we are done
            (contains? rgrammar current-head) (recur (rest stack) rgrammar)
            (not (symbol? current-head)) (recur (rest stack) rgrammar)
            true ; rule is a symbol--look it up
            (if-let [resolved (ns-resolve thens theenv current-head)]
                (recur (concat (mapcat :clauses @resolved) stack)
                       (assoc rgrammar current-head
                              (map #(assoc % :head current-head) @resolved)))
                (TIAE "Cannot resolve rule for head: " current-head))))))

(defn- build-grammar-in-env
  [goal grammar thens theenv]
  (apply concat (vals (grammar-map-env goal grammar thens theenv))))

(defmacro build-parser
  "Builds an earley parser with the given goal rule. build-parser will
  crawl the tree of sub-rules, looking up any symbols in the current namespace
  and figuring out what rules or seqs of rules they map to. If any symbol
  could not be looked up, that is an error. After all the rules are looked up,
  the parser is built."
  ([goal]
   `(build-parser ~goal identity))
  ([goal tokenizer]
   `(build-parser-in-env '~goal ~tokenizer *ns* ~&env)))

(defn build-parser-in-env
  "Fn version of build-parser if you want to provide your own *ns* and enviornment
  map."
  ; TODO: test, change build-parser
  [goal tokenizer thens theenv]
  (earley-parser goal tokenizer (build-grammar-in-env goal {} thens theenv)))
