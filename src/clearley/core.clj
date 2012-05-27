(ns clearley.core
  "An easy-to-use, generalized context-free grammar parser. It will
  accept any seq of inputs, not just text, and parse any context-free grammar.
  Emphasis is on ease of use and dynamic/exploratory programming."
  (:require (clojure string))
  (:use clearley.utils))

(defprotocol EStrable
  (estr [obj] "Returns a shorthand str of this item."))

(defn eprint
  "Prints a shorthand str of the object to out."
  ([obj] (print (estr obj))))

; TODO: what is the purpose of tokenizers?

(defrecord ^:private Rule [head clauses action]
  EStrable
  (estr [_] (str head " -> " (separate-str clauses " "))))

(defn rule
  "Creates a rule associated with a parse action that can be called
  after matching. A rule is simply a map with a required vector of :clauses,
  an :head (optional, since Rules can also be embedded in othe rules),
  and an optional :action (the default action bundles the args into a list).
  The :clauses can be rule heads or rules themselves."
  ; TODO: check what a clause can be. Maybe move grammar-building up here.
  ([clauses] (rule nil clauses nil))
  ([head clauses] (rule head clauses nil))
  ([head clauses action] (Rule. head (vec clauses) action)))

; TODO: better token fns
(defn token
  "A rule that returns whatever it matches."
  ([a-token] (rule nil [a-token] (fn [_] a-token)))
  ([a-token value] (rule nil [a-token] (fn [_] value))))

; A grammar maps rule heads to rules. nil never maps to anything.
(defn- grammar [rules]
  (dissoc (group-by :head rules) nil))

(defn token-match [token] [token])

; Gets a seq of subrules from a clause
(defn- predict-clause [clause grammar]
  (if (sequential? clause)
    clause
    (get grammar clause [])))

; TODO have Rule implement this?
(defprotocol ^:private EarleyItem
  (predict [self])
  (escan [self input-token])
  (is-complete? [self])
  (advance [self]))

(defrecord ^:private REarleyItem [rule dot grammar]
  EarleyItem
  (predict [self]
    (if (not (is-complete? self))
      (map (fn [prediction]
             (REarleyItem. prediction 0 grammar))
           (predict-clause (get (:clauses rule) dot) grammar))))
  (escan [self input-token]
    (if (and (not (is-complete? self))
             (= (get (:clauses rule) dot) input-token))
      [(advance self)]
      []))
  (is-complete? [_]
    (= dot (count (:clauses rule))))
  (advance [self]
    (REarleyItem. rule (inc dot) grammar))
  EStrable
  (estr [_]
    (separate-str (concat [(:head rule) "->"]
                          (take dot (:clauses rule)) ["*"]
                          (drop dot (:clauses rule)))
                  " ")))

(defprotocol ^:private ChartItem
  (cpredict [self])
  (cscan [self input-token input])
  (emerge [self other-item]))

; Builds a rule match from the output stack and pushes the match to the top
; (think of a Forth operator reducing the top of a stack)
; Right now, we build the stack as we parse instead of emitting an output stream...
; this may change in the future e.g. to support pull parsing
(defn- reduce-ostack [ostack rule]
  (let [thecount (count (:clauses rule))]
    (cons (vec (cons rule (reverse (take thecount ostack)))) (drop thecount ostack))))

(defn merge-rstack [rstack rstack2]
  (swap! rstack #(merge-with merge-rstack % @rstack2)))

; earley-item: duh, rstack: @map<item rstack>, ostack: the output "stream"
(defrecord ^:private RChartItem [earley-item rstack ostack]
  ChartItem
  (cpredict [self]
    (if (is-complete? earley-item)
      (map (fn [[item rstack]]
             (RChartItem. (advance item) rstack
                          (reduce-ostack ostack (:rule earley-item)))) @rstack)
      (map (fn [item]
             (RChartItem. item (atom {earley-item rstack}) ostack))
           (predict earley-item))))
  (cscan [self input-token input]
    (map (fn [item]
           (RChartItem. item rstack (cons [input] ostack)))
         (escan earley-item input-token)))
  ; Merges the stacks of this and the other-item. True if there was anything to merge.
  (emerge [self other-item]
    (let [rstack2 @(:rstack other-item)]
      ; Well, this is inefficient. Chalk it up to clojure's non-support for keyset.
      ; Also, we eventually want to replace this with LR items when we
      ; optimize for speed.
      (if (seq (clojure.set/difference (into #{} (keys rstack2))
                                       (into #{} (keys @rstack))))
        (do (swap! rstack #(merge % @(:rstack other-item)))
          true)
        false)))
    ;(merge-rstack rstack (:rstack other-item)))
    ;(swap! rstack #(merge-rstack % (deref (:rstack other-item)))))
  EStrable
  (estr [_] (str (estr earley-item) " | " (separate-str (map (comp estr first) @rstack) ", "))))

(defn- earley-items [rulename grammar]
  (map #(RChartItem. (REarleyItem. % 0 grammar) (atom {}) '())
       (get grammar rulename [])))

; TODO: nuke this protocol
(defprotocol Chart
  (add [self item])
  (cfirst [self])
  (crest [self])
  (chart-seq [self]))

(defrecord RChart [chartvec chartmap dot]
  Chart
  (add [self item]
    (let [ikey (:earley-item item)]
      (if-let [previndex (get chartmap ikey)]
        (if (and (emerge (get chartvec previndex) item)
                 (>= dot previndex)
                 (is-complete? (:earley-item item)))
          ; we already processed this item
          ; but we may need to process extra completions
          ; TODO: this is awkward... chart is a hybrid of smart and dumb data object
          (reduce add self (cpredict item))
          self)
        (RChart. (conj chartvec item)
                 (assoc chartmap ikey (count chartvec)) dot))))
  (cfirst [self]
    (if (not (= dot (count chartvec)))
      (get chartvec dot)))
  (crest [self]
    (RChart. chartvec chartmap (inc dot)))
  (chart-seq [self] chartvec)
  EStrable
  (estr [self]
    (if (= dot (count chartvec))
      (apply str (map #(str (estr %) "\n") chartvec))
      (apply str (update-in (vec (map #(str (estr %) "\n") chartvec))
                            [dot] #(str "* " %))))))

(defn- new-chart [] (RChart. [] {} 0))

(defn- str-charts [charts]
  (apply str (interleave
               (repeat "---\n")
               charts)))

; parse a single chart, scanning into its successor
(defn- parse-chart [pchart1 pchart2 input-token input]
  (loop [chart1 pchart1 chart2 pchart2]
    (if-let [sitem (cfirst chart1)]
      (recur (crest (reduce add chart1 (cpredict sitem)))
             (reduce add chart2 (cscan sitem input-token input)))
      [chart1 chart2])))

; TODO: this scan-for-completions and manually completing the ostack thing
; is a little ugly... we might be better served by having a "goal symbol" like an LR
; parser table
(defn- scan-for-completions [chart thehead]
  (map (fn [x] (first (reduce-ostack (:ostack x) (:rule (:earley-item x)))))
       (filter (fn [ritem]
                 (let [rule (:rule (:earley-item ritem))]
                   (and (= (:head rule) thehead) (is-complete? (:earley-item ritem)))))
               (chart-seq chart))))

(defn- parsefn [inputstr grammar tokenizer goal]
  (loop [str1 inputstr charts [(reduce add (new-chart)
                                       (earley-items goal grammar))]]
    (if-let [thechar (first str1)]
      (let [thetoken (tokenizer thechar)
            [chart1 chart2] (parse-chart (peek charts) (new-chart) thetoken thechar)
            charts2 (conj (conj (pop charts) chart1) chart2)]
        (if (cfirst chart2)
          (recur (rest str1) charts2)
          charts2)) ; early termination on failure
      ; end step
      ; TODO not this hack, separate finish-chart fn?
      (let [[finalchart _] (parse-chart (peek charts) (new-chart) (Object.) (Object.))]
        (conj (pop charts) finalchart)))))

(defprotocol Parser
  (parse [parser input] "Parse the given input with the given parser,
                        yielding a match tree (a tree of the form
                        [rule leaves] where leaves is a seq).")
  (charts [parser input] "Parse the given input with the given parser,
                         yielding the parse charts."))

(defn earley-parser
  "Constructs an Earley parser, provided with a seq of rules and a predefined
  goal symbol. The parser will attempt to match the given input to the goal symbol,
  given the rules provided. The tokenizer should be a fn that maps input objects
  to the input tokens used in your grammar."
  ([goal rules]
   (earley-parser goal identity rules))
  ([goal tokenizer rules]
   (let [grammar (grammar rules)]
     (reify Parser
       (parse [parser input]
         (first (scan-for-completions (peek (charts parser input)) goal)))
       (charts [parser input]
         ; TODO: return entire chart, or just chart seq?
         ; obviously, just chart seq
         (parsefn input grammar tokenizer goal))))))

(defn parse-tree
  "Parses the given input with the given parser, yielding just the abstract
  syntax tree with no rules (effectively, the same as stripping the first elements
  (the rules) from a match tree)."
  [parser input]
  (if-let [match (parse parser input)]
    ((fn f [match] ; This fun will recursively reduce the match tree
       (if-let [submatches (seq (rest match))]
         (vec (map f submatches))
         (first match)))
       match)))

; TODO: expose print-charts and not charts
(defn eprint-charts [charts]
  (dorun (for [chart charts]
           (println (estr chart)))))

(defn take-action [match]
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

; TODO: is there a better way to do this?
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

(defmacro defrule [head & impl-or-impls]
  `(def ~head ~(build-defrule-bodies head impl-or-impls)))

(defmacro extend-rule [head & impl-or-impls]
  `(def ~head (vec (concat ~head ~(build-defrule-bodies head impl-or-impls)))))

; resolves all clauses in a grammar seeded by the given goal,
; populating rule :heads (overriding possibly)
; TODO: better semantics for rule heads
; TODO: don't populate anonymous rules on the seq
(defn- grammar-map-env [goal grammar thens theenv]
  (loop [stack [goal]
         rgrammar grammar]
    (let [current-head (first stack)]
      (cond (nil? current-head) rgrammar ; stack is empty--we are done
            (contains? rgrammar current-head) (recur (rest stack) rgrammar)
            (not (symbol? current-head)) (recur (rest stack) rgrammar)
            true ; so, rule is not in grammar--look it up
            (let [resolved (ns-resolve thens theenv current-head)]
              (if (nil? resolved)
                (TIAE "Cannot resolve rule for head: " current-head)
                (recur (concat (mapcat :clauses @resolved) stack)
                       (assoc rgrammar current-head
                              (map #(assoc % :head current-head) @resolved)))))))))

(defn build-grammar-in-env [goal grammar thens theenv]
  (apply concat (vals (grammar-map-env goal grammar thens theenv))))

(defmacro build-grammar [goal]
  `(build-grammar-in-env ~goal {} *ns* ~&env))

(defmacro build-parser
  ([goal]
   `(build-parser ~goal identity))
  ([goal tokenizer]
   `(earley-parser '~goal ~tokenizer (build-grammar '~goal))))
