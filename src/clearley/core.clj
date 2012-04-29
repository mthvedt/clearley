(ns clearley.core
  "An easy-to-use, generalized context-free grammar parser. It will
  accept any seq of inputs, not just text, and parse any context-free grammar.
  Emphasis is on ease of use and dynamic/exploratory programming."
  (:require (clojure string))
  (:use (clojure test) clearley.utils))

(defprotocol EStrable
  (estr [obj] "Returns a shorthand str of this item."))

(defn eprint
  "Prints a shorthand str of the object to out."
  ([obj] (print (estr obj))))

(defprotocol Rule
  (head [rule] "Returns this rule's head symbol.")
  (clauses [rule] "Returns an indexed seq of this rule's symbols.")
  (action [rule] "Returns this rule's parse action."))

(defrecord RuleImpl [ahead aclauses aaction]
  Rule
  (head [_] ahead)
  (clauses [_] aclauses)
  (action [_] aaction)
  EStrable
  (estr [_] (str ahead " -> " (separate-str aclauses " "))))

(defn rulefn
  "Creates a rule associated with a parse action that can be called
  after matching."
  [head clauses action]
  (RuleImpl. head (vec clauses) action))

(defn- list-identity [& args] args)

; todo symbol is a bad choice of words... letters perhaps?
(defn rule
  "Creates a context-free grammar rule that matches the first given symbol
  (the head symbol) to a sequence of subsymbols (the clauses).
  Any object may be a symbol."
  [head & clauses]
  (rulefn head clauses list-identity))

(extend-protocol Rule
  Object
  (head [rule] rule)
  (clauses [rule] [])
  (action [rule] (fn [] rule)))

; A grammar maps rule heads to rules
(defn grammar [rules]
  (group-by head rules))

(defprotocol Match
  (match-rule [match] "Returns the rule corresponding to the given match.
                      If a token, returns that token.")
  (submatches [match] "Returns the submatches of the given match."))

(defn token-match [token]
  (reify Match
    (match-rule [match] token)
    (submatches [match] [])))
; (take-action [match] token)))

(defprotocol Predictable
  (predict-symbol [thesymbol grammar]))

(extend-protocol Predictable
  RuleImpl
  (predict-symbol [thesymbol grammar] [thesymbol])
  Object
  (predict-symbol [thesymbol grammar]
    (get grammar thesymbol [])))

(defprotocol EarleyItem
  (get-key [self])
  (predict [self])
  (escan [self input-token input])
  (is-complete [self])
  (emerge [self other-item])
  (ematch [self]))

(defprotocol Completer
  (complete [self match]))

(defrecord REarleyItem [rule dot grammar completers match]
  EarleyItem
  (get-key [self] [rule dot])
  (predict [self]
    (if (= dot (count (clauses rule)))
      (map #(complete % (ematch self)) @completers)
      (map (fn [prediction]
             (REarleyItem. prediction 0 grammar
                           (atom [(reify Completer
                                    (complete [self2 match2]
                                      (REarleyItem. rule (inc dot)
                                                    grammar completers
                                                    (conj match match2)))
                                    (toString [self]
                                      ; TODO work on tostr of completions
                                      (str "complete " (estr rule))))])
                           []))
           (get grammar (get (clauses rule) dot) []))))
  (escan [self input-token input]
    (if (and (not (= dot (count (clauses rule))))
             (= (get (clauses rule) dot) input-token))
      [(REarleyItem. rule (inc dot) grammar completers
                     (conj match (token-match input)))]
      []))
  (is-complete [_]
    (= dot (count (clauses rule))))
  (emerge [self other-item]
    (swap! completers #(concat % (deref (:completers other-item))))
    nil)
  (ematch [self]
    (reify Match
      (match-rule [_] rule)
      (submatches [_] match)))
  ; (take-action [_] (apply (:action cfgitem) match))))
  EStrable
  (estr [_]
    (str (head rule) " -> "
         (separate-str (concat (take dot (clauses rule)) ["*"]
                               (drop dot (clauses rule)) ["|"]
                               @completers)
                       " "))))

(defn- earley-items [rulename grammar]
  (map #(REarleyItem. % 0 grammar (atom []) [])
       (get grammar rulename [])))

(defprotocol Chart
  (add [self item])
  (cfirst [self])
  (crest [self])
  (chart-seq [self]))

(defrecord RChart [chartvec chartmap dot]
  Chart
  ; TODO: recurse completions when re-adding already processed item
  (add [self item]
    (let [ikey (get-key item)]
      (if-let [previndex (get chartmap ikey)]
        (do (emerge (get chartvec previndex) item)
          (if (and (>= dot previndex) (is-complete item))
            ; we already processed this item
            ; but we may need to process extra completions
            ; TODO: this is awkward... chart is a hybrid of smart and dumb data object
            (reduce add self (predict item))
            self))
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

(defn- parse-chart [pchart1 pchart2 input-token input]
  (loop [chart1 pchart1 chart2 pchart2]
    (if-let [sitem (cfirst chart1)]
      (recur (crest (reduce add chart1 (predict sitem)))
             (reduce add chart2 (escan sitem input-token input)))
      [chart1 chart2])))

(defn- scan-for-completions [chart thehead]
  (map ematch (filter (fn [ritem]
                        (let [rule (:rule ritem)]
                          (and (= (head rule) thehead) (is-complete ritem))))
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
      ; todo: not this hack, separate finish-chart fn?
      (let [[finalchart _] (parse-chart (peek charts) (new-chart) (Object.) (Object.))]
        (conj (pop charts) finalchart)))))

(defprotocol Parser
  (parse [parser input] "Parse the given input with the given parser,
                        yielding a syntax tree where the leaves are the input.")
  (match [parser input] "Parse the given input with the given parser,
                        yielding match objects.")
  (charts [parser input] "Parse the given input with the given parser,
                         yielding the parse charts."))

(defn match-rules
  "Parse the given input with the given parser, returning a tree of the rules matched
  in the form of a lazy seq (rule & subtrees)."
  [parser input]
  ((fn f [match] (cons (match-rule match) (map f (submatches match))))
     (match parser input)))

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
       (match [parser input]
         (first (scan-for-completions (peek (charts parser input)) goal)))
       (parse [parser input]
         (if-let [match (match parser input)]
           ((fn f [x]
              (let [xs (submatches x)]
                (if (empty? xs)
                  (match-rule x)
                  (vec (map f xs)))))
              match)))
       (charts [parser input]
         ; TODO: return entire chart, or just chart seq?
         (parsefn input grammar tokenizer goal))))))

(defn eprint-charts [charts]
  (dorun (for [chart charts]
           (println (estr chart)))))

(defn take-action [match]
  (let [subactions (map take-action (submatches match))]
    (try
      (apply (action (match-rule match)) subactions)
      (catch clojure.lang.ArityException e
        (throw (RuntimeException. (str "Wrong # of params taking action for rule "
                                       (head (match-rule match)) ", "
                                       "was given " (count subactions))
                                  e))))))

; TODO: is there a better way to do this?
; Macro helper fn. Dequalifies a stringable qualified sym or keyword
(defn- dequalify [strable]
  (let [dequalified (clojure.string/split (str strable) #"/")]
    (symbol (nth dequalified (dec (count dequalified))))))

; Macro helper fn for defrule. Maps rule heads to appropriate unqual'd syms
(defn- clauses-to-syms [syms]
  (map (fn [sym]
         (cond (seq? sym) (if-let [thename (first sym)]
                            thename
                            (throw (IllegalArgumentException.
                                     (str "Not a valid subrule: " sym))))
               (keyword? sym) (dequalify sym)
               (symbol? sym) (dequalify sym)
               (= java.lang.String (type sym)) (symbol (str sym))
               true '_))
    syms))

; Maps rule clauses to the appropriate rule head
; then wraps it in a quote then a quasiquote
(defn- clauses-to-rulenames [clauses]
  (map (fn [sym] `'~sym)
       (map (fn [clause]
              (cond (seq? clause) (second clause)
                    true clause))
            clauses)))

; Macro helper fn. Builds the `(rulefn ...) bodies for defrule.
; Head: a symbol. impls: seq of (bindings bodies+) forms.
(defn- build-defrule-rule-bodies [head impls]
  (vec (map (fn [impl]
              (let [clauses (first impl)]
                (if (vector? clauses)
                  (let [action-bodies (rest impl)
                        ruleheads (clauses-to-rulenames clauses)
                        fnsyms (clauses-to-syms clauses)]
                    `(rulefn '~head [~@ruleheads]
                             (fn [~@fnsyms] ~@action-bodies)))
                  (throw (IllegalArgumentException.
                           "rule clauses must be a vector")))))
            impls)))

; Macro helper fn. Builds the body for defrule and related macros.
; Head: a symbol. impl-or-impls: (bindings bodies+) or ((bindings bodies+)+).
(defn- build-defrule-bodies [head impl-or-impls]
  (let [first-form (first impl-or-impls)]
    (cond (vector? first-form) (build-defrule-rule-bodies head
                                                          [(apply list first-form
                                                                 (rest impl-or-impls))])
          (seq? first-form) (build-defrule-rule-bodies head impl-or-impls)
          true (throw (IllegalArgumentException. (str "Not a valid defrule; "
                                                      "expected clause vector or "
                                                      "clause-body pairs"))))))

(defmacro defrule [head & impl-or-impls]
  `(def ~head ~(build-defrule-bodies head impl-or-impls)))

(defmacro extend-rule [head & impl-or-impls]
  `(def ~head (vec (concat ~head ~(build-defrule-bodies head impl-or-impls)))))

; TODO decide; grammars or ruleseqs?

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
                (throw (IllegalArgumentException.
                         (str "Cannot resolve rule for head: " current-head)))
                (recur (concat (mapcat clauses @resolved) stack)
                       (assoc rgrammar current-head @resolved))))))))

(defn build-grammar-in-env [goal grammar thens theenv]
  (apply concat (vals (grammar-map-env goal grammar thens theenv))))

(defmacro build-grammar [goal]
  `(build-grammar-in-env ~goal {} *ns* ~&env))

(defmacro build-parser
  ([goal]
   `(build-parser ~goal identity))
  ([goal tokenizer]
   `(earley-parser '~goal ~tokenizer (build-grammar '~goal))))
