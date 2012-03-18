(ns clearley.core
  "An easy-to-use, generalized context-free grammar parser. It will
  accept any seq of inputs, not just text, and parse any context-free grammar.
  Emphasis is on ease of use and dynamic/exploratory programming."
  (:use (clojure test) clearley.utils))

(defprotocol Rule
  (head [rule] "Returns this rule's head symbol.")
  (clauses [rule] "Returns an indexed seq of this rule's symbols."))

(defrecord RuleImpl [ahead aclauses]
  Rule
  (head [_] ahead)
  (clauses [_] aclauses)
  (toString [_] (str ahead " -> " (separate-str aclauses " "))))

(defn- action-rule ; todo figure this out
  "Creates a rule associated with a parse action that can be called
  after matching."
  [[head & clauses] action]
  (RuleImpl. head (vec clauses)))

; todo symbol is a bad choice of words... letters perhaps?
(defn rule
  "Creates a context-free grammar rule that matches the first given symbol
  (the head symbol) to a sequence of subsymbols (the clauses).
  Any object may be a symbol."
  [head & clauses]
  (action-rule (apply vector head clauses) (fn [& args] args)))

; A grammar maps rule heads to rules
(defn grammar [rules]
  (group-by head rules))

(defprotocol Match
  (match-rule [match] "Returns the rule corresponding to the given match.
                      If a token, returns that token.")
  (submatches [match] "Returns the submatches of the given match."))
  ; (take-action [match] "Executes any parse action corresponding to the match."))

(defn token-match [token]
  (reify Match
    (match-rule [match] token)
    (submatches [match] [])))
    ; (take-action [match] token)))

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
                                      (str "complete " rule)))]) ; todo better tostr
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
  (toString [_]
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
  (add [self item]
    (let [ikey (get-key item)]
      (if-let [previndex (get chartmap ikey)]
        (do (emerge (get chartvec previndex) item) self)
        (RChart. (conj chartvec item)
                 (assoc chartmap ikey (count chartvec)) dot))))
  (cfirst [self]
    (if (not (= dot (count chartvec)))
      (get chartvec dot)))
  (crest [self]
    (RChart. chartvec chartmap (inc dot)))
  (chart-seq [self] chartvec)
  (toString [self]
    (if (= dot (count chartvec))
      (apply str (map #(str % "\n") chartvec))
      (apply str (update-in (vec (map #(str % "\n") chartvec))
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
        (parsefn input grammar tokenizer goal))))))

#_(defn transform-parser [f parser]
  "Creates a new parser that composes the given parser with the given mapfn.
  The mapfn should accept one arg--an input stream--and return an input stream
  for the parser to consume.
  Useful for, for example, applying a tokenization pass."
  (reify Parser
    (match [parser input] (match parser (f input)))
    (parse [parser input] (parse parser (f input)))
    (charts [parser input] (charts parser (f input)))))

; TODO def rule.
