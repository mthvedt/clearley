(ns org.eightnotrump.clearley
  "A generalized Earley parser, in Clojure. Designed to have an easy-to-use
  context free grammar notation. It can operate on any seq of input, not just
  text, and works without further fuss. In addition it can produce
  parse charts if you want to look under the hood."
  (:use (clojure test)))

; todo: publish this protocol
(defprotocol ^:private Item
  (head [item])
  (body [item])
  (ipredict [self grammar])
  (iscan [self input])
  (is-complete [self])
  (advance [self]))

(defn- separate-str [theseq separator]
  (apply str (drop 1 (interleave (repeat separator) theseq))))

(defrecord ^:private CFGItem [head dot clauses]
  Item
  (head [self] head)
  (body [self] clauses)
  (ipredict [self grammar]
    (if (= dot (count clauses))
      []
      (let [head2 (get clauses dot)]
        (get grammar head2 []))))
  (iscan [self input]
    (if (and (not (= dot (count clauses)))
             (= (get clauses dot) input))
      [(CFGItem. head (inc dot) clauses)]
      []))
  (is-complete [self]
    (= dot (count clauses)))
  (advance [self]
    (CFGItem. head (inc dot) clauses))
  (toString [self]
    (str head " -> "
         (if (= dot 0)
           (separate-str clauses " ")
           (separate-str (concat (take dot clauses) ["*"] (drop dot clauses)) " ")))))

(defn rule
  "Creates a context-free grammar rule that matches the first given symbol
  (the head symbol) to a sequence of subsymbols (the clauses).
  Any object may be a symbol."
  [head & clauses]
  (CFGItem. head 0 (vec clauses)))

(defn- add-to-grammar [grammar rule]
  (assoc grammar (head rule) (conj (get grammar (head rule) []) rule)))

(defprotocol ^:private EarleyItem
  (get-key [self])
  (predict [self])
  (escan [self input])
  (emerge [self other-item]))

(defprotocol ^:private Completer
  (complete [self match]))

(defrecord ^:private REarleyItem [cfgitem grammar completers match]
  EarleyItem
  (get-key [self] cfgitem)
  (predict [self]
    (if (is-complete cfgitem)
      (map #(complete % match) @completers)
      (map (fn [prediction]
             (REarleyItem. prediction grammar
                           (atom [(reify Completer
                                    (complete [self2 match2]
                                      (REarleyItem. (advance cfgitem)
                                                    grammar completers
                                                    (conj match match2)))
                                    (toString [self]
                                      (str "complete " cfgitem)))])
                           []))
           (ipredict cfgitem grammar))))
  (escan [self input]
    (map #(REarleyItem. % grammar completers (conj match input))
         (iscan cfgitem input)))
  (emerge [self other-item]
    (swap! completers #(concat % (deref (:completers other-item)))))
  (toString [self]
    (apply str cfgitem "|" completers ":" (interleave @completers (repeat " ")))))

(defn- earley-items [rulename grammar]
  (map #(REarleyItem. % grammar (atom []) [])
       (get grammar rulename [])))

(defprotocol ^:private Chart
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

(defn- parse-chart [pchart1 pchart2 input]
  (loop [chart1 pchart1 chart2 pchart2]
    (if-let [sitem (cfirst chart1)]
      (recur (crest (reduce add chart1 (predict sitem)))
             (reduce add chart2 (escan sitem input)))
      [chart1 chart2])))

(defn- scan-for-completions [chart thehead]
  (map :match (filter (fn [ritem]
                        (let [cfgitem (:cfgitem ritem)]
                          (and (= (head cfgitem) thehead) (is-complete cfgitem))))
                      (chart-seq chart))))

(defn- parsefn [inputstr grammar goal]
  (loop [str1 inputstr charts [(reduce add (new-chart)
                                       (earley-items goal grammar))]]
    (if-let [thechar (first str1)]
      (let [[chart1 chart2] (parse-chart (peek charts) (new-chart) thechar)
            charts2 (conj (conj (pop charts) chart1) chart2)]
        (if (cfirst chart2)
          (recur (rest str1) charts2)
          charts2)) ; early termination on failure
      ; end step
      (let [[finalchart _] (parse-chart (peek charts) (new-chart) (Object.))]
        (conj (pop charts) finalchart)))))

(defprotocol Parser
  (parse [parser input] "Parse the given input with the given parser.")
  (charts [parser input] "Parse the given input with the given parser,
                         yielding the parse charts."))

(defn earley-parser
  "Constructs an Earley parser, provided with a seq of rules and a predefined
  goal symbol. The parser will attempt to match the given input to the goal symbol,
  given the rules provided."
  [rules goal]
  (let [grammar (reduce add-to-grammar {} rules)]
    (reify Parser
      (parse [parser input]
        (first (scan-for-completions (peek (charts parser input)) goal)))
      (charts [parser input]
        (parsefn input grammar goal)))))
