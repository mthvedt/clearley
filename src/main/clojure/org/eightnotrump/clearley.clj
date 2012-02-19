(ns org.eightnotrump.clearley
  (:use (clojure pprint)))

; A production rule.
(defrecord Rule [head clauses])

(defn rule [head & clauses]
  (Rule. head (vec clauses)))

(defn add-to-rulemap [rulemap rule]
  (let [head (get rule :head)
        mapped-rules (get rulemap head [])]
    (assoc rulemap head (conj mapped-rules (get rule :clauses)))))

(defn to-rulemap [ruleseq]
  (reduce add-to-rulemap {} ruleseq))

(defprotocol Item
  (ipredict [self rulemap])
  (iscan [self input])
  (is-complete [self])
  (advance [self]))

(defrecord RItem [head dot clauses]
  Item
  (ipredict [self rulemap]
    (if (= dot (count clauses))
      []
      (let [head2 (get clauses dot)]
        (map #(RItem. head2 0 %)
             (get rulemap head2 [])))))
  (iscan [self input]
    (if (and (not (= dot (count clauses)))
             (= (get clauses dot) input))
      [(RItem. head (inc dot) clauses)]
      []))
  (is-complete [self]
    (= dot (count clauses)))
  (advance [self]
    (RItem. head (inc dot) clauses))
  (toString [self]
    (str head " -> "
         (apply str (take dot clauses)) "*"
         (apply str (drop dot clauses)))))

(defprotocol EarleyItem
  (get-key [self])
  (predict [self])
  (escan [self input])
  (emerge [self other-item]))

(deftype CompletedItem []
  EarleyItem
  (get-key [self] (Object.))
  (predict [self] [])
  (escan [self input] [])
  (emerge [self other-item] self)
  (toString [self] "Accept"))

(defprotocol Completer
  (complete [self match]))

(defrecord REarleyItem [cfgitem rulemap completers match]
  EarleyItem
  (get-key [self] cfgitem)
  (predict [self]
    (if (is-complete cfgitem)
      (map #(complete % match) @completers)
      (map (fn [prediction]
             (REarleyItem. prediction rulemap
                           (atom [(reify Completer
                                    (complete [self2 match2]
                                      (REarleyItem. (advance cfgitem)
                                                    rulemap completers
                                                    (conj match match2)))
                                    (toString [self]
                                      (str "complete " cfgitem)))])
                           []))
           (ipredict cfgitem rulemap))))
  (escan [self input]
    (map #(REarleyItem. % rulemap completers (conj match input))
         (iscan cfgitem input)))
  (emerge [self other-item]
    (swap! completers #(concat % (deref (:completers other-item)))))
  (toString [self]
    (apply str cfgitem "|" completers ":" (interleave @completers (repeat " ")))))

(defn earley-items [rulename rulemap completers]
  (map #(REarleyItem. (RItem. rulename 0 %) rulemap (atom completers) [])
       (get rulemap rulename [])))

(defprotocol Chart
  (add [self item])
  (cfirst [self])
  (crest [self]))

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
  (toString [self]
    (if (= dot (count chartvec))
      (apply str (map #(str % "\n") chartvec))
      (apply str (update-in (vec (map #(str % "\n") chartvec))
                            [dot] #(str "* " %))))))

(defn new-chart [] (RChart. [] {} 0))

(defn parse-chart [pchart1 pchart2 input]
  (loop [chart1 pchart1 chart2 pchart2]
    (if-let [sitem (cfirst chart1)]
      (recur (crest (reduce add chart1 (predict sitem)))
             (reduce add chart2 (escan sitem input)))
      [chart1 chart2])))

(defn str-charts [charts]
  (apply str (interleave
               (repeat "---\n")
               charts)))

(defn parsefn [inputstr rulemap head completers]
  (loop [str1 inputstr charts [(reduce add (new-chart)
                                       (earley-items head rulemap completers))]]
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
  (parse [self input]))

(defn earley-parser [rulemap head]
  (reify Parser
    (parse [self input] 
      (let [return-match (atom [])
            return-completer (reify Completer
                               (complete [self match]
                                 ; this is ugly... squelches all matches but the last
                                 ; means no meaningful disambiguation
                                 (swap! return-match (fn [_] match))
                                 (CompletedItem.))
                               (toString [self] "accept"))]
        (parsefn input rulemap head [return-completer])
        @return-match))))
