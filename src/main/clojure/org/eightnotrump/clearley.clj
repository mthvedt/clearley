(ns org.eightnotrump.clearley
  (:use (clojure pprint))
  (:import (org.eightnotrump.clearley Ruletype)))

; A production rule.
(defrecord Rule [head clauses])

; Example taken from Wikipedia Earley parsing page
(def myruleset [(Rule. :head [:sum])
                (Rule. :sum [:sum \+ :times])
                (Rule. :sum [:times])
                (Rule. :times [:times \* :num])
                (Rule. :times [:num])
                (Rule. :num [\1])
                (Rule. :num [\2])
                (Rule. :num [\3])
                (Rule. :num [\4])])

(def myinput "2*3+4")

(defn add-to-rulemap [rulemap rule]
  (let [head (get rule :head)
        mapped-rules (get rulemap head [])]
    (assoc rulemap head (conj mapped-rules (get rule :clauses)))))

(defn to-rulemap [ruleseq]
  (reduce add-to-rulemap {} ruleseq))

(def myrulemap (to-rulemap myruleset))

(defprotocol CfgItem
  (ipredict [self rulemap])
  (iscan [self input])
  (is-complete [self])
  (advance [self]))

(defrecord RItem [head dot clauses]
  CfgItem
  (ipredict [self rulemap]
    (if (= dot (count clauses))
      []
      (let [head2 (get clauses dot)]
        (map #(RItem. head2 0 %)
             (get rulemap head2 []))))) ; todo what if empty?
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
  (emerge [self other-item])) ; impl this later to support disambiguity

(defprotocol Completer
  (complete [self match]))

(defrecord REarleyItem [cfgitem rulemap completers match]
  EarleyItem
  (get-key [self] cfgitem)
  (predict [self]
    (if (is-complete cfgitem)
      (map #(complete % match) completers)
      (map (fn [prediction]
             (REarleyItem. prediction rulemap
                           [(reify Completer
                              (complete [self2 match2]
                                (REarleyItem. (advance cfgitem)
                                              rulemap completers
                                              (conj match match2)))
                              (toString [self]
                                (str "complete " cfgitem)))]
                           []))
           (ipredict cfgitem rulemap))))
  (escan [self input]
    (map #(REarleyItem. % rulemap completers (conj match input))
         (iscan cfgitem input)))
<<<<<<< HEAD
  ; (merge [self other-item]
  ;       (throw (UnsupportedOperationException.)))
=======
  (emerge [self other-item]
    (REarleyItem. cfgitem rulemap (concat (:completers other-item) completers)
                  match)) ; support match merging later
>>>>>>> 9a0d7ee... Parser works!
  (toString [self]
    (apply str cfgitem "|" completers)))

(defn initial-ritem [rulename rulemap]
  (REarleyItem.
    (RItem. rulename 0 (first (get rulemap rulename [])))
    rulemap [] []))

(def myitem (initial-ritem :head myrulemap))
(def myitem2 (initial-ritem :num myrulemap))

(defprotocol Chart
  (add [self item])
  (cfirst [self])
  (crest [self]))

(defrecord RChart [chartvec chartmap dot]
  Chart
  (add [self item]
    (let [ikey (get-key item)]
<<<<<<< HEAD
      (if-let [previtem (get chartmap ikey)]
        self ; we'll support disambiguity later
=======
      (if-let [previndex (get chartmap ikey)]
        (let [merged-item (emerge item (get chartvec previndex))]
          (RChart. (assoc chartvec previndex merged-item) chartmap dot))
>>>>>>> 9a0d7ee... Parser works!
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
(def mychart (new-chart))

(defn parse-chart [pchart1 pchart2 input]
  (loop [chart1 pchart1 chart2 pchart2]
    (if-let [sitem (cfirst chart1)]
      (do ;(println (apply str (interleave [chart1 chart2] (repeat "\n-\n"))))
        (recur (crest (reduce add chart1 (predict sitem)))
               (reduce add chart2 (escan sitem input))))
      [chart1 chart2])))

(defn str-charts [charts]
  (apply str (interleave
               (repeat "---\n")
               charts)))

(defn parse [inputstr rulemap head]
  (loop [str1 inputstr charts [(add (new-chart) (earley-item head rulemap))]]
    (if-let [thechar (first str1)]
      (let [[chart1 chart2] (parse-chart (peek charts) (new-chart) thechar)
            charts2 (conj (conj (pop charts) chart1) chart2)]
        (if (cfirst chart2)
          (recur (rest str1) charts2)
          charts2)) ; early termination on failure
      ; end step
      (let [[finalchart _] (parse-chart (peek charts) new-chart (Object.))]
        (conj (pop charts) finalchart)))))
