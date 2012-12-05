(ns clearley.earley
  (import [clearley.rules REarleyItem])
  (use [clearley utils rules]))

(defprotocol ChartItem
  (cpredict [self pos])
  (cscan [self input-token input])
  (emerge [self other-item]))

; Builds a rule match from the output stack and pushes the match to the top
; (think of a Forth operator reducing the top of a stack)
; Right now, we build the stack as we parse instead of emitting an output stream...
; this may change in the future e.g. to support pull parsing
(defn- reduce-ostack [ostack rule]
  (let [thecount (count (clauses rule))]
    (cons (vec (cons rule (reverse (take thecount ostack)))) (drop thecount ostack))))

(defrecord ChartRef [chart dot])

; rstack: map<item chart-ref>
; earley-item: duh, rstack: @map<item rstack>, ostack: the output "stream"
; TODO: eliminate need to wrap rstacks in atoms
(defrecord RChartItem [earley-item rstack ostack]
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
(defn chart-item [head-sym rule grammar]
  (RChartItem. (REarleyItem. head-sym rule 0 0 grammar) (atom {}) '()))

; TODO: nuke this protocol, have data object chart
; data object charts can also serve as prototypes of parsing NDFA states
(defprotocol Chart
  (add-to-chart [self item])
  (current-item [self])
  (inc-chart [self])
  (reset-chart [self])
  (chart-seq [self]))

(defrecord RChart [chartvec chartmap dot]
  Chart
  (add-to-chart [self item]
    (let [ikey (:earley-item item)]
      (if-let [previndex (get chartmap ikey)]
        (do (emerge (get chartvec previndex) item)
          self)
        (RChart. (conj chartvec item)
                 (assoc chartmap ikey (count chartvec)) dot))))
  (current-item [self]
    (if (not (= dot (count chartvec)))
      (get chartvec dot)))
  (inc-chart [self]
    (RChart. chartvec chartmap (inc dot)))
  (reset-chart [self] (RChart. chartvec chartmap 0))
  (chart-seq [self] (seq chartvec))
  PStrable
  (pstr [self]
    (if (= dot (count chartvec))
      (apply str (map #(str (pstr %) "\n") chartvec))
      (apply str (update-in (vec (map #(str (pstr %) "\n") chartvec))
                            [dot] #(str "* " %))))))

(def empty-chart (RChart. [] {} 0))

; scans an input character, seeding a new chart
(defn scan-chart [chart input-token input]
  (reduce add-to-chart empty-chart (mapcat #(cscan % input-token input)
                                           (chart-seq chart))))

; process completions and predictions for a single chart
(defn- parse-chart [chart pos]
  (loop [c chart]
    (if-let [item (current-item c)]
      (recur (inc-chart (reduce add-to-chart c (cpredict item pos))))
     c)))

(defn parse-charts [inputstr grammar tokenizer goal-rule]
  (loop [pos 0
         thestr inputstr
         current-chart (add-to-chart empty-chart (chart-item ::goal goal-rule grammar))
         charts []]
    (if-let [thechar (first thestr)]
      (let [thetoken (tokenizer thechar)
            parsed-chart (parse-chart current-chart pos)
            next-chart (scan-chart parsed-chart thetoken thechar)
            next-charts (conj charts parsed-chart)]
        (if (current-item next-chart)
          (recur (inc pos) (rest thestr) next-chart next-charts)
          ; early termination on failure returning failed chart
          (conj next-charts next-chart)))
      ; end returning all charts
      (conj charts (parse-chart current-chart (inc pos))))))

; Searches a chart for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (mapcat :ostack (filter (comp (partial = ::goal) :rulehead :earley-item)
                          (chart-seq chart))))
