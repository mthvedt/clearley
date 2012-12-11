(ns clearley.earley
  (use [clearley utils rules]))

; ===
; Earley items
; ===

(defrecord REarleyItem [rulehead rule original index match-count]
  PStrable
  (pstr [_]
    (str rulehead " -> " (rule-str rule) " @" index)))

(defn predict-earley-item [earley-item grammar index]
  (let [clause (predict (:rule earley-item))]
    (map #(REarleyItem. (rulehead-clause clause) % % index 0)
         (predict-clause clause grammar))))

(defn scan-earley-item [earley-item input-token]
  (map #(update (assoc earley-item :rule %) :match-count inc)
    (escan (:rule earley-item) input-token)))

(defn advance-earley-item [earley-item]
  (update-all earley-item {:rule advance, :match-count inc}))

(defn earley-itemize [head-sym rule]
  (REarleyItem. head-sym rule rule 0 0))

; ===
; Parse states
; Earley items together with NDFA state and output stack
; ===

; Builds a rule match from the output stack and pushes the match to the top
; (think of a Forth operator reducing the top of a stack)
; Right now, we build the stack as we parse instead of emitting an output stream...
; this may change in the future e.g. to support pull parsing
(defn- reduce-ostack [ostack item]
  (let [thecount (:match-count item)]
    (cons (vec (cons (:original item)
                     (reverse (take thecount ostack)))) (drop thecount ostack))))

; A parse item together with output state
; rstack: map<item chart-ref>
; earley-item: duh, rstack: @map<item rstack>, ostack: the output "stream"
; TODO: eliminate need to wrap rstacks in atoms
(defrecord RChartItem [earley-item rstack ostack]
  PStrable
  (pstr [_] (str (pstr earley-item) " | "
                 (separate-str ", " (map (comp pstr first) @rstack)))))

(defn predict-state [{:keys [earley-item rstack] :as state} pos grammar]
  (map #(merge state {:earley-item %, :rstack (atom {earley-item rstack})})
       (predict-earley-item earley-item grammar pos)))

(defn complete-state [{:keys [earley-item rstack ostack] :as state} pos]
  (map (fn [[item rstack]]
         (RChartItem. (advance-earley-item item) rstack
                      (reduce-ostack ostack earley-item)))
       @rstack))

(defn process-state [state pos grammar]
  (if (is-complete? (:rule (:earley-item state)))
    (complete-state state pos)
    (predict-state state pos grammar)))

(defn scan-state [state input-token input]
  (map (fn [new-item]
         (update (assoc state :earley-item new-item) :ostack (partial cons [input])))
       (scan-earley-item (:earley-item state) input-token)))

; Merges the stacks of two items. True if there was anything to merge.
(defn merge-state [state other-state]
  (swap! (:rstack state) #(merge % @(:rstack other-state))))

; creates an initial earley item with dot and pos 0
(defn chart-item [head-sym rule]
  (RChartItem. (earley-itemize head-sym rule) (atom {}) '()))

; ===
; Parse charts
; ===

; TODO: nuke this protocol, have data object chart
; data object charts can also serve as prototypes of parsing NDFA states
(defprotocol Chart
  (add-to-chart [self item])
  (current-item [self])
  (inc-chart [self])
  (reset-chart [self])
  (chart-seq [self]))

(defrecord RChart [chartvec chartmap dot grammar]
  Chart
  (add-to-chart [self item]
    (let [ikey (:earley-item item)]
      (if-let [previndex (get chartmap ikey)]
        (do (merge-state (get chartvec previndex) item)
          self)
        (RChart. (conj chartvec item)
                 (assoc chartmap ikey (count chartvec)) dot grammar))))
  (current-item [self]
    (if (not (= dot (count chartvec)))
      (get chartvec dot)))
  (inc-chart [self]
    (RChart. chartvec chartmap (inc dot) grammar))
  (reset-chart [self] (RChart. chartvec chartmap 0 grammar))
  (chart-seq [self] (seq chartvec))
  PStrable
  (pstr [self]
    (if (= dot (count chartvec))
      (apply str (map #(str (pstr %) "\n") chartvec))
      (apply str (update (vec (map #(str (pstr %) "\n") chartvec))
                         dot #(str "* " %))))))

(defn initial-chart [grammar] (RChart. [] {} 0 grammar))

; scans an input character, seeding a new chart
(defn scan-chart [chart input-token input]
  (reduce add-to-chart (initial-chart (:grammar chart))
          (mapcat #(scan-state % input-token input)
                  (chart-seq chart))))

; process completions and predictions for a single chart
(defn- parse-chart [chart pos]
  (loop [c chart]
    (if-let [item (current-item c)]
      (recur (inc-chart (reduce add-to-chart c (process-state item pos
                                                              (:grammar chart)))))
     c)))

(defn parse-charts [inputstr grammar tokenizer goal-rule]
  (loop [pos 0
         thestr inputstr
         current-chart (add-to-chart (initial-chart grammar)
                                     (chart-item ::goal goal-rule))
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
