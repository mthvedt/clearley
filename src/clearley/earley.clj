(ns clearley.earley
  (use [clearley utils rules]))

; ===
; Earley items
; An EarleyItem is a rule together with some instrumentation.
; ===

(defrecord REarleyItem [rulehead rule original predictor-chart match-count]
  PStrable
  (pstr [_]
    (str rulehead " -> " (rule-str rule) " @" (:index predictor-chart))))

(defn predict-earley-item [earley-item grammar]
  (let [clause (predict (:rule earley-item))]
    (map #(REarleyItem. (rulehead-clause clause) % % nil 0)
         (predict-clause clause grammar))))

(defn scan-earley-item [earley-item scanning-chart input-token]
  (map (fn [rule]
         (update-all (assoc earley-item :rule rule)
                     {:match-count inc,
                      :predictor-chart #(if % % scanning-chart)}))
    (scan (:rule earley-item) input-token)))

(defn advance-earley-item [earley-item predictor-chart]
  (update-all (if (:predictor-chart earley-item)
                earley-item
                (assoc earley-item :predictor-chart predictor-chart))
              {:rule advance, :match-count inc}))

(defn earley-item [head-sym clause]
  (let [rule (to-rule clause)]
    (REarleyItem. head-sym rule rule nil 0)))

; ===
; Parse states
; Earley items together with NDFA state and output stack
; ===

; Builds a rule match from the output stack and pushes the match to the top
; (think of a Forth operator reducing the top of a stack)
; Right now, we build the stack as we parse instead of emitting an output stream...
; this may change in the future e.g. to support pull parsing
(defn- reduce-ostack [ostack {:keys [match-count original]}]
  (cons (match original (vec (reverse (take match-count ostack))))
        (drop match-count ostack)))

; A parse item together with output state
; earley-item: duh, ostack: the output "stream"
(defrecord State [earley-item ostack]
  PStrable
  (pstr [_] (str (pstr earley-item))))

(defn predict-state [{:keys [earley-item] :as state} grammar]
  (map #(assoc state :earley-item %)
       (predict-earley-item earley-item grammar)))

(defn complete-state [{{:keys [predictor-chart original] :as earley-item} :earley-item
                       :keys [ostack]}]
  (map (fn [{predictor-item :earley-item}]
         (State. (advance-earley-item predictor-item predictor-chart)
                 (reduce-ostack ostack earley-item)))
       (get (:predictor-map predictor-chart) original #{})))

(defn scan-state [state scanning-chart input-token input]
  (map (fn [new-item]
         (update (assoc state :earley-item new-item)
                 :ostack (partial cons (match input []))))
       (scan-earley-item (:earley-item state) scanning-chart input-token)))

; creates an initial state with dot and pos 0
(defn state [head-sym rule]
  (State. (earley-item head-sym rule) '()))

; ===
; Parse charts
; ===

(defn pstr-chart-item [item predictor-map]
  (str (pstr item) " | "
       (separate-str ", " (map pstr (get predictor-map
                                         (:original (:earley-item item)))))))

(defrecord Chart [chartvec chartmap predictor-map index dot]
  PStrable
  (pstr [self]
    (let [strs (map #(str (pstr-chart-item % predictor-map) "\n") chartvec)]
      (if (= dot (count chartvec))
        (apply str strs)
        (apply str (update (vec strs) dot #(str "* " %)))))))

(def initial-chart (Chart. [] {} {} 0 0))

; assoc for a multimap (implemented as a map k -> #{v})
(defn assoc-multi [map k v] (update map k #(conj (if (nil? %) #{} %) v)))

(defn predict-into-chart [{:keys [chartvec chartmap predictor-map] :as chart}
                    {{original :original} :earley-item :as item}
                    predictor]
  (if (contains? predictor-map original)
    (update chart :predictor-map #(assoc-multi % original predictor))
    (update-all chart {:chartvec #(conj % item)
                       :chartmap #(assoc % original (count chartvec))
                       :predictor-map #(assoc-multi % original predictor)})))

; Should only be called on a new chart
(defn add-to-chart [chart item]
  (update chart :chartvec #(conj % item)))

(defn current-state [{:keys [chartvec dot]}]
  (when-not (>= dot (count chartvec))
    (get chartvec dot)))

(defn chart-seq [chart] (seq (:chartvec chart)))

; scans an input character, seeding a new chart
(defn scan-chart [chart pos input-token input]
  (reduce add-to-chart
          (assoc initial-chart :index pos)
          (mapcat #(scan-state % chart input-token input)
                  (chart-seq chart))))

(defn process-state [state chart grammar]
  (if (is-complete? (:rule (:earley-item state)))
    (reduce add-to-chart chart (complete-state state))
    (reduce #(predict-into-chart % %2 state) chart (predict-state state grammar))))

; process completions and predictions for a single chart
(defn- parse-chart [chart grammar]
  (loop [c chart]
    (if-let [s (current-state c)]
      (recur (update (process-state s c grammar)
                     :dot inc))
     c)))

(defn parse-charts [inputstr grammar tokenizer goal]
  (loop [pos 0
         thestr inputstr
         current-chart (add-to-chart initial-chart (state ::goal goal))
         charts []]
    (if-let [thechar (first thestr)]
      (let [thetoken (tokenizer thechar)
            parsed-chart (parse-chart current-chart grammar)
            next-chart (scan-chart parsed-chart (inc pos) thetoken thechar)
            next-charts (conj charts parsed-chart)]
        (if (current-state next-chart)
          (recur (inc pos) (rest thestr) next-chart next-charts)
          ; early termination on failure returning failed chart
          (conj next-charts next-chart)))
      ; end returning all charts
      (conj charts (parse-chart current-chart grammar)))))

; Searches a chart for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (mapcat :ostack (filter (comp (partial = ::goal) :rulehead :earley-item)
                          (chart-seq chart))))
