(ns clearley.earley
  (require [clearley.collections.ordered-set :as os]
           [clearley.collections.ordered-multimap :as omm])
  (use [clearley utils rules]))

; ===
; Earley items
; An EarleyItem is a rule together with some instrumentation.
; ===

(defrecord REarleyItem [rulehead rule original match-count]
  PStrable
  (pstr [_]
    (str rulehead " -> " (rule-str rule))))

(defn predict-earley-item [earley-item grammar]
  (let [clause (predict (:rule earley-item))]
    (map #(REarleyItem. (rulehead-clause clause) % % 0)
         (predict-clause clause grammar))))

(defn scan-earley-item [earley-item input-token]
  (map (fn [rule]
         (update (assoc earley-item :rule rule) :match-count inc))
    (scan (:rule earley-item) input-token)))

(defn advance-earley-item [earley-item]
  (update-all earley-item {:rule advance, :match-count inc}))

(defn earley-item [head-sym clause]
  (let [rule (to-rule clause)]
    (REarleyItem. head-sym rule rule 0)))

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
(defrecord State [earley-item predictor-chart ostack]
  PStrable
  (pstr [_] (str (pstr earley-item) " @" (:index predictor-chart))))

(defn predict-state [{:keys [earley-item] :as state} grammar]
  (map #(merge state {:earley-item % :predictor-chart nil})
       (predict-earley-item earley-item grammar)))

(defn complete-state [{{:keys [original] :as earley-item} :earley-item
                       :keys [predictor-chart ostack]}]
  (map (fn [{predictor-item :earley-item, prev-chart :predictor-chart}]
         (State. (advance-earley-item predictor-item) (if prev-chart
                                                        prev-chart
                                                        predictor-chart)
                 (reduce-ostack ostack earley-item)))
       (omm/get-vec (:predictor-map predictor-chart) original)))

(defn scan-state [state scanning-chart input-token input]
  (map (fn [new-item]
         (update-all (assoc state :earley-item new-item)
                 {:predictor-chart #(if % % scanning-chart)
                  :ostack (partial cons (match input []))}))
       (scan-earley-item (:earley-item state) input-token)))

; creates an initial state with dot and pos 0
(defn state [head-sym rule]
  (State. (earley-item head-sym rule) nil '()))

; ===
; Parse charts
; ===

(defn pstr-chart-item [item predictor-map]
  (str (pstr item) " | "
       (separate-str ", " (map pstr (omm/get-vec predictor-map
                                                 (:original (:earley-item item)))))))

; predictor-map: ordered multimap
(defrecord Chart [states predictor-map index dot]
  PStrable
  (pstr [self]
    (let [strs (map #(str (pstr-chart-item % predictor-map) "\n") states)]
      (if (= dot (count states))
        (apply str strs)
        (apply str (update (vec strs) dot #(str "* " %)))))))

(def initial-chart (Chart. [] omm/empty-ordered-multimap 0 0))

(defn predict-into-chart [{:keys [states predictor-map] :as chart}
                    {{original :original} :earley-item :as item}
                    predictor]
  (if (empty? (omm/get-vec predictor-map original))
    (update-all chart {:states #(conj % item)
                       :predictor-map #(omm/assoc % original predictor)})
    (update chart :predictor-map #(omm/assoc % original predictor))))

; Should only be called on a new chart
(defn add-to-chart [chart item]
  (update chart :states #(conj % item)))

(defn current-state [{:keys [states]} dot]
  (when-not (>= dot (count states))
    (get states dot)))

(defn chart-seq [chart] (seq (:states chart)))

; scans an input character, seeding a new chart
(defn scan-chart [chart pos input-token input]
  (reduce add-to-chart
          (assoc initial-chart :index pos)
          (mapcat #(scan-state % chart input-token input)
                  (chart-seq chart))))

; process completions and predictions for a single chart
(defn- parse-chart [chart grammar]
  (loop [c chart, dot 0]
    (if-let [s (current-state c dot)]
      (recur (if (is-complete? (:rule (:earley-item s)))
                       (reduce add-to-chart c (complete-state s))
                       (reduce #(predict-into-chart % %2 s) c (predict-state s grammar)))
             (inc dot))
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
        (if (current-state next-chart 0) ; chart is nonempty
          (recur (inc pos) (rest thestr) next-chart next-charts)
          ; early termination on failure returning failed chart
          (conj next-charts next-chart)))
      ; end returning all charts
      (conj charts (parse-chart current-chart grammar)))))

; Searches a chart for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (mapcat :ostack (filter (comp (partial = ::goal) :rulehead :earley-item)
                          (chart-seq chart))))
