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
(defrecord State [earley-item predictor-item-set ostack]
  PStrable
  (pstr [_] (str (pstr earley-item) " @" (:index predictor-item-set))))

(defn predict-state [{:keys [earley-item] :as state} grammar]
  (map #(merge state {:earley-item % :predictor-item-set nil})
       (predict-earley-item earley-item grammar)))

(defn complete-state [{{:keys [original] :as earley-item} :earley-item
                       :keys [predictor-item-set ostack]}]
  (map (fn [{predictor-item :earley-item, prev-item-set :predictor-item-set}]
         (State. (advance-earley-item predictor-item) (if prev-item-set
                                                        prev-item-set
                                                        predictor-item-set)
                 (reduce-ostack ostack earley-item)))
       (omm/get-vec (:predictor-map predictor-item-set) original)))

(defn scan-state [state scanning-item-set input-token input]
  (map (fn [new-item]
         (update-all (assoc state :earley-item new-item)
                     {:predictor-item-set #(if % % scanning-item-set)
                      :ostack (partial cons (match input []))}))
       (scan-earley-item (:earley-item state) input-token)))

; creates an initial state with dot and pos 0
(defn state [head-sym rule]
  (State. (earley-item head-sym rule) nil '()))

; ===
; Parse item-sets
; ===

(defn pstr-item-set-item [item predictor-map]
  (str (pstr item) " | "
       (cutoff (separate-str ", " (map pstr (omm/get-vec predictor-map
                                                 (:original (:earley-item item))))))))

; predictor-map: ordered multimap
(defrecord ItemSet [states predictor-map index]
  PStrable
  (pstr [self]
    (apply str (map #(str (pstr-item-set-item % predictor-map) "\n") states))))

(def initial-item-set (ItemSet. [] omm/empty 0))

(defn predict-into-item-set [{:keys [states predictor-map] :as item-set}
                             {{original :original} :earley-item :as item}
                             predictor]
  (if (empty? (omm/get-vec predictor-map original))
    (update-all item-set {:states #(conj % item)
                          :predictor-map #(omm/assoc % original predictor)})
    (update item-set :predictor-map #(omm/assoc % original predictor))))

; Should only be called on a new item-set
(defn add-to-item-set [item-set item]
  (update item-set :states #(conj % item)))

(defn current-state [{:keys [states]} dot]
  (when-not (>= dot (count states))
    (get states dot)))

(defn item-set-seq [item-set] (seq (:states item-set)))

; scans an input character, seeding a new item-set
(defn scan-item-set [item-set pos input-token input]
  (reduce add-to-item-set
          (assoc initial-item-set :index pos)
          (mapcat #(scan-state % item-set input-token input)
                  (item-set-seq item-set))))

; process completions and predictions for a single item-set
(defn- complete-item-set [item-set]
  (loop [c item-set, dot 0]
    (if-let [s (current-state c dot)]
      (recur (if (is-complete? (:rule (:earley-item s)))
               (reduce add-to-item-set c (complete-state s))
               c)
             (inc dot))
      c)))

; TODO: predicting completed items seems to cause combinatorial explosion
(defn- predict-item-set [item-set grammar]
  (loop [c item-set, dot 0]
    (if-let [s (current-state c dot)]
      (recur (if (is-complete? (:rule (:earley-item s)))
               c
               (reduce #(predict-into-item-set % %2 s)
                       c (predict-state s grammar)))
             (inc dot))
      c)))

(defn parse-item-set [item-set grammar]
  (predict-item-set (complete-item-set item-set) grammar))

; ===
; Charts
; ===

; item-set: an ordered set
(defrecord Chart [item-set]
  PStrable
  (pstr [self]
    (separate-str "\n" (map pstr (os/vec item-set)))))

(def empty-chart (Chart. os/empty)) ; TODO change to just empty

(defn seed-chart [goal]
  (assoc empty-chart :item-set
         (os/ordered-set (add-to-item-set initial-item-set (state ::goal goal)))))

(defn process-chart [chart grammar]
  (assoc chart :item-set
         (os/map #(parse-item-set % grammar) (os/vec (:item-set chart)))))

(defn scan-chart [chart pos thetoken thechar]
  (assoc empty-chart :item-set
         (os/map #(scan-item-set % pos thetoken thechar) (os/vec (:item-set chart)))))

(defn parse-charts [inputstr grammar tokenizer goal]
  (loop [pos 0
         thestr inputstr
         current-chart (seed-chart goal)
         charts []]
    (if-let [thechar (first thestr)]
      (let [thetoken (tokenizer thechar)
            parsed-chart (process-chart current-chart grammar)
            next-chart (scan-chart parsed-chart (inc pos) thetoken thechar)
            next-charts (conj charts parsed-chart)]
        (if (some #(current-state % 0) (os/vec (:item-set next-chart)))
          ; an item set is nonempty
          (recur (inc pos) (rest thestr) next-chart next-charts)
          ; early termination on failure returning failed item-set
          (conj next-charts next-chart)))
      ; end returning all item-sets
      (conj charts (process-chart current-chart grammar)))))

; Searches a item-set for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (mapcat
    #(mapcat :ostack (filter (comp (partial = ::goal) :rulehead :earley-item)
                          (item-set-seq %)))
    (os/vec (:item-set chart))))
