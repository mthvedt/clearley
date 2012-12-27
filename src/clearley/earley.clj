(ns clearley.earley
  (require [clearley.collections.ordered-set :as os]
           [clearley.collections.ordered-multimap :as omm])
  (use [clearley utils rules]))

; ===
; Parse items
; An Item is a rule together with some instrumentation.
; Items are the atoms of LR-automaton parsing.
; ===

; rulehead: the clause predicting this item
; TODO reexamine ruleheads?
; rule: the rule for this item
; original: the original (unadvanced) rule, used to populate matches
; match-count: the number of times this rule has been scanned or advanced
(defrecord Item [rulehead rule original match-count]
  PStrable
  (pstr [_]
    (str rulehead " -> " (rule-str rule))))

(defn new-item [head-sym clause]
  (let [rule (to-rule clause)]
    (Item. head-sym rule rule 0)))

(defn predict-item [item grammar]
  (let [clause (predict (:rule item))]
    (map #(new-item (rulehead-clause clause) %)
         (predict-clause clause grammar))))

(defn scan-item [item input-token]
  (map #(update (assoc item :rule %) :match-count inc)
       (scan (:rule item) input-token)))

(defn advance-item [item]
  (update-all item {:rule advance, :match-count inc}))

; Reduces an item given a stack-top state
(defn reduce-item [{:keys [original] :as item} completing-state]
  (map advance-item (omm/get-vec (:predictor-map completing-state) original)))

; ===
; NPDA states
; ===

(defn pstr-state-item [item predictor-map]
  (let [predictor-str
        (cutoff (separate-str ", " (map pstr (omm/get-vec predictor-map
                                                             (:original item)))))]
    (str (pstr item) (if (seq predictor-str) (str " | " predictor-str)))))

; items: a vector of items
; predictor-map: ordered multimap, items -> predicting items
; ostream: the output associated with this item set; a vector (token | item)
; (this is safe because outside users shouldn't be using EarleyItem)
; prev-set: the origin state (will be null for the seed item at index 0)
(defrecord State [items predictor-map ostream rstack]
  PStrable
  (pstr [self]
    (with-out-str
      (println "Item set" (hash self))
      (print "Stack: ")
      (print (separate-str " " (map hash rstack)))
      (println)
      (runmap println (map #(pstr-state-item % predictor-map) items)))))

(def empty-state (State. [] omm/empty [] '()))

(defn predict-into-state [{:keys [items predictor-map] :as state}
                             {original :original :as item} predictor]
  (if (empty? (omm/get-vec predictor-map original))
    (update-all state {:items #(conj % item)
                          :predictor-map #(omm/assoc % original predictor)})
    (update state :predictor-map #(omm/assoc % original predictor))))

(defn current-item [{:keys [items]} dot]
  (when-not (>= dot (count items)) (get items dot)))

; TODO: predicting completed items seems to cause combinatorial explosion
(defn close-state [state grammar]
  (loop [c state, dot 0]
    (if-let [s (current-item c dot)]
      (recur (if (is-complete? (:rule s))
               c
               (reduce #(predict-into-state % %2 s)
                       c (predict-item s grammar)))
             (inc dot))
      c)))

; Should only be called on a new state
; These items are not 'predicted' hence not in predictor-map
(defn seed-state [state items grammar]
  (close-state
    (reduce (fn [s i] (update s :items #(conj % i)))
            state items)
    grammar))

(defn new-state [ostream rstack]
  (merge empty-state {:ostream ostream :rstack rstack}))

; TODO work out when to filter nontrivial
(defn nontrivial? [state] (seq (:items state)))

; scans an input character, seeding a new state
(defn shift-state [{:keys [items ostream rstack] :as state}
                      input-token input grammar]
  (seed-state
    (new-state (conj ostream input) (cons state rstack))
    (mapcat #(scan-item % input-token) items)
    grammar))

; Reduces the rules in an item set
(defn reduce-state [{:keys [ostream rstack items] :as state} grammar]
  (filter nontrivial?
          (mapcat
            (fn [{:keys [rule match-count] :as item}]
              (if (is-complete? rule)
                (map #(seed-state 
                        (new-state (conj ostream item)
                                      (drop (dec match-count) rstack))
                                     [%]
                        grammar)
                     (reduce-item item (nth rstack (dec match-count))))
                []))
            items)))

; ===
; Charts
; Implementing a nondeterministic automaton.
; ===

; states: an ordered set
(defrecord Chart [states]
  PStrable
  (pstr [self]
    (separate-str "---\n" (map pstr (os/vec states)))))

(def empty-chart (Chart. os/empty))

(defn seed-chart [item grammar]
  (assoc empty-chart :states 
         (os/ordered-set (seed-state empty-state [item] grammar))))

; process reductions and predictions for a single state
(defn complete-chart [chart grammar]
  (loop [c chart, dot 0]
    (if-let [set (os/get (:states c) dot)]
      (do
        (recur (reduce (fn [chart state]
                         (update chart :states #(os/conj % state)))
                       c (reduce-state set grammar))
               (inc dot)))
      c)))

(defn scan-chart [chart thetoken thechar grammar]
  (assoc empty-chart :states
         (os/into os/empty
                  (filter nontrivial?
                          (map #(shift-state % thetoken thechar grammar)
                               (os/vec (:states chart)))))))

(defn process-chart [chart token input grammar]
  (complete-chart (scan-chart chart token input grammar) grammar))

; The punch line
(defn parse-charts [inputstr grammar tokenizer goal]
  (loop [pos 0
         thestr inputstr
         current-chart (seed-chart (new-item ::goal goal) grammar)
         charts [current-chart]]
    (if-let [thechar (first thestr)]
      (let [next-chart (process-chart current-chart (tokenizer thechar) thechar grammar)
            next-charts (conj charts next-chart)]
        (if (seq (os/vec (:states next-chart)))
          (recur (inc pos) (rest thestr) next-chart next-charts)
          ; early termination on failure returning failed states
          next-charts))
      ; end returning all states
      charts)))

; TODO: if we omit seq, things break. why?
(defn goals-from-state [state]
  (seq (filter (comp (partial = ::goal) :rulehead) (:items state))))

; Builds a rule match from the output stack and pushes the match to the top
; (think of a Forth operator reducing the top of a stack)
; Final output (for a valid parse) will be a singleton list
(defn reduce-ostream-helper [ostream item]
  (if (instance? clearley.earley.Item item)
    (let [{:keys [match-count original]} item]
      (cons (match original (vec (reverse (take match-count ostream))))
            (drop match-count ostream)))
    (cons (match item []) ostream)))

(defn reduce-ostream [ostream]
  (first (reduce reduce-ostream-helper '() ostream)))

; Searches states for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (map reduce-ostream
       (map :ostream
            (filter goals-from-state (os/vec (:states chart))))))
