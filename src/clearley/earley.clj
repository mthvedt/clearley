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

; ===
; Item sets
; ===

; TODO is match count the same for any item?
(defn pstr-item-set-item [item predictor-map]
  (let [predictor-str
        (cutoff (separate-str ", " (map pstr (omm/get-vec predictor-map
                                                             (:original item)))))]
    (str (pstr item) (if (seq predictor-str) (str " | " predictor-str)))))

; items: a vector of items
; predictor-map: ordered multimap, items -> internal predicting items
(defrecord ItemSet [items predictor-map grammar]
  PStrable
  (pstr [self]
    (with-out-str
      (runmap println (map #(pstr-item-set-item % predictor-map) items)))))

(defn predict-into-item-set [{:keys [items predictor-map] :as item-set}
                             {original :original :as item} predictor]
  (if (empty? (omm/get-vec predictor-map original))
    (update-all item-set {:items #(conj % item)
                          :predictor-map #(omm/assoc % original predictor)})
    (update item-set :predictor-map #(omm/assoc % original predictor))))

(defn current-item [{items :items} dot]
  (when-not (>= dot (count items)) (get items dot)))

; TODO: predicting completed items seems to cause combinatorial explosion
(defn close-item-set [item-set]
  (loop [c item-set, dot 0]
    (if-let [s (current-item c dot)]
      (recur (if (is-complete? (:rule s))
               c
               (reduce #(predict-into-item-set % %2 s)
                       c (predict-item s (:grammar item-set))))
             (inc dot))
      c)))

; seed items don't go in predictor-map, closed items do
(defn new-item-set [items grammar]
  (close-item-set (ItemSet. (vec items) omm/empty grammar)))

; scans an input character, seeding a new state
(defn shift-item-set [{:keys [items grammar] :as item-set} input-token]
  (when-let [r (seq (mapcat #(scan-item % input-token) items))]
    (new-item-set r grammar)))

; Reduces an item given a stack-top item-set
(defn reduce-item-set [item-set {:keys [original]}]
  (map #(new-item-set [%] (:grammar item-set))
       (map advance-item (omm/get-vec (:predictor-map item-set) original))))

(defn complete-rules [{items :items}]
  (filter #(is-complete? (:rule %)) items))

; ===
; NPDA states
; ===

; item-set: the item set for this state
; ostream: the output associated with this item set; a vector (token | item)
; stack: the origin state (will be null for the seed item at index 0)
(defrecord State [item-set ostream stack]
  PStrable
  (pstr [self]
    (with-out-str
      (println "State" (hash self))
      (print "Stack tops" (if (seq stack)
                            (separate-str " " (map hash stack))
                            "(none)"))
      (println)
      (print (pstr item-set)))))

; TODO: shift node, reduce node.
; shift node is easy. reduce node?--match count for a reduced node?
(defn shift-state [{:keys [ostream stack item-set] :as state} input-token input]
  (when-let [n (shift-item-set item-set input-token)]
    (State. n (conj ostream input) (cons state stack))))

; Creates a seq of reductions for a state
; (If more than one, is a reduce-reduce conflict)
(defn reduce-state [{{:keys [items] :as item-set} :item-set
                     :keys [ostream stack] :as state}]
  (mapcat
    (fn [{:keys [rule match-count] :as item}]
      (let [new-stack (drop (dec match-count) stack)]
        (map (fn [item-set]
               (State. item-set (conj ostream item) new-stack))
             (reduce-item-set (:item-set (first new-stack)) item))))
    (complete-rules item-set)))

(defn initial-state [item grammar]
  (State. (new-item-set [item] grammar) [] '{}))

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

(defn initial-chart [item grammar]
  (assoc empty-chart :states (os/ordered-set (initial-state item grammar))))

; process reductions and predictions for a single state
(defn complete-chart [chart]
  (loop [c chart, dot 0]
    (if-let [set (os/get (:states c) dot)]
      (do
        (recur (reduce (fn [chart state]
                         (update chart :states #(os/conj % state)))
                       c (reduce-state set))
               (inc dot)))
      c)))

(defn shift-chart [chart thetoken thechar]
  (assoc empty-chart :states
         (os/into os/empty
                  (remove nil?
                          (map #(shift-state % thetoken thechar)
                               (os/vec (:states chart)))))))

(defn process-chart [chart token input]
  (complete-chart (shift-chart chart token input)))

; The punch line
(defn parse-charts [inputstr grammar tokenizer goal]
  (loop [pos 0
         thestr inputstr
         current-chart (initial-chart (new-item ::goal goal) grammar)
         charts [current-chart]]
    (if-let [thechar (first thestr)]
      (let [next-chart (process-chart current-chart (tokenizer thechar) thechar)
            next-charts (conj charts next-chart)]
        (if (seq (os/vec (:states next-chart)))
          (recur (inc pos) (rest thestr) next-chart next-charts)
          ; early termination on failure returning failed states
          next-charts))
      ; end returning all states
      charts)))

(defn is-goal [state]
  (some #(-> % :rulehead (= ::goal)) (-> state :item-set :items)))

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
            (filter is-goal (os/vec (:states chart))))))
