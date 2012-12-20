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
(defn reduce-item [{:keys [original] :as item} completing-item-set]
  (map advance-item (omm/get-vec (:predictor-map completing-item-set) original)))

; ===
; Parse item sets
; Precalculated automaton states.
; ===

; Builds a rule match from the output stack and pushes the match to the top
; (think of a Forth operator reducing the top of a stack)
; Right now, we build the stack as we parse instead of emitting an output stream...
; this may change in the future e.g. to support pull parsing
(defn- reduce-ostack [ostack {:keys [match-count original]}]
  (cons (match original (vec (reverse (take match-count ostack))))
        (drop match-count ostack)))

(defn pstr-item-set-item [item predictor-map]
  (let [predictor-str
        (cutoff (separate-str ", " (map pstr (omm/get-vec predictor-map
                                                             (:original item)))))]
    (str (pstr item) (if (seq predictor-str) (str " | " predictor-str)))))

; items: a vector of items
; predictor-map: ordered multimap, items -> predicting items
; ostack: the output associated with this item set
; prev-set: the origin itemset (will be null for the seed item at index 0)
(defrecord ItemSet [items predictor-map ostack rstack]
  PStrable
  (pstr [self]
    (with-out-str
      (println "Item set" (hash self))
      (print "Stack: ")
      (print (separate-str " " (map hash rstack)))
      (println)
      (runmap println (map #(pstr-item-set-item % predictor-map) items)))))

(def empty-item-set (ItemSet. [] omm/empty '() '()))

(defn predict-into-item-set [{:keys [items predictor-map] :as item-set}
                             {original :original :as item} predictor]
  (if (empty? (omm/get-vec predictor-map original))
    (update-all item-set {:items #(conj % item)
                          :predictor-map #(omm/assoc % original predictor)})
    (update item-set :predictor-map #(omm/assoc % original predictor))))

; Should only be called on a new item-set
; These items are not 'predicted' hence not in predictor-map
(defn seed-item-set [item-set item] (update item-set :items #(conj % item)))

(defn current-item [{:keys [items]} dot]
  (when-not (>= dot (count items)) (get items dot)))

(defn nontrivial? [item-set] (seq (:items item-set)))

; scans an input character, seeding a new item-set
(defn shift-item-set [{:keys [items ostack rstack] :as item-set} pos input-token input]
  (reduce seed-item-set
          (merge empty-item-set {:ostack (cons (match input []) ostack)
                                 :rstack (cons item-set rstack)})
          (mapcat #(scan-item % input-token) items)))

; Reduces the rules in an item set
(defn reduce-item-set [{:keys [ostack rstack items] :as item-set}]
  (filter nontrivial?
          (mapcat
            (fn [{:keys [rule match-count] :as item}]
              (if (is-complete? rule)
                (map #(merge (seed-item-set empty-item-set %)
                             {:ostack (reduce-ostack ostack item)
                              :rstack (drop (dec match-count) rstack)})
                     (reduce-item item (nth rstack (dec match-count))))
                []))
            items)))

; TODO: predicting completed items seems to cause combinatorial explosion
(defn predict-item-set [item-set grammar]
  (loop [c item-set, dot 0]
    (if-let [s (current-item c dot)]
      (recur (if (is-complete? (:rule s))
               c
               (reduce #(predict-into-item-set % %2 s)
                       c (predict-item s grammar)))
             (inc dot))
      c)))

; ===
; Charts
; Implementing a nondeterministic automaton.
; ===

; item-sets: an ordered set
(defrecord Chart [item-sets]
  PStrable
  (pstr [self]
    (separate-str "---\n" (map pstr (os/vec item-sets)))))

(def empty-chart (Chart. os/empty))

(defn seed-chart [item]
  (assoc empty-chart :item-sets 
         (os/ordered-set (seed-item-set empty-item-set item))))

; process reductions and predictions for a single item-set
(defn complete-chart [chart]
  (loop [c chart, dot 0]
    (if-let [set (os/get (:item-sets c) dot)]
      (do
        (recur (reduce (fn [chart item-set]
                         (update chart :item-sets #(os/conj % item-set)))
                       c (reduce-item-set set))
               (inc dot)))
      c)))

(defn process-chart [chart grammar]
  (let [chart (complete-chart chart)]
    (assoc chart :item-sets
           (os/map #(predict-item-set % grammar) (os/vec (:item-sets chart))))))

(defn scan-chart [chart pos thetoken thechar]
  (assoc empty-chart :item-sets
         (os/into os/empty
                  (filter nontrivial?
                          (map #(shift-item-set % pos thetoken thechar)
                               (os/vec (:item-sets chart)))))))

; ===
; Here be dragons
; ===

(defn parse-charts [inputstr grammar tokenizer goal]
  (loop [pos 0
         thestr inputstr
         current-chart (seed-chart (new-item ::goal goal))
         charts []]
    (if-let [thechar (first thestr)]
      (let [thetoken (tokenizer thechar)
            parsed-chart (process-chart current-chart grammar)
            next-chart (scan-chart parsed-chart (inc pos) thetoken thechar)
            next-charts (conj charts parsed-chart)]
        (if (some #(current-item % 0) (os/vec (:item-sets next-chart)))
          ; an item set is nonempty
          (recur (inc pos) (rest thestr) next-chart next-charts)
          ; early termination on failure returning failed item-sets
          (conj next-charts next-chart)))
      ; end returning all item-sets
      (conj charts (process-chart current-chart grammar)))))

; TODO: if we omit seq, things break. why?
(defn goals-from-itemset [item-set]
  (seq (filter (comp (partial = ::goal) :rulehead) (:items item-set))))

; Searches item-sets for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (mapcat :ostack
          (filter goals-from-itemset (os/vec (:item-sets chart)))))
