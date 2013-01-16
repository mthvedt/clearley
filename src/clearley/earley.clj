(ns clearley.earley
  (require [clearley.collections.ordered-set :as os]
           [clearley.collections.ordered-multimap :as omm]
           [clearley.npda :as npda])
  (use [clearley utils rules]))

; ===
; Parse items
; An Item is a rule together with some instrumentation.
; Items are the atoms of LR-automaton parsing.
; ===

; name: an object representing the clause that predicted this item
; should have a short str representation
; rule: the rule for this item
; original: the original (unadvanced) rule, used to populate matches
; match-count: the number of times this rule has been scanned or advanced
(defrecord Item [name rule original match-count]
  PStrable
  (pstr [_]
    (str name " -> " (rule-str rule))))

(defn new-item [name clause]
  (let [rule (to-rule clause)]
    (Item. name rule rule 0)))

(defn predict-item [item grammar]
  (let [clause (predict (:rule item))]
    (map #(new-item (clause-name clause) %)
         (predict-clause clause grammar))))

(defn scan-item [item input-token]
  (map #(update (assoc item :rule %) :match-count inc)
       (scan (:rule item) input-token)))

(defn advance-item [item]
  (update-all item {:rule advance, :match-count inc}))

; ===
; Item sets
; ===

(defn pstr-item-set-item [item predictor-map]
  (let [predictor-str
        (cutoff (separate-str ", " (map pstr (omm/get-vec predictor-map
                                                             (:original item)))))]
    (str (pstr item) (if (seq predictor-str) (str " | " predictor-str)))))

(declare shift-item-set reduce-item-set item-set-reductions)

; items: a vector of items
; predictor-map: ordered multimap, items -> internal predicting items
(defrecord ItemSet [items predictor-map grammar]
  npda/Node
  (npda/shift [self input] (shift-item-set self input))
  (npda/reduce [self output] (reduce-item-set self output))
  (npda/reductions [self] (item-set-reductions self))
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

(defn close-item-set [item-set]
  (loop [c item-set, dot 0]
    (if-let [s (current-item c dot)]
      (recur (reduce #(predict-into-item-set % %2 s)
                     c (predict-item s (:grammar item-set)))
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
  (when-let [new-items
             (seq (map advance-item (omm/get-vec (:predictor-map item-set) original)))]
    [(new-item-set new-items (:grammar item-set))]))

(defn item-set-reductions [{items :items}]
  (map (fn [{:keys [match-count] :as item}] [item match-count])
       (filter (fn-> :rule is-complete?) items)))

; ===
; Using the automaton
; ===

(defn is-goal [state]
  (some (fn-> :name (= ::goal)) (-> state npda/peek :items)))

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

#_(defn parse [input-str grammar tokenizer goal]
  (npda/run-automaton-2 (new-item-set [(new-item ::goal goal)] grammar)
                        input-str tokenizer))

(defn parse-charts [input-str grammar tokenizer goal]
  (npda/run-automaton (new-item-set [(new-item ::goal goal)] grammar)
                      input-str tokenizer))

; Searches states for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (map (fn-> npda/popone npda/stream reduce-ostream)
       (filter is-goal (npda/states chart))))
