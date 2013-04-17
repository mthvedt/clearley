(ns clearley.glr
  (require [clearley.collections.ordered-set :as os]
           [clearley.collections.ordered-multimap :as omm]
           [clearley.npda :as npda]
           [clearley.rules :as rules]
           [uncore.str :as s])
  (use uncore.core))

; TODO work out format.
; A symbol -> choice of rules
; A tagged clause
; Only two choices.

; ===
; Parse items
; An Item is a rule together with some instrumentation.
; Items are the atoms of LR-automaton parsing.
; ===

; name: an object representing the clause that predicted this item
; should have a short str representation
; rule: the rule for this item
; original: the original (unadvanced) rule, used to populate matches
; TODO should original be a cfg rule? or something more primitive
; match-count: the number of times this rule has been scanned or advanced
; TODO kill ndpa/IPrinting?
(defrecord Item [rule original match-count]
  npda/IPrinting
  (npda/pstr [_] (rules/rule-str rule)))

(defn new-item [rule]
  (Item. rule rule 0))

(defn predict-item [item grammar]
  (map new-item (filter rules/rule? (rules/predict (:rule item) grammar))))

(defn advance-item [item]
  (update-all item {:rule rules/advance, :match-count inc}))

(defn scan-item [item input-token grammar]
  (if (some #(% input-token) (remove rules/rule? (rules/predict (:rule item) grammar)))
    (advance-item item)))

; ===
; Item sets
; ===

(defn pstr-item-set-item [item predictor-map]
  (let [predictor-str (->> item :original (omm/get-vec predictor-map)
                        (map npda/pstr) (s/separate-str ", ") s/cutoff)]
    (str (npda/pstr item) (if (seq predictor-str) (str " | " predictor-str)))))

(declare shift-item-set reduce-item-set item-set-reductions)

; items: a vector of items
; predictor-map: ordered multimap, items -> internal predicting items
(defrecord ItemSet [items predictor-map grammar]
  npda/Node
  (npda/shift [self input] (shift-item-set self input))
  (npda/reduce [self output] (reduce-item-set self output))
  (npda/reductions [self] (item-set-reductions self))
  npda/IPrinting
  (npda/pstr [self]
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
; but only for some grammars (JSON)
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
  (when-let [r (remove nil? (map #(scan-item % input-token grammar) items))]
    (new-item-set r grammar)))

; Reduces an item given a stack-top item-set
(defn reduce-item-set [item-set {:keys [original]}]
  (when-let [new-items
             (seq (map advance-item
                       (omm/get-vec (:predictor-map item-set) original)))]
    [(new-item-set new-items (:grammar item-set))]))

(defn item-set-reductions [{items :items}]
  (map (fn [{:keys [match-count] :as item}] [item match-count])
       (filter (fn-> :rule rules/is-complete?) items)))

; ===
; Using the automaton
; ===

; TODO matches and goals, matches and goals
(defn goal? [state]
  (some #(rules/goal? (:rule %)) (-> state npda/peek :items)))

; TODO need goal rule
; Builds a rule match from the output stack and pushes the match to the top
; (think of a Forth operator reducing the top of a stack)
; Final output (for a valid parse) will be a singleton list
(defn reduce-ostream-helper [ostream item]
  (if (instance? clearley.glr.Item item)
    (let [{:keys [match-count original]} item]
      (cons (rules/match (:original original)
                         (vec (reverse (take match-count ostream))))
            (drop match-count ostream)))
    (cons (rules/match item []) ostream)))

(defn reduce-ostream [ostream]
  (first (reduce reduce-ostream-helper '() ostream)))

(defn parse-charts [input-str grammar tokenizer goal] ; TODO
  (npda/run-automaton (new-item-set [(new-item (rules/goal-rule goal))] grammar)
                      input-str tokenizer))

(defn pstr-charts [charts]
  (dorun (map-> charts npda/pstr println)))

; Searches states for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (map (fn-> npda/popone npda/stream reduce-ostream)
       (filter goal? (npda/states chart))))

; TODO eliminate extra charts
