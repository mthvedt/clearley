(ns clearley.glr
  (require [clearley.collections.ordered-set :as os]
           [clearley.collections.ordered-multimap :as omm]
           [clearley.npda :as npda]
           [clearley.rules :as rules]
           [uncore.str :as s])
  (use uncore.core))

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
(defn close-item-set [item-set grammar]
  (loop [c item-set, dot 0]
    (if-let [s (current-item c dot)]
      (recur (reduce #(predict-into-item-set % %2 s)
                       c (predict-item s grammar))
             (inc dot))
      c)))

(defprotocol GlrState
  (is-goal [self]))

; seed items don't go in predictor-map, closed items do
(defn new-item-set [items grammar mem-atom]
  (if-let [r (get @mem-atom items)]
    r
    (let [r (let [item-set-num (count @mem-atom) ; assumes single thread
                  seed-item-set {:items (vec items) :predictor-map omm/empty}
                  {:keys [items predictor-map]
                   :as the-item-set} (close-item-set seed-item-set grammar)
                  shift-fn (memoize #(shift-item-set items grammar % mem-atom))
                  reductions (item-set-reductions the-item-set)
                  reduces (memoize #(reduce-item-set predictor-map grammar
                                                     % mem-atom))]
              ; items: a vector of items
              ; predictor-map: ordered multimap, items -> internal predicting items
              (reify
                npda/Node
                (npda/node-key [_] item-set-num)
                (npda/shift [_ input] (shift-fn input))
                (npda/reduce [_ output] (reduces (:original output)))
                (npda/reductions [_] reductions)
                GlrState
                (is-goal [_] (some #(rules/goal? (:rule %)) items))
                npda/IPrinting
                (npda/pstr [self]
                  (with-out-str
                    (runmap println (map #(pstr-item-set-item % predictor-map)
                                         items))))))]
      (swap! mem-atom #(assoc % items r))
      r)))


; scans an input character, seeding a new state
(defn shift-item-set [items grammar input-token mem-atom]
  (let [r (remove nil? (map #(scan-item % input-token grammar) items))]
    (if (seq r)
      (new-item-set r grammar mem-atom))))

; Reduces an item given a stack-top item-set
(defn reduce-item-set [predictor-map grammar original mem-atom]
  (when-let [new-items
             (seq (map advance-item (omm/get-vec predictor-map original)))]
    [(new-item-set new-items grammar mem-atom)]))

(defn item-set-reductions [{items :items}]
  (map (fn [{:keys [match-count] :as item}] [item match-count])
       (filter (fn-> :rule rules/is-complete?) items)))

; ===
; Using the automaton
; ===

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

; mem-atom: [map: seed items -> item-set]
(defn parse-charts [input-str grammar tokenizer goal mem-atom]
  (npda/run-automaton (new-item-set [(new-item (rules/goal-rule goal))]
                                    grammar mem-atom)
                      input-str tokenizer))

(defn pstr-charts [charts]
  (dorun (map-> charts npda/pstr println)))

; Searches states for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (map (fn-> npda/popone npda/stream reduce-ostream)
       (filter #(is-goal (npda/peek %)) (npda/states chart))))
