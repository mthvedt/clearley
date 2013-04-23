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
; TODO: is original used?
; original: the original (unadvanced) rule, used to populate matches
; backlink: the item as it first appeared in an Earley set
; TODO should original be a cfg rule? or something more primitive
; match-count: the number of times this rule has been scanned or advanced
; TODO kill ndpa/IPrinting?
(defrecord Item [rule original backlink match-count seed?]
  npda/IPrinting
  (npda/pstr [_] (str (if seed? "- " "+ ") (rules/rule-str rule))))

(defn new-item [rule seed?]
  (Item. rule rule rule 0 seed?))

(defn eager-advance [item grammar prediction?]
  (if item
    (if-let [rule2 (rules/eager-advance (:rule item) grammar)]
      (if prediction?
        (merge item {:rule rule2, :backlink rule2})
        (assoc item :rule rule2)))))

(defn eager-advances [item grammar prediction?]
  (take-while identity (iterate #(eager-advance % grammar prediction?) item)))

(defn predict-item [item grammar]
  (mapcat #(eager-advances (new-item % false) grammar true)
          (filter rules/rule? (rules/predict (:rule item) grammar))))

(defn advance-item [item]
  (assoc (update-all item {:rule rules/advance, :match-count inc}) :seed? true))

(defn scan-item [item input-token grammar]
  (if (some #(% input-token)
            (remove rules/rule? (rules/predict (:rule item) grammar)))
    (advance-item item)))

; ===
; Item sets
; ===

(defn pstr-item-set-item [item backlink-map]
  (let [predictor-str (->> item :original (omm/get-vec backlink-map)
                        (map npda/pstr) (s/separate-str ", ") s/cutoff)]
    (str (npda/pstr item) (if (seq predictor-str) (str " | " predictor-str)))))

(declare shift-item-set advance-item-set item-set-returns)

(defn predict-into-item-set [{:keys [items backlink-map] :as item-set}
                             {backlink :backlink :as item} predictor]
  (if (empty? (omm/get-vec backlink-map backlink))
    (update-all item-set {:items #(conj % item)
                          :backlink-map #(omm/assoc % backlink predictor)})
    (update item-set :backlink-map #(omm/assoc % backlink predictor))))

(defn current-item [{items :items} dot]
  (when-not (>= dot (count items)) (get items dot)))

; TODO: predicting completed items seems to cause combinatorial explosion
; but only for some grammars (JSON)
(defn close-item-set [seed-items grammar]
  (loop [c {:items (vec seed-items), :backlink-map omm/empty}, dot 0]
    (if-let [s (current-item c dot)]
      (recur (reduce #(predict-into-item-set % %2 s)
                     c (predict-item s grammar))
             (inc dot))
      c)))

(defprotocol GlrState
  (is-goal [self]))

; seed items don't go in backlink-map, closed items do
(defn new-item-set [seed-items grammar mem-atom]
  (loop []
    (let [old-atom @mem-atom]
      (if-let [r (get old-atom seed-items)]
        r
        (let [more-seed-items (mapcat #(eager-advances % grammar false) seed-items)
              item-set-num (count old-atom)
              ; items: a vector of items
              ; backlink-map: ordered multimap, items -> internal predicting items
              {all-items :items, :keys [backlink-map]} (close-item-set
                                                         more-seed-items grammar)
              shift-fn (memoize #(shift-item-set all-items grammar % mem-atom))
              returns (item-set-returns more-seed-items)
              continues (memoize #(advance-item-set backlink-map grammar
                                                    % true mem-atom))
              bounces (memoize #(advance-item-set backlink-map grammar
                                                  % false mem-atom)) 
              r (reify
                  npda/Node
                  (npda/node-key [_] item-set-num)
                  (npda/shift [_ input] (shift-fn input))
                  (npda/continue [_ output] (continues (:backlink output)))
                  (npda/bounce [_ output] (bounces (:backlink output)))
                  (npda/return [_] returns)
                  GlrState
                  (is-goal [_] (some #(rules/goal? (:rule %)) all-items))
                  npda/IPrinting
                  (npda/pstr [self]
                    (with-out-str
                      (runmap println (map #(pstr-item-set-item % backlink-map)
                                           all-items)))))]
          (if (compare-and-set! mem-atom old-atom (assoc old-atom seed-items r))
            r
            (recur)))))))

; scans an input character, seeding a new state
(defn shift-item-set [items grammar input-token mem-atom]
  (let [r (remove nil? (map #(scan-item % input-token grammar) items))]
    (if (seq r)
      (new-item-set r grammar mem-atom))))

; continuations an item given a stack-top item-set
(defn advance-item-set [backlink-map grammar backlink seed? mem-atom]
  (when-let [new-items
             (seq (map advance-item 
                       (filter #(= seed? (:seed? %))
                               (omm/get-vec backlink-map backlink))))]
    (new-item-set new-items grammar mem-atom)))

(defn item-set-returns [items]
  (filter (fn-> :rule rules/is-complete?) items))

; ===
; Using the automaton
; ===

; Builds a rule match from the output stack and pushes the match to the top
; (think of a Forth operator reducing the top of a stack)
; Final output (for a valid parse) will be a singleton list
(defn ostream-str [val]
  (if (instance? clearley.rules.Match val)
    (pr-str (rules/take-action* val))
    (pr-str val)))
(defn reduce-ostream-helper [ostream val]
  (if (instance? clearley.glr.Item val)
    (let [{:keys [rule match-count]} val]
      (cons (rules/match (rules/get-original rule)
                         (vec (reverse (take match-count ostream))))
            (drop match-count ostream)))
    (do
      (cons (rules/match val []) ostream))))

(defn reduce-ostream [ostream]
  (first (reduce reduce-ostream-helper '() ostream)))

; mem-atom: [map: seed items -> item-set]
(defn parse-charts [input-str grammar tokenizer goal mem-atom]
  (npda/run-automaton (new-item-set [(new-item (rules/goal-rule goal) true)]
                                    grammar mem-atom)
                      input-str tokenizer))

(defn pstr-charts [charts]
  (dorun (map-> charts npda/pstr println)))

; Searches states for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (map (fn-> npda/stream reduce-ostream)
       (filter #(is-goal (npda/peek %)) (npda/states chart))))
