(ns clearley.glr
  (require clojure.set
           [uncore.collections.worm-ordered-set :as os]
           [uncore.collections.worm-ordered-multimap :as omm]
           [clearley.npda :as npda]
           [clearley.rules :as rules]
           [uncore.str :as s])
  (use uncore.core))
; A GLR automaton.

; ===
; Parse items
; An Item is a rule together with some instrumentation.
; Items are the atoms of LR-automaton parsing.
; ===

; name: an object representing the clause that predicted this item
; should have a short str representation
; rule: the rule for this item
; backlink: the item from the original item set, before shifts
; (but including initial eager advances)
; match-count: the number of times this rule has been scanned or advanced
; follow: the follow set of this item = any terminal that can follow this item
; (depends on predicting items)
(defrecord Item [rule backlink match-count seed? follow]
  npda/IPrinting
  (npda/pstr [_] (str (if seed? "" "+ ") (rules/rule-str rule)
                      " : " (s/separate-str " " (map hexhash follow)))))

(defn new-item [rule seed? follow]
  (Item. rule nil 0 seed? follow))

; TODO test parsing the empty string
(defn eager-advance [item prediction?]
  (if item
    (if-let [rule2 (rules/eager-advance (:rule item))]
      (if (and prediction? (rules/is-complete? rule2))
        nil ; don't eager advance predicted items to completion
        (assoc item :rule rule2)))))

(defn eager-advances [item prediction?]
  (take-while identity (iterate #(eager-advance % prediction?) item)))

(defn predict-item [{:keys [rule follow]}]
  (mapcat #(eager-advances (new-item % false (rules/follow-first rule follow)) true)
          (remove fn? (rules/predict rule))))

(defn advance-item [item]
  (assoc (update-all item {:rule rules/advance, :match-count inc,
                           :backlink #(if % % item)})
         :seed? true))

(defn scan-item [item input-token]
  (if (some #(% input-token)
            (filter fn? (rules/predict (:rule item))))
    (advance-item item)))

; === Item sets ===

(defn pstr-item-set-item [item backlink-map]
  (let [predictor-str (->> item (omm/get-vec backlink-map)
                        (map npda/pstr) (s/separate-str ", ") s/cutoff)]
    (str (npda/pstr item) (if (seq predictor-str) (str " | " predictor-str)))))

(defn predict-into-item-set [{:keys [items backlink-map] :as item-set} item predictor]
  (if (empty? (omm/get-vec backlink-map item))
    (update-all item-set {:items #(conj % item)
                          :backlink-map #(omm/assoc % item predictor)})
    (update item-set :backlink-map #(omm/assoc % item predictor))))

(defn current-item [{items :items} dot]
  (when-not (>= dot (count items)) (get items dot)))

(defn close-item-set [seed-items]
  (loop [c {:items (vec seed-items), :backlink-map omm/empty}, dot 0]
    (if-let [s (current-item c dot)]
      (recur (reduce #(predict-into-item-set % %2 s)
                     c (predict-item s))
             (inc dot))
      c)))

(declare new-item-set)

(defn shift-item-set [items input-token mem-atom]
  (let [r (remove nil? (map #(scan-item % input-token) items))]
    (if (seq r)
      (new-item-set r mem-atom))))

; continuations an item given a stack-top item-set
(defn advance-item-set [backlink-map backlink seed? mem-atom]
  (when-let [new-items
             (seq (map advance-item 
                       (filter #(= seed? (:seed? %))
                               (omm/get-vec backlink-map backlink))))]
    (new-item-set new-items mem-atom)))

(defn item-set-returns [items lookahead]
  (let [acceptor (if (= lookahead :clearley.npda/term)
                   ; Special handling. We don't want to surprise pass ::term
                   ; to someone's scanner
                   #(= % lookahead)
                   #(and (ifn? %) (% lookahead)))]
  (filter (fn [{:keys [rule follow]}]
            (and (rules/is-complete? rule)
                 (some identity (map acceptor follow))))
          items)))

(defprotocol GlrState (is-goal [self]))

(defn new-item-set [seed-items mem-atom]
  (loop []
    (let [old-atom @mem-atom]
      (if-let [r (get old-atom seed-items)]
        r
        (let [more-seed-items (mapcat #(eager-advances % false) seed-items)
              item-set-num (count old-atom)
              ; items: a vector of items
              ; backlink-map: ordered multimap, items -> internal predicting items
              {all-items :items, :keys [backlink-map]} (close-item-set
                                                         more-seed-items)
              shift-fn (memoize #(shift-item-set all-items % mem-atom))
              returns (memoize #(item-set-returns more-seed-items %))
              continues (memoize #(advance-item-set backlink-map % true mem-atom))
              bounces (memoize #(advance-item-set backlink-map % false mem-atom)) 
              r (reify
                  npda/Node
                  (npda/node-key [_] item-set-num)
                  (npda/shift [_ input] (shift-fn input))
                  (npda/continue [_ output] (continues (:backlink output)))
                  (npda/bounce [_ output] (bounces (:backlink output)))
                  (npda/return [_ input-token] (returns input-token))
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

; === Using the automaton ===

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
(defn parse-charts [input-str grammar tokenizer goal mem-atom secondary-mem-atom]
  (binding [rules/*mem-atom* secondary-mem-atom]
    (doall ; TODO man what a hack this is
      (npda/run-automaton (new-item-set [(new-item (rules/goal-rule goal grammar) true
                                                   #{:clearley.npda/term})]
                                        mem-atom)
                          input-str tokenizer))))

(defn pstr-charts [charts]
  (dorun (map-> charts npda/pstr println)))

; Searches states for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (map (fn-> npda/stream reduce-ostream)
       (filter #(is-goal (npda/peek %)) (npda/states chart))))
