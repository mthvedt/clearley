(ns clearley.earley
  (require clojure.set
           [uncore.collections.worm-ordered-set :as os]
           [uncore.collections.worm-ordered-multimap :as omm]
           [clearley.clr :as clr]
           [clearley.npda :as npda]
           [clearley.rules :as rules]
           [uncore.str :as s])
  (use uncore.core))
; A PEP automaton. Implementing Aycock and Horspool's
; Practical Earley Parsing, with minor changes.
(defprotocol EarleyState (goals [self]))

(declare new-item-set)

; Anaphoric; takes a symbol 'mem-atom
(defmacro mem-new [& forms]
  `(memoize (fn [~'%] (new-item-set (~@forms) ~'mem-atom))))

(defn new-item-set [seed-items mem-atom]
  (if (empty? seed-items)
    nil
    (loop []
      (let [old-atom @mem-atom]
        (if-let [r (get old-atom seed-items)]
          r
          (let [more-seed-items (mapcat #(clr/eager-advances % false) seed-items)
                item-set-num (count old-atom)
                item-set (clr/closed-item-set more-seed-items)
                shift-fn (mem-new clr/shift-item-set (:items item-set) %)
                returns (memoize #(clr/returns more-seed-items %))
                continues (mem-new clr/advance-item-set item-set % true)
                bounces (mem-new clr/advance-item-set item-set % false)
                r (reify
                    npda/Node
                    (npda/node-key [_] item-set-num)
                    (npda/shift [_ input] (shift-fn input))
                    (npda/continue [_ output] (continues (:backlink output)))
                    (npda/bounce [_ output] (bounces (:backlink output)))
                    (npda/return [_ input-token] (returns input-token))
                    EarleyState
                    (goals [_] (filter #(rules/goal? (:rule %)) (:items item-set)))
                    npda/IPrinting
                    (npda/pstr [self] (clr/item-set-str item-set)))]
            (if (compare-and-set! mem-atom old-atom (assoc old-atom seed-items r))
              r
              (recur))))))))

; === Using the automaton ===

; Builds a rule match from the output stack and pushes the match to the top
; (think of a Forth operator reducing the top of a stack)
; Final output (for a valid parse) will be a singleton list
(defn ostream-str [val]
  (if (instance? clearley.rules.Match val)
    (pr-str (rules/take-action* val))
    (pr-str val)))
(defn reduce-ostream-helper [ostream val]
  (if (instance? clearley.clr.Item val)
    (let [{:keys [rule match-count]} val]
      (cons (rules/match (rules/get-original rule)
                         (vec (reverse (take match-count ostream))))
            (drop match-count ostream)))
    (do
      (cons (rules/match val []) ostream))))

(defn reduce-ostream [ostream]
  (first (reduce reduce-ostream-helper '() ostream)))

; mem-atom: [map: seed items -> item-set]
(defn parse-charts [input-str grammar tokenizer goal mem-atom secondary-mem-atom
                    return-charts?]
  (binding [rules/*mem-atom* secondary-mem-atom]
    ((if return-charts? doall last) ; TODO man what a hack this is
      (npda/run-automaton (new-item-set [(new-item (rules/goal-rule goal grammar) true
                                                   #{:clearley.npda/term})]
                                        mem-atom)
                          input-str tokenizer))))

(defn pstr-charts [charts]
  (dorun (map-> charts npda/pstr println)))

(defn goal-streams [state]
  (map #(->> state npda/rstream (cons %) reverse reduce-ostream)
       (goals (npda/peek state))))

; Searches states for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (mapcat goal-streams (npda/states chart)))
