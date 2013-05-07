(ns clearley.clr
  ; TODO eliminate npda dependency
  (require [clearley.npda :as npda]
           [clearley.rules :as rules]
           [uncore.collections.worm-ordered-multimap :as omm]
           [uncore.str :as s])
  (use uncore.core))

; Tools for CLR(1) grammars.

; The core abstractions are items and item sets.
; An item is a rule we're trying to match, and might want to match some sequence
; or choice of subrules. They are usually represented as dotted CFG rules:
; rule -> subrule1 • subrule2, signifying an rule that's matched subrule1
; and wants to match subrule2. An rule is said to be 'complete' if it has
; no more subrules to match.

; Items can have lookahead. For a complete item,
; an item to be 'complete with lookahead'
; means that it doesn't count as complete unless the next token in the input
; matches the lookahead. This is very useful for avoiding so-called 'shift reduce'
; conflicts, which are performance issues even in nondeterministic parsers.
; In particular, 'zero or more' items,
; like something that matches zero or more spaces,
; can decide whether it's complete or not based on whether a space is
; in the next input.

; An item set is a set of unique items, together with 'backlinks'
; recording item dependencies. An item 'predicts' subrules. If we have
; rule1 -> subrule1, this item can 'predict' subrule1 and subrule1 is added
; and 'backlinked' to rule1.

; A rule is 'nullable' if it can match the empty string. An item can be 'eager
; advanced' if one of its predicted subrules is nullable.

; TODO s/backlink/oroginal

; === Item record ===
; name: an object representing the clause that predicted this item
; should have a short str representation
; rule: the rule for this item
; backlink: the item from the original item set, before shifts
; (but including initial eager advances)
; match-count: the number of times this rule has been scanned or advanced
; follow: a terminal that can follow this item
; (depends on predicting items)
(defrecord Item [rule backlink match-count seed? follow])

(defn item-str [{:keys [rule seed? follow]}]
  (str (if seed? "" "+ ") (rules/rule-str rule)))

(defn item-str-follow [{follow :follow :as item}]
  (str (item-str item) " : " (hexhash follow)))

(defn new-item [rule seed? follow]
  (->Item rule nil 0 seed? follow))

(defn eager-advance [item prediction?]
  (if item
    (if-let [rule2 (rules/eager-advance (:rule item))]
      (if (and prediction? (rules/is-complete? rule2))
        nil ; don't eager advance predicted items to completion
        (assoc item :rule rule2)))))

(defn eager-advances [item prediction?]
  (take-while identity (iterate #(eager-advance % prediction?) item)))

; TODO don't need rules/follow-first to be a set
(defn predict-item [{:keys [rule follow]}]
  (mapcat #(eager-advances % true)
          (for [prediction (remove fn? (rules/predict rule))
                follow-terminal (rules/follow-first rule follow)]
            (new-item prediction false follow-terminal))))

(defn advance-item [item]
  (assoc (update-all item {:rule rules/advance, :match-count inc,
                           :backlink #(if % % item)})
         :seed? true))

(defn scan-item [item input-token]
  (if (some #(% input-token)
            (filter fn? (rules/predict (:rule item))))
    (advance-item item)))

(defn goal-item [goal grammar]
  (new-item (rules/goal-rule goal grammar) true ::term))

; === Item sets ===

; items; the items in teh set
; backlink-map: maps items -> predictors
(defrecord ItemSet [items backlink-map])

(defn item-set-item-str [item backlink-map]
  (let [predictor-str (->> item (omm/get-vec backlink-map)
                        (map item-str) (s/separate-str ", ") s/cutoff)]
    (str (item-str-follow item) (if (seq predictor-str) (str " | " predictor-str)))))

(defn item-set-str [{:keys [items backlink-map]}]
  (with-out-str
    (runmap println (map #(item-set-item-str % backlink-map) items))))

(defn predict-into-item-set [{:keys [items backlink-map] :as item-set} item predictor]
  (if (empty? (omm/get-vec backlink-map item))
    (update-all item-set {:items #(conj % item)
                          :backlink-map #(omm/assoc % item predictor)})
    (update item-set :backlink-map #(omm/assoc % item predictor))))

(defn current-item [{items :items} dot]
  (when-not (>= dot (count items)) (get items dot)))

(defn closed-item-set [seed-items]
  (loop [c (->ItemSet (vec seed-items) omm/empty), dot 0]
    (if-let [s (current-item c dot)]
      (recur (reduce #(predict-into-item-set % %2 s)
                     c (predict-item s))
             (inc dot))
      c)))

; TODO Item set format:
; 1. goal -> whatever
; 2. whatever -> whatever | called by 1, 3

(defn term-scanner [x] (= x ::term))

; Returns a map. Keys are the next token. Values are one of
; [:shift shifting-item] or [:return item-to-return].
(defn action-map [seed-items item-set]
  (let [rmap (reduce
               (fn [themap {:keys [rule] :as item}]
                 (let [shift-fns (filter fn? (rules/predict rule))]
                   (reduce #(omm/assoc % %2 [:shift item]) themap shift-fns)))
        ;(if (rules/is-complete? rule)
        ; (omm/assoc themap return-lookahead [:return item])
        ;themap))))
        omm/empty (:items item-set))]
    (reduce (fn [themap {:keys [rule follow] :as seed}]
              (if (rules/is-complete? rule)
                (omm/assoc themap follow [:return seed])
                themap))
            rmap seed-items)))

(defn get-actions-for-tag [key action-map tag]
  (for [[tag1 action] (omm/get-vec action-map key) :when (= tag tag1)]
    action))

; stopgap
(defn shift-action-map [input action-map]
  (let [r (some seq (map (fn [shift-fn]
                           (if (shift-fn input)
                             (get-actions-for-tag shift-fn action-map :shift)))
                         (omm/keys action-map)))]
    (map advance-item r)))

(defn has-shifters? [{items :items}]
  (some fn? (mapcat rules/predict (map :rule items))))

; Returns the result of an item set shift, or nil
(defn shift-item-set [{items :items} input-token]
  (seq (remove nil? (map #(scan-item % input-token) items))))

; continuations an item given a stack-top item-set
(defn advance-item-set [{backlink-map :backlink-map} backlink seed?]
  (seq (map advance-item (filter #(= seed? (:seed? %))
                                 (omm/get-vec backlink-map backlink)))))

; The returns (reduces) of some seed items
(defn returns [seed-items lookahead]
  (let [acceptor (if (= lookahead ::term)
                   ; Special handling. We don't want to surprise pass ::term
                   ; to someone's scanner
                   term-scanner
                   #(and (ifn? %) (% lookahead)))]
    (seq (filter (fn [{:keys [rule follow]}]
                   (and (rules/is-complete? rule)
                        (acceptor follow)))
                 seed-items))))

(defn single-return [seed-items]
  (let [returns (filter (fn-> :rule rules/is-complete?) seed-items)]
    (if (= 1 (count returns))
      (first returns)
      nil)))

(defn has-returns? [seed-items]
  (some (fn-> :rule rules/is-complete?) seed-items))
