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

; Items can have lookahead. For a complete item, an item to be 'complete with lookahead'
; means that it doesn't count as complete unless the next token in the input
; matches the lookahead. This is very useful for avoiding so-called 'shift reduce'
; conflicts, which are performance issues even in nondeterministic parsers.
; In particular, 'zero or more' items, like something that matches zero or more spaces,
; can decide whether it's complete or not based on whether a space is in the next input.

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
; follow: the follow set of this item = any terminal that can follow this item
; (depends on predicting items)
(defrecord Item [rule backlink match-count seed? follow]
  npda/IPrinting
  (npda/pstr [_] (str (if seed? "" "+ ") (rules/rule-str rule)
                      " : " (s/separate-str " " (map hexhash follow)))))

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

(defrecord ItemSet [items backlink-map])

(defn pstr-item-set-item [item backlink-map]
  (let [predictor-str (->> item (omm/get-vec backlink-map)
                        (map npda/pstr) (s/separate-str ", ") s/cutoff)]
    (str (npda/pstr item) (if (seq predictor-str) (str " | " predictor-str)))))

(defn pstr-item-set [{:keys [items backlink-map]}]
  (with-out-str
    (runmap println (map #(pstr-item-set-item % backlink-map) items))))

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

; Returns the result of an item set shift, or nil
(defn shift-item-set [items input-token]
  (seq (remove nil? (map #(scan-item % input-token) items))))

; continuations an item given a stack-top item-set
(defn advance-item-set [{backlink-map :backlink-map} backlink seed?]
  (seq (map advance-item (filter #(= seed? (:seed? %))
                                 (omm/get-vec backlink-map backlink)))))

; The returns (reduces) of some seed items
(defn returns [seed-items lookahead]
  (let [acceptor (if (= lookahead :clearley.npda/term)
                   ; Special handling. We don't want to surprise pass ::term
                   ; to someone's scanner
                   #(= % lookahead)
                   #(and (ifn? %) (% lookahead)))]
  (filter (fn [{:keys [rule follow]}]
            (and (rules/is-complete? rule)
                 (some identity (map acceptor follow))))
          seed-items)))
