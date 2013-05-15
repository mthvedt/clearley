(ns clearley.clr
  ; TODO eliminate npda dependency
  (require [clearley.npda :as npda]
           [clearley.rules :as rules]
           [uncore.collections.worm-ordered-multimap :as omm]
           [uncore.collections.worm-ordered-set :as os]
           [uncore.str :as s])
  (use uncore.core uncore.memo))

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

; === Item record ===
; name: an object representing the clause that predicted this item
; should have a short str representation
; rule: the rule for this item
; backlink: the rule from the original item set, before shifts
; (but including initial eager advances)
; match-count: the number of times this rule has been scanned or advanced
; follow: a terminal that can follow this item
; (depends on predicting items)
(defrecord Item [rule backlink match-count seed? follow])

(defn item-str [{:keys [rule seed? follow] :as item}]
  (str (if seed? "" "+") " " (rules/rule-str rule)))

(defn item-str-follow [{follow :follow :as item}]
  (str (item-str item) (if follow (str " : " (hexhash follow)) "")))

(defnmem new-item [rule seed? follow]
  (->Item rule nil 0 seed? follow))

(defnmem eager-advance [item prediction?]
  (if item
    (if-let [rule2 (rules/eager-advance (:rule item))]
      (if (and prediction? (rules/is-complete? rule2))
        nil ; don't eager advance predicted items to completion
        (assoc item :rule rule2)))))

(defnmem eager-advances [item prediction?]
  (take-while identity (iterate #(eager-advance % prediction?) item)))

(defnmem predict-item [{:keys [rule follow]}]
  (mapcat #(eager-advances % true)
          (for [prediction (remove fn? (rules/predict rule))
                follow-terminal (rules/follow-first rule follow)]
            (new-item prediction false follow-terminal))))

(defnmem advance-item [item]
  (assoc (update-all item {:rule rules/advance, :match-count inc,
                           :backlink #(if % % item)})
         :seed? true))

(defnmem scan-item [item input-token]
  (if (some #(% input-token)
            (filter fn? (rules/predict (:rule item))))
    (advance-item item)))

(defn unfollow [item]
  (let [item (assoc item :follow nil)
        item (if (:backlink item)
               (update item :backlink #(assoc % :follow nil))
               item)]
    item))

(defn goal-item [goal grammar]
  (new-item (rules/goal-rule goal grammar) true ::term))

; === Item sets ===

; items: the items in the set
; backlink-map: maps rules -> {:advance, :continue}
; TODO: while building, just make a backlink map.
; gotos: map rule -> item set kernels.
; actinos: map lookahead -> shift (item set kernel) or reduce (item).
; we should eliminate code that peers into back-link-map, seeds, &c.
(defrecord ItemSet [seeds more-seeds items backlink-map])

(defn item-set-item-str [item backlink-map]
  (let [predictors (concat ;(omm/get-vec backlink-map item) TODO
                           (omm/get-vec backlink-map (unfollow item)))
        predictor-str (->> predictors
                        (map item-str) (s/separate-str ", ") s/cutoff)]
    (str (item-str-follow item) (if (seq predictor-str) (str " | " predictor-str)))))

(defn item-set-str [{:keys [items backlink-map]}]
  (with-out-str
    (runmap println (map #(item-set-item-str % backlink-map) items))))

(defn predict-into-item-set [{:keys [items backlink-map] :as item-set}
                             {backlink :backlink :as item} predictor]
  (assert (not backlink))
  (let [item-set (if (empty? (omm/get-vec backlink-map item))
                   (update item-set :items #(conj % item))
                   item-set)
        item-set (update item-set :backlink-map
                         (fn->
                           (omm/assoc item predictor)
                           (omm/assoc (unfollow item) predictor)))]
    item-set))

; TODO Item set format:
; 1. goal -> whatever
; 2. whatever -> whatever | called by 1, 3

(defn term-scanner [x] (= x ::term))

; ordered multimap of scanners -> shift items
(defnmem shift-map [item-set]
  (reduce
    (fn [themap {:keys [rule] :as item}]
      ; TODO rule scanners instead of filter fn?
      (let [shift-fns (filter fn? (rules/predict rule))]
        (reduce #(omm/assoc % %2 item) themap shift-fns)))
    omm/empty (:items item-set)))

; ordered multimap of scanners -> return items
(defnmem reduce-map [item-set]
  (reduce (fn [themap {:keys [rule follow] :as seed}]
            (if (rules/is-complete? rule)
              (omm/assoc themap follow seed)
              themap))
          omm/empty (:more-seeds item-set)))

(defnmem all-scanners [item-set]
  (disj (into #{} (concat (-> item-set shift-map omm/keys)
                          (-> item-set reduce-map omm/keys)))
        :clearley.clr/term))

(declare pep-item-set)
; TODO nix superfluous stuff

(defnmem advances [{backlink-map :backlink-map} backlink seed?]
  (map advance-item (filter #(= seed? (:seed? %))
                                (omm/get-vec backlink-map backlink))))

(defnmem rule-size [rule]
  (- (-> rule :raw-rule :value count) (-> rule :null-results count)))

(defnmem item-set-size [item-set]
  (apply max (map-> (concat (:seeds item-set) (:more-seeds item-set))
                    :rule rule-size)))

; TODO simplify
(defnmem can-shift? [item-set]
  (->> item-set :items (map :rule) (mapcat rules/predict) (filter fn?) seq))

(defnmem reduces [item-set]
  (->> item-set :more-seeds (filter (fn-> :rule rules/is-complete?))))

(defnmem reduce-rules [item-set]
  (->> item-set reduces (map :rule) (apply hash-set)))

(defnmem shift-reduce? [item-set]
  (and (can-shift? item-set) (seq (reduces item-set))))

(defnmem reduce-reduce? [item-set]
  (> (count (reduce-rules item-set)) 1))

(defnmem full-single-reduce [item-set]
  (if (and (:single-reduce item-set) (= (count (reduces item-set)) 1))
    (first (reduces item-set))))

(defnmem goto-map [item-set]
  (zipmap
    (omm/keys (:backlink-map item-set))
    (map (fn [backlink]
           (into {}
                 (filter (comp seq val)
                         {:advance (advances item-set backlink false)
                          :continue (advances item-set backlink true)})))
         (omm/keys (:backlink-map item-set)))))

(defnmem split-conflicts [item-set]
  (into #{} (map-> (filter (fn-> val count (> 1)) (goto-map item-set)) key)))

; Gets the next item set for some backlink
(defnmem advance-item-set [{backlink-map :backlink-map :as item-set} backlink seed?]
  (pep-item-set (advances item-set backlink seed?)
                (split-conflicts item-set)))

; TODO rename full-item-set
; TODO move single-reduce to :single-reduce-pass1
(defn item-set-pass1 [seeds]
  (if (seq seeds)
    (let [more-seeds (mapcat #(eager-advances % false) seeds)]
      (loop [c (->ItemSet seeds more-seeds (vec more-seeds) omm/empty), dot 0]
        (if-let [s (get (:items c) dot)]
          (recur (reduce #(predict-into-item-set % %2 s)
                         c (predict-item s))
                 (inc dot))
          (let [single-reduce (if (or (can-shift? c) (reduce-reduce? c))
                                nil
                                (-> c reduces first unfollow))]
            (assoc c :single-reduce single-reduce)))))))

; Returns the seed items for all child item sets of this one
(defnmem all-children [item-set]
  (let [r (concat (->> item-set shift-map omm/vals (map os/vec))
                  (->> item-set goto-map vals (mapcat vals)))]
    ;(dorun (map println (map #(map item-str %) r)))
    r))

(defnmem child-item-sets [item-set]
  (map #(item-set-pass1 % (split-conflicts item-set))
       (all-children item-set)))

; Removes the lookahead from any item not :marked
(defnmem filter-items [item-set]
  (let [the-filter (fn [items]
                     (distinct (map
                                 (fn [item]
                                   (if (:marked item)
                                     (do (println "marked") item)
                                     (unfollow item)))
                                 items)))]
    (update-all item-set {:seeds the-filter
                          :more-seeds the-filter
                          :items the-filter})))

; Nocollapses: the item set for which we cannot collapse return values.
; (This is to avoid state-split conflicts, where both a seed and a non-seed
; are advanced in a calling item set.)
; TODO kill 'build'
(defn build-item-set-pass2 [item-set nocollapses]
  (if item-set
    ;(println "pass2")
    ;(println (item-set-str item-set-pass1))
    ;(prn (map item-str-follow nocollapses))
    ;(println (when-let [x (:single-reduce item-set-pass1)] (item-str-follow x)))
    (let [item-set (if (nocollapses (:backlink (:single-reduce item-set)))
                     (do
                       (assoc item-set :single-reduce nil))
                     item-set)
          children (child-item-sets item-set)]
      ; Item set build phase 2
      ; TODO collapse item sets as much as possible
      (if (:single-reduce item-set)
        ; If this is a single reduce item, kill all lookahead!
        (do
          (println (item-set-str (filter-items item-set)))
          (filter-items item-set))
        item-set))))

; For some tagged vals of the form [tag val], returns vals matching the tag
; TODO does this belong here?
(defn untag [tagged-vals tag]
  (for [[tag1 val] tagged-vals :when (= tag1 tag)] val))

; Some key not dependent on order. Any item set with the same items is the same set
(defnmem item-set-key [item-set] [(into #{} (:items item-set))
                                  (:single-reduce item-set)])

; Closes an item set, adding eager advances
; Because item-sets don't support =, this should be the single point of creation
; for all item sets.
(def pep-item-set
  (let [pass1 (fcache #(into #{} %) item-set-pass1)]
    (fmem #(build-item-set-pass2 (pass1 %) %2))))
