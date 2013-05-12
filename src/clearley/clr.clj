(ns clearley.clr
  ; TODO eliminate npda dependency
  (require [clearley.npda :as npda]
           [clearley.rules :as rules]
           [uncore.collections.worm-ordered-multimap :as omm]
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

; TODO s/backlink/oroginal

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

(defn goal-item [goal grammar]
  (new-item (rules/goal-rule goal grammar) true ::term))

; === Item sets ===

; items: the items in the set
; backlink-map: maps rules -> {:advance, :continue}
; TODO: while building, just make a backlink map.
; What do we absolutely NEED in an item-set? what is the key?
; gotos: map rule -> item set kernels.
; actinos: map lookahead -> shift (item set kernel) or reduce (item).
; we should eliminate code that peers into back-link-map, seeds, &c.
(defrecord ItemSet [seeds more-seeds items backlink-map])

(defn item-set-item-str [item backlink-map]
  (let [predictors (concat ;(omm/get-vec backlink-map item) TODO
                           (omm/get-vec backlink-map (assoc item :follow nil)))
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
                           (omm/assoc (assoc item :follow nil) predictor)))]
    item-set))

; TODO Item set format:
; 1. goal -> whatever
; 2. whatever -> whatever | called by 1, 3

(defn term-scanner [x] (= x ::term))

; Returns a map. Keys are the next token. Values are one of
; [:shift shifting-item] or [:return item-to-return].
(defn action-map [seeds items]
  (let [rmap (reduce
               (fn [themap {:keys [rule] :as item}]
                 (let [shift-fns (filter fn? (rules/predict rule))]
                   (reduce #(omm/assoc % %2 [:shift item]) themap shift-fns)))
        omm/empty items)]
    (reduce (fn [themap {:keys [rule follow] :as seed}]
              (if (rules/is-complete? rule)
                (omm/assoc themap follow [:reduce seed])
                themap))
            rmap seeds)))

(declare pep-item-set)

(defnmem advances [{backlink-map :backlink-map} backlink seed?]
  (map advance-item (filter #(= seed? (:seed? %))
                                (omm/get-vec backlink-map backlink))))

; Gets the next item set for some backlink
(defnmem advance-item-set [{backlink-map :backlink-map :as item-set} backlink seed?]
  (pep-item-set (advances item-set backlink seed?)
                (:split-conflicts item-set)))

(defn build-item-set-pass1 [seeds more-seeds]
  (loop [c (->ItemSet seeds more-seeds (vec more-seeds) omm/empty), dot 0]
    (if-let [s (get (:items c) dot)]
      (recur (reduce #(predict-into-item-set % %2 s)
                     c (predict-item s))
             (inc dot))
      ; Get all initial actions--shift and reduce
      (let [actions (action-map more-seeds (:items c))
            ; Search for shift-reduce and reduce-reduce conflicts
            can-shift? (->> c :items (map :rule) (mapcat rules/predict) (filter fn?)
                         seq)
            ; All the possible reduce values.
            reduces (->> c :more-seeds (filter (fn-> :rule rules/is-complete?)))
            reduce-rules (->> reduces (map :rule) (apply hash-set))
            ; Are there conflicts in this item set?
            shift-reduce? (and can-shift? (seq reduces))
            reduce-reduce? (> (count reduce-rules) 1)
            ; If not null, item set exists only to return a single value.
            ; TODO should returns be backlinks?
            single-reduce (if (or can-shift? reduce-reduce?) nil
                            (-> (first reduces)
                              (assoc :follow nil)
                              (update :backlink #(assoc % :follow nil))))
            gotos (zipmap
                    (omm/keys (:backlink-map c))
                    (map (fn [backlink]
                           (into {}
                                 (filter (comp seq val)
                                         {:advance (advances c backlink false)
                                          :continue (advances c backlink true)})))
                         (omm/keys (:backlink-map c))))
            ; This gives us all items for which there is a state split conflict.
            split-conflicts (into #{} (map key (filter (fn-> val count (> 1)) gotos)))
            c (merge c {:actions actions
                        :shift-reduce? shift-reduce? :reduce-reduce? reduce-reduce?
                        :gotos gotos :single-reduce single-reduce
                        :split-conflicts split-conflicts})]
        c))))

; Nocollapses: the item set for which we cannot collapse return values.
; (This is to avoid state-split conflicts, where both a seed and a non-seed
; are advanced in a calling item set.)
; TODO plug in
(defn build-item-set-pass2 [item-set-pass1 nocollapses]
  (if (nocollapses (:single-reduce item-set-pass1))
    (assoc item-set-pass1 :single-reduce nil)
    item-set-pass1))

; TODO: ADD accepting a single return.
; TODO That's godo enough for now... LATER we can add return value detection
; to remove items.
;
; TODO: what this will look like: the ability to mark items as needed or unneeded.
; This will have two step processing: an item set first sets up basic stuff,
; then sets up its children, gets their return values, and finalizes.
; So there are two phases: phase 1 has the full CLR(1) item set, phase 2
; reduces the item set.

; For some tagged vals of the form [tag val], returns vals matching the tag
(defn untag [tagged-vals tag]
  (for [[tag1 val] tagged-vals :when (= tag1 tag)] val))

; Closes an item set, adding eager advances
; Because item-sets don't support =, this should be the single point of creation
; for all item sets.
(def pep-item-set
  (let [pass1 (fcache #(into #{} %)
                      (fn [seeds]
                        (if (seq seeds)
                          (let [more-seeds (mapcat #(eager-advances % false) seeds)]
                            (build-item-set-pass1 seeds more-seeds)))))]
    (fmem #(build-item-set-pass2 (pass1 %) %2))))

; Some key not dependent on order. Any item set with the same seeds is the same set
; TODO Need to cache, unify based on seeds w/o initial item no.?
; TODO do we need this?
(defn item-set-key [item-set] (into #{} (:seeds item-set)))

; === Item set generation

; Step: All actions in item set.
; Actions are: lookahead shift|reduce, rule continue|advance.
; Format: lookahead -> shift [item-set-seeds], reduce [rule1 rule2 rule3].
; Step: Collapsible item sets. Only need reduce collapses. Being able to map
; a set of seeds to a unified collapsed set.
; Step: Parent collapsing item sets. Children before parent in breadcrumb fashion.

; Returns a coll of all item sets reachable from seed
#_(defn generate-all-item-sets [seeds]
  (loop [work-stack [seed] r #{}]
    (if-let [i (peek work-stack)]
      (let [seeds (map #(if (:seed-num %) % (assoc % :seed-num %2)) seeds (range))
            more-seeds (mapcat #(eager-advances % false) seeds)
            item-set (closed-item-set more-seeds)]

      (r)))))
