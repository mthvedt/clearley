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

(defn item-str [{:keys [rule seed?] :as item}]
  (str (if seed? "" "+ ") (rules/rule-str rule)))

; TODO better follow strs
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
          (for [prediction (rules/predict rule)
                follow-terminal (rules/follow-first rule follow)]
            (new-item prediction false follow-terminal))))

(defnmem advance-item [item]
  (assoc (update-all item {:rule rules/advance, :match-count inc,
                           :backlink #(if % % item)})
         :seed? true))

(defn scan-item [item input-token]
  (if (some #(% input-token) (rules/scanner (:rule item)))
    (advance-item item)))

(defn unfollow [item]
  (let [item (assoc item :follow nil)
        item (if (:backlink item)
               (update item :backlink #(assoc % :follow nil))
               item)]
    item))

(defn goal-item [goal grammar]
  (new-item (rules/goal-rule goal grammar) true [:term nil]))

; === Item sets ===

; items: the items in the set
; backlink-map: maps items -> predicting items
; gotos: map rule -> item set kernels.
; actinos: map lookahead -> shift (item set kernel) or reduce (item).
; we should eliminate code that peers into back-link-map, seeds, &c.
(defrecord ItemSet [seeds more-seeds items backlink-map])

(defn item-set-item-str [item backlink-map]
  (let [predictors (concat (omm/get-vec backlink-map item); TODO into ordered set
                           (omm/get-vec backlink-map (unfollow item)))
        predictor-str (->> predictors
                        (map item-str) (s/separate-str ", ") s/cutoff)]
    (str (item-str-follow item) (if (seq predictor-str) (str " | " predictor-str)))))

(defn item-set-str [{:keys [items backlink-map]}]
  (with-out-str
    (runmap println (map #(item-set-item-str % backlink-map) items))))

(defn predict-into-item-set [{:keys [items backlink-map] :as item-set}
                             {backlink :backlink :as item} predictor]
  (assert predictor) (assert (not backlink))
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

; ordered multimap of scanners -> shift items
(defnmem shift-map [item-set]
  (reduce (fn [m {:keys [rule] :as item}]
            (if-let [s (rules/scanner rule)]
              (omm/assoc m s (advance-item item))
              m))
    omm/empty (:items item-set)))

; ordered multimap of scanners -> return items
(defnmem reduce-map [item-set]
  (reduce (fn [themap {:keys [rule follow] :as seed}]
            (if (rules/is-complete? rule)
              (omm/assoc themap follow seed)
              themap))
          omm/empty (:more-seeds item-set)))

(defnmem all-scanners [item-set]
  (into #{} (concat (-> item-set shift-map omm/keys)
                    (-> item-set reduce-map omm/keys))))

(defnmem advances [{backlink-map :backlink-map} backlink seed?]
  (map advance-item (filter #(= seed? (:seed? %))
                            (omm/get-vec backlink-map backlink))))

(defnmem rule-size [rule]
  (- (-> rule :raw-rule :value count) (-> rule :null-results count)))

(defnmem item-set-size [item-set]
  (apply max (map-> (concat (:seeds item-set) (:more-seeds item-set))
                    :rule rule-size)))

(defnmem can-shift? [item-set]
  (->> item-set :items (map :rule) (mapcat rules/predict) seq))

; All possible reduces for this item-set
(defnmem raw-reduces [item-set]
  (->> item-set :more-seeds (filter (fn-> :rule rules/is-complete?))))

(defnmem reduce-rules [item-set]
  (->> item-set raw-reduces (map :rule) (apply hash-set)))

; If there's a shift reduce conflict NOT accounting for lookahead.
(defnmem shift-reduce? [item-set]
  (and (can-shift? item-set) (seq (raw-reduces item-set))))

; If there's a reduce reduce conflict NOT accounting for lookahead.
(defnmem reduce-reduce? [item-set]
  (> (count (reduce-rules item-set)) 1))

; Returns a const-reduce with follow if it exists
#_(defnmem const-reduce-follow [item-set]
  (if (and (:const-reduce item-set) (= (count (raw-reduces item-set)) 1))
    (first (raw-reduces item-set))))

; All possible reduces for this item-set, or the single reduce (in a seq)
(defnmem reduces [item-set]
  (if-let [r (:const-reduce item-set)]
    [r]
    (raw-reduces item-set)))

(defnmem goto-map [item-set]
  (zipmap
    (omm/keys (:backlink-map item-set))
    (map (fn [backlink]
           (into {}
                 (filter (comp seq val)
                         {:advance (advances item-set backlink false)
                          :continue (advances item-set backlink true)})))
         (omm/keys (:backlink-map item-set)))))

(defnmem shifts [item-set]
  (->> item-set shift-map omm/vals (map os/vec)))

(defnmem shift-advances [item-set]
  (->> item-set goto-map vals (map :advance) (remove nil?)))

(defnmem continues [item-set deterministic?]
  (if deterministic?
    (->> item-set goto-map vals (filter #(= (count %) 1))
      (map :continue) (remove nil?))
    (assert false) ; not yet implemented
    ))

; Returns all backlinks for which there is a state split conflict
(defnmem split-conflicts [item-set]
  (into #{} (map key (filter (fn-> val count (> 1)) (goto-map item-set)))))

(defn item-set-pass0 [seeds]
  (if (seq seeds)
    (let [more-seeds (mapcat #(eager-advances % false) seeds)]
      (loop [c (->ItemSet seeds more-seeds (vec more-seeds) omm/empty), dot 0]
        (if-let [s (get (:items c) dot)]
          (recur (reduce #(predict-into-item-set % %2 s)
                         c (predict-item s))
                 (inc dot))
          (let [const-reduce (if (or (can-shift? c) (reduce-reduce? c))
                                nil
                                (-> c raw-reduces first unfollow))]
            (-> c
              (assoc :const-reduce const-reduce)
              ; Save the split-conflicts--they might be removed form the
              ; backlink map, but we still need to remember them
              (assoc :split-conflicts (split-conflicts c)))))))))

; === Building item sets: second pass

; Removes the lookahead from any item not in filter-set
(defn filter-items [item-set filter-set]
  (let [the-filter (fn [items]
                     (distinct (map
                                 (fn [item]
                                   (if (filter-set item)
                                     (do (println "marked") item)
                                     (unfollow item)))
                                 items)))]
    (update-all item-set {:seeds the-filter
                          :more-seeds the-filter
                          :items the-filter})))

; Removes everything from backlink map not present in rvals
; TODO this might be obsolete once advancer loops are fixed
(defn filter-backlinks [item-set rvals]
  (update item-set :backlink-map
          (fn [themap]
            (reduce #(if (rvals %2) % (omm/dissoc % %2))
                    themap (omm/keys themap)))))

; === Building item sets: actually doing it

; Figures out the return values for an item set and all its continuations.
; This works in pass2 because continuations are guaranteed to not be recursive.
(defnmem item-set-pass1 [seeds keep-backlinks]
  (if (seq seeds)
    (let [item-set (item-set-pass0 seeds)
          item-set (if (keep-backlinks (:backlink (:const-reduce item-set)))
                     (assoc item-set :const-reduce nil)
                     item-set)
          conflicts (:split-conflicts item-set)
          my-continues (map #(item-set-pass1 % conflicts)
                             (continues item-set true))
          descendant-deep-reduces (into #{} (mapcat :deep-reduces my-continues))
          deep-reduces (into descendant-deep-reduces
                             (map :backlink (reduces item-set)))
          item-set (assoc item-set :deep-reduces deep-reduces)]
      item-set)))

; Builds a pass2, given pass1 children and pass2 non-recurisve children.
(defnmem item-set-pass2 [item-set children]
  (if item-set
    (let [child-deep-reduces (into #{} (mapcat :deep-reduces children))]
      (if (:const-reduce item-set)
        ; If we don't need lookahead for this item, kill all of it!
        (filter-items item-set #{})
        (filter-backlinks item-set child-deep-reduces)))))

; Some key not dependent on order. Any item set with the same items is the same set
(defnmem item-set-key [item-set] [(into #{} (:items item-set))
                                  (:const-reduce item-set)])

; Prevent infinite recursion. When building an item set, pass2 needs to know
; of the pass2 of its recursive children but only the pass1 of itself if recursive
(def ^:dynamic *crumbs* nil)

; Builds an item set, and as a side effect, all its children.
(defn build-item-set* [seeds keep-backlinks]
  (cond (empty? seeds) nil,
        (lookup ::item-set [seeds keep-backlinks])
        (lookup ::item-set [seeds keep-backlinks]),
        ; If item-set is being built somewhere on the stack, Return the pass1 version.
        (get @*crumbs* [seeds keep-backlinks])
        (get @*crumbs* [seeds keep-backlinks]),
        true
        (let [item-set (item-set-pass1 seeds keep-backlinks)
              ; Save item-set pass 1 in crumbs
              _ (swap! *crumbs* assoc [seeds keep-backlinks] item-set)
              _ (if (zero? (mod (count @*crumbs*) 100))
                  (println (count @*crumbs*) "candidate item sets examined"))
              shift-children (map #(build-item-set* % (:split-conflicts item-set))
                                   (concat (shifts item-set)
                                           (shift-advances item-set)))
              ; Just runmap these for now
              my-continues (runmap #(build-item-set* % (:split-conflicts item-set))
                                   (continues item-set true))
              prev-save-count (save-count ::dedup-item-set)
              item-set (item-set-pass2 item-set shift-children)
              ; Dedup and capture the deduped value
              item-set (save-or-get! ::dedup-item-set
                                     (item-set-key item-set) item-set)
              curr-save-count (save-count ::dedup-item-set)]
          (when (and (not (= prev-save-count curr-save-count))
                     (zero? (mod curr-save-count 100)))
            (println (save-count ::dedup-item-set) "item sets built"))
          (save! ::item-set [seeds keep-backlinks] item-set))))

(defn pep-item-set [seeds keep-backlinks]
  (binding [*crumbs* (atom {})]
    (build-item-set* seeds keep-backlinks)))

; Gets the next item set for some backlink
(defnmem advance-item-set [item-set backlink seed?]
  (pep-item-set (advances item-set backlink seed?)
                (:split-conflicts item-set)))
