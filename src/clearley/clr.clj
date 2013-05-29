(ns clearley.clr
  (require [clearley.rules :as rules]
           [uncore.collections.worm-ordered-multimap :as omm]
           [uncore.collections.worm-ordered-set :as os]
           [uncore.str :as s])
  (use uncore.core uncore.memo))

; Tools for CLR(1) grammars.

; === Item record ===
; name: an object representing the clause that predicted this item
; should have a short str representation
; rule: the rule for this item
; seed? is this a seed item?
; exit? is this an exit item?
; (For mutual-automaton parsing models, certain items must be 'exit items'
; that cause automaton states to return. These might collide with non exit items.
; The current impl generates items for an Earley automaton.)
; backlink: the rule from the original item set, before shifts
; (but including initial eager advances)
; follow: a terminal that can follow this item
; (depends on predicting items)
(defrecord Item [rule backlink seed? exit? follow])

(defn follow-str [[tag follow]]
  (case tag
    :token (pr-str follow)
    :scanner (pr-str follow)
    :term "$"))

(defn item-str [{:keys [rule seed? exit?] :as item}]
  (str (if seed?
         ; We want nonseeds, and returns, to have a visual offset.
        (if exit? " ↩" "")
        (if exit? "+↩ " "+  ")) (rules/rule-str rule)))

(defn item-str-follow [{follow :follow :as item}]
  (str (item-str item) (if follow (str " : " (follow-str follow)) "")))

(defnmem new-item [rule seed? exit? follow]
  (->Item rule nil seed? exit? follow))

(defnmem eager-advance [item prediction?]
  (if item
    (if-let [rule2 @(:eager-advance (:rule item))]
      (if-not (and prediction? (rules/is-complete? rule2))
        ; don't eager advance predicted items to completion
        (assoc item :rule rule2)))))

(defnmem eager-advances [item prediction?]
  (take-while identity (iterate #(eager-advance % prediction?) item)))

(defnmem predict [{:keys [rule seed? follow]}]
  (mapcat #(eager-advances % true)
          (for [prediction (rules/predict rule)
                follow-terminal (rules/follow-first rule follow)]
            ; An item predicted by a seed item is an exit item.
            (new-item prediction false seed? follow-terminal))))

(defn unfollow [item] (assoc item :follow nil))

(defnmem advance [item]
  (assoc (update-all item {:rule (fn-> :advance deref)
                           :backlink #(if % % (unfollow item))}) :seed? true))

(defnmem goal-item [goal grammar]
  (new-item (rules/goal-rule goal grammar) true true [:term nil]))

; === Item sets ===

; TODO Item set format:
; 1. goal -> whatever
; 2. whatever -> whatever | called by 1, 3
; items: the items in the set
; backlink-map: maps items -> predicting items
; gotos: map rule -> item set kernels.
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

; ordered multimap of scanners -> shift items
(defnmem shift-map [item-set]
  (reduce (fn [m {:keys [rule] :as item}]
            (if-let [s (rules/terminal rule)]
              (omm/assoc m s (advance item))
              m))
    omm/empty (:items item-set)))

; ordered multimap of scanners -> return items
(defnmem reduce-map [item-set]
  (reduce (fn [themap {:keys [rule follow] :as seed}]
            (if (rules/is-complete? rule)
              (omm/assoc themap follow (unfollow seed))
              themap))
          omm/empty (:more-seeds item-set)))

(defnmem all-scanners [item-set]
  (into #{} (concat (-> item-set shift-map omm/keys)
                    (-> item-set reduce-map omm/keys))))

(defnmem advances [{backlink-map :backlink-map} backlink seed?]
  (map advance (filter #(= seed? (:seed? %))
                            (omm/get-vec backlink-map backlink))))

(defnmem rule-size [rule]
  (- (-> rule :raw-rule :value count) (-> rule :null-results count)))

(defnmem item-set-size [item-set]
  (apply max (map-> (concat (:seeds item-set) (:more-seeds item-set))
                    :rule rule-size)))

(defnmem can-shift? [item-set]
  (->> item-set :items (map :rule) (mapcat rules/predict) seq))

; All possible reduces for this item-set
(defnmem reduces [item-set]
  (->> item-set :more-seeds (filter (fn-> :rule rules/is-complete?))))

; If there's a shift reduce conflict NOT accounting for lookahead.
(defnmem shift-reduce? [item-set]
  (and (can-shift? item-set) (seq (reduces item-set))))

; If there's a reduce reduce conflict NOT accounting for lookahead.
(defnmem reduce-reduce? [item-set]
  (->> item-set reduces (map unfollow) (apply hash-set) count (< 1)))

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

(def item-set-pass0
  (fcache
    (fn [seeds] (into #{} seeds))
    (fn [seeds]
      (if (seq seeds)
        (let [more-seeds (mapcat #(eager-advances % false) seeds)]
          (loop [c (->ItemSet seeds more-seeds (vec more-seeds) omm/empty), dot 0]
            (if-let [s (get (:items c) dot)]
              (recur (reduce #(predict-into-item-set % %2 s)
                             c (predict s))
                     (inc dot))
                c)))))))

; === Building item sets: actually doing it

; TODO don't need to save deep reduces, can defnmem it
; Figures out the return values for an item set and all its continuations.
; This works in pass2 because continuations are guaranteed to not be recursive.
(defnmem item-set-pass1 [seeds]
  (if (seq seeds)
    (let [item-set (item-set-pass0 seeds)
          my-continues (map item-set-pass1 (continues item-set true))
          descendant-deep-reduces (into #{} (mapcat :deep-reduces my-continues))
          deep-reduces (into descendant-deep-reduces
                             (map :backlink (reduces item-set)))
          item-set (assoc item-set :deep-reduces deep-reduces)]
      item-set)))

; Some key not dependent on order. Item set is fully determined by items
(defnmem item-set-key [item-set] [(into #{} (:items item-set))])

; Prevent infinite recursion. When building an item set, pass2 needs to know
; of the pass2 of its recursive children but only the pass1 of itself if recursive
(def ^:dynamic *crumbs* nil)

; Builds an item set, and as a side effect, all its children.
(defn build-item-set* [seeds]
  (let [build-key (apply hash-set seeds)]
    (cond (empty? seeds) nil,
          (lookup ::item-set build-key)
          (lookup ::item-set build-key),
          ; Indicates a parent of this thread has this item set on the stack.
          (get *crumbs* build-key)
          (get *crumbs* build-key),
          true
          (let [item-set (item-set-pass1 seeds)]
            (binding [*crumbs* (assoc *crumbs* build-key item-set)]
              ; Eagerly build child item sets
              (runmap build-item-set* (concat (shifts item-set)
                                              (shift-advances item-set)))
              (runmap build-item-set* (continues item-set true))
              ; Dedup and capture the deduped value
              ; Also get stuff for progress reporting
              (let [prev-save-count (save-count ::dedup-item-set)
                    item-set (save-or-get! ::dedup-item-set
                                           (item-set-key item-set) item-set)
                    curr-save-count (save-count ::dedup-item-set)]
                (when (and (not (= prev-save-count curr-save-count))
                           (zero? (mod curr-save-count 100)))
                  (println (save-count ::dedup-item-set) "item sets built"))
                (save! ::item-set build-key item-set)))))))

(defn pep-item-set [seeds]
  (binding [*crumbs* {}]
    (build-item-set* seeds)))

; TODO progress reporting optional
(defn build-item-sets [goal grammar]
  (println "Building item sets...")
  (pep-item-set [(goal-item goal grammar)])
  (println (save-count ::dedup-item-set) "item sets built"))

; Gets the next item set for some backlink
(defnmem advance-item-set [item-set backlink seed?]
  (pep-item-set (advances item-set backlink seed?)))
