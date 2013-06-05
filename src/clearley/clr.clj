(ns clearley.clr
  (require [clearley.rules :as rules]
           [uncore.collections.worm-ordered-multimap :as omm]
           [uncore.collections.worm-ordered-set :as os]
           [uncore.str :as str]
           [clojure.set :as set])
  (use uncore.core uncore.memo))
; Tools for CLR(1) grammars.

(defn mapm [f m]
  (into {} (map (fn [[k v]] (f k v)) m)))

(defn mapkeys [f m]
  (into {} (map (fn [[k v]] [(f k) v]) m)))

; TODO move to uncore
(defn mapvals [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

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
(defrecord Item [rule backlink seed? exit?])

(defnmem new-item [rule seed? exit?]
  (->Item rule nil seed? exit?))

(defnmem eager-advance [item prediction?]
  (if item
    (if-let [rule2 @(:eager-advance (:rule item))]
      (if-not (and prediction? (rules/is-complete? rule2))
        ; don't eager advance predicted items to completion
        (assoc item :rule rule2)))))

(defnmem eager-advances [item prediction?]
  (take-while identity (iterate #(eager-advance % prediction?) item)))

(defnmem predict [{:keys [rule seed?]}]
  (mapcat #(eager-advances % true)
          (for [prediction (rules/predict rule)]
            ; TODO map not for
            ; An item predicted by a seed item is an exit item.
            (new-item prediction false seed?))))

(defnmem advance [item]
  (assoc (update-all item {:rule (fn-> :advance deref)
                           :backlink #(if % % item)}) :seed? true))

(defnmem goal-item [goal grammar]
  (new-item (rules/goal-rule goal grammar) true true))

(defn item-str [{:keys [rule seed? exit?] :as item}]
  (str (if seed?
         ; We want nonseeds, and returns, to have a visual offset.
         (if exit? " ↩" "")
         (if exit? "+↩ " "+  ")) (rules/rule-str rule)))

(defn follow-str [[tag follow]]
  (case tag
    :token (pr-str follow)
    :scanner (pr-str follow)
    :item (str "<item" (item-str follow) ">")
    :term "$"))

(defn item-str-follow [item {follow-map :follow-map}]
  (let [follows (get follow-map item)
        follows-str (str/separate-str " " (map follow-str follows))]
    (str (item-str item) (if (seq follows-str) (str " : " follows-str) ""))))

; === Item sets ===

; TODO Item set format:
; 1. goal -> whatever
; 2. whatever -> whatever | called by 1, 3

; seeds: the seed items of an item set. an item's seeds completely determine
; the item set.
; more-seeds: all seed items inc. generated from eager advances of the original seeds.
; items: the items in the set, including seeds. we include one item for each
; item/follow combination and one 'general' item with no follow.
; backlink-map: maps items -> predicting items
; marks: maps items -> delays. we diabolically deref delays to 'mark' if an item
; is used or not. who says clojure is a functional language?
; at the end of item set construction, all unused items are tossed.
(defrecord ItemSet [seeds more-seeds items backlink-map follow-map])

(defn item-set-item-str [item item-set]
  (let [predictors (omm/get-vec (:backlink-map item-set) item)
        predictor-str (->> predictors
                        (map item-str) (str/separate-str ", ") #_str/cutoff)]
    (str (item-str-follow item item-set)
         (if (seq predictor-str) (str " | " predictor-str)))))

(defn item-set-str [item-set]
  (str/separate-str \newline (map #(item-set-item-str % item-set) (:items item-set))))

; ordered multimap scanner -> shift items
(defn shift-map [item-set]
  (reduce (fn [m {:keys [rule] :as item}]
            (if-let [s (rules/terminal rule)]
              (omm/assoc m s item)
              m))
    omm/empty (:items item-set)))

; ordered multimap of scanners -> return items
(defnmem reduce-map [item-set]
  (reduce (fn [themap {rule :rule :as seed}]
            (if (rules/is-complete? rule)
              (reduce #(omm/assoc % %2 seed)
                      themap (get-in item-set [:follow-map seed]))
              themap))
          omm/empty (:more-seeds item-set)))

(defnmem all-scanners [item-set]
  (into #{} (concat (-> item-set shift-map omm/keys)
                    (-> item-set reduce-map omm/keys))))

; seq of advanced seeds, given a backlink
; TODO do we need seed?
(defnmem advancables [{backlink-map :backlink-map} backlink seed?]
  (filter #(= seed? (:seed? %)) (omm/get-vec backlink-map backlink)))

(defnmem rule-size [rule]
  (- (-> rule :raw-rule :value count) (-> rule :null-results count)))

(defnmem item-set-size [item-set]
  (apply max (map-> (:more-seeds item-set) :rule rule-size)))

; TODO clean up some more of this shit
(defnmem can-shift? [item-set]
  (->> item-set :items (map :rule) (mapcat rules/predict) seq))

(defnmem returns [{:keys [more-seeds]}] (distinct (map :backlink more-seeds)))

; If there's a shift reduce conflict NOT accounting for lookahead.
(defnmem shift-reduce? [item-set]
  (and (can-shift? item-set) (seq (returns item-set))))

; If there's a reduce reduce conflict NOT accounting for lookahead.
(defnmem reduce-reduce? [item-set]
  (->> item-set returns (apply hash-set) count (< 1)))

; The item set's return if there's only one. Implies no lookahead. Note that
; reduce-map will be empty if a const-reduce exists!
(defnmem const-reduce [item-set]
  (let [my-returns (returns item-set)]
    (if-not (or (shift-reduce? item-set) (reduce-reduce? item-set))
      ; TODO fix
      (first (:more-seeds item-set)))))

(defn advancable-map [item-set seed?]
  (into {} (filter second (zipmap
                              (omm/keys (:backlink-map item-set))
                              (map #(seq (advancables item-set % seed?))
                                   (omm/keys (:backlink-map item-set)))))))

; === Building item sets

; Adds a follow set for a predictor to an item in an item-set.
; If the given item is one advance from completion (meaning some follow sets
; might depend on its own), transitively closes those follow sets by recursion.
(defn add-follow [{:keys [items backlink-map follow-map] :as item-set} item predictor
                  new?]
  (assert predictor) (assert item)
  (let [fset1 (get-in item-set [:follow-map item] #{})
        ffirst (-> predictor :rule :follow-first deref)
        ;_ (println (item-str item))
        ;_ (println (item-str predictor))
        ;_ (println (map follow-str fset1))
        ;_ (println (map follow-str ffirst))
        ; TODO integrate into rules/follow-first
        ; TODO require set
        ; Transitive close if we need
        ffirst (if (rules/null-result @(:advance (:rule predictor)))
                 (set/union ffirst (get-in item-set [:follow-map predictor]))
                 ffirst)
        ;_ (println (map follow-str ffirst))
        fset2 (set/union fset1 ffirst)
        item-set (assoc-in item-set [:follow-map item] fset2)]
    (if new?
      ; If this was a new item, there are no transitive follow sets to update.
      item-set
      ; Only do the transitive closure if A) we need to and
      ; B) the follow set was updated.
      (if (and (rules/null-result @(:advance (:rule item)))
               (not= fset1 fset2))
        (do
          ;(println "Recursing")
          (let [r (reduce #(add-follow % %2 predictor false) item-set (predict item))]
            ;(println "Done recursing")
            r))
        item-set))))

(defn predict-into-item-set [{:keys [items backlink-map follow-map] :as item-set}
                             item predictor]
  (assert predictor) (assert item)
  (when-not (rules/is-complete? item)
    (let [item-set (if (empty? (omm/get-vec backlink-map item))
                     (-> item-set
                       (update :items #(conj % item))
                       (add-follow item predictor true))
                     (add-follow item-set item predictor false))
          item-set (update item-set :backlink-map #(omm/assoc % item predictor))]
      item-set)))

; TODO make more elegant, perhaps parents store child item set numbers eagerly
(defn seeds-key [seeds follow-map]
  (into #{} (map #(assoc % :lane (-> follow-map (get %))) seeds)))

(defn populate-item-set [item-set]
  (loop [c item-set, dot 0]
    (if-let [s (get (:items c) dot)]
      (recur (reduce #(predict-into-item-set % %2 s)
                     c (predict s))
             (inc dot))
      c)))

(def clr-item-set
  (fcache
    seeds-key
    (fn [seeds parent-follow]
      (if (seq seeds)
        (let [more-seeds (mapcat #(eager-advances % false) seeds)
              follow-map (reduce (fn [m [item follow]]
                                   (reduce (fn [m aitem]
                                             (update m aitem #(set/union % follow)))
                                           m (eager-advances item false)))
                                 parent-follow parent-follow)]
          ;(println "pass0")
          ;(println (item-set-str
          ;           (->ItemSet seeds more-seeds (vec more-seeds) omm/empty nil)))
          (populate-item-set
            (->ItemSet seeds more-seeds (vec more-seeds) omm/empty follow-map)))))))

; === Building item sets: actually doing it

(defnmem item-set-pass1 [seeds follow-map]
  (update (clr-item-set seeds follow-map) :follow-map
          (fn [m]
            (mapvals (fn [v] (apply hash-set (map identity v)))
                     m))))

; Build the final follow map
(defn item-set-pass2 [item-set]
  ; TODO error here if deterministic? Or maybe helper fn to find
  ; conflicts that is invoked in quentin
  ;(println "========")
  ;(println "Building item set")
  ;(println (item-set-str (dissoc item-set :follow-map)))
  ;(println)
  (let [r item-set]
    ;(println)
    ;(println "Built")
    ;(println (item-set-str r))
    ;(println "========")
    r))

; Deduplication key for a fully built item set
; TODO not separate fn
(defnmem item-set-key [{:keys [items follow-map]}] [items follow-map])

; Used to prevent infinite recursion
(def ^:dynamic *crumbs* nil)

(declare build-item-set*)

; builds an item set map from a map backlink->seeds and parent follow-map
(defn item-set-map-helper [m follow-map]
  (into {} (map (fn [k]
                  (let [v (get m k)]
                    ; ugly and inefficient. Oh well
                    [k (build-item-set* (map advance v)
                                        (->> v
                                          (select-keys follow-map)
                                          (mapkeys advance)))]))
                (keys m))))

; Builds an item set, and as a side effect, all its children. Returns a ref.
; TODO this delay thing is a hack
(defn build-item-set* [seeds follow-map]
  (let [build-key (seeds-key seeds follow-map)]
    (cond (empty? seeds) (ref-box nil),
          (lookup ::item-set build-key)
          (lookup ::item-set build-key),
          ; Prevent infinite recursion.
          (get *crumbs* build-key)
          ; Look it up later (the original result will be saved).
          (delay @(lookup ::item-set build-key)),
          true
          (let [item-set (item-set-pass1 seeds follow-map)
                follow-map (:follow-map item-set)]
            (binding [*crumbs* (assoc *crumbs* build-key item-set)]
              ; Eagerly build child item sets
              (let [item-set
                    (merge
                      item-set
                      {:shift-map
                       (item-set-map-helper (mapvals os/vec (shift-map item-set))
                                            follow-map),
                       :shift-advances
                       (item-set-map-helper (advancable-map item-set false)
                                            follow-map),
                       :continue-advances
                       (item-set-map-helper (advancable-map item-set true)
                                            follow-map)}),
                    ; Eliminate unused follow information
                    item-set (item-set-pass2 item-set)
                    ; Dedup and capture the deduped value
                    ; Also get stuff for progress reporting
                    prev-save-count (save-count ::dedup-item-set)
                    item-set (save-or-get! ::dedup-item-set
                                           (item-set-key item-set) item-set)
                    curr-save-count (save-count ::dedup-item-set)]
                (when (and (not (= prev-save-count curr-save-count))
                           (zero? (mod curr-save-count 100)))
                  (println (save-count ::dedup-item-set) "item sets built"))
                (save! ::item-set build-key (ref-box item-set))))))))

; TODO progress reporting optional
(defn build-item-sets [goal grammar]
  (println "Building item sets...")
  (binding [*crumbs* {}]
    (let [g (goal-item goal grammar)
          r @(build-item-set* [g] {g #{[:term nil]}})]
      (println (save-count ::dedup-item-set) "item sets built")
      r)))
