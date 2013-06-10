(ns clearley.clr
  ; Tools for CLR(1) grammars.
  (require [clearley.rules :as rules]
           [uncore.collections.worm-ordered-multimap :as omm]
           [uncore.collections.worm-ordered-set :as os]
           [uncore.str :as str]
           [clojure.set :as set])
  (use uncore.core uncore.memo uncore.ref))

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
          ; An item predicted by a seed item is an exit item.
          ; Note that if exit? is always false, we have a standard CLR parser.
          (map #(new-item % false seed?) (rules/predict rule))))

(defnmem advance [item]
  (assoc (update-all item {:rule (fn-> :advance deref)
                           :backlink #(if % % item)}) :seed? true))

(defnmem goal-item [goal grammar]
  (new-item (rules/goal-rule goal grammar) true true))

(defn item-str [{:keys [rule seed? exit?] :as item}]
  (str (if seed?
         ; Some visual offsetting is done also
         (if exit? " ↩" "")
         (if exit? "+↩ " "+  ")) (rules/rule-str rule)))

(defn follow-str [[tag follow]]
  (case tag
    :token (pr-str follow)
    :scanner (pr-str follow)
    :item (str "<item " (item-str follow) " >")
    :term "$"))

; An item-str with follow information attached
(defn item-str-follow [item {follow-map :follow-map}]
  (let [follows (get follow-map item)
        follows-str (str/separate-str " " (map follow-str follows))]
    (str (item-str item) (if (seq follows-str) (str " : " follows-str) ""))))

; === Item sets ===

; seeds: the seed items of an item set. an item's seeds completely determine
; the item set.
; more-seeds: all seed items inc. generated from eager advances of the original seeds.
; items: the items in the set, including seeds. we include one item for each
; item/follow combination and one 'general' item with no follow.
; backlink-map: maps items -> predicting items
; follow-map: maps items -> follow sets
(defrecord ItemSet [seeds more-seeds items backlink-map follow-map])
(declare build-item-set)

(defn item-set-item-str [item item-set]
  (let [predictors (omm/get-vec (:backlink-map item-set) item)
        predictor-str (->> predictors
                        (map item-str) (str/separate-str ", ") #_str/cutoff)]
    (str (item-str-follow item item-set)
         (if (seq predictor-str) (str " | " predictor-str)))))

(defn item-set-str [item-set]
  (str/separate-str \newline (map #(item-set-item-str % item-set) (:items item-set))))

; map scanner -> [shift items]
(defnmem shift-map [item-set]
  (omm/map
    (reduce (fn [m {:keys [rule] :as item}]
              (if-let [s (rules/terminal rule)]
                (omm/assoc m s item)
                m))
            omm/empty (:items item-set))))

; ordered multimap of scanners -> return items
(defnmem reduce-map [item-set]
  (reduce (fn [themap {rule :rule :as seed}]
            (if (rules/is-complete? rule)
              (reduce #(omm/assoc % %2 seed)
                      themap (get-in item-set [:follow-map seed]))
              themap))
          omm/empty (:more-seeds item-set)))

; For an item X predicted by A B C, finds A B C. filters by seed/nonseed
(defnmem advancables [{backlink-map :backlink-map} backlink seed?]
  (filter #(= seed? (:seed? %)) (omm/get-vec backlink-map backlink)))

(defnmem rule-size [rule]
  (- (-> rule :raw-rule :value count) (-> rule :null-results count)))

; The max number of clauses in an item set's seeds.
; TODO move to quentin?
(defnmem item-set-size [item-set]
  (apply max (map-> (:more-seeds item-set) :rule rule-size)))

(defnmem returns [{:keys [more-seeds]}] (distinct (map :backlink more-seeds)))

; If there's a shift reduce conflict NOT accounting for lookahead.
(defnmem shift-reduce? [item-set]
  (and (->> item-set :items (map :rule) (mapcat rules/predict) seq) ; can we shift?
       (seq (returns item-set))))

; If there's a reduce reduce conflict NOT accounting for lookahead.
(defnmem reduce-reduce? [item-set]
  (->> item-set :more-seeds (filter #(rules/is-complete? (:rule %)))
    (apply hash-set) count (< 1)))

; The item set's return if there's only one. Implies no lookahead. Note that
; reduce-map will be empty if a const-reduce exists!
(defnmem const-reduce [item-set]
  (if-not (or (shift-reduce? item-set) (reduce-reduce? item-set))
    (first (:more-seeds item-set))))

; A map item -> [predictor items] as in advancables
(defn advancable-map [item-set seed?]
  (into {} (filter second (zipmap
                              (omm/keys (:backlink-map item-set))
                              (map #(seq (advancables item-set % seed?))
                                   (omm/keys (:backlink-map item-set)))))))

; === Building item sets ===

; Adds a follow set for a predictor to an item in an item-set.
; If the given item is one advance from completion (meaning some follow sets
; might depend on its own), transitively closes those follow sets by recursion.
(defn add-follow [{:keys [items backlink-map follow-map] :as item-set} item predictor
                  new?]
  (let [fset1 (get-in item-set [:follow-map item] #{})
        ffirst (-> predictor :rule :follow-first deref)
        ; TODO integrate into rules/follow-first
        ; Transitive close if we need
        ffirst (if (rules/null-result @(:advance (:rule predictor)))
                 (set/union ffirst (get-in item-set [:follow-map predictor]))
                 ffirst)
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
          (let [r (reduce #(add-follow % %2 predictor false) item-set (predict item))]
            r))
        item-set))))

(defn predict-into-item-set [{:keys [items backlink-map follow-map] :as item-set}
                             item predictor]
  (when-not (rules/is-complete? item)
    (let [item-set (if (empty? (omm/get-vec backlink-map item))
                     (-> item-set
                       (update :items #(conj % item))
                       (add-follow item predictor true))
                     (add-follow item-set item predictor false))
          item-set (update item-set :backlink-map #(omm/assoc % item predictor))]
      item-set)))

(defn seeds-key [seeds follow-map]
  (into #{} (map #(assoc % :lane (-> follow-map (get %))) seeds)))

(defn populate-item-set [item-set]
  (loop [c item-set, dot 0]
    (if-let [s (get (:items c) dot)]
      (recur (reduce #(predict-into-item-set % %2 s)
                     c (predict s))
             (inc dot))
      c)))

; TODO can we combine eager-advances w/ predict-into-item-set?
; Given a follow map s -> v, where s are items and v are follow sets,
; creates a follow map contaning all eager advances of s
(defn eager-advance-follow-map [follow-map]
  (reduce (fn [m [item follow]]
            (reduce (fn [m aitem]
                      (update m aitem #(set/union % follow)))
                    m (eager-advances item false)))
          follow-map follow-map))

; Builds an item set with "holes" you can plug follow information into.
; There is one hole for each seed item. Basically it is a LR(1) set with missing
; information you can add later.
(defnmem holey-item-set [seeds]
  (if (seq seeds)
    (let [more-seeds (mapcat #(eager-advances % false) seeds)
          follow-map (zipmap seeds (map (fn [s] #{[:item s]}) seeds))
          follow-map (eager-advance-follow-map follow-map)]
      (populate-item-set
        (->ItemSet seeds more-seeds (vec more-seeds) omm/empty follow-map)))))

; For a map any -> [items], turns it into any -> item-set
; using a fn [seeds] -> item-set. The items are advanced to seed the set
(defn map-item-set-builder [builder m]
  (mapvals builder (mapvals #(map advance %) m)))

(declare conflicts)

; For a map any -> [items], where the items can be built into item sets,
; finds all conflicts for all those item sets.
(defn map-conflicts [m]
  (->> m
    (map-item-set-builder holey-item-set)
    vals
    (mapcat conflicts)))

; Returns any conflicts, viz. deep reduces for which we need to know the follow map.
(defnmem conflicts [item-set]
  ; If there's a shift reduce conflict, the returnable seeds are conflicts
  ; Also, if any continues have conflicts those belong to us too
  (let [my-conflicts (if (or (shift-reduce? item-set) (reduce-reduce? item-set))
                        (->> item-set
                          :more-seeds
                          (filter (fn-> :rule rules/is-complete?))
                          (map :backlink)
                          (remove nil?)))
        child-conflicts (map-conflicts (advancable-map item-set true))]
    (into #{} (concat my-conflicts child-conflicts))))

(defnmem filter-follows [item-set]
  (let [child-conflicts (map-conflicts (merge (shift-map item-set)
                                              (advancable-map item-set false)))
        my-conflicts (filter #((conflicts item-set) (:backlink %))
                             (:more-seeds item-set))
        my-conflicts (concat my-conflicts child-conflicts)]
    (update item-set :follow-map #(select-keys % my-conflicts))))

(defnmem fill-item-set [item-set follow-map]
  (let [r (update item-set :follow-map
                  (fn [m]
                    (mapvals
                      (fn [follows]
                        (reduce
                          (fn [follows f]
                            (if (= :item (first f))
                              (set/union (disj follows f)
                                         (get follow-map (second f)))
                              follows))
                          follows follows))
                      m)))]
    r))

; Builds a LR(1) item-set given some seed items and a parent follow map.
(def clr-item-set
  (fcache
    seeds-key
    (fn [seeds follow-map]
      (if (seq seeds)
        (fill-item-set (holey-item-set seeds) follow-map)))))

; Deduplication key for a fully built item set
; TODO not separate fn
(defnmem item-set-key [{:keys [items follow-map]}] [items follow-map])

; Used to prevent infinite recursion
(def ^:dynamic *crumbs* nil)

; builds an item set map from a map backlink->seeds and parent follow-map
(defn item-set-map-helper [m follow-map]
  (into {} (map (fn [k]
                  (let [v (get m k)]
                    ; ugly and inefficient. Oh well
                    [k (build-item-set (map advance v)
                                        (->> v
                                          (select-keys follow-map)
                                          (mapkeys advance)))]))
                (keys m))))

; Builds an item set, and as a side effect, all its children. Returns a ref.
; TODO this delay thing is a hack
(defn build-item-set [seeds follow-map]
  (let [build-key (seeds-key seeds follow-map)]
    (cond (empty? seeds) (ref-box nil),
          (lookup ::item-set build-key)
          (lookup ::item-set build-key),
          ; Prevent infinite recursion.
          (get *crumbs* build-key)
          ; Look it up later (the original result will be saved).
          (delay @(lookup ::item-set build-key)),
          true
          (let [item-set (fill-item-set (holey-item-set seeds) follow-map)
                follow-map (:follow-map item-set)]
            (binding [*crumbs* (assoc *crumbs* build-key item-set)]
              ; Eagerly build child item sets
              (let [item-set
                    ; TODO don't need to add
                    ; TODO assumes a defnmem
                    (merge
                      item-set
                      {:shift-map
                       (item-set-map-helper (shift-map item-set) follow-map),
                       :shift-advances
                       (item-set-map-helper (advancable-map item-set false)
                                            follow-map),
                       :continue-advances
                       (item-set-map-helper (advancable-map item-set true)
                                            follow-map)}),
                    item-set (filter-follows item-set)
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
          r @(build-item-set [g] {g #{[:term nil]}})]
      (println (save-count ::dedup-item-set) "item sets built")
      r)))
