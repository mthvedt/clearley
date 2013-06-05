(ns clearley.clr
  (require [clearley.rules :as rules]
           [uncore.collections.worm-ordered-multimap :as omm]
           [uncore.collections.worm-ordered-set :as os]
           [uncore.str :as s])
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
; follow: a terminal that can follow this item
; (depends on predicting items)
(defrecord Item [rule backlink seed? exit? follow])

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

; TODO kill
(defn unfollow [item] (if (:follow item) (assoc item :follow nil) item))

(defnmem advance [item]
  (assoc (update-all item {:rule (fn-> :advance deref)
                           :backlink #(if % % (unfollow item))}) :seed? true))

(defnmem goal-item [goal grammar]
  (new-item (rules/goal-rule goal grammar) true true [:term nil]))

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

(defn item-str-follow [{follow :follow :as item} follow-map]
  (let [follows (get follow-map (unfollow item))
        follows-str (s/separate-str " " (map follow-str follows))]
    (str (item-str item) (if follow (str " : " (follow-str follow)) "")
         " || " follows-str)))

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

(defn item-set-item-str [item backlink-map follow-map]
  (let [predictors (omm/get-vec backlink-map (unfollow item))
        predictor-str (->> predictors
                        (map item-str) (s/separate-str ", ") s/cutoff)]
    (str (item-str-follow item follow-map)
         (if (seq predictor-str) (str " | " predictor-str)))))

(defn item-set-str [{:keys [items backlink-map follow-map]}]
  (with-out-str ; TODO change
    (runmap println (map #(item-set-item-str % backlink-map follow-map) items))))

; A follow set is formed from the union of follow-first and follow sets.
; Check out any book on parsing theory for more information.
; These unions are stored in delays so we can tell if they are ever derefenced
; by the item set builder (if they never are we can just discard them).
; If a follow set contains any follow sets from other rules, we iteratively derefernce
; those sets, and so on. Eventually we will end up with the full follow set.
; We also need to be able to mark which follow sets we need to know about,
; and to be able to construct them lazily.
(defn follow-set! [item follow-map]
  (let [item (unfollow item)]
    (loop [follow-set #{}
           queue [item]
           breadcrumbs #{}]
      ; We loop until there are no more transitive follows to dereference.
      (if-let [top (first queue)]
        (let [{:keys [visited transitive-follows follow-firsts delay-follow-set]}
              (get follow-map (unfollow top))]
          ; Delay follow sets will only be carried over from noncyclic parent
          ; item sets. There's no danger of unbounded recursion.
          (if delay-follow-set
            (do
              @visited
              (recur (clojure.set/union follow-set @delay-follow-set)
                     (rest queue) (conj breadcrumbs top)))
            (let [new-follow-set (apply clojure.set/union follow-set
                                        (map-> follow-firsts :rule :follow-first
                                               deref))
                  ; TODO should transitive follows b unfollowed?
                  ; Only add transitive follow sets to the queue we haven't seen
                  new-queue (concat queue (remove breadcrumbs transitive-follows))]
              ; Mark that we've visited this follow set
              @visited
              (recur new-follow-set (rest new-queue) (conj breadcrumbs item)))))
        follow-set))))

(defn predict-into-item-set [{:keys [items backlink-map follow-map] :as item-set}
                             item predictor]
  (assert predictor) (assert item)
  (when-not (rules/is-complete? item)
    (let [item-set (if (empty? (omm/get-vec backlink-map item))
                     (update item-set :items #(conj % item))
                     item-set)
          ; TODO simplify
          item-set (update-in item-set [:follow-map (unfollow item) :follow-firsts]
                              (fnil conj #{}) (unfollow predictor))
          item-set (if (rules/null-result @(:advance (:rule predictor)))
                     (update-in item-set [:follow-map (unfollow item)
                                          :transitive-follows]
                                (fnil conj #{}) (unfollow predictor))
                     item-set)
          item-set (update-in item-set [:follow-map (unfollow item) :visited]
                              #(if % % (delay)))
          item-set (update item-set :backlink-map
                           (fn-> ; TODO rather hacky TODO eventually won't need
                                 (omm/assoc item nil)
                                 (omm/assoc (unfollow item) predictor)))]
      item-set)))

; ordered multimap scanner -> shift items
(defn shift-map [item-set]
  (reduce (fn [m {:keys [rule] :as item}]
            (if-let [s (rules/terminal rule)]
              (omm/assoc m s item)
              m))
    omm/empty (:items item-set)))

; ordered multimap of scanners -> return items
(defnmem reduce-map [item-set]
  (reduce (fn [themap {:keys [rule follow] :as seed}]
            (if (rules/is-complete? rule)
              (reduce #(omm/assoc % %2 (unfollow seed))
                      themap (get-in item-set [:follow-map (unfollow seed)]))
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

(defnmem returns [{:keys [more-seeds]}] (distinct (map unfollow
                                                       (map :backlink more-seeds))))

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
    ;(if (and (= 1 (count my-returns)) (not (shift-reduce? item-set)))
      (unfollow (first (:more-seeds item-set))))))

(defn advancable-map [item-set seed?]
  (into {} (filter second (zipmap
                              (omm/keys (:backlink-map item-set))
                              (map #(seq (advancables item-set % seed?))
                                   (omm/keys (:backlink-map item-set)))))))

; TODO make more elegant, perhaps parents store child item set numbers eagerly
(defn seeds-key [seeds follow-map]
  (into #{} (map (fn [seed]
                   (let [{:keys [transitive-follows follow-firsts]}
                         (get follow-map (unfollow seed))]
                     (merge seed {:transitive-follows transitive-follows
                                  :follow-firsts follow-firsts})))
                 seeds)))

; TODO no need for fchce
(def item-set-pass0
  (fcache
    seeds-key
    (fn [seeds follow-map]
      (if (seq seeds)
        (let [more-seeds (mapcat #(eager-advances % false) seeds)
              ; TODO the below is guly
              follow-map (apply hash-map
                                (mapcat (fn [[k v]]
                                          (mapcat #(list % v)
                                                  (eager-advances k false)))
                                        follow-map))]
          (loop [c (->ItemSet seeds more-seeds (vec more-seeds) omm/empty follow-map),
                 dot 0]
            (if-let [s (get (:items c) dot)]
              (recur (reduce #(predict-into-item-set % %2 s)
                             c (predict s))
                     (inc dot))
                c)))))))

; === Building item sets: actually doing it

; TODO do we even need follow-firsts in follow-map? or just transitive follows?
;
; TODO pass0 and pass1 can be combined? nah shoudl stay modular
; but don't name pass0 and pass1
(defnmem item-set-pass1 [seeds follow-map]
  (if (seq seeds)
    (let [item-set (item-set-pass0 seeds follow-map)
          ; Wrap the follow set calculations in delays.
          item-set (update item-set :follow-map
                           (fn [old-follow-set]
                             (mapm (fn [k v]
                                     [k (assoc v :delay-follow-set
                                               (delay
                                                 (follow-set! k old-follow-set)))])
                                   old-follow-set)))]
      item-set)))

; The final pass. Resolve conflicts.
(defn item-set-pass2 [item-set]
  ; This fn will build the final follow-map
  ; TODO error here if deterministic? Or maybe helper fn to find
  ; conflicts that is invoked in quentin
  (let [follow-map-fn (if
                        ;true
                        (or (shift-reduce? item-set) (reduce-reduce? item-set))
                        ; resolve conflicts--we have to resolve all of them
                        (fn [{:keys [visited delay-follow-set]}]
                          ;(println "Realizing unrealized follow")
                          @visited  @delay-follow-set)
                        ; Only resolve a conflict if visited has been deref'd
                        (fn [{:keys [visited delay-follow-set]}]
                          (if (realized? visited)
                            (do
                              ;(println "Visited!")
                              @delay-follow-set)
                            #_(println "Unvisited; discarding follow"))))]
    (update item-set :follow-map #(mapvals follow-map-fn %))))

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
                    ;(println "keys")
  ;(runmap println (map item-str v))
  ;                  (println "a")
  ;(runmap println (map item-str (keys follow-map)))
  ;                  (println "b")
  ;(runmap println (map item-str (keys (mapkeys advance (select-keys follow-map (map unfollow v))))))
                    [k (build-item-set* (map advance v)
                                        (->> v (map unfollow)
                                          (select-keys follow-map)
                                          (mapkeys advance)))]))
                                        ;(mapkeys advance
                                         ;        (select-keys follow-map (map unfollow v))))]))
                (keys m))))

; Builds an item set, and as a side effect, all its children. Returns a ref.
; TODO this delay thing is a hack
(defn build-item-set* [seeds follow-map]
  ;(let [build-key (apply hash-set seeds)]
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
                      ; TODO no reason to break up advance map
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
                  ; TODO this is not the acutal number of item sets
                  (println (save-count ::dedup-item-set) "item sets built"))
                (save! ::item-set build-key (ref-box item-set))))))))

; TODO progress reporting optional
; TODO build everything in advance
(defn build-item-sets [goal grammar]
  (println "Building item sets...")
  (binding [*crumbs* {}]
    (let [g (goal-item goal grammar)
          r @(build-item-set* [g] {(unfollow g)
                                   {:visited (delay)
                                    :delay-follow-set (delay #{[:term nil]})}})]
      (println (save-count ::dedup-item-set) "item sets built")
      r)))
