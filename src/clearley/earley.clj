(ns clearley.earley
  (require [clearley.collections.ordered-set :as os]
           [clearley.collections.ordered-multimap :as omm])
  (use [clearley utils rules]))

; ===
; Earley items
; An EarleyItem is a rule together with some instrumentation.
; ===

(defrecord REarleyItem [rulehead rule original match-count]
  PStrable
  (pstr [_]
    (str rulehead " -> " (rule-str rule))))

(defn predict-earley-item [earley-item grammar]
  (let [clause (predict (:rule earley-item))]
    (map #(REarleyItem. (rulehead-clause clause) % % 0)
         (predict-clause clause grammar))))

(defn scan-earley-item [earley-item input-token]
  (map (fn [rule]
         (update (assoc earley-item :rule rule) :match-count inc))
       (scan (:rule earley-item) input-token)))

(defn advance-earley-item [earley-item]
  (update-all earley-item {:rule advance, :match-count inc}))

(defn earley-item [head-sym clause]
  (let [rule (to-rule clause)]
    (REarleyItem. head-sym rule rule 0)))

; ===
; Parse states
; Earley items together with NDFA state and output stack
; ===

; A parse item together with output state
(defrecord State [earley-item prev-set]
  PStrable
  (pstr [_] (str (pstr earley-item) (if-let [i (:index prev-set)]
                                      (str " @" i)
                                      ""))))

(defn predict-state [{:keys [earley-item] :as state} grammar]
  (map #(merge state {:earley-item % :prev-set nil})
       (predict-earley-item earley-item grammar)))

(defn complete-state [{{:keys [original] :as earley-item} :earley-item
                       :keys [prev-set] :as state}]
  (map (fn [{predictor-item :earley-item, old-prev-set :prev-set}]
         (State. (advance-earley-item predictor-item) (if old-prev-set
                                                        old-prev-set
                                                        prev-set)))
       (omm/get-vec (:predictor-map prev-set) original)))

(defn scan-state [state scanning-item-set input-token input]
  (map (fn [new-item]
         (update (assoc state :earley-item new-item)
                     :prev-set #(if % % scanning-item-set)))
       (scan-earley-item (:earley-item state) input-token)))

; creates an initial state with dot and pos 0
(defn state [head-sym rule]
  (State. (earley-item head-sym rule) nil))

; ===
; Parse item sets
; ===

; Builds a rule match from the output stack and pushes the match to the top
; (think of a Forth operator reducing the top of a stack)
; Right now, we build the stack as we parse instead of emitting an output stream...
; this may change in the future e.g. to support pull parsing
(defn- reduce-ostack [ostack {:keys [match-count original]}]
  (cons (match original (vec (reverse (take match-count ostack))))
        (drop match-count ostack)))

(defn pstr-item-set-item [item predictor-map]
  (let [predictor-str (cutoff
                        (separate-str ", "
                                      (map pstr
                                           (omm/get-vec predictor-map
                                                        (:original
                                                          (:earley-item item))))))]
    (str (pstr item) (if (seq predictor-str)
                       (str " | " predictor-str)
                       ""))))

; states: a vector of states (predicted states)
; predictor-map: ordered multimap, states -> predicting states
; index: the parsing position of this state TODO can this be eliminated?
; ostack: the output associated with this state
; prev-set: the origin itemset (will be null for the seed item at index 0)
(defrecord ItemSet [states predictor-map index ostack prev-set]
  PStrable
  (pstr [self] ; TODO switch args in cutoff
    (with-out-str
      (println (str "ItemSet#" (hash self) "@" index))
      (println "Predictor#" (hash prev-set))
      (println "ostream " (cutoff (str ostack 80)))
      (runmap println (map #(pstr-item-set-item % predictor-map) states)))))

(def empty-item-set (ItemSet. [] omm/empty 0 '() nil))

(defn predict-into-item-set [{:keys [states predictor-map] :as item-set}
                             {{original :original} :earley-item :as item}
                             predictor]
  (if (empty? (omm/get-vec predictor-map original))
    (update-all item-set {:states #(conj % item)
                          :predictor-map #(omm/assoc % original predictor)})
    (update item-set :predictor-map #(omm/assoc % original predictor))))

; Should only be called on a new item-set
; These states are not 'predicted' hence not in predictor-map
(defn seed-item-set [item-set item] (update item-set :states #(conj % item)))

(defn current-state [{:keys [states]} dot]
  (when-not (>= dot (count states))
    (get states dot)))

(defn nontrivial? [item-set] (seq (:states item-set)))

; scans an input character, seeding a new item-set
(defn scan-item-set [{:keys [states ostack] :as item-set} pos input-token input]
  (reduce seed-item-set
          (merge empty-item-set {:index pos, :ostack (cons (match input []) ostack)})
          (mapcat #(scan-state % item-set input-token input) states)))

; The completed item-sets from one item-set
(defn completions [{:keys [ostack states] :as item-set}]
  (filter nontrivial?
          (mapcat
            (fn [{{rule :rule :as earley-item} :earley-item :as state}]
              (if (is-complete? rule)
                (map #(assoc (seed-item-set empty-item-set %)
                             :ostack (reduce-ostack ostack earley-item))
                     (complete-state state))
                []))
            states)))

; TODO: predicting completed items seems to cause combinatorial explosion
(defn predict-item-set [item-set grammar]
  (loop [c item-set, dot 0]
    (if-let [s (current-state c dot)]
      (recur (if (is-complete? (:rule (:earley-item s)))
               c
               (reduce #(predict-into-item-set % %2 s)
                       c (predict-state s grammar)))
             (inc dot))
      c)))

; ===
; Charts
; ===

; item-sets: an ordered set
(defrecord Chart [item-sets]
  PStrable
  (pstr [self]
    (separate-str "---\n" (map pstr (os/vec item-sets)))))

(def empty-chart (Chart. os/empty))

(defn seed-chart [state]
  (assoc empty-chart :item-sets 
         (os/ordered-set (seed-item-set empty-item-set state))))

; process completions and predictions for a single item-sets
(defn complete-chart [chart]
  (loop [c chart, dot 0]
    (if-let [set (os/get (:item-sets c) dot)]
      (do
        (recur (reduce (fn [chart item-set]
                         (update chart :item-sets #(os/conj % item-set)))
                     c (completions set))
             (inc dot)))
      c)))

(defn process-chart [chart grammar]
  (let [chart (complete-chart chart)]
    (assoc chart :item-sets
           (os/map #(predict-item-set % grammar) (os/vec (:item-sets chart))))))

(defn scan-chart [chart pos thetoken thechar]
  (assoc empty-chart :item-sets
         (os/into os/empty
                  (filter nontrivial?
                          (map #(scan-item-set % pos thetoken thechar)
                               (os/vec (:item-sets chart)))))))

; ===
; Here be dragons
; ===

(defn parse-charts [inputstr grammar tokenizer goal]
  (loop [pos 0
         thestr inputstr
         current-chart (seed-chart (state ::goal goal))
         charts []]
    (if-let [thechar (first thestr)]
      (let [thetoken (tokenizer thechar)
            parsed-chart (process-chart current-chart grammar)
            next-chart (scan-chart parsed-chart (inc pos) thetoken thechar)
            next-charts (conj charts parsed-chart)]
        (if (some #(current-state % 0) (os/vec (:item-sets next-chart)))
          ; an item set is nonempty
          (recur (inc pos) (rest thestr) next-chart next-charts)
          ; early termination on failure returning failed item-sets
          (conj next-charts next-chart)))
      ; end returning all item-sets
      (conj charts (process-chart current-chart grammar)))))

(defn goals-from-itemset [item-set]
  (seq (filter (comp (partial = ::goal) :rulehead :earley-item)
               (:states item-set))))

; Searches item-sets for completed parse of the goal rule, returning all matches
(defn scan-goal [chart]
  (mapcat :ostack
          (filter goals-from-itemset (os/vec (:item-sets chart)))))
