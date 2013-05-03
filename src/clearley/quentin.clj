(ns clearley.quentin
  (require [uncore.throw :as t]
           [uncore.str :as s]
           [clearley.rules :as rules])
  (use clearley.clr uncore.core))

(defrecord ParseStream [input output pos])

(defn parse-stream [input]
  (->ParseStream input [] 0))

; Parse stream handler: parse stream -> parse stream

(declare continue-parsing item-parser-fn)

(defn tramp [initial-fn]
  (let [r (initial-fn)]
    (if (ifn? r)
      (recur r)
      r)))

(defn item-parser-fn* [seeds mem]
  (let [more-seeds (mapcat #(eager-advances % false) seeds)
        {item-set-items :items :as item-set} (closed-item-set more-seeds)

        shifter-fn (memoize #(shift-item-set item-set-items %))
        return-fn (memoize #(returns more-seeds %))
        continuer (continue-parsing item-set mem)]
    (fn [{:keys [input output pos] :as stream} continuation]
      (let [current-input (first input)
            shift (if (seq input) (shifter-fn current-input))
            current-input (if (seq input) current-input :clearley.clr/term)
            return-set (return-fn current-input)]
        ;(println "===")
        ;(println "Input: " current-input)
        ;(print (item-set-str item-set))
        ; TODO
        (if (and shift return-set)
          (println "Shift-reduce conflict in item set\n" (item-set-str item-set)))
        (if (and return-set (> (count return-set) 1))
          (println "Reduce-reduce conflict: "
                   (s/separate-str " " (map item-str-follow return-set))))
        (cond shift
              (fn []
                ((item-parser-fn shift mem)
                   (update-all stream {:input rest, :output #(conj % current-input),
                                        :pos inc})
                  #(continuer % continuation)))
              #_(continuer
                 ((item-parser-fn shift mem) 
                    (update-all stream {:input rest, :output #(conj % current-input),
                                        :pos inc})))

              return-set
              ; TODO don't ignore reduce-reduce
              ; TODO put rule in output.
              (fn []
                (continuation (update stream :output #(conj % (first return-set)))))

              true (t/RE "Failure to parse at position" pos))))))

(defn item-parser-fn [seeds mem]
  (if (seq seeds)
    (let [old-mem @mem]
      (if-let [r (get old-mem seeds)]
        r
        (let [r2 (item-parser-fn* seeds mem)]
          (if (compare-and-set! mem old-mem (assoc old-mem seeds r2))
            r2
            (recur seeds mem))))))) ; Spin loop

(defn continue-parsing [item-set mem]
  (let [shift-advancer (memoize #(advance-item-set item-set % false))
        continue-advancer (memoize #(advance-item-set item-set % true))]
    (fn f [{:keys [input output pos] :as stream} continuation]
      (let [current-backlink (:backlink (peek output))
            shift-advance (shift-advancer current-backlink)
            continue-advance (continue-advancer current-backlink)]
        ;(println "Stack top: " (-> current-backlink :rule :raw-rule :original))
        (if (and shift-advance continue-advance)
          (println "Stack split in item set\n" (item-set-str item-set)))
        (cond shift-advance
              ;(recur ((item-parser-fn shift-advance mem) stream))
              (fn [] ((item-parser-fn shift-advance mem) stream #(f % continuation)))
              ; Keep us on the stack, see what comes next

              continue-advance
              #((item-parser-fn continue-advance mem) stream continuation)
              ;((item-parser-fn continue-advance mem) stream)
              ; Basically a tailcall

              true (t/RE "Failure to parse at position" pos))))))

(defn parse [grammar goal input mem mem2]
  (try
    (binding [rules/*mem-atom* mem2]
      (tramp #((item-parser-fn [(goal-item goal grammar)] mem)
                 (parse-stream input)
                 identity)))
    (catch RuntimeException e nil))) ; TODO this is a hack

(defn pprint-parse-stream [{:keys [input output] :as stream}]
  ;(println "Input:" input)
  ;(println "Output:")
  (runmap (fn [item] (if (instance? clearley.clr.Item item)
                       (println (item-str item))
                       (prn item)))
          output))

; Builds a rule match from the output stack and pushes the match to the top
; (think of a Forth operator reducing the top of a stack)
; Final output (for a valid parse) will be a singleton list
(defn ostream-str [val]
  (if (instance? clearley.rules.Match val)
    (pr-str (rules/take-action* val))
    (pr-str val)))
(defn reduce-ostream-helper [ostream val]
  (if (instance? clearley.clr.Item val)
    (let [{:keys [rule match-count]} val]
      (cons (rules/match (rules/get-original rule)
                         (vec (reverse (take match-count ostream))))
            (drop match-count ostream)))
    (do
      (cons (rules/match val []) ostream))))

(defn reduce-ostream [ostream]
  (first (reduce reduce-ostream-helper '() ostream)))

(defn finalize-state [{output :output}]
  (reduce-ostream output))
