(ns clearley.quentin
  (require [uncore.throw :as t]
           [uncore.str :as s]
           [clearley.rules :as rules])
  (use clearley.clr uncore.core))

(defrecord ParseStream [input output pos])

(defn parse-stream [input]
  (->ParseStream input [] 0))

; Parse stream handler: parse stream -> parse stream

(declare continue-parsing)

(defn item-parser-fn [seeds]
  ;shift-fn (mem-new clr/shift-item-set (:items item-set) %)
  ;returns (memoize #(clr/returns more-seed-items %))
  ;continues (mem-new clr/advance-item-set item-set % true)
  ;bounces (mem-new clr/advance-item-set item-set % false)

  (fn [{:keys [input output pos] :as stream}]
    (let [current-input (first input)
          more-seeds (mapcat #(eager-advances % false) seeds)
          item-set (closed-item-set more-seeds)
          shift (if (seq input)
                  (shift-item-set (:items item-set) current-input)
                  nil)
          current-input (if (seq input) current-input :clearley.clr/term)
          return-set (returns more-seeds current-input)]
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
            ((continue-parsing item-set)
               ((item-parser-fn shift) 
                  (update-all stream {:input rest, :output #(conj % current-input),
                                      :pos inc})))

            return-set
            ; TODO don't ignore reduce-reduce
            ; TODO put rule in output.
            (update stream :output #(conj % (first return-set)))

            true (t/RE "Failure to parse at position" pos)))))

(defn continue-parsing [item-set]
  (fn [{:keys [input output pos] :as stream}]
    (let [current-backlink (:backlink (peek output))
          shift-advance (advance-item-set item-set current-backlink false)
          continue-advance (advance-item-set item-set current-backlink true)]
      ;(println "Stack top: " (-> current-backlink :rule :raw-rule :original))
      (if (and shift-advance continue-advance)
        (println "Stack split in item set\n" (item-set-str item-set)))
      (cond shift-advance (recur ((item-parser-fn shift-advance) stream))
            ; Keep us on the stack, see what comes next

            continue-advance ((item-parser-fn continue-advance) stream)
            ; Basically a tailcall

            true (t/RE "Failure to parse at position" pos)))))

(defn parse [grammar goal input]
  (try
    (binding [rules/*mem-atom* (atom {})]
      ((item-parser-fn [(goal-item goal grammar)]) (parse-stream input)))
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
