(ns clearley.quentin
  (require [uncore.throw :as t]
           [uncore.str :as s]
           [clearley.rules :as rules]
           [uncore.collections.worm-ordered-multimap :as omm]
           clojure.stacktrace)
  (use clearley.clr uncore.core))

; A Deterministic node can parse without any shift-reduce or reduce-reduce conflicts.
; They call each other directly like a recursive ascent parser, and do not trampoline.
(defprotocol DNode
  (parse-without-memo [self state]))
  ;(parse-with-memo [self state]))

; NDNodes may have shift-reduce or reduce-reduce conflicts.
; They must trampoline.
#_(defprotocol NDNode
  (parse-node [self state]))

(defprotocol IParseStream
  (shift [self output])
  (return [self output])
  (get-input [self])
  (get-output [self])
  (get-pos [self]))

(defrecord ParseStream [input output pos]
  IParseStream
  (shift [_ output2]
    (->ParseStream (rest input) (conj output output2) (inc pos)))
  (return [_ output2]
    (->ParseStream input (conj output output2) pos))
  (get-input [_] input)
  (get-output [_] output)
  (get-pos [_] pos))

(defn parse-stream [input]
  (->ParseStream input [] 0))

; Parse stream handler: parse stream -> parse stream

(declare continue-parsing item-parser-fn)

(defn get-all-advances [{backlink-map :backlink-map :as item-set} seed? mem]
  (let [all-backlinks (omm/keys backlink-map)]
    (into {}
          (remove (fn-> second nil?)
            (map (fn [backlink] [backlink (item-parser-fn
                                            (advance-item-set item-set backlink seed?)
                                            mem)])
                 all-backlinks)))))

(def nilref (delay nil))

(defn item-parser-fn* [seeds mem]
  (let [more-seeds (mapcat #(eager-advances % false) seeds)
        item-set (closed-item-set more-seeds)
        get-shift-parser (memoize #(item-parser-fn (shift-item-set item-set %) mem))
        shift-advances (get-all-advances item-set false mem)
        continue-advances (get-all-advances item-set true mem)
        #_get-shift-advancer #_(memoize #(item-parser-fn
                                       (advance-item-set item-set % false) mem))
        #_get-continue-advancer #_(memoize #(item-parser-fn
                                          (advance-item-set item-set % true) mem))
        get-return-set (memoize #(returns more-seeds %))]
    (fn [stream]
      (let [istream (get-input stream)
            current-input (first istream)
            shift-parser (if (seq istream) @(get-shift-parser current-input))
            current-input (if (seq istream) current-input :clearley.clr/term)
            return-set (get-return-set current-input)]
        ;(println "===")
        ;(println "Input: " current-input)
        ;(print (item-set-str item-set))
        (if (and shift-parser return-set)
          (println "Shift-reduce conflict in item set\n" (item-set-str item-set)))
        (if (and return-set (> (count return-set) 1))
          (println "Reduce-reduce conflict: "
                   (s/separate-str " " (map item-str-follow return-set))))
        (cond shift-parser
              (loop [result (shift-parser (shift stream current-input))]
                (let [returned (:backlink (peek (get-output result)))
                      shift-advance @(get shift-advances returned nilref)
                      continue-advance @(get continue-advances returned nilref)]
                  (if (and shift-advance continue-advance)
                    (println "Stack split in item set\n" (item-set-str item-set)))
                  (cond shift-advance
                        ; Keep us on the stack, see what comes next
                        (recur (shift-advance result))

                        ; Tail-call (not really) the next parser fn
                        continue-advance (continue-advance result))))

              return-set
              ; TODO put rule in output.
              (return stream (first return-set))

              true (t/RE "Failure to parse at position " (get-pos stream)))))))

(defn item-parser-fn [seeds mem]
  (let [old-mem @mem]
    (if-let [r (get old-mem seeds)]
      r
      (let [r2 (if (seq seeds) (delay (item-parser-fn* seeds mem)) nilref)]
        (if (compare-and-set! mem old-mem (assoc old-mem seeds r2))
          r2
          (recur seeds mem)))))) ; Spin loop--each item parser should exist only once

(defn parse [grammar goal input mem mem2]
  (try
    (binding [rules/*mem-atom* mem2]
      (@(item-parser-fn [(goal-item goal grammar)] mem) (parse-stream input)))
    (catch RuntimeException e (clojure.stacktrace/print-stack-trace e)))) ; TODO

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
