(ns clearley.quentin
  (require [uncore.throw :as t]
           [uncore.str :as s]
           [clearley.rules :as rules]
           [uncore.collections.worm-ordered-multimap :as omm]
           clojure.stacktrace clojure.pprint)
  (:refer-clojure :exclude [compile])
  (import clearley.ParseState clearley.TransientParseState
          java.util.ArrayList)
  (use clearley.clr uncore.core uncore.memo))
; TODO what does aot do?
; TODO eliminate state, then have parser return results.
; TODO do we need locking?
;
; TODO a lot of this is ugly. One thing we can do is put deterministic item parsers
; in protocols (we will need to do this anyway). Then extra generated methods
; can be statically linked.

(def ^:dynamic *myns*)

(defn- compile [f]
  (try (binding [*ns* *myns*]
         (eval f))
    (catch Exception e
      (binding [*out* *err*]
        (println "Exception compiling")
        (clojure.pprint/pprint f))
      (throw e))))

(defn parse-stream [input]
  (TransientParseState. (seq input)))

; The core abstraction is
; Parse stream handler: parse stream -> parse stream
(declare continue-parsing item-parser-sym embed-parser-with-lookahead)

; For a factory seed, obj, and a factory method, factory,
; looks up the value created by the seed obj in the given namespace's map.
; If it doesn't exist, creates it, interns it, and returns the sym.
(defn get-or-bind [obj factory a-str]
  (let [item-set-var-map @(ns-resolve *myns* 'item-set-var-map)
        ns-lock @(ns-resolve *myns* 'ns-lock)]
    (locking ns-lock
      (let [sym (symbol (str a-str "-" (count (get @item-set-var-map a-str {}))))]
        (if-let [sym0 (get-in @item-set-var-map [a-str obj])]
          sym0
          (let [r (factory obj)]
            (println (s/cutoff (str "Interning " sym " : " r) 80))
            (intern *myns* sym r)
            (swap! item-set-var-map #(assoc-in % [a-str obj] sym))
            sym))))))

; An evil way to def a thunk that, when called, generates a fn and redefs the sym
; to the fn! Returns the bound symbol.
(defn lookup-thunk [key candidate-thunk a-str]
  (let [item-set-var-map @(ns-resolve *myns* 'item-set-var-map)
        ns-lock @(ns-resolve *myns* 'ns-lock)]
    (locking ns-lock
      (let [sym (symbol (str a-str "-" (count (get @item-set-var-map a-str {}))))]
        (if-let [sym0 (get-in @item-set-var-map [a-str key])]
          sym0
          ; Assumption: thunk has no side effects
          (let [thunk (fn [& args]
                        (let [r (candidate-thunk)]
                          (println (s/cutoff (str "Interning " sym " : " r) 80))
                          (intern *myns* sym r)
                          (apply r args)))]
            (intern *myns* sym thunk)
            (swap! item-set-var-map #(assoc-in % [a-str key] sym))
            sym))))))

(defn item-id [item]
  (generate ::item item (fn [_ id] id)))

(defn fail [^ParseState stream] (t/RE "Failure to parse at position: " (.pos stream)))

; === Advance loops ===

; Creates an interleaved seq of case-num, body pairs for splicing into a case.
; Produces, (1 2 3) foo, where 1 2 3 are generated by case-label-fn and grouped
; by case-key-fn, and the bodies are done by calling case-body-fn on the keys.
(defn collate-cases [case-label-fn case-key-fn case-body-fn vals]
  (mapcat (fn [[k ls]]
            (list (apply list ls) (case-body-fn k)))
          (reduce (fn [k->ls v]
                    (if-let [k (case-key-fn v)]
                      (assoc k->ls k (conj (get k->ls k []) (case-label-fn v)))
                      k->ls))
                  {} vals)))

; TODO get state-split conflicts
; Anaphoric, needs a ~'result and a wrapping loop/recur and a ~'state
; For a backlink, calls the item-parser represented by that advance,
; and either looks up and recurs to a main branch, or calls the continuance.
(defn gen-advance-handler [shift-advance]
  `(recur (~(item-parser-sym shift-advance ['v0])
              ~'state
              (.returnValue ~'state))))

; TODO PROBLEM. After single-returns, shift-adv and cont-adv get squashed?
; Why do we even have this distinction? An item set should always know
; if it will return or not.
;
; TODO maybe a 'lookahead required' distinction. also need stack split warning.

(defn shift-advance [item-set backlink]
  (when-let [r (advance-item-set item-set backlink false)]
    (when (advance-item-set item-set backlink true)
      (println "State split conflict for item set:")
      (println (item-set-str item-set))
      (println "and item:")
      (println (item-str-follow backlink))
      (assert ((:split-conflicts item-set) backlink)))
    r))

; TODO first follow conflicts?

; TODO redoc
; TODO state split conflicts
; TODO we can make this smaller since we know
; a token consuming shift only happens once
; For an item set, builds the 'continuing table' which handles all steps after
; the first initial shift. Implemented as a loop/recur with a case branch.
; The branch table matches all possible advances (advancing shifts, continues)
; to the appropriate branch. The appropriate branch either recurs (advancing shift)
; or calls the next item set (continue).
(defn gen-advance-loop [{backlink-map :backlink-map :as item-set}]
  `(loop [~'state ~'state]
     (case (.getGoto ~(apply symbol '(^ParseState state)))
       ~@(collate-cases item-id #(shift-advance item-set %)
                        gen-advance-handler (omm/keys backlink-map))
       ~'state))) ; Return and let parent item-set-parser pick it up

; Creates an advancer fn. Takes in a parse state and an ArrayList partial match.
(defn advance-looper [item-set]
  (let [r `(fn [~(apply symbol '(^ParseState state))]
             ~(gen-advance-loop item-set))]
    ;(println "Advancer") (clojure.pprint/pprint r)
    (compile r)))

(defn gen-body-parser [item-set]
  (lookup-thunk item-set #(advance-looper item-set) "advancer"))

(defn action-sym [action] (get-or-bind action identity "action"))
(defn scanner-sym [scanner] (get-or-bind scanner identity "scanner"))
(defn item-sym [item] (get-or-bind item identity "item"))

; === Shifts and scanning

(defn apply-args [arg-count action-sym]
  `(~action-sym ~@(map (fn [i] `(.get ~'partial-match ~i)) (range arg-count))))

(defn gen-return [item working-syms arg-count]
  ; TODO item is null for some reason
  (let [action (-> item :rule rules/get-original :action)
        backlink (-> item :backlink item-id)]
    `(do
       (.setReturnValue ~'state
                        ~(if working-syms
                           `(~(action-sym action) ~@working-syms)
                           (apply-args arg-count (action-sym action))
                           #_(apply ~(action-sym action)
                                   ~'partial-match)))
       (.reduce ~'state ~(item-sym item) ~backlink))))

; Requires 'input, 'state, 'advancer, and 'partial-match OR non-null 'working-syms.
; Returns a code snippet for handling a state after a scanner is matched.
; Can either take action and return, or call a advancer returning the result.
(defn gen-scanner-handler [scanner {actions :actions :as item-set} working-syms
                           arg-count]
  (let [shift (untag (omm/get-vec actions scanner) :shift)
        return (untag (omm/get-vec actions scanner) :reduce)]
    (if (and (> (count return) 1) (not (:single-reduce item-set)))
      (do
        (assert (:reduce-reduce? item-set))
        (println "Reduce-reduce conflict in item set\n" (item-set-str item-set))
        (println "for items" (s/separate-str " " (map item-str-follow return)))))
    (if (seq shift)
      (do
        (if (seq return)
          (do
            (assert (:shift-reduce? item-set))
            (println "Shift-reduce conflict in item set\n" (item-set-str item-set))))
        [:shift [scanner
                 `(~(item-parser-sym (pep-item-set (map advance-item shift)
                                                   (:split-conflicts item-set)) ['v0])
                      (.shift ~'state ~'input)
                      ~'input)]])
      ; ignore reduce-reduce for now
      (if (seq return)
        [:reduce [scanner (gen-return (first return) working-syms arg-count)]]
        [:reduce [scanner nil]]))))

; A cond chain for all actions
; TODO unify with gen-parser-body
(defn gen-parser [{actions :actions :as item-set} working-syms]
  (let [cond-pair-fn (fn [[scanner code]]
                       [(list (scanner-sym scanner) 'input) code])
        handlers (map #(gen-scanner-handler % item-set working-syms -1)
                      (remove #(= :clearley.clr/term %) (omm/keys actions)))
        return-handlers (untag handlers :reduce)
        shift-handlers (untag handlers :shift)
        next-sym (symbol (str "v" (count working-syms)))
        [_ [_ term-handler]] (gen-scanner-handler :clearley.clr/term item-set
                                                  working-syms -1)]

    (if-let [single-return (or (:full-single-reduce item-set)
                               (:single-reduce item-set))]
      (do
        (assert (empty? shift-handlers))
        ;(println "====single-return:" (item-str-follow single-return))
        (gen-return single-return working-syms -1))
      ; Special handling for terminus (terminus always returns, BTW)
      `(cond (not (.hasCurrent ~'state)) ~(if term-handler term-handler
                                            `(fail ~'state))
             ; See if we should return
             ~@(mapcat cond-pair-fn return-handlers)
             ; Shift, get the result, continue with the result
             true (let [~(apply symbol '(^ParseState state))
                        (cond ~@(mapcat cond-pair-fn shift-handlers)
                              true (fail ~'state)),
                        ~(apply symbol '(^ParseState state))
                        (~(gen-body-parser item-set) ~'state),
                        ~'input (.getCurrent ~'state)
                        ~next-sym (.returnValue ~'state)]
                    (case (.getGoto ~'state)
                      ~@(collate-cases item-id #(advance-item-set item-set % true)
                                       (fn [item-set]
                                         (gen-parser
                                           item-set (conj working-syms next-sym)))
                                       (omm/keys (:backlink-map item-set)))))))))

(declare bind-slow-cont-parser)

; TODO redo names, eliminate code duplication
(defn gen-slow-parser [{actions :actions :as item-set} argcount]
  (let [cond-pair-fn (fn [[scanner code]]
                       [(list (scanner-sym scanner) 'input) code])
        handlers (map #(gen-scanner-handler % item-set nil argcount)
                      (remove #(= :clearley.clr/term %) (omm/keys actions)))
        return-handlers (untag handlers :reduce)
        shift-handlers (untag handlers :shift)
        [_ [_ term-handler]] (gen-scanner-handler :clearley.clr/term item-set nil
                                                  argcount)]

    ; Special handling for terminus (terminus always returns, BTW)
    (if-let [single-return (or (:full-single-reduce item-set)
                               (:single-reduce item-set))]
      (do
        (assert (empty? shift-handlers))
        ;(println "====single-return:" (item-str-follow single-return))
        (gen-return single-return nil argcount))
      `(let [~'input (.getCurrent ~'state)]
         (cond (not (.hasCurrent ~'state))
               ~(if term-handler term-handler `(fail ~'state)),
               ; See if we should return
               ~@(mapcat cond-pair-fn return-handlers)
               ; Shift, get the result, continue with the result
               true (let [~(apply symbol '(^ParseState state))
                          (cond ~@(mapcat cond-pair-fn shift-handlers)
                                true (fail ~'state)),
                          ~(apply symbol '(^ParseState state))
                          (~(gen-body-parser item-set) ~'state),]
                      ;(.set ~'partial-match ~argcount (.returnValue ~'state)) ; TODO
                      (.add ~'partial-match (.returnValue ~'state))
                      (case (.getGoto ~'state)
                        ~@(collate-cases item-id #(advance-item-set item-set % true)
                                         (fn [item-set]
                                           `(~(bind-slow-cont-parser item-set
                                                                     (inc argcount))
                                                ~'state ~'partial-match))
                                         (omm/keys (:backlink-map item-set))))))))))

(defn get-slow-parser [item-set initial-symbols]
  (let [r `(fn [~(apply symbol '(^ParseState state))
                ~@(if (seq initial-symbols) ['first-arg] [])] ; TODO ugly
             (let [~'partial-match (ArrayList. ~(item-set-size item-set))]
               ;~(println "====size====" (item-set-size item-set))
               ~(case (count initial-symbols)
                  0 (gen-slow-parser item-set 0)
                  ;1 `(do (.set ~'partial-match 0 ~'first-arg) ; TODO
                  1 `(do (.add ~'partial-match ~'first-arg)
                       ~(gen-slow-parser item-set 1)))))]
    r))

(defn gen-slow-cont-parser [item-set argcount]
  (let [r `(fn [~(apply symbol '(^ParseState state))
                ~(apply symbol '(^ArrayList partial-match))]
             ~(gen-slow-parser item-set argcount))
        f (compile r)]
    ;(println "slow cont parser") (clojure.pprint/pprint r)
    f
    #_(fn [& args]
      (println "Parsing item set")
      (print (item-set-str item-set))
      (println "with code")
      (clojure.pprint/pprint r)
      (println "and args")
      (prn args)
      (apply f args))))

(defn bind-slow-cont-parser [item-set argcount]
  (lookup-thunk [:cont-slow-parser (item-set-key item-set) argcount]
                (fn []
                  ;(println "Compiling parser for item set:")
                  ;(println (item-set-str item-set))
                  (println "Compiling parser...")
                  (gen-slow-cont-parser item-set argcount))
                "continuing-item-parser"))

; TODO find a better way.
(defn code-size [l]
  (if (coll? l)
    (reduce + 0 (map code-size l))
    1))

; Sets up all the stuff to execute gen-initial-shift
(defn gen-parser-body [item-set initial-symbols]
  (let [r `(fn [~(apply symbol '(^ParseState state))
                ~@initial-symbols]
             (let [~'input (.getCurrent ~'state)]
               ~(gen-parser item-set initial-symbols))) ; TODO move to own fn
        r (if (> (code-size r) 64)
            (do
              ;(println "Parser too big!")
              (get-slow-parser item-set initial-symbols))
            r)
        f (compile r)]
    ;(clojure.pprint/pprint "item-set") (println (item-set-str item-set)) (clojure.pprint/pprint "parser") (clojure.pprint/pprint r) (println "compiled to" f)
    f
    #_(fn [& args]
      (println "Parsing item set")
      (print (item-set-str item-set))
      (println "with code")
      (clojure.pprint/pprint r)
      (println "and args")
      (prn args)
      (apply f args))))

(defn item-parser-sym [^clearley.clr.ItemSet item-set initial-symbols]
  (when item-set
    (lookup-thunk [(item-set-key item-set) (count initial-symbols)]
                  (fn []
                    ;(println "Loading code for item set:")
                    ;(println (item-set-str item-set))
                    (println "Compiling item parser...")
                    (gen-parser-body item-set initial-symbols)) "item-parser")))

(defn new-ns []
  (let [sym (gensym "quentin")
        r (create-ns sym)]
    ;(remove-ns sym) ; TODO
    (binding [*ns* r]
      (use 'clojure.core 'clearley.quentin)
      (import 'clearley.ParseState 'java.util.ArrayList))
    (intern r 'item-set-var-map (atom {})) ; map: seeds -> symbol
    (intern r 'ns-lock (Object.))
    r))

(defn parse [grammar goal input myns mem]
  (try
    (with-memoizer mem
      (binding [*myns* myns]
        (.returnValue
          (@(ns-resolve *myns*
                        (item-parser-sym (pep-item-set [(goal-item goal grammar)] #{})
                                         []))
              (parse-stream input)))))
    ; TODO the below
    (catch RuntimeException e (clojure.stacktrace/print-stack-trace e) nil)))
