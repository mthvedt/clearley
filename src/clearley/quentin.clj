(ns clearley.quentin
  (require [uncore.throw :as t]
           [uncore.str :as s]
           [clearley.rules :as rules]
           [uncore.collections.worm-ordered-multimap :as omm]
           clojure.stacktrace clojure.pprint)
  (:refer-clojure :exclude [compile])
  (import clearley.ParseState clearley.TransientParseState)
  (use clearley.clr uncore.core uncore.memo))
; TODO what does aot do?
; TODO eliminate state, then have parser return results.
; TODO figure out locking?

(def ^:dynamic *print-code* true)
(defn print-code [& vals]
  (binding [*print-meta* true]
    (if *print-code* (runmap
                       #(if (sequential? %) (clojure.pprint/pprint %) (println %))
                       vals))))

(def ^:dynamic *parse-trace* false)

(def ^:dynamic *print-compile* false)
(defn print-compile [& vals]
  (if *print-compile* (runmap println vals)))

(def ^:dynamic *myns*)

(defn- compile [f]
  (try (binding [*ns* *myns*]
         (eval f))
    (catch Exception e
      (binding [*out* *err*] ; TODO bind out to error?
        (println "Exception compiling")
        (clojure.pprint/pprint f))
      (throw e)) ; TODO
    ; TODO don't catch and rethrow
    (catch java.lang.ExceptionInInitializerError e
      (binding [*out* *err*]
        (println "Exception compiling")
        (clojure.pprint/pprint f))
      (throw e))))

;(defn parse-stream [input] (TransientParseState. (seq input)))
;For now assume input is a string
(defn parse-stream [input] (TransientParseState. input))

(declare continue-parsing item-parser-sym cont-parser-sym embed-parser-with-lookahead)

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
            (print-compile (s/cutoff (str "Interning " sym " : " r) 80))
            (intern *myns* sym r)
            (swap! item-set-var-map #(assoc-in % [a-str obj] sym))
            sym))))))

; An evil way to def a thunk that, when called, generates a fn and redefs the sym
; to the fn! Returns the bound symbol.
(defn lookup-thunk [key candidate-thunk a-str tag]
  (let [item-set-var-map @(ns-resolve *myns* 'item-set-var-map)
        ns-lock @(ns-resolve *myns* 'ns-lock)]
    (locking ns-lock
      (let [sym (symbol (str a-str "-" (count (get @item-set-var-map a-str {}))))]
        (if-let [sym0 (get-in @item-set-var-map [a-str key])]
          sym0
          ; Assumption: candidate-thunk has no side effects
          ; TODO this is not actually true
          (let [thunk (fn [& args]
                        (let [r (candidate-thunk)]
                          (print-compile (s/cutoff (str "Interning " sym " : " r) 80))
                          (intern *myns* sym r)
                          (apply r args)))]
            (intern *myns* (if tag (with-meta sym {:tag tag}) sym) thunk)
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

(defnmem shift-advance [item-set backlink]
  (when-let [r (advance-item-set item-set backlink false)]
    (when (advance-item-set item-set backlink true)
      (println "State split conflict for item set:")
      (println (item-set-str item-set))
      (println "and item:")
      (println (item-str-follow backlink))
      (assert ((:split-conflicts item-set) backlink)))
    r))

; Anaphoric, needs a ~'result and a wrapping loop/recur and a ~'state
; For a backlink, calls the item-parser represented by that advance,
; and either looks up and recurs to a main branch, or calls the continuance.
(defn gen-advance-handler
  ([item-set advanced-item-set]
   (gen-advance-handler item-set advanced-item-set
                        `(~(item-parser-sym advanced-item-set) ~'state)))
  ([item-set advanced-item-set form]
   ; If we know what case comes next, we can inline it.
   (let [rs (:deep-reduces advanced-item-set)
         r (if (= (count rs) 1) (first rs))]
     (if r
       (if-let [advanced-2 (shift-advance item-set r)]
         ; Inline instead of recur
         (gen-advance-handler item-set advanced-2
                              `(~(item-parser-sym advanced-2) ~form))
         ; Return directly
         form)
       ; Don't know what's happing next, have to recur
       `(recur ~form)))))

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
                        #(gen-advance-handler item-set %)
                        (omm/keys backlink-map))
       ~'state))) ; Return and let parent item-set-parser pick it up

; Creates an advancer fn. Takes in a parse state and an array partial match.
(defn advance-looper [item-set]
  (let [r `(fn [~(apply symbol '(^ParseState state))]
             ~(gen-advance-loop item-set))
        _ (print-code "advancer" (item-set-str item-set) "with-code" r)
        f (compile r)]
    (print-code "compiled to" f)
    (if *parse-trace*
      (fn [& args]
        (println "Calling advancer" f)
        (print (item-set-str item-set))
        (println "with code")
        (clojure.pprint/pprint r)
        (println "and args")
        (clojure.pprint/pprint args)
        (let [r (apply f args)]
          (println f "returned")
          (println r)
          r))
      f)))

(defn gen-body-parser [item-set]
  (lookup-thunk item-set #(advance-looper item-set) "advancer" `ParseState))

(defn action-sym [action] (get-or-bind action identity "action"))
(defn scanner-sym [scanner] (get-or-bind scanner identity "scanner"))
(defn obj-sym [obj] (get-or-bind obj identity "match-result"))

; === Shifts and scanning

; Apply an action to an array of submatches
(defn apply-args [rule operands]
  (let [elisions (:elisions rule)
        match-count (:dot rule)
        raw-action (-> rule :raw-rule :action)]
    `(~(action-sym raw-action)
         ~@(loop [r [] operands operands i 0]
             ; TODO rewrite... with for?
             (cond (= match-count i) r ; return
                   (contains? elisions i)
                   (recur (conj r (obj-sym (get elisions i))) operands (inc i))

                   true
                   (recur (conj r (first operands)) (rest operands) (inc i)))))))

(defn gen-return [item working-syms arg-count]
  (let [backlink-id (-> item :backlink item-id)
        action-operands (if working-syms working-syms 
                          (map (fn [i] `(aget ~'partial-match ~i)) (range arg-count)))
        hidden? (:hidden? item)]
    `(.reduce ~'state ~backlink-id ~(if hidden? nil
                                      (apply-args (:rule item) action-operands)))))

(defn filter-keys [m f]
  (apply hash-map (mapcat (fn [[k v]] (if (f k) [k v] [])) m)))

; Scanners: from gen-scanner-handler, of form [scanner code-form]
; TODO nomenclature: some scanners are tokens and some scanners?
(defn gen-shift-table [scanners term? continuation]
  (let [selector (fn [wanted-tag]
                   (remove nil? (map (fn [[[tag scanner] form]]
                                       (if (= tag wanted-tag)
                                         [scanner form]))
                                     scanners)))
        ; Scanners are handled with a cond statement, after doing tokens
        scanners (selector :scanner)
        body (if (seq scanners)
               `(cond ~@(mapcat (fn [[scanner form]]
                                  `((~(scanner-sym scanner) ~'input) ~form))
                                scanners)
                      true ~continuation)
               continuation)

        ; Tokens are handled with a case statement
        tokens (selector :token)
        body (if (seq tokens)
               `(case ~'input 
                  ~@(mapcat (fn [[scanner form]] `(~(int scanner) ~form)) tokens)
                  ~body)
               body)

        ; Terminus handled first
        term (first (selector :term)) ; there can be only one
        body (if term?
               `(if (not (.hasCurrent ~'state))
                  ~(if term (second term) `(fail ~'state))
                  ~body)
               body)]
    body))

; Requires 'input, 'state, 'advancer, and 'partial-match OR non-null 'working-syms.
; Returns a code snippet for handling a state after a scanner is matched.
; Can either take action and return, or call a advancer returning the result.
(defn gen-scanner-handler [scanner item-set working-syms arg-count]
  (let [shift (omm/get-vec (shift-map item-set) scanner)
        return (omm/get-vec (reduce-map item-set) scanner)
        ; TODO enable/disable
        scanner (if (char? scanner) (long scanner) scanner)]
   (if (and (> (count return) 1) (not (:const-reduce item-set)))
      (do
        (assert (reduce-reduce? item-set))
        (println "Reduce-reduce conflict in item set\n" (item-set-str item-set))
        (println "for items" (s/separate-str " " (map item-str-follow return)))))
    (if (seq shift)
      (do
        (if (seq return)
          (do
            (assert (shift-reduce? item-set))
            (println "Shift-reduce conflict in item set\n" (item-set-str item-set))))
        [:shift [scanner
                 `(~(item-parser-sym
                      (pep-item-set shift (:split-conflicts item-set)))
                      (.shift ~'state ~'input))]])
      ; ignore reduce-reduce for now
      (if (seq return)
        [:reduce [scanner (gen-return (first return) working-syms arg-count)]]
        [:reduce [scanner nil]]))))

; For some tagged vals of the form [tag val], returns vals matching the tag
(defn untag [tagged-vals tag]
  (for [[tag1 val] tagged-vals :when (= tag1 tag)] val))

; Generates the parser fn for an item set. Can be monolithic or split
; into continuing parsers. If monolithic, will have working-syms defined.
; If split, will have argcount > 0.
; TODO collate continuing cases
(defn gen-parser [item-set working-syms argcount]
  (let [handlers (map #(gen-scanner-handler % item-set working-syms argcount)
                      (all-scanners item-set))
        return-handlers (untag handlers :reduce)
        shift-handlers (untag handlers :shift)
        next-sym (symbol (str "v" (count working-syms)))]

    ; Short-circuit if this is a trivial parser body.
    (if-let [const-reduce (:const-reduce item-set)]
      (do
        (assert (empty? shift-handlers))
        (gen-return const-reduce working-syms argcount))

      (gen-shift-table
        ; First, should return?
        return-handlers true
        ; OK, let's shift
        (if (empty? shift-handlers)
          ; Fail early if we can't shift
          `(fail ~'state)
          `(let [~(apply symbol '(^ParseState state))
                 ~(gen-shift-table shift-handlers false `(fail ~'state)),
                 ~(apply symbol '(^ParseState state))
                 (~(gen-body-parser item-set) ~'state)
                 ; If monolithic, also get input and next-sym here
                 ~@(if working-syms
                     `(~'input (.getCurrent ~'state)
                          ~next-sym (.returnValue ~'state))
                     ())]
             ; Save the output if we need to
             ~@(if working-syms
                 ()
                 `((aset ~'partial-match ~argcount (.returnValue ~'state))))
             ; Figure out what parser to call next
             (case (.getGoto ~'state)
               ~@(collate-cases item-id #(advance-item-set item-set % true)
                                (if working-syms
                                  ; If monolithc, splice in the next parser...
                                  (fn [item-set]
                                    (gen-parser
                                      item-set
                                      (conj working-syms next-sym) -1))
                                  ; Othwerise, it's a funcall
                                  (fn [item-set]
                                    `(~(cont-parser-sym item-set
                                                        (inc argcount))
                                         ~'state ~'partial-match)))
                                (omm/keys (:backlink-map item-set))))))))))

(defn gen-slow-cont-parser [item-set argcount]
  (let [r `(fn [~(apply symbol '(^ParseState state))
                ~(apply symbol '(^objects partial-match))]
             ; Some parsers are trivial. If so, we short-circuit.
             ~(if-let [const-reduce (:const-reduce item-set)]
                (gen-return const-reduce nil argcount)
                `(let [~'input (.getCurrent ~'state)]
                  ~(gen-parser item-set nil argcount))))
        f (compile r)]
    (print-code "item-set" (item-set-str item-set) "continuing parser" r
                "compiled to" f)
    (if *parse-trace*
      (fn [& args]
        (println "Parsing item set")
        (print (item-set-str item-set))
        (println "with code")
        (clojure.pprint/pprint r)
        (println "and args")
        (clojure.pprint/pprint args)
        (let [r (apply f args)]
          (println "Returned")
          (println r)
          r))
      f)))

(defn cont-parser-sym [item-set argcount]
  (lookup-thunk [:cont-slow-parser (item-set-key item-set) argcount]
                (fn []
                  (print-compile "Compiling item parser...")
                  (gen-slow-cont-parser item-set argcount))
                "continuing-item-parser" `ParseState))

(defn gen-slow-parser [item-set initial?]
  (let [r `(fn [~(apply symbol '(^ParseState state))]
             (let [~'partial-match (object-array ~(item-set-size item-set))
                   ~'input (.getCurrent ~'state)
                   ~@(if initial? [] `(~'arg0 (.returnValue ~'state)))]
               ~(if initial?
                  (gen-parser item-set nil 0)
                  `(do (aset ~'partial-match 0 ~'arg0)
                     ~(gen-parser item-set nil 1)))))]
    r))

; TODO find a better way.
(defn code-size [l]
  (if (coll? l)
    (reduce + 0 (map code-size l))
    1))

; Sets up all the stuff to execute gen-initial-shift
(defn gen-parser-body [item-set initial?]
  (let [r `(fn [~(apply symbol '(^ParseState state))]
             ; Some parsers are trivial. If so, we short-circuit.
             ~(if-let [const-reduce (:const-reduce item-set)]
                (if initial?
                  (gen-return const-reduce [] -1)
                  `(let [~'v0 (.returnValue ~'state)]
                     ~(gen-return const-reduce ['v0] -1)))
                `(let [~'input (.getCurrent ~'state)
                       ~@(if initial? () `(~'v0 (.returnValue ~'state)))]
                   ~(gen-parser item-set (if initial? [] ['v0]) -1))))
        r (if (> (code-size r) 64)
            (do
              ;(println "Parser too big!")
              (gen-slow-parser item-set initial?))
            r)
        f (compile r)]
    (print-code "item-set" (item-set-str item-set) "parser" r "compiled to" f)
    (if *parse-trace*
      (fn [& args]
        (println "Parsing item set")
        (print (item-set-str item-set))
        (println "with code")
        (clojure.pprint/pprint r)
        (println "and args")
        (clojure.pprint/pprint args)
        (let [r (apply f args)]
          (println "Returned")
          (println r)
          r))
      f)))

(defn item-parser-sym
  ([item-set] (item-parser-sym item-set false))
  ([item-set initial?]
   (when item-set
     (lookup-thunk [(item-set-key item-set) initial?]
                   (fn []
                     (print-compile "Compiling item parser...")
                     (gen-parser-body item-set initial?)) "item-parser"
                   `ParseState))))

(defn new-ns []
  (let [sym (gensym "quentin")
        r (create-ns sym)]
    ;(remove-ns sym) ; TODO
    (binding [*ns* r]
      (use 'clojure.core 'clearley.quentin)
      (import 'clearley.ParseState)
      (set! *unchecked-math* true))
    (intern r 'item-set-var-map (atom {})) ; map: seeds -> symbol
    (intern r 'ns-lock (Object.))
    r))

(defn parse [grammar goal input myns mem]
  (with-memoizer mem
    (try
      (binding [*myns* myns]
        (.returnValue
          (@(ns-resolve *myns*
                        (item-parser-sym (pep-item-set [(goal-item goal grammar)] #{})
                                         true))
              (parse-stream input))))
      ; TODO the below
      (catch Exception e (clojure.stacktrace/print-stack-trace e) nil))))
