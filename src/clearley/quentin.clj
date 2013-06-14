(ns clearley.quentin
  (require [uncore.throw :as t]
           [uncore.str :as s]
           [uncore.collections.worm-ordered-multimap :as omm]
           clojure.stacktrace clojure.pprint)
  (import clearley.ParseState)
  (use clearley.clr uncore.core uncore.memo
       [uncore.ref :only [oderef]]))

(def ^:dynamic *print-code* false)
(defn print-code [& vals]
  (binding [*print-meta* true]
    (if *print-code* (runmap
                       #(if (sequential? %) (clojure.pprint/pprint %) (println %))
                       vals))))

(def ^:dynamic *parse-trace* false)

; TODO
(def ^:dynamic *print-build* false)
(defn print-build [& vals]
  (if *print-build* (runmap println vals)))

(def ^:dynamic *myns*)
(def ^:dynamic *queue*)

(def ^:dynamic *opts*)
(def default-opts {:stream-type :chars})

(declare continue-parsing item-parser-sym cont-parser-sym embed-parser-with-lookahead)

; We will need these in the namespaces we build
(defn do-imports []
  (import '[clearley ParseState ParseStream TransientParseState
            ObjParseStream CharParseStream SeqParseStream StringParseStream]))
(do-imports)

; Returns the symbol ~'stream with appropriate type hint
(defn stream-sym []
  (let [stream-tag (case (:stream-type *opts*)
                     :chars `CharParseStream
                     :objs `ObjParseStream)]
    (vary-meta 'stream assoc :tag stream-tag)))

; We can expand this in the future
(defn new-stream [input]
  (case (:stream-type *opts*)
    :chars (StringParseStream. input)
    :objs (SeqParseStream. (seq input))))

; For a factory seed, obj, and a factory method, factory,
; looks up a sym for the value created by the seed obj in the given namespace's map.
; If it doesn't exist, creates it, interns it, and returns the sym.
; TODO improve this
; TODO don't need meta
(defn get-or-bind [obj factory a-str a-meta]
  (let [item-set-var-map @(ns-resolve *myns* 'item-set-var-map)
        ns-lock @(ns-resolve *myns* 'ns-lock)]
    (locking ns-lock
      (let [sym (vary-meta
                  (symbol (str a-str "-" (count (get @item-set-var-map a-str {}))))
                          #(merge a-meta %))]
        (if-let [sym0 (get-in @item-set-var-map [a-str obj])]
          sym0
          (let [r (factory obj)]
            (print-build (s/cutoff (str "Interning " sym " : " r) 80))
            (intern *myns* sym r)
            (swap! item-set-var-map #(assoc-in % [a-str obj] sym))
            sym))))))

(defn item-id [item]
  (generate ::item item
            (fn [_ id] (print-build (str "Generated item id " id " for item "
                                         (when item (item-str item))))
              id)))

(defn fail [^ParseStream stream, #_item-set]
  (t/RE "Failure to parse at position: " (.pos stream)))

; === Protocol generator

; parser bodies: {:tag, :name, :args, :body}
(defn interface-body [{:keys [tag name args]}]
  (if tag
    `(~name ~(with-meta (vec (rest args)) {:tag tag}))
    `(~name (vec args))))

(defn impl-body [{:keys [name args body]}]
  ; Dissoc tags--let compiler automatch type hints, avoids some hint match bugs
  `(~name ~(vec (map #(vary-meta % dissoc :tag) args)) ~(deref body)))

(defn trace [name info body]
  (if *parse-trace*
    `(do
       (println "Calling" ~name)
       ~@(if info `((println ~info)))
       (println "with code")
       (clojure.pprint/pprint ~(get-or-bind body identity "code-form" {}))
       ;(println "and state") TODO
       ~body
       ; TODO why does this error?
       #_(let [r# ~body]
         (println "Method" ~name "returned")
         (prn r#)
         r#))
    body))

; Generates a method body for gen-praser-protocols.
; Body should be a ref (probably a delay, for mutual references to other bodies).
; Stashed into the memo key ::parse-fns. If we've seen the given
; keys before, does nothing instad. Returns a method name.
(defn stash-method [name info tag args body & key]
  (let [body (delay
               ; TODO describe tags, arg
               (print-code "Generating" name)
               (if info (print-code info))
               (print-code "with code")
               (print-code (with-out-str (clojure.pprint/pprint @body)))
               (trace (str "." name) info @body))]
    (->>
      (generate ::parse-fns (apply vector name key)
                (fn [_ i]
                  (let [name (symbol (str name "_" i))]
                    (swap! *queue* conj body)
                    {:body body :idx i :name name :tag tag :args args})))
      :name (str ".") symbol)))

(defn gen-parser-protocols []
  ; We need a protocol and an implementing deftype. This is the only facility
  ; in Clojure that generates Java classes at 100% Java speed--gen-class won't
  ; cut it. Nor do fns for our use case--we have too many fns, causing
  ; repeated var lookups (which are normally optimized away).

  ; Sorting not neccesary but looks nice 
  (let [bodies (sort (comparator (fn [x y] (< (:idx x) (:idx y))))
                     (vals (get-mmap ::parse-fns)))]
    `(do
       (definterface ~'ParserInterface
         ~@(map interface-body bodies))
       (deftype ~'Parser [~(vary-meta (stream-sym) assoc :unsynchronized-mutable true)
                          ~(apply symbol '(^long ^:unsynchronized-mutable goto))]
         ~'ParserInterface
         ~@(map impl-body bodies)))))

; === Advance loops ===

; TODO better names for everything
(defn action-sym [action]
  (if (symbol? action)
    action
    (get-or-bind action identity "action" {})))

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

; Anaphoric, needs a ~'result and a wrapping loop/recur and a ~'self and ~'nextVal
; Generates the code required to parse an item set and handle the result.
; Can return or recur.
; TODO unify advancers with same bodies
(defn gen-advance-handler
  ([item-set advanced-item-set]
   (gen-advance-handler item-set advanced-item-set 'next-val))
  ([item-set advanced-item-set form]
   (assert form)
   (let [rs (returns advanced-item-set)
         ; If we know what's coming next, we can inline it.
         r (if (= (count rs) 1) (first rs))
         advanced-2 (get-in item-set [:shift-advances r])
         form (if-let [a-const-reduce (const-reduce advanced-item-set)]
                ; If we know the advanced item set's behavior, we can inline it.
                (if advanced-2
                  ; If we furthermore know it's a shift advance, we do not have
                  ; to set the goto number.
                  `(~(-> a-const-reduce :rule :raw-rule :action action-sym) ~form)
                  `(let [~'return ~form]
                     (set! ~'goto ~(item-id r))
                     (~(-> a-const-reduce :rule :raw-rule :action action-sym)
                         ~'return)))
                `(~(item-parser-sym advanced-item-set false false)
                     ~'self ~form))]
     (if r
       (if advanced-2
         ; We can inline this
         (gen-advance-handler item-set @advanced-2 form)
         ; We can rturn directly
         form)
       ; We don't know what's coming next... let the main loop handle it
       `(recur ~form)))))

; For an item set, determines the finite state automaton required to parse
; that item set's body. See Aycock and Horspool's paper series
; on faster GLR parsing. Right now we do not use the full power of this graph
; but at least it's there if we need it.
; TODO explode to graph library
; TODO we can probably use backlink-map instead of all this item set nonsense.
(defn gen-advance-graph [{backlink-map :backlink-map :as item-set}]
  (let [s-sets (map deref (vals (:shift-map item-set)))
        entries (mapcat returns s-sets)
        [fgraph bgraph]
        (loop [fgraph {} bgraph {} queue s-sets breadcrumbs #{}]
          (if-let [node (first queue)]
            (do
              (if (breadcrumbs (item-set-key node))
              (recur fgraph bgraph (rest queue) breadcrumbs)
              (let [edges (returns node)
                    new-nodes (map
                                (fn [edge]
                                  (if-let [r (get-in item-set [:shift-advances edge])]
                                    @r
                                    :return))
                                edges)]
                (recur (assoc fgraph node edges)
                       (merge fgraph (zipmap edges new-nodes))
                       (concat (rest queue) new-nodes)
                       (conj breadcrumbs (item-set-key node))))))
            [fgraph bgraph]))]
    ; A labeled directional graph. The nodes are parser syms and edges are
    ; return items. Entries are the entry edges. :return means we return that edge.
    [(into #{} entries) fgraph bgraph]))

; Generates the automaton that handles the body (non-seed items) of a parse state.
; When the automaton is done,
; it returns a value that can be used to advance seed items.
; TODO how to inline shifts?
(defn gen-body-automaton [{backlink-map :backlink-map :as item-set}]
  (let [[entries fgraph bgraph] (gen-advance-graph item-set)
        ; Our simple implementation just recurs whenever the graph bifurcates.
        ; Here, we get all the recur points.
        entries (reduce into entries (for [[_ v] fgraph :when (> (count v) 1)] v))
        cases (collate-cases item-id #(oderef (get-in item-set [:shift-advances %]))
                             #(gen-advance-handler item-set %)
                             entries)]
    (if (seq cases)
      (stash-method "advance"
                    (str "with item set\n" (item-set-str item-set))
                    java.lang.Object
                    ['self 'next-val]
                    (delay `(loop [~'next-val ~'next-val]
                              (case ~'goto
                                ~@cases
                                ; If cases don't match it's probably intended for
                                ; the parent item set parser.
                                ~'next-val)))
                    ; TODO which key?
                    (item-set-key item-set))
                    ;(omm/map backlink-map))
      (do 
        (print-code "No advancer for" (item-set-str item-set))
        nil))))

(defn scanner-sym [scanner]
  (if (symbol? scanner)
    scanner
    (get-or-bind scanner identity "scanner" {})))

(defn obj-sym [obj] (get-or-bind obj identity "match-result" {}))

; === Shifts and scanning

; A code snippet for applying a rule's action to some operands, faster than 'apply
(defn apply-args [rule operands]
  (let [elisions (:elisions rule)
        match-count (:dot rule)
        raw-action (-> rule :raw-rule :action)]
    `(~(action-sym raw-action)
           ; Substitute in eager matches and operands
         ~@(loop [r [] operands operands i 0]
             (cond (= match-count i) r ; return
                   (contains? elisions i)
                   (recur (conj r (obj-sym (get elisions i))) operands (inc i))

                   true
                   (recur (conj r (first operands)) (rest operands) (inc i)))))))

; A code snippet for when an item set returns
(defn gen-return [item working-syms arg-count]
  (let [backlink-id (-> item :backlink item-id)
        action-operands (if working-syms working-syms 
                          (map (fn [i] `(aget ~'partial-match ~i))
                               (range arg-count)))]
    `(do
       (set! ~'goto ~backlink-id)
       ~(apply-args (:rule item) action-operands))))

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
        body (let [test-pairs
                   (seq (mapcat (fn [[scanner form]]
                                  `((~(scanner-sym scanner) ~'input) ~form))
                                  scanners))]
               (case (count test-pairs)
                 0 continuation
                 2 `(if ~(first test-pairs) ~(second test-pairs) ~continuation)
                 `(cond ~@test-pairs true ~continuation)))

        ; Tokens are handled with a case statement
        tokens (selector :token)
        body (if (seq tokens)
               `(case ~'input 
                  ~@(mapcat (fn [[scanner form]]
                              `(~(if (= :chars (:stream-type *opts*))
                                   (int scanner)
                                   scanner)
                              ~form)) tokens)
                  ~body)
               body)

        ; Get input if we need
        body (if (or (seq tokens) (seq scanners))
               `(let [~'input (.current ~'stream)]
                  ~body)
               body)

        ; Terminus handled first
        term (first (selector :term)) ; there can be only one
        body (if term?
               `(if (not (.hasInput ~'stream))
                  ~(if term (second term) `(fail ~'stream))
                  ~body)
               body)]
    body))

; Requires 'input, 'self, 'advancer, and 'partial-match OR non-null 'working-syms.
; Returns a code snippet for handling a state after a scanner is matched.
; Can either take action and return, or call a advancer returning the result.
(defn gen-scanner-handler [scanner item-set working-syms arg-count]
  (let [shift (get (:shift-map item-set) scanner)
        shift (if shift @shift)
        return (omm/get-vec (reduce-map item-set) scanner)
        ; TODO mode switching
        ; TODO fix long/char in json test
        scanner (if (char? scanner) (long scanner) scanner)]
   (if (> (count return) 1)
      (do
        (println "Reduce-reduce conflict in item set\n" (item-set-str item-set))
        (println "for items" (s/separate-str " " (map #(item-str-follow
                                                         % (:follow-map item-set))
                                                      return)))
        (assert (reduce-reduce? item-set))))
    (if shift
      (do
        (if (seq return)
          (do
            (println "Shift-reduce conflict in item set\n" (item-set-str item-set))
            (assert (shift-reduce? item-set))))
        [:shift [scanner
                 `(~(item-parser-sym shift false true)
                      ~'self)]])
      (if (seq return)
        [:reduce [scanner (gen-return (first return) working-syms arg-count)]]
        [:reduce [scanner nil]]))))

; For some tagged vals of the form [tag val], returns vals matching the tag
(defn untag [tagged-vals tag]
  (for [[tag1 val] tagged-vals :when (= tag1 tag)] val))

; Generates the parser fn for an item set. Can be monolithic or split
; into continuing parsers. If monolithic, will have working-syms defined.
; If split, will have argcount > 0.
(defn gen-parser [item-set working-syms argcount]
  (let [handlers (map #(gen-scanner-handler % item-set working-syms argcount)
                      (into #{} (concat (-> item-set shift-map omm/keys)
                                        (-> item-set reduce-map omm/keys))))
        return-handlers (untag handlers :reduce)
        shift-handlers (untag handlers :shift)
        next-sym (symbol (str "v" (count working-syms)))
        ; TODO safe deref? item set getter helper?
        next-cases (collate-cases item-id #(oderef (get-in item-set
                                                              [:continue-advances %]))
                                  (if working-syms
                                    ; If monolithc, splice in the next parser...
                                    (fn [item-set]
                                      (gen-parser
                                        item-set
                                        (conj working-syms next-sym) -1))
                                    ; Othwerise, it's a funcall
                                    (fn [item-set]
                                      `(~(cont-parser-sym item-set (inc argcount))
                                           ~'self ~'partial-match)))
                                  (omm/keys (:backlink-map item-set)))]

    ; Short-circuit if this is a trivial parser body.
    (if-let [a-const-reduce (const-reduce item-set)]
      (do
        (assert (empty? shift-handlers))
        (gen-return a-const-reduce working-syms argcount))

      (gen-shift-table
        ; First, should return?
        return-handlers true
        ; OK, let's shift
        (if (empty? shift-handlers)
          ; Fail early if we can't shift
          `(fail ~'stream)
          `(let [~'rval
                 ~(gen-shift-table shift-handlers false `(fail ~'stream)),
                 ; Splice in a body parser if we need
                 ~@(if-let [automaton (gen-body-automaton item-set)]
                     `(~'rval
                          (~automaton ~'self ~'rval)))
                 ; If monolithic, also get next-sym here
                 ~@(if working-syms
                     `(~next-sym ~'rval))]
             ; Save the output if we need
             ~@(if-not working-syms
                 `((aset ~'partial-match ~argcount ~'rval)))
             ; Figure out what parser to call next
             ~@(case (count next-cases)
                 2 (list (second next-cases)) ; just put in the next case
                 ; default: splice in all cases
                 `((case ~'goto ~@next-cases)))))))))

(defn gen-slow-cont-parser [item-set argcount]
  (if-let [a-const-reduce (const-reduce item-set)]
    (gen-return a-const-reduce nil argcount)
    (gen-parser item-set nil argcount)))

(defn cont-parser-sym [item-set argcount]
  (stash-method "continue"
                (str "with item set\n" (item-set-str item-set))
                java.lang.Object
                (vector 'self (with-meta 'partial-match
                                         {:tag (class (make-array Object 0))}))
                (delay (gen-slow-cont-parser item-set argcount))
                (item-set-key item-set) argcount))

(defn gen-slow-parser [item-set initial? shift?]
  ; TODO kill apply symbols
  `(let [~'partial-match (object-array ~(item-set-size item-set))]
     ~(if initial?
        (gen-parser item-set nil 0)
        `(do 
           (aset ~'partial-match 0 ~(if shift?  `(.current ~'stream) 'v0))
           ~@(if shift? `((.shift ~'stream)))
           ~(gen-parser item-set nil 1)))))

(defn code-size [l]
  (if (coll? l)
    (reduce + 0 (map code-size l))
    1))

(defn gen-parser-body [item-set initial? shift?]
  (let [initial-arglist (if (or initial? shift?) [] ['v0])
        body (if-let [a-const-reduce (const-reduce item-set)]
                ; Some parsers are trivial. If so, we short-circuit.
                (if shift?
                  `(let [~'v0 (.current ~'stream)]
                     (.shift ~'stream)
                     ~(gen-return a-const-reduce ['v0] -1))
                  (gen-return a-const-reduce initial-arglist -1))
                ; TODO this can use cleanup.
                (if shift?
                  `(let [~'v0 (.current ~'stream)]
                     (.shift ~'stream)
                     ~(gen-parser item-set ['v0] -1))
                     (gen-parser item-set initial-arglist -1)))]
    ; 160 is the optimal code size number, chosen by magic.
    ; Also if the code gets big it can hit the 64k JVM limit.
    (if (> (code-size body) 160)
      (gen-slow-parser item-set initial? shift?)
      body)))

; TODO rename
(defn item-parser-sym
  [item-set initial? shift?]
  (when item-set
    (stash-method "parse"
                  (str "with item set\n" (item-set-str item-set))
                  java.lang.Object
                  (vec (if (or shift? initial?) ['self] ['self 'v0]))
                  (delay (gen-parser-body item-set initial? shift?))
                  (item-set-key item-set) initial? shift?)))

(defn new-ns []
  (let [sym (gensym "quentin")
        r (create-ns sym)]
    (binding [*ns* r]
      (use 'clojure.core 'clearley.quentin)
      (do-imports))
    (intern r 'item-set-var-map (atom {})) ; map: seeds -> symbol
    (intern r 'ns-lock (Object.))
    r))

(defn parse-fn [grammar goal myns mem opts]
  (with-memoizer mem
    (binding [*myns* myns
              *opts* (merge default-opts opts)
              *queue* (atom [])]
      (let [method-name (item-parser-sym (build-item-sets goal grammar)
                                         true false)]
        (loop []
          ; Loop through all methods in the queue, knowing that
          ; evaluating them might add to the queue.
          (if-let [method-body (first @*queue*)]
            (do
              (swap! *queue* rest)
              ; Realize method body if it was lazy
              (code-size @method-body)
              (recur))
            (let [ppcode (gen-parser-protocols)]
              (print-code "===")
              (print-code ppcode)
              (print-code "===")
              (binding [*ns* *myns*]
                (try
                  (eval ppcode)
                  ; TODO doesn't need eval in namespace?
                  (eval `(defn ~'parse-state [stream#]
                           (~method-name (~'->Parser stream# 0)))))
                (catch Exception e
                  (binding [*out* *err* ; TODO
                            *print-meta* true]
                    (println "Exception compiling:" (.getMessage e))
                    (clojure.pprint/pprint ppcode))
                  (throw (RuntimeException. "Exception compiling parser" e))))
              @(ns-resolve *myns* 'parse-state))))))))

(defn parse [a-parse-fn input myns mem opts]
  (with-memoizer mem
    (try
      (binding [*myns* myns
                *opts* (merge default-opts opts)]
        (a-parse-fn (new-stream input)))
      ; TODO parsing should exception on failure
      (catch Exception e (clojure.stacktrace/print-stack-trace e) nil))))
