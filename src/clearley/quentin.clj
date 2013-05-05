(ns clearley.quentin
  (require [uncore.throw :as t]
           [uncore.str :as s]
           [clearley.rules :as rules]
           [uncore.collections.worm-ordered-multimap :as omm]
           clojure.stacktrace clojure.pprint)
  (use clearley.clr uncore.core))

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

; The core abstraction is
; Parse stream handler: parse stream -> parse stream
(declare continue-parsing get-item-parser-ref get-continuer-ref item-parser-sym)

(defn fail [stream] (t/RE "Failure to parse at position: " (get-pos stream)))

(defn get-all-advances [{backlink-map :backlink-map :as item-set} seed? mem]
  (let [all-backlinks (omm/keys backlink-map)]
    (into {}
          (remove (fn-> second nil?)
                  (map (fn [backlink]
                         [backlink (get-item-parser-ref
                                     (advance-item-set item-set backlink seed?)
                                                  mem)])
                       all-backlinks)))))

(def nilref (delay nil))

(defn continuer [item-set myns]
  (let [shift-advances (get-all-advances item-set false myns)
        continue-advances (get-all-advances item-set true myns)]
    (fn [result]
      (let [returned (:backlink (peek (get-output result)))
            shift-advance @(get shift-advances returned nilref)
            continue-advance @(get continue-advances returned nilref)]
        (if (and shift-advance continue-advance)
          (println "Stack split in item set\n" (item-set-str item-set)
                   "for return value " (item-str returned)))
        (cond shift-advance
              ; Keep us on the stack, see what comes next
              (recur (shift-advance result))

              ; Tail-call (not really) the next parser fn
              continue-advance (continue-advance result))))))

; Gets the value referred to by obj, creating the value with the given factory
; if it can't be found in the master ns-map. Will map it in ns-map prefixed
; with the given str.
(defn get-or-bind [obj factory myns a-str]
  (let [item-set-var-map @(ns-resolve myns 'item-set-var-map)
        ns-lock @(ns-resolve myns 'ns-lock)]
    (locking ns-lock
      (let [sym (symbol (str a-str "-" (count (get @item-set-var-map a-str {}))))]
        (if-let [sym0 (get-in @item-set-var-map [a-str obj])]
          sym0
          (let [r (factory obj)]
            (intern myns sym r)
            (swap! item-set-var-map #(assoc-in % [a-str obj] sym))
            sym))))))

(defn continuer-ref [item-set myns]
  (get-or-bind item-set #(continuer % myns) myns "continuer"))

; Anaphoric, requiring a 'current-input, 'stream, and a 'continuer--the current input
; may be ::term.
; The code that should be executed if a scanner is matched.
; Can either return or call a continuer returning the result.
(defn get-action-code [scanner item-set action-map continuer-sym myns]
  (let [shift (get-actions-for-tag scanner action-map :shift)
        return (get-actions-for-tag scanner action-map :return)]
    ; TODO inspect for shift-shift, shift-reduce
    (if (> (count return) 1)
      (do
        (println "Reduce-reduce conflict in item set\n" (item-set-str item-set))
        (println "for items" (s/separate-str " " (map item-str-follow return)))))
    (if (seq shift)
      (do
        (if (seq return)
          (println "Shift-reduce conflict in item set\n" (item-set-str item-set)))
        `(~continuer-sym (@~(item-parser-sym (map advance-item shift) myns)
                              (shift ~'stream ~'current-input))))
      ; TODO perhaps return an item num
      (if (seq return)
        `(return ~'stream ~(get-or-bind (first return) identity myns "item"))))))

; A cond chain for all actions
(defn get-actions-code [item-set action-map continuer-sym myns]
  (let [pair-constructor (fn [scanner]
                           [(list (get-or-bind scanner identity myns "scanner")
                                  'current-input)
                            (get-action-code scanner item-set action-map continuer-sym
                                             myns)])]
    `(if (seq ~'istream)
       (cond ~@(mapcat pair-constructor (remove #(= :clearley.clr/term %)
                                                (omm/keys action-map)))
             true (fail ~'stream))
       ; Special handling for terminus
       ~(if-let [term-action-code (get-action-code :clearley.clr/term item-set
                                                   action-map continuer-sym myns)]
          term-action-code
          `(fail ~'stream)))))

; Sets up all the stuff to execute get-actions-code
(defn gen-parser-body [seeds myns]
  (let [more-seeds (mapcat #(eager-advances % false) seeds)
        item-set (closed-item-set more-seeds)
        action-map (action-map more-seeds item-set)
        continuer-sym (continuer-ref item-set myns)]
    `(fn [~'stream]
       (let [~'istream (get-input ~'stream)
             ~'current-input (first ~'istream)]
         ~(get-actions-code item-set action-map continuer-sym myns)))))

(defn get-item-parser* [seeds myns]
  ;(clojure.pprint/pprint (gen-parser-body seeds myns))
  (let [ns-lock @(ns-resolve myns 'ns-lock)]
    (locking ns-lock
      (binding [*ns* myns]
        (eval (gen-parser-body seeds myns))))))

(defn item-parser-sym [seeds myns]
  (get-or-bind seeds #(delay (get-item-parser* % myns)) myns "item-set"))

(defn get-item-parser-ref [seeds myns]
  (if (seq seeds)
    @(ns-resolve myns (get-or-bind seeds #(delay (get-item-parser* % myns))
                                   myns "item-set"))
    (delay nil)))

(defn new-ns []
  (let [sym (gensym "quentin")
        r (create-ns sym)]
    ;(remove-ns sym) ; TODO
    (binding [*ns* r] (use 'clojure.core 'clearley.quentin))
    (intern r 'item-set-var-map (atom {})) ; map: seeds -> symbol
    (intern r 'ns-lock (Object.))
    r))

(defn parse [grammar goal input myns mem]
  (try
    (binding [rules/*mem-atom* mem]
      (@(get-item-parser-ref [(goal-item goal grammar)] myns) (parse-stream input)))
    (catch RuntimeException e (clojure.stacktrace/print-stack-trace e)))) ; TODO

(defn pprint-parse-stream [{:keys [input output] :as stream}]
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
