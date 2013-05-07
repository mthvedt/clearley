(ns clearley.quentin
  (require [uncore.throw :as t]
           [uncore.str :as s]
           [clearley.rules :as rules]
           [uncore.collections.worm-ordered-multimap :as omm]
           clojure.stacktrace clojure.pprint)
  (import clearley.ParseState clearley.TransientParseState)
  (use clearley.clr uncore.core))
; TODO what does aot do?
; TODO eliminate state, then have parser return results.

(defn parse-stream [input]
  (TransientParseState. (seq input)))

; The core abstraction is
; Parse stream handler: parse stream -> parse stream
(declare continue-parsing item-parser-ref item-parser-sym)

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

(def nilref (delay nil))

(defn fail [^ParseState stream] (t/RE "Failure to parse at position: " (.pos stream)))

; Generate a map from return value to item set parser
(defn get-all-advances [{backlink-map :backlink-map :as item-set} seed? myns]
  (let [all-backlinks (omm/keys backlink-map)]
    (into {}
          (filter second
                  (map (fn [backlink]
                         [backlink (item-parser-ref
                                     (advance-item-set item-set backlink seed?)
                                                  myns)])
                       all-backlinks)))))

; Anaphoric, needs a ~'result and a wrapping loop/recur and a ~'state
; For a backlink, calls the item-parser represented by that advance,
; and either looks up and recurs to a main branch, or calls the continuance.
(defn gen-advance-handler [item-set backlink branch-code-map myns]
  (let [shift-advance-seeds (advance-item-set item-set backlink false)
        continue-advance-seeds (advance-item-set item-set backlink true)]
    (if (seq shift-advance-seeds)
      (do
        (if (seq continue-advance-seeds)
          (println "Stack split in item set\n" (item-set-str item-set)
                   "for return value " (item-str backlink)))
        ; Create a subtable
        `(case (.lastReturnId (@~(item-parser-sym shift-advance-seeds myns)
                                      ~'state))
           ~@(mapcat
               (fn [id continuance]
                 `(~id (recur ~(get branch-code-map (:backlink continuance)))))
           (range) shift-advance-seeds)))
        ; Just call the continuance
        `(@~(item-parser-sym continue-advance-seeds myns) ~'state))))

; Anaphoric: requires ~'branch
; TODO we can make this smaller since we know
; a token consuming shift only happens once
; For an item set, builds the 'continuing table' which handles all steps after
; the first initial shift. Implemented as a loop/recur with a case branch.
; The branch table matches all possible advances (advancing shifts, continues)
; to the appropriate branch. The appropriate branch either sets up a subtable
; (if it is a shift) to capture the result and goto the main branch
; or calls the continue.
(defn gen-advance-loop [{backlink-map :backlink-map :as item-set} branch-nums myns]
  (when-not (seq branch-nums) nil);(assert false)) TODO
  `(loop [~'branch ~'branch]
     (case ~'branch
       ~@(mapcat (fn [[backlink id]]
                   `(~id ~(gen-advance-handler item-set backlink branch-nums myns)))
                 branch-nums))))

(defn advance-looper [item-set branch-nums myns]
  (let [r `(fn [~(apply symbol '(^ParseState state)) ~'branch]
             ~(gen-advance-loop item-set branch-nums myns))]
    (clojure.pprint/pprint r)
    (binding [*ns* myns]
      (eval r))))

#_(defn advance-looper [item-set myns]
  (let [shift-advances (get-all-advances item-set false myns)
        continue-advances (get-all-advances item-set true myns)]
    (fn [^ParseState result]
      (let [returned (:backlink (.peek result))
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

(defn get-advancer-sym [item-set branch-nums myns]
  (get-or-bind item-set #(advance-looper % branch-nums myns) myns "advancer"))

; For an initial shift, looks up a branch num to branch to in the continuing loop.
(defn gen-shift-subtable [item-set shift-item-seeds branch-code-map myns]
  `(case (.lastReturnId (@~(item-parser-sym shift-item-seeds myns)
                                (.shift ~'state ~'input)))
     ~@(mapcat
         (fn [id seed]
           `(~id (~(get-advancer-sym item-set branch-code-map myns)
                     ~'state ~(get branch-code-map (:backlink seed)))))
         (range) shift-item-seeds)))

; Anaphoric, requiring a 'input, 'state, and a 'advancer--current input
; may be ::term.
; The code that should be executed if a scanner is matched.
; Can either return or call a advancer returning the result.
(defn gen-scanner-handler [scanner item-set action-map branch-nums myns]
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
        (gen-shift-subtable item-set (map advance-item shift) branch-nums myns))
      ; TODO perhaps return an item num
      (if (seq return)
        `(.reduce ~'state ~(get-or-bind (first return) identity myns "item")
                  ~(-> return first :seed-num))))))

; A cond chain for all actions
(defn gen-initial-shift [item-set action-map branch-nums myns]
  (let [cond-pair-fn (fn [scanner]
                       [(list (get-or-bind scanner identity myns "scanner") 'input)
                        (gen-scanner-handler scanner item-set action-map
                                             branch-nums myns)])]
    `(if (seq ~'istream)
       (cond ~@(mapcat cond-pair-fn (remove #(= :clearley.clr/term %)
                                            (omm/keys action-map)))
             true (fail ~'state))
       ; Special handling for terminus
       ~(if-let [term-handler (gen-scanner-handler :clearley.clr/term item-set
                                                   action-map branch-nums myns)]
          term-handler
          `(fail ~'state)))))

; Sets up all the stuff to execute gen-initial-shift
(defn gen-parser-body [seeds myns]
  (let [more-seeds (mapcat #(eager-advances % false) seeds)
        item-set (closed-item-set more-seeds)
        action-map (action-map item-set)
        branch-nums (zipmap (omm/keys (:backlink-map item-set)) (range))]
    (let [r `(fn [~(apply symbol '(^ParseState state))]
               (let [~'istream (.input ~'state)
                     ~'input (first ~'istream)]
                 ; TODO rename
                 ~(gen-initial-shift item-set action-map branch-nums myns)))
          ;(println (item-set-str item-set))
          ;(clojure.pprint/pprint r)
          f (binding [*ns* myns] (eval r))]
      (fn [& args]
        (println "Parsing item set")
        (print (item-set-str item-set))
        (println "with code")
        (clojure.pprint/pprint r)
        (apply f args)))))

(defn get-item-parser* [seeds myns]
  ; TODO get rid of myns everywhere!
  (let [ns-lock @(ns-resolve myns 'ns-lock)]
    (locking ns-lock
      (gen-parser-body seeds myns))))

(defn item-parser-sym [seeds myns]
  (get-or-bind seeds #(delay (get-item-parser* % myns)) myns "item-set"))

; Gets a delay pointing to an item parser for the given seeds
(defn item-parser-ref [seeds myns]
  (if (seq seeds)
    @(ns-resolve myns (get-or-bind seeds #(delay (get-item-parser* % myns))
                                   myns "item-set"))
    (delay nil)))

(defn new-ns []
  (let [sym (gensym "quentin")
        r (create-ns sym)]
    ;(remove-ns sym) ; TODO
    (binding [*ns* r]
      (use 'clojure.core 'clearley.quentin)
      (import 'clearley.ParseState))
    (intern r 'item-set-var-map (atom {})) ; map: seeds -> symbol
    (intern r 'ns-lock (Object.))
    r))

(defn parse [grammar goal input myns mem]
  (try
    (binding [rules/*mem-atom* mem]
      (@(item-parser-ref [(goal-item goal grammar)] myns) (parse-stream input)))
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

; TODO make thread safe
(defn finalize-state [^ParseState state]
  (reduce-ostream (if state (.output state) nil)))
