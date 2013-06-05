(ns clearley.rules
  (require clearley.grammar clojure.string clojure.set
           [uncore.throw :as t]
           [uncore.str :as s])
  (use uncore.core uncore.memo))
; Core stuff for context free grammar parsing.

; A rule with instrumentation
(defrecord CfgRule [dot raw-rule elisions grammar advance eager-advance])

(declare rule-str cfg-rule populate-cfg-rule)

(defn goal-rule [sym grammar]
  (cfg-rule {:name ::goal, :tag :seq,
             :value [(clearley.grammar/normalize sym nil)],
             :action clearley.grammar/fast-identity}
            grammar))

; Helpers for get-nulled-action
(defn interleave-map [coll i->vals]
  (loop [r [] coll coll i 0]
    (if (contains? i->vals i)
      ;(if (= ::ignore (get i->vals i))
        ;(recur r coll (inc i))
        (recur (conj r (get i->vals i)) coll (inc i))
      (if (seq coll)
        (recur (conj r (first coll)) (rest coll) (inc i))
        r))))

(defn rpartial [f themap]
  (fn [& args]
    (apply f (interleave-map args themap))))

; For a rule that has a null-advance, returns an action with the null results
; "spliced in". Note that Quentin does not use this because it generates cod instead.
(defnmem get-nulled-action
  [{{:keys [action] :as raw-rule} :raw-rule :keys [elisions]}]
  (if (seq elisions)
    (assoc raw-rule :action (rpartial action elisions))
    raw-rule))

; === Predicting rules ===
; A 'predction' refers to a subrule that the parent rule wants to match.
; When the rule is matched the parser may 'advance' a rule.
; A rule is 'complete' when it is 100% matched.

; Anaphoric, uses 'value and 'dot
; Makes sure a rule has only one subrule
(defmacro check-singleton [tag result]
  `(if (= (count ~'value) 1)
     ~result
     (t/IAE ~tag "must have only one subrule")))

; Predicts rule that can only be advanced once
(defmacro predict-oneshot [result]
  `(when-not (= ~'dot 1) ~result))

; Predicts a rule, returning a seq [rule | fn]
(defmulti predict* (fn-> :raw-rule :tag))
(defmethod predict* :symbol [{dot :dot, grammar :grammar, {value :value} :raw-rule
                              :as rule}]
  (if-let [got (get grammar (first value))]
    (predict-oneshot (check-singleton :symbol [got]))
    (t/RE "Cannot find symbol in grammar: " (first value)
          " for rule: " (rule-str rule))))
(defmethod predict* :or [{dot :dot, grammar :grammar, {value :value} :raw-rule}]
  (predict-oneshot value))
(defmethod predict* :seq [{dot :dot, grammar :grammar, {value :value} :raw-rule}]
  (when-not (= dot (count value))
    [(get value dot)]))
#_(defmethod predict* :star [{dot :dot, grammar :grammar, {value :value} :raw-rule}]
    (check-singleton :star value))
(defmethod predict* :scanner [_] [])
(defmethod predict* :token [_] [])
(defmethod predict* :default [rule]
  (t/RE "Don't know how to predict:" (pr-str rule)))

(defnmem predict [{grammar :grammar :as a-rule}]
  (map #(cfg-rule % grammar) (predict* a-rule)))

(defmulti terminal (fn-> :raw-rule :tag))
(defmethod terminal :token [{dot :dot {[value] :value} :raw-rule}]
  (predict-oneshot [:token value]))
(defmethod terminal :scanner [{dot :dot {[value] :value} :raw-rule}]
  (predict-oneshot [:scanner value]))
(defmethod terminal :default [_] nil)

(defmulti is-complete? (fn-> :raw-rule :tag))
;(defmethod is-complete? :star [{:keys [dot]}] true)
(defmethod is-complete? :seq [{dot :dot {value :value} :raw-rule}]
  (= dot (count value)))
(defmethod is-complete? :default [{:keys [dot]}] (= 1 dot))

(defmulti rule-str (fn-> :raw-rule :tag))
(defmethod rule-str :seq [{dot :dot, {:keys [name value]} :raw-rule :as rule}]
  (str name " -> "
       (if-let [clause-strs (seq (map :name value))]
         (cond (= dot 0) (s/separate-str " " clause-strs)
               (is-complete? rule) (s/separate-str " " (concat clause-strs ["✓"]))
               true (s/separate-str " " (concat (take dot clause-strs) ["•"]
                                                (drop dot clause-strs))))
         "(empty)")))

(defn singleton-rule-str [{dot :dot, {:keys [name tag]} :raw-rule} clause-str]
  (str name " -> " tag " " clause-str (if (zero? dot) "" " ✓")))
(defmethod rule-str :or [{{value :value} :raw-rule :as rule}]
  (singleton-rule-str rule (str "(" (s/separate-str " " (map :name value)) ")")))
#_(defmethod rule-str :star [{{value :value} :raw-rule :as rule}]
  (singleton-rule-str rule (:name (first value))))
(defmethod rule-str :default [{{value :value} :raw-rule :as rule}]
  (singleton-rule-str rule (pr-str (first value))))

(defn advance [cfg-rule]
  (assert (not (is-complete? cfg-rule)))
  (let [;predictions (predict cfg-rule)
        ;prediction (if (= 1 (count predictions)) (first predictions))
        ; disallow hidden subrules if >1 prediction, semantics don't make sense here
        ;_ (when-not prediction (if (some (fn-> :raw-rule :hidden?) predictions)
         ;                        (println "Warning: hidden rule inside"
          ;                                (rule-str cfg-rule)
           ;                               "being ignored")))
        ;cfg-rule (if (:hidden? (:raw-rule prediction))
         ;          (update cfg-rule :elisions #(assoc % (:dot cfg-rule) ::ignore))
          ;         cfg-rule)
        cfg-rule (update cfg-rule :dot inc)]
    (populate-cfg-rule cfg-rule)))

(defn goal? [cfg-rule]
  (and (is-complete? cfg-rule)
       (-> cfg-rule :raw-rule :name (= ::goal))))

; === First sets, follow sets, and null matches

(defrecord Match [rule submatches])
(def match ->Match)

; TODO: test something of the form a -> x -> b -> y -> a for legal/illegal matches,
; where x and y are nullable.
(def ^:dynamic *breadcrumbs*)
; Basically, this does a top-down match on the empty string for a rule.
; If more than one match is possible, returns the first one (silently!)
; TODO make this not silent
(defn null-result* [rule]
  (if (contains? *breadcrumbs* rule)
    (get *breadcrumbs* rule)
    (do
      ; Avoid infinite recursion
      (set! *breadcrumbs* (assoc *breadcrumbs* rule nil))
      (let [r (if (is-complete? rule)
                (match (:raw-rule rule) [])
                (if-let [r (some identity (map null-result* (predict rule)))]
                  (if-let [r2 (null-result* @(:advance rule))]
                    (match (:raw-rule rule)
                           (apply vector r (:submatches r2))))))]
        (set! *breadcrumbs* (assoc *breadcrumbs* rule r))
        r))))

(defnmem null-result [^CfgRule rule]
  (when rule
    (binding [*breadcrumbs* {}]
      (null-result* rule))))

; first sets can't be calculated lazily by clr
(def ^:dynamic *breadcrumbs-firsts*)
; Gets the first set of an item, including ::empty if item is nullable
(defn- first-set* [rule]
  (cond
    (not rule) #{}
    (terminal rule) #{(terminal rule)}
    ; The first set of a rule contains the first sets of its predicted subrules.
    ; The 'canon first set' is a full first set of a subrule (as opposed to the
    ; partial ones this fn calculates, because it terminates on recursion)
    (lookup ::canon-first-set rule) (lookup ::canon-first-set rule)
    (get *breadcrumbs-firsts* rule) (get *breadcrumbs-firsts* rule)
    true
    (do
      ; Avoid infinite recursion by noting which rules are on the stack.
      (set! *breadcrumbs-firsts* (assoc *breadcrumbs-firsts* rule #{}))
      (let [r (apply clojure.set/union (map first-set* (predict rule)))
            r (if (or (null-result rule)
                      (some identity (map null-result (predict rule))))
                (clojure.set/union r (first-set* @(:advance rule)))
                r)]
        (set! *breadcrumbs-firsts* (assoc *breadcrumbs-firsts* rule r))
        r))))

(defn first-set [^CfgRule rule]
  (if-let [r (lookup ::canon-first-set rule)]
    r
    (binding [*breadcrumbs-firsts* {}]
      (save! ::canon-first-set rule (first-set* rule)))))

; TODO delete
; For a rule R with follow X that predicts subrule R1, caclulates the follow for R1.
(defn follow-first [^CfgRule rule parent-follow]
  (let [follow-set @(:follow-first rule)]
    (if (null-result @(:advance rule))
      (conj follow-set parent-follow)
      follow-set)))

(defn take-action* [match]
  (if (nil? match)
    (throw (RuntimeException. "Failure to parse"))
    (let [{:keys [rule submatches]} match
          subactions (map take-action* submatches)
          action (get rule :action)
          action (if (symbol? action) (resolve action) action)]
      (try
        (apply action subactions)
        (catch clojure.lang.ArityException e
          (throw (RuntimeException. (str "Wrong # of params taking action for rule "
                                         (rule-str rule) ", "
                                         "was given " (count subactions))
                                    e)))))))

; Null advance: for advancing a rule with a nullable subrule.
; See Aycock+Horspool Practical Earley Parsing
(defn- null-advance [rule result]
  (let [dot (:dot rule)]
    (populate-cfg-rule
      (update-all rule {:dot inc, :elisions #(assoc % dot result)}))))

(defn eager-advance [rule]
  (when-let [eager-match (some identity (map null-result (predict rule)))]
    (null-advance rule (take-action* eager-match))))

; Two reasons for this... one is performance, other is so that
; any advance/eager-advance is only constructed once. This matters for example
; for eager matches we store that don't obey =
; TODO is this true? defnmem should take care of it
(defn populate-cfg-rule [r]
  (let [r (assoc r :eager-advance (delay (eager-advance r)))
        r (assoc r :advance (delay (if-not (is-complete? r) (advance r))))
        ; TODO not necessary
        r (assoc r :follow-first (delay (first-set @(:advance r))))]
    r))

; Construct a cfg rule from an initial rule. These are the only ones we need
; to deduplicate
(def cfg-rule 
  (fcache (fn [rule _] rule)
          (fn [rule grammar]
            (populate-cfg-rule (->CfgRule 0 rule {} grammar nil nil)))))
