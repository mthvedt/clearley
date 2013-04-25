(ns clearley.rules
  "Back-end stuff for Clearley. Work in progress with unstable API."
  (require clojure.string uncore.rpartial
           [uncore.throw :as t]
           [uncore.str :as s])
  (use uncore.core))

; TODO this ns is such a mess
; in particular we should have once-only invocations for creating rules

; TODO maybe we can nuke this entirely; move it into GLR

; TODO merge into core?
(defrecord Match [rule submatches])

(defn match [rule submatches]
  (Match. rule submatches))

; The instrumented core abstraction
; TODO a lot of records not neccesary?
(defrecord CfgRule [name tag value dot toplevel? original null-results])

(defn advance [cfg-rule]
  (update cfg-rule :dot inc))

(defn cfg-rule [{:keys [name tag value] :as rule}]
  (CfgRule. name tag value 0 true rule {}))

(defn rule? [x]
  (instance? clearley.rules.CfgRule x))

(defn goal? [rule]
  (= (:name rule) ::goal))

; TODO figure out where to put this
(defn goal-rule [r]
  (CfgRule. ::goal :seq [r] 0 true
            {:name :goal, :tag :seq, :value [r], :action identity} {}))

(defn get-original [{:keys [null-results original] :as cfg-rule}]
  (let [a (:action original)]
    (if (seq null-results)
      (assoc original :action (uncore.rpartial/gen-rpartial a null-results))
      original)))

(defn null-advance [rule result]
  (let [dot (:dot rule)]
    (update-all rule {:dot inc, :null-results #(assoc % dot result)})))

; Anaphoric, uses 'value and 'dot
; Makes sure a rule has only one subrule
(defmacro check-singleton [tag result]
  `(if (= (count ~'value) 1)
     ~result
     (t/IAE ~tag "must have only one subrule")))

; For rules that only match one clause, once
(defmacro predict-singleton [tag result]
  `(check-singleton ~tag (if (= ~'dot 1) [] ~result)))

; Predicts a rule, returning a seq [rule | fn]
; TODO just return a rule, not a fn. also get rid of 'rule?
; glr can check for scaners directly
(defmulti predict (fn [rule _] (:tag rule)))
(defmethod predict :symbol [{:keys [dot value]} grammar]
  (map cfg-rule (predict-singleton :symbol [(get grammar (first value))])))
(defmethod predict :or [{:keys [dot value]} grammar]
  (map cfg-rule (if (= dot 1) [] value)))
(defmethod predict :seq [{:keys [value dot]} grammar]
  (if (= dot (count value)) []
    [(cfg-rule (get value dot))]))
(defmethod predict :star [{:keys [value dot]} grammar]
  (map cfg-rule (check-singleton :star value)))
(defmethod predict :scanner [{:keys [value dot]} _]
  (predict-singleton :scanner value))
; TODO kill
(defrecord TokenScanner [token]
  clojure.lang.IFn
  (invoke [_ val] (= token val))
  (applyTo [_ args] (= token (first args))))
(defmethod predict :token [{:keys [value dot]} _]
  (predict-singleton :token [(TokenScanner. (first value))]))

; Is this rule complete?
(defmulti is-complete? :tag)
(defmethod is-complete? :star [{:keys [dot]}] true)
(defmethod is-complete? :seq [{:keys [value dot]}]
  (= dot (count value)))
(defmethod is-complete? :default [{:keys [dot]}]
  (= 1 dot))

(defn clause-strs
  ([clauses] (clause-strs clauses 0))
  ([clauses dot]
   (let [clause-strs (map pr-str clauses)]
     (concat (take dot clause-strs)
             (if (zero? dot) [] ["*"])
             (drop dot clause-strs)))))

(defn clause-name [clause]
  (if (:name clause) (:name clause) (pr-str clause)))

; TODO rule-str for paren'd rules is wrong, dot should be outside parens
(defmulti rule-str :tag)
(defmethod rule-str :seq [{:keys [name value dot]}]
  (let [clause-strs (map clause-name value)]
    (s/separate-str " " (concat [name "->"] (take dot clause-strs)
                                ["*"] (drop dot clause-strs)))))
(defmethod rule-str :default [{:keys [name tag value dot]}]
  (str name " -> " tag " ("
       (s/separate-str " " (map clause-name value))
       ")"
       (if (zero? dot) "" " *")))

(def ^:dynamic *breadcrumbs*)
; Basically, this does an LL match on the empty string
(defn null-result* [rule grammar]
  (if (rule? rule)
    (if (contains? *breadcrumbs* rule)
      (get *breadcrumbs* rule)
      (do
        (set! *breadcrumbs* (assoc *breadcrumbs* rule nil))
        (let [r (if (is-complete? rule)
                  (match (:original rule) [])
                  (if-let [r (some identity (map #(null-result* % grammar)
                                                 (predict rule grammar)))]
                    (if-let [r2 (null-result* (advance rule) grammar)]
                      (match (:original rule)
                             (apply vector r (:submatches r2))))))]
          (set! *breadcrumbs* (assoc *breadcrumbs* rule r))
          r)))
    nil))
(defn null-result [rule grammar]
  (binding [*breadcrumbs* {}]
    (null-result* rule grammar)))

(defn take-action* [match]
  (if (nil? match)
    (throw (RuntimeException. "Failure to parse"))
    (let [{:keys [rule submatches]} match
          subactions (map take-action* submatches)
          action (get rule :action (fn [] rule))
          action (if (= :token (:tag rule)) (fn [_] (action)) action)]
      ; TODO shouldn't need the above, naked tokens should not appear
      ; on the stack.
      (try
        (apply action subactions)
        (catch clojure.lang.ArityException e
          (throw (RuntimeException. (str "Wrong # of params taking action for rule "
                                         (if (rule? rule)
                                           (rule-str rule)
                                           (pr-str rule)) ", "
                                         "was given " (count subactions))
                                    e)))))))

(defn eager-advance [rule grammar]
  (if-let [eager-match (some identity (map #(null-result % grammar)
                                           (predict rule grammar)))]
    (null-advance rule (take-action* eager-match))))
