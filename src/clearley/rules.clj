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

; Core abstraction:
; * Rule. Looks like this: clause-name -> clause. Can be advanced multiple times.
; * Clause. The atoms. Predicts a seq of [rule | scanner].
; Some clauses can be predicted inline; others must be turned into rules.

(defrecord CfgRule [name type clauses dot toplevel? original null-results])

(defn clause-type [clause]
  (cond (map? clause) :rule-map
        (seq? clause) (first clause)
        (symbol? clause) :symbol
        true :token))

(defn advance [cfg-rule]
  (update cfg-rule :dot inc))

(defn cfg-from-defrule [name {:keys [value action] :as rule}]
  (CfgRule. name (first value) (vec (rest value)) 0 true rule {}))

(defn goal-rule [sym]
  (CfgRule. ::goal :seq [sym] 0 false sym {}))

(defn goal? [rule]
  (= (:name rule) ::goal))

(defn rule? [x]
  (instance? clearley.rules.CfgRule x))

; This protocool only exists so we can have actions that = each other
; and I can't figure out how to impl ifn with rest args
(defprotocol Actionable
  (to-action [self]))
(extend-protocol Actionable
  Object
  (to-action [self] (fn [& args] (apply self args))))
(defrecord Constantly [object]
  Actionable
  (to-action [self] (fn [& args] object)))
(defn list-identity [& args] args)

(defn action [rule]
  (to-action (get rule :action (fn [] rule))))

(defn wrap-rule [rule]
  (if (map? rule)
    rule
    {:value rule}))

(defn get-original [{:keys [null-results original] :as cfg-rule}]
  (let [a (action original)]
    (if (seq null-results)
      (assoc (wrap-rule original)
             :action
             (uncore.rpartial/gen-rpartial a null-results))
      original)))

(defn null-advance [rule result]
  (let [dot (:dot rule)]
    (update-all rule {:dot inc, :null-results #(assoc % dot result)})))

(defmacro hierarchy [& derivations]
  `(-> (make-hierarchy) ~@(map #(cons derive %) derivations)))

(def cfg-hierarchy (hierarchy 
                     (:or ::any) (:token ::any) (:scanner ::any)
                     (:symbol ::any) (::sequential ::any) (:rule-map ::any)
                     ; Special handling here
                     (:seq ::sequential) (:star ::sequential)))

; Makes a clause into a rule
(defmulti to-rule (fn [_ x] (clause-type x)) :hierarchy #'cfg-hierarchy)
(defmethod to-rule :rule-map [name clause]
  (cfg-from-defrule name clause))
(defmethod to-rule :symbol [name clause]
  (CfgRule. name (clause-type clause) (vector clause) 0 false
            {:clauses (vector clause) :action identity} {}))
(defmethod to-rule :token [name clause]
  (CfgRule. name (clause-type clause) (vector clause) 0 false
            {:clauses (vector clause) :action (Constantly. clause)} {}))
(defmethod to-rule ::sequential [name tagged-clause]
  (CfgRule. name (first tagged-clause) (vec (rest tagged-clause)) 0
            false {:clauses tagged-clause :action list-identity} {}))
(defmethod to-rule ::any [name tagged-clause]
  (CfgRule. name (first tagged-clause) (vec (rest tagged-clause)) 0
            false {:clauses tagged-clause :action identity} {}))

; Returns a seq, [rule | fn]. if fn, is a scanner.
(defmulti predict-clause (fn [clause _] (clause-type clause))
   :hierarchy #'cfg-hierarchy)
(defmethod predict-clause :symbol [clause grammar]
  (let [got (get grammar clause)
        type (first got)]
    (if (= type :defrule) ; Special type used only by defrule
      (map #(cfg-from-defrule (str clause) %) (rest got))
      [(to-rule (str clause) (get grammar clause))])))
(defmethod predict-clause :or [clause _]
  (map #(to-rule "anon-or" %) (rest clause)))
(defmethod predict-clause :rule-map [clause _]
  [(cfg-from-defrule "anon" clause)])
(defmethod predict-clause ::any [clause _]
  [(to-rule "anon" clause)])

; Predicts a rule, returning a seq [rule | fn] as in predict-clause
; TODO names in clauses
(defmulti predict (fn [rule _] (:type rule)) :hierarchy #'cfg-hierarchy)
; TODO unify with star
(defmethod predict :symbol [{:keys [dot type clauses]} grammar]
  (if (= dot 1) []
    (predict-clause (first clauses) grammar)))
(defmethod predict :or [{:keys [dot type clauses]} grammar]
  (if (= dot 1) []
    (predict-clause (cons type clauses) grammar)))
(defmethod predict :seq [{:keys [clauses dot]} grammar]
  (if (= dot (count clauses)) []
    (predict-clause (get clauses dot) grammar)))
(defmethod predict :star [{:keys [clauses dot]} grammar]
  (if (= (count clauses) 1)
    (predict-clause (first clauses) grammar)
    (t/IAE ":star accepts only one rule")))
(defmethod predict :scanner [{:keys [clauses]} _]
  [(first clauses)])
(defrecord TokenScanner [token]
  clojure.lang.IFn
  (invoke [_ val] (= token val))
  (applyTo [_ args] (= token (first args))))
(defmethod predict :token [{:keys [clauses dot]} _]
  (if (= dot 1) []
    [(TokenScanner. (first clauses))]))

; Is this rule complete?
(defmulti is-complete? :type :hierarchy #'cfg-hierarchy)
(defmethod is-complete? :star [{:keys [dot]}] true)
(defmethod is-complete? :seq [{:keys [clauses dot]}]
  (= dot (count clauses)))
(defmethod is-complete? ::any [{:keys [dot]}]
  (= 1 dot))

(def ^:dynamic *breadcrumbs*)
; Basically, this does an LL match on the empty string
(defn null-result* [rule grammar]
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
        r))))
(defn null-result [rule grammar]
  (binding [*breadcrumbs* {}]
    (null-result* rule grammar)))

(defn eager-advance [rule grammar]
  (if-let [eager-match (some identity (map #(null-result % grammar)
                                           (predict rule grammar)))]
    (null-advance rule (take-action eager-match))))

(defn clause-strs
  ([clauses] (clause-strs clauses 0))
  ([clauses dot]
   (let [clause-strs (map pr-str clauses)]
     (concat (take dot clause-strs)
             (if (zero? dot) [] ["*"])
             (drop dot clause-strs)))))

; TODO rule-str for paren'd rules is wrong, dot should be outside parens
(defmulti rule-str :type :hierarchy #'cfg-hierarchy)
(defmethod rule-str :seq [{:keys [name clauses dot]}]
  (s/separate-str " " (concat [name "->"] (clause-strs clauses dot))))
(defmethod rule-str ::any [{:keys [name type clauses dot]}]
  (str name " -> ("
       (s/separate-str " " (cons type (clause-strs clauses)))
       ")"
       (if (zero? dot) "" " *")))
