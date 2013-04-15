(ns clearley.rules
  "Back-end stuff for Clearley. Work in progress with unstable API."
  (require clojure.string
           [uncore.throw :as t]
           [uncore.str :as s])
  (use uncore.core))

; TODO maybe we can nuke this entirely; move it into GLR

; TODO merge into core?
(defrecord Match [rule submatches])

(defn match [rule submatches]
  (Match. rule submatches))

; Core abstraction:
; * Rule. Looks like this: clause-name -> clause. Can be advanced multiple times.
; * Clause. The atoms. Predicts a seq of [rule | scanner].
; Some clauses can be predicted clauseable; others must be turned into rules.
; TODO action
; TODO passthrough matches?
;
; Iron rule of clauses: One clause -> one match.
; They're all just combinators.
(defrecord CfgRule [name type clauses dot toplevel? original])

(defn rule-str [{:keys [name clauses dot]}]
  (let [clause-strs (map pr-str clauses)]
    (s/separate-str " " (concat [name "->"]
                                (take dot clause-strs)
                                (if (zero? dot) [] ["*"])
                                (drop dot clause-strs)))))

(defn advance [cfg-rule]
  (update cfg-rule :dot inc))

(defn basic-cfg-rule [name tagged-clause]
  (CfgRule. name (first tagged-clause) (vec (rest tagged-clause)) 0
            false tagged-clause))

(defn cfg-from-defrule [name {:keys [value action] :as rule}]
  (CfgRule. name (first value) (vec (rest value)) 0 true rule))

(defn goal-rule [sym]
  (CfgRule. ::goal :seq [sym] 0 false sym))

(defn goal? [rule]
  (= (:name rule) ::goal))

(defn rule? [x]
  (instance? clearley.rules.CfgRule x))

(defn action [rule] nil)

(defn clause-type [clause]
  (cond (map? clause) ::map
        (list? clause) (first clause)
        (symbol? clause) ::symbol
        true ::token))

(defmacro hierarchy [& derivations]
  `(-> (make-hierarchy) ~@(map #(cons derive %) derivations)))

(def cfg-hierarchy (hierarchy 
                     ; A rule-only clause must become a new rule upon prediction
                     ; An clauseable clause can be predicted from within another rule
                     (::rule-only ::any) (::clauseable ::any)
                     (:or ::clauseable) (::token ::clauseable) (::symbol ::clauseable)
                     (:seq ::rule-only) (::map ::rule-only)))

; TODO consider clause 
; Returns a seq, [rule | fn]. if fn, is a scanner.
(defmulti predict-clause (fn [clause _] (clause-type clause))
   :hierarchy #'cfg-hierarchy)
(defmethod predict-clause ::symbol [clause grammar]
  (let [got (get grammar clause)
        type (first got)]
    (if (= type :defrule) ; Special type used only by defrule
      ; TODO special type defrule neccesary?
      (map #(cfg-from-defrule (str clause) %) (rest got))
      [(basic-cfg-rule (str clause) (get grammar clause))])))
(defmethod predict-clause ::token [clause _]
  [#(= % clause)])
(defmethod predict-clause ::or [clause _] ; TODO better rulename
  (map #(basic-cfg-rule "anon-or" %) (rest clause)))
(defmethod predict-clause ::map [clause _]
  [(cfg-from-defrule "anon" clause)])
(defmethod predict-clause ::rule-only [clause _]
  [(basic-cfg-rule "anon" clause)])

(defmulti predict (fn [rule _] (:type rule)) :hierarchy #'cfg-hierarchy)
(defmethod predict ::clauseable [{:keys [type clauses]} grammar]
  (predict-clause (cons type clauses) grammar))
(defmethod predict :seq [{:keys [clauses dot]} grammar]
  (predict-clause (get clauses dot) grammar))

(defmulti is-complete? :type :hierarchy #'cfg-hierarchy)
(defmethod is-complete? :seq [{:keys [clauses dot]}]
  (= dot (count clauses)))
(defmethod is-complete? ::any [{:keys [dot]}]
  (= 1 dot))
