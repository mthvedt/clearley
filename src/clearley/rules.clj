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
; Some clauses can be predicted inline; others must be turned into rules.
; TODO action
; TODO passthrough matches?
;
; Iron rule of clauses: One clause -> one match.
; They're all just combinators.
(defrecord CfgRule [name type clauses dot toplevel? original])

(defn clause-type [clause]
  (cond (map? clause) ::map
        (seq? clause) (first clause)
        (symbol? clause) ::symbol
        true ::token))

(defn advance [cfg-rule]
  (update cfg-rule :dot inc))

(defn cfg-from-defrule [name {:keys [value action] :as rule}]
  (CfgRule. name (first value) (vec (rest value)) 0 true rule))

(defn goal-rule [sym]
  (CfgRule. ::goal :seq [sym] 0 false sym))

(defn goal? [rule]
  (= (:name rule) ::goal))

(defn rule? [x]
  (instance? clearley.rules.CfgRule x))

(defn action [rule]
  (get rule :action (fn [] rule)))

(defmacro hierarchy [& derivations]
  `(-> (make-hierarchy) ~@(map #(cons derive %) derivations)))

(def cfg-hierarchy (hierarchy 
                     ; A rule-only clause must become a new rule upon prediction
                     (::rule-only ::any) (::inline ::any) (::singleton ::any)
                     ; An inline clause can be predicted from within another rule
                     (:or ::inline) (:scanner ::inline)
                     ; A singleton can be predicted from within another rule
                     ; and must always be on its own within a rule.
                     ; TODO diff between inline and singleton?
                     (::token ::singleton) (::symbol ::singleton)
                     (:seq ::rule-only) (::map ::rule-only) (:plus ::rule-only)))

; TODO this whole multimethod stuff is awkward, and needs work.

; Makes a clause into a rule
(defmulti to-rule (fn [_ x] (clause-type x)) :hierarchy #'cfg-hierarchy)
(defmethod to-rule ::map [name clause]
  (cfg-from-defrule name clause))
(defmethod to-rule ::symbol [name clause]
  (CfgRule. name (clause-type clause) (vector clause) 0 false
            {:clauses (vector clause) :action identity}))
(defmethod to-rule ::token [name clause]
  (CfgRule. name (clause-type clause) (vector clause) 0 false
            {:clauses (vector clause) :action (fn [& args] clause)}))
(defmethod to-rule ::any [name tagged-clause]
  (CfgRule. name (first tagged-clause) (vec (rest tagged-clause)) 0
            false {:clauses tagged-clause :action identity}))

; Returns a seq, [rule | fn]. if fn, is a scanner.
(defmulti predict-clause (fn [clause _] (clause-type clause))
   :hierarchy #'cfg-hierarchy)
(defmethod predict-clause ::symbol [clause grammar]
  (let [got (get grammar clause)
        type (first got)]
    (if (= type :defrule) ; Special type used only by defrule
      (map #(cfg-from-defrule (str clause) %) (rest got))
      [(to-rule (str clause) (get grammar clause))])))
(defmethod predict-clause ::token [clause _]
  [#(= % clause)])
(defmethod predict-clause :scanner [clause _]
  [(second clause)])
(defmethod predict-clause :or [clause _]
  (map #(to-rule "anon-or" %) (rest clause)))
(defmethod predict-clause ::map [clause _]
  [(cfg-from-defrule "anon" clause)])
(defmethod predict-clause ::rule-only [clause _]
  [(to-rule "anon" clause)])

; Predicts a rule, returning a seq [rule | fn] as in predict-clause
; TODO: we should wrap-rule properly, not have this ::singleton hack
(defmulti predict (fn [rule _] (:type rule)) :hierarchy #'cfg-hierarchy)
(defmethod predict ::singleton [{:keys [clauses]} grammar]
  (predict-clause (first clauses) grammar))
(defmethod predict ::inline [{:keys [type clauses]} grammar]
  (predict-clause (cons type clauses) grammar))
(defmethod predict :seq [{:keys [clauses dot]} grammar]
  (if (empty? clauses)
    (t/RE "Cannot have an empty :seq")
    (predict-clause (get clauses dot) grammar)))

; Is this rule complete?
(defmulti is-complete? :type :hierarchy #'cfg-hierarchy)
(defmethod is-complete? :seq [{:keys [clauses dot]}]
  (= dot (count clauses)))
(defmethod is-complete? ::any [{:keys [dot]}]
  (= 1 dot))

(defn clause-strs [clauses dot]
  (let [clause-strs (map pr-str clauses)]
    (concat (take dot clause-strs)
            (if (zero? dot) [] ["*"])
            (drop dot clause-strs))))

; TODO rule-str for paren'd rules is wrong, dot should be outside parens
(defmulti rule-str :type :hierarchy #'cfg-hierarchy)
(defmethod rule-str :seq [{:keys [name clauses dot]}]
  (s/separate-str " " (concat [name "->"] (clause-strs clauses dot))))
(defmethod rule-str ::any [{:keys [name type clauses dot]}]
  (str name " -> ("
       (s/separate-str " " (cons type (clause-strs clauses dot)))
       ")"))
