(ns clearley.rules
  (require clearley.grammar clojure.string uncore.rpartial clojure.set
           [uncore.throw :as t]
           [uncore.str :as s])
  (use uncore.core))
; Core stuff for context free grammar parsing.

; A somewhat ugly hack to make calculating first sets and null advances O(1)
; Note that if body is recrusive, returns lower on the stack
; will overwrite higher ones; this is intentional
; TODO this is dangerous. safe if done right, but can introduce bugs. kill with fire.
(def ^:dynamic *mem-atom* nil)
(defmacro local-memo [key datum & body]
  `(if *mem-atom*
     (let [m# (get @*mem-atom* ~key {})]
       (if (contains? m# ~datum)
         (get m# ~datum)
         (let [r# (do ~@body)]
           (swap! *mem-atom* #(assoc % ~key (assoc (get % ~key {}) ~datum r#)))
           r#)))))

; just for speed
(defrecord Match [rule submatches])

(def match ->Match)

; The instrumented core abstraction
(defrecord CfgRule [dot raw-rule null-results grammar])

(defn cfg-rule [rule grammar]
  (CfgRule. 0 rule {} grammar))

(defn rule? [x]
  (instance? clearley.rules.CfgRule x))

(defn goal-rule [sym grammar]
  (CfgRule. 0 {:name ::goal, :tag :seq,
               :value [(clearley.grammar/normalize sym nil)], :action identity}
            {} grammar))

; For a rule that has been nulled out, gets a reduced version of the rule
; appropriate for take-action.
; This is a stopgap and won't be neccesary in a real GLL parser.
(defn get-original [{{:keys [action] :as raw-rule} :raw-rule :keys [null-results]}]
  (if (seq null-results)
    (assoc raw-rule :action (uncore.rpartial/gen-rpartial action null-results))
    raw-rule))

(defn advance [cfg-rule]
  (update cfg-rule :dot inc))

; Null advance: for advancing a rule that can predict the empty string
; See Aycock+Horspool Practical Earley Parsing
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
(defmulti predict* (fn-> :raw-rule :tag))
(defmethod predict* :symbol [{dot :dot, grammar :grammar, {value :value} :raw-rule}]
  (if-let [resolved (predict-singleton :symbol (get grammar (first value)))]
    (if (= resolved [])
      []
      [(cfg-rule resolved grammar)])
    (t/RE "Cannot find symbol in grammar: " (first value))))
(defmethod predict* :or [{dot :dot, grammar :grammar, {value :value} :raw-rule}]
  (map #(cfg-rule % grammar) (if (= dot 1) [] value)))
(defmethod predict* :seq [{dot :dot, grammar :grammar, {value :value} :raw-rule}]
  (if (= dot (count value)) []
    [(cfg-rule (get value dot) grammar)]))
(defmethod predict* :star [{dot :dot, grammar :grammar, {value :value} :raw-rule}]
  (map #(cfg-rule % grammar) (check-singleton :star value)))
(defmethod predict* :scanner [{dot :dot {value :value} :raw-rule}]
  (predict-singleton :scanner value))
(defmethod predict* :token [{dot :dot {value :value} :raw-rule}]
  (predict-singleton :token [(fn [x] (= x (first value)))]))
(defmethod predict* :default [rule]
  (t/RE "Don't know how to predict:" (pr-str rule)))

(defn predict [cfg-rule]
  (local-memo :predict cfg-rule (predict* cfg-rule)))

; Is this rule complete?
(defmulti is-complete? (fn-> :raw-rule :tag))
(defmethod is-complete? :star [{:keys [dot]}] true)
(defmethod is-complete? :seq [{dot :dot {value :value} :raw-rule}]
  (= dot (count value)))
(defmethod is-complete? :default [{:keys [dot]}]
  (= 1 dot))

(defn goal? [cfg-rule]
  (and (is-complete? cfg-rule)
       (-> cfg-rule :raw-rule :name (= ::goal))))

(defn clause-strs
  ([clauses] (clause-strs clauses 0))
  ([clauses dot]
   (let [clause-strs (map pr-str clauses)]
     (concat (take dot clause-strs)
             (if (zero? dot) [] ["*"])
             (drop dot clause-strs)))))

(defn clause-name [clause]
  (if (:name clause) (:name clause) (pr-str clause)))

(defmulti rule-str (fn-> :raw-rule :tag))
(defmethod rule-str :seq [{dot :dot, {:keys [name value]} :raw-rule :as rule}]
  (let [clause-strs (map clause-name value)]
    (str name " -> "
         (cond (= dot 0) (s/separate-str " " clause-strs)
               (is-complete? rule) (s/separate-str " " (concat clause-strs ["✓"]))
               true (s/separate-str " " (concat (take dot clause-strs) ["•"]
                                                (drop dot clause-strs)))))))
(defmethod rule-str :default [{dot :dot, {:keys [name tag value]} :raw-rule}]
  (str name " -> " tag " ("
       (s/separate-str " " (map clause-name value))
       ")"
       (if (zero? dot) "" " ✓")))

(def ^:dynamic *breadcrumbs*)
; Basically, this does an LL match on the empty string
(defn null-result* [rule-or-fn]
  (if (rule? rule-or-fn)
    (if (contains? *breadcrumbs* rule-or-fn)
      (get *breadcrumbs* rule-or-fn)
      (local-memo :null-result rule-or-fn
                  (set! *breadcrumbs* (assoc *breadcrumbs* rule-or-fn nil))
                  (let [r (if (is-complete? rule-or-fn)
                            (match (:raw-rule rule-or-fn) [])
                            (if-let [r (some identity (map null-result*
                                                           (predict rule-or-fn)))]
                              (if-let [r2 (null-result* (advance rule-or-fn))]
                                (match (:raw-rule rule-or-fn)
                                       (apply vector r (:submatches r2))))))]
                    (set! *breadcrumbs* (assoc *breadcrumbs* rule-or-fn r))
                    r)))
    nil))
(defn null-result [rule]
  (binding [*breadcrumbs* {}]
    (null-result* rule)))

(def ^:dynamic *breadcrumbs-firsts*)
; Gets the first set of an item, including ::empty if item is nullable
(defn first-set* [rule-or-fn]
  (if (rule? rule-or-fn)
    (if (contains? *breadcrumbs-firsts* rule-or-fn)
      (get *breadcrumbs-firsts* rule-or-fn)
      (local-memo :first-set rule-or-fn
                  (set! *breadcrumbs-firsts* (assoc *breadcrumbs-firsts*
                                                    rule-or-fn #{}))
                  (let [r (apply clojure.set/union (map first-set* (predict
                                                                     rule-or-fn)))
                        r (cond (null-result rule-or-fn) (conj r ::empty)
                                (r ::empty) (disj
                                              (clojure.set/union
                                                r (first-set* (advance rule-or-fn)))
                                              ::empty)
                                true r)]
                    (set! *breadcrumbs-firsts* (assoc *breadcrumbs-firsts*
                                                      rule-or-fn r))
                    r)))
    #{rule-or-fn}))
(defn first-set [rule]
  (binding [*breadcrumbs-firsts* {}]
    (first-set* rule)))

; For a rule R that predicts items R1..N, calculates the follow set for those items.
(defn follow-first [rule parent-follow]
  (let [follow-set (first-set (advance rule))]
    (if (follow-set ::empty)
      (disj (conj follow-set parent-follow) ::empty)
      follow-set)))

(defn take-action* [match]
  (if (nil? match)
    (throw (RuntimeException. "Failure to parse"))
    (let [{:keys [rule submatches]} match
          subactions (map take-action* submatches)
          ; TODO eventually, token rules shouldn't appear on the stack?
          action (get rule :action (fn [] rule))
          action (if (= :token (:tag rule)) (fn [_] (action)) action)]
      (try
        (apply action subactions)
        (catch clojure.lang.ArityException e
          (throw (RuntimeException. (str "Wrong # of params taking action for rule "
                                         (if (rule? rule)
                                           (rule-str rule)
                                           (pr-str rule)) ", "
                                         "was given " (count subactions))
                                    e)))))))

(defn eager-advance [rule]
  (if-let [eager-match (some identity (map null-result (predict rule)))]
    (null-advance rule (take-action* eager-match))))
