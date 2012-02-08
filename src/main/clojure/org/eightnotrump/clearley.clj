(ns org.eightnotrump.clearley
  (:import (org.eightnotrump.clearley Ruletype)))

; A rulehead may be:
; a clause
; a clause identifier
; any other object

; A clause is a sequence of ruleheads

; A production rule.
(defrecord Rule [head clauses])

; Example taken from Wikipedia Earley parsing page
(def myruleset [(Rule. ::head [::sum])
                (Rule. ::sum [::sum \+ ::times])
                (Rule. ::sum [::times])
                (Rule. ::times [::times \* ::num])
                (Rule. ::times [::num])
                (Rule. ::num (map vector [\1 \2 \3 \4]))
                (Rule. ::one [\1])])

(def myinput "2*3+4")

(defn add-to-rulemap [rulemap rule]
  (let [head (get rule :head)
        mapped-rules (get rulemap head [])]
    (assoc rulemap head (conj mapped-rules (get rule :clauses)))))

(defn to-rulemap [ruleseq]
  (reduce add-to-rulemap {} ruleseq))

(def myrulemap (to-rulemap myruleset))

(defprotocol EarleyItem
  (get-predictions [self rulemap])
  (get-scans [self input])
  (advance [self]))

(defrecord RItem [head dot clauses]
  EarleyItem
  (get-predictions [self rulemap]
    (if (= dot (alength clauses))
      []
      (map #(RItem. self 0 %)
           (get rulemap (aget clauses dot))))) ; todo what if empty?
  (get-scans [self input]
    (if (and (not (= dot (alength clauses)))
             (= (aget clauses dot) input))
      [(RItem. head (inc dot) clauses)]
      []))
  (advance [self]
    (RItem. head (inc dot) clauses))
  (toString [self]
    (str head " -> "
         (take dot clauses) "*"
         (drop dot clauses))))

(defrecord RStatefulItem [ritem awaiters])

(defn initial-ritem [rule rulemap]
  (RStatefulItem.
    (RItem. rule 0 (into-array (first (get rulemap rule []))))
    []))

(def myitem (initial-ritem ::head myrulemap))
(def myitem2 (initial-ritem ::one myrulemap))

(defprotocol Chart
  (add [self ritem]))

(defrecord RChart [chartseq chartmap]
  Chart
  (add [self {:keys [ritem awaiters] :as rsitem}]
    (if-let [previtem (get chartmap ritem)]
      (RChart. chartseq (assoc chartmap ritem
                       (RStatefulItem. ritem (concat
                                               (get :awaiters previtem)
                                               awaiters))))
      (RChart. (conj chartseq rsitem) (assoc chartmap ritem rsitem)))))

(def mychart (RChart. [] {}))
