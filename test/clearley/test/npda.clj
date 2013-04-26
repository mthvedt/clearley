(ns clearley.test.npda
  (require [clearley.npda :as npda])
  (use uncore.core uncore.test.utils lazytest.deftest))

(defn popone [state]
  (first (npda/pop state 0)))

(defrecord ANode [value]
  npda/Node
  (npda/shift [self input]
    (ANode. input))
  (npda/node-key [_] value)
  (npda/continue [self input]
    (ANode. (+ (dec input) (dec value))))
  (npda/bounce [self input] nil) ; TODO test
  (npda/return [self]
    [(inc value)]))

(deftest da-test
  ; a certain liddle test dat come checkout acceptanss time
  ; will pass wit flying colorss called... DA TEST
  (let [n1 (-> (npda/initial-state (ANode. 1))
          (npda/shift-state 2 2 1)
          (npda/shift-state 3 3 2)) ; stack is 3 2 1, ostream 3 . 2 1
        ; (the dot denotes ostreams sitting in unpopped states)
        n2 (-> n1 popone popone) ; should be 1
        ; stack is 4 3 2 1. spinning 4 returns 5. 3 becomes 2 + 4 = 6
        ; ostream is now 5 4 3 . 2 1
        ; stack becomes 6 2 1 
        n2a (-> n1 (npda/shift-state 4 4 3) (npda/spin-state 0) first)
        ; spinning 6 returns 7, stack becomes 7 1
        ; ostream is now 7 5 4 3 . 2 1. pop it and it's 7 5 4 3 2 . 1
        n2b (-> n2a (npda/spin-state 0) first popone)]
    (is= (:value (npda/peek n1)) 3)
    (is= (:value (npda/peek n2)) 1)
    (is= (vec (npda/stream n2)) [2 3])
    (is= (:value (npda/peek n2a)) 6)
    (is= (:value (npda/peek n2b)) 1)
    (is= (npda/stream n2b) [2 3 4 5 7])))

; This is a little complicated
; Set this aside while we work on perf
#_(deftest nda-test
  (let [n1 (-> (npda/state (ANode. 1))
             (npda/shift-state 1 1)
             (npda/shift-state 2 2)
             (npda/shift-state 5 5))
        n2 (-> (npda/state (ANode. 2))
             (npda/shift-state 1 1)
             (npda/shift-state 2 2)
             (npda/shift-state 3 3)
             (npda/shift-state 5 5))
        n3 (-> (npda/state (ANode. 3))
             (npda/shift-state 1 1)
             (npda/shift-state 2 2)
             (npda/shift-state 3 3)
             (npda/shift-state 4 4)
             (npda/shift-state 5 5))
        ; n* is in state 5; child stacks are 2 1, 3 2 1, 4 3 2 1
        n* (npda/unify n1 (npda/unify n2 n3))
        nx (npda/pop n* 0) ; 3 stacks
        ny (mapcat #(npda/pop % 0) nx) ; Still three stacks
        ; test nondeterministic reducing
        ; put 6 on the stack and spin... yields stack-top 10
        ; spin again... yields 11 1, 12 2 1, 13 3 2 1
        nr (-> n* (npda/shift-state 6 6) (npda/spin-state 0) first npda/spin-state)
        ; 11, 13 1, 15 2 1
        nr2 (mapcat #(npda/spin-state 0) nr)
        nr3 [(first nr2)
             (-> (second nr2) (npda/spin-state 0) first)
             (-> (nth nr2 2) (npda/spin-state 0) first (npda/spin-state 0) first)]]
    (is= (:value (npda/peek n*)) 5)
    (is= (count nx) 3)
    (is= (count ny) 3)
    (is= (map npda/stream nx) [[2 5] [3 5] [4 5]])
    (is= (map-> nx npda/peek :value) [2 3 4])
    (is= (map npda/stream ny) [[1 2 5] [2 3 5] [3 4 5]])
    (is= (map-> ny npda/peek :value) [1 2 3])
    (is= (count nr) 3)
    (is= (count nr2) 3)
    (is= (map-> nr npda/peek :value) [11 12 13])
    (is= (map-> nr2 npda/peek :value) [11 13 15])
    (is= (map-> nr3 npda/peek :value) [11 13 16])
    (is= (map npda/stream nr3) [[1 2 5 6 7 11 12]
                                [1 2 3 5 6 7 11 13 14]
                                [1 2 3 4 5 6 7 11 14 16 17]])))
