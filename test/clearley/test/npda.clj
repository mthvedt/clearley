(ns clearley.test.npda
  (require [clearley.npda :as npda])
  (use clearley.utils
       clearley.test.utils
       lazytest.deftest))

(defrecord ANode [value]
  npda/Node
  (npda/shift [self input]
    (ANode. input))
  (npda/reduce [self input]
    [(ANode. (dec value))])
  (npda/reductions [self]
    [[(inc value) 2]]))

(deftest da-test
  ; a certain liddle test dat come checkout acceptanss time will pass wit flying colorss
  ; called... DA TEST
  (let [n1 (-> (npda/state (ANode. 1))
          (npda/shift-state 2 2)
          (npda/shift-state 3 3))
        n2 (-> n1 npda/popone npda/popone)
        n2a (-> n1 (npda/shift-state 4 4) npda/spin-state first)
        n2b (-> n2a npda/spin-state first npda/popone)]
    (is= (:value (npda/peek n2)) 1)
    (is= (vec (npda/stream n2)) [2 3])
    ; Test reducing
    ; Flow of state n2a (starting from n1):
    ; stack 1 2 3, output 2 3
    ; stack 1 2 3 4, output 2 3 4
    ; reducing rule 4: stack 1 2, output 2 3 4 5, goto 1
    (is= (:value (npda/peek n2a)) 1)
    ; reducing rule 1: stack 1, output 2 3 4 5 2, goto 0
    ; pop one
    (is= (:value (npda/peek n2b)) 1)
    (is= (npda/stream n2b)) [2 3 4 5 2]))

; This is a little complicated
(deftest nda-test
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
        n* (npda/unify n1 (npda/unify n2 n3))
        nx (npda/pop n*)
        ny (mapcat npda/pop nx)
        ; test nondeterministic reducing
        nr (-> n* (npda/shift-state 6 6) npda/spin-state)
        nr2 (mapcat npda/spin-state nr)
        nr3 [(npda/popone (first nr2))
             (-> (second nr2) npda/spin-state first npda/popone)
             (-> (nth nr2 2) npda/spin-state first npda/spin-state first npda/popone)]]
    (is= (:value (npda/peek n*)) 5)
    (is= (count nx) 3)
    (is= (count ny) 3)
    (is= (map npda/stream nx) [[2 5] [3 5] [4 5]])
    (is= (map-> nx npda/peek :value) [2 3 4])
    (is= (map npda/stream ny) [[1 2 5] [2 3 5] [3 4 5]])
    (is= (map-> ny npda/peek :value) [1 2 3])
    ; Test reducing
    ; nr: stacks are 1 2 5 6, 1 2 3 5 6, 1 2 3 4 5 6
    (is= (count nr) 3)
    ; nr2: stacks are 1 2 1, 1 2 3 2, 1 2 3 4 3
    ; after reducing, 1 0, 1 2 1, 1 2 3 2
    (is= (count nr2) 3)
    (is= (map-> nr npda/peek :value) [1 2 3])
    (is= (map-> nr2 npda/peek :value) [0 1 2])
    ; nr3.1: after reducing once, stack is 1 0
    ; nr3.2: stack is 1 2 1, then 1 0
    (is= (map-> nr3 npda/peek :value) [1 1 1])
    (is= (map npda/stream nr3) [[1 2 5 6 7 2]
                                [1 2 3 5 6 7 3 2]
                                [1 2 3 4 5 6 7 4 3 2]])))
