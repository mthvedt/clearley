(ns clearley.test.npda
  (require [clearley.npda :as npda])
  (use clearley.test.utils
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
    ; Flow of state n2a (starting from n1):
    ; stack 1 2 3, output 2 3
    ; stack 1 2 3 4, output 2 3 4
    ; reducing rule 4: stack 1 2, output 2 3 4 5, goto 1
    (is= (:value (npda/peek n2a)) 1)
    ; reducing rule 1: stack 1, output 2 3 4 5 2, goto 0
    ; pop one
    (is= (:value (npda/peek n2b)) 1)
    (is= (vec (npda/stream n2b)) [2 3 4 5 2])))

#_(deftest nda-test
  (let [n1 (-> (npda/state 1)
             (npda/shift 1 [1])
             (npda/shift 5 [5]))
        n2 (-> (npda/state 2)
             (npda/shift 2 [2])
             (npda/shift 5 [5]))
        n3 (-> (npda/state 3)
             (npda/shift 3 [3])
             (npda/shift 5 [5]))
        n* (npda/unify n1 (npda/unify n2 n3))
        nx (npda/pop n*)
        ny (mapcat npda/pop nx)]
    (is= (npda/peek n*) 5)
    (is= (count nx) 3)
    (is= (count ny) 3)
    (is= (vec (map #(vec (npda/stream %)) nx))
         [[1 5] [2 5] [3 5]])
    (is= (vec (map npda/peek nx)) [1 2 3])))
