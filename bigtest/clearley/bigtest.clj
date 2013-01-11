(ns clearley.bigtest
  (:use clearley.core clojure.test))

; Test a grammar that should be O(n^3)

(defrule S
  ([(s1 S) (s2 S)] (str s1 s2))
  ([\s] "s"))

(def s-parser (build-parser S))

(deftest combinatorial-explosion-test
  ; If state reduction works, should execute in O(n^3)
  ; An NDFA without state reduction will see exponential blowup
  (is (parse s-parser
             "ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss")))
; Snakes... why did it have to be snakes?

(extend-rule S
             ([(s1 S) (s2 S) (s3 S)] (str s1 s2 s3)))

(def s-parser-2 (build-parser S))

; TODO
(deftest big-O-test
  (is (parse s-parser
             "ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss")))
