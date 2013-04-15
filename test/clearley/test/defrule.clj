(ns clearley.test.defrule
  (:use clearley.core clearley.defrule clearley.test.utils lazytest.deftest))

; ===
; Defrule smoke test
; ===

(defrule sum
  ([sum \+ times] (+ sum times))
  ([times] times))
(defrule times
  ([times \* digit] (* times digit))
  ([digit] digit))
(defrule digit [\3] 3)

(def grammar1 (build-grammar sum))

(extend-rule sum [sum \- times] (- sum times))
(def grammar2 (build-grammar sum))

(deftest build-grammar-smoke-test
  (is grammar1 true)
  (is (get grammar1 'sum))
  (is (get grammar1 'times))
  (is (get grammar1 'digit))

  (is= (count (get grammar1 'sum)) 3)
  (is= (count (get grammar2 'sum)) 4))

; Testing the parser

; TODO: build parser with grammar
(def parser1 (build-parser sum))

(deftest parser-smoke-test
  ; First smoke test: just check out the charts
  (is (with-out-str (print-charts parser1 "1+1"))))
