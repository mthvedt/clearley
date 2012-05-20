(ns clearley.examples.simplest-calculator
  (use clearley.core))

(defrule sum
  ([sum \+ term] (+ sum term)) ; left associative
  ([sum \- term] (- sum term))
  ([term] term))
(defrule term
  ([term \* number] (* term number))
  ([term \/ number] (/ term number))
  ([number] number))
(defrule number
  ([\- number] (- number))
  ([number digit] (+ (* 10 number) digit))
  ([digit] digit))
; todo: 'or-token' or something instead
(defrule digit [(a-digit (map (comp token char) (range (int \0) (inc (int \9)))))]
  (- (int a-digit) (int \0)))

(def my-calculator (build-parser sum))

(use 'lazytest.deftest 'clearley.test.utils)

(deftest simple-calculator-test
  (with-parser my-calculator
    (is-action 1 "1")
    (is-action 2 "1+1")
    (is-action 15 "15")
    (is-action 153 "0153")
    (is-action -2 "-2")
    (is-action -2 "2-2-2")
    (is-action 4 "2--2")
    (is-action 0 "2-----2")))
