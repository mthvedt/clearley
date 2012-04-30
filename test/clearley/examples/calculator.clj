(ns clearley.examples.calculator
  (use clearley.core
       clearley.test.utils
       lazytest.deftest
       clojure.math.numeric-tower))

(defrule sum
  ([sum \+ term] (+ sum term))
  ([sum \- term] (- sum term)) ; left associative
  ([term] term))
(defrule term
  ([term \* pow] (* term pow))
  ([term parenexpr] (* term parenexpr))
  ([parenexpr term] (* parenexpr term))
  ([term \/ pow] (/ term pow))
  ([pow] pow))
(defrule pow
  ([numexpr \^ pow] (expt numexpr pow)) ; right associative
  ([numexpr] numexpr))
(defrule numexpr
  ([\- numexpr] (- numexpr))
  ([parenexpr] parenexpr)
  ([number] number))
(defrule parenexpr
  ([\( sum \)] sum))
(defrule number
  ; TODO: error when it's numexpr not number?
  ([number digit] (+ (* 10 number) digit))
  ([digit] digit))
(defrule digit
  ([\0] 0)
  ([\1] 1)
  ([\2] 2)
  ([\3] 3)
  ([\4] 4)
  ([\5] 5)
  ([\6] 6)
  ([\7] 7)
  ([\8] 8)
  ([\9] 9)) ; Good candidate for the | notation, not used in this example

(def my-calculator (build-parser sum))

(deftest simple-calculator-test
  (with-parser my-calculator
    (is-action 1 "1")
    (is-action 2 "1+1")
    (is-action 15 "15")
    (is-action 153 "0153")
    (is-action 512 "2^3^2")
    (is-action -2 "-2")
    (is-action -2 "2-2-2")
    (is-action 2 "2-(2-2)")
    (is-action 4 "2--2")
    (is-action 0 "2-----2")
    (is-action 12 "2*2^2+2*2")
    (is-action 98 "2+(2-2--2)*2*-(2+2)+100+2^2*3")))
