(ns clearley.examples.calculator
  (use clearley.core
       clojure.math.numeric-tower))

(defrule sum
  ([sum \+ term] (+ sum term))
  ([sum \- term] (- sum term)) ; left associative
  ([term] term))
(defrule term
  ([term \* pow] (* term pow))
  ([term parenexpr] (* term parenexpr))
  ([parenexpr term] (* parenexpr term))
  ([term parenexpr (term2 term)] (* term parenexpr term2)) ; need an alias
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
  ([number digit] (+ (* 10 number) digit))
  ([digit] digit))
(def digit (char-range \0 \9 (fn [c] (- (int c) (int \0)))))

(def my-calculator (build-parser sum))

(use 'lazytest.deftest 'clearley.test.utils)

(deftest calculator-test
  (with-parser my-calculator
    (is-action 1 "1")
    (is-action 2 "1+1")
    (is-action 15 "15")
    (is-action 153 "0153")
    (is-action 512 "2^3^2")
    (is-action -2 "-2")
    (is-action -2 "2-2-2")
    (is-action 2 "2-(2-2)")
    (is-action 8 "2(2+2)")
    (is-action 8 "(2+2)2")
    (is-action 16 "(2+2)(2+2)")
    (is-action 16 "2(2+2)2")
    (is-action 16 "2(2+2)(1+1-1)2")
    (is-action 2 "(((((2)))))")
    (is-action 4 "2--2")
    (is-action 0 "2-----2")
    (is-action 12 "2*2^2+2*2")
    (is-action 98 "2+(2-2--2)*2*-(2+2)+100+2^2*3")))
