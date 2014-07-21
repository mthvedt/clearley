(ns clearley.examples.calculator
  (require clearley.core)
  (use clearley.match clearley.lib clojure.math.numeric-tower))

(defmatch sum
  ([sum \+ term] (+ sum term))
  ([sum \- term] (- sum term)) ; left associative
  term)
(defmatch term
  ([term \* pow] (* term pow))
  ([term parenexpr] (* term parenexpr))
  ([parenexpr term] (* parenexpr term))
  ([term parenexpr (term2 term)] (* term parenexpr term2)) ; need an alias
  ([term \/ pow] (/ term pow))
  pow)
(defmatch pow
  ([numexpr \^ pow] (expt numexpr pow)) ; right associative
  numexpr)
(defmatch numexpr
  ([\- numexpr] (- numexpr))
  parenexpr
  natnum)
(def parenexpr (parens sum))

(def my-calculator (clearley.core/build-parser sum))

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
