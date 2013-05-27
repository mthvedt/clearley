(ns clearley.examples.calculator
  (use clearley.match clearley.lib clojure.math.numeric-tower))

(defmatch sum
  ([sum \+ term] (+ sum term))
  ([sum \- term] (- sum term)) ; left associative
  term)
; Prevent a state split conflict
(defmatch term
  posterm
  ([\- term] (- term)))
(defmatch posterm
  ([posterm \* pow] (* posterm pow))
  ([posterm \/ pow] (/ posterm pow))
  bare-pow ; Avoid -term and -pow conflict
  parenexpr)
(defmatch parenexpr
  ; Getting this right while remaining LR(1) is a little tricky.
  ; The approach is: 1) stay left-recursive, 2) two bare-pows cannot be adjacent.
  ; We do a sort of induction.
  ; base cases
  paren-sum
  ([bare-pow paren-sum] (* bare-pow paren-sum))
  ([paren-sum bare-pow] (* bare-pow paren-sum))
  ([(p1 bare-pow) paren-sum (p2 bare-pow)] (* p1 p2 paren-sum))
  ; recursive cases
  ([parenexpr paren-sum] (* parenexpr paren-sum))
  ([parenexpr paren-sum bare-pow] (* parenexpr paren-sum bare-pow)))
(defmatch pow
  ([\- pow] (- pow))
  paren-sum
  bare-pow)
(defmatch bare-pow
  ([natnum \^ pow] (expt natnum pow)) ; right associative
  natnum)
(def paren-sum (parens sum))

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
    (is-action 32 "2(2)2(2)2")
    (is-action 2 "(((((2)))))")
    (is-action 4 "2--2")
    (is-action 0 "2-----2")
    (is-action 12 "2*2^2+2*2")
    (is-action 98 "2+(2-2--2)*2*-(2+2)+100+2^2*3")))
