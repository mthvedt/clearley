(ns clearley.examples.calculator
  (use clearley.match clearley.lib clojure.math.numeric-tower))

; It turns out making ordinary arithmetic an LR(1) grammar is pretty tough!
; TODO add ignore conflicts
(defmatch sum
  ([sum \+ term] (+ sum term))
  ([sum \- term] (- sum term)) ; left associative
  term)
(defmatch term
  ([term \* pow] (* term pow))
  ([term \/ pow] (/ term pow))
  ([\- term] (- term))
  ;([\- parenexpr] (- parenexpr))
  parenexpr
  term-lparen)
(defmatch term-lparen
  ([term-lparen parenexpr] (* term-lparen parenexpr))
  ([parenexpr term-rparen] (* parenexpr term-rparen))
  ([(p1 parenexpr) (p2 parenexpr)] (* p1 p2))
  ([term-lparen parenexpr pow-noparen] (* term-lparen parenexpr pow-noparen))
  pow-noparen)
(defmatch term-rparen ; neccesary for LR(1)
  ([parenexpr term-rparen] (* parenexpr term-rparen))
  ([\- parenexpr term-rparen] (- (* parenexpr term-rparen)))
  pow-noparen)
(defmatch pow
  parenexpr
  ([\- parenexpr] (- parenexpr))
  pow-noparen)
(defmatch pow-noparen
  ([numexpr \^ pow] (expt numexpr pow)) ; right associative
  numexpr)
(defmatch numexpr
  ;([\- numexpr] (- numexpr))
  ([natnum] natnum))
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
    (is-action 32 "2(2)2(2)2")
    (is-action 2 "(((((2)))))")
    (is-action 4 "2--2")
    (is-action 0 "2-----2")
    (is-action 12 "2*2^2+2*2")
    (is-action 98 "2+(2-2--2)*2*-(2+2)+100+2^2*3")))
