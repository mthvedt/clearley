(ns clearley.test.defrule
  (:use clearley.core clearley.defrule clearley.test.utils lazytest.deftest))

; === Just a smoke test ===
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
(def parser1 (build-parser sum))

(deftest parser-smoke-test
  (is (with-out-str (print-charts parser1 "3+3"))))

; === Test the basics ===
(def-parser-test parsing1 parser1
    (is-parsing "3+3")
    (not-parsing "4+4"))

(def-parser-test match1 parser1
    (is-ast [[[\3]]] "3")
    (is-ast [[[[\3]]] \+ [[[\3]] \* [\3]]] "3+3*3"))

(def-parser-test defrule1 parser1
  (with-parser parser1
    (is-action 0 "3-3")
    (is-action 6 "3+3")
    (is-action 9 "3*3")
    (is-action 15 "3+3*3+3")))

; A little more invovled; also testing build from grammar
; TODO build from grammar
(defrule digit ([\1] 1) ([\2] 2) ([\3] 3) ([\4] 4) ([\5 \5] 55))
(def parser2 (build-parser sum))

(def-parser-test basic-parser-test parser2
  (is-parsing "1+2")
  (is-parsing "1+2*3+4")
  (is-parsing "1*2+3*4")
  (is-parsing "1+55*3+2*55")
  ;(is-parsing "777") ; test string (seq of chars) literals
  (isnt (parses? "44"))
  (isnt (parses? "55*23"))
  (isnt (parses? "1+2a"))
  ;(isnt (parses? "7777"))
  (is-parsing "1+55*2*55+3+55*4")
  (is-ast [[[\1]]] "1")
  (is-ast [[[[\2]]] \+ [[[\3]] \* [\4]]] "2+3*4")
  (is-ast [[[[[\1]]] \+ [[[\2]] \* [\3]]] \+ [[[\4]] \* [\1]]] "1+2*3+4*1")
  (is-ast [[[\5 \5]]] "55"))

; Rule aliasing
(defrule sum
  ([sum \+ (t times)] (+ sum t))
  ([(t times)] t))
(def aliasing-parser (build-parser sum))

(def-parser-test rule-aliasing aliasing-parser
  (is-action 6 "3+3"))

; Rule literals
(def digits67 '(:or \6 \7))
(extend-rule digit [digits67] digits67)
(def literal-parser (build-parser sum))

#_(def-parser-test rule-literals literal-parser
  (is-action 2 "7-5"))
