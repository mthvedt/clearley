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

(deftest build-grammar-smoke-test
  (is grammar1 true)
  (is (get grammar1 'sum))
  (is (get grammar1 'times))
  (is (get grammar1 'digit))
  (is= (count (get grammar1 'sum)) 3))

; Testing the parser
(def parser1 (build-parser sum))

(deftest parser-smoke-test
  (is (with-out-str (print-charts parser1 "3+3"))))

; === Test the basics ===
; TODO don't test asts
(def-parser-test parsing1 parser1
    (is-parsing "3+3")
    (not-parsing "4+4"))

#_(def-parser-test match1 parser1
    (is-ast [[[\3]]] "3")
    (is-ast [[[[\3]]] \+ [[[\3]] \* [\3]]] "3+3*3"))

(def-parser-test defrule-test parser1
  (with-parser parser1
    (is-action 6 "3+3")
    (is-action 9 "3*3")
    (is-action 15 "3+3*3+3")))

; A little more invovled
; TODO test build from grammar
(defrule digit ([\1] 1) ([\2] 2) ([\3] 3) ([\4] 4) ([\5 \5] 55))
(def parser2 (build-parser sum))

(def-parser-test basic parser2
  (is-parsing "1+2")
  (is-parsing "1+2*3+4")
  (is-parsing "1*2+3*4")
  (is-parsing "1+55*3+2*55")
  (isnt (parses? "44"))
  (isnt (parses? "55*23"))
  (isnt (parses? "1+2a"))
  (is-parsing "1+55*2*55+3+55*4"))
  ;(is-ast [[[\1]]] "1")
  ;(is-ast [[[[\2]]] \+ [[[\3]] \* [\4]]] "2+3*4")
  ;(is-ast [[[[[\1]]] \+ [[[\2]] \* [\3]]] \+ [[[\4]] \* [\1]]] "1+2*3+4*1")
  ;(is-ast [[[\5 \5]]] "55"))

; Rule aliasing
(defrule sum
  ([sum \+ (t times)] (+ sum t))
  ([(t times)] t))
(def aliasing-parser (build-parser sum))

(def-parser-test rule-aliasing aliasing-parser
  (is-action 6 "3+3"))

; Rule literals
; TODO have :defrule?
(def digits67 '(:or \6 (:seq \7 \7)))
(defrule digits67* [digits67] 10) ; ok, maybe this is bad practice
(defrule digit ([\1] 1) ([\2] 2) ([\3] 3) ([\4] 4) ([\5 \5] 55)
  ([digits67*] digits67*))
(def literal-parser (build-parser sum))

(def-parser-test rule-literals literal-parser
  (is-action 15 "2+3+6")
  (is-action 15 "2+3+77"))
  ;(is-ast [[[[\2]]] \+ [[[\3]] \* [[[\6]]]]] "2+3*6")
  ;(is-ast [[[[\2]]] \+ [[[\3]] \* [[[\7 \7]]]]] "2+3*77"))

; Scanner test
(def digit (char-range \0 \9 #(- (int %) (int \0))))
(def scanner-parser (build-parser sum))

(def-parser-test scanners scanner-parser
  (is-action 6 "1+2+3"))

; star test
(defrule number [(digits (plus 'digit))]
  (reduce #(+ % (* %2 10)) digits))
(defrule times
  ([times \* number] (* times number))
  ([number] number))

(def star-parser (build-parser sum))

(def-parser-test star star-parser
  (is-action 2 "1+1")
  (is-action 22 "11+11")
  (is-action 771 "1+22*33+44"))

; opt test
(def foo `(:star (:seq \x ~(opt \y))))
(def foo-parser (build-parser foo))

(def-parser-test opt-test foo-parser
  (is-action [[\x nil] [\x nil]] "xx")
  (is-action [[\x nil] [\x \y]] "xxy")
  (not-parsing "xyy"))

; hidden left recursion test
(defrule alpha
  ([beta alpha mu] (str beta alpha mu))
  ("w" "w"))
(defrule mu "y" "y")
(def beta
  `(:or \x (:seq)))
(def hidden-left-recursion-parser (build-parser alpha))
(def-parser-test hidden-left-recursion hidden-left-recursion-parser 
  (is-action "xxwyy" "xxwyy")
  (is-action "wyy" "wyy"))

;TODO tests on grammars alone
