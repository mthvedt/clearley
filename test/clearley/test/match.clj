(ns clearley.test.match
  (use clearley.core clearley.match [clearley.lib :exclude [digit]] clearley.grammar
       clearley.test.utils uncore.test.utils lazytest.deftest))

; TODO tests for match
; TODO simplify def-parser-test

; === Just a smoke test ===
(defmatch sum
  ([sum \+ times] (+ sum times))
  times)
(defmatch times
  ([times \* digit] (* times digit))
  digit)
(defmatch digit [\3] 3)

(def grammar1 (build-grammar sum))

(deftest build-grammar-smoke-test
  (is grammar1 true)
  (is (get grammar1 'clearley.test.match/sum))
  (is (get grammar1 'clearley.test.match/times))
  (is (get grammar1 'clearley.test.match/digit)))

; Testing the parser
(def parser1 (build-parser sum))

; TODO
#_(deftest parser-smoke-test
  (is (with-out-str (print-charts parser1 "3+3"))))

; === Test the basics ===
(def-parser-test parsing1 parser1
    (is-parsing "3+3")
    (not-parsing "4+4"))

(def-parser-test defmatch-test parser1
  (with-parser parser1
    (is-action 6 "3+3")
    (is-action 9 "3*3")
    (is-action 15 "3+3*3+3")))

; A little more invovled
(defmatch digit ([\1] 1) ([\2] 2) ([\3] 3) ([\4] 4) ([\5 \5] 55))
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

(def g (build-grammar sum))
(def grammar-parser (parser 'sum g))
(def-parser-test build-from-grammar grammar-parser
  (is-parsing "1+2")
  (is-parsing "1+2*3+4")
  (is-parsing "1*2+3*4")
  (is-parsing "1+55*3+2*55")
  (isnt (parses? "44"))
  (isnt (parses? "55*23"))
  (isnt (parses? "1+2a")))

; Rule aliasing
(defmatch sum
  ([sum \+ (t times)] (+ sum t))
  ([(t times)] t))
(def aliasing-parser (build-parser sum))

(def-parser-test rule-aliasing aliasing-parser
  (is-action 6 "3+3"))

; Rule literals
(def digits67 '(:or \6 (:seq \7 \7)))
(defmatch digits67* [digits67] 10) ; ok, maybe this is bad practice
(defmatch digit ([\1] 1) ([\2] 2) ([\3] 3) ([\4] 4) ([\5 \5] 55)
  ([digits67*] digits67*))
(def literal-parser (build-parser sum))

(def-parser-test rule-literals literal-parser
  (is-action 15 "2+3+6")
  (is-action 15 "2+3+77"))

; Scanner test
(def digit (char-range \0 \9 #(- (int %) (int \0))))
(def scanner-parser (build-parser sum))

(def-parser-test scanners scanner-parser
  (is-action 6 "1+2+3"))

; star test
(defmatch number [(digits (plus 'digit))]
  (reduce #(+ % (* %2 10)) digits))
(defmatch times
  ([times \* number] (* times number))
  ([number] number))

(def star-parser (build-parser sum))

(def-parser-test star-test star-parser
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
(defmatch alpha
  ([beta alpha mu] (str beta alpha mu))
  ("w" "w"))
(defmatch mu "y" "y")
(def beta
  `(:or \x (:seq)))
(def hidden-left-recursion-parser (build-parser alpha))
#_(def-parser-test hidden-left-recursion hidden-left-recursion-parser 
  (is-action "xxwyy" "xxwyy")
  (is-action "wyy" "wyy"))

; Test from 'Practical Earley Parsing'
; http://courses.engr.illinois.edu/cs421/sp2012/project/PracticalEarleyParsing.pdf

(defbind S [a1 A a2 A a3 A a4 A] (str a1 a2 a3 a4))
(def A `(:or \a (:seq)))
(def pep-tester (build-parser S))
#_(def-parser-test practical-earley-parsing pep-tester
  (is-action "" "")
  (is-action "a" "a")
  (is-action "aa" "aa")
  (is-action "aaa" "aaa")
  (is-action "aaaa" "aaaa"))

(defbind any-object-test-rule [_ :foo
                          _ (token 'bar)
                          _ (scanner vector?)]
  "hello world!")
(def any-object-test-parser (build-parser any-object-test-rule))
(def-parser-test match-any-object any-object-test-parser
  (is-action "hello world!" [:foo 'bar []]))
