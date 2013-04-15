(ns clearley.test.core
  (:use clearley.core clearley.defrule clearley.test.utils lazytest.deftest))

(defn rulefn
  [name & clauses]
  (rule name clauses nil))

(def sum1 (rulefn :sum :sum \+ :times))
(def sum2 (rulefn :sum :times))
(def num1 (rulefn :num \1))

; Some basic tests
(def simple-parser-rules
  {:sum [sum1 sum2]
   :times [(rulefn :times :times \* :num) (rulefn :times :num)]
   :num [num1 (rulefn :num \2) (rulefn :num \3) (rulefn :num \4)
         (rulefn :num \5 \5) (rule :num "777" nil)]})

(def simple-parser (parser :sum simple-parser-rules))

(def-parser-test basic-parser-test simple-parser
  (is-parsing "1+2")
  (is-parsing "1+2*3+4")
  (is-parsing "1*2+3*4")
  (is-parsing "1+55*3+2*55")
  (is-parsing "777") ; test string (seq of chars) literals
  (isnt (parses? "44"))
  (isnt (parses? "55*23"))
  (isnt (parses? "1+2a"))
  (isnt (parses? "7777"))
  (is-parsing "1+55*2*55+3+55*4")
  (is-ast [[[\1]]] "1")
  (is-ast [[[[\2]]] \+ [[[\3]] \* [\4]]] "2+3*4")
  (is-ast [[[[[\1]]] \+ [[[\2]] \* [\3]]] \+ [[[\4]] \* [\1]]] "1+2*3+4*1")
  (is-ast [[[\5 \5]]] "55"))

; Parse trees
(def-parser-test parse-tree-test simple-parser
  (is-parse [sum2 [(rulefn :times :num) [num1 [\1]]]] "1")
  (is-parse [sum2 [(rulefn :times :num) [(rulefn :num \5 \5) [\5] [\5]]]] "55")
  (is-parse [sum1 [sum1 [sum2 [(rulefn :times :num) [num1 [\1]]]] [\+]
                   [(rulefn :times :times \* :num)
                    [(rulefn :times :num) [(rulefn :num \2) [\2]]]
                    [\*] [(rulefn :num \3) [\3]]]]
             [\+]
             [(rulefn :times :times \* :num) [(rulefn :times :num)
                                              [(rulefn :num \4) [\4]]]
              [\*] [(rulefn :num \5 \5) [\5] [\5]]]]
            "1+2*3+4*55"))

; Tokenizers
(defn letter-to-num [thechar]
  (if (java.lang.Character/isLetter thechar)
    (char (- (int thechar) 48))
    thechar))

(def letter-to-num-parser (parser :sum letter-to-num simple-parser-rules))

(def-parser-test basic-tokenizer-test letter-to-num-parser
  (is-ast [[[\a]]] "a")
  (is-ast [[[[[\a]]] \+ [[[\2]] \* [\c]]] \+ [[[\d]] \* [\1]]] "a+2*c+d*1")
  (is-parse [sum2 [(rulefn :times :num) [num1 [\a]]]] "a"))

; Action tests
(def calculator-rules
  {:sum [(rule :sum [:sum \+ :times] (fn [a _ b] (+ a b)))
         (rule :sum [:times] identity)]
   :times [(rule :times [:times \* :num] (fn [a _ b] (* a b)))
           (rule :times [:num] identity)]
   :num [(rule :num [\2] (fn [_] 2))
         (rule :num [\3] (fn [_] 3))]})

(def calculator-parser (parser :sum calculator-rules))

(def-parser-test calculator-test calculator-parser
  (is-action 5 "2+3")
  (is-action 6 "2*3")
  (is-action 19 "2*3+2*2+3*3"))

; Rule embedding
(def embedded-rules
    {:a [(rule :a [\a [\b \c] (rule :d [\d] nil)] nil)]})

(def embedded-rule-parser (parser :a embedded-rules))

(def-parser-test rule-embedding-test embedded-rule-parser
    (is-parsing "abd")
    (is-parsing "acd")
    (not-parsing "abcd"))

; Test of defrule
(defrule sum
  ([sum \+ times] (+ sum times))
  ([times] times))
(defrule times
  ([times \* digit] (* times digit))
  ([digit] digit))
(defrule digit [\3] 3)

(def parser2 (build-parser sum))

(def-parser-test build-parser-test parser2
  (is-action 3 "3")
  (is-action 9 "3*3")
  (is-action 6 "3+3")
  (is-action 15 "3+3*3+3"))

; Extending rules
(extend-rule digit [\4] 4)
(def parser3 (build-parser sum))

(def-parser-test extend-rule-test parser3
  (is-action 7 "3+4")
  (is-action 12 "3*4"))

; Rule aliasing
(extend-rule sum [sum \- (foo times)] (- sum foo))
(def parser4 (build-parser sum))

(def-parser-test rule-aliasing-test parser4
  (is-action 0 "3-3"))

; Grammars... we will use this later
(def incomplete-grammar (build-grammar sum))

; Rule literals in defrule
(def digits567 [(token \5 5) (token \6 6) (token \7 7)])
(extend-rule digit
             ([digits567] digits567)
             ([(a-digit [(token \8 8) (token \9 9)])] a-digit))
(def parser5 (build-parser sum))

(def-parser-test rule-literal-test parser5
  (is-action 2 "7-5")
  (is-action 1 "9-8")
  (is-action 4 "9-5"))

; Chart str format isn't fixed... just test not nil for now
(deftest print-charts-test
  (is (with-out-str
        (print-charts parser5 "3*4+5-6+7"))))

; Scanners
(add-rules digit (scanner #(= \0 %) (fn [_] 0)))
(def parser6 (build-parser sum))

(def-parser-test scanner-test parser6
  (is-action 3 "0+3")
  (is-action 1 "3+0*5*4+0+3-5"))

; Char ranges
(def digit (char-range \0 \9 (fn [c]
                               (- (int c)
                                  (int \0)))))
(def parser7 (build-parser sum))
(def full-grammar (build-grammar sum))

(def-parser-test char-range-test parser7
  (is-action 1 "1")
  (is-action 3 "1+2")
  (is-action 23 "0+1*2+3*4+9"))

;TODO tests on grammars alone

(def one-or-more-s (one-or-more \s))
(def one-or-more-test-parser (build-parser one-or-more-s))
(deftest special-rules-test
  (with-parser one-or-more-test-parser
    (is-parsing "sssss")
    (isnt (parses? "sssst"))))
