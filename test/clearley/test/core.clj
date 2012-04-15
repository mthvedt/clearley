(ns clearley.test.core
  (:use clearley.core clojure.test))

(def sum1 (rule :sum :sum \+ :times))
(def sum2 (rule :sum :times))
(def num1 (rule :num \1))

(def simple-parser-rules [sum1
                          sum2
                          (rule :times :times \* :num)
                          (rule :times :num)
                          num1
                          (rule :num \2)
                          (rule :num \3)
                          (rule :num \4)
                          (rule :num \5 \5)])

(def simple-parser-grammar (grammar simple-parser-rules))

(def simple-parser (earley-parser :sum simple-parser-rules))

(deftest grammars
  (is (= [sum1 sum2] (vec (get simple-parser-grammar :sum)))))

(defmacro is= [& forms]
  `(is (= ~@forms)))

(def ^:dynamic local-parser)

(defmacro with-parser [parser & forms]
  `(binding [local-parser ~parser] ~@forms))

(defmacro is-parse [expected testval]
  `(is= ~expected (parse local-parser ~testval)))

(defn parses? [input]
  (not (nil? (parse local-parser input))))

(deftest simple-parser-test
  (with-parser simple-parser
    (is (parses? "1+2"))
    (is (parses? "1+2*3+4"))
    (is (parses? "1*2+3*4"))
    (is (parses? "1+55*3+2*55"))
    (is (not (parses? "44")))
    (is (not (parses? "55*23")))
    (is (parses? "1+55*2*55+3+55*4"))
    (is-parse [[[\1]]] "1")
    (is-parse [[[[\2]]] \+ [[[\3]] \* [\4]]] "2+3*4")
    (is-parse [[[[[\1]]] \+ [[[\2]] \* [\3]]] \+ [[[\4]] \* [\1]]] "1+2*3+4*1")
    (is-parse [[[\5 \5]]] "55")))
; todo: add test for incomplete parse.

; valued trees of the form (value & branches)
; neccesary for comparing heterogeneous seqables (here, vec vs lazy-seq)
(defn tree-eq [tree1 tree2]
  (if (nil? tree1)
    (nil? tree2)
    (and (not (nil? tree2))
         (= (first tree1) (first tree2))
         (= (count tree1) (count tree2))
         ; this is ugly because and is a macro--is there an and fn?
         (reduce #(and % %2) true (map tree-eq (rest tree1) (rest tree2))))))

(deftest tree-eq-test
  ; only test for falsehoods here... avoid false positives in later testing
  (is (not (tree-eq [\1] [\2])))
  (is (not (tree-eq [] [[]])))
  (is (not (tree-eq [\1 []] [\1])))
  (is (not (tree-eq [] nil))))

(defmacro is-match [expected testval]
  `(is (tree-eq ~expected (match-rules local-parser ~testval))))

(deftest simple-match-test
  (with-parser simple-parser
    (is-match [sum2 [(rule :times :num) [num1 [\1]]]] "1")
    (is-match [sum2 [(rule :times :num) [(rule :num \5 \5) [\5] [\5]]]] "55")
    (is-match [sum1 [sum1 [sum2 [(rule :times :num) [num1 [\1]]]] [\+]
                     [(rule :times :times \* :num)
                      [(rule :times :num) [(rule :num \2) [\2]]]
                      [\*] [(rule :num \3) [\3]]]]
               [\+]
               [(rule :times :times \* :num) [(rule :times :num) [(rule :num \4) [\4]]]
                [\*] [(rule :num \5 \5) [\5] [\5]]]]
              "1+2*3+4*55")))

(defn letter-to-num [thechar]
  (if (java.lang.Character/isLetter thechar)
    (char (- (int thechar) 48))
    thechar))

(def letter-to-num-parser (earley-parser :sum letter-to-num simple-parser-rules))

(deftest simple-tokenizer-test
  (with-parser letter-to-num-parser
    (is-parse [[[\a]]] "a")
    (is-parse [[[[[\a]]] \+ [[[\2]] \* [\c]]] \+ [[[\d]] \* [\1]]] "a+2*c+d*1")
    (is-match [sum2 [(rule :times :num) [num1 [\a]]]] "a")))

(def calculator-rules
  [(rulefn :sum [:sum \+ :times] (fn [a _ b] (+ a b)))
   (rulefn :sum [:times] identity)
   (rulefn :times [:times \* :num] (fn [a _ b] (* a b)))
   (rulefn :times [:num] identity)
   (rulefn :num [\2] (fn [_] 2))
   (rulefn :num [\3] (fn [_] 3))])

(def calculator-parser (earley-parser :sum calculator-rules))

(defmacro is-action [expected testval]
  `(is= ~expected (take-action (match local-parser ~testval))))

(deftest calculator-test
  (with-parser calculator-parser
    (is-action 5 "2+3")
    (is-action 6 "2*3")
    (is-action 19 "2*3+2*2+3*3")))

; (def weird-ruleset [(rule :head :a :b)
;                     (rule :a :a :b \a)
;                     (rule :b :a :b \b)])

; (def weird-parser (earley-parser weird-ruleset :head))

; TODO: test lr vs ll

(def simple-parser-rules [sum1
                          sum2
                          (rule :times :times \* :num)
                          (rule :times :num)
                          num1
                          (rule :num \2)
                          (rule :num \3)
                          (rule :num \4)
                          (rule :num \5 \5)])

(defrule sum
  ([sum \+ times] (+ sum times))
  ([times] times))
(defrule times
  ([times \* digit] (* times digit))
  ([digit] digit))
(defrule digit [\3] 3)

(def parser2 (build-parser sum identity))

(deftest build-parser-test
  (with-parser parser2
    (is-action 3 "3")
    (is-action 9 "3*3")
    (is-action 6 "3+3")
    (is-action 15 "3+3*3+3")))

(extend-rule digit [\4] 4)
(def parser2 (build-parser sum identity))

(deftest extend-rule-test
  (with-parser parser2
    (is-action 7 "3+4")
    (is-action 12 "3*4")))
