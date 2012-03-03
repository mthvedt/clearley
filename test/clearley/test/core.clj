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

(def simple-parser (earley-parser simple-parser-rules :sum))

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

(deftest simple-match-test
  (is (tree-eq [sum2 [(rule :times :num) [num1 [\1]]]]
               (match-rules simple-parser "1")))
  (is (tree-eq [sum2 [(rule :times :num) [(rule :num \5 \5) [\5] [\5]]]]
               (match-rules simple-parser "55")))
  (is (tree-eq [sum1 [sum1 [sum2 [(rule :times :num) [num1 [\1]]]] [\+]
                      [(rule :times :times \* :num)
                       [(rule :times :num) [(rule :num \2) [\2]]]
                       [\*] [(rule :num \3) [\3]]]]
                [\+]
                [(rule :times :times \* :num) [(rule :times :num) [(rule :num \4) [\4]]]
                 [\*] [(rule :num \5 \5) [\5] [\5]]]]
               (match-rules simple-parser "1+2*3+4*55"))))

; (def weird-ruleset [(rule :head :a :b)
;                     (rule :a :a :b \a)
;                     (rule :b :a :b \b)])

; (def weird-parser (earley-parser weird-ruleset :head))
