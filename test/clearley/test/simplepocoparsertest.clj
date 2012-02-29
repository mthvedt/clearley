(ns clearley.test.simplepocoparsertest
  "A simple unit test suite for a simple CFG parser that emits ASTs of POCOs
  (Plain Old Clojure Objects)."
  (:use clearley.core clojure.test))

(def base-parser-ruleset [(rule :sum :sum \+ :times)
                          (rule :sum :times)
                          (rule :times :times \* :num)
                          (rule :times :num)
                          (rule :num \1)
                          (rule :num \2)
                          (rule :num \3)
                          (rule :num \4)])

(def base-parser (earley-parser base-parser-ruleset :sum))

; (defn test-ns-hook [] nil) ; Tell clojure.test to ignore tests in this file

; For ambiguating parsers--impl later
;(defmacro is-parse [expected testval]
;  `(testing ~(str "Parsing " testval)
;            (let [result# (parse base-parser ~testval)]
;              (is (= 1 (count result#))) ; returns a vector of results
;              (is (= ~expected result#)))))

(defmacro is-parse [expected testval]
  `(is (= ~expected (parse base-parser ~testval))))

(deftest base-parser-test
  (is-parse [[[\1]]] "1")
  (is-parse [[[[\2]]] \+ [[[\3]] \* [\4]]] "2+3*4")
  (is-parse [[[[[\1]]] \+ [[[\2]] \* [\3]]] \+ [[[\4]] \* [\1]]] "1+2*3+4*1"))
  ; todo: add test for incomplete parse.

(def weird-ruleset [(rule :head :a :b)
                    (rule :a :a :b \a)
                    (rule :b :a :b \b)])

(def weird-parser (earley-parser weird-ruleset :head))
