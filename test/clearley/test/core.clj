(ns clearley.test.core
  "Tests for core. Very little is tested here--most goes in
  various parser testers."
  (use clearley.core clearley.match clearley.grammar clearley.test.utils
       uncore.test.utils lazytest.deftest))

(defmatch sum
  ([sum \+ times] (+ sum times))
  times)
(defmatch times
  ([times \* digit] (* times digit))
  digit)
(defmatch digit [\3] 3)

(def parser1 (build-parser quentin-parser sum))

(def-parser-test build-parser-test parser1
  (testing "Basic parsing"
           (is-parsing "3+3")
           (not-parsing "4+4"))
  (testing "Basic actions"
           (is-action 6 "3+3")
           (is-action 9 "3*3")
           (is-action 15 "3+3*3+3")))

(defmatch digit ([\1] 1) ([\2] 2) ([\3] 3) ([\4] 4) ([\5 \5] 55))
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
