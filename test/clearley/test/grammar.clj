(ns clearley.test.grammar
  (use clearley.match clearley.grammar lazytest.deftest))

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
  (is (get grammar1 'clearley.test.grammar/sum))
  (is (get grammar1 'clearley.test.grammar/times))
  (is (get grammar1 'clearley.test.grammar/digit)))
