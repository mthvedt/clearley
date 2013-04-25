(ns clearley.test.multithreaded
  (use clearley.core clearley.defrule clearley.test.utils uncore.test.utils
       lazytest.deftest [clearley.examples.json :exclude [json-parser]]))
; This tests multithreading parsers (important if you're memoizing)

(defrule S
  ([(s1 S) (s2 S)] (str s1 s2))
  ([(s1 S) (s2 S) (s3 S)] (str s1 s2 s3))
  ([\s] "s"))

; Fresh parser (don't use json's)
(def json-parser (build-parser whitespace-object))
(def json-parser-parallel (build-parser whitespace-object))

(def-parser-test multithreaded-parser-test json-parser
  (let [r-atom (atom [])
        test-map (read (java.io.PushbackReader.
                         (get-resource "clearley/examples/json_test.edn")))
        input-string (slurp (get-resource "clearley/examples/json_test.json"))]
    (is= test-map (execute json-parser input-string))
    (is (reduce #(or % %2) (doall (pmap #(= test-map (execute json-parser-parallel %))
                                        (repeat 10 input-string)))))))
