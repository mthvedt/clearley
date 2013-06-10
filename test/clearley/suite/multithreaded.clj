(ns clearley.suite.multithreaded
  (use clearley.core clearley.match clearley.test.utils uncore.test.utils
       lazytest.deftest [clearley.examples.json :exclude [json-parser]]))
; Make sure building and running parsers is thread safe

; Fresh parser (don't use json's)
#_(def json-ns (find-ns 'clearley.examples.json))
#_(def json-parser (build-parser-dynamic-with-ns
                   json-ns 'clearley.examples.json/whitespace-object))
#_(def json-parser-parallel (build-parser-dynamic-with-ns
                            json-ns 'clearley.examples.json/whitespace-object))

#_(def-parser-test multithreaded-parser-test json-parser
  (let [r-atom (atom [])
        test-map (read (java.io.PushbackReader.
                         (get-resource "clearley/examples/json_test.edn")))
        input-string (slurp (get-resource "clearley/examples/json_test.json"))]
    (is= test-map (execute json-parser input-string))
    (is (reduce #(or % %2) (doall (pmap #(= test-map (execute json-parser-parallel %))
                                        (repeat 10 input-string)))))))
