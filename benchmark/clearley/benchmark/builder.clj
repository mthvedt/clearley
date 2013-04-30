(ns clearley.benchmark.builder
  (:gen-class)
  (require clearley.grammar [uncore.throw :as t] clearley.benchmark.core
           clearley.examples.json)
  (use clearley.core criterium.core clearley.test.utils))

(defn -main []
  (clearley.benchmark.core/print-sep)
  ; Benchmark parser building
  ; Run an input file that should test the gamut.
  (let [input-str (slurp (get-resource "clearley/examples/json_test.json"))
        grammar (clearley.grammar/build-grammar-with-ns
                  'whitespace-object
                  (find-ns 'clearley.examples.json))]
    (let [parser (parser 'clearley.examples.json/whitespace-object grammar)]
      (when-not (= (read (java.io.PushbackReader.
                           (get-resource "clearley/examples/json_test.edn")))
                   (execute parser input-str))
        (t/RE "Sanity check failed")))
    (println "Benchmark: parser building")
    (bench (let [parser (parser 'clearley.examples.json/whitespace-object grammar)]
             (execute parser input-str)))))
