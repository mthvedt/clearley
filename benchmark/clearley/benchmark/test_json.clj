(ns clearley.benchmark.test-json
  (require [clearley.examples.json :as json])
  (use clearley.core clearley.benchmark.core criterium.core))

(defn -main []
  ; JSON grammar is relatively complex--LR parsers should tend to do well
  ; With no lookahead, each consecutive space adds O(1) ambiguity
  (print-sep)
  #_(bench-recognizer "Formatted JSON: recognizing"
                      json/json-parser "small-test-formatted.json")
  (with-progress-reporting
    (bench-from-file "Formatted JSON: parsing"
                     (build-parser quentin-parser
                                   clearley.examples.json/whitespace-object)
                     "small-test-formatted.json")))
