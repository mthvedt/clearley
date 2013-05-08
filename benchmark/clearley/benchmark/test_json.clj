(ns clearley.benchmark.test-json
  (require [clearley.examples.json :as json])
  (use clearley.core clearley.benchmark.core criterium.core))

(defn -main []
  ; JSON grammar is relatively complex--LR parsers should tend to do well
  ; With no lookahead, each consecutive space adds O(1) ambiguity
  (print-sep)
  (bench-recognizer "Formatted JSON: recognizing"
                    json/json-parser "small-test-formatted.json")
  (bench-from-file "Formatted JSON: parsing"
                   json/json-parser "small-test-formatted.json"))
