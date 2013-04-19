(ns clearley.benchmark.test-json
  (require [clearley.examples.json :as json])
  (use clearley.core clearley.benchmark.core criterium.core))

(defn -main []
  (sanity-check json/json-parser "small-test.json" "small-test-json.clj")
  (sanity-check json/json-parser "small-test-formatted.json" "small-test-json.clj")

  ; JSON grammar is relatively complex--LR parsers should tend to do well
  ;(bench-from-file "JSON" json/json-parser "small-test.json")
  ; With no lookahead, each consecutive space adds O(1) ambiguity
  (print-sep)
  (bench-from-file "Formatted JSON" json/json-parser "small-test-formatted.json")
  )
