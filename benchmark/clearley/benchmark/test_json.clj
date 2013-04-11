(ns clearley.benchmark.test-json
  (require [clojure.java.io :as io]
           [clearley.examples.json :as json]
           clojure.data.json
           clj-json.core
           cheshire.core)
  (use clearley.core clearley.benchmark.core criterium.core))

(defn compariton [str subject]
  (println "Benchmarking for comparison:" str)
  (let [loaded-file (slurp (get-resource "small-test-formatted.json"))]
    (bench (subject loaded-file))))

(defn -main []
  (sanity-check json/json-parser "small-test.json" "small-test-json.clj")
  (sanity-check json/json-parser "small-test-formatted.json" "small-test-json.clj")

  ; JSON grammar is relatively complex--LR parsers should tend to do well
  ;(bench-from-file "JSON" json/json-parser "small-test.json")
  ; With no lookahead, each consecutive space adds O(1) ambiguity
  (bench-from-file "Formatted JSON" json/json-parser "small-test-formatted.json")

  (compariton "clojure.core/data.json" clojure.data.json/read-str)
  (compariton "clj-json" #(clj-json.core/parse-string %))
  (compariton "Cheshire" #(cheshire.core/parse-string %))
  )
