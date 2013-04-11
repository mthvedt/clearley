(ns clearley.benchmark.test-json
  (require [clojure.java.io :as io]
           [clearley.examples.json :as json])
  (use clearley.core clearley.benchmark.core criterium.core))

(defn -main []
  (sanity-check json/json-parser "small-test.json" "small-test-json.clj"))
