(ns clearley.benchmark.test-json-comparo
  (require [clojure.java.io :as io]
           clojure.data.json
           clj-json.core
           cheshire.core)
  (use clearley.benchmark.core criterium.core))

(defn compariton [str subject]
  (println "Benchmarking for comparison:" str)
  (let [loaded-file (slurp (get-resource "small-test-formatted.json"))]
    (bench (subject loaded-file)))
  (print-sep))

(defn -main []
  (print-sep)

  (compariton "clojure.core/data.json" clojure.data.json/read-str)
  (compariton "clj-json" #(clj-json.core/parse-string %))
  (compariton "Cheshire" #(cheshire.core/parse-string %))
  )
