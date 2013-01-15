(ns clearley.benchmark.json.test
  (require [clojure.java.io :as io]
           [clearley.examples.json :as json])
  (use [clearley core]
       [criterium core]))

(def prefix "clearley/benchmark/json/")

(defn bench-parser [filename]
  (println "Loading" filename "into memory")
  (let [loaded-file (-> (str prefix filename)
                      io/resource io/reader slurp)]
    (println "Chars loaded:" (count loaded-file))
    (println "Benchmarking")
    (bench (parse json/json-parser loaded-file))))

(defn -main [& args]
  (bench-parser "small-test.json")
  ; With no lookahead, each consecutive space adds O(1) ambiguity
  (bench-parser "small-test-formatted.json")
  )
