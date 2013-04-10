(ns clearley.benchmark.core
  (require [clojure.java.io :as io])
  (use clearley.core clearley.benchmark.core criterium.core))

(def prefix "clearley/benchmark/")

(defn print-sep []
  (println (apply str (repeat 80 \=))))

(defn bench-str [name parser str]
  (println "Benchmark:" name)
  (println "Input size:" (count str))
  (if-let [r (parse parser str)]
    (do
      (println "Benchmarking")
      (bench (parse parser str))
      (print-sep))
    (println "!!!!Failure to parse!!!!")))

(defn bench-from-file [name parser filename]
  (println "Loading" filename "into memory")
  (let [loaded-file (-> (str prefix filename)
                      io/resource io/reader slurp)]
    (bench-str name parser loaded-file)))

