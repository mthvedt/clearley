(ns clearley.benchmark.core
  (require [clojure.java.io :as io]
           [clearley.test.utils :as utils]
           [uncore.throw :as t])
  (use clearley.core clearley.benchmark.core criterium.core))

(def prefix "clearley/benchmark/")

(defn print-sep []
  (println (apply str (repeat 80 \=))))

(defn bench-str [name parser str parse?]
  (println "Benchmark:" name)
  (println "Input size:" (count str))
  (if (execute parser str)
    (do
      (println "Benchmarking")
      (if parse?
        (bench (execute parser str))
        (bench (execute parser str)))
      (print-sep))
    (println "!!!!Failure to parse!!!!")))

(defn get-resource [filename]
  (utils/get-resource (str prefix filename)))

(defn bench-recognizer [name parser filename]
  (println "Loading" filename "into memory")
  (let [loaded-file (slurp (get-resource filename))]
    (bench-str name parser loaded-file false)))

(defn bench-from-file [name parser filename]
  (println "Loading" filename "into memory")
  (let [loaded-file (slurp (get-resource filename))]
    (bench-str name parser loaded-file true)))
