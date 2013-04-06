(ns clearley.benchmark.test
  (require [clojure.java.io :as io]
           [clearley.examples.json :as json]
           [clearley.examples.calculator :as calc])
  (use [clearley core]
       [criterium core]))

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

(defrule S
  ([(s1 S) (s2 S)] (str s1 s2))
  ([(s1 S) (s2 S) (s3 S)] (str s1 s2 s3))
  ([\s] "s"))

(def pathological-parser-1 (build-parser S))

(defn -main [& args]
  (print-sep)

  ; Test memory bounds
  ; (println "Trying memory bounds")
  ; (parse calc/my-calculator (cons \1 (apply concat (repeat (* 512 1000 1000) "+1"))))

  ; Start with some microbenchmarks
  (bench-str "Left-recursive calculator" calc/my-calculator "1+2+3+4+5+6+7+8")
  ; Should have linear growth
  (bench-str "Left-recursive calculator 2"
             calc/my-calculator "1+2+3+4+5+6+7+8+1+2+3+4+5+6+7+8")
  (bench-str "Right-recursive calculator" calc/my-calculator "1^2^3^4^5^6^7^8")
  ; This has quadratic growth without lookahead, linear growth with it.
  (bench-str "Right-recursive calculator 2"
             calc/my-calculator "1^2^3^4^5^6^7^8^1^2^3^4^5^6^7^8")
  ; Snakes... why did it have to be snakes?
  ; This is a quadratic grammar, but can incur O(n^4) parse time in some GLR impls. is a quadratic grammar, but can incur O(n^4) parse time in some GLR impls.
  ; TODO: add a cubic grammar.
  (bench-str "Pathological grammar 1.1" pathological-parser-1 (repeat 20 \s)) 
  (bench-str "Pathological grammar 1.2" pathological-parser-1 (repeat 40 \s))
  (bench-str "Pathological grammar 1.3" pathological-parser-1 (repeat 80 \s))

  ; JSON grammar is relatively complex--LR parsers should tend to do well
  (bench-from-file "JSON" json/json-parser "small-test.json")
  ; With no lookahead, each consecutive space adds O(1) ambiguity
  (bench-from-file "Formatted JSON" json/json-parser "small-test-formatted.json")
  )
