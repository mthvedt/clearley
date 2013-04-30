(ns clearley.benchmark.test
  (require [clojure.java.io :as io]
           [clearley.examples.json :as json]
           [clearley.examples.calculator :as calc])
  (use [clearley core defmatch]
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

; Snakes... why did it have to be snakes? Ssssssss!
(defmatch S
  ([(s1 S) (s2 S)] (str s1 s2))
  ([(s1 S) (s2 S) (s3 S)] (str s1 s2 s3))
  ([\s] "s"))

(def pathological-parser-1 (build-parser S))

(defn -main []
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
  ; This is quadratic in GLR, but seems to be cubic in the current impl.
  ; It can incur O(n^4) behavior in naïve GLR impls.
  ; (In fact, naïve GLR algos are O(n^O(m)) where m is grammar ambiguity)
  (bench-str "Pathological grammar 1.1" pathological-parser-1 (repeat 20 \s)) 
  (bench-str "Pathological grammar 1.2" pathological-parser-1 (repeat 40 \s))
  )
