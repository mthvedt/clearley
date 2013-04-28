(ns clearley.benchmark.main
  (require [clearley.benchmark test grammars test-json])
  (:gen-class)
  (use clearley.benchmark.core))

(defmacro do-main [the-ns]
  `(do
     (~(symbol (str the-ns "/-main")))))

(defn -main []
  (do-main clearley.benchmark.test)
  (do-main clearley.benchmark.builder)
  (do-main clearley.benchmark.test-json))
