(ns clearley.benchmark.main
  (require [clearley.benchmark test test-json])
  (use clearley.benchmark.core))

(defmacro do-main [the-ns]
  `(do
     ; (require '~the-ns) TODO: why doesn't this work?
     (~(symbol (str the-ns "/-main")))))

(defn -main []
  (do-main clearley.benchmark.test)
  (do-main clearley.benchmark.test-json))
