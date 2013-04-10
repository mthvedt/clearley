(ns clearley.benchmark.test-json
  (require [clojure.java.io :as io]
           [clearley.examples.json :as json]
           [clearley.examples.calculator :as calc])
  (use [clearley core]
       [clearley.benchmark core]
       [criterium core]))
