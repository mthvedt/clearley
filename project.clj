(defproject clearley "0.0.2.SNAPSHOT"
  :description "Parsing for Earthlings"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/math.numeric-tower "0.0.1" :scope "test"]
                 [com.stuartsierra/lazytest "2.0.0-SNAPSHOT" :scope "test"]
                 [criterium "0.3.1" :scope "test"]]
  :profiles {:benchmark {:source-paths ["benchmark" "test"]
                         :resource-paths ["benchmark"]
                         :main clearley.benchmark.test}}
  :repositories {"stuartsierra-releases" "http://stuartsierra.com/maven2"
                 "stuartsierra-snapshots" "http://stuartsierra.com/m2snapshots"})
