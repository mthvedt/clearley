(defproject clearley "0.2.0-SNAPSHOT"
  :description "Parsing for Earthlings"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/math.numeric-tower "0.0.1" :scope "test"]
                 [com.stuartsierra/lazytest "2.0.0-SNAPSHOT" :scope "test"]
                 [criterium "0.3.1" :scope "test"]
                 [eightnotrump/uncore "0.1.0-SNAPSHOT"]]
  :plugins [[eightnotrump/lein-lazytest "1.0.5"]]
  :profiles {:benchmark {:source-paths ["benchmark" "test"]
                         :resource-paths ["benchmark"]
                         :main clearley.benchmark.test}}
  :repositories {"stuartsierra-releases" "http://stuartsierra.com/maven2"
                 "stuartsierra-snapshots" "http://stuartsierra.com/m2snapshots"})
