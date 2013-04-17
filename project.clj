(defproject clearley "0.3.0-SNAPSHOT"
  :description "Parsing for Earthlings"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [eightnotrump/uncore "0.1.0"]]
  :plugins [[eightnotrump/lein-lazytest "1.0.5"]]
  :profiles {:dev {:repositories {"stuartsierra-releases" "http://stuartsierra.com/maven2"
                                  "stuartsierra-snapshots" "http://stuartsierra.com/m2snapshots"}
                   :dependencies [[com.stuartsierra/lazytest "2.0.0-SNAPSHOT" :scope "test"]
                                  [org.clojure/math.numeric-tower "0.0.1" :scope "test"]
                                  [criterium "0.3.1" :scope "test"]]}
             :benchmark {:source-paths ["benchmark" "test"]
                         :resource-paths ["benchmark"]
                         :main clearley.benchmark.test}})
