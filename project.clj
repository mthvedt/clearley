(defproject clearley "0.3.0-SNAPSHOT"
  :description "Parsing for Earthlings"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [eightnotrump/uncore "0.1.0"]]
  :plugins [[eightnotrump/lein-lazytest "1.0.5"]]
  :profiles {:dev {:repositories {"stuartsierra-releases"
                                  "http://stuartsierra.com/maven2",
                                  "stuartsierra-snapshots"
                                  "http://stuartsierra.com/m2snapshots"}
                   :dependencies [[com.stuartsierra/lazytest "2.0.0-SNAPSHOT" ]
                                  [org.clojure/math.numeric-tower "0.0.1" ]
                                  [criterium "0.3.1"]]}
             :benchmark {:dependencies [[org.clojure/data.json "0.2.2"]
                                        [clj-json "0.5.3"]
                                        [cheshire "5.1.0"]]
                         :source-paths ["benchmark" "test"]
                         :resource-paths ["benchmark"]
                         :main clearley.benchmark.test}})
