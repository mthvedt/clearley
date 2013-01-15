(defproject clearley "0.1.1.ALPHA-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/math.numeric-tower "0.0.1" :scope "test"]
                 [com.stuartsierra/lazytest "2.0.0-SNAPSHOT" :scope "test"]
                 [criterium "0.3.1" :scope "test"]]
  :profiles {:bigtest {:test-paths ["bigtest"]}
             :benchmark {:source-paths ["benchmark" "test"]
                         :resource-paths ["benchmark"]
                         :main clearley.benchmark.json.test}}
  :repositories {"stuartsierra-releases" "http://stuartsierra.com/maven2"
                 "stuartsierra-snapshots" "http://stuartsierra.com/m2snapshots"})
