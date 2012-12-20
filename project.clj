(defproject clearley "0.1.1.ALPHA-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/math.numeric-tower "0.0.1" :scope "test"]
                 [com.stuartsierra/lazytest "2.0.0-SNAPSHOT" :scope "test"]]
  :profiles {:bigtest {:test-paths ["bigtest"]}}
  :repositories {"stuartsierra-releases" "http://stuartsierra.com/maven2"
                 "stuartsierra-snapshots" "http://stuartsierra.com/m2snapshots"})
