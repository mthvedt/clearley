(defproject clearley "0.4.0-SNAPSHOT"
  :description "Parsing for Earthlings."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [eightnotrump/uncore "0.2.0"]
                 [backtick "0.3.0-20130220.225123-3"]]
  :test-resource-paths ["test"]
  :java-source-paths ["java"]
  :aliases {"bench" ["with-profile" "dev,benchmark" "run"]
            "bench-verbose" ["with-profile" "dev,benchmark,vminfo" "run"]}
  :profiles {:dev {:repositories {"stuartsierra-releases"
                                  "http://stuartsierra.com/maven2",
                                  "stuartsierra-snapshots"
                                  "http://stuartsierra.com/m2snapshots"}
                   :dependencies [[com.stuartsierra/lazytest "2.0.0-SNAPSHOT"]
                                  [org.clojure/math.numeric-tower "0.0.2"]
                                  [criterium "0.3.1"]]
                   :plugins [[eightnotrump/lein-lazytest "1.0.5"]
                             [codox "0.6.4"]]
                   :global-vars {*assert* true}
                   :codox {:exclude [clearley.npda clearley.earley
                                     clearley.rules]}}
             :eightnotrump {:codox
                            {:src-dir-uri
                             "http://github.com/eightnotrump/clearley/blob/master",
                             :src-linenum-anchor-prefix "L"
                             :output-dir "doc/codox"}}
             :benchmark {:dependencies [[org.clojure/data.json "0.2.2"]
                                        [clj-json "0.5.3"]
                                        [cheshire "5.1.0"]]
                         :gloabl-vars {*warn-on-reflection* true}
                         :source-paths ["benchmark" "test"]
                         :resource-paths ["benchmark"]
                         :main clearley.benchmark.main}
             :vminfo {:jvm-opts ["-XX:+PrintGCDetails"
                                 "-XX:+PrintCompilation"]}})
