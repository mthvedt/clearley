(ns clearley.test
  (require lazytest.find lazytest.report.nested lazytest.runner.console
           lazytest.watch lazytest.reload)
  (use clearley.core clearley.test.utils))
; clojure.test supports modular tests, but it stinks.
; lazytest supports modular tests, but some work is required.
; this class is the main entry point for tests--don't use lein lazytest.

; addendum: someday, hopefully, we can replace this all with test2

(defn run-parser-tests [builder namespaces]
  (binding [*parser-builder* builder]
    (apply require namespaces)
    (apply lazytest.runner.console/run-tests namespaces)))

(def base-test-seq
  '[clearley.suite.deterministic
    clearley.suite.multithreaded
    clearley.examples.json
    clearley.examples.calculator
    clearley.examples.simplest-calculator])

(def nd-test-seq (cons 'clearley.suite.nondeterministic base-test-seq))

(defn run-tests [& _]
  (doseq [r [
             (run-parser-tests quentin-parser base-test-seq)
             ]]
    (lazytest.report.nested/report r)))

;(defn -main [] (run-tests))

(defn -main []
  (lazytest.watch/start ["src" "test"]
                        :run-fn run-tests
                        :report-fn (fn [& _] nil)))
