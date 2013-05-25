(ns clearley.test.quentin
  (use clearley.core clearley.match clearley.lib clearley.test.utils))

(defbind any-object-test-rule [_ :foo
                          _ (token 'bar)
                          _ (scanner vector?)]
  "hello world!")
(def any-object-test-parser (build-parser any-object-test-rule :stream-type :objs))
(def-parser-test match-any-object any-object-test-parser
  (is-action "hello world!" [:foo 'bar []]))
