(ns clearley.test.utils
  (require [clojure.java.io :as io]
           [backtick]
           [clearley.grammar :as g]
           lazytest.find lazytest.suite)
  (:use uncore.test.utils clearley.core lazytest.test-case lazytest.deftest))

; TODO test building parser from different ns in core

(def ^:dynamic *local-parser*)
(def ^:dynamic *parser-ns*)
(def ^:dynamic *parser-builder*
  (fn [goal grammar] (parser goal grammar)))

(defn build-parser-dynamic-with-ns [a-ns form]
  (*parser-builder* form (g/build-grammar-with-ns form a-ns)))

; Macro for defing a test that builds a parser with the dynamically bound
; parser builder fn and runs the given tests
(defmacro defptest [name goal & forms]
  `(let [goal# (backtick/resolve-symbol '~goal) ; Capture the namespaced goal
         ; Avoid ugly local state dumps on test failure. I call this
         ; the Brad Pitt technique because print-object doesn't know what's in the box
         grammar# (clojure.lang.Box. (g/build-grammar ~goal))]
     (def ~name
       (test-case
         (vary-meta (fn []
                      (binding [*local-parser* (*parser-builder* goal#
                                                                 (.val grammar#))]
                        ~@forms))
                    merge '~(meta name) {:name '~name})))))

(defmacro with-parser [parser & forms]
  `(binding [*local-parser* ~parser] ~@forms))

(defmacro def-parser-test [test-name parser & forms]
  `(deftest ~test-name
     (with-parser ~parser
       ~@forms)))

(defn parses? [input]
  (execute *local-parser* input))

(defmacro is-parsing [input]
  `(is (parses? ~input)))

(defmacro not-parsing [input]
  `(is (not (parses? ~input))))

(defmacro action-throws [exception-type input]
  `(is (thrown? ~exception-type (execute *local-parser* ~input))))

; valued trees of the form (value & branches)
; neccesary for comparing heterogeneous seqables (here, vec vs lazy-seq)
#_(defn tree-eq [tree1 tree2]
  (if (nil? tree1)
    (nil? tree2)
    (and (not (nil? tree2))
         (= (first tree1) (first tree2))
         (= (count tree1) (count tree2))
         ; this is ugly because and is a macro--is there an and fn?
         (reduce #(and % %2) true (map tree-eq (rest tree1) (rest tree2))))))

#_(deftest tree-eq-test
  ; only need to test for falsehoods here... avoid false positives
  (is (not (tree-eq [\1] [\2])))
  (is (not (tree-eq [] [[]])))
  (is (not (tree-eq [\1 []] [\1])))
  (is (not (tree-eq [] nil))))

; Yields a simple AST of the match, in the form [rule submatches*]
#_(defn match-tree [match]
  ((fn f [m]
     (if (instance? clearley.rules.Match m)
       (let [{:keys [rule submatches]} m]
         (apply vector rule (map f submatches)))
       m)) match))

; Strips rule nodes from tree
#_(defn stripped-match-tree [match]
  ((fn f [m] ; This fn will recursively reduce the match tree
     (if-let [submatches (seq (rest m))]
       (vec (map f submatches))
       (first m)))
     (match-tree match)))

#_(defmacro is-ast [expected testval]
  `(is= ~expected (stripped-match-tree (parse *local-parser* ~testval))))

#_(defmacro is-parse [expected testval]
  `(is (tree-eq ~expected (match-tree (parse *local-parser* ~testval)))))

(defmacro is-action [expected testval]
  `(is= ~expected (execute *local-parser* ~testval)))

(defn get-resource [filename]
  (-> filename io/resource io/reader))

(defn compare-to-file [parser parse-file clojure-file]
  (let [test-parse (execute parser (slurp (get-resource parse-file)))
        test-comparo (read (java.io.PushbackReader. (get-resource clojure-file)))]
    (= test-parse test-comparo)))
