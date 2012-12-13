(ns clearley.test.utils
  (:use clearley.core lazytest.deftest))

(defmacro is= [& forms]
  `(is (= ~@forms)))

(defmacro isnt [form]
  `(is (not ~form)))

(defmacro isnt= [& forms]
  `(is (not (= ~@forms))))

(def ^:dynamic local-parser)

(defmacro with-parser [parser & forms]
  `(binding [local-parser ~parser] ~@forms))

; I want to make this more compact by building the parser inline,
; but deftest closes over the tests you pass it -> can't build a parser off
; a local symbol...

; TODO: use macro for other tests
(defmacro def-parser-test [test-name parser & forms]
  `(deftest ~test-name
     (with-parser ~parser
       ~@forms)))

(defn parses? [input]
  (not (nil? (parse local-parser input))))

(defmacro is-parsing [input]
  `(is (parses? ~input)))

(defmacro not-parsing [input]
  `(is (not (parses? ~input))))

(defmacro action-throws [exception-type input]
  `(is (thrown? ~exception-type (take-action (parse local-parser ~input)))))

; valued trees of the form (value & branches)
; neccesary for comparing heterogeneous seqables (here, vec vs lazy-seq)
(defn tree-eq [tree1 tree2]
  (if (nil? tree1)
    (nil? tree2)
    (and (not (nil? tree2))
         (= (first tree1) (first tree2))
         (= (count tree1) (count tree2))
         ; this is ugly because and is a macro--is there an and fn?
         (reduce #(and % %2) true (map tree-eq (rest tree1) (rest tree2))))))

(deftest tree-eq-test
  ; only need to test for falsehoods here... avoid false positives
  (is (not (tree-eq [\1] [\2])))
  (is (not (tree-eq [] [[]])))
  (is (not (tree-eq [\1 []] [\1])))
  (is (not (tree-eq [] nil))))

; Yields a simple AST of the match, in the form [rule submatches*]
(defn match-tree [match]
  ((fn f [m]
     (if (instance? clearley.rules.Match m)
       (let [{:keys [rule submatches]} m]
         (apply vector rule (map f submatches)))
       m)) match))

; Strips rule nodes from tree
(defn stripped-match-tree [match]
  ((fn f [m] ; This fn will recursively reduce the match tree
     (if-let [submatches (seq (rest m))]
       (vec (map f submatches))
       (first m)))
     (match-tree match)))

(defmacro is-ast [expected testval]
  `(is= ~expected (stripped-match-tree (parse local-parser ~testval))))

(defmacro is-parse [expected testval]
  `(is (tree-eq ~expected (match-tree (parse local-parser ~testval)))))

(defmacro is-action [expected testval]
  `(is= ~expected (take-action (parse local-parser ~testval))))
