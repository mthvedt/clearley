(ns clearley.test.utils
  (:use clearley.core lazytest.deftest))

(defmacro is= [& forms]
  `(is (= ~@forms)))

(def ^:dynamic local-parser)

(defmacro with-parser [parser & forms]
  `(binding [local-parser ~parser] ~@forms))

(defmacro is-parse [expected testval]
  `(is= ~expected (parse local-parser ~testval)))

(defn parses? [input]
  (not (nil? (parse local-parser input))))

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
  ; only test for falsehoods here... avoid false positives in later testing
  (is (not (tree-eq [\1] [\2])))
  (is (not (tree-eq [] [[]])))
  (is (not (tree-eq [\1 []] [\1])))
  (is (not (tree-eq [] nil))))

(defmacro is-match [expected testval]
  `(is (tree-eq ~expected (match-rules local-parser ~testval))))

(defmacro is-action [expected testval]
  `(is= ~expected (take-action (match local-parser ~testval))))

