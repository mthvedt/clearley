(ns clearley.suite.nondeterministic
  (use clearley.match clearley.test.utils uncore.test.utils))
; Nondeterministic parser tests

; TODO what here?
#_(defmatch S
  ([(s1 S) (s2 S)] (str s1 s2))
  ([(s1 S) (s2 S) (s3 S)] (str s1 s2 s3)))

; Test from 'Practical Earley Parsing'
; http://courses.engr.illinois.edu/cs421/sp2012/project/PracticalEarleyParsing.pdf

(defbind S [a1 A a2 A a3 A a4 A] (str a1 a2 a3 a4))
(def A `(:or \a (:seq)))
#_(defptest practical-earley-parsing S
  (is-action "" "")
  (is-action "a" "a")
  (is-action "aa" "aa")
  (is-action "aaa" "aaa")
  (is-action "aaaa" "aaaa"))

; hidden left recursion test
; TODO make a version that works for the deterministic parser
; TODO some "isn't" tests
(defmatch alpha
  ([beta alpha mu] (str beta alpha mu))
  ("w" "w"))
(defmatch mu "y" "y")
(def beta
  `(:or \x (:seq)))
#_(defptest hidden-left-recursion alpha 
  (is-action "xxwyy" "xxwyy")
  (is-action "wyy" "wyy"))
