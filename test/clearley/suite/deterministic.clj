(ns clearley.suite.deterministic
  (require [lazytest.suite :as suite])
  (use clearley.match [clearley.lib :exclude [digit]] clearley.test.utils
       uncore.test.utils lazytest.deftest))
; Deterministic parser tests

(defmatch sum
  ([sum \+ times] (+ sum times))
  times)
(defmatch times
  ([times \* digit] (* times digit))
  digit)
(defmatch digit [\3] 3)

; === Test the basics ===
(defptest parsing1 sum
  (testing "Basic parsing"
           (is-parsing "3+3")
           (not-parsing "4+4"))
  (testing "Basic actions"
           (is-action 6 "3+3")
           (is-action 9 "3*3")
           (is-action 15 "3+3*3+3")))

; A little more invovled
(defmatch digit ([\1] 1) ([\2] 2) ([\3] 3) ([\4] 4) ([\5 \5] 55))

(defptest multiple-choice sum
  (is-parsing "1+2")
  (is-parsing "1+2*3+4")
  (is-parsing "1*2+3*4")
  (is-parsing "1+55*3+2*55")
  (isnt (parses? "44"))
  (isnt (parses? "55*23"))
  (isnt (parses? "1+2a"))
  (is-parsing "1+55*2*55+3+55*4"))

; Rule aliasing
(defmatch sum
  ([sum \+ (t times)] (+ sum t))
  ([(t times)] t))

(defptest rule-aliasing sum
  (is-action 6 "3+3"))

; Rule literals
(def digits67 '(:or \6 (:seq \7 \7)))
(defmatch digits67* [digits67] 10) ; Living on the edge!
(defmatch digit ([\1] 1) ([\2] 2) ([\3] 3) ([\4] 4) ([\5 \5] 55)
  ([digits67*] digits67*))

(defptest rule-literals sum
  (is-action 15 "2+3+6")
  (is-action 15 "2+3+77"))

; Scanner test
(def digit (char-range \0 \9 #(- (int %) (int \0))))

(defptest scanners sum
  (is-action 6 "1+2+3"))

; star test
(defmatch times
  ([times \* natnum] (* times natnum))
  ([natnum] natnum))

(defptest plus-test sum
  (is-action 2 "1+1")
  (is-action 22 "11+11")
  (is-action 771 "1+22*33+44"))

; opt test
(defstar foo `(:seq \x ~(opt \y)) (fn ([] []) ([x] [x]) ([arr x] (conj arr x))))

; TODO figure this out. char tokens should return chars
; in string mode.
(defptest opt-test foo
  (is-action [[120 nil] [120 nil]] "xx")
  (is-action [[120 nil] [120 121]] "xxy")
  (not-parsing "xyy"))

; hidden left recursion test
; TODO make a version that works for the deterministic parser
(defmatch alpha
  ([beta alpha mu] (str beta alpha mu))
  ("w" "w"))
(defmatch mu "y" "y")
(def beta
  `(:or \x (:seq)))
#_(defptest hidden-left-recursion alpha 
  (is-action "xxwyy" "xxwyy")
  (is-action "wyy" "wyy"))
