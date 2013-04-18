(ns clearley.examples.json
  (require clearley.core)
  (use clearley.defrule clojure.math.numeric-tower))

; JSON spec:
; https://www.ietf.org/rfc/rfc4627.txt?number=4627

; Start with whitepsace. JSON's whitespace is different from Java.
; At the end, we will define a JSON value as 'value, surrounded by whitespace.
(def whitespace-char '(:or \u0020 \u0009 \u000A \u000D))
(def whitespace '(:star whitespace-char))

; JSON recognizes 7 types of value. Three are represented by keywords.
(defrule true-token "true" true)
(defrule false-token "false" false)
(defrule null-token "null" nil)

; Fourth is the string.
; First we need to parse digits and hex numbers.
; Clojure doesn't support character arithmetic so we need a manual implementation.
(defn char-to-digit [char-start num-start]
  (let [char-start-int (int char-start)]
    #(+ num-start (- (int %) char-start-int))))

; We need this for escaped characters. Notice they return a number not a char.
(def digit (char-range \0 \9 (char-to-digit \0 0)))
(def hex-char `(:or digit
                    ~(char-range \a \f (char-to-digit \a 10))
                    ~(char-range \A \F (char-to-digit \A 10))))

; Accepts any non-escaped character. Rejected are \\, \",
; and all JSON control characters (which are not the same as in Unicode!)
(def char-scanner
  (scanner (fn [c] (and (char? c) (> (int c) 0x1f)
                        (not (= \\ c)) (not (= \" c))))))

(defrule string-char
  ; an escaped char
  ([\\ (escaped-char '(:or \" \\ \/ \b \f \n \r \t))] escaped-char)
  ; a unicode char, using a rule literal
  ([\\ \u (hex (rule "unicode-hex" [hex-char hex-char hex-char hex-char]
                     ; hex-char returns an int... we turn that into Unicode char
                     (fn [& chars] (char (reduce (fn [a b] (+ (* 16 a) b)) chars)))))]
   hex)
  ([char-scanner] char-scanner))

(defrule string
  ([\" (string-body '(:star string-char)) \"]
   (java.lang.String. (char-array string-body))))

; Fifth, the Number, the most complex.
(def digit1-9 (char-range \1 \9 (char-to-digit \1 1)))
(def digits (plus 'digit))
(defn make-num [digits]
  (reduce #(+ (* 10 %) %2) 0 digits))

(defrule natnum
  ([\0] 0)
  ([digit1-9] digit1-9)
  ([digit1-9 digits] (make-num (cons digit1-9 digits))))

(defrule decimal
  ([natnum] natnum)
  ([natnum \. digits] (+ natnum (/ (make-num digits)
                                     (expt 10 (count digits))))))

(defrule mantissa
  ([(_ (opt \+)) digits] (make-num digits))
  ([\- digits] (- (make-num digits))))

; It appears JSON numbers are exact although JS numbers are double floats.
(defrule posnum
  ([decimal] decimal)
  ([decimal '(:or \e \E) mantissa] (* decimal (expt 10 mantissa))))

(defrule number
  ([posnum] posnum)
  ([\- posnum] (- posnum)))

; The sixth type is the array.
(defrule array-values
  ([value] [value])
  ([array-values \, value] (conj array-values value)))

(defrule array
  ([\[ whitespace \]] [])
  ([\[ array-values \]] array-values))

; and the seventh (and also the goal type): Object.
(defrule pair [whitespace string whitespace \: value]
  [(keyword string) value])

(defrule pairs
  ([pair] (apply hash-map pair))
  ([pairs \, pair] (conj pairs pair)))

(defrule object
  ([\{ whitespace \}] {})
  ([\{ pairs \}] pairs))
(defrule whitespace-object [whitespace object whitespace] object)

; Put it all together...
(def value* '(:or true-token false-token null-token
                 string number array object))
(defrule value [whitespace value* whitespace] value*)

; And we are done
(def json-parser (clearley.core/build-parser whitespace-object))

; Let's prove that it works
(use 'clearley.test.utils 'lazytest.deftest)

(def json-value-parser (clearley.core/build-parser value))

(def-parser-test json-literal-test json-value-parser
  (is-action true "true")
  (is-action false "false")
  (is-action nil "null")
  (not-parsing "TRUE"))

(def-parser-test json-number-test json-value-parser
  (is-action 1 "1")
  (is-action 0 "0")
  (is-action 10 "10")
  (is-action 12345 "12345")
  (is-action 0 "-0")
  (is-action -1 "-1")
  (is-action -12345 "-12345")
  (not-parsing "--1")
  (not-parsing "01")
  (is-action (/ 11 10) "1.1") ; Exact num, not float
  (is-action (/ 123456 1000) "123.456")
  (is-action (- (/ 5 100)) "-0.05")
  (is-action 1000 "1e3")
  (is-action 1100 "1.1e3")
  (is-action 1 "1e0")
  (is-action -1100 "-1.1e+3")
  (is-action 0 "0e2")
  (is-action (/ 1 1000) "1e-3")
  (is-action (- (/ 5 100)) "-0.5e-1")
  (not-parsing "1.1e1.1"))

(def-parser-test json-string-test json-value-parser
  (is-action "a" "\"a\"")
  (is-action "abc" "\"abc\"")
  (not-parsing "abc")
  (is-action "a" "\"\\u0061\"")
  (is-action "abc" "\"a\\u0062c\"")
  (is-action "\\" "\"\\\\\"")
  (is-action "\"escaped\\string\"" "\"\\\"escaped\\\\string\\\"\""))

(def-parser-test json-array-test json-value-parser
  (is-action [1] "[1]")
  (is-action [] "[]")
  (is-action [1] " [ 1 ] ")
  (is-action [] "  [   ]  ")
  (is-action [1 true] "[1,true]")
  (not-parsing "[1 true]")
  (is-action [1 true] " [ 1,true ]")
  (is-action [true "yo"] " [true, \"yo\"]")
  (is-action [1 true "yo"] " [1, true,\"yo\" ] ")
  (is-action [1 [2]] "[1,[2]]")
  (is-action [[1] [2]] "[[1],[2]]")
  (is-action [1 true "yo" [2 3]] "[1,true,\"yo\" ,[2,3]]")
  (is-action [1 true "yo" [2 3]] "  [1,true  ,\"yo\" , [ 2,3]] "))

; Only Objects are valid json parses.
(def-parser-test json-test json-parser
  (not-parsing "1")
  (not-parsing "true")
  (not-parsing "\"a\"")
  (not-parsing "[1]")
  (is-action {:a 1} "{\"a\" : 1}")
  (is-action {} "{}")
  (is-parsing "{\"a\" : 1, \"a\" : 2}")
  ;(action-throws RuntimeException "{\"a\" : 1, \"a\" : 2}")
  (not-parsing "{a : 1}")
  (not-parsing "{\"a\" : 1 \"b\" : 2}")
  (is-action {:a 1 :b 2} "{\"a\" : 1, \"b\" : 2}")
  (is-action {:a 1 :b true :c "3"} "{\"a\" : 1, \"b\" : true, \"c\" : \"3\"}"))
