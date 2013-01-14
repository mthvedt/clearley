(ns clearley.examples.json
  (use clearley.core
       clojure.math.numeric-tower))

; JSON spec:
; https://www.ietf.org/rfc/rfc4627.txt?number=4627

; JSON recognizes 7 types of value. Three are represented by keywords.
(defrule true-token "true" true)
(defrule false-token "false" false)
(defrule null-token "null" nil)

; Fourth is the string.

; Clojure doesn't support character arithmetic so we need a manual implementation.
(defn char-to-num [char-start num-start]
  (let [char-start-int (int char-start)]
    (fn [c] (+ num-start (- (int c) char-start-int)))))

(def digit (char-range \0 \9 (char-to-num \0 0)))
(def hex-char [digit
               (char-range \a \f (char-to-num \a 10))
               (char-range \A \F (char-to-num \A 10))])

(def string-char-scanner
  (scanner (fn [c] (and (char? c) (not (= \\ c)) (not (= \" c))))))

(defrule string-char
  ; an escaped char
  ([\\ (escaped-char [\" \\ \/ \b \f \n \r \t])] escaped-char)
  ; a unicode char, using a rule literal
  ([\\ \u (hex (rule 'unicode-hex [hex-char hex-char hex-char hex-char]
                     ; hex-char returns an int... we turn that into Unicode char
                     (fn [& chars] (char (reduce (fn [a b] (+ (* 16 a) b)) chars)))))]
   hex)
  ([string-char-scanner] string-char-scanner))

(def string-body (one-or-more string-char))

(defrule string
  ([\" \"] "")
  ([\" string-body \"] (java.lang.String. (char-array string-body))))

; Fifth, the Number, the most complex.

(def digit1-9 (char-range \1 \9 (char-to-num \1 1)))
(def digits (one-or-more digit))
(defn digits-to-number [digits]
  (reduce (fn [a b] (+ (* 10 a) b)) 0 digits))

(defrule natnum
  ([\0] 0)
  ([digit1-9] digit1-9)
  ([digit1-9 digits] (digits-to-number (cons digit1-9 digits))))

(defrule fraction
  ([natnum] natnum)
  ([natnum \. digits] (+ natnum (/ (digits-to-number digits)
                                     (expt 10 (count digits))))))

(defrule mantissa
  ([\+ digits] (digits-to-number digits))
  ([digits] (digits-to-number digits))
  ([\- digits] (- (digits-to-number digits))))

; It appears JSON numbers are exact although JS numbers are double floats.
(defrule posnum
  ([fraction] fraction)
  ([fraction [\e \E] mantissa] (* fraction (expt 10 mantissa))))

(defrule number
  ([posnum] posnum)
  ([\- posnum] (- posnum)))

; The sixth type is the array.

; First we need to define some structural tokens. These can contain whitespace.
; JSON recognizes fewer whitespace chars than Java. These are they.
(def whitespace-char [\u0020 \u0009 \u000A \u000D])

(def whitespace (one-or-more "whitespace" whitespace-char))

; Returns a seq of rules representing the given token
; surrounded by any amount of insignificant whitespace
(defn whitespaced-rule [clause]
  [(rule nil [clause] identity)
   (rule nil [whitespace clause] (fn [_ x] x))
   (rule nil [clause whitespace] (fn [x _] x))
   (rule nil [whitespace clause whitespace] (fn [_ x _] x))])

(def array-begin (whitespaced-rule \[))
(def array-end (whitespaced-rule \]))
(def comma (whitespaced-rule \,))

(defrule array-values
  ([value] [value])
  ([array-values comma value] (conj array-values value)))

(defrule array [array-begin array-values array-end] array-values)

; and the seventh (and also the goal type): Object.

(def object-begin (whitespaced-rule \{))
(def object-end (whitespaced-rule \}))
(def colon (whitespaced-rule \:))

(defrule object-value [string colon value] [(keyword string) value])
(defrule object-values
  ([object-value] (let [[k v] object-value] {k v}))
  ([object-values comma object-value] (let [[k v] object-value
                                            o object-values]
                                        (if (contains? o k)
                                          (throw (RuntimeException.
                                                   (str "Duplicate key: " o)))
                                          (assoc o k v)))))

(defrule object [object-begin object-values object-end] object-values)

; Put it all together...
(def value [true-token false-token null-token
            string number array object])

; And we are done
(def json-parser (build-parser object))

; Let's prove that it works
(use 'clearley.test.utils 'lazytest.deftest)

(def json-value-parser (build-parser value))

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
  (is-action [1] " [ 1 ] ")
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
  (is-parsing "{\"a\" : 1, \"a\" : 2}")
  (action-throws RuntimeException "{\"a\" : 1, \"a\" : 2}")
  (not-parsing "{a : 1}")
  (not-parsing "{\"a\" : 1 \"b\" : 2}")
  (is-action {:a 1 :b 2} "{\"a\" : 1, \"b\" : 2}")
  (is-action {:a 1 :b true :c "3"} "{\"a\" : 1, \"b\" : true, \"c\" : \"3\"}"))
