(ns clearley.examples.json
  (require clearley.core)
  (use clearley.match clojure.math.numeric-tower clearley.lib))

(set! *unchecked-math* true)

; JSON spec:
; https://www.ietf.org/rfc/rfc4627.txt?number=4627

; Start with whitepsace. JSON has fewer whitespace chars than Java.
; At the end, we will define a JSON value as 'value, surrounded by whitespace.
(defstar whitespace '(:or \space \tab \newline \return)
  (fn ([] nil) ([_] nil) ([_ _] nil)))

; JSON recognizes 7 types of value. Three are represented by keywords.
(defmatch json-keyword ("true" true) ("false" false) ("null" nil))

; Fourth is the string.
; This is a valid JSON string character. It can be any non-escaped character,
; except \, ", and for some reason, ASCII control characters below u001f.
(def char-scanner
  (scanner (fn [^long c] (and (> c 0x1f)
                              (not (= (long \\) c)) (not (= (long \") c))))))

; Matches a unicode literal: \u007f for example
(defmatch hex [\\ \u
              (r (rule "unicode-hex" [hex-digit hex-digit hex-digit hex-digit]
                     ; hex-char returns an int... we turn that into Unicode char
                       (fn [^long a ^long b ^long c ^long d]
                         (+ (* 16 (+ (* 16 (+ (* 16 a) b)) c)) d))))]
                     ;(fn [& chars] (char (reduce (num-reducer 16) chars)))))]
  r)

; A string character can be an escaped char, a hex char, or anything else.
(defmatch string-char
  ; TODO the below is ugly
  ([\\ (escaped-char `(:or ~(long \") ~(long \\) ~(long \/) ~(long \b) ~(long \f)
                           ~(long \n) ~(long \r) ~(long \t)))] escaped-char)
  hex char-scanner)

(defn string-builder
  ([^StringBuilder s ^long c1] (doto s
                                 (.append (char c1))))
  ([^StringBuilder s ^long c1 ^long c2] (doto s
                                          (.append (char c1))
                                          (.append (char c2))))
  ([^StringBuilder s ^long c1 ^long c2 ^long c3] (doto s
                                                   (.append (char c1))
                                                   (.append (char c2))
                                                   (.append (char c3)))))

(def-unrolled-star string-body 'string-char
  (fn ([] "")
    ([& cs] (apply string-builder (java.lang.StringBuilder.) cs)))
  string-builder)

(def string (quotes string-body (fn [^Object s] (.toString s))))

; Fifth, the Number, the most complex. JSON accepts canonical integers,
; decimals, and scientific notation.
(defmatch decimal
  canonical-natnum
  ([canonical-natnum \. digits-ar]
   (+ canonical-natnum (/ (reduce (num-reducer) digits-ar)
                          (expt 10 (count digits-ar))))))

; A mantiss uses 'natnum' because it may start with an arbitrary number of 0s.
; Which is odd if you think about it.
(defmatch mantissa
  ([\+ natnum] natnum)
  natnum
  ([\- natnum] (- natnum)))

; The exactness of JSON numbers is undefined (it's just a data exchange after all).
; We'll assume 100% exactness.
(defmatch posnum
  decimal
  ([decimal '(:or \e \E) mantissa] (* decimal (expt 10 mantissa))))

(defmatch number
  posnum
  ([\- posnum] (- posnum)))

; The sixth type is the array.
(defdelimit array-values 'value \, (fn ([x] (transient [x]))
                                    ([arr y] (conj! arr y))))

(def array (brackets (match ([whitespace] []) ([array-values]
                                               (persistent! array-values)))))

; and the seventh (and also the goal type): Object.
(defmatch pair [whitespace string whitespace \: value] (transient {string value}))
(defmatch pairs
  ([pairs \, whitespace string whitespace \: value]
                 (assoc! pairs string value))
  pair)
;(defmatch pair [whitespace string whitespace \: value] [string value])

#_(defdelimit pairs 'pair \, (fn ([m] (transient (apply hash-map m)))
                             ([m p] (conj! m p))))

(def object (braces (match ([whitespace] {}) ([pairs] (persistent! pairs)))))

; Put it all together...
(def value (surround `whitespace `(:or json-keyword string number array object)))
(def whitespace-object (surround `whitespace object))

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
  (is-action {"a" 1} "{\"a\" : 1}")
  (is-action {} "{}")
  (is-action {} " { } ")
  (is-parsing "{\"a\" : 1, \"a\" : 2}")
  (not-parsing "{a : 1}")
  (not-parsing "{\"a\" : 1 \"b\" : 2}")
  (is-action {"a" 1 "b" 2} "{\"a\" : 1, \"b\" : 2}")
  (is-action {"a" 1 "b" true "c" "3"} "{\"a\" : 1, \"b\" : true, \"c\" : \"3\"}")
  (is (compare-to-file json-parser "clearley/examples/json_test.json"
                   "clearley/examples/json_test.edn")))
