(ns clearley.examples.json
  (use clearley.core
       lazytest.deftest
       clearley.test.utils
       clojure.math.numeric-tower))

; JSON spec:
; https://www.ietf.org/rfc/rfc4627.txt?number=4627

; JSON recognizes 7 types of value. Three are represented by keywords.
(defrule true-token "true" true)
(defrule false-token "false" false)
(defrule null-token "null" nil)

; Fourth is the string.

; Unfortunately, Clojure doesn't like adding and subtracting from Chars...
; we need to do it ourselves.
(defn char-to-num [char-start num-start]
  (let [char-start-int (int char-start)]
    (fn [c] (+ num-start (- (int c) char-start-int)))))

; TODO: should def and defrule be interchangable for the single rule case?
(def digit (token-range \0 \9 (char-to-num \0 0)))
(def hex-char [digit
               (token-range \a \f (char-to-num \a 10))
               (token-range \A \F (char-to-num \A 10))])

(def string-char-scanner
  (scanner (fn [c] (and (char? c)
                        (not (= \\ c))
                        (not (= \" c)))) identity))

(defrule string-char
  ; an escaped char
  ([\\ (escaped-char (map token [\" \\ \/ \b \f \n \r \t]))] escaped-char)
  ; a unicode char, using a rule literal
  ([\\ \u (hex (rule 'unicode-hex [hex-char hex-char hex-char hex-char]
                     ; hex-char returns an int... we turn that into Unicode char
                     (fn [& chars] (char (reduce (fn [a b] (+ (* 16 a) b)) chars)))))])
  ([string-char-scanner] string-char-scanner))

(def string-body (one-or-more "char+" string-char
                              (fn [& chars] (java.lang.String
                                              (char-array string-body)))))

(defrule string
  ([\" \"] "")
  ([\" string-body \"] string-body))

; Fifth, the Number, the most complicated one...

(def digit1-9 (token-range \1 \9 (char-to-num \1 1)))
(def digits (one-or-more digit))
(defn digits-to-number [digits]
  (reduce (fn [a b] (+ (* 10 a) b)) 0 digits))

(defrule integer
  ([\- number] (- number))
  ([\0] 0)
  ([digit1-9] digit1-9)
  ([digit1-9 digits] (digits-to-number (cons digit1-9 digits))))

(defrule fraction
  ([integer] integer)
  ([integer \. digits] (+ integer (/ (digits-to-number digits)
                                     (expt 10 (count digits))))))

(defrule mantissa
  ([\+ digits] digits)
  ([digits] digits)
  ([\- digits] (- digits)))

(defrule number
  ([fraction] fraction)
  ([fraction [\e \E] mantissa] (expt fraction mantissa)))

; The sixth type is the array...

; First we need to define some structural tokens. These can contain whitespace.
; JSON recognizes fewer whitespace chars than Java. These are it.
(def whitespace-char
  (map token [\u0020 \u0009 \u000A \u000D]))

(defrule whitespace
  ([whitespace-char] nil)
  ([whitespace-char whitespace] nil))

; Returns a seq of rules representing the given token
; surrounded by any amount of insignificant whitespace
(defn whitespaced-rule [clause]
  [(rule [clause])
   (rule [whitespace clause])
   (rule [clause whitespace])
   (rule [clause whitespace clause])])

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

(defrule object-value [string colon value] (list [string value]))
; TODO: what about object collision?
(defrule object-values
  ([object-value] object-value)
  ([object-values comma object-value] (cons object-value object-values)))

(defrule object [object-begin object-values object-end]
  (apply hash-map object-values))

; Put it all together...
; TODO: support the below syntax instead:
#_(def value [true-token false-token null-token
            string number array object])

(defrule value
  ([true-token] true-token)
  ([false-token] false-token)
  ([null-token] null-token)
  ([string] string)
  ([number] number)
  ([array] array)
  ([object] object))

; And we are done
(def json-parser (build-parser object))

; Let's prove that it works

; First, test values
(def json-value-parser (build-parser value))

(def-parser-test json-value-test json-value-parser
  (is-action 1 "1")
  (is-action 0 "0")
  (is-action 10 "10")
  (is-action 12345 "12345")
  (not-parsing "01"))

#_(deftest json-test
  (with-parser json-parser
    (is-parse 1 "1")))
