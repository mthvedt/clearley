(ns clearley.utils)
; Some utils used by Clearley.

(defmacro thrownew [extype & strs]
  `(throw (new ~extype (str ~@strs))))

(defmacro TIAE [& strs]
  `(thrownew IllegalArgumentException ~@strs))

(defmacro TRE [& strs]
  `(thrownew RuntimeException ~@strs))

(defn cutoff [thestr]
  (if (< 100 (count thestr))
    (str (subs thestr 0 100) "(...)")
    thestr))

(defn separate-str [theseq separator]
  (cutoff (apply str (drop 1 (interleave (repeat separator) theseq)))))

(def ^:dynamic *debug* true)

(defmacro debug
  ([statement]
   `(debug ~statement ~statement))
  ([statement & forms]
   (if *debug*
     `(do (println ~statement) ~@forms)
     `(do ~@forms))))

(defmacro idebug [& forms]
  `(debug ~forms))

(defmacro with-rethrow [ex-class form ex-str-form]
  `(try ~form
     (catch ~ex-class e#
       (throw (new ~ex-class ~ex-str-form e#)))))
