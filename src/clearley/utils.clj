(ns clearley.utils)
; Some utils used by Clearley.

(defmacro thrownew [extype & strs]
  `(throw (new ~extype (str ~@strs))))

(defmacro TIAE [& strs]
  `(thrownew IllegalArgumentException ~@strs))

(defmacro TRE [& strs]
  `(thrownew RuntimeException ~@strs))

(defn separate-str [theseq separator]
  (apply str (drop 1 (interleave (repeat separator) theseq))))

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
