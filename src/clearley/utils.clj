(ns clearley.utils)
; Some utils used by Clearley.

(defmacro thrownew [extype & strs]
  `(throw (new ~extype (str ~@strs))))

(defmacro TIAE [& strs]
  `(thrownew IllegalArgumentException ~@strs))

(defmacro TRE [& strs]
  `(thrownew RuntimeException ~@strs))

(defn cutoff
  ([thestr] (cutoff thestr 80))
  ([thestr val]
   (if (< val (count thestr))
     (str (subs thestr 0 val) "(...)")
     thestr)))

(defn separate-str
  [separator theseq] (apply str (drop 1 (interleave (repeat separator) theseq))))

(defn update [map key f] (update-in map [key] f))

(defn update-all [map keyvals]
  (reduce (fn [m [k f]] (update m k f)) map keyvals))

; Eagerly evaluated map
(defn domap [& args] (doall (apply map args)))

; Eagerly evaluated map, no memory leaks
(defn runmap [& args] (dorun (apply map args)))

(defmacro map-> [coll & forms]
  `(map
     (fn [x#] (-> x# ~@forms))
     ~coll))

(defmacro fn-> [& forms]
  `(fn [x#] (-> x# ~@forms)))

(defn hexhash [obj]
  (Integer/toHexString (hash obj)))

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
