(ns clearley.utils)
; Some utils used by Clearley.

(defn separate-str [theseq separator]
  (apply str (drop 1 (interleave (repeat separator) theseq))))
