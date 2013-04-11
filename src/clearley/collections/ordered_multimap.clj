(ns clearley.collections.ordered-multimap
  (:refer-clojure :exclude [get keys assoc empty])
  (require [clojure.core :as core])
  (require [clearley.collections.ordered-set :as os]))

; Stuff for manipulating ordered multimaps. It's just here
; so it gets its own namespace.

; ordered multimaps are just maps of key->ordered-set
(def empty {})

(defn get [mm k] (core/get mm k os/empty))

(defn keys [mm] (core/keys mm))

(defn assoc [mm k v]
  (core/assoc mm k (os/conj (get mm k) v)))

(defn get-vec [mm k]
  (os/vec (get mm k)))
