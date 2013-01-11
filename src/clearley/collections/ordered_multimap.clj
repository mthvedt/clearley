(ns clearley.collections.ordered-multimap
  (:refer-clojure :exclude [get assoc empty])
  (require [clojure.core :as core])
  (require [clearley.collections.ordered-set :as os])
  (use clearley.utils))
; TODO rename this to something that isn't collections?

; don't use a defrecord, just use a map
(def empty {})

(defn get [mm k] (core/get mm k os/empty))

(defn assoc [mm k v]
  (core/assoc mm k (os/conj (get mm k) v)))

(defn get-vec [mm k]
  (os/vec (get mm k)))
