(ns clearley.collections.ordered-map
  (:refer-clojure :exclude [get assoc empty keys vals into])
  (require [clojure.core :as core])
  (require [clearley.collections.ordered-set])
  (use clearley.utils))

; See comments for clearley.collections.ordered-set.

(defprotocol IOrderedMap
  (get [self k])
  (get-key [self i])
  (keys [self])
  (vals [self])
  (assoc [self k v]))

; The keys in this map can only be added to or altered, never removed.
; i->k: a vector, k->v: a key-value map.
(deftype AOrderedMap [i->k k->v]
  IOrderedMap
  (get [self k]
    (core/get k->v k))
  (get-key [self i]
    (core/get i->k i))
  (keys [self]
    (clearley.collections.ordered_set.AOrderedSet. i->k (core/keys k->v)))
  (vals [self]
    (map #(core/get k->v %) i->k))
  (assoc [self k v]
    (AOrderedMap.
      (if (contains? k->v k) i->k (conj i->k k))
      (core/assoc k->v k v))))

(defn get-index [ordered-map i]
  (get ordered-map (get-key ordered-map i)))

(def empty (AOrderedMap. [] {}))
