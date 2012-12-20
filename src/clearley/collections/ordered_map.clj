(ns clearley.collections.ordered-map
  (:refer-clojure :exclude [get assoc empty keys])
  (require [clojure.core :as core])
  (require [clearley.collections.ordered-set])
  (use clearley.utils))

; See comments for clearley.collections.ordered-set.

(defprotocol IOrderedMap
  (get [self k])
  (keys [self])
  (assoc [self k v]))

; The keys in this map can only be added to, never removed.
(deftype AOrderedMap [i->k k->v]
  IOrderedMap
  (get [self k]
    (core/get k->v k))
  (keys [self]
    (clearley.collections.ordered_set.AOrderedSet. i->k (core/keys k->v)))
  (assoc [self k v]
    (AOrderedMap.
      (if (contains? k->v k) i->k (conj i->k k))
      (core/assoc k->v k v))))

(def empty (AOrderedMap. [] {}))
