(ns clearley.collections.ordered-multimap
  (:refer-clojure :exclude [get assoc])
  (require [clojure.core :as core])
  (require [clearley.collections.ordered-set :as os])
  (use clearley.utils))

; See comments for clearley.collections.ordered-set.

(defprotocol IOrderedMultimap
  (get [self k])
  (assoc [self k v]))

; A multimap with ordered values that can only be added to, not removed from.
(deftype AOrderedMultimap [k->v]
  IOrderedMultimap
  (get [self k]
    (core/get k->v k os/empty-ordered-set))
  (assoc [self k v]
    (AOrderedMultimap. (core/assoc k->v k (os/conj (get self k) v)))))

(def empty-ordered-multimap (AOrderedMultimap. {}))

(defn get-vec [mm k]
  (os/vec (get mm k)))
