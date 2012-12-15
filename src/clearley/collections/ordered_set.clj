(ns clearley.collections.ordered-set
  (:refer-clojure :exclude [conj vec contains? into])
  (require [clojure.core :as core])
  (use clearley.utils))

; A poorly written library containing some collections
; that the earley parser needs.

; I'm not sure about the standard protocols in Clojurescript so
; rolling my own protocols for now. stuff like conj is different
; for ordered sets anyway.

; An ordered set that can only be added to, not disj'd from.
(defprotocol IOrderedSet
  (conj [self k])
  (vec [self])
  (contains? [self k]))

(deftype AOrderedSet [i->k ks]
  IOrderedSet
  (conj [self k]
    (if (core/contains? ks k)
      self
      (AOrderedSet. (core/conj i->k k) (core/conj ks k))))
  (vec [self] i->k)
  (contains? [self k] (core/contains? ks k)))

(def empty-ordered-set (AOrderedSet. [] #{}))

(defn into [os coll]
  (reduce conj os coll))
