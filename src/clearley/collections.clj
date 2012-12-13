(ns clearley.collections
  (use clearley.utils))

; A poorly written library containing some collections
; that the earley parser needs.

; An ordered set that can only be added to, not disj'd from.
(defprotocol IOrderedSet
  (conj-os [self k])
  (vec-os [self])
  (contains-os? [self k]))

(defrecord AOrderedSet [i->k ks]
  IOrderedSet
  (conj-os [self k]
    (if (contains? ks k)
      self
      (AOrderedSet. (conj i->k k) (conj ks k))))
  (vec-os [self] i->k)
  (contains-os? [self k] (contains? ks k)))

(def empty-ordered-set (AOrderedSet. [] #{}))

(defprotocol IOrderedMultimap
  (assoc-omm [self k v])
  (get-omm [self k]))

; A multimap with ordered values that can only be added to, not removed from.
(defrecord AOrderedMultimap [k->v]
  IOrderedMultimap
  (get-omm [self k]
    (get k->v k empty-ordered-set))
  (assoc-omm [self k v]
    (AOrderedMultimap. (assoc k->v k (conj-os (get-omm self k) v)))))

(def empty-ordered-multimap (AOrderedMultimap. {}))

(defn get-vec-omm [omm k]
  (vec-os (get-omm omm k)))
