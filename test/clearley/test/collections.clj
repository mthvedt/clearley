(ns clearley.test.collections
  (use clearley.test.utils lazytest.deftest clearley.collections))

(def os1 (reduce conj-os empty-ordered-set [:m :o :u :s :e]))

(deftest ordered-set-test
  (is= (vec-os os1) [:m :o :u :s :e])
  (is= (vec-os (reduce conj-os os1 [:m :i :c :k :e :y]))
       [:m :o :u :s :e :i :c :k :y])
  (is (contains-os? os1 :m))
  (isnt (contains-os? os1 :q)))

(defn add-values [m k vs]
  (reduce #(assoc-omm % k %2) m vs))

(def mm1
  (-> empty-ordered-multimap
    (add-values 1 [:c :a :r :l])
    (add-values 2 [:d :i :t :t :e :r :s])
    (add-values 3 [:v :o :n])
    (add-values 4 [:d :i :t :t :e :r :s :d :o :r :f])))

(deftest ordered-multimap-test
  (is= (get-vec-omm mm1 1) [:c :a :r :l])
  (is= (get-vec-omm mm1 2) [:d :i :t :e :r :s])
  (is= (get-vec-omm mm1 3) [:v :o :n])
  (is= (get-vec-omm mm1 4) [:d :i :t :e :r :s :o :f])
  (is= (get-vec-omm mm1 5) [])
  (let [mm2 (add-values mm1 4 (get-vec-omm mm1 1))]
    (is= (get-vec-omm mm2 4) [:d :i :t :e :r :s :o :f :c :a :l])))
