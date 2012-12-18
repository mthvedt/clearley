(ns clearley.test.collections
  (require [clearley.collections.ordered-set :as os]
           [clearley.collections.ordered-multimap :as omm])
  (use clearley.test.utils lazytest.deftest))

(def os1 (reduce os/conj os/empty [:m :o :u :s :e]))

(deftest ordered-set-test
  (is= (os/vec os1) [:m :o :u :s :e])
  (is= (os/vec (reduce os/conj os1 [:m :i :c :k :e :y]))
       [:m :o :u :s :e :i :c :k :y])
  (is (os/contains? os1 :m))
  (isnt (os/contains? os1 :q)))

(defn add-values [m k vs]
  (reduce #(omm/assoc % k %2) m vs))

(def mm1
  (-> omm/empty
    (add-values 1 [:c :a :r :l])
    (add-values 2 [:d :i :t :t :e :r :s])
    (add-values 3 [:v :o :n])
    (add-values 4 [:d :i :t :t :e :r :s :d :o :r :f])))

(deftest ordered-multimap-test
  (is= (omm/get-vec mm1 1) [:c :a :r :l])
  (is= (omm/get-vec mm1 2) [:d :i :t :e :r :s])
  (is= (omm/get-vec mm1 3) [:v :o :n])
  (is= (omm/get-vec mm1 4) [:d :i :t :e :r :s :o :f])
  (is= (omm/get-vec mm1 5) [])
  (let [mm2 (add-values mm1 4 (omm/get-vec mm1 1))]
    (is= (omm/get-vec mm2 4) [:d :i :t :e :r :s :o :f :c :a :l])))
