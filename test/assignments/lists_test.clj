(ns assignments.lists-test
  (:require [clojure.test :refer :all]
            [assignments.lists :refer :all]))

(deftest map'-test
  (testing "identity with single coll"
    (is (= [1 2 3] (map' identity [1 2 3]))))
  (testing "addition with two collections"
    (is (= [2 4 6] (map' + [1 2 3] [1 2 3]))))
  )

(deftest filter'-test
  (testing "filter even numbers"
    (is (= [2 4] (filter' even? [1 2 3 4]))))
  (testing "filter not nil values"
    (is (= [1 2 4] (filter' (comp not nil?) [1 nil 2 nil nil 4]))))
  )

(deftest reduce'-test
  (testing "without initial value"
    (is (= 10 (reduce' + [1 2 3 4]))))
  (testing "with initial value"
    (is (= 4 (reduce - 10 [1 2 3]))))
  )

(deftest count'-test
  (testing "with non-empty collection"
    (is (= 4 (count' [1 2 3 4]))))
  (testing "with empty collection"
    (is (= 0 (count' []))))
  (testing "with nested collection"
    (is (= 5 (count' [[1] [1 2] [] [1 2 3 4] [5]]))))
  )

(deftest reverse'-test
  (testing "with non-empty collection"
    (is (= [4 3 2 1] (reverse' [1 2 3 4]))))
  (testing "with empty collection"
    (is (= [] (reverse' []))))
  (testing "with nested collection"
    (is (= [[5] [1 2 3 4] [] [1 2] [1]] (reverse' [[1] [1 2] [] [1 2 3 4] [5]]))))
  )

(deftest every?'-test
  (testing "with non-empty collection"
    (is (= false (every?' even? [1 2 3 4]))))
  (testing "with empty collection"
    (is (= true (every?' even? []))))
  (testing "with nested collection"
    (is (= false (every?' #(not (empty? %)) [[1] [1 2] [] [1 2 3 4] [5]]))))
  )

(deftest some?'-test
  (testing "with non-empty collection"
    (is (= true (some?' even? [1 2 3 4]))))
  (testing "with empty collection"
    (is (= false (some?' even? []))))
  (testing "with nested collection"
    (is (= false (some?' empty? [[1] [1 2] [1 2 3 4] [5]]))))
  )

(deftest ascending?-test
  (testing "with non-empty collection in ascending order"
    (is (= true (ascending? [1 2 3 4]))))
  (testing "with non-empty collection in descending order"
    (is (= false (ascending? [1 2 6 4]))))
  (testing "with empty collection"
    (is (= true (ascending? []))))
  )
