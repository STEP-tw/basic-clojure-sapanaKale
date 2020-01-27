(ns assignments.conditions-test
  (:require [clojure.test :refer :all]
            [assignments.conditions :refer :all]))

(deftest safe-division
  (testing "non zero denominator"
    (is (= 2 (safe-divide 4 2))))
  (testing "zero denominator"
    (is (nil? (safe-divide 3 0)))))

(deftest informative-division
  (testing "non zero denominator"
    (is (= 2 (informative-divide 4 2))))
  (testing "zero denominator"
    (is (= :infinite (informative-divide 3 0)))))

(deftest harishchandra-test
  (testing "truthy value"
    (is (= 2 (harishchandra 2))))
  (testing "nil value"
    (is (nil? (harishchandra nil))))
  (testing "falsy value"
    (is (nil? (harishchandra false)))))

(deftest yudishtira-test
  (testing "truthy value"
    (is (= 2 (yudishtira 2))))
  (testing "nil value"
    (is (= :ashwathama (yudishtira nil))))
  (testing "falsy value"
    (is (= :ashwathama (yudishtira false)))))

(deftest duplicate-first-test
  (testing "non-empty collection"
    (is (= [1 1 2 3] (duplicate-first [1 2 3]))))
  (testing "empty collection"
    (is (nil? (duplicate-first [])))))

(deftest five-point-someone-test
  (testing "y is 5"
    (is (= :chetan-bhagat (five-point-someone 2 5))))
  (testing "x is 5"
    (is (= :satan-bhagat (five-point-someone 5 3))))
  (testing "x is greater than y"
    (is (= :greece (five-point-someone 6 4))))
  (testing "y is greater than x"
    (is (= :universe (five-point-someone 3 4)))))

(deftest conditions-apply-test
  (testing "for collections containing 1 and 3"
    (is (= :wonder-woman (conditions-apply [0 1 0 3]))))
  (testing "for collection containing :a :b and :c"
    (is (= :durga (conditions-apply [:a :b :e :c]))))
  (testing "for collection containing [2 3] and [4 5]"
    (is (= :cleopatra (conditions-apply [[0 0] [2 3] [1 1] [4 5] [5 6]]))))
  (testing "for collection which does contain any of above"
    (is (= :tuntun (conditions-apply [7 8 9])))))

(deftest repeat-and-truncate-test
  (testing "when repeat and truncate both are true"
    (is (= [1 2 1] (repeat-and-truncate [1 2] true true 3))))
  (testing "when repeat and truncate both are false"
    (is (= [1 2] (repeat-and-truncate [1 2] false false 4))))
  (testing "when repeat is true and truncate is false"
    (is (= [1 2 1 2] (repeat-and-truncate [1 2] true false 3))))
  (testing "when repeat is false and truncate is true"
    (is (= [1] (repeat-and-truncate [1 2] false true 1)))))

(deftest order-in-words-test
  (testing "for x > y and y > z"
    (is (= [:x-greater-than-y :y-greater-than-z] (order-in-words 4 3 2))))
  (testing "for x > y and z > x"
    (is (= [:x-greater-than-y :z-greater-than-x] (order-in-words 4 3 5))))
  (testing "for z > x"
    (is (= [:z-greater-than-x] (order-in-words 2 3 4)))))

(deftest zero-aliases-test
  (testing "zero"
    (is (= :zero (zero-aliases 0))))
  (testing "empty-list"
    (is (= :empty (zero-aliases '()))))
  (testing "empty-vector"
    (is (= :empty (zero-aliases []))))
  (testing "empty-set"
    (is (= :empty-set (zero-aliases #{}))))
  (testing "empty-map"
    (is (= :empty-map (zero-aliases {}))))
  (testing "empty-string"
    (is (= :empty-string (zero-aliases ""))))
  (testing "not-zero"
    (is (= :not-zero (zero-aliases 6)))))


(deftest zero-separated-palindrome-test
  (testing "with non-empty collection"
    (is (= [3 2 0 2 3] (zero-separated-palindrome [1 2]))))
  (testing "with empty collection"
    (is (= [0] (zero-separated-palindrome [])))))
