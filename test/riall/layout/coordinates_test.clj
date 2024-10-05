(ns riall.layout.coordinates-test
  (:require [riall.layout.coordinates :refer :all]
            [clojure.test :refer [deftest testing is are]]))

(deftest test-build-layer-solver
  (testing "Uniform test cases"
    (let [tt (build-layer-solver (constantly 1) ;; edge weights
                                 {1 [11 12],
                                  2 [22]}
                                 (constantly 1) ;; node heights
                                 20)]
      (testing "Point has one parent, inherits its coordinate"
        (is (= (do {22 8, 2 8})
               (tt {22 8} [2]))))
      (testing "Point is placed exactly between parent coordinates"
        (is (= (do {11 10, 12 12, 1 11})
               (tt {11 10, 12 12} [1]))))))
  (testing "Mixed test cases"
    (let [t (build-layer-solver (constantly 1)
                                {1 [11 12] 2 [21 22] 3 [33] 4 []}
                                {1 2, 2 2, 3 3, 4 1,
                                 11 1, 12 1, 21 1, 22 1, 33 1} ;; heights
                                20)]
      (testing "node has height=1, no parents, no initial position. can be placed freely."
        (is (= {4 0} (t {} [4]))))
      (testing "Point is placed exactly between parents but it is wide (w=2)"
        (is (= {11 10, 12 12, 1 10} (t {11 10, 12 12} [1]))))
      (testing "Point '3' cannot go between 11 and 12 so it stays on the edge"
        #_(is (= {11 7, 12 9, 33 8, 1 7, 3 9} (t {11 7, 12 9, 33 8} [1 3])))))))

