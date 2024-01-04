(ns riall.layout.coordinates-test
  (:require [riall.layout.coordinates :refer :all]
            [clojure.test :refer [deftest testing is are]]))

(deftest test-build-layer-solver
  (let [t (build-layer-solver {1 [11 12] 2 [21 22] 3 [33] 4 []}
                              {1 2, 2 2, 3 3, 4 1,
                               11 1, 12 1, 21 1, 22 1, 33 1} ;; heights
                              20)]
    (is (= {4 18} (t {} [4]))) ;; why 18?
    (testing "Point is placed exactly between parents"
      (is (= {11 10, 12 12, 1 11} (t {11 10, 12 12} [1]))))
    (testing "Point '3' cannot go between 11 and 12 so it stays on the edge"
      #_(is (= {11 7, 12 9, 33 8, 1 7, 3 9} (t {11 7, 12 9, 33 8} [1 3]))))))

