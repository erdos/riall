(ns riall.common-test
  (:require [riall.common :refer :all]
            [clojure.test :refer [deftest testing is are]]))

(deftest testing-index-of
  (testing "Empty input"
    (is (thrown? NullPointerException (index-of nil nil)))
    (is (thrown? NullPointerException (index-of nil 1)))
    (is (= nil (index-of [] 1)))
    (is (= nil (index-of [] nil))))
  (is (= nil (index-of [1] 0)))
  (is (= 2 (index-of [:a :b :c :d] :c)))
  (is (= 2 (index-of [:a :b nil :d] nil))))

(deftest testing-median
  (is (= 4 (median [4])))
  (is (= 3 (median [-1 2 3 4 10])))
  (is (= 3.5 (median [0 3 4 12]))))

(deftest testing-mean
  (is (= 4.0 (mean [1 4 7])))
  (is (= 2.0 (mean [2])))
  (is (thrown? clojure.lang.ExceptionInfo (mean [])))
  (is (thrown? clojure.lang.ExceptionInfo (mean nil))))