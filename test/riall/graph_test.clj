(ns riall.graph-test
  (:require [clojure.test :refer [deftest testing is are]]
            [riall.graph :refer :all]))

(deftest test-flood
  (is (= #{3} (flood {} 3)))
  (is (= #{0 1 2 11 12 21 22} (flood {0 [1 2] 1 [11 12] 2 [21 22] 3 [4]} 0))))

(defn- edge [a b] {:edge/source a :edge/target b})

(deftest test-edges->components
  (is (= [] (edges->components [])))
  (is (= [[(edge 'X 'Y)] [(edge 'A 'B) (edge 'A 'C)]]
         (edges->components [(edge 'A 'B) (edge 'A 'C) (edge 'X 'Y)]))))

(deftest test-remove-cycles
  (is (= '[{:edge/back? false, :edge/source A, :edge/target B}
           {:edge/back? false, :edge/source B, :edge/target C}
           {:edge/back? true, :edge/source C, :edge/target A}]
         (remove-cycles [(edge 'A 'B) (edge 'B 'C) (edge 'C 'A)])))
  (is (= '[{:edge/back? false, :edge/source A, :edge/target B}
           {:edge/back? false, :edge/source B, :edge/target C}
           {:edge/back? false, :edge/source D, :edge/target B}]
         (remove-cycles [(edge 'A 'B) (edge 'B 'C) (edge 'D 'B)]))))

(deftest test-layer-assignment
  (is (= [] (layer-assignment [])))
  (is (= [[1] [2]] (layer-assignment [(edge 1 2)])))
  (is (= [[1 3] [4 2]] (layer-assignment [(edge 1 2) (edge 3 4)])))
  (is (= [[1] [2] [3]] (layer-assignment [(edge 1 2) (edge 2 3)])))
  (is (= [[1 3] [2]] (layer-assignment [(edge 1 2) (edge 3 2)]))))

(deftest test-add-hidden-edges
  (is (= [[] []] (add-hidden-edges [] [])))
  (is (= [[[1 2] [3 4]] []] (add-hidden-edges [[1 2] [3 4]] [])))
  (let [[nodes edges] (add-hidden-edges [[1] [] [2]] [(edge 1 2)])]
    ;; FIXME: properly cover with tests
    (is (= 3 (count nodes)))
    (is (= 2 (count edges)))))

(deftest test-iterate-evenodd
  (is (= [0 1 2 3 6 7] (take 6 (iterate-evenodd inc #(* 2 %) 0)))))

(deftest test-wmedian-mapfn
  (is (= [5] (wmedian-mapfn {5 [1 2 3 4]} [1 2 3 4] [5]))) ;; not moved
  (is (= [3 2 1] (wmedian-mapfn {1 [11] 2 [22] 3 [33]} [33 22 11] [1 2 3])))
  (is (= [5 4 3] (wmedian-mapfn {5 [] 4 [] 3 []} [1] [5 4 3]))))

(deftest test-wmedian
  (is (= '([1 2] (5 4 3)) (wmedian {5 [1 2] 4 [1 2] 3 [1 2]} '([1 2] (5 4 3)))))
  (is (= '([1 2] (3 4 5)) (wmedian {5 [1 2]} '([1 2] (3 4 5))))))

(deftest test-layer-crossings
  (testing "failure cases"
    (is (= 0 (layer-crossings {} [] [])))
    (is (= 0 (layer-crossings {} [1 2] [3 4]))))
  (testing "Untangled"
    (is (= 0 (layer-crossings {1 [11] 2 [22] 3 [33]} [1 2 3] [11 22 33])))
    (is (= 0 (layer-crossings {1 [11 111] 2 [22 222] 3 [33 333]} [1 2 3] [11 111 22 222 33 333])))
    #_ (is (= 0 (layer-crossings {1 [11 111] 2 [22 222]} [1 2] [11 22]))))
  ;; above are the trivial cases. now...
  (testing "full graph"
    (is (= 1 (layer-crossings {1 [3 4] 2 [3 4]} [1 2] [3 4])))
    #_(is (= 9 (layer-crossings {1 [4 5 6] 2 [5 6 4] 3 [6 5 4]} [1 2 3] [4 5 6]))))
  (testing "double edges"
    (is (= 4 (layer-crossings {1 [5 6] 2 [4 4]} [1 2 3] [4 5 6])))))


(deftest test-vertex-ordering
  (testing "Trivial case does not change"
    (is (= [[1 2] [3 4]] (vertex-ordering [[1 2] [3 4]] [(edge 1 3) (edge 2 4)]))))
  (testing "untangle 1"
    (is (= [[1 2] [4 3]] (vertex-ordering [[1 2] [3 4]] [(edge 1 4) (edge 2 3)]))))
  (testing "Impossible to untangle"
      (is (= [[1 2] [3 4]] (vertex-ordering [[1 2] [3 4]] [(edge 1 4) (edge 1 3) (edge 2 3) (edge 2 4)])))))

