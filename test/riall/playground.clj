

(defn- permutations [xs]
  (if-not (next xs)
    (list xs)
    (for [head xs, tail (permutations (disj (set xs) head))]
      (cons head tail))))

;; checks if two edges intersect anywhere but the endpoints of both. returns either
;; nil on no intersection, :overlap on overlapping, :intersect on intersection
(defn check-intersect [p1x p1y, p2x p2y,, q1x q1y, q2x q2y]
  (letfn [(cross [a b c d] (- (* a d) (* b c)))]
    (let [rx (- p2x p1x) ry (- p2y p1y)
          sx (- q2x q1x) sy (- q2y q1y)
          numerator   (cross (- q1x p1x) (- q1y p1y) rx ry)
          denominator (cross rx ry sx sy)]
      (cond (= 0 numerator denominator) ;; colinear
            (when (and (or (= p1x q1x q2x p2x)
                           (< (min p1x p2x) q1x (max p1x p2x)) (< (min p1x p2x) q2x (max p1x p2x))
                           (< (min q1x q2x) p1x (max q1x q2x)) (< (min q1x q2x) p2x (max q1x q2x)))
                       (or (= p1y q1y q2y p2y)
                           (< (min p1y p2y) q1y (max p1y p2y)) (< (min p1y p2y) q2y (max p1y p2y))
                           (< (min q1y q2y) p1y (max q1y q2y)) (< (min q1y q2y) p2y (max q1y q2y))))
              :overlap)
            (not (zero? denominator)) ;; not parallel
            (let [u (/ numerator denominator)
                  t (/ (cross (- q1x p1x) (- q1y p1y) sx sy) denominator)]
              (when (and (< 0 t 1) (< 0 u 1))
                :intersect))))))


;; lazy seq of filling a priority DAG
(defn weighted-fill [get-neighbors start-set]
  (assert (sorted? start-set)) (assert (set? start-set))
  (when-let [[s] (seq start-set)]
    (let [neighbors (get-neighbors s)]
      (cons s (lazy-seq (weighted-fill get-neighbors (-> start-set (disj s) (into neighbors))))))))

(deftest test-weighted-fill
  (is (= [10 8 6 1 9 11]
         (weighted-fill {10 [9 8] 8 [6 11] 6 [1]} (sorted-set 10)))))

;; calls function with all 8 permutations of the arguments.
(defn- testing-check-intersect [expected a b c d e f g h]
  (letfn [(split2 [coll] ((juxt seq reverse) (split-at (/ (count coll) 2) coll)))]
    (doseq [[abcd efgh] (split2 [a b c d e f g h])
            [[a b] [c d]] (split2 abcd)
            [[e f] [g h]] (split2 efgh)]
      (is (= expected (check-intersect a b c d e f g h))))))

(deftest test-check-intersect
  (testing "Not intersecting"
    (testing "Colinear, not touching"
      (testing-check-intersect nil 0 0, 1 1,, 2 2, 3 3))
    (testing "Colinear, touching endpoints"
      (testing-check-intersect nil 0 0, 1 1,, 1 1, 2 2)
      (testing-check-intersect nil 0 0, 1 0,, 1 0, 2 0))
    (testing "Shared base point"
      (is (nil? (check-intersect 0 0, 0 1,, 0 0, 1 0)))))
  (testing "Overlapping"
    (testing-check-intersect :overlap 0 0 8 4,, 2 1, 4 2)
    (testing "Sharing an endpoint"
      (testing-check-intersect :overlap 0 0 2 0,, 0 0, 1 0)
      (testing-check-intersect :overlap 0 0, 2 0,, 1 0, 2 0)))
  (testing "Intersecting in the middle at a single point"
    (testing-check-intersect :intersect 0 0, 2 2,, 0 2, 2 0))
  (testing "Various other tests"
    (testing-check-intersect nil 1 1 2 1 0 1 1 1)
    (testing-check-intersect :overlap 1 0 2 0 1 0 3 0)
    (testing-check-intersect :intersect 2 1 0 0, 0 1 1 0)))
