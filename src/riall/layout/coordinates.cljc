(ns riall.layout.coordinates
  "Coordinate assignment step of the layout algorithm."
  (:require [clojure.math :as math]
            [riall.common :refer [mean +inf mean]]))

;; makes coordinate expressions easier to read
(defmacro ^:private idx-> [f a x y & v] `(~f ~a (+ ~x (* (mod ~y ~'max-height) (inc ~'max-x))) ~@v))

;; Calculates the score to minimize when node is placed on y
(defmulti node-score (fn [node->parents heights node->y node y] :midbox))

;; distance quare sum between top vertical positions of nodes
(defmethod node-score :top-sq [node->parents heights node->y node y]
  (reduce + (for [p (node->parents node)] (math/pow (- y (node->y p)) 2))))

;; distance square to midpoint of parents' convex hull
(defmethod node-score :midbox [node->parents heights node->y node y]
  (if (seq (node->parents node))
    (let [p-top (reduce min (for [p (node->parents node)] (node->y p)))
          p-btm (reduce max (for [p (node->parents node)] (+ (node->y p) (heights p))))]
      (math/pow (- (+ y (/ (heights node) 2) ) (/ (+ p-top p-btm) 2)) 2))
    0))

;; distance square sum between the midpoints of nodes
(defmethod node-score :sq [node->parents heights node->y node y]
  (reduce + (for [p (node->parents node)]
               (math/pow (- (+ y (/ (heights node) 2)) (+ (node->y p) (/ (heights p) 2))) 2))))

(defn build-layer-solver [node->parents heights max-y]
  (fn [node->y layer]
    (let [max-x      (count layer)
          max-height (transduce (map heights) max 1 layer)
          states     (object-array (* (inc max-x) max-height))
          costs      (double-array (* (inc max-x) max-height))
          score-at   (fn [x y] (node-score node->parents heights node->y (nth layer x) y))]
      (dotimes [x max-x]
        (idx-> aset costs x max-y +inf))
      (dotimes [y' max-y]
        (dotimes [x (min max-x (- max-y y'))] ;; 0 1 2 ... mx-1 <---- but order does not matter.
          (let [y          (- max-y y' 1)
                jump-y     (+ y (int (heights (nth layer x))))
                cost-place (if (< jump-y max-y)
                             (+ (score-at x y) (idx-> aget costs (inc x) jump-y))
                             +inf)
                cost-skip  (idx-> aget costs x (inc y))]
            (if (< cost-place cost-skip)
              (do (idx-> aset costs x y cost-place)
                  (idx-> aset states x y (cons y (idx-> aget states (inc x) jump-y))))
              (do (idx-> aset costs x y cost-skip)
                  (idx-> aset states x y (idx-> aget states x (inc y))))))))
      (->> (idx-> aget states 0 0) (map vector layer) (into node->y)))))


;; --------------

(comment
  (def ^:private layer-step-inf [Double/POSITIVE_INFINITY {}])
  (defmacro memo [f args body]
    `(let [m# (fn [f# ~@args] (let [~f (partial f# f#)] ~body))
          m# (memoize m#)]
      (partial m# m#)))
  (defn build-layer-solver-1 [node->parents heights state max-y]
    (memo f [layer y]
      (cond (>= y max-y)    layer-step-inf
            (empty? layer) [0 state]
            :else
            (let [[score state'] (f (next layer) (+ y (heights (first layer))))]
              (if (infinite? score)
                layer-step-inf
                (let [delta (node-score node->parents heights state (nth layer x) y)]
                  (min-key first [(+ score delta) (assoc state' (first layer) y)]
                                (f layer (inc y)))))))))
  (defn build-layer-solver [node->parents heights max-y]
    (assert (every? pos-int? (vals heights)))
    (fn [node->y layer]
      (let [[score node->y] ((build-layer-solver-1 node->parents heights node->y max-y) layer 0)]
        (assert (not (infinite? score)))
        (assert (= (count layer) (count (set (map node->y layer)))))
        node->y)))
comment)


(defn- node-parents [edges]
  (reduce (fn [m e] (update m (:edge/target e) conj (:edge/source e))) {} edges))

(defn- node-children [edges]
  (reduce (fn [m e] (update m (:edge/source e) conj (:edge/target e))) {} edges))

(defn- node-weight [edges]
  (let [node->incoming (group-by :edge/target edges)
        node->outgoing (group-by :edge/source edges)
        nodes          (set (concat (keys node->incoming) (keys node->outgoing)))]
    (zipmap nodes
            (for [n nodes] (max (transduce (map :edge/weight) + (node->incoming n))
                                (transduce (map :edge/weight) + (node->outgoing n)))))))

(defn assign-coordinates [layers edges]
  (let [node->weight (node-weight edges)
        edges        (remove :edge/back? edges)
        nodes      (keys node->weight)
        max-weight (apply max (vals node->weight))
        max-logical-width 6 ;; could come from parameter?
        ;; FIXME: we should consider scales when we get there

        ;;; logical heights of each node
        heights (update-vals node->weight (fn [weight] (long (math/ceil (/ (* weight max-logical-width) max-weight)))))
        
        ;; pessimistic scenario, when all the wide nodes are on the same layer
        max-y (* max-logical-width (reduce max (map count layers)))

        solve-forwards (build-layer-solver (node-parents edges) heights max-y)
        solve-backward (build-layer-solver (node-children edges) heights max-y)

        ;; we do multiple iterations: forward-backward-forward
        node->y (as-> {} m
                  (reduce solve-forwards m layers)
                  (reduce solve-backward m (next (reverse layers))) ;; last layer is fixed
                  (reduce solve-forwards m (next layers))) ;; first layer is fixed

        ;; elements are not filled from the top so we need to push it back
        min-y        (reduce min (vals node->y))
        coordinates  (reduce-kv (fn [m x layer]
                                  (reduce (fn [m n] (assoc m n [x (- (node->y n) min-y)])) m (vec layer)))
                                {} (vec layers))
        grid-heights (repeat (- max-y min-y) (/ max-weight max-logical-width))]
    {:coordinates          coordinates
     :node-logical-heights heights
     :grid-heights         grid-heights}))
