(ns riall.layout.coordinates
  "Coordinate assignment step of the layout algorithm."
  (:require [clojure.math :as math]
            [riall.common :refer [+inf]]))

(set! *warn-on-reflection* true)

;; makes coordinate expressions easier to read
(defmacro ^:private idx-> [f a x y & v] `(~f ~a (+ ~x (* (mod ~y ~'max-height) (inc ~'max-x))) ~@v))

;; Calculates the score to minimize when node is placed on y
#_{:clj-kondo/ignore [:unused-binding]}
(defmulti node-score (fn [edge-weight-fn node->parents heights node->y node y] :sq-weighted))

;; distance quare sum between top vertical positions of nodes
(defmethod node-score :top-sq [_ node->parents _ node->y node y]
  (reduce + (for [p (node->parents node)] (math/pow (- y (node->y p)) 2))))

;; distance square to midpoint of parents' convex hull
(defmethod node-score :midbox [_ node->parents heights node->y node y]
  (empty? (node->parents node))
  0
  (let [p-top (reduce min (for [p (node->parents node)] (node->y p)))
        p-btm (reduce max (for [p (node->parents node)] (+ (node->y p) (heights p))))]
    (math/pow (- (+ y (/ (heights node) 2)) (/ (+ p-top p-btm) 2)) 2)))

;; distance square sum between the midpoints of nodes
(defmethod node-score :sq [_ node->parents heights node->y node y]
  (reduce + (for [p (node->parents node)]
               (math/pow (- (+ y (/ (heights node) 2)) (+ (node->y p) (/ (heights p) 2))) 2))))

;; distance square sum between the midpoints of nodes, weighted by node
(defmethod node-score :sq-weighted [edge-weight-fn node->parents heights node->y node y]
  (reduce + (for [p (node->parents node)]
              (* (edge-weight-fn p node)
                 (math/pow (- (+ y (/ (heights node) 2)) (+ (node->y p) (/ (heights p) 2))) 2)))))

; Creates a reducer function that given a layer (list of nodes), updates a node->y mapping
; so that the of total sum of (node-score) over each nodes is minimal.
(defn build-layer-solver [edge-weight-fn node->parents heights max-y]
  (fn [node->y layer]
    (let [max-x      (count layer)
          max-height (transduce (map heights) max 1 layer)
          states     (object-array (* (inc max-x) max-height))
          costs      (double-array (* (inc max-x) max-height))
          cost-of    (fn [node new-y] (node-score edge-weight-fn node->parents heights node->y node new-y))]
      (dotimes [x max-x]
        (idx-> aset costs x max-y (double +inf)))
      (dotimes [y' max-y]
        (dotimes [x (min max-x (- max-y y'))] ;; 0 1 2 ... mx-1 <---- but order does not matter.
          (let [xth-node   (nth layer x)
                y          (- max-y y' 1)
                jump-y     (+ y (int (heights (nth layer x))))
                cost-place (if (< jump-y max-y)
                             (+ (cost-of xth-node y) (idx-> aget costs (inc x) jump-y))
                             +inf) ;; cannot place outside of grid
                cost-skip  (idx-> aget costs x (inc y))]
            (if (<= cost-place cost-skip) ;; FIXME: should it be < instead?
              (do (idx-> aset costs x y (double cost-place))
                  (idx-> aset states x y (cons y (idx-> aget states (inc x) jump-y))))
              (do (idx-> aset costs x y cost-skip)
                  (idx-> aset states x y (idx-> aget states x (inc y))))))))
      (->> (idx-> aget states 0 0) (map vector layer) (into node->y)))))


;; --------------

(comment
  (def ^:private layer-step-inf [+inf {}])
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
  (defn build-layer-solver [_ node->parents heights max-y]
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
            (for [n nodes] (do (do (max (transduce (map :edge/weight) + (node->incoming n))
                                        (transduce (map :edge/weight) + (node->outgoing n))))) ))))

(def resolution 6)

(defn assign-coordinates [layers edges]
  (let [node->weight (node-weight edges)
        edges        (remove :edge/back? edges)
        max-weight (apply max (vals node->weight))
        ;; FIXME: we should consider scales when we get there
        
        ;;; logical heights of each node
        heights (update-vals node->weight (fn [weight] (long (math/ceil (/ (* weight resolution) max-weight)))))
        ;; pessimistic scenario, when all the wide nodes are on the same layer
        max-y (* resolution (reduce max (map count layers)))

        edge-weight-fn (fn [parent node]
                         (reduce + (for [e edges
                                         :when (= parent (:edge/source e))
                                         :when (= node (:edge/target e))]
                                     (:edge/weight e))))
        solve-forwards (build-layer-solver edge-weight-fn (node-parents edges) heights max-y)
        solve-backward (build-layer-solver edge-weight-fn (node-children edges) heights max-y)

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
        grid-heights (repeat (- max-y min-y) (/ max-weight resolution))]
    {:coordinates          coordinates
     :node-logical-heights heights
     :grid-heights         grid-heights}))
