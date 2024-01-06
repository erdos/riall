(ns riall.model
  (:require [riall.config :refer [get-config get-node-config *model*]]
            [riall.layout :refer :all]
            [riall.style :refer :all]
            [riall.common :refer :all]))

(defn node-pos
  ([] (doto (:coords *model*) assert))
  ([node] (get (node-pos) node)))

(defn incoming-edges [node]
  (->> (get-in *model* [:incoming-edges node])
       (sort-by (juxt :edge/back?
                      (comp second node-pos :edge/source)
                      (comp - first node-pos :edge/source);; ?
                      ))))

(defn outgoing-edges [node]
  (->> (get-in *model* [:outgoing-edges node])
       (sort-by (juxt :edge/back?
                      (comp second node-pos :edge/target)
                      (comp - first node-pos :edge/target) ;; ?
                      ))))

(defn all-nodes [] (:nodes *model*))
(defn id->edge ([] (:id->edge *model*)) ([id] (get-in *model* [:id->edge id])))

(def linear-scale
  (reify NodeScale
    (source-weight [_ node]
      (transduce (map :edge/weight) + (incoming-edges node)))
    (target-weight [_ node]
      (transduce (map :edge/weight) + (outgoing-edges node)))))

(defn get-column-width []
  (get-config :column :width))

;; Builds a [x y width height] vector for a given node
(defmulti node->bbox (fn [node] #_(get-node-config node :grid :vertical-align)
                                (assert *model*) :middle))
;; TODO: get dispatch from config

(defmethod node->bbox :middle [node]
  (let [[x y]   (node-pos node)
        lo-he   (get-in *model* [:node-logical-heights node])
        top-pos (transduce (take y) + (:grid-heights *model*))
        grid-height (reduce + (take lo-he (drop y (:grid-heights *model*))))
        height  (weight linear-scale node)]
    [(* (get-column-width) x)
     (+ top-pos (/ (- grid-height height) 2))
     (get-node-config node :width)
     height]))

(defmethod node->bbox :top [node]
  (let [[x y] (node-pos node)]
    [(* (get-column-width) x)
     (transduce (take y) + (:grid-heights *model*)) ;; sum of every heights above
     (get-node-config node :width)
     (weight linear-scale node)]))

(defmethod node->bbox :bottom [node]
  (let [[x y]      (node-pos node)
        lo-he      (get-in *model* [:node-logical-heights node])
        bottom-pos (reduce + (take (+ lo-he y) (:grid-heights *model*)))
        height     (weight linear-scale node)]
    [(* (get-column-width) x)
     (- bottom-pos height)
     (get-node-config node :width)
     height]))

(defn node->midpt [node]
  (let [[x y w h] (node->bbox node)]
    [(+ x) (+ y (/ h 2))]))

;; returns [src-top% src-height%, target-top%, target-height%]
(defn edge->ratios [edge-id]
  (assert (symbol? edge-id))
  (let [{:edge/keys [source target weight] :as edge} (id->edge edge-id)
        ;; ----
        outgoings  (reductions + 0 (map :edge/weight (outgoing-edges source)))
        source-sum (last outgoings)
        source-off (nth outgoings (index-of (outgoing-edges source) edge))
        ;; ----
        incomings  (reductions + 0 (map :edge/weight (incoming-edges target)))
        target-sum (last incomings)
        target-off (nth incomings (index-of (incoming-edges target) edge))]
    [(/ source-off source-sum) (/ weight source-sum)
     (/ target-off target-sum) (/ weight target-sum)]))
