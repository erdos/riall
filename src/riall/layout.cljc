(ns riall.layout
  (:require [riall.common :refer [timing]]
            [riall.graph :as graph]
            [riall.layout.coordinates :as coords]))

;; all edges belong to the same components
(defn layouts [edges]
  (let [edges          (timing "remove cycles" (graph/remove-cycles edges)) ;; mark cycle nodes with :edge/back
        back-edges     (filter :edge/back? edges)
        edges          (remove :edge/back? edges)
        layers         (timing "layer assignment" (graph/layer-assignment edges))
        [layers edges] (graph/add-hidden-edges layers edges)
        layers         (timing "vertex ordering" (graph/vertex-ordering layers edges))
        edges          (into (set edges) back-edges)
        coords         (timing "assign coords" (coords/assign-coordinates layers edges))]
    (merge {:edges edges} coords)))
