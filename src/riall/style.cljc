(ns riall.style
  (:require [riall.config :refer [get-node-config]]))

(def tableau10 ["#5778a4" "#e49444" "#d1615d" "#85b6b2" "#6a9f58" "#e7ca60" "#a87c9f" "#f1a2a9" "#967662" "#b8b0ac"])

;; TODO: make node->color deterministic. sometimes colors change on rerender.

(defmulti node->color (fn [node] (get-node-config node :background)))

(let [colors    (atom (cycle tableau10))
      get-color (memoize (fn [_] (ffirst (swap-vals! colors next))))]
  (defmethod node->color "theme" [node] (get-color node)))

(defmethod node->color :default [node] (get-node-config node :background))

;; Returns the scale assigned to the node or the default scale
; (defn node-scale [node] )

;; 
; (defprotocol ColumnSizing ?)
; eg: (->ScaledColumnSizing 0.80 :gaps-inner :top-bottom :center-align) (uses at most 80% of screen height. the remaining 20% is used for gaps)
