(ns riall.style)

(def tableau10 ["#5778a4" "#e49444" "#d1615d" "#85b6b2" "#6a9f58" "#e7ca60" "#a87c9f" "#f1a2a9" "#967662" "#b8b0ac"])

;; TODO: make node->color deterministic. sometimes colors change on rerender.

(let [colors (atom (cycle tableau10))]
  (def node->color (memoize (fn [_] (ffirst (swap-vals! colors next))))))

;; Used to get the sizes of the boxes
;; Eg.: ->LinearScale, ->LogScale ...
(defprotocol NodeScale
  (source-weight [_ node])
  (target-weight [_ node]))

(defn weight [scale node]
  (max (source-weight scale node)
       (target-weight scale node)))

;; Returns the scale assigned to the node or the default scale
; (defn node-scale [node] )

;; 
; (defprotocol ColumnSizing ?)
; eg: (->ScaledColumnSizing 0.80 :gaps-inner :top-bottom :center-align) (uses at most 80% of screen height. the remaining 20% is used for gaps)
