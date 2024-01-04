(ns riall.core
  (:gen-class)
  (:require [clojure.string :as str]
            [riall.common :refer [*debug*]]
            [riall.config :refer [get-config]]
            [riall.layout :refer [layouts]]
            [riall.model :refer :all]
            [riall.style :refer :all] [riall.parse]
            [riall.svg :as svg :refer [hiccup]]))


(set! *warn-on-reflection* true)


(defn get-canvas-dimensions []
  (let [margin (get-config :canvas :margin)
        width  (get-config :canvas :width)
        height (get-config :canvas :height)
        scale-x (/ (- (double width) margin margin)
                   (* (get-column-width) (:max-column *model*)))
        scale-y (->> (keys (node-pos))
                     (map node->bbox)
                     (map (fn [[_ y _ h]] (+ y h)))
                     (reduce max)
                     (/ (- (double height) margin margin)))]
    {:margin margin
     :height height
     :width  width
     :scale-x scale-x
     :scale-y scale-y}))


(defn make-svg []
  (let [{:keys [width height] :as cfg} (get-canvas-dimensions)]
    [:svg {:xmlns "http://www.w3.org/2000/svg"
           :width width
           :height height
           :viewBox (format "0 0 %d %d" (long width) (long height))}
     [:defs (map svg/edge-gradient (set (for [e (vals (id->edge))] (or (:edge/original e) e))))]
     \newline
     (when-let [fill (get-config :canvas :background)]
       [:rect {:width "100%" :height "100%" :fill fill}])
     (when *debug*
       [:g (svg/render-debug cfg)])
     (for [b? [false true]
           {:edge/keys [back?] :as edge} (sort-by :edge/weight > (vals (id->edge)))
           :when (nil? (:edge/original edge))
           :when (= b? back?)]
       (if back?
         (svg/render-backedge cfg edge)
         (svg/render-edge cfg edge)))
     (->> (id->edge) (vals) (sort-by :edge/id)
          (filter :edge/original) (group-by :edge/original) (vals)
          (map (partial svg/render-edge-seq cfg)))
     (let [elems (for [n (all-nodes)
                       :when (or *debug* (not (riall.graph/hidden-node? n)))]
                   (svg/render-node cfg n))]
       [:g (map first elems) (map second elems)])
     [:g]]))


(defn render-svg []
  (println "<?xml version=\"1.0\"?>")
  (hiccup (make-svg)))


(defn parsed->model [input]
  (let [layout    (layouts (filter :edge/id input))
        node->pos (:coordinates layout)
        edges     (for [e (:edges layout)]
                    (assoc e :edge/back? (>= (first (node->pos (:edge/source e)))
                                             (first (node->pos (:edge/target e))))))]
    {:config         (reduce (fn [m c] (assoc-in m (conj (:config/path c) :value) (:config/value c)))
                             {} (filter :config/path input))
     :coords               (:coordinates layout)
     :grid-heights         (:grid-heights layout)
     :node-logical-heights (:node-logical-heights layout)
     :max-column     (transduce (map first) max -1 (vals node->pos)) ;; index only
     :incoming-edges (group-by :edge/target edges)
     :outgoing-edges (group-by :edge/source edges)
     :nodes          (set (mapcat (juxt :edge/source :edge/target) edges))
     :id->edge       (reduce (fn [m e] (assoc m (:edge/id e) e)) {} edges)}))


(defn -main [& args]
  (binding [*debug* (boolean (some #{"--debug"} args))]
    (let [input (->> *in*
                     (slurp) (clojure.string/split-lines)
                     (sequence riall.parse/xform))]
      (binding [*model* (parsed->model input)]
        (render-svg)))))