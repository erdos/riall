(ns riall.svg
  (:require [clojure.string :as str]
            [riall.config :refer [get-edge-config get-node-config *model*]]
            [riall.model :refer :all]
            [riall.style :refer [node->color]]
            [riall.common :refer [mean]]))

(defn path-d [& xs]
  (reify
    clojure.lang.Seqable
    (seq [_] (seq xs))
    Object
    (toString [_]
       (str/join " " (for [x xs, x x]
                  (cond (keyword? x) (name x)
                        (integer? x) (str x)
                        (double? x)  (format "%.1f" x)
                        :else (assert false)))))))

(defn escape-html [text]
  (.. (str text) (replace "&"  "&amp;") (replace "<"  "&lt;") (replace ">"  "&gt;") (replace "\"" "&quot;")))

(defn hiccup [elem]
  (cond (vector? elem)
        (let [[tag args & bodies] (if (map? (second elem))
                                    elem (list* (first elem) {} (next elem)))]
          (print (str "<" (name tag)))
          (when (seq args)
            (doseq [[k v] args] (print (str " " (name k) \= \" (escape-html v) \"))))
          (if (seq bodies)
            (do (print ">") (run! hiccup bodies) (print (str "</" (name tag) ">")))
            (print "/>")))
        (seq? elem) (run! hiccup elem)
        :else       (print (escape-html elem))))

(defn projecting [{:keys [margin scale-x scale-y]} [x y w h]]
  [(+ margin (* x scale-x))
   (+ margin (* y scale-y))
   (some-> w (* scale-x))
   (some-> h (* scale-y))])

(defn edge-gradient [{:edge/keys [source target back? id]}]
  [:linearGradient {:id id :x1 (if back? 1 0) :y1 0 :x2 (if back? 0 1) :y2 0}
    [:stop {:offset "0%" :stop-color (node->color source)}]
    [:stop {:offset "100%" :stop-color (node->color target)}]])


(defn render-node [config node]
  (let [[_ _ w _ :as box] (node->bbox node)
        [x y _ h]         (projecting config box)
        [mx my]           (projecting config (node->midpt node))
        [x-off anchor]    (case (get-node-config node :label :align)
                            "right"  [(/ w 2) "start"]
                            "left"   [(/ w -2) "end"]
                            "center" [0 "middle"])]
    (list
     [:rect {:width  w
             :height h
             :x      (- x (/ w 2))
             :y      y
             :rx     (get-node-config node :shape :rx)
             :ry     (get-node-config node :shape :ry)
             :style  (format "fill:%s; fill-opacity: %f; stroke-width: %f; stroke: %s"
                              (node->color node)
                              (get-node-config node :opacity)
                              (get-node-config node :stroke :width)
                              (get-node-config node :stroke :color))}]
     [:text {:x (+ x x-off)
             :y my
             :filter "url(#labelbg)"
             :fill (get-node-config node :label :color)
             :alignment-baseline "middle" :dominant-baseline "middle"
             :text-anchor anchor
             :font-family "Arial, Helvetica, sans-serif"
             :font-size   "11"
             :font-weight (get-node-config node :label :font :weight)
             }
            (str node)])))

(defmulti edge-fill (fn [edge] (get-edge-config edge :background)))

(defmethod edge-fill :default [edge]
  (get-edge-config edge :background))

(defmethod edge-fill "gradient" [edge]
  (str "url(#" (:edge/id edge) ")"))

(defmethod edge-fill "source" [edge]
  (-> edge :edge/source node->color))

(defmethod edge-fill "target" [edge]
  (-> edge :edge/target node->color))

(defn- render-frontedge [{:as cfg :keys [scale-x scale-y]} {:edge/keys [id source target back?] :as e}]
  (assert (not back?))
  (let [[src-top% src-height% trg-top% trg-height%] (edge->ratios id)
        [x1 y1 src-width] (node->bbox source)
        [x2 y2 dst-width] (node->bbox target)
        [x1 y1] (projecting cfg [x1 y1])
        [x2 y2] (projecting cfg [x2 y2])

        x1 (+ x1 (max 0 (- (/ src-width 2) (get-node-config source :shape :rx))))
        x2 (- x2 (max 0 (- (/ dst-width 2) (get-node-config target :shape :rx))))

        ;; not full height is used but rather we are filling both sides top-down
        h1 (* scale-y (target-weight source))
        h2 (* scale-y (source-weight target))
        w1 (* src-top% h1)
        w2 (* trg-top% h2)
        r1 (* src-height% h1)
        r2 (* trg-height% h2)
        handle (* scale-x 44)]
    [:path {:fill (edge-fill e)
            :fill-opacity (get-edge-config e :opacity)
            :stroke       (get-edge-config e :stroke :color)
            :stroke-width (get-edge-config e :stroke :width)
            :d (path-d
                [:M x1 (+ y1 w1)] ;; top of first node
                [:C (+ x1 handle) (+ y1 w1) ;; ctrl pt of first top
                    (- x2 handle) (+ y2 w2) ;; ctrl pt of second top
                    x2            (+ y2 w2)] ;; top of second node
                [:v r2] ;; bottom of second
                [:C (- x2 handle) (+ y2 w2 r2)  ;; ctrl1
                    (+ x1 handle) (+ y1 w1 r1)  ;; ctrl2
                    x1            (+ y1 w1 r1)] ;; target
                [:v (- r1)] ;; not necessary
                #_[:Z])}]))

(defn render-edge-seq [cfg edges]
  (->> {:fill          (edge-fill original)
        :fill-opacity  (get-edge-config original :opacity)
        :stroke        (get-edge-config original :stroke :color)
        :stroke-width  (get-edge-config original :stroke :width)
        :d  (apply path-d (concat [(nth (first paths) 0)]
                                  (for [p paths] (nth p 1))
                                  [(nth (last paths) 2)]
                                  (for [p (reverse paths)] (nth p 3))
                                  [(nth (first paths) 4)]
                                  [[:Z]]))}
       (vector :path)
       (let [original (-> edges first :edge/original)
             paths    (for [e edges] (-> (render-frontedge cfg e) second :d seq))])))

(defn- render-backedge [{:as cfg :keys [scale-x scale-y height margin] }
                       {:edge/keys [id source target back?] :as edge}]
  (let [[src-top% src-height% trg-top% trg-height%] (edge->ratios id)
        [x1 y1] (projecting cfg (node->bbox source))
        [x2 y2] (projecting cfg (node->bbox target))
                ;; not full height is used but rather we are filling both sides top-down
        h1 (* scale-y (target-weight source))
        h2 (* scale-y (source-weight target))
        w1 (* src-top% h1)
        w2 (* trg-top% h2)
        r1 (* src-height% h1)
        r2 (* trg-height% h2)
        off 10
        t  (- height margin)]
    [:path {:fill         (edge-fill edge)
            :fill-opacity (get-edge-config edge :opacity)
            :stroke       (get-edge-config edge :stroke :color)
            :stroke-width (get-edge-config edge :stroke :width)
            :d (path-d [:M x1 (+ y1 w1)]
                       [:h off]
                       [:a r1 r1 0 0 1 r1 r1]
                       [:V t]
                       [:a r1 r1 0 0,1 (- r1) r1]
                       [:h (- off)]
                       [:L x2 (+ t r2)]
                       [:h (- off)]
                       [:a r2 r2 0 0,1 (- r2) (- r2)]
                       [:V (+ y2 w2 r2)] ;; diagonal
                       [:a r2,r2 0 0,1 r2 (- r2)]
                       [:h off]
                       [:v (+ r2)]
                       [:h (- off)]
                       [:V t]
                       [:H x1]
                       [:h off]
                       [:V (+ y1 w1 r1)]
                       [:h (- off)]
                       [:z])}]))

(defn render-edge [cfg edge]
  (if (:edge/back? edge)
    (render-backedge cfg edge)
    (render-frontedge cfg edge)))

;; return [x y] tuple of label position
(defn- edge-label-position [{:as cfg :keys [scale-y]} {:edge/keys [source target id]}]
  #_(let [[src-top% src-height% _ _] (edge->ratios id)
          [x1 y1 src-width] (node->bbox source)
          [x1 y1] (projecting cfg [x1 y1])
          h1 (* scale-y (target-weight linear-scale source))
          w1 (* src-top% h1)
          r1 (* src-height% h1)]
      [(+ x1 5 (/ src-width 2))
       (+ y1 w1 (/ r1 2))])
  (let [[src-top% src-height% dst-top% dst-height%] (edge->ratios id)
        [x1 y1] (projecting cfg (node->bbox source))
        h1 (* scale-y (target-weight source))
        w1 (* src-top% h1)
        r1 (* src-height% h1)
        [x2 y2] (projecting cfg (node->bbox target))
        h2 (* scale-y (source-weight target))
        w2 (* dst-top% h2)
        r2 (* dst-height% h2)]
    [(mean [x1 x2])
     (mean [(+ y1 w1 (/ r1 2)) (+ y2 w2 (/ r2 2))])]))

;; returns an edge label to be placed near the source node
(defn render-edge-src-label [cfg {:as edge :edge/keys [source weight original]}]
  (when (or (not original) (= source (:edge/source original)))
    (let [[x y] (edge-label-position cfg edge)]
      [:text {:x x :y y
              :filter "url(#edgelabelbg)"
              :text-anchor "middle"
              :alignment-baseline "middle"
              :dominant-baseline "middle"
              :font-family "Arial, Helvetica, sans-serif"
              :font-size   "11"
              :font-weight (get-edge-config edge :label :font :weight)}
       (str weight)])))

(defn render-debug [{:as cfg :keys [width margin]}]
  [:g
    (for [h (reductions + 0 (:grid-heights *model*))
          :let [[x y w _] (projecting cfg [0 h width 0])]]
      [:line {:stroke "silver" :x1 x :x2 (- width margin) :y1 y :y2 y}])])