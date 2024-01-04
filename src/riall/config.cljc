(ns riall.config
  "Database of configuration options")

(def ^:private configuration {})

(defmacro ^:private defcfg [path default descr]
  `(alter-var-root #'configuration assoc-in ~(mapv keyword path)
                   ~{::description descr
                     ::default default
                     ::parser (cond (number? default) 'parse-double
                                    :else             'identity)}))

(defcfg [canvas width] 500 "SVG image width in pixels")
(defcfg [canvas height] 500 "SVG image height in pixels")
(defcfg [canvas background] nil "SVG image background color.")
(defcfg [canvas margin] 30 "Margin around boxes in canvas")
(defcfg [canvas gaps] 4 "Gap between rows")

(defcfg [edge background] "gradient"
  "Default background of edges. Either: 'source', 'gradient', 'target', or a color name.")

(defcfg [edge opacity] 1.0
  "Opacity of edge. Value is between 0.0 (fully transparent) to 1.0 (fully opaque).")

(defcfg [edge stroke color] "white"
  "Color of outline of edges.")
(defcfg [edge stroke width] 1
  "Width of edge stroke.")

;; TODO: top, bottom, center, stretch
;(defcfg [edge justify] "top"
;  "Justification of edge. Values: top bottom center stretch")
;(defcfg [edge source justify] [edge justify]
;  "Justification of edge on the source side.")
;(defcfg [edge target justify] [edge justify]
;  "Justification of edge on the source side.")

(defcfg [node background] "tableau10"
  "Default background of edges.")
(defcfg [node opacity] 1.0
  "Opacity of node. Value is between 0.0 (fully transparent) to 1.0 (fully opaque).")

(defcfg [node stroke color] "white"
  "Color of outline of node rectangles.")
(defcfg [node stroke width] 1.0
  "Width of node outline.")

(defcfg [node width] 15
  "Width of nodes.")

(defcfg [node shape rx] 0 "Horizontal rounding of the node's rectangle")
(defcfg [node shape ry] 0 "Vertical rounding of the node's rectangle")

(defcfg [node label align] "center"
  "Node label alignment: left, right, center")

(defcfg [column width] 120
  "Width of logical columns")

(defn config-exists? [config]
  (boolean (get-in configuration (conj (:config/path config) ::description))))

(defmacro get-config [& path]
  (assert (every? keyword? path))
  (assert (contains? (get-in configuration path) ::description)
          (pr-str ["Unknown configuration!" configuration path]))
  `(or  (assert riall.model/*model*)
        (some-> (get-in riall.model/*model* [:config ~@path :value]) ~(::parser (get-in configuration path)))
       ~(::default (get-in configuration path))))

;; 
(defmacro get-node-config [node & path]
  (assert (symbol? node))
  `(get-config :node ~@path))

(defmacro get-edge-config [edge & path]
  (assert (symbol? edge))
  `(get-config :edge ~@path))

;; return a seq of config options
(defn render-help-options []

)