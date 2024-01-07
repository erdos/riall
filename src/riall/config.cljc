(ns riall.config
  "Database of configuration options"
  (:require [riall.graph]))

;; the parsed model
(def ^:dynamic *model*)

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

(defcfg [edge label background] "rgba(215,215,215,0.2)"
  "Backgroud color of edge labels.")

(defcfg [edge opacity] 1.0
  "Opacity of edge. Value is between 0.0 (fully transparent) to 1.0 (fully opaque).")

(defcfg [edge stroke color] "white"
  "Color of outline of edges.")
(defcfg [edge stroke width] 1
  "Width of edge stroke.")

(defcfg [node background] "theme"
  "Default background of edges.")
(defcfg [node opacity] 1.0
  "Opacity of node. Value is between 0.0 (fully transparent) to 1.0 (fully opaque).")

(defcfg [node stroke color] "white"
  "Color of outline of node rectangles.")
(defcfg [node stroke width] 1.0
  "Width of node outline.")

(defcfg [node width] 15
  "Width of nodes.")

(defcfg [node-hidden width] 0 "implementation detail")

(defcfg [node shape rx] 0
  "Horizontal rounding of the node's rectangle")
(defcfg [node shape ry] 0 
  "Vertical rounding of the node's rectangle")

(defcfg [node label align] "center"
  "Node label alignment: left, right, center")

(defcfg [node label color] "black"
  "Color of node label text")

(defcfg [node label background] "rgba(255,255,255,0.2)"
  "Backgroud color of node labels.")

(defcfg [column width] 120
  "Width of logical columns")


(defn config-exists? [config]
  (boolean (get-in configuration (conj (:config/path config) ::description))))

(defmacro get-config [& path]
  (assert (every? keyword? path))
  (assert (contains? (get-in configuration path) ::description)
          (pr-str ["Unknown configuration!" configuration path]))
  `(or  (assert *model*)
        (some-> (get-in *model* [:config ~@path :value]) ~(::parser (get-in configuration path)))
       ~(::default (get-in configuration path))))

(defn- get-model-config [selector path]
  (assert *model*)
  (assert (contains? (get-in configuration (cons :node path)) ::description))
  (when-let [value (get-in *model* (concat [:config selector] path [:value]))]
    ((::parser (get-in configuration (cons :node path))) value)))

(defn -get-node-config [node path]
  (or
   ;; value configured for this node specifically.
   (get-model-config node path)

   (when (@#'riall.graph/hidden-node? node)
     (or (get-model-config :node-hidden path)
         (::default (get-in configuration (cons :node-hidden path)))))
   
   ;; node is both source and sink
   (condp = [(some? (get-in *model* [:incoming-edges node]))
             (some? (get-in *model* [:outgoing-edges node]))]
     [true false] (get-model-config :node-sink path)
     [false true] (get-model-config :node-source path)
     [false false] (get-model-config :node-isolated path)
     nil)

   (get-model-config :node path)

   ;; default value for any nodes
   (::default (get-in configuration (cons :node path)))))

;; resolves configuration for node
;; - first, by id
;; - last, from general node config or the default value
(defmacro get-node-config [node & path]
  (assert (symbol? node))
  (assert (every? keyword? path))
  `(-get-node-config ~node [~@path]))

(defmacro get-edge-config [edge & path]
  (assert (symbol? edge))
  `(get-config :edge ~@path))

;; return a seq of config options
(defn render-help-options []

)