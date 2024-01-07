(ns riall.parse
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)

;; TODO: let me read floatingpt weights as well.

(defn- parse-edge [line]
  (when-let [[_ from weight to] (re-matches #"^ *([^\[]+) +\[(\d+(?:\.\d+)?)\] +(.*) *$" line)]
    {:edge/id (gensym 'edge)
     :edge/source from
     :edge/target to
     :edge/weight (parse-double weight)}))


(defn parse-config [line]
  (when-let [[_ path value] (re-matches #"set +([a-zA-Z.\-]+) +([^ ]?.*[^ ]) *$" line)]
    (let [[p & path] (seq (.split ^String path "\\."))
          p (case p ("canvas" "edge" "node" "column") (keyword p) p)]
      {:config/path  (into [p] (map keyword) path)
       :config/value value})))


(defn- warn [line]
  (binding [*out* *err*]
    (println "Could not parse line:" line)))


(def xform
  (comp (map str/trim)
        (remove str/blank?)
        (remove #(str/starts-with? % "#"))
        (remove #(str/starts-with? % "//"))
        (keep (some-fn parse-edge parse-config warn))))
