(ns riall.common)

(set! *warn-on-reflection* true)

(def ^:dynamic *debug* false)

(defn index-of [coll elem]
  (let [idx (.indexOf ^java.util.List coll elem)]
     (when-not (= -1 idx) idx)))

(defn median [coll]
  (assert (not-empty coll))
  (let [sorted  (sort coll)
        cnt     (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (/ (+ (nth sorted (dec halfway)) (nth sorted halfway)) 2.0))))

(defn mean [coll]
  (when (empty? coll) (throw (ex-info "Cannot calculate mean on empty coll" {})))
  (/ (reduce + 0.0 coll) (count coll)))

(def +inf Double/POSITIVE_INFINITY)

(defn fixpt [f x] (let [fx (f x)] (if (= x fx) x (recur f fx))))

(defmacro timing [label x]
  `(if-not *debug* 
      ~x
      (let [before# (System/nanoTime)
            value#  (doto ~x dorun)
            after#  (System/nanoTime)]
        (binding [*out* *err*]
          (println "Timed" ~label (quot (- after# before#) 1000000) "ms"))
        value#)))
