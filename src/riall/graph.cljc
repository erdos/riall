(ns riall.graph
  (:require [clojure.set :refer [difference union]]
            [clojure.string :refer [starts-with?]]
            [riall.common :refer :all]))

(set! *warn-on-reflection* true)

;; source: https://publications.lib.chalmers.se/records/fulltext/161388.pdf

(defn flood [f init]
  (loop [result #{}
         job    [init]]
    (if-let [[j] (seq job)]
      (if (result j)
        (recur result (next job))
        (recur (conj result j) (-> job next (into (f j)))))
      result)))

;; split up the graph to components and these can be processed independently from each other
;; returns a coll of colls of edges: [[e e] [e] [e e e]]
(defn edges->components [edges]
  ;; flood fills repeatedly.
  (let [node->out       (group-by :edge/source edges)
        node->in        (group-by :edge/target edges)
        node->neighbors (fn [n] (concat (map :edge/source (node->in n))
                                        (map :edge/target (node->out n))))]
    (loop [nodes  (concat (keys node->out) (keys node->in))
           result ()]
      (if-let [[n] (seq nodes)]
        (let [f (flood node->neighbors (first nodes))]
          (recur (remove f nodes) (conj result (mapcat node->in f))))
        result))))

;; Returns an updated edge list, where :edge/back? is set to true for removed edges.
;; 
;; Implements a simple heuristic from
;; B. Berger and P. W. Shor, “Approximation algorithms for the maximum acyclic subgraph problem”
;;
(defn remove-cycles [edges]
  (let [node->out (group-by :edge/source edges)
        node->in  (group-by :edge/target edges)]
    (loop [nodes (set (concat (keys node->in) (keys node->out)))
           reversed #{}
           removed   #{}]
      (if-let [[n] (seq nodes)]
        (let [ins (remove removed (node->in n)) outs (remove removed (node->out n))]
          (recur (next nodes)
                 (into reversed (min-key count outs ins))
                 (-> removed (into ins) (into outs))))
        (for [edge edges]
          (assoc edge :edge/back? (contains? reversed edge)))))))

;; edges that dont have a continuation
(defn- sink-edges [edges]
  (let [a (set (map :edge/source edges))]
    (remove (comp a :edge/target) edges)))
(defn- source-edges [edges]
  (let [a (set (map :edge/target edges))]
    (remove (comp a :edge/source) edges)))

;; Returns updated edge list
;; 
;; Implements an enhanced greedy heuristic from
;; P. Eades, X. Lin, and W. Smyth, “A fast and effective heuristic for the feedback arc set problem”
;;
;; (runtime complexity is broken due to heavy use of list operations)
(defn remove-cycles [edges']
  (loop [er #{}
         edges edges']
    (if-not (seq edges)
      (for [edge edges'] (assoc edge :edge/back? (contains? er edge)))
      (let [edges (fixpt (fn [edges] (remove (set (sink-edges edges)) edges)) edges)
            edges (fixpt (fn [edges] (remove (set (source-edges edges)) edges)) edges)]
        (if (empty? edges)
          (recur er edges) ;; goto start
          (let [e- (group-by :edge/source edges)
                e+ (group-by :edge/target edges)
                _ (assert (= (set (keys e-)) (set (keys e+)))) ;; we only have cycles now
                v (apply max-key (fn [v] (- (count (e+ v)) (count (e- v)))) (keys e-))]
            (recur (into er (e- v)) (->> edges (remove (set (e- v))) (remove (set (e+ v)))))))))))

;; TODO: test

;;;;;;;;;;;;;; LAYER ASSIGNMENT ;;;;;;;;;;;;;;;;;;
;;
;;
;; Given a DAG, assign one layer for each node.
;; Returns layers in the form of nested vectors of nodes: [[node]]
;; 
;; Implements "Longest path" algorithm from
;; R. Tamassia, “Handbook of Graph Drawing and Visualization”
;; 
(defn layer-assignment [edges]
  (assert (not-any? :edge/back? edges))
  (assert (every? :edge/source edges))
  (assert (every? :edge/target edges))
  (let [node->parents (reduce (fn [m x] (update m (:edge/target x) conj (:edge/source x))) {} edges)
        nodes         (set (flatten (seq node->parents)))]
    (loop [seen #{}
           nodes nodes
           result []]
      (if-not (seq nodes)
        result
        (if-let [candidates (seq (for [n nodes :when (every? seen (node->parents n))] n))]
          (recur (into seen candidates)
                 (remove (set candidates) nodes)
                 (conj result candidates))
          (throw (ex-info "Cycle detected!" {})))))))

;;;;;;;;;;; VERTEX ORDERING ;;;;;;;;;;;;;

;; Sorts nodes in current layer by the median position of parent layers in prev layer.
(defn wmedian-mapfn [node->parents previous current]
  (let [parent-pos (partial index-of previous)
        keyfn      (fn [n] (some->> n node->parents (keep parent-pos) seq median))]
    (sort-by (memoize keyfn) current)))

;; sort each layer one by one from beginning to end
(defn wmedian [node->parents layers]
  (cons (first layers)
        (map (partial wmedian-mapfn node->parents)
             layers (next layers))))

;; Calculate number of edge crossings in a bipartite graph
;; TODO: this is one very naive implementation and we can surely improve it.
(comment
  (defn layer-crossings [node->parents nodes parents]
    (with-local-vars [s (sorted-map-by (fn [a b] (compare (index-of parents a) (index-of parents b)))), n 0]
      (doseq [node nodes
              parent (sort-by #(or (index-of parents %) 999) (node->parents node))
              :when  (index-of parents parent)
              :let [greater (transduce (map second) + (subseq (var-get s) > parent))]]
        (var-set n (+ (var-get n) greater))
        (var-set s (update (var-get s) parent (fnil inc 0))))
      (var-get n)))
  :end-of-comment)

;; calculate number of edge crossings in a bipartite graph
(defn layer-crossings [node->parents nodes parents]
  (->> (update-vals node->parents (partial sort-by (partial index-of parents)))
       (reduce-kv (fn [n+s node parents]
                    (reduce (fn [[n s] parent]
                              [(+ n (transduce (map second) + (subseq s > parent)))
                               (update s parent (fnil inc 0))])
                            n+s parents))
                  [0 (sorted-map-by (fn [a b] (compare (index-of parents a) (index-of parents b))))])
       (first)))


(defn crossings [node->parents layers]
  (reduce + (map (partial layer-crossings node->parents) layers (next layers))))

(defn vertex-ordering+1 [node->parents _ layers]
  (wmedian node->parents layers))

(defn vertex-ordering-1 [_ node->children layers]
  (reverse (wmedian node->children (reverse layers))))

;; returns a lazy seq of x, (f x), (g (f x)), (f (g (f x))), etc.
(defn iterate-evenodd [f g x]
  (cons x (lazy-seq (iterate-evenodd g f (f x)))))

;; order vertices in each layers to reduce crossings.
;; Sadly, it is an NP-hard problem, so we have to come up with heuristics.
(defn vertex-ordering [layers edges]
  ;; do a bunch of iterations and find one with the least crossings
  (let [node->parents  (reduce (fn [m e] (update m (:edge/target e) conj (:edge/source e))) {} edges)
        node->children (reduce (fn [m e] (update m (:edge/source e) conj (:edge/target e))) {} edges)]
    (->> layers
         (iterate-evenodd (partial vertex-ordering+1 node->parents node->children)
                          (partial vertex-ordering-1 node->parents node->children))
         (take 1000) ;; TODO: if we already have 0 crossings, we could stop iterating!
                     ;; TODO: it shuold come from config!
         (apply min-key (partial crossings node->parents)))))

;; adding hidden edges

;; FIXME: introduce a new type?
(defn ->hidden-node [] (gensym "hidden-node"))
(defn hidden-node? [n] (and (symbol? n) (starts-with? (name n) "hidden-node")))

;; adding hidden edges in place of long edges.
(defn add-hidden-edges [layers edges]
  (assert (vector? layers)) (assert (every? coll? layers))
  (assert (coll? edges)) (assert (every? map? edges))
  (let [node->layer (into {} (apply concat (map-indexed (fn [idx layer] (for [n layer] [n idx])) layers)))
        orange      (fn [x y] (range (inc (min x y)) (max x y)))]
    (reduce (fn [[layers edges] edge]
              (if-let [m (seq (orange (node->layer (:edge/source edge)) (node->layer (:edge/target edge))))]
                (let [pts (zipmap m (repeatedly ->hidden-node))]
                  [(reduce-kv (fn [layers idx p] (update layers idx conj p)) layers pts)
                   (into edges
                         (for [[e f] (partition 2 1 (concat [(:edge/source edge)] (vals pts) [(:edge/target edge)]))]
                           {:edge/source e
                            :edge/target f
                            :edge/hidden? true
                            :edge/original edge
                            :edge/id (gensym 'edge-hidden)
                            :edge/weight (:edge/weight edge)}
                           #_(assoc edge :edge/source e :edge/target f :edge/hidden? true :edge/original edge)))])
                [layers (conj edges edge)]))
            [layers ()] edges)))

#_ ;; never used
(defn rm-hidden-edges [layers edges]
  [(for [layer layers] (remove hidden-node? layer))
   (set (for [edge edges] (or (:edge/original edge) edge)))])
