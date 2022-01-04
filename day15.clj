(require '[clojure.data.priority-map :refer [priority-map]])
(require ['clojure.string :as 'str])

(def lines
  (->> (slurp "./day15_input_large")
       (str/split-lines)
       (map #(str/split % #""))))

(def nx (count lines))
(def ny (count (first lines)))

(def node-weights
  (->> (for [i (range nx)]
         (for [j (range ny)]
           (hash-map (list i j) (Integer/parseInt (nth (nth lines i) j)))))
       flatten
       (apply merge)))


;; from https://www.ummels.de/2014/06/08/dijkstra-in-clojure/
(defn map-vals [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remove-keys [m pred]
  (select-keys m (filter (complement pred) (keys m))))

(defn dijkstra
  "Computes single-source shortest path distances in a directed graph.

  Given a node n, (f n) should return a map with the successors of n
  as keys and their (non-negative) distance from n as vals.
 
  Returns a map from nodes to their distance from start."
  [start f]
  (loop [q (priority-map start 0) r {}]
    (if-let [[v d] (peek q)]
      (let [dist (-> (f v) (remove-keys r) (map-vals (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(defn neighbors [v ws]
  (let [x (first v)
        y (second v)
        all-ns (list (list (dec x) y) (list (inc x) y) (list x (inc y)) (list x (dec y)))]
    (filter #(contains? ws %) all-ns)))

(defn costs [weights]
  (fn [v]
    (let [neighbors (neighbors v weights)
          nweights (map weights neighbors)
          pairs (map hash-map neighbors nweights)]
      (apply merge pairs))))

((dijkstra '(0 0) (costs node-weights)) (list (dec nx) (dec ny)))

;;15b
(defn wrapped-weights [i j repx repy]
  (let [orig (Integer/parseInt (nth (nth lines i) j))
        incd (+ orig repx repy)]
    (if (> incd 9) (nth (range 1 10) (rem incd 10)) incd)))

(def wrapped-node-weights
  (->> (for [repx (range 5)]
         (for [repy (range 5)]
           (for [i (range nx)]
             (for [j (range ny)]
               (hash-map (list (+ i (* nx repx)) (+ j (* ny repy))) (wrapped-weights i j repx repy))))))
       flatten
       (apply merge)))

((dijkstra '(0 0) (costs wrapped-node-weights)) (list (dec (* 5 nx)) (dec (* 5 ny))))


