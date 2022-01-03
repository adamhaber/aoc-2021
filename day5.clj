(require ['clojure.string :as 'str])

(def lines (str/split-lines (slurp "./day5_input_large")))

(def final-map (atom {}))

(def str-coords
  (->> lines
       (mapv #(str/split % #" -> "))
       (mapv #(str/join "," %))
       (mapv #(str/split % #","))))

(def num-coords (for [v str-coords] (map #(Integer/parseInt %) v)))

(defn update-both [xs ys]
  (let [all-vals (map list xs ys)]
    (doseq [k all-vals]
      (if-let [e (find @final-map k)]
        (swap! final-map assoc k (inc (val e)))
        (swap! final-map assoc k 1)))))


;; 5b
(loop [coords num-coords]
  (let [v (first coords)
        r (rest coords)
        x1 (first v)
        y1 (second v)
        x2 (nth v 2)
        y2 (nth v 3)]
    (if (empty? v) nil
        (do
          (cond
            (= x1 x2) (update-both (repeat x1) (range (min y1 y2) (inc (max y1 y2))))
            (= y1 y2) (update-both (range (min x1 x2) (inc (max x1 x2))) (repeat y1))
            (and (< x1 x2) (< y1 y2)) (update-both (range x1 (inc x2)) (range y1 (inc y2)))
            (and (< x1 x2) (> y1 y2)) (update-both (range x1 (inc x2)) (range y1 (dec y2) -1))
            (and (> x1 x2) (< y1 y2)) (update-both (range x1 (dec x2) -1) (range y1 (inc y2)))
            (and (> x1 x2) (> y1 y2)) (update-both (range x1 (dec x2) -1) (range y1 (dec y2) -1)))
          (recur r)))))

;; 19929
(->> @final-map
     vals
     (filter #(<= 2 %))
     count)


;; old - 5b now generalizes these functions
#_(defn update-x [x minval maxval]
    (let [all-vals (map #(list x %) (range minval maxval))]
      (doseq [k all-vals]
        (if-let [e (find @final-map k)]
          (swap! final-map assoc k (inc (val e)))
          (swap! final-map assoc k 1)))))

#_(defn update-y [y minval maxval]
    (let [all-vals (map #(list % y) (range minval maxval))]
      (doseq [k all-vals]
        (if-let [e (find @final-map k)]
          (swap! final-map assoc k (inc (val e)))
          (swap! final-map assoc k 1)))))

;; 5a
#_(loop [coords num-coords]
    (let [v (first coords)
          r (rest coords)
          x1 (first v)
          y1 (second v)
          x2 (nth v 2)
          y2 (nth v 3)]
      (if (empty? v) nil
          (do
            (cond
              (= x1 x2) (update-x x1 (min y1 y2) (inc (max y1 y2)))
              (= y1 y2) (update-y y1 (min x1 x2) (inc (max x1 x2)))
              :else nil)
            (recur r)))))

