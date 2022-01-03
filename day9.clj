(require ['clojure.string :as 'str])



(def lines
  (->> (slurp "./day9_input_large")
       (str/split-lines)
       (mapv #(str/split % #""))))

(def num-lines
  (let [n (count (first lines))
        f (flatten lines)
        num (map #(Integer/parseInt %) f)]
    (partition n num)))

(defn safe-nth [col i]
  (if (and (not (nil? col)) (>= i 0) (< i (count col))) (nth col i) nil))

(defn f [i j]
  (let [current (safe-nth (safe-nth num-lines i) j)
        above (safe-nth (safe-nth num-lines i) (dec j))
        below (safe-nth (safe-nth num-lines i) (inc j))
        left (safe-nth (safe-nth num-lines (dec i)) j)
        right (safe-nth (safe-nth num-lines (inc i)) j)]
    (if (every? #(< current %) (filter #(not (nil? %)) [above below left right]))
      (inc current) 0)))

;; 9a
(apply + (flatten (for [i (range (count num-lines))]
                    (for [j (range (count (first num-lines)))]
                      (f i j)))))
;; 9b
(def pairs
  (partition 2 (filter #(not (nil? %)) (flatten (for [i (range (count num-lines))]
                                                  (for [j (range (count (first num-lines)))]
                                                    (if (> (f i j) 0) [i j] nil)))))))
pairs


(defn in-visited? [pair]
  (some #(= % pair) @visited))

(defn basin-size [i j]
  (cond
    (in-visited? (list i j)) 0
    (= 9 (safe-nth (safe-nth num-lines i) j)) 0
    (nil? (safe-nth (safe-nth num-lines i) j)) 0
    :else (do
            (swap! visited conj (list i j))
            (+ 1 (basin-size (inc i) j) (basin-size (dec i) j) (basin-size i (dec j)) (basin-size i (inc j))))))

(def visited (atom ()))

(->> pairs
     (map #(basin-size (first %) (second %)))
     (sort >)
     (take 3)
     (apply *))