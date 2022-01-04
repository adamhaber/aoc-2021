(require ['clojure.string :as 'str])

(def lines
  (->> (slurp "./day14_input")
       (str/split-lines)
       (map #(str/split % #" -> "))))

(def template (first (first lines)))
(def rules (mapv #(vector (first %) (last %)) (rest (rest lines))))
(def m (reduce conj {} rules))

(defn get-polymer [steps]
  (loop [i 1
         template template]
    (let [pairs (map str template (subs template 1))
          insersions (map m pairs)
          last-letter (last template)
          new-template-prefix (str/join (map #(str (first %1) %2) pairs insersions))
          res (str/join (list new-template-prefix last-letter))]
      (do (println (count pairs))
          (if (= i steps) res
              (recur (inc i) res))))))

(->> (get-polymer 10)
     frequencies
     (sort-by val)
     (#(- (second (last %)) (second (first %)))))

;; 14b
(def init-freqs (frequencies (map str template (subs template 1))))

(defn lift [counts k]
  (let [c (counts k)
        to-insert (m k)
        pair1 (str (first k) to-insert)
        pair2 (str to-insert (last k))]
    (hash-map pair1 c pair2 c)))

(defn get-polymer-counts [steps]
  (loop [i 1
         freqs init-freqs]
    (let [new-freqs (apply merge-with + (map #(lift freqs %) (keys freqs)))]
      (if (= i steps) new-freqs
          (recur (inc i) new-freqs)))))

(def poly-counts (get-polymer-counts 40))

(defn f [k]
  (let [b (last k)
        v (poly-counts k)]
    (hash-map b v)))

(->> poly-counts
     keys
     (map f)
     (apply merge-with +)
     (merge-with (hash-map (first template) 1))
     (sort-by val)
     (#(- (second (last %)) (second (first %)))))


