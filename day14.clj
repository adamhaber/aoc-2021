(require ['clojure.string :as 'str])

(def lines
  (->> (slurp "./day14_input_large")
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

(->> (get-polymer 20)
     frequencies
     (sort-by val)
     (#(- (second (last %)) (second (first %)))))


(frequencies template)


(defn get-polymer-counts [steps]
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


