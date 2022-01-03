(require ['clojure.string :as 'str])

(defn to-nums [v]
  (map #(Integer/parseInt %) v))

(def lines
  (->> (slurp "./day11_input")
       (str/split-lines)
       (map #(str/split % #""))
       (map to-nums)))

(defn inc-all [ls]
  (map #(map inc %) ls))



lines
(inc-all lines)
