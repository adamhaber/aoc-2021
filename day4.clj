(require ['clojure.string :as 'str])

(def lines (str/split-lines (slurp "./day4_input_large")))

(def all-nums
  (->> lines
       first
       (#(str/split % #","))))

(def init-boards
  (->> lines
       rest
       (partition 6)
       (map rest)
       (map #(str/join " " %))
       (map #(str/split % #" "))
       (map #(filter not-empty %))))
(first init-boards)

(defn mark-board [num board]
  (replace (hash-map num (list num "true")) board))

(defn won-row? [row]
  (every? true? (map list? row)))

(defn won-board? [board]
  (if-not (every? false? (map won-row? board))
    true
    false))

(defn sum-board [pred board]
  (if (false? pred) 0
      (->> board
           (apply concat)
           (filter #(not (list? %)))
           (map #(Integer/parseInt %))
           (apply +))))

(defn transpose [xs]
  (apply map list xs))

(defn day4 [draws boards]
  (loop [draws draws
         boards boards]
    (let [num (first draws)
          marked-boards (map #(mark-board num %) boards)
          marked-square-boards (map #(partition 5 %) marked-boards)
          transposed-boards (map transpose marked-square-boards)
          new-wins (map won-board? marked-square-boards)
          new-wins-transpose (map won-board? transposed-boards)
          wins (map #(or %1 %2) new-wins new-wins-transpose)]
      (if (empty? draws) "bla"
          (if (some true? wins)
            (* (Integer/parseInt num) (apply + (map sum-board wins marked-square-boards)))
            (recur (rest draws) marked-boards))))))

(defn day4b [draws boards]
  (loop [draws draws
         boards boards
         prev-wins (repeat (count boards) false)]
    (let [num (first draws)
          marked-boards (map #(mark-board num %) boards)
          marked-square-boards (map #(partition 5 %) marked-boards)
          transposed-boards (map transpose marked-square-boards)
          new-wins (map won-board? marked-square-boards)
          new-wins-transpose (map won-board? transposed-boards)
          wins (map #(or %1 %2) new-wins new-wins-transpose)]
      (println num)
      (println (second marked-square-boards))
      (println (second transposed-boards))
      (if (empty? draws) "bla"
          (if (every? true? wins)
            (* (Integer/parseInt num) (apply + (map sum-board (map not prev-wins) marked-square-boards)))
            (recur (rest draws) marked-boards wins))))))

(day4 all-nums init-boards)
(day4b all-nums init-boards)
