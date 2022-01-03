(require ['clojure.string :as 'str])

(def lines
  (->> (slurp "./day13_input_large")
       (str/split-lines)
       (map #(str/split % #","))
       (partition-by #(= [""] %))))

(def coords (first lines))
(def folds (last lines))

(def pairs
  (->> coords
       flatten
       (map #(Integer/parseInt %))
       (partition 2)))

(def xs (map first pairs))
(def ys (map second pairs))

(defn get-fold-num [fold]
  (->> fold
       first
       (#(str/split % #"="))
       last
       (#(Integer/parseInt %))))

(defn get-fold-dir [fold]
  (->> fold
       first
       (#(str/split % #"="))
       first
       last))

(defn relfect [x y]
  (if (> x y) (- y (- x y)) x))

(defn update-coords [coords dir num]
  (let [xs (first coords)
        ys (second coords)]
    (cond
      (= dir \y) [xs (map #(relfect % num) ys)]
      (= dir \x) [(map #(relfect % num) xs) ys]
      :else "foo")))

(def new-coords
  (loop [folds (list (vec (first folds)))
         new-coords [xs ys]]
    (let [current-fold (first folds)
          dir (if-not (empty? folds) (get-fold-dir current-fold) nil)
          num (if-not (empty? folds) (get-fold-num current-fold) nil)]
      (if (empty? folds) new-coords
          (recur (rest folds) (update-coords new-coords dir num))))))

(->> new-coords
     (#(map list (first %) (second %)))
     distinct
     count)

(def new-coords
  (loop [folds folds
         new-coords [xs ys]]
    (let [current-fold (first folds)
          dir (if-not (empty? folds) (get-fold-dir current-fold) nil)
          num (if-not (empty? folds) (get-fold-num current-fold) nil)]
      (if (empty? folds) new-coords
          (recur (rest folds) (update-coords new-coords dir num))))))

(def sol (->> new-coords
     (#(map list (first %) (second %)))
     distinct))


(def inits (for [i (range 1 (inc ))]
  (for [j (range 1 (inc 7))]
    0)))