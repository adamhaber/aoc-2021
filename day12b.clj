(require ['clojure.string :as 'str])

(def lines
  (->> (slurp "./day12_input")
       (str/split-lines)
       (map #(str/split % #"-"))))
#_(map #(hash-map (keyword (first %)) (keyword (second %))))

(defn big-cave? [cave]
  (every? #(Character/isUpperCase %) cave))

(defn build-graph [lines G]
  (let [line (first lines)
        tail (rest lines)
        A (first line)
        B (second line)
        GA (G A)
        GB (G B)
        new-GA (if (nil? GA) (list B) (cons B GA))
        new-GB (if (nil? GB) (list A) (cons A GB))]
    (cond
      (empty? line) G
      :else (build-graph tail (assoc G A new-GA B new-GB)))))

(def G (build-graph lines {}))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

;;12b
(defn all-small-distinct [visited]
  (let [visited-small-caves (filter #(not (big-cave? %)) visited)]
    (apply distinct? visited-small-caves)))

(defn legal? [cave visited]
  (and (not (= "start" cave))
       (or (big-cave? cave)
           (not (in? visited cave))
           (all-small-distinct visited))))

(defn traverse-G [G current-node current-path visited]
  (let [next-nodes (G current-node)
        new-visited (cons current-node visited)
        legal-next-nodes (filter #(legal? % new-visited) next-nodes)]
    (if (= "end" current-node) (list (cons "end" current-path))
        (mapcat #(traverse-G G % (cons current-node current-path) new-visited) legal-next-nodes))))

(->> (traverse-G G "start" () ())
     distinct
     count)

