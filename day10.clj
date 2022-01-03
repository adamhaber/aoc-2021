(require ['clojure.string :as 'str])

(def lines
  (->> (slurp "./day10_input_large")
       (str/split-lines)
       (map #(str/split % #""))))


(defn analyze-lines [line stack]
  (let [current-char (first line)
        tail (rest line)
        top-stack (peek stack)]
    (do (println stack)
        (cond
          (and (empty? line) (not (empty? stack))) "incomplete"
          (and (= current-char ">") (= top-stack "<")) (analyze-lines tail (pop stack))
          (and (= current-char ")") (= top-stack "(")) (analyze-lines tail (pop stack))
          (and (= current-char "}") (= top-stack "{")) (analyze-lines tail (pop stack))
          (and (= current-char "]") (= top-stack "[")) (analyze-lines tail (pop stack))
          (= current-char ">") ">"
          (= current-char ")") ")"
          (= current-char "}") "}"
          (= current-char "]") "]"
          :else (analyze-lines tail (conj stack current-char))))))

(defn give-score [char]
  (cond
    (= char ">") 25137
    (= char "}") 1197
    (= char "]") 57
    (= char ")") 3))

(->> lines
     (map #(analyze-lines % '()))
     (filter #(not (= % "incomplete")))
     (map give-score)
     (apply +))

;; 10b
(defn analyze-lines-b [line stack]
  (let [current-char (first line)
        tail (rest line)
        top-stack (peek stack)]
    (do (println stack)
        (cond
          (and (empty? line) (not (empty? stack))) stack
          (and (= current-char ">") (= top-stack "<")) (analyze-lines-b tail (pop stack))
          (and (= current-char ")") (= top-stack "(")) (analyze-lines-b tail (pop stack))
          (and (= current-char "}") (= top-stack "{")) (analyze-lines-b tail (pop stack))
          (and (= current-char "]") (= top-stack "[")) (analyze-lines-b tail (pop stack))
          (= current-char ">") nil
          (= current-char ")") nil
          (= current-char "}") nil
          (= current-char "]") nil
          :else (analyze-lines-b tail (conj stack current-char))))))

(defn give-score-b [char]
  (cond
    (= char "<") 4
    (= char "{") 3
    (= char "[") 2
    (= char "(") 1))

(defn f [a b]
  (+ (* a 5) b))

(defn g [l]
  (->> l
       (map give-score-b)
       (reduce f 0)))

(->> lines
     (map #(analyze-lines-b % '()))
     (map g)
     (filter #(< 0 %))
     median)

(defn median [ns]
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))
