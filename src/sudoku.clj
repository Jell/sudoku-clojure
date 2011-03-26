(ns sudoku)
(use '(incanter core io))

(def escargot ['(1 0 0 0 0 7 0 9 0)
               '(0 3 0 0 2 0 0 0 8)
               '(0 0 9 6 0 0 5 0 0)
               '(0 0 5 3 0 0 9 0 0)
               '(0 1 0 0 8 0 0 0 2)
               '(6 0 0 0 0 4 0 0 0)
               '(3 0 0 0 0 0 0 1 0)
               '(0 4 0 0 0 0 0 0 7)
               '(0 0 7 0 0 0 3 0 0)])

(defn normalize-matrix [m]
  (matrix (matrix-map #(if (zero? %) 0 1) m)))

(defn values-for-mask [m, mask]
  (map #(int %) (filter #(not (zero? %)) (flatten (to-list (mult m mask))))))

(defn available-values [m, mask]
  (apply disj (cons (set (range 1 10)) (values-for-mask m mask))))

(defn line-mask [i]
  (concat (replicate (- i 1) (replicate 9 0))
          (replicate 1       (replicate 9 1))
          (replicate (- 9 i) (replicate 9 0))))

(defn vertical-mask [row]
  (apply bind-columns (line-mask row)))

(defn horizontal-mask [column]
  (apply bind-rows (line-mask column)))

(defn square-range [row]
  (let [i (+ 1 (* 3 (int (/ (- row 1) 3))))]
  (range i (+ i 3))))

(defn square-mask [row, column]
  (mult (apply plus (map vertical-mask (square-range column)))
        (apply plus (map horizontal-mask (square-range row)))))

(defn mask-for [row, column]
  (normalize-matrix (plus (square-mask row column)
                          (horizontal-mask row)
                          (vertical-mask column))))

(defn available-values-for-cell [m, row, column]
  (available-values (matrix m) (mask-for row column)))

(defn constraints-for-cell [m ,row, column]
  (- 9 (count (available-values-for-cell m row column))))

(defn get-cell [m, row, column]
  (nth (nth m (- row 1)) (- column 1)))

(defn set-cell [m, cell, new-value]
  (let [row (- (first cell) 1) column (- (last cell) 1)]
  (map-indexed  (fn [i, row-vector]
                  (if (== row i)
                      (map-indexed  (fn [j, value]
                                      (if (== column j)
                                          new-value
                                          value))
                                    row-vector)
                      row-vector))
                m)))

(defn constraint-matrix [m]
  (map-indexed  (fn [row, row-vector]
                  (map-indexed  (fn [column, value]
                                  [ (+ row 1)
                                    (+ column 1)
                                    (if (zero? (get-cell m (+ row 1) (+ column 1)))
                                        (constraints-for-cell m (+ row 1) (+ column 1))
                                        -1)])
                                row-vector))
                m))

(defn empty-cells [m]
  (filter #(>= (nth % 2) 0) (apply concat (constraint-matrix m))))

(defn empty-cells-ordered-by-contraints [m]
  (map #(take 2 %) (sort-by #(nth % 2) > (empty-cells m))))

(defn pretty-matrix [m]
  (doseq [row m] (println row)))

(defn sudoku [m]
  (pretty-matrix m)
  (println "\n----------------\n")
  (let [cell (first (empty-cells-ordered-by-contraints m))]
  (if (nil? cell)
      m
      (let [availables (apply available-values-for-cell (cons m cell))]
      (if (empty? availables)
          nil
          (take 9 (for [value availables result (sudoku (set-cell m cell value)) :while result] result)))))))


;(defn sudoku [m]
;  (pretty-matrix m)
;  (println "\n----------------\n")
;  (if (empty? (empty-cells m))
;      m
;      (let [cell (first (empty-cells-ordered-by-contraints m))]
;        (let [availables (apply available-values-for-cell (cons m cell))]
;          (if (empty? availables)
;            nil
;            (take 9 (for [value availables result (sudoku (set-cell m cell value)) :while result] result)))))))
