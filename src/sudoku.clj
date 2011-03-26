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

(def GRID_WIDTH 9)
(def SUB_GRID_WIDTH 3)

(defn pretty-matrix [m]
  (doseq [row m] (println row)))

(defn normalize-matrix [m]
  (matrix (matrix-map #(if (zero? %) 0 1) m)))

(defn line-mask [i]
  (concat (replicate i (replicate GRID_WIDTH 0))
          (replicate 1 (replicate GRID_WIDTH 1))
          (replicate (- GRID_WIDTH 1 i) (replicate GRID_WIDTH 0))))

(def line-mask (memoize line-mask))

(defn vertical-mask [row]
  (apply bind-columns (line-mask row)))

(def vertical-mask (memoize vertical-mask))

(defn horizontal-mask [column]
  (apply bind-rows (line-mask column)))

(def horizontal-mask (memoize horizontal-mask))

(defn square-range [row]
  (let [i (* SUB_GRID_WIDTH (int (/ row SUB_GRID_WIDTH)))]
  (range i (+ i SUB_GRID_WIDTH))))

(def square-range (memoize square-range))

(defn square-mask [row, column]
  (mult (apply plus (map vertical-mask (square-range column)))
        (apply plus (map horizontal-mask (square-range row)))))

(def square-mask (memoize square-mask))

(defn mask-for [row, column]
  (normalize-matrix (plus (square-mask row column)
                          (horizontal-mask row)
                          (vertical-mask column))))

(def mask-for (memoize mask-for))

(defn values-for-mask [m, mask]
  (map #(int %) (filter #(not (zero? %)) (flatten (to-list (mult m mask))))))

(defn available-values-for-mask [m, mask]
  (apply disj (cons (set (take GRID_WIDTH (iterate inc 1))) (values-for-mask m mask))))

(defn get-cell [m, row, column]
  (nth (nth m row) column))

(defn set-cell [m, cell, new-value]
  (let [row (first cell) column (last cell)]
  (map-indexed  (fn [i, row-vector]
                  (if (== row i)
                      (map-indexed  (fn [j, value] (if (== column j) new-value value))
                                    row-vector)
                      row-vector))
                m)))

(defn available-values-for-cell [m, row, column]
  (available-values-for-mask (matrix m) (mask-for row column)))

(defn constraints-for-cell [m ,row, column]
  (- GRID_WIDTH (count (available-values-for-cell m row column))))

(defn constraint-matrix [m]
  (map-indexed  (fn [row, row-vector]
                  (map-indexed  (fn [column, value]
                                  [ row
                                    column
                                    (if (zero? (get-cell m row column))
                                        (constraints-for-cell m row column)
                                        -1)])
                                row-vector))
                m))

(defn empty-cells [m]
  (filter #(>= (nth % 2) 0) (apply concat (constraint-matrix m))))

(defn empty-cells-ordered-by-contraints [m]
  (map #(take 2 %) (sort-by #(nth % 2) > (empty-cells m))))

(defn sudoku [m]
;  (pretty-matrix m)
;  (println "\n----------------\n")
  (let [cell (first (empty-cells-ordered-by-contraints m))]
  (if (nil? cell)
      m
      (let [availables (apply available-values-for-cell (cons m cell))]
      (if (empty? availables)
          nil
          (take GRID_WIDTH
                (for [value availables result (sudoku (set-cell m cell value)) :while result]
                     result)))))))
