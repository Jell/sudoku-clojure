## How to

In REPL, run:
    (in-ns 'sudoku)
    (def sudoku-grid ['(1 0 0 0 0 7 0 9 0)
                      '(0 3 0 0 2 0 0 0 8)
                      '(0 0 9 6 0 0 5 0 0)
                      '(0 0 5 3 0 0 9 0 0)
                      '(0 1 0 0 8 0 0 0 2)
                      '(6 0 0 0 0 4 0 0 0)
                      '(3 0 0 0 0 0 0 1 0)
                      '(0 4 0 0 0 0 0 0 7)
                      '(0 0 7 0 0 0 3 0 0)])
    (time (pretty-matrix (sudoku sudoku-grid)))