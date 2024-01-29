#lang racket

; these can be changed!
(define r 3) 
(define c 3)

(define dim (* r c))
(define cell-count (* dim dim))

(define default-hints (build-list (* r c) (lambda (n) (+ n 1))))
(define e default-hints)

; some boards for demoing/debugging

(define empty-board (make-list 9 (make-list 9 e)))

(define hard-board
        (list (list e 2 e e e e e e e)
              (list e e e 6 e e e e 3)
              (list e 7 4 e 8 e e e e)
              (list e e e e e 3 e e 2)
              (list e 8 e e 4 e e 1 e)
              (list 6 e e 5 e e e e e)
              (list e e e e 1 e 7 8 e)
              (list 5 e e e e 9 e e e)
              (list e e e e e e e 4 e)))

(define medium-board
        (list (list e e e 5 e 7 e e e)
              (list e 4 e 2 6 3 e e e)
              (list 1 e 7 4 e e e e e)
              (list 3 6 e e e e e 4 5)
              (list e e 2 e 5 e 7 e e)
              (list 7 9 e e e e e 6 2)
              (list e e e e e 9 4 e 1)
              (list e e e 1 3 4 e 9 e)
              (list e e e 6 e 5 e e e)))

(define easy-board
        (list (list e e e 2 6 e 7 e 1) 
            (list 6 8 e e 7 e e 9 e)
            (list 1 9 e e e 4 5 e e)
            (list 8 2 e 1 e e e 4 e)
            (list e e 4 6 e 2 9 e e)
            (list e 5 e e e 3 e 2 8)
            (list e e 9 3 e e e 7 4)
            (list e 4 e e 5 e e 3 6)
            (list 7 e 3 e 1 8 e e e)))

; format cell into an ascii symbol
; a dot represents an unfilled cell
(define (cell->string cell)
    (string-append (if (list? cell) "." (number->string cell)) " "))

; put additional space between groups
(define (row->string row)
    (apply 
        string-append 
        (map (lambda (i) (let ([cell (list-ref row i)])
                              (if (equal? (modulo i r) 0)
                                  (string-append "  " (cell->string cell))
                                  (cell->string cell))))
             (build-list (length row) values))))

; put space between groups vertically
(define (grid->string grid)
        (apply
            string-append
            (map (lambda (i) (if (equal? 0 (modulo i c)) 
                                 (string-append "\n\n" (row->string (list-ref grid i))) 
                                 (string-append "\n" (row->string (list-ref grid i)))))
                 (build-list (length grid) values))))

; print a graph to console
(define (print-grid grid)
        (printf (string-append (grid->string grid) "\n\n\n")))

; print a graph to console (including hints) for debugging
(define (show grid)
        (begin (printf "\n\n")
               (for ([row grid])
                    (begin (display row)
                           (printf "\n")))))

; simplify the hints according to uniqueness within rows
(define (reduce-rows grid)
        (for/list ([row grid])
                  (reduce-row row)))

; reduce hints in a single row
(define (reduce-row row)
        (reduce-row-helper row 0))

(define (reduce-row-helper row index)
        (if (equal? index (length row))
            row ; base case
            (let ([val (list-ref row index)])
                (if (number? val)
                    (reduce-row-helper
                        (for/list ([elem row])
                            (if (list? elem)
                                (filter 
                                    (lambda (hint) (not (equal? hint val)))
                                    elem)
                                elem))
                        (+ 1 index))
                    (reduce-row-helper row (+ 1 index))))))


; from: https://stackoverflow.com/questions/30775032/transpose-a-matrix-in-racket-list-of-lists
(define (transpose grid)
        (apply map list grid))

; reshape a group into a row
(define (group->row grid row col)
        (for/list ([i (build-list c values)]
                #:when #t
                [j (build-list r values)])
                (list-ref (list-ref grid (+ row i)) (+ col j))))

; this is a weird sort of transpose, so it turns out to be its own inverse
(define (groups->rows grid)
        (for/list ([row (build-list r values)]
                #:when #t
                [col (build-list c values)])
                (group->row grid (* row c) (* col r))))

; apply a row-defined process to rows, columns, and groups
(define (apply-to-houses proc grid)
        (groups->rows (proc (groups->rows (transpose (proc (transpose (proc grid))))))))

; simplify hints
; this is a bad name but I've already used it a million times
(define (reduce grid)
        (apply-to-houses reduce-rows grid))

; fill in cells with only one hint
(define (eliminate-lone-singles grid)
        (map (lambda (row) (map (lambda (cell) (if (list? cell)
                                                   (if (equal? 1 (length cell))
                                                       (first cell)
                                                       cell)
                                                   cell)) 
                                row))
             grid))

; detects if a hint only occurs once in a row (i.e. the cell it contains is a hidden single)
(define (hidden-single? hint row)
        (let ([contains-hint? (lambda (cell) (if (list? cell)
                                                (member hint cell)
                                                #false))])
            (let ([count (length (filter contains-hint? row))])
                (if (equal? count 1)
                    #true
                    #false))))

; fill in any hidden singles in the row
(define (eliminate-hidden-singles-from-row row)
        (for/list ([cell-index (build-list (length row) values)])
                (let ([cell (list-ref row cell-index)])
                    (if (list? cell)
                        (let ([singles (filter (lambda (hint) (hidden-single? hint row)) cell)])
                                (if (equal? (length singles) 1)
                                    (first singles)
                                    cell))
                        cell))))

; eliminate hidden singles of rows
(define (eliminate-hidden-singles-from-rows grid)
        (map eliminate-hidden-singles-from-row grid))

; single pass of eliminating hidden singles
; hidden single: https://www.learn-sudoku.com/hidden-singles.html
(define (eliminate-hidden-singles grid)
        (let ([r eliminate-hidden-singles-from-rows])
             (reduce (groups->rows (r (groups->rows (reduce (transpose (r (transpose (reduce (r grid))))))))))))

; eliminates hidden and lone singles until the board doesn't change
; assumes grid is reduced
; returned grid is reduced
(define (eliminate-singles grid)
        (let* ([step (lambda (grid) (reduce (eliminate-hidden-singles (reduce (eliminate-lone-singles grid)))))]
               [new-grid (step grid)]
               [changed (not (equal? new-grid grid))])
              (if changed
                  (eliminate-singles new-grid)
                  new-grid)))

; returns pair (row . col) of the first row-major cell without a value
(define (first-unfilled-cell grid)
        (let* ([first-cols (map (lambda (row) (index-where row list?)) grid)]
               [first-row (index-where first-cols number?)]
               [first-col (list-ref first-cols first-row)])
              (cons first-row first-col)))

; #t iff all cells are set
(define (filled-in? grid)
        (equal? (* r r c c) (apply + (map (lambda (row) (length (filter number? row))) grid))))

; grid solver
; uses analytical approach (lone-singles, hidden-singles, hint reduction)
; if/when no more progress can be made, employs backtracking by making a guess
; and seeing if it yields a solvable board
; returns either a solved board or #f if there is no solution
(define (solve grid)
        (let* ([reduced-grid (reduce (eliminate-singles (reduce grid)))]
               [valid (valid? reduced-grid)])
              (if valid
                  (if (filled-in? reduced-grid)
                      reduced-grid
                      (let* ([fuf (first-unfilled-cell reduced-grid)]
                             [row (car fuf)]
                             [col (cdr fuf)]
                             [hints (list-ref (list-ref reduced-grid row) col)])
                            (if (empty? hints)
                                #f
                                (try-hints reduced-grid row col hints 0))))
                  #f)))

; return a grid with a changed value
(define (update-grid grid row-index col-index val)
        (list-update grid row-index (lambda (row) (list-set row col-index val))))

; tries all hints for the given cell
; if none give a solvable board, then return false (board is unsolvable)
; if solutions is found, return it
(define (try-hints grid row col hints index)
        (if (equal? (length hints) index)
            #f
            (let* ([hint (list-ref hints index)]
                   [new-grid (solve (reduce (update-grid grid row col hint)))])
                  (if (equal? #f new-grid)
                      (try-hints grid row col hints (+ index 1))
                      new-grid))))

; check if rows are not violating any rules
(define (valid-rows? grid)
        (let ([valid-row? (lambda (row) (let ([nums (filter number? row)])
                                             (equal? (length nums) (set-count (list->set nums)))))])
              (andmap valid-row? grid)))

; check if grid is not violating any rules
(define (valid? grid)
        (and (valid-rows? grid) (valid-rows? (transpose grid)) (valid-rows? (groups->rows grid))))

; check if rows are filled in and contains unique values
(define (solved-rows grid)
        (andmap (lambda (row) (equal? dim (set-count (list->set row)))) grid))

; check if board is filled in and obeys all rules
(define (solved? grid)
        (and (solved-rows grid) (solved-rows (transpose grid)) (solved-rows (groups->rows grid))))

; 2d list-ref
(define (get grid row col)
        (list-ref (list-ref grid row) col))

; looks for any cells with no hints
(define (unsolvable? grid)
        (ormap (lambda (row) (member (list) row)) grid))

; put in n random values that obey the basic rules
(define (fill-in grid n)
        (if (equal? 0 n)
            grid
            (let* ([dim (* r c)]
                   [row (random 0 dim)]
                   [col (random 0 dim)]
                   [val (get grid row col)])
                   (if (number? val)
                       (fill-in grid n)
                       (let ([new-grid (update-grid grid row col (random 1 (+ 1 dim)))])
                            (if (and (valid? new-grid) (not (unsolvable? grid)))
                                (fill-in new-grid (- n 1))
                                (fill-in grid n)))))))

; puts values in that don't violate rules but does not guarantee solvability
(define (generate-grid start-count)
        (fill-in (make-list (* r c) (make-list (* r c) e)) start-count))

; number of set values in the grid
(define (count grid)
        (apply + (map (lambda (row) (length (filter number? row))) grid)))

; clear a random cell
(define (try-remove grid)
        (let* ([row (random 0 dim)]
               [col (random 0 dim)])
              (update-grid grid row col default-hints)))

; keep clearing cells until n remain
(define (keep-n grid n)
        (if (equal? n (count grid))
            grid
            (keep-n (try-remove grid) n)))

; generate a grid that is guaranteed to have at least one solution
; 1) naively generate a grid with a few "seed" values
; 2) solve the grid
; 3) keep a subset of size start-count from the solved grid
; 4) return subset as solvable puzzle
(define (generate-solvable-grid start-count)
        (let* ([dummy (generate-grid 3)]
               [solved (solve dummy)])
              (if (equal? #f solved)
                  (generate-solvable-grid start-count) ; try again
                  (keep-n solved start-count))))

; harness for demoing solver
(define (test-solve grid)
        (let* ([solution (solve grid)]
               [found-solution (not (equal? #f solution))]
               [validity (if found-solution (valid? solution) #f)])
              (if found-solution
                  (begin (printf "Problem:")
                         (print-grid grid)
                         (printf "Solution:")
                         (print-grid solution)
                         (if validity
                             (printf "Solution is valid\n")
                             (printf "Solution is invalid\n")))
                  (begin (printf "Problem:")
                         (print-grid grid)
                         (printf "Board is unsolvable\n")))))


; (test-solve easy-board)
; (test-solve medium-board)
; (test-solve hard-board)

(test-solve (generate-solvable-grid 17))