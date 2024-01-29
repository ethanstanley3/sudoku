#lang racket

(define r 3)
(define c 3)
(define cell-count (* r r c c))

(define default-hints (build-list (* r c) (lambda (n) (+ n 1))))
(define e default-hints)

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

; (define easy-board
;     (list (list e e e 2 e e 7 e 1) 
;           (list e 8 e e 7 e e 9 e)
;           (list 1 e e e e 4 e e e)
;           (list e e e 1 e e e e e)
;           (list e e e e e e e e e)
;           (list e 5 e e e 3 e e e)
;           (list e e e 3 e e e e 4)
;           (list e e e e 5 e e e 6)
;           (list 7 e e e e e e e e)))

(define (make-empty-row len)
    (if (> len 0) (cons default-hints (make-empty-row (- len 1))) (list)))

(define (make-empty-grid-helper rows cols)
    (if (> rows 0) (cons (make-empty-row cols) (make-empty-grid-helper (- rows 1) cols)) (list)))

(define (make-empty-grid rows cols)
    (make-empty-grid-helper (* rows cols) (* rows cols)))

(define (cell->string cell)
    (string-append (if (list? cell) "." (number->string cell)) " "))

(define (row->string row)
    (apply 
        string-append 
        (for/list 
            ([i (build-list (length row) values)])
            (let ([cell (list-ref row i)])
                (if (equal? (modulo i r) 0)
                    (string-append "  " (cell->string cell))
                    (cell->string cell))))))

(define (grid->string grid)
        (apply
            string-append
            (for/list ([i (build-list (length grid) values)])
                (if (equal? 0 (modulo i c)) 
                    (string-append "\n\n" (row->string (list-ref grid i))) 
                    (string-append "\n" (row->string (list-ref grid i)))))))

(define (print-grid! grid)
        (if (equal? #f grid)
            (printf "Board is unsolvable\n\n")
            (printf (string-append (grid->string grid) "\n\n\n"))))

(define (show grid)
        (begin (printf "\n\n")
               (for ([row grid])
                    (begin (display row)
                           (printf "\n")))))

(define (reduce-rows grid)
        (for/list ([row grid])
                  (reduce-row row)))

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


; from stackoverflow
(define (transpose grid)
        (apply map list grid))

(define (group->row grid row col)
        (for/list ([i (build-list c values)]
                #:when #t
                [j (build-list r values)])
                (list-ref (list-ref grid (+ row i)) (+ col j))))

; this is a weird sort of transpose, so it turns out to be its own inverse!
(define (groups->rows grid)
        (for/list ([row (build-list r values)]
                #:when #t
                [col (build-list c values)])
                (group->row grid (* row c) (* col r))))

(define (apply-to-houses proc grid)
        (groups->rows (proc (groups->rows (transpose (proc (transpose (proc grid))))))))

(define (reduce grid)
        (apply-to-houses reduce-rows grid))

; (define (eliminate-lone-singles grid)
;         (for/list ([row grid])
;                 (for/list ([cell row])
;                             (if (list? cell)
;                                 (if (equal? 1 (length cell))
;                                     (first cell)
;                                     cell)
;                                 cell))))

(define (eliminate-lone-singles grid)
        (map (lambda (row) (map (lambda (cell) (if (list? cell)
                                                   (if (equal? 1 (length cell))
                                                       (first cell)
                                                       cell)
                                                   cell)) 
                                row))
             grid))

(define (hidden-single? hint row)
        (let ([contains-hint? (lambda (cell) (if (list? cell)
                                                (member hint cell)
                                                #false))])
            (let ([count (length (filter contains-hint? row))])
                (if (equal? count 1)
                    #true
                    #false))))

; assume that hints have been reduced
(define (eliminate-hidden-singles-from-row row)
        (for/list ([cell-index (build-list (length row) values)])
                (let ([cell (list-ref row cell-index)])
                    (if (list? cell)
                        (let ([singles (filter (lambda (hint) (hidden-single? hint row)) cell)])
                                (if (equal? (length singles) 1)
                                    (first singles)
                                    cell))
                        cell))))

(define (eliminate-hidden-singles-from-rows grid)
        (map eliminate-hidden-singles-from-row grid))

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

(define (filled-in? grid)
        (equal? (* r r c c) (apply + (map (lambda (row) (length (filter number? row))) grid))))


; (define (solve grid)
;         (solve-helper (reduce grid)))

; returns solved grid or #f if the grid cannot be solved
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

(define (update-grid grid row-index col-index val)
        (list-update grid row-index (lambda (row) (list-set row col-index val))))

; returns solved grid
; or false if the grid is unsolvable
(define (try-hints grid row col hints index)
        (if (equal? (length hints) index)
            #f
            (let* ([hint (list-ref hints index)]
                   [new-grid (solve (reduce (update-grid grid row col hint)))])
                  (if (equal? #f new-grid)
                      (try-hints grid row col hints (+ index 1))
                      new-grid))))

(define (valid-rows? grid)
        (let ([valid-row? (lambda (row) (let ([nums (filter number? row)])
                                             (equal? (length nums) (set-count (list->set nums)))))])
              (andmap valid-row? grid)))

(define (valid? grid)
        (and (valid-rows? grid) (valid-rows? (transpose grid)) (valid-rows? (groups->rows grid))))

(define (solved-rows grid)
        (andmap (lambda (row) (equal? (* r c) (set-count (list->set row)))) grid))


(define (solved? grid)
        (and (solved-rows grid) (solved-rows (transpose grid)) (solved-rows (groups->rows grid))))

(define (get grid row col)
        (list-ref (list-ref grid row) col))

(define (unsolvable? grid)
        (ormap (lambda (row) (member (list) row)) grid))

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

(define (generate-grid start-count)
        (fill-in (make-list (* r c) (make-list (* r c) e)) start-count))

(define (generate-solvable-grid start-count))

; (define (generate-grid cells)
;         (let* ([start (reduce (make-random-grid (quotient (* r r c c) 5)))]
;                [solved (solve start)])
;               (begin (show start)
;                      (print-grid! solved)
;                      (if (not (equal? #f solved))
;                          (display (valid? solved))
;                          (display "hi")))))

(define (test-solve grid)
        (let* ([solution (solve grid)]
               [found-solution (not (equal? #f solution))]
               [validity (if found-solution (valid? solution) #f)])
              (if found-solution
                  (begin (printf "Problem:")
                         (print-grid! grid)
                         (printf "Solution:")
                         (print-grid! solution)
                         (if validity
                             (printf "Solution is valid\n")
                             (printf "Solution is invalid\n")))
                  (begin (printf "Problem:")
                         (print-grid! grid)
                         (printf "Board is unsolvable\n")))))

(test-solve (generate-grid 1))