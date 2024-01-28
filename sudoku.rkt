#lang racket

(define r 3)
(define c 3)

(define default-hints (build-list (* r c) (lambda (n) (+ n 1))))
(define e default-hints)

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
        (printf (string-append (grid->string grid) "\n\n\n")))


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

(define (eliminate-lone-singles grid)
        (for/list ([row grid])
                (for/list ([cell row])
                            (if (list? cell)
                                (if (equal? 1 (length cell))
                                    (first cell)
                                    cell)
                                cell))))

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
        (apply-to-houses eliminate-hidden-singles-from-rows grid))

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


(print-grid! easy-board)
; (display (reduce easy-board))
(print-grid! (eliminate-singles easy-board))
(display (eliminate-singles easy-board))

; (display (row->string (group->row easy-board 3 0)))



; (define reduced-easy-row (reduce-row (list-ref easy-board 0)))

; (display reduced-easy-row)

; (display (reduce-rows easy-board))


#|
1. guess

2. reduce

3. hidden/lone singles
    - if changed, go back to step 2

reduce:

b b b
b b b
b b b

b b b
b b b
b b b

b b b
b b b
b b b



|#

