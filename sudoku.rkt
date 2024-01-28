#lang racket

(define r 3)
(define c 3)

(define default-hints (list 1 2 3 4 5 6 7 8 9))
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

(define (reduce grid)
    (groups->rows (reduce-rows (groups->rows (transpose (reduce-rows (transpose (reduce-rows grid))))))))

(print-grid! easy-board)
(display (reduce easy-board))

; (display (row->string (group->row easy-board 3 0)))



; (define reduced-easy-row (reduce-row (list-ref easy-board 0)))

; (display reduced-easy-row)

; (display (reduce-rows easy-board))


#|
1. guess

2. reduce

3. hidden/naked singles
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

