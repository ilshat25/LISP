#lang scheme

(define (square x) (* x x))

(define (square-list-1 items)
  (if (null? items)
      null
      (cons (square (car items))
            (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

; Testing

(define test-list (list 1 2 3 4 5 6))

(square-list-1 test-list)
(square-list-2 test-list)


