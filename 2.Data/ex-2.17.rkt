#lang racket

(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

; Testing
(define x (list 1 2 3 4 5))
(last-pair x)