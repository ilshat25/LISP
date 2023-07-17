#lang sicp

(define (make-accumulator sm)
  (lambda (amount)
    (set! sm (+ sm amount))
    sm))

;;; Test
(define A (make-accumulator 5))
(A 10)
(A 15)
