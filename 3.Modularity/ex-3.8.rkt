#lang scheme

(define f-my
  (let ((prev 0))
    (lambda (n)
      (let ((cur prev))
        (set! prev n)
        cur))))

(define (g y)
  (define (f x)
    (let ((z y))
      (set! y x)
      z))
  f)

(define f (g 0))

;; Evaluates from left to right
;(+ (f 0) (f 1))
(+ (f 1) (f 0))
