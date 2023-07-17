#lang racket

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (plus f-1 f-2)
  (lambda (f) (lambda (x) ((f-1 f) ((f-2 f) x)))))

; Testing
(define (square x) (* x x))

((one square) 2)
((two square) 2)
(((add-1 two) square) 2)
(((plus two two) square) 2)