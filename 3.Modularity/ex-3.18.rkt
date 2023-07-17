#lang sicp

(define (contain-cycle? x)
  (define (iter v prev)
    (cond ((not (pair? v)) false)
          ((memq v prev) true)
          (else
           (or (iter (car v) (cons v prev))
               (iter (cdr v) (cons v prev))))))
  (iter x '()))

(define cycle (list 'a 'b 'c))
(set-cdr! (cddr cycle) cycle)

(define no-cycle (list 'a 'b 'c))

(contain-cycle? cycle)
(contain-cycle? no-cycle)
        