#lang sicp

(define (contains-cycle? x)
  (define (iter fast slow)
    (cond ((or (not (pair? fast)) (not (pair? (cdr fast))) (not (pair? slow))) false)
          ((eq? fast slow) true)
          (else (iter (cddr fast) (cdr slow)))))
  (if (pair? x)
      (iter (cdr x) x)
      false))

(define cycle (list 'a 'b 'c))
(set-cdr! (cddr cycle) cycle)
(define non-cycle (list 'a 'b 'c))

(contains-cycle? cycle)
(contains-cycle? non-cycle)
  