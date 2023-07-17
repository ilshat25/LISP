#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


(define x (cons 'a 'b))
(define y (cons 'c 'd))
(define z (cons x y))
; 3
(count-pairs z)
; 4
(set-car! x y)
(count-pairs z)
; 5
(set-cdr! x y)
(count-pairs z)
;7
(set-cdr! z x)
(count-pairs z)
; inf
(set-car! y z)
(count-pairs z)
