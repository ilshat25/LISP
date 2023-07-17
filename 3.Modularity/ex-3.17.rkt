#lang sicp

(define (count-pairs x)
  (define visited '())
  (define (iter x)
    (if (or (not (pair? x)) (memq x visited))
        0
        (begin
          (set! visited (cons x visited))
          (+ (iter (car x))
             (iter (cdr x))
             1))))
  (iter x))

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

